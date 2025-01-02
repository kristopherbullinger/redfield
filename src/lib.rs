use std::str::FromStr;

use compact_str::CompactString;
pub mod lexer;

pub fn parse_document_from_str(inp: &str) -> Result<Document, ParseError> {
    let mut iter = lexer::lex_document_iter(inp);
    let mut base_url = None;
    let Some(tok) = iter.next() else {
        return Ok(Document {
            base_url: None,
            services: vec![],
            definitions: vec![],
        });
    };

    let mut tok = tok?;
    let mut definitions = vec![];
    let mut services = vec![];

    // parse top-level identifiers
    loop {
        let line = iter.line;
        let col = iter.col;
        match tok.type_ {
            lexer::TokenType::Ident(ide) => {
                if ide.as_str() == "base_url" {
                    base_url = Some(munch_base_url(&mut iter)?);
                } else {
                    return Err(parse_error(
                        line,
                        col,
                        ParseErrorType::UnknownTopLevelIdent(ide.0),
                    ));
                }
            }
            _ => {
                break;
            }
        }
        tok = match iter.next() {
            Some(tok) => tok?,
            None => {
                return Ok(Document {
                    base_url,
                    services,
                    definitions,
                });
            }
        }
    }
    // parse the rest of the document
    loop {
        let line = iter.line;
        let col = iter.col;
        match tok.type_ {
            lexer::TokenType::Ident(ide) => {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::TopLevelIdentNotAtBeginningOfFile(ide),
                ));
            }
            lexer::TokenType::KeywordService => {
                let svc = munch_service(&mut iter)?;
                services.push(svc);
            }
            lexer::TokenType::KeywordMessage => {
                let msg = munch_message(&mut iter)?;
                definitions.push(Definition::Message(msg));
            }
            lexer::TokenType::KeywordEnum => {
                let enm = munch_enum(&mut iter)?;
                definitions.push(Definition::Enum(enm));
            }
            lexer::TokenType::KeywordOneof => {
                let oneof = munch_oneof(&mut iter)?;
                definitions.push(Definition::OneOf(oneof));
            }
            _ => {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::ExpectedServiceMessageEnumOrOneof,
                ));
            }
        }
        tok = match iter.next() {
            Some(tok) => tok?,
            None => {
                return Ok(Document {
                    base_url,
                    services,
                    definitions,
                });
            }
        }
    }
}

fn parse_error(line: usize, col: usize, type_: ParseErrorType) -> ParseError {
    ParseError { line, col, type_ }
}

fn next_not_eof(iter: &mut lexer::TokenIter<'_>) -> Result<lexer::Token, ParseError> {
    match iter.next() {
        Some(tok) => Ok(tok?),
        None => Err(ParseError {
            line: iter.line,
            col: iter.col,
            type_: ParseErrorType::UnexpectedEof,
        }),
    }
}

fn munch_literal_string(iter: &mut lexer::TokenIter<'_>) -> Result<CompactString, ParseError> {
    match next_not_eof(iter)?.type_ {
        lexer::TokenType::Literal(lexer::Literal::String(s)) => Ok(s.0),
        _ => Err(parse_error(
            iter.line,
            iter.col,
            ParseErrorType::ExpectedStringLiteral,
        )),
    }
}

// assumes ident "base_url" already chomped
// looking for "=" string ";"
fn munch_base_url(iter: &mut lexer::TokenIter<'_>) -> Result<CompactString, ParseError> {
    expect_next_equals(&mut *iter, lexer::TokenType::Equals)?;
    let base_url = munch_literal_string(&mut *iter)?;
    expect_next_equals(&mut *iter, lexer::TokenType::Semicolon)?;
    Ok(base_url)
}

// assumes "service" keyword already chomped
fn munch_service(iter: &mut lexer::TokenIter<'_>) -> Result<Service, ParseError> {
    let name = munch_ident(&mut *iter)?;
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    let mut procedures = vec![];
    loop {
        let tok = next_not_eof(iter)?;
        let verb = match tok.type_ {
            lexer::TokenType::Verb(v) => v,
            lexer::TokenType::RCurly => return Ok(Service { name, procedures }),
            _ => {
                return Err(parse_error(
                    iter.line,
                    iter.col,
                    ParseErrorType::ExpectedProcedureVerb,
                ))
            }
        };
        let name = munch_ident(&mut *iter)?;
        expect_next_equals(&mut *iter, lexer::TokenType::LParen)?;
        let line = iter.line;
        let col = iter.col;
        let tok = next_not_eof(iter)?;
        let input = match tok.type_ {
            lexer::TokenType::RParen => None,
            lexer::TokenType::Ident(i) => {
                expect_next_equals(&mut *iter, lexer::TokenType::RParen)?;
                Some(i)
            }
            _ => {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::ExpectedIdentOrRParen,
                ))
            }
        };
        let line = iter.line;
        let col = iter.col;
        let tok = next_not_eof(iter)?;
        let output = match tok.type_ {
            lexer::TokenType::Semicolon => None,
            lexer::TokenType::Arrow => Some(munch_ident(&mut *iter)?),
            _ => {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::ExpectedIdentOrSemicolon,
                ))
            }
        };
        expect_next_equals(&mut *iter, lexer::TokenType::Semicolon)?;
        procedures.push(Procedure {
            verb,
            name,
            input,
            output,
        });
    }
}

// assumes "[" already munched
fn munch_list_size(iter: &mut lexer::TokenIter<'_>) -> Result<Option<usize>, ParseError> {
    let line = iter.line;
    let col = iter.col;
    let tok = next_not_eof(iter)?;
    match tok.type_ {
        lexer::TokenType::RBracket => Ok(None),
        lexer::TokenType::Literal(lexer::Literal::DecimalNumber(d)) => {
            let sz = d
                .as_str()
                .parse()
                .map_err(|_| parse_error(line, col, ParseErrorType::IntegerParseError))?;
            expect_next_equals(iter, lexer::TokenType::RBracket)?;
            Ok(Some(sz))
        }
        _ => Err(parse_error(
            line,
            col,
            ParseErrorType::ExpectedRBracketOrNumber,
        )),
    }
}

fn munch_type(iter: &mut lexer::TokenIter<'_>) -> Result<Type, ParseError> {
    let mut arena = smallvec::SmallVec::<[TypeNode; 4]>::new();
    let line = iter.line;
    let col = iter.col;
    loop {
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::LBracket => arena.push(TypeNode::List(munch_list_size(&mut *iter)?)),
            lexer::TokenType::Ident(i) => {
                arena.push(TypeNode::TypeOrIdent(TypeOrIdent::Ident(i)));
                break;
            }
            lexer::TokenType::BaseType(t) => {
                arena.push(TypeNode::TypeOrIdent(TypeOrIdent::Type(t)));
                break;
            }
            _ => {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::ExpectedLBracketTypeOrIdent,
                ))
            }
        }
    }
    Ok(Type { arena })
}

fn munch_decimal_number<T: FromStr>(iter: &mut lexer::TokenIter<'_>) -> Result<T, ParseError> {
    let line = iter.line;
    let col = iter.col;
    let tok = next_not_eof(iter)?;
    match tok.type_ {
        lexer::TokenType::Literal(lexer::Literal::DecimalNumber(d)) => d
            .as_str()
            .parse()
            .map_err(|_| parse_error(line, col, ParseErrorType::IntegerParseError)),
        _ => {
            return Err(parse_error(
                line,
                col,
                ParseErrorType::ExpectedIntegerLiteral,
            ))
        }
    }
}

// assumes "message" keyword already chomped
fn munch_message(iter: &mut lexer::TokenIter<'_>) -> Result<Message, ParseError> {
    let name = munch_ident(&mut *iter)?.0;
    let mut definitions = vec![];
    let mut fields = vec![];
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    loop {
        let line = iter.line;
        let col = iter.col;
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::KeywordEnum => {
                definitions.push(Definition::Enum(munch_enum(&mut *iter)?))
            }
            lexer::TokenType::KeywordOneof => {
                definitions.push(Definition::OneOf(munch_oneof(&mut *iter)?))
            }
            lexer::TokenType::KeywordMessage => {
                definitions.push(Definition::Message(munch_message(&mut *iter)?))
            }
            lexer::TokenType::Ident(name) => {
                // ident@4?:
                expect_next_equals(&mut *iter, lexer::TokenType::AtSign)?;
                let field_number = munch_decimal_number(&mut *iter)?;
                let mut optional = false;
                let line = iter.line;
                let col = iter.col;
                let tok = next_not_eof(iter)?;
                match tok.type_ {
                    lexer::TokenType::QuestionMark => {
                        optional = true;
                        expect_next_equals(&mut *iter, lexer::TokenType::Colon)?;
                    }
                    lexer::TokenType::Colon => {}
                    _ => {
                        return Err(parse_error(
                            line,
                            col,
                            ParseErrorType::ExpectedColonOrQuestionMark,
                        ))
                    }
                }
                let type_ = munch_type(&mut *iter)?;
                expect_next_equals(&mut *iter, lexer::TokenType::Comma)?;
                fields.push(MessageField {
                    field_number,
                    optional,
                    type_,
                    name,
                });
            }
            lexer::TokenType::RCurly => {
                return Ok(Message {
                    name,
                    definitions,
                    fields,
                });
            }
            _ => {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::ExpectedEnumMessageOneOfOrIdent,
                ))
            }
        }
    }
}

fn munch_ident(iter: &mut lexer::TokenIter<'_>) -> Result<Ident, ParseError> {
    let line = iter.line;
    let col = iter.col;
    let tok = next_not_eof(iter)?;
    match tok.type_ {
        lexer::TokenType::Ident(i) => Ok(i),
        _ => {
            return Err(parse_error(line, col, ParseErrorType::ExpectedIdent));
        }
    }
}

// assumed "enum" keyword already chomped
fn munch_enum(iter: &mut lexer::TokenIter<'_>) -> Result<Enum, ParseError> {
    // munch ident
    let name = munch_ident(&mut *iter)?;
    // munch "{"
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    // munch { variant } [ "UNKNOWN" ] "}"
    let mut variants: Vec<EnumVariant> = vec![];
    let mut has_unknown = false;
    loop {
        let line = iter.line;
        let col = iter.col;
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::Ident(name) => {
                if name.as_str() == "UNKNOWN" {
                    if has_unknown {
                        return Err(parse_error(
                            line,
                            col,
                            ParseErrorType::DuplicatedEnumVariantUnknown,
                        ));
                    }
                    has_unknown = true;
                } else {
                    if has_unknown {
                        return Err(parse_error(
                            line,
                            col,
                            ParseErrorType::EnumVariantUnknownNotLast,
                        ));
                    } else {
                        expect_next_equals(&mut *iter, lexer::TokenType::Equals)?;
                        let line = iter.line;
                        let col = iter.col;
                        let tok = next_not_eof(iter)?;
                        let value = match tok.type_ {
                            lexer::TokenType::Literal(lit) => match lit {
                                lexer::Literal::DecimalNumber(n) => {
                                    n.as_str().parse::<u16>().map_err(|_| {
                                        parse_error(line, col, ParseErrorType::IntegerParseError)
                                    })?
                                }
                                lexer::Literal::BinaryNumber(n) => {
                                    u16::from_str_radix(n.as_str(), 2).map_err(|_| {
                                        parse_error(line, col, ParseErrorType::IntegerParseError)
                                    })?
                                }
                                lexer::Literal::HexNumber(n) => u16::from_str_radix(n.as_str(), 16)
                                    .map_err(|_| {
                                        parse_error(line, col, ParseErrorType::IntegerParseError)
                                    })?,
                                lexer::Literal::Float(_)
                                | lexer::Literal::String(_)
                                | lexer::Literal::EnumVariant(_)
                                | lexer::Literal::Bool(_) => {
                                    return Err(parse_error(
                                        line,
                                        col,
                                        ParseErrorType::ExpectedIntegerLiteral,
                                    ));
                                }
                            },
                            _ => {
                                return Err(parse_error(
                                    line,
                                    col,
                                    ParseErrorType::ExpectedIntegerLiteral,
                                ));
                            }
                        };
                        variants.push(EnumVariant { name, value });
                    };
                }
                expect_next_equals(&mut *iter, lexer::TokenType::Comma)?;
            }
            lexer::TokenType::RCurly => {
                return Ok(Enum {
                    name,
                    variants,
                    has_unknown,
                });
            }
            _ => {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::ExpectedIdentOrRCurly,
                ))
            }
        }
    }
}

// assumes "oneof" keyword already chomped
fn munch_oneof(iter: &mut lexer::TokenIter<'_>) -> Result<OneOf, ParseError> {
    let name = munch_ident(&mut *iter)?.0;
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    let mut definitions: Vec<Definition> = vec![];
    let mut variants: Vec<OneOfVariant> = vec![];
    loop {
        let line = iter.line;
        let col = iter.col;
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::RCurly => {
                return Ok(OneOf {
                    name,
                    definitions,
                    variants,
                })
            }
            lexer::TokenType::KeywordEnum => {
                let enm = munch_enum(iter)?;
                definitions.push(Definition::Enum(enm));
            }
            lexer::TokenType::KeywordOneof => {
                let oneof = munch_oneof(iter)?;
                definitions.push(Definition::OneOf(oneof));
            }
            lexer::TokenType::KeywordMessage => {
                let msg = munch_message(iter)?;
                definitions.push(Definition::Message(msg));
            }
            lexer::TokenType::Ident(i) => {
                let field_name = i.0;
                expect_next_equals(iter, lexer::TokenType::AtSign)?;
                let field_number = munch_decimal_number(iter)?;
                expect_next_equals(iter, lexer::TokenType::Colon)?;
                let type_ = munch_type(iter)?;
                expect_next_equals(iter, lexer::TokenType::Comma)?;
                variants.push(OneOfVariant {
                    field_name,
                    field_number,
                    type_,
                });
            }
            _ => {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::ExpectedEnumMessageOneOfOrIdent,
                ))
            }
        }
    }
}

// error if eof or next token not equal to input
fn expect_next_equals(
    iter: &mut lexer::TokenIter<'_>,
    type_: lexer::TokenType,
) -> Result<(), ParseError> {
    let line = iter.line;
    let col = iter.col;
    let tok = next_not_eof(iter)?;
    if tok.type_ == type_ {
        Ok(())
    } else {
        Err(parse_error(line, col, ParseErrorType::Expected(type_)))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    line: usize,
    col: usize,
    type_: ParseErrorType,
}

impl From<lexer::ParseError> for ParseError {
    fn from(e: lexer::ParseError) -> ParseError {
        ParseError {
            line: e.line,
            col: e.col,
            type_: ParseErrorType::InvalidSyntax(e.type_),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErrorType {
    ExpectedTypeOrIdent,
    ExpectedColonOrQuestionMark,
    ExpectedAtSignOrQuestionMark,
    InvalidSyntax(lexer::ParseErrorType),
    ExpectedRBracketOrNumber,
    DuplicateBaseUrl,
    ExpectedLBracketTypeOrIdent,
    ExpectedEnumMessageOneOfOrIdent,
    TopLevelIdentNotAtBeginningOfFile(Ident),
    UnknownTopLevelIdent(CompactString),
    DuplicatedEnumVariantUnknown,
    IntegerParseError,
    EnumVariantUnknownNotLast,
    Expected(lexer::TokenType),
    ExpectedIdent,
    ExpectedIdentOrRParen,
    ExpectedIdentOrSemicolon,
    ExpectedProcedureVerb,
    ExpectedArrowOrSemicolon,
    ExpectedStringLiteral,
    ExpectedIdentOrRCurly,
    ExpectedServiceMessageEnumOrOneof,
    ExpectedIntegerLiteral,
    CustomMessage(CompactString),
    UnexpectedEof,
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Failed to parse document:\n")?;
        write!(f, "line: {}\n", self.line + 1)?;
        write!(f, " col: {}\n", self.col + 1)?;
        match self.type_ {
            ParseErrorType::ExpectedTypeOrIdent => write!(f, "expected type or identifier")?,
            ParseErrorType::ExpectedColonOrQuestionMark => write!(f, "expected `:` or `?`")?,
            ParseErrorType::ExpectedAtSignOrQuestionMark => write!(f, "expected `@` or `?`")?,
            ParseErrorType::InvalidSyntax(ref e) => match e {
                lexer::ParseErrorType::UnexpectedEof => write!(f, "unexpected end of input")?,
                lexer::ParseErrorType::UnknownValue => write!(f, "unknown token")?,
                lexer::ParseErrorType::ParseLiteral(_) => {
                    write!(f, "failed to parse value literal")?
                }
            },
            ParseErrorType::ExpectedRBracketOrNumber => write!(f, "expected `]` or number")?,
            ParseErrorType::DuplicateBaseUrl => write!(f, "`base_url` must appear a single time")?,
            ParseErrorType::ExpectedLBracketTypeOrIdent => {
                write!(f, "expected `[`, type, or identifier")?
            }
            ParseErrorType::ExpectedEnumMessageOneOfOrIdent => write!(
                f,
                "expected keyword `enum`, keyword `message`, keyword `oneof`, or identifier"
            )?,
            ParseErrorType::TopLevelIdentNotAtBeginningOfFile(ref i) => write!(
                f,
                "top-level identifiers must appear at the top of the file; identifier `{}`",
                i.0,
            )?,
            ParseErrorType::UnknownTopLevelIdent(ref i) => {
                write!(f, "unknown or unsupported top-level identifier: {}", i)?
            }
            ParseErrorType::DuplicatedEnumVariantUnknown => {
                write!(f, "special `UNKNOWN` enum variant must appear only once")?
            }
            ParseErrorType::IntegerParseError => write!(f, "failed to parse integer")?,
            ParseErrorType::EnumVariantUnknownNotLast => {
                write!(f, "special `UNKNOWN` enum variant must appear last")?
            }
            ParseErrorType::Expected(ref tok) => write!(f, "expected {}", tok.debug_str())?,
            ParseErrorType::ExpectedIdent => write!(f, "expected an identifier")?,
            ParseErrorType::ExpectedIdentOrRParen => write!(f, "expected an identifier or `)`")?,
            ParseErrorType::ExpectedIdentOrSemicolon => write!(f, "expected identifier or `;`")?,
            ParseErrorType::ExpectedProcedureVerb => {
                write!(f, "expected a procedure verb `GET` or `POST`")?
            }
            ParseErrorType::ExpectedArrowOrSemicolon => write!(f, "expected `->` or `;`")?,
            ParseErrorType::ExpectedStringLiteral => write!(f, "expected a string literal")?,
            ParseErrorType::ExpectedIdentOrRCurly => write!(f, "expected identifier or `{{`")?,
            ParseErrorType::ExpectedServiceMessageEnumOrOneof => write!(
                f,
                "expected keyword `enum`, `oneof`, `message`, or`service`"
            )?,
            ParseErrorType::ExpectedIntegerLiteral => write!(f, "expected an integer literal")?,
            ParseErrorType::CustomMessage(ref s) => write!(f, "{}", s)?,
            ParseErrorType::UnexpectedEof => write!(f, "unexpected end of input")?,
        }
        Ok(())
    }
}

// impl From<lexer::ParseError> for ParseError {
//     fn from(e: lexer::ParseError) -> ParseError {
//         ParseError::InvalidSyntax(e)
//     }
// }

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Document {
    base_url: Option<CompactString>,
    pub services: Vec<Service>,
    pub definitions: Vec<Definition>,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Service {
    pub name: Ident,
    pub procedures: Vec<Procedure>,
}

impl Document {
    pub fn base_url(&self) -> Option<&str> {
        self.base_url.as_deref()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Procedure {
    pub verb: Verb,
    pub name: Ident,
    pub input: Option<Ident>,
    pub output: Option<Ident>,
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct Ident(pub(crate) CompactString);
impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Verb {
    Get,
    Post,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Definition {
    Enum(Enum),
    OneOf(OneOf),
    Message(Message),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Enum {
    pub name: Ident,
    pub variants: Vec<EnumVariant>,
    pub has_unknown: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: Ident,
    pub value: u16,
}

#[derive(Default, Debug, PartialEq, Eq)]
pub struct OneOf {
    pub name: CompactString,
    pub definitions: Vec<Definition>,
    pub variants: Vec<OneOfVariant>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OneOfVariant {
    field_name: CompactString,
    pub field_number: u16,
    pub type_: Type,
}

impl OneOfVariant {
    pub fn field_name(&self) -> &str {
        self.field_name.as_str()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeOrIdent {
    Type(BaseType),
    Ident(Ident),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeNode {
    List(Option<usize>),
    TypeOrIdent(TypeOrIdent),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Type {
    arena: smallvec::SmallVec<[TypeNode; 4]>,
}

#[derive(Clone, PartialEq, Eq, Copy, Debug)]
pub enum BaseType {
    Bool,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    String,
    Bytes,
    List,
    SizedList(usize),
    Void,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Message {
    pub name: CompactString,
    pub definitions: Vec<Definition>,
    pub fields: Vec<MessageField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MessageField {
    pub field_number: u16,
    pub type_: Type,
    pub name: Ident,
    pub optional: bool,
}
