use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use compact_str::CompactString;
pub mod lexer;

pub fn parse_document_from_str(inp: &str) -> Result<RawDocument, ParseError> {
    let mut iter = lexer::lex_document_iter(inp);
    let mut base_url = None;
    let Some(tok) = iter.next() else {
        return Ok(RawDocument {
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
        match tok.type_ {
            lexer::TokenType::Ident(ide) => {
                if ide.as_str() == "base_url" {
                    base_url = Some(munch_base_url(&mut iter)?);
                } else {
                    return Err(parse_error(
                        tok.line,
                        tok.span.0,
                        ParseErrorType::UnknownTopLevelIdent(ident(tok.line, tok.span.0, ide)),
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
                return Ok(RawDocument {
                    base_url,
                    services,
                    definitions,
                });
            }
        }
    }
    // parse the rest of the document
    let mut tld_names: HashSet<Ident> = HashSet::new();
    let doc = loop {
        match tok.type_ {
            lexer::TokenType::Ident(ide) => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::TopLevelIdentNotAtBeginningOfFile(ident(
                        tok.line, tok.span.0, ide,
                    )),
                ));
            }
            lexer::TokenType::KeywordService => {
                let line = iter.line;
                let col = iter.col;
                let svc = munch_service(&mut iter)?;
                if !tld_names.insert(svc.name.clone()) {
                    return Err(parse_error(
                        line,
                        col,
                        ParseErrorType::DuplicateIdentifier(svc.name),
                    ));
                }
                services.push(svc);
            }
            lexer::TokenType::KeywordMessage => {
                let msg = munch_message(&mut iter)?;
                if !tld_names.insert(msg.name.clone()) {
                    return Err(parse_error(
                        msg.name.line,
                        msg.name.col,
                        ParseErrorType::DuplicateIdentifier(msg.name),
                    ));
                }
                definitions.push(Definition::Message(msg));
            }
            lexer::TokenType::KeywordEnum => {
                let enm = munch_enum(&mut iter)?;
                if !tld_names.insert(enm.name.clone()) {
                    return Err(parse_error(
                        enm.name.line,
                        enm.name.col,
                        ParseErrorType::DuplicateIdentifier(enm.name),
                    ));
                }
                definitions.push(Definition::Enum(enm));
            }
            lexer::TokenType::KeywordOneof => {
                let oneof = munch_oneof(&mut iter)?;
                if !tld_names.insert(oneof.name.clone()) {
                    return Err(parse_error(
                        oneof.name.line,
                        oneof.name.col,
                        ParseErrorType::DuplicateIdentifier(oneof.name),
                    ));
                }
                definitions.push(Definition::OneOf(oneof));
            }
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::ExpectedServiceMessageEnumOrOneof,
                ));
            }
        }
        tok = match iter.next() {
            Some(tok) => tok?,
            None => {
                break RawDocument {
                    base_url,
                    services,
                    definitions,
                };
            }
        }
    };
    // ensure all service inputs and outputs refer to top-level messages
    let def_names: HashMap<&str, DefinitionType> =
        HashMap::from_iter(doc.definitions.iter().map(|def| (def.name(), def.type_())));
    for svc in doc.services.iter() {
        for prc in svc.procedures.iter() {
            if let Some(inp) = prc.input.as_ref() {
                match def_names.get(inp.as_str()).copied() {
                    Some(DefinitionType::Message) => {}
                    Some(type_) => {
                        return Err(parse_error(
                            inp.line,
                            inp.col,
                            ParseErrorType::ProcedureInputNotMessage(
                                svc.name.clone(),
                                prc.name.clone(),
                                inp.clone(),
                                type_,
                            ),
                        ))
                    }
                    None => {
                        return Err(parse_error(
                            inp.line,
                            inp.col,
                            ParseErrorType::UnknownIdent(inp.clone()),
                        ))
                    }
                }
            }
            if let Some(inp) = prc.output.as_ref() {
                match def_names.get(inp.as_str()).copied() {
                    Some(DefinitionType::Message) => {}
                    Some(type_) => {
                        return Err(parse_error(
                            inp.line,
                            inp.col,
                            ParseErrorType::ProcedureOutpuNotMessage(
                                svc.name.clone(),
                                prc.name.clone(),
                                inp.clone(),
                                type_,
                            ),
                        ))
                    }
                    None => {
                        return Err(parse_error(
                            inp.line,
                            inp.col,
                            ParseErrorType::UnknownIdent(inp.clone()),
                        ))
                    }
                }
            }
        }
    }
    // resolve all nested identifiers to include a namespace
    Ok(doc)
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
    let tok = next_not_eof(iter)?;
    match tok.type_ {
        lexer::TokenType::Literal(lexer::Literal::String(s)) => Ok(s.0),
        _ => Err(parse_error(
            tok.line,
            tok.span.0,
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
    let mut procedure_names = HashSet::new();
    loop {
        let tok = next_not_eof(iter)?;
        let verb = match tok.type_ {
            lexer::TokenType::Verb(v) => v,
            lexer::TokenType::RCurly => return Ok(Service { name, procedures }),
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::ExpectedProcedureVerb,
                ))
            }
        };
        let name = {
            let line = iter.line;
            let col = iter.col;
            let n = munch_ident(&mut *iter)?;
            if !procedure_names.insert(n.clone()) {
                return Err(parse_error(
                    line,
                    col,
                    ParseErrorType::DuplicateIdentifier(n),
                ));
            }
            n
        };
        expect_next_equals(&mut *iter, lexer::TokenType::LParen)?;
        let tok = next_not_eof(iter)?;
        let input = match tok.type_ {
            lexer::TokenType::RParen => None,
            lexer::TokenType::Ident(i) => {
                expect_next_equals(&mut *iter, lexer::TokenType::RParen)?;
                Some(ident(tok.line, tok.span.0, i))
            }
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::ExpectedIdentOrRParen,
                ))
            }
        };
        let tok = next_not_eof(iter)?;
        let output = match tok.type_ {
            lexer::TokenType::Semicolon => None,
            lexer::TokenType::Arrow => Some(munch_ident(&mut *iter)?),
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
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
    let tok = next_not_eof(iter)?;
    match tok.type_ {
        lexer::TokenType::RBracket => Ok(None),
        lexer::TokenType::Literal(lexer::Literal::DecimalNumber(d)) => {
            let sz = d.as_str().parse().map_err(|_| {
                parse_error(tok.line, tok.span.0, ParseErrorType::IntegerParseError)
            })?;
            expect_next_equals(iter, lexer::TokenType::RBracket)?;
            Ok(Some(sz))
        }
        _ => Err(parse_error(
            tok.line,
            tok.span.0,
            ParseErrorType::ExpectedRBracketOrNumber,
        )),
    }
}

fn munch_type(iter: &mut lexer::TokenIter<'_>) -> Result<Type, ParseError> {
    let mut arena = smallvec::SmallVec::<[TypeNode; 4]>::new();
    loop {
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::LBracket => arena.push(TypeNode::List(munch_list_size(&mut *iter)?)),
            lexer::TokenType::Ident(i) => {
                arena.push(TypeNode::TypeOrIdent(TypeOrIdent::Ident(ident(
                    tok.line, tok.span.0, i,
                ))));
                break;
            }
            lexer::TokenType::BaseType(t) => {
                arena.push(TypeNode::TypeOrIdent(TypeOrIdent::Type(t)));
                break;
            }
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::ExpectedLBracketTypeOrIdent,
                ))
            }
        }
    }
    Ok(Type { arena })
}

struct WithPosition<T> {
    line: usize,
    span: (usize, usize),
    val: T,
}
fn munch_decimal_number<T: FromStr>(
    iter: &mut lexer::TokenIter<'_>,
) -> Result<WithPosition<T>, ParseError> {
    let tok = next_not_eof(iter)?;
    match tok.type_ {
        lexer::TokenType::Literal(lexer::Literal::DecimalNumber(d)) => Ok(WithPosition {
            line: tok.line,
            span: tok.span,
            val: d.as_str().parse().map_err(|_| {
                parse_error(tok.line, tok.span.0, ParseErrorType::IntegerParseError)
            })?,
        }),
        _ => {
            return Err(parse_error(
                tok.line,
                tok.span.0,
                ParseErrorType::ExpectedIntegerLiteral,
            ))
        }
    }
}

// assumes "message" keyword already chomped
fn munch_message(iter: &mut lexer::TokenIter<'_>) -> Result<Message, ParseError> {
    let name = munch_ident(&mut *iter)?;
    let mut definitions = vec![];
    let mut fields = vec![];
    // for ensuring all names and numbers are unique
    let mut field_names = HashSet::new();
    let mut field_numbers = HashSet::new();
    let mut nested_identifier_names = HashSet::new();

    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    loop {
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::KeywordEnum => {
                let enm = munch_enum(&mut *iter)?;
                if !nested_identifier_names.insert(enm.name.clone()) {
                    return Err(parse_error(
                        enm.name.line,
                        enm.name.col,
                        ParseErrorType::DuplicateIdentifier(enm.name),
                    ));
                }
                definitions.push(Definition::Enum(enm));
            }
            lexer::TokenType::KeywordOneof => {
                let oneof = munch_oneof(&mut *iter)?;
                if !nested_identifier_names.insert(oneof.name.clone()) {
                    return Err(parse_error(
                        oneof.name.line,
                        oneof.name.col,
                        ParseErrorType::DuplicateIdentifier(oneof.name),
                    ));
                }
                definitions.push(Definition::OneOf(oneof));
            }
            lexer::TokenType::KeywordMessage => {
                let msg = munch_message(&mut *iter)?;
                if !nested_identifier_names.insert(msg.name.clone()) {
                    return Err(parse_error(
                        msg.name.line,
                        msg.name.col,
                        ParseErrorType::DuplicateIdentifier(msg.name),
                    ));
                }
                definitions.push(Definition::Message(msg))
            }
            lexer::TokenType::Ident(name) => {
                // ident@4?:
                // check duplicated field name
                if !field_names.insert(name.clone()) {
                    return Err(parse_error(
                        tok.line,
                        tok.span.0,
                        ParseErrorType::DuplicateFieldName(ident(tok.line, tok.span.0, name)),
                    ));
                }
                expect_next_equals(&mut *iter, lexer::TokenType::AtSign)?;
                let field_number = {
                    let munched = munch_decimal_number(&mut *iter)?;
                    let num = munched.val;
                    // check duplicated field number
                    if !field_numbers.insert(num) {
                        return Err(parse_error(
                            munched.line,
                            munched.span.0,
                            ParseErrorType::DuplicateFieldNumber(num),
                        ));
                    }
                    num
                };
                let mut optional = false;
                let tok = next_not_eof(iter)?;
                match tok.type_ {
                    lexer::TokenType::QuestionMark => {
                        optional = true;
                        expect_next_equals(&mut *iter, lexer::TokenType::Colon)?;
                    }
                    lexer::TokenType::Colon => {}
                    _ => {
                        return Err(parse_error(
                            tok.line,
                            tok.span.0,
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
                    name: ident(tok.line, tok.span.0, name),
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
                    tok.line,
                    tok.span.0,
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
        lexer::TokenType::Ident(i) => Ok(Ident {
            value: i,
            line: tok.line,
            col: tok.span.0,
        }),
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
    let mut variant_names = HashSet::new();
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
                        if !variant_names.insert(name.clone()) {
                            return Err(parse_error(
                                line,
                                col,
                                ParseErrorType::DuplicateIdentifier(ident(
                                    tok.line, tok.span.0, name,
                                )),
                            ));
                        }
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
                        variants.push(EnumVariant {
                            name: ident(tok.line, tok.span.0, name),
                            value,
                        });
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
    let name = munch_ident(&mut *iter)?;
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    let mut definitions: Vec<Definition> = vec![];
    let mut variants: Vec<OneOfVariant> = vec![];
    let mut nested_identifier_names = HashSet::new();
    let mut field_names = HashSet::new();
    let mut field_numbers = HashSet::new();
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
                let line = iter.line;
                let col = iter.col;
                let enm = munch_enum(iter)?;
                if !nested_identifier_names.insert(enm.name.clone()) {
                    return Err(parse_error(
                        line,
                        col,
                        ParseErrorType::DuplicateIdentifier(enm.name),
                    ));
                }
                definitions.push(Definition::Enum(enm));
            }
            lexer::TokenType::KeywordOneof => {
                let line = iter.line;
                let col = iter.col;
                let oneof = munch_oneof(iter)?;
                if !nested_identifier_names.insert(oneof.name.clone()) {
                    return Err(parse_error(
                        line,
                        col,
                        ParseErrorType::DuplicateIdentifier(oneof.name),
                    ));
                }
                definitions.push(Definition::OneOf(oneof));
            }
            lexer::TokenType::KeywordMessage => {
                let line = iter.line;
                let col = iter.col;
                let msg = munch_message(iter)?;
                if !nested_identifier_names.insert(msg.name.clone()) {
                    return Err(parse_error(
                        line,
                        col,
                        ParseErrorType::DuplicateIdentifier(msg.name),
                    ));
                }
                definitions.push(Definition::Message(msg));
            }
            lexer::TokenType::Ident(field_name) => {
                if !field_names.insert(field_name.clone()) {
                    return Err(parse_error(
                        line,
                        col,
                        ParseErrorType::DuplicateFieldName(ident(tok.line, tok.span.0, field_name)),
                    ));
                }
                expect_next_equals(iter, lexer::TokenType::AtSign)?;
                let field_number = {
                    let munched = munch_decimal_number(iter)?;
                    let num = munched.val;
                    if !field_numbers.insert(num) {
                        return Err(parse_error(
                            munched.line,
                            munched.span.0,
                            ParseErrorType::DuplicateFieldNumber(num),
                        ));
                    }
                    num
                };
                expect_next_equals(iter, lexer::TokenType::Colon)?;
                let type_ = munch_type(iter)?;
                expect_next_equals(iter, lexer::TokenType::Comma)?;
                variants.push(OneOfVariant {
                    field_name: ident(tok.line, tok.span.0, field_name),
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
    DuplicateFieldName(Ident),
    DuplicateFieldNumber(u16),
    DuplicateIdentifier(Ident),
    ExpectedColonOrQuestionMark,
    ExpectedAtSignOrQuestionMark,
    InvalidSyntax(lexer::ParseErrorType),
    ExpectedRBracketOrNumber,
    DuplicateBaseUrl,
    ExpectedLBracketTypeOrIdent,
    ExpectedEnumMessageOneOfOrIdent,
    TopLevelIdentNotAtBeginningOfFile(Ident),
    UnknownTopLevelIdent(Ident),
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
    // (service name,  procedure name, input name, procedure input type)
    ProcedureInputNotMessage(Ident, Ident, Ident, DefinitionType),
    // (service name,  procedure name, procedure output type)
    ProcedureOutpuNotMessage(Ident, Ident, Ident, DefinitionType),
    UnknownIdent(Ident),
    UnexpectedEof,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DefinitionType {
    Message,
    Enum,
    OneOf,
}

impl DefinitionType {
    fn as_str(&self) -> &'static str {
        match self {
            DefinitionType::Message => "message",
            DefinitionType::Enum => "enum",
            DefinitionType::OneOf => "oneof",
        }
    }
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
                i.value,
            )?,
            ParseErrorType::UnknownTopLevelIdent(ref i) => write!(
                f,
                "unknown or unsupported top-level identifier: {}",
                i.value
            )?,
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
            ParseErrorType::DuplicateFieldName(ref i) => {
                write!(f, "duplicate field name `{}`", i.value)?
            }
            ParseErrorType::DuplicateFieldNumber(n) => write!(f, "duplicate field number `{}`", n)?,
            ParseErrorType::DuplicateIdentifier(ref i) => {
                write!(f, "duplicate identifier name `{}`", i.value)?;
            }
            ParseErrorType::ProcedureInputNotMessage(ref svc, ref prc, ref inp, typ) => {
                write!(f, "procedure input types must be of type `message`: service `{}` procedure `{}` input `{}` is of type {}", svc.as_str(), prc.as_str(), inp.as_str(), typ.as_str())?;
            }
            ParseErrorType::ProcedureOutpuNotMessage(ref svc, ref prc, ref outpt, typ) => {
                write!(f, "procedure output types must be of type `message`: service `{}` procedure `{}` output `{}` is of type {}", svc.as_str(), prc.as_str(), outpt.as_str(), typ.as_str())?;
            }
            ParseErrorType::UnknownIdent(ref i) => {
                write!(f, "could not find definition for ident `{}`", i.value)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct RawDocument {
    base_url: Option<CompactString>,
    pub services: Vec<Service>,
    pub definitions: Vec<Definition>,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Service {
    pub name: Ident,
    pub procedures: Vec<Procedure>,
}

impl RawDocument {
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

// #[derive(Clone, PartialEq, Eq, Default, Debug, Hash)]
// pub struct Ident(pub(crate) CompactString);
impl Ident {
    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
}

#[derive(Clone, PartialEq, Eq, Default, Debug, Hash)]
pub struct Ident {
    pub(crate) value: CompactString,
    pub(crate) line: usize,
    pub(crate) col: usize,
}

fn ident(line: usize, col: usize, value: CompactString) -> Ident {
    Ident { value, line, col }
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

impl Definition {
    fn name(&self) -> &str {
        match self {
            Definition::Enum(inner) => inner.name.as_str(),
            Definition::OneOf(inner) => inner.name.as_str(),
            Definition::Message(inner) => inner.name.as_str(),
        }
    }

    fn type_(&self) -> DefinitionType {
        match self {
            Definition::Enum(_) => DefinitionType::Enum,
            Definition::OneOf(_) => DefinitionType::OneOf,
            Definition::Message(_) => DefinitionType::Message,
        }
    }
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
    pub name: Ident,
    pub definitions: Vec<Definition>,
    pub variants: Vec<OneOfVariant>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OneOfVariant {
    field_name: Ident,
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
    Null,
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Message {
    pub name: Ident,
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
