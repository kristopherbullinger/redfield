use std::str::FromStr;

use compact_str::CompactString;
// use std::iter::Peekable;
pub mod lexer;

pub fn parse_document_from_str(inp: &str) -> Result<Document, ParseError> {
    let mut iter = lexer::lex_document_iter(inp).peekable();
    let iter_ref = iter.by_ref();
    let mut peekable = iter_ref.peekable();
    let mut base_url = None;
    // parse top-level identifiers: [ "base_url", ]
    let Some(tok) = peekable.next() else {
        return Ok(Document {
            base_url: None,
            services: vec![],
            definitions: vec![],
        });
    };

    let tok = tok?;
    let mut definitions = vec![];
    let mut services = vec![];
    match tok.type_ {
        lexer::TokenType::Ident(ide) => {
            if ide.as_str() == "base_url" {
                base_url = Some(munch_base_url(peekable.by_ref())?);
            } else {
                return Err(ParseError::UnknownTopLevelIdent(ide.0));
            }
        }
        mut token_type => loop {
            match token_type {
                lexer::TokenType::KeywordService => {
                    let svc = munch_service(peekable.by_ref())?;
                    services.push(svc);
                }
                lexer::TokenType::KeywordMessage => {
                    let msg = munch_message(peekable.by_ref())?;
                    definitions.push(Definition::Message(msg));
                }
                lexer::TokenType::KeywordEnum => {
                    let enm = munch_enum(peekable.by_ref())?;
                    definitions.push(Definition::Enum(enm));
                }
                lexer::TokenType::KeywordOneof => {
                    let oneof = munch_oneof(peekable.by_ref())?;
                    definitions.push(Definition::OneOf(oneof));
                }
                lexer::TokenType::Ident(i) => {
                    return Err(ParseError::TopLevelIdentNotAtBeginningOfFile(i));
                }
                _ => {
                    return Err(ParseError::ExpectedOneOf(vec![
                        lexer::TokenType::KeywordService,
                        lexer::TokenType::KeywordMessage,
                        lexer::TokenType::KeywordEnum,
                        lexer::TokenType::KeywordOneof,
                    ]));
                }
            }
            token_type = match peekable.next() {
                Some(tok) => tok?.type_,
                None => break,
            }
        },
    }
    Ok(Document {
        base_url,
        services,
        definitions,
    })
}

fn munch_literal_string(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<CompactString, ParseError> {
    match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
        lexer::TokenType::Literal(lexer::Literal::String(s)) => Ok(s.0),
        _ => Err(ParseError::ExpectedStringLiteral),
    }
}
// assumes ident "base_url" already chomped
// looking for "=" string ";"
fn munch_base_url(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<CompactString, ParseError> {
    expect_next_equals(iter.by_ref(), lexer::TokenType::Equals)?;
    let base_url = munch_literal_string(iter.by_ref())?;
    expect_next_equals(iter.by_ref(), lexer::TokenType::Semicolon)?;
    Ok(base_url)
}

// assumes "service" keyword already chomped
fn munch_service(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<Service, ParseError> {
    let name = munch_ident(iter.by_ref())?;
    expect_next_equals(iter.by_ref(), lexer::TokenType::LCurly)?;
    let mut procedures = vec![];
    loop {
        let tok = iter.next().ok_or(ParseError::UnexpectedEof)??;
        let verb = match tok.type_ {
            lexer::TokenType::Verb(v) => v,
            lexer::TokenType::RCurly => return Ok(Service { name, procedures }),
            _ => return Err(ParseError::ExpectedProcedureVerb),
        };
        let name = munch_ident(iter.by_ref())?;
        expect_next_equals(iter.by_ref(), lexer::TokenType::LParen)?;
        let input = match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
            lexer::TokenType::RParen => None,
            lexer::TokenType::Ident(i) => {
                expect_next_equals(iter.by_ref(), lexer::TokenType::RParen)?;
                Some(i)
            }
            _ => return Err(ParseError::ExpectedRParenOrIdent),
        };
        let output = match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
            lexer::TokenType::Semicolon => None,
            lexer::TokenType::Arrow => Some(munch_ident(iter.by_ref())?),
            _ => return Err(ParseError::ExpectedIdentOrSemicolon),
        };
        expect_next_equals(iter.by_ref(), lexer::TokenType::Semicolon)?;
        procedures.push(Procedure {
            verb,
            name,
            input,
            output,
        });
    }
}
fn munch_list_size(
    _iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<Option<usize>, ParseError> {
    todo!()
}

fn munch_type_(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<Type_, ParseError> {
    let mut arena = smallvec::SmallVec::<[TypeNode; 4]>::new();
    loop {
        let tok = iter.next().ok_or(ParseError::UnexpectedEof)??;
        match tok.type_ {
            lexer::TokenType::LBracket => {
                arena.push(TypeNode::List(munch_list_size(iter.by_ref())?))
            }
            lexer::TokenType::Ident(i) => {
                arena.push(TypeNode::TypeOrIdent(TypeOrIdent::Ident(i)));
                break;
            }
            lexer::TokenType::BaseType(t) => {
                arena.push(TypeNode::TypeOrIdent(TypeOrIdent::Type(t)));
                break;
            }
            _ => return Err(ParseError::ExpectedBracketTypeOrIdent),
        }
    }
    Ok(Type_ { arena })
}

fn munch_decimal_number<T: FromStr>(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<T, ParseError> {
    let tok = iter.next().ok_or(ParseError::UnexpectedEof)??;
    match tok.type_ {
        lexer::TokenType::Literal(lexer::Literal::DecimalNumber(d)) => d
            .as_str()
            .parse()
            .map_err(|_| ParseError::IntegerParseError),
        _ => return Err(ParseError::ExpectedIntegerLiteral),
    }
}

// assumes "message" keyword already chomped
fn munch_message(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<Message, ParseError> {
    let _name = munch_ident(iter.by_ref())?;
    let mut definitions = vec![];
    let mut fields = vec![];
    expect_next_equals(iter.by_ref(), lexer::TokenType::LCurly)?;
    loop {
        match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
            lexer::TokenType::KeywordEnum => {
                definitions.push(Definition::Enum(munch_enum(iter.by_ref())?))
            }
            lexer::TokenType::KeywordOneof => {
                definitions.push(Definition::OneOf(munch_oneof(iter.by_ref())?))
            }
            lexer::TokenType::KeywordMessage => {
                definitions.push(Definition::Message(munch_message(iter.by_ref())?))
            }
            lexer::TokenType::Ident(name) => {
                // ident@4?:
                expect_next_equals(iter.by_ref(), lexer::TokenType::AtSign)?;
                let field_number = munch_decimal_number(iter.by_ref())?;
                let mut optional = false;
                match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
                    lexer::TokenType::QuestionMark => {
                        optional = true;
                        expect_next_equals(iter.by_ref(), lexer::TokenType::Colon)?;
                    }
                    lexer::TokenType::Colon => {}
                    _ => return Err(ParseError::ExpectedColonOrQuestionMark),
                }
                let type_ = munch_type_(iter.by_ref())?;
                expect_next_equals(iter.by_ref(), lexer::TokenType::Comma)?;
                fields.push(MessageField {
                    field_number,
                    optional,
                    type_,
                    name,
                });
            }
            lexer::TokenType::RCurly => {
                return Ok(Message {
                    definitions,
                    fields,
                });
            }
            _ => return Err(ParseError::ExpectedEnumMessageOneOfOrIdent),
        }
    }
}

// fn munch_type(
//     mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
// ) -> Result<Type, ParseError> {
//     Ok(match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
//         lexer::TokenType::QuestionMark => {
//             // ?
//             match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
//                 lexer::TokenType::LBracket => {
//                     match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
//                         // ?[
//                         lexer::TokenType::Literal(lexer::Literal::DecimalNumber(n)) => {
//                             // ?[3
//                             let sz = n
//                                 .as_str()
//                                 .parse::<usize>()
//                                 .map_err(|_| ParseError::IntegerParseError)?;
//                             expect_next_equals(iter.by_ref(), lexer::TokenType::RBracket)?;
//                             match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
//                                 lexer::TokenType::Ident(i) => Type {
//                                     optional: true,
//                                     list_size: Some(ListSize::Sized(sz)),
//                                     type_or_ident: TypeOrIdent::Ident(i),
//                                 }, // ?[3]MyType
//                                 lexer::TokenType::BaseType(t) => Type {
//                                     optional: true,
//                                     list_size: Some(ListSize::Sized(sz)),
//                                     type_or_ident: TypeOrIdent::Type(t),
//                                 }, // ?[3]u16
//                                 _ => return Err(ParseError::ExpectedTypeOrIdent),
//                             }
//                         }
//                         lexer::TokenType::RBracket => {
//                             // ?[]
//                             match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
//                                 lexer::TokenType::Ident(i) => Type {
//                                     optional: true,
//                                     list_size: Some(ListSize::Unsized),
//                                     type_or_ident: TypeOrIdent::Ident(i),
//                                 }, // ?[]MyType
//                                 lexer::TokenType::BaseType(t) => Type {
//                                     optional: true,
//                                     list_size: Some(ListSize::Unsized),
//                                     type_or_ident: TypeOrIdent::Type(t),
//                                 }, // ?[]u16
//                                 _ => return Err(ParseError::ExpectedTypeOrIdent),
//                             }
//                         }
//                         _ => return Err(ParseError::ExpectedRBracketOrNumber),
//                     }
//                 } // ?[
//                 lexer::TokenType::Ident(i) => Type {
//                     optional: true,
//                     list_size: None,
//                     type_or_ident: TypeOrIdent::Ident(i),
//                 }, // ?MyType
//                 lexer::TokenType::BaseType(t) => Type {
//                     optional: true,
//                     list_size: None,
//                     type_or_ident: TypeOrIdent::Type(t),
//                 }, // ?u16
//                 _ => return Err(ParseError::ExpectedBracketTypeOrIdent),
//             }
//         }
//         lexer::TokenType::LBracket => match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
//             // [
//             lexer::TokenType::Literal(lexer::Literal::DecimalNumber(n)) => {
//                 // [3
//                 let sz = n
//                     .as_str()
//                     .parse::<usize>()
//                     .map_err(|_| ParseError::IntegerParseError)?;
//                 expect_next_equals(iter.by_ref(), lexer::TokenType::RBracket)?;
//                 match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
//                     lexer::TokenType::Ident(i) => Type {
//                         optional: false,
//                         list_size: Some(ListSize::Sized(sz)),
//                         type_or_ident: TypeOrIdent::Ident(i),
//                     }, // [3]MyType
//                     lexer::TokenType::BaseType(t) => Type {
//                         optional: false,
//                         list_size: Some(ListSize::Sized(sz)),
//                         type_or_ident: TypeOrIdent::Type(t),
//                     }, // [3]u16
//                     _ => return Err(ParseError::ExpectedTypeOrIdent),
//                 }
//             }
//             lexer::TokenType::RBracket => {
//                 // []
//                 match iter.next().ok_or(ParseError::UnexpectedEof)??.type_ {
//                     lexer::TokenType::Ident(i) => Type {
//                         optional: false,
//                         list_size: Some(ListSize::Unsized),
//                         type_or_ident: TypeOrIdent::Ident(i),
//                     }, // []MyType
//                     lexer::TokenType::BaseType(t) => Type {
//                         optional: false,
//                         list_size: Some(ListSize::Unsized),
//                         type_or_ident: TypeOrIdent::Type(t),
//                     }, // []u16
//                     _ => return Err(ParseError::ExpectedTypeOrIdent),
//                 }
//             }
//             _ => return Err(ParseError::ExpectedRBracketOrNumber),
//         },
//         lexer::TokenType::Ident(i) => Type {
//             optional: false,
//             list_size: None,
//             type_or_ident: TypeOrIdent::Ident(i),
//         }, // MyType
//         lexer::TokenType::BaseType(t) => Type {
//             optional: false,
//             list_size: None,
//             type_or_ident: TypeOrIdent::Type(t),
//         }, // u16
//         _ => return Err(ParseError::ExpectedQuestionMarkBracketTypeOrIdent),
//     })
// }

fn munch_ident(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<Ident, ParseError> {
    let tok = iter.next().ok_or(ParseError::UnexpectedEof)?;
    match tok?.type_ {
        lexer::TokenType::Ident(i) => Ok(i),
        _ => {
            return Err(ParseError::ExpectedIdent);
        }
    }
}

// assumed "enum" keyword already chomped
fn munch_enum(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<Enum, ParseError> {
    // munch ident
    let name = munch_ident(iter.by_ref())?;
    // munch "{"
    expect_next_equals(iter.by_ref(), lexer::TokenType::LCurly)?;
    // munch { variant } [ "UNKNOWN" ] "}"
    let mut variants: Vec<EnumVariant> = vec![];
    let mut has_unknown = false;
    loop {
        let tok = iter.next().ok_or(ParseError::UnexpectedEof)?;
        match tok?.type_ {
            lexer::TokenType::Ident(name) => {
                if name.as_str() == "UNKNOWN" {
                    if has_unknown {
                        return Err(ParseError::DuplicatedEnumVariantUnknown);
                    }
                    has_unknown = true;
                } else {
                    if has_unknown {
                        return Err(ParseError::EnumVariantUnknownNotLast);
                    } else {
                        expect_next_equals(iter.by_ref(), lexer::TokenType::Equals)?;
                        let tok = iter.next().ok_or(ParseError::UnexpectedEof)?;
                        let value = match tok?.type_ {
                            lexer::TokenType::Literal(lit) => match lit {
                                lexer::Literal::DecimalNumber(n) => n
                                    .as_str()
                                    .parse::<u16>()
                                    .map_err(|_| ParseError::IntegerParseError)?,
                                lexer::Literal::BinaryNumber(n) => {
                                    u16::from_str_radix(n.as_str(), 2)
                                        .map_err(|_| ParseError::IntegerParseError)?
                                }
                                lexer::Literal::HexNumber(n) => u16::from_str_radix(n.as_str(), 16)
                                    .map_err(|_| ParseError::IntegerParseError)?,
                                lexer::Literal::Float(_)
                                | lexer::Literal::String(_)
                                | lexer::Literal::EnumVariant(_)
                                | lexer::Literal::Bool(_) => {
                                    return Err(ParseError::ExpectedIntegerLiteral);
                                }
                            },
                            _ => {
                                return Err(ParseError::ExpectedIntegerLiteral);
                            }
                        };
                        variants.push(EnumVariant { name, value });
                    };
                }
                expect_next_equals(iter.by_ref(), lexer::TokenType::Comma)?;
            }
            lexer::TokenType::RCurly => {
                return Ok(Enum {
                    name,
                    variants,
                    has_unknown,
                });
            }
            _ => return Err(ParseError::ExpectedIdentOrRCurly),
        }
    }
}

// assumes "oneof" keyword already chomped
fn munch_oneof(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
) -> Result<OneOf, ParseError> {
    let _name = munch_ident(iter.by_ref())?;
    expect_next_equals(iter.by_ref(), lexer::TokenType::LCurly)?;
    let _definitions: Vec<Definition> = vec![];
    let _variants: Vec<OneOfVariant> = vec![];
    loop {
        let _tok = iter.next().ok_or(ParseError::UnexpectedEof)?;
    }
}

// error if eof or next token not equal to input
fn expect_next_equals(
    mut iter: impl Iterator<Item = Result<lexer::Token, lexer::ParseError>>,
    type_: lexer::TokenType,
) -> Result<(), ParseError> {
    match iter.next() {
        None => Err(ParseError::Expected(type_)),
        Some(tok) => {
            if tok?.type_ == type_ {
                Ok(())
            } else {
                Err(ParseError::Expected(type_))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ParseError {
    ExpectedTypeOrIdent,
    ExpectedColonOrQuestionMark,
    ExpectedAtSignOrQuestionMark,
    InvalidSyntax(lexer::ParseError),
    ExpectedRBracketOrNumber,
    DuplicateBaseUrl,
    ExpectedBracketTypeOrIdent,
    ExpectedQuestionMarkBracketTypeOrIdent,
    ExpectedEnumMessageOneOfOrIdent,
    TopLevelIdentNotAtBeginningOfFile(Ident),
    UnknownTopLevelIdent(CompactString),
    DuplicatedEnumVariantUnknown,
    IntegerParseError,
    EnumVariantUnknownNotLast,
    Expected(lexer::TokenType),
    ExpectedIdent,
    ExpectedRParenOrIdent,
    ExpectedIdentOrSemicolon,
    ExpectedProcedureVerb,
    ExpectedDecimalIntegerLiteral,
    ExpectedArrowOrSemicolon,
    ExpectedStringLiteral,
    ExpectedIdentOrRCurly,
    ExpectedOneOf(Vec<lexer::TokenType>),
    ExpectedIntegerLiteral,
    CustomMessage(CompactString),
    UnexpectedEof,
}

impl From<lexer::ParseError> for ParseError {
    fn from(e: lexer::ParseError) -> ParseError {
        ParseError::InvalidSyntax(e)
    }
}
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
pub struct Type_ {
    arena: smallvec::SmallVec<[TypeNode; 4]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Type {
    optional: bool,
    // None indicates not a list
    list_size: Option<ListSize>,
    type_or_ident: TypeOrIdent,
}

#[derive(Debug, PartialEq, Eq)]
enum ListSize {
    Sized(usize),
    Unsized,
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
    pub definitions: Vec<Definition>,
    pub fields: Vec<MessageField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MessageField {
    pub field_number: u16,
    pub type_: Type_,
    pub name: Ident,
    pub optional: bool,
    // TODO: allow setting default values in the schema?
    // pub default_value: Option<CompactString>,
}

// impl MessageField {
//     pub fn default_value(&self) -> Option<&str> {
//         self.default_value.as_deref()
//     }
// }
