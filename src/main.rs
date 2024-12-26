fn main() {
    let f = std::fs::read_to_string("./schema.rdf").unwrap();
    let tokens = lex_document(f.as_str()).unwrap();
    println!("{:?}", tokens);
}
use std::io::Read;

#[derive(Debug)]
struct Document {
    services: Vec<Service>,
}

#[derive(Debug)]
struct Service {
    name: Ident,
    base_url: Option<String>,
    endpoints: Vec<Endpoint>,
    definitions: Vec<Definition>,
}

#[derive(Debug)]
struct Endpoint {
    verb: Verb,
    name: Ident,
    input: Option<Ident>,
    output: Option<Ident>,
}

#[derive(Debug)]
struct Ident(String);

#[derive(Clone, Debug)]
enum Verb {
    Get,
    Post,
}

#[derive(Debug)]
enum Definition {
    Enum(Enum),
    OneOf(OneOf),
    Message(Message),
}

#[derive(Debug)]
struct Enum {
    variants: Vec<EnumVariant>,
    has_unknown: bool,
}

#[derive(Debug)]
struct EnumVariant {
    name: Ident,
    value: u16,
}

#[derive(Debug)]
struct OneOf {
    nested_definitions: Vec<NestedDefinition>,
    variants: Vec<OneOfVariant>,
}

#[derive(Debug)]
enum NestedDefinition {
    OneOf(OneOf),
    Enum(Enum),
    Message(Message),
}

#[derive(Debug)]
struct OneOfVariant {
    field_number: u16,
    type_: TypeOrIdent,
}

#[derive(Debug)]
enum TypeOrIdent {
    Type(BaseType),
    Ident(Ident),
}

#[derive(Clone, Copy, Debug)]
enum BaseType {
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
}

#[derive(Debug)]
struct Message {
    nested_definitions: Vec<NestedDefinition>,
    fields: Vec<MessageField>,
}

#[derive(Debug)]
struct MessageField {
    field_number: u16,
    type_: TypeOrIdent,
    name: Ident,
    optional: bool,
    default_value: Option<String>,
}

#[derive(Debug)]
struct Token {
    line: usize,
    span: (usize, usize),
    type_: TokenType,
}

#[derive(Debug, Clone)]
enum Literal {
    DecimalNumber(String),
    BinaryNumber(String),
    HexNumber(String),
    String(String),
    Float(String),
    Bool(bool),
    List(Vec<Literal>),
}

#[derive(Clone, Debug)]
enum TokenType {
    LParen,
    RParen,
    LBracket,
    RBracket,
    // Brackets,
    LCurly,
    RCurly,
    QuestionMark,
    Comma,
    Colon,
    Semicolon,
    Equals,
    Ident(String),
    BaseType(BaseType),
    KeywordEnum,
    KeywordMessage,
    KeywordOneof,
    KeywordService,
    AtSign,
    // Number(String),
    Verb(Verb),
    True,
    False,
    Arrow,
    Literal(Literal),
    Eof,
}

#[derive(Debug)]
struct ParseError {
    line: usize,
    col: usize,
    type_: ParseErrorType,
}
#[derive(Debug)]
enum ParseErrorType {
    UnexpectedEof,
    UnknownValue,
    ParseLiteral(ParseLiteralError),
}
static KEYWORDS_TOKENS: &[(&str, TokenType)] = &[
    ("service", TokenType::KeywordService),
    ("enum", TokenType::KeywordEnum),
    ("oneof", TokenType::KeywordOneof),
    ("message", TokenType::KeywordMessage),
    ("true", TokenType::True),
    ("false", TokenType::False),
    ("GET", TokenType::Verb(Verb::Get)),
    ("POST", TokenType::Verb(Verb::Post)),
];
static BASE_TYPES_TOKENS: &[(&str, TokenType)] = &[
    ("bool", TokenType::BaseType(BaseType::Bool)),
    ("u8", TokenType::BaseType(BaseType::U8)),
    ("i8", TokenType::BaseType(BaseType::I8)),
    ("u16", TokenType::BaseType(BaseType::U16)),
    ("i16", TokenType::BaseType(BaseType::I16)),
    ("u32", TokenType::BaseType(BaseType::U32)),
    ("i32", TokenType::BaseType(BaseType::I32)),
    ("u64", TokenType::BaseType(BaseType::U64)),
    ("i64", TokenType::BaseType(BaseType::I64)),
    ("string", TokenType::BaseType(BaseType::String)),
    ("bytes", TokenType::BaseType(BaseType::Bytes)),
];
fn lex_document(mut inp: &str) -> Result<Vec<Token>, ParseError> {
    let mut c = 0;
    let mut line = 0;
    let mut tokens: Vec<Token> = vec![];
    loop {
        dbg!(inp.len());
        dbg!(&tokens);
        let _ = std::io::stdin().read_line(&mut String::new());
        match *inp.as_bytes() {
            [] => break,
            [b, ..] => {
                // keywords
                println!("trying single char...");
                match b {
                    b'\n' => {
                        line += 1;
                        c = 0;
                        inp = &inp[1..];
                        continue;
                    }
                    b'(' => {
                        tokens.push(token(line, (c, c + 1), TokenType::LParen));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b')' => {
                        tokens.push(token(line, (c, c + 1), TokenType::RParen));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b'{' => {
                        tokens.push(token(line, (c, c + 1), TokenType::LCurly));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b'}' => {
                        tokens.push(token(line, (c, c + 1), TokenType::RCurly));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b'[' => {
                        tokens.push(token(line, (c, c + 1), TokenType::LBracket));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b']' => {
                        tokens.push(token(line, (c, c + 1), TokenType::RBracket));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b'?' => {
                        tokens.push(token(line, (c, c + 1), TokenType::QuestionMark));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b',' => {
                        tokens.push(token(line, (c, c + 1), TokenType::Comma));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b':' => {
                        tokens.push(token(line, (c, c + 1), TokenType::Colon));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b';' => {
                        tokens.push(token(line, (c, c + 1), TokenType::Semicolon));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b'=' => {
                        tokens.push(token(line, (c, c + 1), TokenType::Equals));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    b'@' => {
                        tokens.push(token(line, (c, c + 1), TokenType::AtSign));
                        c += 1;
                        inp = &inp[1..];
                        continue;
                    }
                    _ => {
                        // munch whitespace
                        println!("trying whitespace...");
                        if is_whitespace(b) {
                            c += 1;
                            inp = &inp[1..];
                            continue;
                        }
                        // ->
                        println!("trying arrow...");
                        if let Some(rest) = inp.strip_prefix("->") {
                            tokens.push(token(line, (c, c + 2), TokenType::Arrow));
                            c += 2;
                            inp = rest;
                            continue;
                        }

                        // eat comment
                        println!("trying comments...");
                        if inp.starts_with("//") {
                            inp = take_until_byte(inp, b'\n');
                            line += 1;
                            c = 0;
                            continue;
                        }
                        // eat ident or reserved word
                        println!("trying idents...");
                        if let Some((ide, rest)) = munch_ident(inp) {
                            if let Some((_, tok)) =
                                BASE_TYPES_TOKENS.iter().find(|tup| tup.0 == ide)
                            {
                                tokens.push(token(line, (c, c + ide.len()), tok.clone()));
                            } else if let Some((_, tok)) =
                                KEYWORDS_TOKENS.iter().find(|tup| tup.0 == ide)
                            {
                                tokens.push(token(line, (c, c + ide.len()), tok.clone()));
                            } else {
                                tokens.push(token(
                                    line,
                                    (c, c + ide.len()),
                                    TokenType::Ident(ide.to_string()),
                                ));
                            }
                            c += ide.len();
                            inp = rest;
                            continue;
                        }
                        // eat literal
                        println!("trying literal...");
                        match munch_literal(inp) {
                            Ok(Some((lit, rest))) => {
                                let token_len = inp.len() - rest.len();
                                tokens.push(token(
                                    line,
                                    (c, c + token_len),
                                    TokenType::Literal(lit),
                                ));
                                inp = rest;
                                c += token_len;
                                continue;
                            }
                            _ => {}
                        }

                        return Err(ParseError {
                            line,
                            col: c,
                            type_: ParseErrorType::UnknownValue,
                        });
                    }
                }
            }
        }
    }
    tokens.push(token(line, (c, c + 1), TokenType::Eof));
    Ok(tokens)
}

fn munch_ident(inp: &str) -> Option<(&str, &str)> {
    let mut start = 0;
    let first = inp.as_bytes().first().copied()?;
    if !first.is_ascii_alphabetic() {
        return None;
    }
    loop {
        match inp.as_bytes()[start..] {
            [] => break,
            [b, ..] => match b {
                b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => {
                    start += 1;
                }
                _ => break,
            },
        }
    }
    if start > 0 {
        Some((&inp[..start], &inp[start..]))
    } else {
        None
    }
}

fn take_until_byte(mut inp: &str, b: u8) -> &str {
    loop {
        match *inp.as_bytes() {
            [] => return "",
            [f, ..] => {
                if f == b {
                    return &inp[1..];
                } else {
                    inp = &inp[1..];
                }
            }
        }
    }
}

fn is_whitespace(b: u8) -> bool {
    (b as char).is_whitespace()
}

fn token(line: usize, span: (usize, usize), type_: TokenType) -> Token {
    Token { line, span, type_ }
}

fn munch_literal(inp: &str) -> Result<Option<(Literal, &str)>, ParseLiteralError> {
    if let Some((s, rest)) = munch_literal_string(inp)? {
        return Ok(Some((Literal::String(s), rest)));
    }
    if let Some((n, rest)) = munch_literal_decimal_number(inp)? {
        return Ok(Some((Literal::DecimalNumber(n), rest)));
    }
    if let Some((n, rest)) = munch_literal_hex_number(inp)? {
        return Ok(Some((Literal::DecimalNumber(n), rest)));
    }
    if let Some((n, rest)) = munch_literal_binary_number(inp)? {
        return Ok(Some((Literal::DecimalNumber(n), rest)));
    }
    if let Some((slc, rest)) = munch_literal_float(inp)? {
        return Ok(Some((Literal::Float(slc), rest)));
    }
    // println!("trying parse list...");
    // if let Some((slc, rest)) = munch_literal_list(inp)? {
    //     return Ok(Some((Literal::List(slc), rest)));
    // }
    Ok(None)
}

#[derive(Clone, Debug)]
enum ParseLiteralError {
    UnexpectedEndOfInput,
    InvalidEscape(char),
    InvalidFloatMissingDecimal,
    InvalidFloatMissingExp,
}

// const F: f64 = -.;

fn munch_literal_string(inp: &str) -> Result<Option<(String, &str)>, ParseLiteralError> {
    if !inp.starts_with('"') {
        return Ok(None);
    }
    let mut escaping = false;
    let mut scratch = String::new();
    let mut iter = inp[1..].chars();
    loop {
        match iter.next() {
            Some('\n') | None => return Err(ParseLiteralError::UnexpectedEndOfInput),
            Some('"') if escaping => {
                scratch.push('"');
                escaping = false;
            }
            Some('\\') if escaping => {
                scratch.push('\\');
                escaping = false;
            }
            Some('/') if escaping => {
                scratch.push('/');
                escaping = false;
            }
            // Some('b') if escaping => {
            //     scratch.push('\x08');
            //     escaping = false;
            // }
            // Some('f') if escaping => {
            //     scratch.push('\x0c');
            //     escaping = false;
            // }
            Some('n') if escaping => {
                scratch.push('\n');
                escaping = false;
            }
            Some('r') if escaping => {
                scratch.push('\r');
                escaping = false;
            }
            Some('t') if escaping => {
                scratch.push('\t');
                escaping = false;
            }
            Some(c) if escaping => {
                return Err(ParseLiteralError::InvalidEscape(c as char));
            }
            Some('"') => return Ok(Some((scratch, iter.as_str()))),
            Some(c) => scratch.push(c),
        }
    }
}

// returns the string content of the number
fn munch_literal_decimal_number(inp: &str) -> Result<Option<(String, &str)>, ParseLiteralError> {
    let mut i = 0;
    if let Some(b'-' | b'+') = inp.as_bytes().first().copied() {
        i += 1;
    }
    loop {
        match inp[i..].as_bytes() {
            [b'0'..=b'9', ..] => {
                i += 1;
            }
            _ => break,
        }
    }
    if i > 0 {
        Ok(Some((inp[..i].to_string(), &inp[i..])))
    } else {
        Ok(None)
    }
}

fn munch_literal_hex_number(inp: &str) -> Result<Option<(String, &str)>, ParseLiteralError> {
    let mut i = 0;
    if let Some(b'-' | b'+') = inp.as_bytes().first().copied() {
        i += 1;
    }
    let Some(inp) = inp[i..].strip_prefix("0x") else {
        return Ok(None);
    };
    i += 2; // "0x"
    loop {
        match inp.as_bytes().get(i) {
            Some(&b) => match b {
                b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' => i += 1,
                _ => break,
            },
            None => break,
        }
    }
    if i > 0 {
        Ok(Some((inp[..i].to_string(), &inp[i..])))
    } else {
        Ok(None)
    }
}

fn munch_literal_binary_number(inp: &str) -> Result<Option<(String, &str)>, ParseLiteralError> {
    let mut i = 0;
    let Some(inp) = inp.strip_prefix("0b") else {
        return Ok(None);
    };
    i += 2; // "0b"
    loop {
        match inp.as_bytes().get(i) {
            Some(&b) => match b {
                b'0' => {
                    i += 1;
                }
                b'1' => {
                    i += 1;
                }
                _ => break,
            },
            None => break,
        }
    }
    if i > 0 {
        Ok(Some((inp[..i].to_string(), &inp[i..])))
    } else {
        Ok(None)
    }
}

fn munch_literal_float(inp: &str) -> Result<Option<(String, &str)>, ParseLiteralError> {
    let mut i = 0;
    // eat maybe sign
    if let Some(b'-' | b'+') = inp.as_bytes().get(0).copied() {
        i += 1;
    }
    //eat numbers, decimal, and more numbers
    let mut found_decimal = false;
    loop {
        match inp[i..].as_bytes().get(i).copied() {
            Some(c) if c >= b'0' && c <= b'9' => {
                i += 1;
            }
            Some(b'.') => {
                if found_decimal {
                    break;
                } else {
                    found_decimal = true;
                    i += 1;
                }
            }
            Some(_) => break,
            None => break,
        }
    }
    if let Some(b'e' | b'E') = inp[i..].as_bytes().get(0).copied() {
        i += 1;
        let mut exp_i = 0;
        loop {
            match inp.as_bytes().get(i + exp_i).copied() {
                Some(c) if c >= b'0' && c <= b'9' => {
                    exp_i += 1;
                }
                Some(_) | None => break,
            }
        }
        if exp_i == 0 {
            return Err(ParseLiteralError::InvalidFloatMissingExp);
        } else {
            i += exp_i;
        }
    }
    if i > 0 && found_decimal {
        Ok(Some((inp[..i].to_string(), &inp[i..])))
    } else {
        Ok(None)
    }
}
