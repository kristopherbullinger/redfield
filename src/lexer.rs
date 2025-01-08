use compact_str::CompactString;
#[derive(PartialEq, Eq, Debug)]
pub struct Token {
    pub line: usize,
    pub span: (usize, usize),
    pub type_: TokenType,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TokenType {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LCurly,
    RCurly,
    QuestionMark,
    Comma,
    Colon,
    Semicolon,
    Equals,
    Comment(Comment),
    Whitespace(Whitespace),
    Ident(CompactString),
    // Ident(crate::Ident),
    BaseType(crate::BaseType),
    KeywordEnum,
    KeywordMessage,
    KeywordOneof,
    KeywordService,
    KeywordVoid,
    AtSign,
    Verb(crate::Verb),
    True,
    False,
    Arrow,
    Literal(Literal),
}

impl TokenType {
    pub(crate) fn debug_str(&self) -> &'static str {
        match *self {
            TokenType::LParen => "`(`",
            TokenType::RParen => "`)`",
            TokenType::LBracket => "`[`",
            TokenType::RBracket => "`]`",
            TokenType::LCurly => "`{`",
            TokenType::RCurly => "`}`",
            TokenType::QuestionMark => "`?`",
            TokenType::Comma => "`,`",
            TokenType::Colon => "`:`",
            TokenType::Semicolon => "`;`",
            TokenType::Equals => "`=`",
            TokenType::Comment(_) => "a comment",
            TokenType::Whitespace(_) => "whitespace",
            TokenType::Ident(_) => "an identifier",
            TokenType::BaseType(_) => "a type",
            TokenType::KeywordEnum => "keyword `enum`",
            TokenType::KeywordMessage => "keyword `message`",
            TokenType::KeywordOneof => "keyword `oneof`",
            TokenType::KeywordService => "keyword `service`",
            TokenType::KeywordVoid => "keyword `void`",
            TokenType::AtSign => "`@`",
            TokenType::Verb(ref vb) => match vb {
                crate::Verb::Get => "keyword `GET`",
                crate::Verb::Post => "keyword `POST`",
            },
            TokenType::True => "keyword `true`",
            TokenType::False => "keyword `false`",
            TokenType::Arrow => "`->`",
            TokenType::Literal(_) => "a value literal",
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct EnumVariant {
    content: CompactString,
}

impl EnumVariant {
    pub fn content(&self) -> &str {
        self.content.as_str()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Comment {
    content: CompactString,
}

impl Comment {
    pub fn content(&self) -> &str {
        self.content.as_str()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Whitespace {
    content: CompactString,
}

impl Whitespace {
    pub fn content(&self) -> &str {
        self.content.as_str()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ParseLiteralError {
    UnexpectedEndOfInput,
    InvalidEscape(char),
    InvalidFloatMissingDecimal,
    InvalidFloatMissingExp,
}

static KEYWORDS_TOKENS: &[(&str, TokenType)] = &[
    ("service", TokenType::KeywordService),
    ("enum", TokenType::KeywordEnum),
    ("oneof", TokenType::KeywordOneof),
    ("message", TokenType::KeywordMessage),
    ("true", TokenType::True),
    ("false", TokenType::False),
    ("GET", TokenType::Verb(crate::Verb::Get)),
    ("POST", TokenType::Verb(crate::Verb::Post)),
];

static BASE_TYPES_TOKENS: &[(&str, TokenType)] = &[
    ("bool", TokenType::BaseType(crate::BaseType::Bool)),
    ("u8", TokenType::BaseType(crate::BaseType::U8)),
    ("i8", TokenType::BaseType(crate::BaseType::I8)),
    ("u16", TokenType::BaseType(crate::BaseType::U16)),
    ("i16", TokenType::BaseType(crate::BaseType::I16)),
    ("u32", TokenType::BaseType(crate::BaseType::U32)),
    ("i32", TokenType::BaseType(crate::BaseType::I32)),
    ("u64", TokenType::BaseType(crate::BaseType::U64)),
    ("i64", TokenType::BaseType(crate::BaseType::I64)),
    ("string", TokenType::BaseType(crate::BaseType::String)),
    ("bytes", TokenType::BaseType(crate::BaseType::Bytes)),
    ("void", TokenType::BaseType(crate::BaseType::Void)),
];

fn is_whitespace(b: u8) -> bool {
    (b as char).is_whitespace()
}
fn munch_literal_string(inp: &str) -> Result<Option<(CompactString, &str)>, ParseLiteralError> {
    if !inp.starts_with('"') {
        return Ok(None);
    }
    let mut escaping = false;
    let mut scratch = CompactString::default();
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
fn munch_literal_decimal_number(
    inp: &str,
) -> Result<Option<(CompactString, &str)>, ParseLiteralError> {
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
        Ok(Some((inp[..i].into(), &inp[i..])))
    } else {
        Ok(None)
    }
}

fn munch_literal_hex_number(inp: &str) -> Result<Option<(CompactString, &str)>, ParseLiteralError> {
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
        Ok(Some((inp[..i].into(), &inp[i..])))
    } else {
        Ok(None)
    }
}

fn munch_literal_binary_number(
    inp: &str,
) -> Result<Option<(CompactString, &str)>, ParseLiteralError> {
    let mut i = 0;
    let Some(inp) = inp.strip_prefix("0b") else {
        return Ok(None);
    };
    i += 2; // "0b"
    loop {
        match inp.as_bytes().get(i) {
            Some(&b) => match b {
                b'0' | b'1' => {
                    i += 1;
                }
                _ => break,
            },
            None => break,
        }
    }
    if i > 0 {
        Ok(Some((inp[..i].into(), &inp[i..])))
    } else {
        Ok(None)
    }
}

fn munch_enum_value(inp: &str) -> Option<(CompactString, &str)> {
    let rest = inp.strip_prefix('.')?;
    let (ident, rest) = munch_ident(rest)?;
    Some((inp[0..ident.len() + 1].into(), rest))
}

fn munch_literal_float(inp: &str) -> Result<Option<(CompactString, &str)>, ParseLiteralError> {
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
        Ok(Some((inp[..i].into(), &inp[i..])))
    } else {
        Ok(None)
    }
}

fn munch_literal(inp: &str) -> Result<Option<(Literal, &str)>, ParseLiteralError> {
    if let Some((s, rest)) = munch_literal_string(inp)? {
        let lit = Literal::String(LiteralString(s.into()));
        return Ok(Some((lit, rest)));
    }
    if let Some((ide, rest)) = munch_enum_value(inp) {
        let lit = Literal::EnumVariant(CompactString::new(ide));
        return Ok(Some((lit, rest)));
    }
    if let Some((n, rest)) = munch_literal_decimal_number(inp)? {
        let lit = Literal::DecimalNumber(LiteralDecimalNumber(n.into()));
        return Ok(Some((lit, rest)));
    }
    if let Some((n, rest)) = munch_literal_hex_number(inp)? {
        let lit = Literal::HexNumber(LiteralHexNumber(n.into()));
        return Ok(Some((lit, rest)));
    }
    if let Some((n, rest)) = munch_literal_binary_number(inp)? {
        let lit = Literal::DecimalNumber(LiteralDecimalNumber(n.into()));
        return Ok(Some((lit, rest)));
    }
    if let Some((slc, rest)) = munch_literal_float(inp)? {
        let lit = Literal::Float(LiteralFloat(slc.into()));
        return Ok(Some((lit, rest)));
    }
    Ok(None)
}

fn token(line: usize, span: (usize, usize), type_: TokenType) -> Token {
    Token { line, span, type_ }
}

pub fn lex_document(inp: &str) -> Result<Vec<Token>, ParseError> {
    TokenIter::new(inp).collect::<Result<Vec<_>, _>>()
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub(crate) struct TokenIter<'s> {
    pub(crate) inp: &'s str,
    pub(crate) col: usize,
    pub(crate) line: usize,
}

impl<'s> TokenIter<'s> {
    fn new(inp: &'s str) -> TokenIter<'s> {
        TokenIter {
            inp,
            col: 0,
            line: 0,
        }
    }

    fn next_token(&mut self) -> Option<Result<Token, ParseError>> {
        loop {
            match *self.inp.as_bytes() {
                [] => break,
                [b, ..] => {
                    match b {
                        b'\n' => {
                            self.line += 1;
                            self.col = 0;
                            self.inp = &self.inp[1..];
                            continue;
                        }
                        b'(' => {
                            let tok = token(self.line, (self.col, self.col + 1), TokenType::LParen);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b')' => {
                            let tok = token(self.line, (self.col, self.col + 1), TokenType::RParen);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b'{' => {
                            let tok = token(self.line, (self.col, self.col + 1), TokenType::LCurly);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b'}' => {
                            let tok = token(self.line, (self.col, self.col + 1), TokenType::RCurly);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b'[' => {
                            let tok =
                                token(self.line, (self.col, self.col + 1), TokenType::LBracket);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b']' => {
                            let tok =
                                token(self.line, (self.col, self.col + 1), TokenType::RBracket);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b'?' => {
                            let tok =
                                token(self.line, (self.col, self.col + 1), TokenType::QuestionMark);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b',' => {
                            let tok = token(self.line, (self.col, self.col + 1), TokenType::Comma);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b':' => {
                            let tok = token(self.line, (self.col, self.col + 1), TokenType::Colon);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b';' => {
                            let tok =
                                token(self.line, (self.col, self.col + 1), TokenType::Semicolon);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b'=' => {
                            let tok = token(self.line, (self.col, self.col + 1), TokenType::Equals);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        b'@' => {
                            let tok = token(self.line, (self.col, self.col + 1), TokenType::AtSign);
                            self.col += 1;
                            self.inp = &self.inp[1..];
                            return Some(Ok(tok));
                        }
                        _ => {
                            // munch whitespace
                            if is_whitespace(b) {
                                self.col += 1;
                                self.inp = &self.inp[1..];
                                continue;
                            }
                            // ->
                            if let Some(rest) = self.inp.strip_prefix("->") {
                                let tok =
                                    token(self.line, (self.col, self.col + 2), TokenType::Arrow);
                                self.col += 2;
                                self.inp = rest;
                                return Some(Ok(tok));
                            }

                            // eat comment
                            if self.inp.starts_with("//") {
                                self.inp = take_until_byte(self.inp, b'\n');
                                self.line += 1;
                                self.col = 0;
                                continue;
                            }
                            // eat enum variant
                            // if let Some((ide, rest)) = munch_enum_value(self.inp) {
                            //     self.c += ide.len();
                            //     self.inp = rest;
                            //     let tok = token(
                            //         self.line,
                            //         (self.c, self.c + ide.len()),
                            //         TokenType::EnumVariant(EnumVariant { content: ide }),
                            //     );
                            //     return Some(Ok(tok));
                            // }
                            // eat ident or reserved word
                            if let Some((ide, rest)) = munch_ident(self.inp) {
                                let tok = if let Some((_, tok)) =
                                    BASE_TYPES_TOKENS.iter().find(|tup| tup.0 == ide)
                                {
                                    token(self.line, (self.col, self.col + ide.len()), tok.clone())
                                } else if let Some((_, tok)) =
                                    KEYWORDS_TOKENS.iter().find(|tup| tup.0 == ide)
                                {
                                    token(self.line, (self.col, self.col + ide.len()), tok.clone())
                                } else {
                                    token(
                                        self.line,
                                        (self.col, self.col + ide.len()),
                                        TokenType::Ident(ide.into()),
                                    )
                                };
                                self.col += ide.len();
                                self.inp = rest;
                                return Some(Ok(tok));
                            }
                            // eat literal
                            match munch_literal(self.inp) {
                                Ok(Some((lit, rest))) => {
                                    let token_len = self.inp.len() - rest.len();
                                    let tok = token(
                                        self.line,
                                        (self.col, self.col + token_len),
                                        TokenType::Literal(lit),
                                    );
                                    self.inp = rest;
                                    self.col += token_len;
                                    return Some(Ok(tok));
                                }
                                _ => {}
                            }

                            return Some(Err(ParseError {
                                line: self.line,
                                col: self.col,
                                type_: ParseErrorType::UnknownValue,
                            }));
                        }
                    }
                }
            }
        }
        None
    }
}

impl<'s> Iterator for TokenIter<'s> {
    type Item = Result<Token, ParseError>;
    fn next(&mut self) -> Option<Self::Item> {
        TokenIter::next_token(self)
    }
}

pub(crate) fn lex_document_iter<'s>(inp: &'s str) -> TokenIter<'s> {
    TokenIter::new(inp)
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Literal {
    DecimalNumber(LiteralDecimalNumber),
    BinaryNumber(LiteralBinaryNumber),
    HexNumber(LiteralHexNumber),
    String(LiteralString),
    Float(LiteralFloat),
    EnumVariant(CompactString),
    // EnumVariant(crate::Ident),
    Bool(bool),
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LiteralDecimalNumber(CompactString);
impl LiteralDecimalNumber {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LiteralBinaryNumber(CompactString);
impl LiteralBinaryNumber {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LiteralHexNumber(CompactString);
impl LiteralHexNumber {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LiteralString(pub(crate) CompactString);
impl LiteralString {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn new(s: &str) -> LiteralString {
        LiteralString(CompactString::new(s))
    }
}
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct LiteralFloat(CompactString);
impl LiteralFloat {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ParseError {
    pub line: usize,
    pub col: usize,
    pub type_: ParseErrorType,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ParseErrorType {
    UnexpectedEof,
    UnknownValue,
    ParseLiteral(ParseLiteralError),
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
