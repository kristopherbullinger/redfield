use std::{
    collections::{BTreeMap, HashMap, HashSet},
    str::FromStr,
};

use compact_str::CompactString;
use smallvec::SmallVec;
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
    let mut definitions: Vec<Definition> = vec![];
    let mut services: Vec<ServiceRaw> = vec![];
    let mut ctx = Context::new();
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
                        ParseErrorType::UnknownTopLevelIdent(new_ident(tok.line, tok.span.0, ide)),
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
                    services: vec![],
                    definitions,
                });
            }
        }
    }
    // parse the rest of the document
    loop {
        match tok.type_ {
            lexer::TokenType::Ident(ide) => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::TopLevelIdentNotAtBeginningOfFile(new_ident(
                        tok.line, tok.span.0, ide,
                    )),
                ));
            }
            lexer::TokenType::KeywordService => {
                let svc = munch_service(&mut iter)?;
                services.push(svc);
            }
            lexer::TokenType::KeywordMessage => {
                let msg = munch_message(&mut iter, &mut ctx)?;
                definitions.push(Definition::Message(msg));
            }
            lexer::TokenType::KeywordEnum => {
                let enm = munch_enum_(&mut iter, &mut ctx)?;
                definitions.push(Definition::Enum(enm));
            }
            lexer::TokenType::KeywordOneof => {
                let oneof = munch_oneof_(&mut iter, &mut ctx)?;
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
                break;
            }
        }
    }
    // ensure top-level identifiers are unique
    {
        let mut toplevel_names: HashSet<&str> = Default::default();
        for svc in services.iter() {
            let ident = &svc.ident;
            if !toplevel_names.insert(ident.value.as_str()) {
                return Err(parse_error(
                    ident.line,
                    ident.col,
                    ParseErrorType::DuplicateIdentifier(ident.clone()),
                ));
            }
        }
        for def in definitions.iter() {
            let ident = def.ident(&ctx);
            if !toplevel_names.insert(ident.value.as_str()) {
                return Err(parse_error(
                    ident.line,
                    ident.col,
                    ParseErrorType::DuplicateIdentifier(ident.clone()),
                ));
            }
        }
    }
    // detect recursion of user-defined types and insert indirection
    {
        for def in definitions.iter_mut() {
            match *def {
                Definition::Enum(_) => {}
                Definition::OneOf(ref mut oneof) => oneof.set_indirection(&ctx),
                Definition::Message(ref mut msg) => msg.set_indirection(&ctx),
            }
        }
    }
    Ok(RawDocument {
        base_url,
        services: validate_svcs(services, &ctx)?,
        definitions,
    })
}

// ensure all procedure inputs and outputs refer to a valid message type
fn validate_svcs(services: Vec<ServiceRaw>, ctx: &Context) -> Result<Vec<Service>, ParseError> {
    let mut validated_svcs: Vec<Service> = vec![];
    for svc in services.into_iter() {
        let mut prcs: Vec<Procedure> = vec![];
        for prc in svc.procedures.into_iter() {
            prcs.push(prc.resolve_idents(ctx, &svc.ident)?);
        }
        validated_svcs.push(Service {
            ident: svc.ident.clone(),
            procedures: prcs,
        });
    }
    Ok(validated_svcs)
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
fn munch_service(iter: &mut lexer::TokenIter<'_>) -> Result<ServiceRaw, ParseError> {
    let ident = munch_ident(&mut *iter)?;
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    let mut procedures: Vec<ProcedureRaw> = vec![];
    loop {
        let tok = next_not_eof(iter)?;
        let verb = match tok.type_ {
            lexer::TokenType::Verb(v) => v,
            lexer::TokenType::RCurly => {
                // ensure all procedure names are unique
                let mut procedure_names: HashSet<&str> = HashSet::new();
                for prc in procedures.iter() {
                    if !procedure_names.insert(prc.ident.value.as_str()) {
                        return Err(parse_error(
                            prc.ident.line,
                            prc.ident.col,
                            ParseErrorType::DuplicateIdentifier(prc.ident.clone()),
                        ));
                    }
                }
                return Ok(ServiceRaw { ident, procedures });
            }
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::ExpectedProcedureVerb,
                ))
            }
        };
        let ident = munch_ident(&mut *iter)?;
        expect_next_equals(&mut *iter, lexer::TokenType::LParen)?;
        let tok = next_not_eof(iter)?;
        let input = match tok.type_ {
            lexer::TokenType::RParen => None,
            lexer::TokenType::Ident(i) => {
                expect_next_equals(&mut *iter, lexer::TokenType::RParen)?;
                Some(new_ident(tok.line, tok.span.0, i))
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
        procedures.push(ProcedureRaw {
            verb,
            ident,
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

fn munch_type(iter: &mut lexer::TokenIter<'_>) -> Result<TypeRaw, ParseError> {
    let mut modifiers = SmallVec::<[ListSize; 4]>::new();
    let base = loop {
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::LBracket => {
                let sz = munch_list_size(&mut *iter)?;
                modifiers.push(match sz {
                    None => ListSize::Unsized,
                    Some(s) => ListSize::Sized(s),
                });
            }
            lexer::TokenType::Ident(i) => {
                break TypeOrIdent::Ident(new_ident(tok.line, tok.span.0, i));
            }
            lexer::TokenType::BaseType(t) => {
                break TypeOrIdent::Type(t);
            }
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::ExpectedLBracketTypeOrIdent,
                ))
            }
        }
    };
    Ok(TypeRaw { modifiers, base })
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

type ScopeLayer = HashMap<CompactString, (TypeToken, DefinitionType)>;
#[derive(Debug, Default)]
struct Scopes {
    global: ScopeLayer,
    nested: Vec<ScopeLayer>,
}

impl Scopes {
    fn deepest(&mut self) -> &mut ScopeLayer {
        match self.nested.last_mut() {
            Some(s) => s,
            None => &mut self.global,
        }
    }

    fn push_scope_layer(&mut self) {
        self.nested.push(Default::default());
    }

    fn push_scope_layer_<'a>(&'a mut self) -> ScopeDropGuard<'a> {
        self.nested.push(Default::default());
        ScopeDropGuard(self)
    }

    fn pop_scope_layer(&mut self) {
        self.nested.pop();
    }

    fn lookup_ident(&self, i: &Ident) -> Option<(TypeToken, DefinitionType)> {
        match self
            .nested
            .iter()
            .rev()
            .find_map(|layer| layer.get(i.as_str()))
        {
            Some(tup) => Some(*tup),
            None => Some(self.global.get(i.as_str()).copied()?),
        }
    }
}

struct Context {
    scopes: Scopes,
    types: Types,
}

struct ScopeDropGuard<'a>(&'a mut Scopes);
impl<'a> std::ops::Drop for ScopeDropGuard<'a> {
    fn drop(&mut self) {
        self.0.pop_scope_layer()
    }
}

impl Context {
    fn new() -> Context {
        Context {
            scopes: Default::default(),
            types: Default::default(),
        }
    }

    fn get_ident(&self, tok: TypeToken) -> &Ident {
        &self.types.idents[tok.0]
    }

    // add an ident, but error if that ident already exists
    fn push_new_ident(&mut self, i: Ident, typ: DefinitionType) -> Result<TypeToken, ParseError> {
        if self.scopes.deepest().contains_key(i.as_str()) {
            Err(parse_error(
                i.line,
                i.col,
                ParseErrorType::DuplicateIdentifier(i),
            ))
        } else {
            let tok = self.types.add_ident(i.clone());
            self.scopes.deepest().insert(i.value, (tok, typ));
            Ok(tok)
        }
    }

    fn push_scope_layer(&mut self) {
        self.scopes.push_scope_layer()
    }

    fn pop_scope_layer(&mut self) {
        self.scopes.pop_scope_layer()
    }

    fn lookup_ident(&self, i: &Ident) -> Option<(TypeToken, DefinitionType)> {
        self.scopes.lookup_ident(i)
    }

    fn lookup_ident_token(&self, i: &Ident) -> Option<TypeToken> {
        self.scopes.lookup_ident(i).map(|tup| tup.0)
    }

    fn add_link(&mut self, a: TypeToken, b: TypeToken, dir: Indirection) {
        self.types.add_link(a, b, dir)
    }
}

// assumes "message" keyword already chomped
fn munch_message(
    iter: &mut lexer::TokenIter<'_>,
    ctx: &mut Context,
) -> Result<Message, ParseError> {
    fn munch_message_inner(
        iter: &mut lexer::TokenIter<'_>,
        ctx: &mut Context,
        msg_token: TypeToken,
    ) -> Result<Message, ParseError> {
        let mut definitions: Vec<Definition> = vec![];
        let mut fields_: Vec<(Ident, u16, TypeRaw, bool)> = vec![];
        // test for field number uniqueness
        let mut field_numbers = HashSet::new();
        expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
        let msg = loop {
            let tok = next_not_eof(iter)?;
            match tok.type_ {
                lexer::TokenType::KeywordEnum => {
                    let enm = munch_enum_(&mut *iter, &mut *ctx)?;
                    definitions.push(Definition::Enum(enm));
                }
                lexer::TokenType::KeywordOneof => {
                    let oneof = munch_oneof_(&mut *iter, &mut *ctx)?;
                    definitions.push(Definition::OneOf(oneof));
                }
                lexer::TokenType::KeywordMessage => {
                    let msg = munch_message(&mut *iter, &mut *ctx)?;
                    definitions.push(Definition::Message(msg))
                }
                lexer::TokenType::Ident(name) => {
                    let ident = new_ident(tok.line, tok.span.0, name);
                    // ident@4?:
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
                    fields_.push((ident, field_number, type_, optional));
                    expect_next_equals(&mut *iter, lexer::TokenType::Comma)?;
                }
                lexer::TokenType::RCurly => {
                    let mut names: HashSet<&str> = Default::default();
                    for ident in fields_.iter().map(|tup| &tup.0) {
                        // 1. ensure all fields have unique names
                        if !names.insert(ident.as_str()) {
                            return Err(parse_error(
                                ident.line,
                                ident.col,
                                ParseErrorType::DuplicateIdentifier(ident.clone()),
                            ));
                        }
                    }
                    let mut fields: Vec<MessageField> = vec![];
                    for (ident, field_number, type_, optional) in fields_.into_iter() {
                        let (type_base, indirection) = match type_.base {
                            TypeOrIdent::Type(t) => (TypeOrToken::Type(t), Indirection::Direct),
                            TypeOrIdent::Ident(i) => {
                                let tok = ctx.lookup_ident_token(&i).ok_or(parse_error(
                                    i.line,
                                    i.col,
                                    ParseErrorType::UnknownIdent(i.clone()),
                                ))?;
                                // if the field's user type is in an unsized list, it is indirectly referenced
                                let indirection = if type_
                                    .modifiers
                                    .iter()
                                    .any(|m| matches!(m, ListSize::Unsized))
                                {
                                    Indirection::Indirect
                                } else {
                                    Indirection::Direct
                                };
                                // insert associations between this message and type referenced by this field
                                ctx.add_link(msg_token, tok, indirection);
                                (TypeOrToken::Token(tok), indirection)
                            }
                        };
                        fields.push(MessageField {
                            ident,
                            field_number,
                            type_: Type {
                                modifiers: type_.modifiers,
                                base: type_base,
                            },
                            optional,
                            indirection,
                        });
                    }
                    break Message {
                        ident: msg_token,
                        definitions,
                        fields,
                    };
                }
                _ => {
                    return Err(parse_error(
                        tok.line,
                        tok.span.0,
                        ParseErrorType::ExpectedEnumMessageOneOfOrIdent,
                    ))
                }
            }
        };
        Ok(msg)
    }
    let ident = munch_ident(&mut *iter)?;
    let msg_token = ctx.push_new_ident(ident, DefinitionType::Message)?;
    ctx.push_scope_layer();
    let res = munch_message_inner(iter, ctx, msg_token);
    ctx.pop_scope_layer();
    res
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
fn munch_enum_(iter: &mut lexer::TokenIter<'_>, ctx: &mut Context) -> Result<Enum, ParseError> {
    // munch ident
    let ident = munch_ident(&mut *iter)?;
    let type_token = ctx.push_new_ident(ident, DefinitionType::Enum)?;
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
                        variants.push(EnumVariant {
                            ident: new_ident(tok.line, tok.span.0, name),
                            value,
                        });
                    };
                }
                expect_next_equals(&mut *iter, lexer::TokenType::Comma)?;
            }
            lexer::TokenType::RCurly => {
                let mut names: HashSet<&str> = Default::default();
                for v in variants.iter() {
                    if !names.insert(v.ident.value.as_str()) {
                        return Err(parse_error(
                            line,
                            col,
                            ParseErrorType::DuplicateIdentifier(v.ident.clone()),
                        ));
                    }
                }
                return Ok(Enum {
                    ident: type_token,
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
fn munch_oneof_(iter: &mut lexer::TokenIter<'_>, ctx: &mut Context) -> Result<OneOf, ParseError> {
    fn munch_oneof_inner(
        iter: &mut lexer::TokenIter<'_>,
        ctx: &mut Context,
        oneof_token: TypeToken,
    ) -> Result<OneOf, ParseError> {
        expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
        let mut definitions: Vec<Definition> = vec![];
        let mut variants_: Vec<(Ident, u16, TypeRaw)> = vec![];
        let mut field_numbers = HashSet::new();
        loop {
            let tok = next_not_eof(iter)?;
            match tok.type_ {
                lexer::TokenType::RCurly => {
                    // ensure field names are unique
                    let mut names: HashSet<&str> = Default::default();
                    for (name, _, _) in variants_.iter() {
                        if !names.insert(name.as_str()) {
                            return Err(parse_error(
                                tok.line,
                                tok.span.0,
                                ParseErrorType::DuplicateFieldName(name.clone()),
                            ));
                        }
                    }
                    let mut variants: Vec<OneOfVariant> = vec![];
                    for (ident, field_number, type_) in variants_ {
                        let (type_base, indirection) = match type_.base {
                            TypeOrIdent::Type(t) => (TypeOrToken::Type(t), Indirection::Direct),
                            TypeOrIdent::Ident(i) => {
                                let tok = ctx.lookup_ident_token(&i).ok_or(parse_error(
                                    i.line,
                                    i.col,
                                    ParseErrorType::UnknownIdent(i.clone()),
                                ))?;
                                // if the field's user type is in an unsized list, it is indirectly referenced
                                let indirection = if type_
                                    .modifiers
                                    .iter()
                                    .any(|m| matches!(m, ListSize::Unsized))
                                {
                                    Indirection::Indirect
                                } else {
                                    Indirection::Direct
                                };
                                // insert associations between this message and type referenced by this field
                                ctx.add_link(oneof_token, tok, indirection);
                                (TypeOrToken::Token(tok), indirection)
                            }
                        };
                        variants.push(OneOfVariant {
                            ident,
                            field_number,
                            type_: Type {
                                modifiers: type_.modifiers,
                                base: type_base,
                            },
                            indirection,
                        })
                    }
                    // field number uniqueness check during parsing
                    return Ok(OneOf {
                        ident: oneof_token,
                        definitions,
                        variants,
                    });
                }
                lexer::TokenType::KeywordEnum => {
                    let enm = munch_enum_(iter, ctx)?;
                    definitions.push(Definition::Enum(enm));
                }
                lexer::TokenType::KeywordOneof => {
                    let oneof = munch_oneof_(iter, ctx)?;
                    definitions.push(Definition::OneOf(oneof));
                }
                lexer::TokenType::KeywordMessage => {
                    let msg = munch_message(iter, ctx)?;
                    definitions.push(Definition::Message(msg));
                }
                lexer::TokenType::Ident(field_name) => {
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
                    variants_.push((
                        new_ident(tok.line, tok.span.0, field_name),
                        field_number,
                        type_,
                    ));
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
    let ident = munch_ident(&mut *iter)?;
    let token = ctx.push_new_ident(ident, DefinitionType::OneOf)?;
    ctx.push_scope_layer();
    let res = munch_oneof_inner(iter, ctx, token);
    ctx.pop_scope_layer();
    res
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

// a service with raw idents for inputs and outputs
#[derive(Debug, PartialEq, Eq, Default)]
pub struct ServiceRaw {
    pub ident: Ident,
    pub procedures: Vec<ProcedureRaw>,
}

// a service whose procedures inputs and outputs have been validated
#[derive(Debug, PartialEq, Eq, Default)]
pub struct Service {
    /// The name of the service
    pub ident: Ident,
    pub procedures: Vec<Procedure>,
}

impl RawDocument {
    pub fn base_url(&self) -> Option<&str> {
        self.base_url.as_deref()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ProcedureRaw {
    pub verb: Verb,
    pub ident: Ident,
    pub input: Option<Ident>,
    pub output: Option<Ident>,
}

impl ProcedureRaw {
    fn resolve_idents(self, ctx: &Context, svc: &Ident) -> Result<Procedure, ParseError> {
        let prc = self;
        let input = match prc.input.as_ref() {
            Some(inp) => match ctx.lookup_ident(inp) {
                Some((tok, DefinitionType::Message)) => Some(tok),
                Some((_, type_)) => {
                    return Err(parse_error(
                        inp.line,
                        inp.col,
                        ParseErrorType::ProcedureInputNotMessage(
                            svc.clone(),
                            prc.ident.clone(),
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
            },
            None => None,
        };
        let output = match prc.output.as_ref() {
            Some(inp) => match ctx.lookup_ident(inp) {
                Some((tok, DefinitionType::Message)) => Some(tok),
                Some((_, type_)) => {
                    return Err(parse_error(
                        inp.line,
                        inp.col,
                        ParseErrorType::ProcedureOutpuNotMessage(
                            svc.clone(),
                            prc.ident.clone(),
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
            },
            None => None,
        };
        Ok(Procedure {
            verb: prc.verb,
            ident: prc.ident,
            input,
            output,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Procedure {
    pub verb: Verb,
    pub ident: Ident,
    pub input: Option<TypeToken>,
    pub output: Option<TypeToken>,
}

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

fn new_ident(line: usize, col: usize, value: CompactString) -> Ident {
    Ident { value, line, col }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
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
pub enum DefinitionRaw {
    Enum(Enum),
    OneOf(OneOfRaw),
    Message(MessageRaw),
}

impl DefinitionRaw {
    fn resolve_idents(self, ctx: &mut Context) -> Result<Definition, ParseError> {
        Ok(match self {
            DefinitionRaw::Enum(e) => Definition::Enum(e),
            DefinitionRaw::OneOf(oneof) => Definition::OneOf(oneof.resolve_idents(ctx)?),
            DefinitionRaw::Message(msg) => Definition::Message(msg.resolve_idents(ctx)?),
        })
    }
}

impl Definition {
    fn ident<'a>(&self, ctx: &'a Context) -> &'a Ident {
        match self {
            Definition::Enum(inner) => ctx.get_ident(inner.ident),
            Definition::OneOf(inner) => ctx.get_ident(inner.ident),
            Definition::Message(inner) => ctx.get_ident(inner.ident),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Enum {
    pub ident: TypeToken,
    pub variants: Vec<EnumVariant>,
    pub has_unknown: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub ident: Ident,
    pub value: u16,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OneOf {
    pub ident: TypeToken,
    pub definitions: Vec<Definition>,
    pub variants: Vec<OneOfVariant>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OneOfRaw {
    pub ident: TypeToken,
    pub definitions: Vec<DefinitionRaw>,
    pub variants: Vec<OneOfVariantRaw>,
}

impl OneOfRaw {
    fn resolve_idents(self, ctx: &mut Context) -> Result<OneOf, ParseError> {
        Ok(OneOf {
            ident: self.ident,
            definitions: self
                .definitions
                .into_iter()
                .map(|def| def.resolve_idents(ctx))
                .collect::<Result<_, ParseError>>()?,
            variants: self
                .variants
                .into_iter()
                .map(|var| var.resolve_idents(self.ident, ctx))
                .collect::<Result<_, ParseError>>()?,
        })
    }
}

impl OneOf {
    // all fields which point to `self` must be marked as requiring indirection
    fn set_indirection(&mut self, ctx: &Context) {
        for variant in self.variants.iter_mut() {
            if variant.indirection == Indirection::Indirect {
                // it has already been determined that this field should use indirection
                continue;
            }
            let tok = match variant.type_.base {
                TypeOrToken::Type(_) => continue, // this field does not point to a user-defined type
                TypeOrToken::Token(tok) => tok,
            };
            if can_reach(tok, self.ident, ctx) {
                // cycle detected -- add indirection
                variant.indirection = Indirection::Indirect;
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct OneOfVariant {
    pub ident: Ident,
    pub field_number: u16,
    pub type_: Type,
    pub indirection: Indirection,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OneOfVariantRaw {
    pub ident: Ident,
    pub field_number: u16,
    pub type_: TypeRaw,
    pub indirection: Indirection,
}

impl OneOfVariantRaw {
    fn resolve_idents(
        self,
        oneof_token: TypeToken,
        ctx: &mut Context,
    ) -> Result<OneOfVariant, ParseError> {
        let (type_base, indirection) = match self.type_.base {
            TypeOrIdent::Type(t) => (TypeOrToken::Type(t), Indirection::Direct),
            TypeOrIdent::Ident(i) => {
                let tok = ctx.lookup_ident_token(&i).ok_or(parse_error(
                    i.line,
                    i.col,
                    ParseErrorType::UnknownIdent(i.clone()),
                ))?;
                // if the field's user type is in an unsized list, it is indirectly referenced
                let indirection = if self
                    .type_
                    .modifiers
                    .iter()
                    .any(|m| matches!(m, ListSize::Unsized))
                {
                    Indirection::Indirect
                } else {
                    Indirection::Direct
                };
                // insert associations between this type and type referenced by this field
                ctx.add_link(oneof_token, tok, indirection);
                (TypeOrToken::Token(tok), indirection)
            }
        };
        Ok(OneOfVariant {
            ident: self.ident,
            field_number: self.field_number,
            type_: Type {
                modifiers: self.type_.modifiers,
                base: type_base,
            },
            indirection,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeOrIdent {
    Type(BaseType),
    Ident(Ident),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeOrToken {
    Type(BaseType),
    Token(TypeToken),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeNode {
    List(Option<usize>),
    TypeOrIdent(TypeOrIdent),
}

#[derive(Debug, PartialEq, Eq)]
enum ListSize {
    Unsized,
    Sized(usize),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeRaw {
    modifiers: smallvec::SmallVec<[ListSize; 4]>,
    base: TypeOrIdent,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Type {
    modifiers: smallvec::SmallVec<[ListSize; 4]>,
    base: TypeOrToken,
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

#[derive(Debug, PartialEq, Eq)]
pub struct Message {
    pub ident: TypeToken,
    pub definitions: Vec<Definition>,
    pub fields: Vec<MessageField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MessageRaw {
    pub ident: TypeToken,
    pub definitions: Vec<DefinitionRaw>,
    pub fields: Vec<MessageFieldRaw>,
}

impl MessageRaw {
    fn resolve_idents(self, ctx: &mut Context) -> Result<Message, ParseError> {
        ctx.push_scope_layer();
        ctx.pop_scope_layer();
        Ok(Message {
            ident: self.ident,
            definitions: self
                .definitions
                .into_iter()
                .map(|def| def.resolve_idents(ctx))
                .collect::<Result<_, ParseError>>()?,
            fields: self
                .fields
                .into_iter()
                .map(|fld| fld.resolve_idents(self.ident, ctx))
                .collect::<Result<_, ParseError>>()?,
        })
    }
}

impl Message {
    // all fields which point to `self` must be marked as requiring indirection
    fn set_indirection(&mut self, ctx: &Context) {
        for f in self.fields.iter_mut() {
            if f.indirection == Indirection::Indirect {
                // it has already been determined that this field should use indirection
                continue;
            }
            let tok = match f.type_.base {
                TypeOrToken::Type(_) => continue, // this field does not point to a user-defined type
                TypeOrToken::Token(tok) => tok,
            };
            if can_reach(tok, self.ident, ctx) {
                // cycle detected -- add indirection
                f.indirection = Indirection::Indirect;
            }
        }
    }
}

// is there a path through the type graph from `start` to `target`?
fn can_reach(start: TypeToken, target: TypeToken, ctx: &Context) -> bool {
    let mut seen: HashSet<TypeToken> = Default::default();
    let mut stack = vec![start];
    while let Some(tok) = stack.pop() {
        if tok == target {
            return true;
        }
        seen.insert(tok);
        stack.extend(
            ctx.types
                .direct_links_from(tok)
                .filter(|tok| !seen.contains(tok)),
        );
    }
    false
}

#[derive(Debug, PartialEq, Eq)]
pub struct MessageField {
    pub ident: Ident,
    pub field_number: u16,
    pub type_: Type,
    pub optional: bool,
    pub indirection: Indirection,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MessageFieldRaw {
    pub ident: Ident,
    pub field_number: u16,
    pub type_: TypeRaw,
    pub optional: bool,
    pub indirection: Indirection,
}

impl MessageFieldRaw {
    fn resolve_idents(
        self,
        msg_token: TypeToken,
        ctx: &mut Context,
    ) -> Result<MessageField, ParseError> {
        let (type_base, indirection) = match self.type_.base {
            TypeOrIdent::Type(t) => (TypeOrToken::Type(t), Indirection::Direct),
            TypeOrIdent::Ident(i) => {
                let tok = ctx.lookup_ident_token(&i).ok_or(parse_error(
                    i.line,
                    i.col,
                    ParseErrorType::UnknownIdent(i.clone()),
                ))?;
                // if the field's user type is in an unsized list, it is indirectly referenced
                let indirection = if self
                    .type_
                    .modifiers
                    .iter()
                    .any(|m| matches!(m, ListSize::Unsized))
                {
                    Indirection::Indirect
                } else {
                    Indirection::Direct
                };
                // insert associations between this message and type referenced by this field
                ctx.add_link(msg_token, tok, indirection);
                (TypeOrToken::Token(tok), indirection)
            }
        };
        Ok(MessageField {
            ident: self.ident,
            field_number: self.field_number,
            type_: Type {
                modifiers: self.type_.modifiers,
                base: type_base,
            },
            optional: self.optional,
            indirection,
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
pub struct TypeToken(usize);

// append-only sink for idents of user-defined types
#[derive(Debug, Default)]
struct Types {
    // TypeToken.0 is an index into this
    idents: Vec<Ident>,
    // type with ident at tup.0 points to type with ident at tup.1 with or
    // without indirection
    links: BTreeMap<(usize, usize), Indirection>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Indirection {
    Direct,
    Indirect,
}

impl Types {
    fn add_ident(&mut self, i: Ident) -> TypeToken {
        let len = self.idents.len();
        self.idents.push(i);
        TypeToken(len)
    }

    fn add_link(&mut self, from: TypeToken, to: TypeToken, dir: Indirection) {
        self.links.insert((from.0, to.0), dir);
    }

    fn direct_links_from<'a>(
        &'a self,
        tok: TypeToken,
    ) -> impl Iterator<Item = TypeToken> + use<'a> {
        let i = tok.0;
        self.links
            .range((i, 0)..(i + 1, 0))
            .filter(|tup| *tup.1 == Indirection::Direct)
            .map(|(k, _)| TypeToken(k.1))
    }
}
