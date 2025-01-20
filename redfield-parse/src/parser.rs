use std::{
    collections::{BTreeMap, HashMap, HashSet},
    str::FromStr,
};

use crate::lexer;
use crate::BaseType;
use crate::Container;
use crate::Indirection;
use crate::Verb;
use compact_str::CompactString;
use smallvec::SmallVec;

const MAX_FIELD_NUMBER: u16 = u16::MAX >> 3;

fn to_type(type_: TypeRaw) -> crate::Type {
    crate::Type {
        containers: crate::Containers(type_.containers),
        base: match type_.base {
            BaseOrUser::Base(t) => crate::BaseOrUser::Base(t),
            BaseOrUser::User(i, _) => crate::BaseOrUser::User(crate::Ident(i.value)),
        },
    }
}

fn to_enum(e: Enum) -> crate::Enum {
    crate::Enum {
        name: crate::Ident(e.ident.value),
        variants: e
            .variants
            .into_iter()
            .map(|var| crate::EnumVariant {
                name: crate::Ident(var.ident.value),
                value: var.value,
            })
            .collect(),
        has_unknown: e.has_unknown,
    }
}

fn to_oneof(oneof: OneOfRaw) -> crate::OneOf {
    crate::OneOf {
        name: crate::Ident(oneof.ident.value),

        variants: oneof
            .variants
            .into_iter()
            .map(|var| crate::OneOfVariant {
                name: crate::Ident(var.ident.value),
                field_number: var.field_number,
                type_: to_type(var.type_),
                indirection: var.indirection,
            })
            .collect(),
        messages: oneof.messages.into_iter().map(to_message).collect(),
        oneofs: oneof.oneofs.into_iter().map(to_oneof).collect(),
        enums: oneof.enums.into_iter().map(to_enum).collect(),
    }
}

fn to_message(msg: MessageRaw) -> crate::Message {
    crate::Message {
        name: crate::Ident(msg.ident.value),

        fields: msg
            .fields
            .into_iter()
            .map(|fld| crate::MessageField {
                name: crate::Ident(fld.ident.value),
                field_number: fld.field_number,
                type_: to_type(fld.type_),
                optional: fld.optional,
                indirection: fld.indirection,
            })
            .collect(),
        messages: msg.messages.into_iter().map(to_message).collect(),
        oneofs: msg.oneofs.into_iter().map(to_oneof).collect(),
        enums: msg.enums.into_iter().map(to_enum).collect(),
    }
}

// checks for uniqueness of the names of all user types
fn unique_names<'a>(
    enms: &'a [Enum],
    oneofs: &'a [OneOfRaw],
    messages: &'a [MessageRaw],
) -> Result<(), ParseError> {
    let mut names = HashSet::<&str>::new();
    for item in enms {
        if !names.insert(item.ident.value.as_str()) {
            return Err(parse_error(
                item.ident.line,
                item.ident.col,
                ParseErrorType::DuplicateIdentifier(item.ident.clone()),
            ));
        }
    }
    for item in oneofs {
        if !names.insert(item.ident.value.as_str()) {
            return Err(parse_error(
                item.ident.line,
                item.ident.col,
                ParseErrorType::DuplicateIdentifier(item.ident.clone()),
            ));
        }
    }
    for item in messages {
        if !names.insert(item.ident.value.as_str()) {
            return Err(parse_error(
                item.ident.line,
                item.ident.col,
                ParseErrorType::DuplicateIdentifier(item.ident.clone()),
            ));
        }
    }
    Ok(())
}

fn to_document(doc: Document) -> crate::Document {
    let mut services: Vec<crate::Service> = vec![];
    for svc in doc.services {
        services.push(crate::Service {
            base_url: svc.base_url.map(|s| s.to_string()),
            name: crate::Ident(svc.ident.value),
            procedures: svc
                .procedures
                .into_iter()
                .map(|prc| crate::Procedure {
                    verb: prc.verb,
                    name: crate::Ident(prc.ident.value),
                    input: prc.input.map(|n| crate::Ident(n.value)),
                    output: prc.output.map(|n| crate::Ident(n.value)),
                })
                .collect(),
        })
    }
    crate::Document {
        // base_url: doc.base_url.map(|s| s.into_string()),
        services,
        messages: doc.messages.into_iter().map(to_message).collect(),
        oneofs: doc.oneofs.into_iter().map(to_oneof).collect(),
        enums: doc.enums.into_iter().map(to_enum).collect(),
    }
}

pub(crate) fn document_from_str(inp: &str) -> Result<crate::Document, ParseError> {
    let mut iter = lexer::lex_document_iter(inp);
    let Some(tok) = iter.next() else {
        return Ok(crate::Document {
            services: vec![],
            messages: vec![],
            oneofs: vec![],
            enums: vec![],
        });
    };

    // parse top-level definitions
    let mut messages: Vec<MessageRaw> = vec![];
    let mut oneofs: Vec<OneOfRaw> = vec![];
    let mut enums: Vec<Enum> = vec![];
    let mut services: Vec<ServiceRaw> = vec![];
    let mut token_gen = TokenGenerator::default();
    let mut tok = tok?;
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
                let msg = munch_message(&mut iter, &mut token_gen)?;
                messages.push(msg);
            }
            lexer::TokenType::KeywordEnum => {
                let enm = munch_enum(&mut iter, &mut token_gen)?;
                enums.push(enm);
            }
            lexer::TokenType::KeywordOneof => {
                let oneof = munch_oneof(&mut iter, &mut token_gen)?;
                oneofs.push(oneof);
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
    unique_names(&enums, &oneofs, &messages)?;
    // ensure service names don't collide with data definition names and validate svc inp/outputs
    {
        let mut toplevel_names: HashMap<&str, DefinitionType> = Default::default();
        for msg in messages.iter() {
            toplevel_names.insert(msg.ident.value.as_str(), DefinitionType::Message);
        }
        for oneof in oneofs.iter() {
            toplevel_names.insert(oneof.ident.value.as_str(), DefinitionType::OneOf);
        }
        for enm in enums.iter() {
            toplevel_names.insert(enm.ident.value.as_str(), DefinitionType::Enum);
        }
        validate_svcs(&services, &toplevel_names)?
    };

    // validate idents and add type associations
    let mut scopes = {
        // initialize the global scope with top-level idents
        let mut scopes = Scopes::default();
        for msg in messages.iter() {
            scopes.global.insert(msg.ident.value.clone(), msg.token);
        }
        for oneof in oneofs.iter() {
            scopes.global.insert(oneof.ident.value.clone(), oneof.token);
        }
        for enm in enums.iter() {
            scopes.global.insert(enm.ident.value.clone(), enm.token);
        }
        scopes
    };
    // do field types point to valid user-defined types? if they do,
    // record associations between the definition and pointed-to types
    let mut links = Links::default();
    for msg in messages.iter_mut() {
        msg.validate_fields_and_insert_links(&mut scopes, &mut links)?;
    }
    for oneof in oneofs.iter_mut() {
        oneof.validate_fields_and_insert_links(&mut scopes, &mut links)?;
    }
    // detect recursion of user-defined types and insert indirection
    {
        for msg in messages.iter_mut() {
            let target = msg.token;
            for field in msg.fields.iter_mut() {
                if let (Indirection::Direct, BaseOrUser::User(_, tok)) =
                    (field.indirection, &field.type_.base)
                {
                    let start = *tok;
                    insert_indirection(start, target, &mut field.indirection, &mut links);
                }
            }
        }
        for oneof in oneofs.iter_mut() {
            let target = oneof.token;
            for var in oneof.variants.iter_mut() {
                if let (Indirection::Direct, BaseOrUser::User(_, tok)) =
                    (var.indirection, &var.type_.base)
                {
                    let start = *tok;
                    insert_indirection(start, target, &mut var.indirection, &mut links);
                }
            }
        }
    }
    let doc = Document {
        services,
        messages,
        oneofs,
        enums,
    };
    Ok(to_document(doc))
}

// if the type graph can be traversed from `field` to `container`, there is type recursion
// and indirection must be inserted
fn insert_indirection(
    // the type-token of a oneof variant or message field
    field: Token,
    // the type-token of the containing oneof or message
    container: Token,
    indir: &mut Indirection,
    links: &mut BTreeMap<(usize, usize), Indirection>,
) {
    let mut seen = HashSet::<Token>::new();
    let mut stack: Vec<Token> = vec![field];
    while let Some(tok) = stack.pop() {
        if tok == container {
            *indir = Indirection::Indirect;
            // the container indirectly contains the field
            links.insert((container.0, field.0), Indirection::Indirect);
            break;
        } else {
            seen.insert(tok);
            stack.extend(direct_links_from(tok, &*links).filter(|tok| !seen.contains(tok)));
        }
    }
}

// ensure all procedure inputs and outputs refer to a valid message type
// and that service names don't collide with names of user types
fn validate_svcs(
    services: &[ServiceRaw],
    top_level_identifiers: &HashMap<&str, DefinitionType>,
) -> Result<(), ParseError> {
    for svc in services.iter() {
        if top_level_identifiers.contains_key(svc.ident.as_str()) {
            return Err(parse_error(
                svc.ident.line,
                svc.ident.col,
                ParseErrorType::DuplicateIdentifier(svc.ident.clone()),
            ));
        }
        for prc in svc.procedures.iter() {
            prc.validate_idents(&svc.ident, top_level_identifiers)?;
        }
    }
    Ok(())
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
        lexer::TokenType::Literal(lexer::Literal::String(s)) => Ok(s),
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
    let mut base_url: Option<CompactString> = None;
    loop {
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::Ident(s) => {
                if s == "base_url" {
                    if base_url.is_some() {
                        return Err(parse_error(
                            tok.line,
                            tok.span.0,
                            ParseErrorType::DuplicateServiceBaseUrl,
                        ));
                    }
                    base_url = Some(munch_base_url(&mut *iter)?);
                } else {
                    return Err(parse_error(
                        tok.line,
                        tok.span.0,
                        ParseErrorType::UnexpectedIdent(new_ident(tok.line, tok.span.0, s)),
                    ));
                }
            }
            lexer::TokenType::Verb(verb) => {
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
                            ParseErrorType::ExpectedArrowOrSemicolon,
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
                return Ok(ServiceRaw {
                    base_url,
                    ident,
                    procedures,
                });
            }
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::ExpectedProcedureVerb,
                ))
            }
        };
    }
}

fn munch_type(iter: &mut lexer::TokenIter<'_>) -> Result<TypeRaw, ParseError> {
    // series of containers around the base type, with outermost appearing first
    let mut containers = SmallVec::<[Container; 4]>::new();
    enum StackItem {
        CloseList,
    }
    let mut queue: Vec<StackItem> = Default::default();
    let base = loop {
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::KeywordList => {
                expect_next_equals(&mut *iter, lexer::TokenType::LAngleBracket)?;
                queue.push(StackItem::CloseList);
            }
            lexer::TokenType::Ident(i) => {
                // UNRESOLVED overwritten later after type checking
                break BaseOrUser::User(new_ident(tok.line, tok.span.0, i), UNRESOLVED);
            }
            lexer::TokenType::BaseType(t) => {
                break BaseOrUser::Base(t);
            }
            _ => {
                return Err(parse_error(
                    tok.line,
                    tok.span.0,
                    ParseErrorType::ExpectedSemicolonOrRightAngleBracket,
                ))
            }
        }
    };
    while let Some(item) = queue.pop() {
        let tok = next_not_eof(iter)?;
        match item {
            StackItem::CloseList => match tok.type_ {
                lexer::TokenType::Semicolon => {
                    let sz = munch_decimal_number(&mut *iter)?;
                    containers.push(Container::SizedList(sz.val));
                    expect_next_equals(iter, lexer::TokenType::RAngleBracket)?;
                }
                lexer::TokenType::RAngleBracket => {
                    containers.push(Container::UnsizedList);
                }
                _ => {
                    return Err(parse_error(
                        tok.line,
                        tok.span.0,
                        ParseErrorType::ExpectedSemicolonOrRightAngleBracket,
                    ))
                }
            },
        }
    }
    containers.reverse();
    Ok(TypeRaw { containers, base })
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
        _ => Err(parse_error(
            tok.line,
            tok.span.0,
            ParseErrorType::ExpectedIntegerLiteral,
        )),
    }
}

fn munch_field_number(iter: &mut lexer::TokenIter<'_>) -> Result<WithPosition<u16>, ParseError> {
    let f = munch_decimal_number::<u16>(iter)?;
    if f.val > MAX_FIELD_NUMBER {
        return Err(parse_error(
            f.line,
            f.span.0,
            ParseErrorType::FieldNumberTooLarge(f.val),
        ));
    }
    Ok(f)
}

type ScopeLayer = HashMap<CompactString, Token>;
#[derive(Debug, Default)]
struct Scopes {
    global: ScopeLayer,
    nested: Vec<ScopeLayer>,
}

// pops the scope stack when dropped so we don't forget
struct ScopeGuard<'a>(&'a mut Scopes);
impl<'a> std::ops::Drop for ScopeGuard<'a> {
    fn drop(&mut self) {
        self.0.nested.pop();
    }
}

impl Scopes {
    fn next(&mut self) -> ScopeGuard<'_> {
        self.nested.push(Default::default());
        ScopeGuard(self)
    }

    fn deepest_mut(&mut self) -> &mut ScopeLayer {
        match self.nested.last_mut() {
            Some(s) => s,
            None => &mut self.global,
        }
    }

    fn lookup_ident(&self, i: &Ident) -> Option<Token> {
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

// assumes "message" keyword already chomped
fn munch_message(
    iter: &mut lexer::TokenIter<'_>,
    token_gen: &mut TokenGenerator,
) -> Result<MessageRaw, ParseError> {
    let ident = munch_ident(&mut *iter)?;
    let token = token_gen.next();
    let mut messages: Vec<MessageRaw> = vec![];
    let mut oneofs: Vec<OneOfRaw> = vec![];
    let mut enums: Vec<Enum> = vec![];
    let mut fields: Vec<MessageFieldRaw> = vec![];
    // test for field number uniqueness
    let mut field_numbers = HashSet::new();
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    let msg = loop {
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::KeywordEnum => {
                let enm = munch_enum(&mut *iter, &mut *token_gen)?;
                enums.push(enm);
            }
            lexer::TokenType::KeywordOneof => {
                let oneof = munch_oneof(&mut *iter, &mut *token_gen)?;
                oneofs.push(oneof);
            }
            lexer::TokenType::KeywordMessage => {
                let msg = munch_message(&mut *iter, &mut *token_gen)?;
                messages.push(msg);
            }
            lexer::TokenType::Ident(name) => {
                let ident = new_ident(tok.line, tok.span.0, name);
                // ident@4?:
                expect_next_equals(&mut *iter, lexer::TokenType::AtSign)?;
                let field_number = {
                    let munched = munch_field_number(&mut *iter)?;
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
                let indirection = if type_
                    .containers
                    .iter()
                    .any(|m| matches!(m, Container::UnsizedList))
                {
                    Indirection::Indirect
                } else {
                    Indirection::Direct
                };
                fields.push(MessageFieldRaw {
                    ident,
                    field_number,
                    type_,
                    optional,
                    indirection,
                });
                expect_next_equals(&mut *iter, lexer::TokenType::Comma)?;
            }
            lexer::TokenType::RCurly => {
                let mut names: HashSet<&str> = Default::default();
                for ident in fields.iter().map(|fld| &fld.ident) {
                    // 1. ensure all fields have unique names
                    if !names.insert(ident.as_str()) {
                        return Err(parse_error(
                            ident.line,
                            ident.col,
                            ParseErrorType::DuplicateIdentifier(ident.clone()),
                        ));
                    }
                }
                unique_names(&enums, &oneofs, &messages)?;
                break MessageRaw {
                    ident,
                    token,
                    messages,
                    oneofs,
                    enums,
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
        _ => Err(parse_error(line, col, ParseErrorType::ExpectedIdent)),
    }
}

// assumed "enum" keyword already chomped
fn munch_enum(
    iter: &mut lexer::TokenIter<'_>,
    token_gen: &mut TokenGenerator,
) -> Result<Enum, ParseError> {
    // munch ident
    let ident = munch_ident(&mut *iter)?;
    let token = token_gen.next();
    // munch "{"
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    // munch { variant } [ "UNKNOWN" ] "}"
    let mut variants: Vec<EnumVariant> = vec![];
    let mut values: HashSet<u16> = Default::default();
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
                } else if has_unknown {
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
                            lexer::Literal::BinaryNumber(n) => u16::from_str_radix(n.as_str(), 2)
                                .map_err(|_| {
                                parse_error(line, col, ParseErrorType::IntegerParseError)
                            })?,
                            lexer::Literal::HexNumber(n) => u16::from_str_radix(n.as_str(), 16)
                                .map_err(|_| {
                                    parse_error(line, col, ParseErrorType::IntegerParseError)
                                })?,
                            lexer::Literal::Float(_) | lexer::Literal::String(_) => {
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
                    if !values.insert(value) {
                        return Err(parse_error(
                            line,
                            col,
                            ParseErrorType::DuplicateEnumVariantValue(ident, value),
                        ));
                    }
                    variants.push(EnumVariant {
                        ident: new_ident(tok.line, tok.span.0, name),
                        value,
                    });
                };
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
                    ident,
                    token,
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
fn munch_oneof(
    iter: &mut lexer::TokenIter<'_>,
    token_gen: &mut TokenGenerator,
) -> Result<OneOfRaw, ParseError> {
    let ident = munch_ident(&mut *iter)?;
    let token = token_gen.next();
    expect_next_equals(&mut *iter, lexer::TokenType::LCurly)?;
    let mut messages: Vec<MessageRaw> = vec![];
    let mut oneofs: Vec<OneOfRaw> = vec![];
    let mut enums: Vec<Enum> = vec![];
    let mut variants: Vec<OneOfVariantRaw> = vec![];
    let mut field_numbers = HashSet::new();
    loop {
        let tok = next_not_eof(iter)?;
        match tok.type_ {
            lexer::TokenType::RCurly => {
                // ensure field names are unique
                let mut names: HashSet<&str> = Default::default();
                for var in variants.iter() {
                    if !names.insert(var.ident.as_str()) {
                        return Err(parse_error(
                            tok.line,
                            tok.span.0,
                            ParseErrorType::DuplicateFieldName(var.ident.clone()),
                        ));
                    }
                }
                unique_names(&enums, &oneofs, &messages)?;
                return Ok(OneOfRaw {
                    ident,
                    token,
                    oneofs,
                    messages,
                    enums,
                    variants,
                });
            }
            lexer::TokenType::KeywordEnum => {
                let enm = munch_enum(iter, token_gen)?;
                enums.push(enm);
            }
            lexer::TokenType::KeywordOneof => {
                let oneof = munch_oneof(iter, token_gen)?;
                oneofs.push(oneof);
            }
            lexer::TokenType::KeywordMessage => {
                let msg = munch_message(iter, token_gen)?;
                messages.push(msg);
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
                let indirection = if type_
                    .containers
                    .iter()
                    .any(|m| matches!(m, Container::UnsizedList))
                {
                    Indirection::Indirect
                } else {
                    Indirection::Direct
                };
                expect_next_equals(iter, lexer::TokenType::Comma)?;
                variants.push(OneOfVariantRaw {
                    ident: new_ident(tok.line, tok.span.0, field_name),
                    field_number,
                    type_,
                    indirection,
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

impl ParseError {
    // One-based line of the input on which the error occurred
    pub fn line(&self) -> usize {
        self.line + 1
    }

    // One-based column on the line which an error occurred
    pub fn column(&self) -> usize {
        self.col + 1
    }
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
pub(crate) enum ParseErrorType {
    DuplicateEnumVariantValue(Ident, u16),
    DuplicateFieldName(Ident),
    ExpectedSemicolonOrRightAngleBracket,
    DuplicateFieldNumber(u16),
    FieldNumberTooLarge(u16),
    DuplicateIdentifier(Ident),
    ExpectedColonOrQuestionMark,
    DuplicateServiceBaseUrl,
    InvalidSyntax(lexer::ParseErrorType),
    ExpectedEnumMessageOneOfOrIdent,
    TopLevelIdentNotAtBeginningOfFile(Ident),
    DuplicatedEnumVariantUnknown,
    IntegerParseError,
    EnumVariantUnknownNotLast,
    Expected(lexer::TokenType),
    ExpectedIdent,
    ExpectedIdentOrRParen,
    ExpectedProcedureVerb,
    ExpectedArrowOrSemicolon,
    ExpectedStringLiteral,
    ExpectedIdentOrRCurly,
    ExpectedServiceMessageEnumOrOneof,
    ExpectedIntegerLiteral,
    // (service name,  procedure name, input name, procedure input type)
    ProcedureInputNotMessage(Box<(Ident, Ident, Ident, DefinitionType)>),
    // (service name,  procedure name, procedure output type)
    ProcedureOutpuNotMessage(Box<(Ident, Ident, Ident, DefinitionType)>),
    UnknownIdent(Ident),
    UnexpectedIdent(Ident),
    UnexpectedEof,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum DefinitionType {
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
        writeln!(f, "Failed to parse document:")?;
        writeln!(f, "line: {}", self.line + 1)?;
        writeln!(f, " col: {}", self.col + 1)?;
        match self.type_ {
            ParseErrorType::ExpectedColonOrQuestionMark => write!(f, "expected `:` or `?`")?,
            ParseErrorType::DuplicateEnumVariantValue(ref i, v) => {
                write!(f, "duplicate variant value in enum `{}`: {}", i.as_str(), v)?
            }
            ParseErrorType::InvalidSyntax(ref e) => match e {
                lexer::ParseErrorType::UnexpectedEof => write!(f, "unexpected end of input")?,
                lexer::ParseErrorType::UnknownValue => write!(f, "unknown token")?,
                lexer::ParseErrorType::ParseLiteral(_) => {
                    write!(f, "failed to parse value literal")?
                }
            },
            ParseErrorType::ExpectedEnumMessageOneOfOrIdent => write!(
                f,
                "expected keyword `enum`, keyword `message`, keyword `oneof`, or identifier"
            )?,
            ParseErrorType::TopLevelIdentNotAtBeginningOfFile(ref i) => write!(
                f,
                "top-level identifiers must appear at the top of the file; identifier `{}`",
                i.value,
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
            ParseErrorType::ExpectedProcedureVerb => {
                write!(f, "expected a procedure verb `GET` or `POST`")?
            }
            ParseErrorType::ExpectedArrowOrSemicolon => write!(f, "expected `->` or `;`")?,
            ParseErrorType::ExpectedStringLiteral => write!(f, "expected a string literal")?,
            ParseErrorType::ExpectedIdentOrRCurly => write!(f, "expected identifier or `}}`")?,
            ParseErrorType::ExpectedServiceMessageEnumOrOneof => write!(
                f,
                "expected keyword `enum`, `oneof`, `message`, or`service`"
            )?,
            ParseErrorType::ExpectedIntegerLiteral => write!(f, "expected an integer literal")?,
            ParseErrorType::UnexpectedEof => write!(f, "unexpected end of input")?,
            ParseErrorType::DuplicateFieldName(ref i) => {
                write!(f, "duplicate field name `{}`", i.value)?
            }
            ParseErrorType::DuplicateFieldNumber(n) => write!(f, "duplicate field number `{}`", n)?,
            ParseErrorType::DuplicateIdentifier(ref i) => {
                write!(f, "duplicate identifier name `{}`", i.value)?;
            }
            ParseErrorType::ProcedureInputNotMessage(ref inner) => {
                let (ref svc, ref prc, ref inp, typ) = **inner;
                write!(f, "procedure input types must be of type `message`: service `{}` procedure `{}` input `{}` is of type {}", svc.as_str(), prc.as_str(), inp.as_str(), typ.as_str())?;
            }
            ParseErrorType::ProcedureOutpuNotMessage(ref inner) => {
                let (ref svc, ref prc, ref outpt, typ) = **inner;
                write!(f, "procedure output types must be of type `message`: service `{}` procedure `{}` output `{}` is of type {}", svc.as_str(), prc.as_str(), outpt.as_str(), typ.as_str())?;
            }
            ParseErrorType::UnknownIdent(ref i) => {
                write!(f, "could not find definition for ident `{}`", i.value)?;
            }
            ParseErrorType::FieldNumberTooLarge(n) => write!(
                f,
                "field with number {} exceeds maximum of {}",
                n, MAX_FIELD_NUMBER
            )?,
            ParseErrorType::ExpectedSemicolonOrRightAngleBracket => {
                write!(f, "expected `;` or `>`")?;
            }
            ParseErrorType::DuplicateServiceBaseUrl => {
                write!(f, "duplicate service base url")?;
            }
            ParseErrorType::UnexpectedIdent(ref i) => {
                write!(f, "unexpected ident `{}`", i.value)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
pub(crate) struct Document {
    pub(crate) services: Vec<ServiceRaw>,
    pub(crate) messages: Vec<MessageRaw>,
    pub(crate) oneofs: Vec<OneOfRaw>,
    pub(crate) enums: Vec<Enum>,
}

// a service with raw idents for inputs and outputs
#[derive(Debug, PartialEq, Eq, Default)]
pub(crate) struct ServiceRaw {
    pub(crate) base_url: Option<CompactString>,
    pub(crate) ident: Ident,
    pub(crate) procedures: Vec<ProcedureRaw>,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ProcedureRaw {
    pub(crate) verb: Verb,
    pub(crate) ident: Ident,
    pub(crate) input: Option<Ident>,
    pub(crate) output: Option<Ident>,
}

impl ProcedureRaw {
    fn validate_idents(
        &self,
        svc: &Ident,
        top_level_identifiers: &HashMap<&str, DefinitionType>,
    ) -> Result<(), ParseError> {
        let prc = self;
        if let Some(inp) = prc.input.as_ref() {
            match top_level_identifiers.get(inp.as_str()).copied() {
                Some(DefinitionType::Message) => {}
                Some(type_) => {
                    return Err(parse_error(
                        inp.line,
                        inp.col,
                        ParseErrorType::ProcedureInputNotMessage(Box::new((
                            svc.clone(),
                            prc.ident.clone(),
                            inp.clone(),
                            type_,
                        ))),
                    ))
                }
                None => {
                    return Err(parse_error(
                        inp.line,
                        inp.col,
                        ParseErrorType::UnknownIdent(inp.clone()),
                    ))
                }
            };
        };
        if let Some(inp) = prc.output.as_ref() {
            match top_level_identifiers.get(inp.as_str()).copied() {
                Some(DefinitionType::Message) => {}
                Some(type_) => {
                    return Err(parse_error(
                        inp.line,
                        inp.col,
                        ParseErrorType::ProcedureOutpuNotMessage(Box::new((
                            svc.clone(),
                            prc.ident.clone(),
                            inp.clone(),
                            type_,
                        ))),
                    ))
                }
                None => {
                    return Err(parse_error(
                        inp.line,
                        inp.col,
                        ParseErrorType::UnknownIdent(inp.clone()),
                    ))
                }
            };
        };
        Ok(())
    }
}

impl Ident {
    pub(crate) fn as_str(&self) -> &str {
        self.value.as_str()
    }
}

#[derive(Clone, PartialEq, Eq, Default, Debug, Hash)]
pub struct Ident {
    pub value: CompactString,
    pub line: usize,
    pub col: usize,
}

fn new_ident(line: usize, col: usize, value: CompactString) -> Ident {
    Ident { value, line, col }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Enum {
    pub(crate) ident: Ident,
    pub(crate) token: Token,
    pub(crate) variants: Vec<EnumVariant>,
    pub(crate) has_unknown: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct EnumVariant {
    pub(crate) ident: Ident,
    pub(crate) value: u16,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct OneOfRaw {
    pub(crate) ident: Ident,
    pub(crate) token: Token,
    pub(crate) messages: Vec<MessageRaw>,
    pub(crate) oneofs: Vec<OneOfRaw>,
    pub(crate) enums: Vec<Enum>,
    pub(crate) variants: Vec<OneOfVariantRaw>,
}

impl OneOfRaw {
    fn validate_fields_and_insert_links(
        &mut self,
        scopes: &mut Scopes,
        links: &mut Links,
    ) -> Result<(), ParseError> {
        let _guard = scopes.next();
        let scopes = &mut *_guard.0;
        let layer = scopes.deepest_mut();
        // insert declarations into scope
        for msg in self.messages.iter() {
            layer.insert(msg.ident.value.clone(), msg.token);
        }
        for oneof in self.oneofs.iter() {
            layer.insert(oneof.ident.value.clone(), oneof.token);
        }
        for enm in self.enums.iter() {
            layer.insert(enm.ident.value.clone(), enm.token);
        }
        // validate nested definitions
        for msg in self.messages.iter_mut() {
            msg.validate_fields_and_insert_links(scopes, links)?;
        }
        for oneof in self.oneofs.iter_mut() {
            oneof.validate_fields_and_insert_links(scopes, links)?;
        }
        // validate variants
        for var in self.variants.iter_mut() {
            insert_link(
                self.token,
                &mut var.type_.base,
                var.indirection,
                scopes,
                links,
            )?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct OneOfVariantRaw {
    pub(crate) ident: Ident,
    pub(crate) field_number: u16,
    pub(crate) type_: TypeRaw,
    pub(crate) indirection: Indirection,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum BaseOrUser {
    Base(BaseType),
    User(Ident, Token),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct TypeRaw {
    containers: smallvec::SmallVec<[Container; 4]>,
    base: BaseOrUser,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MessageRaw {
    pub(crate) ident: Ident,
    pub(crate) token: Token,
    pub(crate) messages: Vec<MessageRaw>,
    pub(crate) oneofs: Vec<OneOfRaw>,
    pub(crate) enums: Vec<Enum>,
    pub(crate) fields: Vec<MessageFieldRaw>,
}

impl MessageRaw {
    fn validate_fields_and_insert_links(
        &mut self,
        scopes: &mut Scopes,
        links: &mut Links,
    ) -> Result<(), ParseError> {
        let _guard = scopes.next();
        let scopes = &mut *_guard.0;
        let layer = scopes.deepest_mut();
        // insert declarations into scope
        for msg in self.messages.iter() {
            layer.insert(msg.ident.value.clone(), msg.token);
        }
        for oneof in self.oneofs.iter() {
            layer.insert(oneof.ident.value.clone(), oneof.token);
        }
        for enm in self.enums.iter() {
            layer.insert(enm.ident.value.clone(), enm.token);
        }
        // validate nested definitions
        for msg in self.messages.iter_mut() {
            msg.validate_fields_and_insert_links(scopes, links)?;
        }
        for oneof in self.oneofs.iter_mut() {
            oneof.validate_fields_and_insert_links(scopes, links)?;
        }
        // validate fields
        for fld in self.fields.iter_mut() {
            insert_link(
                self.token,
                &mut fld.type_.base,
                fld.indirection,
                scopes,
                links,
            )?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MessageFieldRaw {
    pub(crate) ident: Ident,
    pub(crate) field_number: u16,
    pub(crate) type_: TypeRaw,
    pub(crate) optional: bool,
    pub(crate) indirection: Indirection,
}

// attempt to add association between the type indicated by `start_token` and
// a user-defined type inside `base_type`, if it is one;
// error if the type's ident is not known
fn insert_link(
    // the token for the container type
    start_token: Token,
    // the type of one of the fields in the container
    base_type: &mut BaseOrUser,
    indirection: Indirection,
    scopes: &mut Scopes,
    links: &mut Links,
) -> Result<(), ParseError> {
    match base_type {
        BaseOrUser::Base(_) => {}
        BaseOrUser::User(ref i, ref mut field_type_token) => {
            let tok = scopes.lookup_ident(i).ok_or(parse_error(
                i.line,
                i.col,
                ParseErrorType::UnknownIdent(i.clone()),
            ))?;
            // overwrite UNRESOLVED and finalize type association
            *field_type_token = tok;
            // insert associations between this message and type referenced by this field
            links.insert((start_token.0, tok.0), indirection);
        }
    };
    Ok(())
}

// a globally-unique identifier for a user-defined type
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
pub(crate) struct Token(usize);

// inserted into field and variant type references prior to type checking
// and overwritten if type check succeeds
const UNRESOLVED: Token = Token(usize::MAX);

type Links = BTreeMap<(usize, usize), Indirection>;

// append-only sink for idents of user-defined types
#[derive(Debug, Default)]
struct TokenGenerator(usize);
impl TokenGenerator {
    fn next(&mut self) -> Token {
        let n = self.0;
        self.0 += 1;
        Token(n)
    }
}

fn direct_links_from(tok: Token, links: &Links) -> impl Iterator<Item = Token> + use<'_> {
    let i = tok.0;
    links
        .range((i, 0)..(i + 1, 0))
        .filter(|tup| *tup.1 == Indirection::Direct)
        .map(|(k, _)| Token(k.1))
}
