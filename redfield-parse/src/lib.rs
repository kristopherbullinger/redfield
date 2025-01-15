#![deny(elided_lifetimes_in_paths)]
use compact_str::CompactString;
mod lexer;
mod parser;
pub use parser::ParseError;

/// Parse input into a document.
pub fn from_str(inp: &str) -> Result<Document, ParseError> {
    parser::document_from_str(inp)
}

/// The name of a type or field.
#[derive(Debug)]
pub struct Ident(CompactString);

impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// Represents a parsed and validated Redfield document.
#[derive(Debug)]
pub struct Document {
    /// The url at which the document's services are hosted.
    pub base_url: Option<String>,
    /// The services and their procedures defined in the document.
    pub services: Vec<Service>,
    /// A list of user-authored types defined at the top-level of the document.
    pub definitions: Vec<Definition>,
}

/// A service is a group of procedures hosted at a single URL.
#[derive(Debug)]
pub struct Service {
    /// The name of the service.
    pub name: Ident,
    /// The procedures (or "endpoints") available in this service.
    pub procedures: Vec<Procedure>,
}

/// A procedure is a single operation or "endpoint" exposed by a service.
#[derive(Debug)]
pub struct Procedure {
    pub verb: Verb,
    pub name: Ident,
    pub input: Option<Ident>,
    pub output: Option<Ident>,
}

/// The Verb corresponds to which HTTP Verb is associated with a procedure.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Verb {
    Get,
    Post,
}

/// A definitions is a user-defined type.
#[derive(Debug)]
pub enum Definition {
    Enum(Enum),
    OneOf(OneOf),
    Message(Message),
}

/// An Enum is a C-like enum, with each variant having a single integer value. It optionally also has an `UNKNOWN`
/// variant for capturing values not specified in the source document.
#[derive(Debug)]
pub struct Enum {
    pub name: Ident,
    pub variants: Vec<EnumVariant>,
    pub has_unknown: bool,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: Ident,
    pub value: u16,
}

/// A OneOf is similar to a `Message`, but it will always have exactly one of its variants set at a time.
#[derive(Debug)]
pub struct OneOf {
    pub name: Ident,
    pub definitions: Vec<Definition>,
    pub variants: Vec<OneOfVariant>,
}

#[derive(Debug)]
pub struct OneOfVariant {
    /// The name or label for this variant.
    pub name: Ident,
    pub field_number: u16,
    pub type_: Type,
    /// For code generation purposes, whether this variant should be put behind a pointer.
    pub indirection: Indirection,
}

/// A series of modifiers or wrappers that apply to a type.
#[derive(Debug)]
pub struct Modifiers(smallvec::SmallVec<[ListSize; 4]>);

impl Modifiers {
    pub fn as_slice(&self) -> &[ListSize] {
        self.0.as_slice()
    }
}

#[derive(Debug)]
pub struct Type {
    /// A series of modifiers to apply to this type. For example, the type `[][3]u16` contains two "list" modifiers.
    pub modifiers: Modifiers,
    /// The inner type to which all modifiers are applied.
    pub base: BaseOrUser,
}

/// Differentiate between redfield primitive types and user-defined types.
#[derive(Debug)]
pub enum BaseOrUser {
    Base(BaseType),
    User(Ident),
}

/// Indicates whether a Message field or OneOf variant should be behind a pointer in the generated code.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Indirection {
    /// The field or variant may directly occupy its position in the parent.
    Direct,
    /// The field or variant should be placed behind a pointer.
    Indirect,
}

/// The basic well-known types supported by redfield.
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

/// List types may be with or without a size.
#[derive(Debug, PartialEq, Eq)]
pub enum ListSize {
    /// The list has no specified size, ie `[]u16`.
    Unsized,
    /// The list has a specified size, ie `[3]u16`.
    Sized(usize),
}

/// A basic, compound, struct- or object-like user-defined type.
#[derive(Debug)]
pub struct Message {
    pub name: Ident,
    /// User type definitions nested in this message.
    pub definitions: Vec<Definition>,
    pub fields: Vec<MessageField>,
}

/// A field embedded in a message definition.
#[derive(Debug)]
pub struct MessageField {
    pub name: Ident,
    pub field_number: u16,
    pub type_: Type,
    /// Indicates whether this field may be omitted from the encoded form.
    pub optional: bool,
    /// Indicates whether generated data definitions should put this field behind a pointer.
    pub indirection: Indirection,
}

#[cfg(test)]
mod tests {
    use crate::from_str;

    #[test]
    fn parse_test_document_succeeds() {
        static DOC: &str = include_str!("../../sample_documents/pokemon.rdf");
        let res = from_str(DOC).unwrap();
        println!("{:#?}", res);
    }
}
