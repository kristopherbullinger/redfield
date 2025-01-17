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
    /// A list of top-level user-authored messages in the document.
    pub messages: Vec<Message>,
    /// A list of top-level user-authored oneofs in the document.
    pub oneofs: Vec<OneOf>,
    /// A list of top-level user-authored messages in the document.
    pub enums: Vec<Enum>,
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
    /// A list of top-level user-authored messages in this OneOf.
    pub messages: Vec<Message>,
    /// A list of top-level user-authored oneofs in this OneOf.
    pub oneofs: Vec<OneOf>,
    /// A list of top-level user-authored messages in this OneOf.
    pub enums: Vec<Enum>,
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

/// A series of modifiers or wrappers that apply to a type, with the outermost container
/// appearing first.
#[derive(Debug)]
pub struct Containers(smallvec::SmallVec<[Container; 4]>);

impl Containers {
    pub fn as_slice(&self) -> &[Container] {
        self.0.as_slice()
    }
}

#[derive(Debug)]
pub struct Type {
    /// A series of wrappers or container types around this type, with the outermost container appearing first.
    /// For example, the type `List<List<u16; 3>>` contains two list containers: `[UnsizedList, SizedList(3)]`
    pub containers: Containers,
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
    Null,
}

/// Container types which wrap around a base or user [Type](crate::Type), with outermost containers appearing first.
/// For example, the type `List<List<u16; 3>>` is type u16 with containers `[UnsizedList, SizedList(3)]`.
#[derive(Debug, PartialEq, Eq)]
pub enum Container {
    /// The list has no specified size, ie `List<u16>`.
    UnsizedList,
    /// The list has a specified size, ie `List<u16; 3>`.
    SizedList(usize),
}

/// A basic, compound, struct- or object-like user-defined type.
#[derive(Debug)]
pub struct Message {
    pub name: Ident,
    /// A list of user-authored messages nested in this Message.
    pub messages: Vec<Message>,
    /// A list of user-authored oneofs nested in this Message.
    pub oneofs: Vec<OneOf>,
    /// A list of user-authored messages nested in this Message.
    pub enums: Vec<Enum>,
    /// This message's fields.
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
