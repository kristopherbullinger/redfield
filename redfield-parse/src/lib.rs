#![deny(elided_lifetimes_in_paths)]
use std::collections::{hash_map::Entry, BTreeSet, HashMap, HashSet};

use compact_str::CompactString;
mod lexer;
mod parser;

#[derive(Debug)]
pub struct Error(Error_);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "file: {}", self.0.filename)?;
        match self.0.type_ {
            ErrorType::Io(ref e) => writeln!(f, "caused by: io error: {}", e)?,
            ErrorType::Parse(ref p) => writeln!(f, "caused by: syntax error: {}", p)?,
            ErrorType::ImportCycle => writeln!(f, "import cycle detected")?,
        };
        Ok(())
    }
}

#[derive(Debug)]
struct Error_ {
    filename: String,
    type_: ErrorType,
}

#[derive(Debug)]
enum ErrorType {
    Io(std::io::Error),
    Parse(parser::ParseError),
    ImportCycle,
}

fn parse_raw_document_from_file(
    path: impl AsRef<str>,
    token_gen: &mut parser::TokenGenerator,
) -> Result<parser::Document, Error> {
    let path = path.as_ref();
    // - read contents of file
    let contents = std::fs::read_to_string(path).map_err(|e| {
        Error(Error_ {
            filename: path.to_string(),
            type_: ErrorType::Io(e),
        })
    })?;
    let doc = parser::parse_raw_document_from_str(contents.as_str(), token_gen).map_err(|e| {
        Error(Error_ {
            filename: path.to_string(),
            type_: ErrorType::Parse(e),
        })
    })?;
    Ok(doc)
}

/// Parse the given file and all of its dependencies
pub fn parse_file(p: impl AsRef<str>) -> Result<Vec<Document>, Error> {
    let mut token_gen = parser::TokenGenerator::new();
    let mut path = camino::Utf8PathBuf::from(p.as_ref());
    // - read contents of file
    let doc = parse_raw_document_from_file(path.as_str(), &mut token_gen)?;
    // Vec<Document> <- "FileToken" is the position in this vec of a file, root file is position 0
    // push first document into sink
    // initialize stack with 0
    // pop item off stack
    // retrieve from sink with stack item
    // for all imports which haven't already been visited,
    //   parse the document, put it into the sink, push
    //   its id into the stack, and mark it as seen
    let mut docs: Vec<parser::Document> = vec![doc];
    let mut imports: BTreeSet<(usize, usize)> = BTreeSet::new();
    let mut filenames: Vec<CompactString> = vec![path.file_stem().unwrap().into()];
    // map of filename without extension to the position in docs
    let mut document_ids: HashMap<CompactString, usize> = HashMap::default();
    {
        let mut stack = vec![0];
        while let Some(doc_id) = stack.pop() {
            for d in 0..docs[doc_id].imports.len() {
                let doc = &docs[doc_id];
                let import = &doc.imports[d];
                let imported_doc_id: usize = match document_ids.entry(import.name.value.clone()) {
                    Entry::Occupied(o) => *o.get(),
                    Entry::Vacant(v) => {
                        path.pop();
                        filenames.push(import.name.value.clone());
                        path.push(format!("{}.rdf", import.name.value));
                        let imported_doc =
                            parse_raw_document_from_file(path.as_str(), &mut token_gen)?;
                        let imported_doc_id = docs.len();
                        v.insert(imported_doc_id);
                        docs.push(imported_doc);
                        stack.push(imported_doc_id);
                        imported_doc_id
                    }
                };
                imports.insert((doc_id, imported_doc_id));
            }
        }
    }
    // HashMap<String, FileToken> <- query file "id" (token) by name
    // BTreeSet<(FileToken, FileToken)> <- import graph: item.0 imports item.1
    // Vec<FileToken> <- toposorted nodes, import cycle exists if toposort not possible

    // - repeat for all imported documents
    // - form graph of imports -- reject if import cycle detected
    // - toposort import graph
    // - perform typechecking on leaf nodes until all documents complete
    //     - for each document that contains imports, provide access to top-level
    //       data definitions from each import for type checking
    let docs = resolve_deps(docs, &*filenames, &imports)?;
    Ok(docs)
}

/// The name of a type or field.
#[derive(Debug)]
pub struct Ident(CompactString);

/// The name of a type along with its namespace, if it was imported from another document.
#[derive(Debug)]
pub struct FullIdent {
    pub namespace: Option<Ident>,
    pub name: Ident,
}

impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// Represents a parsed and validated Redfield document.
#[derive(Debug)]
pub struct Document {
    pub imports: Vec<Import>,
    /// The services and their procedures defined in the document.
    pub services: Vec<Service>,
    /// A list of top-level user-authored messages in the document.
    pub messages: Vec<Message>,
    /// A list of top-level user-authored oneofs in the document.
    pub oneofs: Vec<OneOf>,
    /// A list of top-level user-authored messages in the document.
    pub enums: Vec<Enum>,
}

#[derive(Debug)]
pub struct Import {
    pub name: CompactString,
    pub alias: Option<CompactString>,
}

/// A service is a group of procedures hosted at a single URL.
#[derive(Debug)]
pub struct Service {
    /// The name of the service.
    pub name: Ident,
    /// The procedures (or "endpoints") available in this service.
    pub procedures: Vec<Procedure>,
    /// The endpoint where the service is hosted, if specified.
    pub base_url: Option<String>,
}

/// A Typeref is a module and name of a user-defined type. If the
/// module is None, then the type is defined locally.
#[derive(Debug)]
pub struct TypeRef {
    module: Option<Ident>,
    name: Ident,
}

/// A procedure is a single operation or "endpoint" exposed by a service.
#[derive(Debug)]
pub struct Procedure {
    pub verb: Verb,
    pub name: Ident,
    pub input: Option<TypeRef>,
    pub output: Option<TypeRef>,
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
    User(FullIdent),
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

enum MaybeResolvedDoc {
    Raw(parser::Document),
    Resolved(Document),
}

impl MaybeResolvedDoc {
    fn is_raw(&self) -> bool {
        matches!(self, MaybeResolvedDoc::Raw(_))
    }
}

fn resolve_deps(
    nodes: Vec<parser::Document>,
    filenames: &[CompactString],
    edges: &BTreeSet<(usize, usize)>,
) -> Result<Vec<Document>, Error> {
    let mut links: parser::Links = Default::default();
    let mut seen = Default::default();
    let mut nodes = nodes
        .into_iter()
        .map(|doc| MaybeResolvedDoc::Raw(doc))
        .collect::<Vec<_>>();
    resolve_deps_dfs(&mut nodes, filenames, edges, &mut seen, &mut links, 0)?;
    let docs = nodes
        .into_iter()
        .map(|doc| match doc {
            MaybeResolvedDoc::Raw(_) => panic!("internal error: failed to parse all documents"),
            MaybeResolvedDoc::Resolved(doc) => doc,
        })
        .collect();
    Ok(docs)
}

fn resolve_deps_dfs(
    nodes: &mut [MaybeResolvedDoc],
    filenames: &[CompactString],
    // dependency graph edges for documents
    edges: &BTreeSet<(usize, usize)>,
    seen: &mut HashSet<usize>,
    // dependency graph edges for types
    links: &mut parser::Links,
    from: usize,
) -> Result<(), Error> {
    if !seen.insert(from) {
        return Err(Error(Error_ {
            filename: filenames[from].to_string(),
            type_: ErrorType::ImportCycle,
        }));
    }
    // resolve all raw dependencies
    for (_, dep) in edges.range((from, 0)..(from + 1, 0)) {
        if nodes[*dep].is_raw() {
            resolve_deps_dfs(nodes, filenames, edges, seen, links, *dep)?;
        }
    }
    // all dependencies have been resolved, so self is ready to resolve
    let doc = match nodes[from] {
        MaybeResolvedDoc::Raw(ref mut doc) => std::mem::take(doc),
        MaybeResolvedDoc::Resolved(_) => panic!("processed raw document twice"),
    };
    // gather imported namespace items
    let mut imports: parser::Imports = Default::default();
    for (_, dep) in edges.range((from, 0)..(from + 1, 0)) {
        let mut tlds = HashMap::<CompactString, parser::DefinitionType>::new();
        let dep_doc = match nodes[*dep] {
            MaybeResolvedDoc::Raw(_) => panic!("unexpected raw doc"),
            MaybeResolvedDoc::Resolved(ref doc) => doc,
        };
        for msg in dep_doc.messages.iter() {
            tlds.insert(msg.name.0.clone(), parser::DefinitionType::Message);
        }
        for oneof in dep_doc.oneofs.iter() {
            tlds.insert(oneof.name.0.clone(), parser::DefinitionType::OneOf);
        }
        for enm in dep_doc.enums.iter() {
            tlds.insert(enm.name.0.clone(), parser::DefinitionType::Enum);
        }
        imports.insert(filenames[*dep].clone(), tlds);
        for imp in doc.imports.iter() {
            if let Some(ref alias) = imp.alias {
                let types = imports.remove(imp.name.as_str()).unwrap();
                imports.insert(alias.value.clone(), types);
            }
        }
    }
    let doc = parser::resolve_types(doc, imports, links).map_err(|e| {
        Error(Error_ {
            filename: filenames[from].to_string(),
            type_: ErrorType::Parse(e),
        })
    })?;
    nodes[from] = MaybeResolvedDoc::Resolved(doc);
    seen.remove(&from);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test_document_succeeds() {
        static DOC_PATH: &str = "../documents/imports_root.rdf";
        let docs = parse_file(DOC_PATH).unwrap();
        println!("{:#?}", docs);
    }
}
