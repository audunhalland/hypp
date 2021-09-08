//! Intermediate Representation used during codegen

use proc_macro2::TokenStream;

use crate::variable;

/// A node reference that needs to be stored within the component
pub struct StructField {
    pub ident: syn::Ident,
    pub ty: StructFieldType,
}

/// Type of a struct field
#[derive(Clone)]
pub enum StructFieldType {
    DomText,
    Component(ComponentPath),
    Enum(usize),
}

// A type path representing another component
#[derive(Clone)]
pub struct ComponentPath {
    pub type_path: syn::TypePath,
}

impl ComponentPath {
    pub fn new(mut type_path: syn::TypePath) -> Self {
        let mut last_segment = type_path.path.segments.last_mut().unwrap();
        last_segment.arguments = syn::PathArguments::AngleBracketed(syn::parse_quote! {
            <A>
        });
        Self { type_path }
    }

    pub fn ident(&self) -> syn::Ident {
        self.type_path.path.segments.last().unwrap().ident.clone()
    }
}

/// A 'block' of code that should run atomically.
#[derive(Default)]
pub struct Block {
    pub struct_fields: Vec<StructField>,
    pub constructor_stmts: Vec<TokenStream>,
    pub vars: Vec<TemplateVar>,
    pub component_updates: Vec<syn::Stmt>,
    pub matches: Vec<Match>,
}

pub struct TemplateVar {
    pub variable: variable::Variable,
    pub node_ident: syn::Ident,
    pub field_ident: syn::Ident,
}

/// A conditional modelled over a match expression
pub struct Match {
    pub enum_type: StructFieldType,
    pub variant_field_ident: syn::Ident,
    pub expr: syn::Expr,
    pub arms: Vec<Arm>,
}

/// An arm of a conditional
pub struct Arm {
    /// The ident of the enum variant to instantiate
    pub enum_variant_ident: syn::Ident,
    /// The value match pattern matching this arm
    pub pattern: syn::Pat,
    /// The code 'block' to execute
    pub block: Block,
}
