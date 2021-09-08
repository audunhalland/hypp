//! Intermediate Representation used during codegen

use crate::markup;
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

    pub fn ident(&self) -> &syn::Ident {
        &self.type_path.path.segments.last().unwrap().ident
    }

    pub fn props_path(&self) -> syn::Path {
        let mut props_path = self.type_path.path.clone();
        if let Some(last_props_path_segment) = props_path.segments.last_mut() {
            last_props_path_segment.ident = quote::format_ident!("{}Props", self.ident());
            last_props_path_segment.arguments = syn::PathArguments::None;
        }
        props_path
    }
}

/// A 'block' of code that should run atomically.
#[derive(Default)]
pub struct Block {
    pub struct_fields: Vec<StructField>,
    pub constructor_stmts: Vec<Statement>,
    pub vars: Vec<TemplateVar>,
    pub component_updates: Vec<Statement>,
    pub matches: Vec<Match>,
}

pub enum Statement {
    /// __vm.enter_element(tag_name)?
    EnterElement { tag_name: syn::LitStr },

    /// __vm.text(text)?
    TextConst { text: syn::LitStr },

    /// let binding = __vm.text(var)?;
    LetTextVar {
        binding: syn::Ident,
        var: syn::Ident,
    },

    /// __vm.exit_element()?
    ExitElement,

    /// let binding = path::new(props, __vm)
    LetInstantiateComponent {
        binding: syn::Ident,
        path: ComponentPath,
        props: Vec<(syn::Ident, markup::AttrValue)>,
    },

    /// ident.update(props, __vm);
    UpdateComponent {
        in_self: bool,
        ident: syn::Ident,
        path: ComponentPath,
        props: Vec<(syn::Ident, markup::AttrValue)>,
    },
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
