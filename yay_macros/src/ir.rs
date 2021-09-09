//! Intermediate Representation used during codegen

use crate::markup;

/// A node reference that needs to be stored within the component
pub struct StructField {
    pub field: FieldId,
    pub ty: StructFieldType,
}

#[derive(Clone, Copy)]
pub struct FieldId(pub usize);

impl quote::ToTokens for FieldId {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = quote::format_ident!("__f{}", self.0);
        tokens.extend(quote::quote! { #ident });
    }
}

/// Type of a struct field
#[derive(Clone)]
pub enum StructFieldType {
    DomElement,
    DomText,
    Component(ComponentPath),
    Enum(usize),
    Variable(syn::Type),
}

// A type path representing another component
#[derive(Clone)]
pub struct ComponentPath {
    pub type_path: syn::TypePath,
}

impl ComponentPath {
    pub fn new(type_path: syn::TypePath) -> Self {
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
    pub program: Vec<OpCode>,
}

pub enum OpCode {
    /// __vm.enter_element(tag_name)?
    EnterElement {
        field: FieldId,
        tag_name: syn::LitStr,
    },

    /// __vm.text(text)?
    TextConst { text: syn::LitStr },

    /// let node_binding = __vm.text(expr)?;
    TextVar {
        node_field: FieldId,
        variable_field: FieldId,
        expr: syn::Ident,
    },

    /// __vm.exit_element()?
    ExitElement,

    /// let binding = path::new(props, __vm)
    Component {
        parent: Option<FieldId>,
        field: FieldId,
        path: ComponentPath,
        props: Vec<(syn::Ident, markup::AttrValue)>,
    },

    // let binding = match expr { arms };
    Match {
        parent: Option<FieldId>,
        field: FieldId,
        enum_type: StructFieldType,
        expr: syn::Expr,
        arms: Vec<Arm>,
    },
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
