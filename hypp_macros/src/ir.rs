//! Intermediate Representation used during codegen

use crate::ast;

/// A node reference that needs to be stored within the component
pub struct StructField {
    pub field: FieldId,
    pub ty: StructFieldType,
}

#[derive(Clone, Copy)]
pub struct FieldId(pub u16);

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
    Enum(u16),
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
    pub statements: Vec<Statement>,
}

/// Something we assign to a variable
pub struct Statement {
    /// Where to assign the expression, or nothing if the expression is
    /// not to be assigned to anything
    pub field: Option<FieldId>,

    /// The expression to evaluate
    pub expression: Expression,
}

/// Something which can be evaluated to a value with a type
pub enum Expression {
    /// A constant DOM program
    ConstDom(ConstDomProgram),

    /// A text variable (in the DOM)
    VariableText {
        variable_field: FieldId,
        expr: syn::Ident,
    },

    /// A local variable field (Var<T> field)
    LocalVar,

    /// A component instantiation
    Component {
        parent: Option<FieldId>,
        path: ComponentPath,
        props: Vec<(syn::Ident, ast::AttrValue)>,
    },

    /// A match expression (something which is conditional)
    Match {
        parent: Option<FieldId>,
        enum_type: StructFieldType,
        expr: syn::Expr,
        arms: Vec<Arm>,
    },
}

/// A DOM program which can be stored in static memory
/// (i.e. no runtime evaluation is necessary)
pub struct ConstDomProgram {
    pub id: u16,
    pub ty: ConstDomProgramTy,
    pub opcodes: Vec<DomOpCode>,
}

///
/// The Type of a DOM program, i.e. the type
/// it produces when finished running
///
pub enum ConstDomProgramTy {
    Element,
    Text,
}

pub enum DomOpCode {
    EnterElement(syn::LitStr),
    Text(syn::LitStr),
    ExitElement,
}

/// An arm of a conditional
pub struct Arm {
    /// The ident of the enum variant to instantiate
    pub enum_variant_ident: syn::Ident,
    /// The ident of the function that mounts the component arm
    pub mount_fn_ident: syn::Ident,
    /// The value match pattern matching this arm
    pub pattern: syn::Pat,
    /// The code 'block' to execute
    pub block: Block,
}
