//! Intermediate Representation used during codegen

use std::collections::BTreeSet;

use crate::callback;
use crate::param;
use crate::template_ast;

/// The way something must be owned in order to work.
/// Things with internal cyclic references need Shared ownership,
/// for example when callbacks are involved.
pub enum HandleKind {
    Unique,
    Shared,
}

impl Default for HandleKind {
    fn default() -> Self {
        Self::Unique
    }
}

#[derive(Clone, Copy)]
pub struct DomDepth(pub u16);

pub enum NodeType {
    Element,
    Text,
}

/// Something which is destined to be stored somewhere within the component struct,
/// directly or indirectly
pub struct StructField {
    pub ident: FieldIdent,
    pub ty: StructFieldType,
}

#[derive(Clone)]
pub enum FieldIdent {
    Id(u16),
    Param(syn::Ident),
    Props,
}

/// Type of a struct field
#[derive(Clone)]
pub enum StructFieldType {
    DomElement,
    DomText,
    Props,
    Callback,
    Param(param::Param),
    Component(ComponentPath),
    Enum(u16),
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
            last_props_path_segment.ident = quote::format_ident!("__{}Props", self.ident());
            last_props_path_segment.arguments = syn::PathArguments::None;
        }
        props_path
    }
}

/// A 'block' of code that should run atomically.
#[derive(Default)]
pub struct Block {
    pub handle_kind: HandleKind,
    pub struct_fields: Vec<StructField>,
    pub statements: Vec<Statement>,
    pub param_deps: ParamDeps,
}

/// Something we assign to a variable
pub struct Statement {
    /// Where to assign the expression, or nothing if the expression is
    /// not to be assigned to anything
    pub field: Option<FieldIdent>,

    /// Whether this statement operates at the root of the component
    pub dom_depth: DomDepth,

    /// Which parameters this statement depends on
    pub param_deps: ParamDeps,

    /// The expression to evaluate
    pub expression: Expression,
}

#[derive(Clone)]
pub enum ParamDeps {
    Const,
    // Dependent upon some params
    Some(BTreeSet<u16>),
    // Dependent upon all params (i.e. undecidable which ones)
    All,
}

impl ParamDeps {
    pub fn extend(&mut self, other: &ParamDeps) {
        match self {
            Self::Const => *self = other.clone(),
            Self::Some(ref mut ids) => match other {
                Self::Const => {}
                Self::Some(other_ids) => {
                    ids.extend(other_ids.iter());
                }
                Self::All => *self = Self::All,
            },
            Self::All => {}
        }
    }
}

impl Default for ParamDeps {
    fn default() -> Self {
        Self::Const
    }
}

/// Something which can be evaluated to a value with a type
pub enum Expression {
    /// A constant DOM program
    ConstDom(ConstDomProgram),

    AttributeCallback(callback::Callback),

    /// Something that should evaluate to text at runtime
    Text(syn::Expr),

    /// A component instantiation
    Component {
        path: ComponentPath,
        props: Vec<template_ast::Attr>,
    },

    /// A match expression (something which is conditional)
    Match {
        enum_type: StructFieldType,
        expr: syn::Expr,
        arms: Vec<Arm>,
    },
}

/// A DOM program which can be stored in static memory
/// (i.e. no runtime evaluation is necessary)
pub struct ConstDomProgram {
    pub id: u16,
    pub opcodes: Vec<DomOpCode>,
}

impl ConstDomProgram {
    pub fn last_node_opcode(&self) -> Option<&DomOpCode> {
        self.opcodes.iter().rev().find(|opcode| match opcode {
            DomOpCode::EnterElement(_) => true,
            DomOpCode::AttrName(_) => false,
            DomOpCode::AttrTextValue(_) => false,
            DomOpCode::Text(_) => true,
            DomOpCode::ExitElement => true,
        })
    }

    pub fn last_node_type(&self) -> Option<NodeType> {
        self.last_node_opcode().and_then(|opcode| match opcode {
            DomOpCode::EnterElement(_) | DomOpCode::ExitElement => Some(NodeType::Element),
            DomOpCode::AttrName(_) | DomOpCode::AttrTextValue(_) => None,
            DomOpCode::Text(_) => Some(NodeType::Text),
        })
    }
}

pub enum DomOpCode {
    EnterElement(syn::LitStr),
    AttrName(syn::LitStr),
    AttrTextValue(syn::LitStr),
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
