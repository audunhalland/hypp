//!
//! Main code generator for one component
//!

use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};

use crate::component_ast;
use crate::ir;
use crate::lowering;
use crate::param;

use crate::misc_codegen::*;

///
/// Fully analyzed component, ready for code generation
///
pub struct Component {
    pub comp_ctx: CompCtx,
    pub dom_programs: Vec<TokenStream>,
    pub params: Vec<param::Param>,
    pub root_block: ir::Block,
    pub span_typedefs: Vec<TokenStream>,
    pub fn_stmts: Vec<syn::Stmt>,
    pub methods: Vec<syn::ItemFn>,
}

impl Parse for Component {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let component_ast::Component {
            ident,
            generics,
            params,
            methods,
            template,
        } = input.parse()?;

        let ns = generics.ns;

        let (component_kind, root_block) =
            lowering::lower_root_node(template, generics.ns.traversal_direction(), &params)?;

        // Root idents which are used as prefixes for every global ident generated:
        let comp_ctx = CompCtx::new(ident, generics, component_kind, ns);

        let mut dom_programs = vec![];
        collect_dom_programs(&root_block.statements, &mut dom_programs);

        let mut span_typedefs = vec![];
        collect_span_typedefs(&root_block, None, &comp_ctx, &mut span_typedefs);

        Ok(Self {
            comp_ctx,
            dom_programs,
            params,
            root_block,
            span_typedefs,
            fn_stmts: vec![],
            methods,
        })
    }
}
