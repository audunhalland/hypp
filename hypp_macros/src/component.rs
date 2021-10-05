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

use crate::namespace;

///
/// Fully analyzed component, ready for code generation
///
pub struct Component {
    pub params: Vec<param::Param>,
    pub root_block: ir::Block,
    pub fn_stmts: Vec<syn::Stmt>,
    pub methods: Vec<syn::ItemFn>,

    pub component_ident: syn::Ident,
    pub generics: component_ast::ComponentGenerics,
    pub kind: ir::ComponentKind,
    pub public_props_ident: syn::Ident,
    pub mod_ident: syn::Ident,

    pub namespace: namespace::Namespace,

    pub patch_ctx_ty_root: TokenStream,
    pub patch_ctx_ty_inner: TokenStream,
}

impl Parse for Component {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let component_ast::Component {
            ident: component_ident,
            generics,
            params,
            methods,
            template,
        } = input.parse()?;

        let namespace = generics.ns;

        let (kind, root_block) =
            lowering::lower_root_node(template, generics.ns.traversal_direction(), &params)?;

        let comp_string = component_ident.clone().to_string();

        let hypp_ident = &generics.hypp_ident;

        let public_props_ident = quote::format_ident!("__{}Props", component_ident);
        let mod_ident = quote::format_ident!("__{}", comp_string.to_lowercase());

        let patch_ctx_ty_root = match kind {
            ir::ComponentKind::Basic => quote! {
                ::hypp::patch::PatchCtx<#hypp_ident, __NS>
            },
            ir::ComponentKind::SelfUpdatable => quote! {
                ::hypp::patch::PatchBindCtx<#hypp_ident, __NS, #component_ident<#hypp_ident>>
            },
        };

        // PatchCtx type used in closures where last parameter may be inferred
        let patch_ctx_ty_inner = match kind {
            ir::ComponentKind::Basic => quote! {
                ::hypp::patch::PatchCtx<#hypp_ident, __NS>
            },
            ir::ComponentKind::SelfUpdatable => quote! {
                ::hypp::patch::PatchBindCtx<#hypp_ident, __NS, #component_ident<#hypp_ident>>
            },
        };

        Ok(Self {
            params,
            root_block,
            fn_stmts: vec![],
            methods,
            component_ident,
            generics,
            kind,
            public_props_ident,
            namespace,
            mod_ident,
            patch_ctx_ty_root,
            patch_ctx_ty_inner,
        })
    }
}
