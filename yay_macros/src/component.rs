use proc_macro2::TokenStream;
use quote::quote;

use crate::template;

pub fn generate_component(template: template::Template, input_fn: syn::ItemFn) -> TokenStream {
    let AppliedTemplate {
        idents: Idents {
            component_ident,
            props_ident,
        },
        props_struct,
        node_fields,
        props_destructuring,
        constructor_stmts,
        fn_stmts,
        update_stmts,
        vars,
    } = apply_template(template, input_fn);

    let node_var_params = vars.iter().map(|var| {
        let ident = &var.field_ident;
        let ty = &var.variable.ty;

        quote! {
            #ident: Var<#ty>,
        }
    });

    let node_field_defs = node_fields.iter().map(|node_field| {
        let ident = &node_field.ident;
        let ty = &node_field.ty;

        quote! {
            #ident: #ty,
        }
    });

    let var_struct_params = vars.iter().map(|var| {
        let ident = &var.field_ident;
        quote! {
            #ident: Var::new(),
        }
    });

    let node_struct_params = node_fields.iter().map(|node_field| {
        let ident = &node_field.ident;

        quote! {
            #ident,
        }
    });

    quote! {
        #props_struct

        pub struct #component_ident<A: Awe> {
            #(#node_var_params)*
            #(#node_field_defs)*

            __phantom: PhantomField<A>
        }

        impl<A: Awe> #component_ident<A> {
            pub fn new(#props_destructuring, __vm: &mut dyn DomVM<A>) -> Result<Self, Error> {
                #(#fn_stmts)*
                #(#constructor_stmts)*

                Ok(Self {
                    #(#var_struct_params)*
                    #(#node_struct_params)*

                    __phantom: std::marker::PhantomData
                })
            }
        }

        impl<'p, A: Awe> Component<'p, A> for #component_ident<A> {
            type Props = #props_ident<'p>;

            fn update(&mut self, #props_destructuring, __vm: &mut dyn DomVM<A>) {
                #(#fn_stmts)*
                #(#update_stmts)*
            }

            fn unmount(&mut self, __vm: &mut dyn DomVM<A>) {
                // TODO: we actually want to panic here if things are not OK?
                panic!("implement unmount")
            }
        }
    }
}

fn apply_template(
    template::Template {
        node_fields,
        constructor_stmts,
        vars,
        component_updates,
    }: template::Template,
    input_fn: syn::ItemFn,
) -> AppliedTemplate {
    let ident = input_fn.sig.ident;
    let props_ident = quote::format_ident!("{}Props", ident);

    let idents = Idents {
        component_ident: ident,
        props_ident,
    };

    let props_struct = create_props_struct(&input_fn.sig.inputs, &idents);
    let props_destructuring = create_props_destructuring(&input_fn.sig.inputs, &idents);

    let mut update_stmts = vec![];

    for var in vars.iter() {
        let ident = &var.variable.ident;
        let field_ident = &var.field_ident;
        let node_ident = &var.node_ident;

        let stmt: syn::Stmt = syn::parse_quote! {
            if let Some(v) = self.#field_ident.update(#ident) {
                A::set_text(&self.#node_ident, v);
            }
        };

        update_stmts.push(stmt);
    }

    update_stmts.extend(component_updates);

    AppliedTemplate {
        idents,
        props_struct,
        node_fields,
        props_destructuring,
        constructor_stmts,
        fn_stmts: input_fn.block.stmts,
        update_stmts,
        vars,
    }
}

fn create_props_struct(
    inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
    idents: &Idents,
) -> syn::ItemStruct {
    let props_ident = &idents.props_ident;

    let fields = inputs.iter().filter_map(|input| match input {
        syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => Some(quote! {
            pub #pat: #ty,
        }),
        _ => None,
    });

    syn::parse_quote! {
        pub struct #props_ident<'p> {
            #(#fields)*

            pub __phantom: PhantomProp<'p>
        }
    }
}

fn create_props_destructuring(
    inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
    idents: &Idents,
) -> syn::FnArg {
    let props_ident = &idents.props_ident;

    let fields = inputs.iter().filter_map(|input| match input {
        syn::FnArg::Typed(syn::PatType { pat, .. }) => Some(quote! {
            #pat,
        }),
        _ => None,
    });

    syn::parse_quote! {
        #props_ident {
            #(#fields)*
            ..
        }: #props_ident
    }
}

struct AppliedTemplate {
    idents: Idents,
    props_struct: syn::ItemStruct,
    node_fields: Vec<template::NodeField>,
    props_destructuring: syn::FnArg,
    constructor_stmts: Vec<TokenStream>,
    fn_stmts: Vec<syn::Stmt>,
    update_stmts: Vec<syn::Stmt>,
    vars: Vec<template::TemplateVar>,
}

struct Idents {
    component_ident: syn::Ident,
    props_ident: syn::Ident,
}
