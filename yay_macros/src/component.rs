use proc_macro2::TokenStream;
use quote::quote;

use crate::template;

pub fn generate_component(template: template::Template, input_fn: syn::ItemFn) -> TokenStream {
    let AppliedTemplate {
        idents: Idents {
            component_ident,
            props_ident,
        },
        update_fn,
        props_struct,
        node_fields,
        static_initializers,
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

    let static_initializers = static_initializers;

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

    let mount_impl = if let Some(root) = node_fields.first() {
        let root_ident = &root.ident;
        quote! {
            Y::append_child(mount_point.parent, &self.#root_ident.as_node())
        }
    } else {
        quote! {}
    };

    quote! {
        #props_struct

        pub struct #component_ident<Y: Yay> {
            #(#node_var_params)*
            #(#node_field_defs)*
        }

        impl<Y: Yay> #component_ident<Y> {
            pub fn new(y: &Y) -> Result<Self, Error> {
                #(#static_initializers)*

                Ok(Self {
                    #(#var_struct_params)*
                    #(#node_struct_params)*
                })
            }
        }

        impl<'p, Y: Yay> Component<'p, Y> for #component_ident<Y> {
            type Props = #props_ident<'p>;

            fn mount(&self, mount_point: MountPoint<Y>) -> Result<(), Error> {
                #mount_impl
            }

            #update_fn
        }
    }
}

fn apply_template(
    template::Template {
        node_fields,
        static_initializers,
        vars,
        component_updates,
    }: template::Template,
    mut update_fn: syn::ItemFn,
) -> AppliedTemplate {
    let mut ident = syn::parse_quote! { update };
    std::mem::swap(&mut update_fn.sig.ident, &mut ident);

    let props_ident = quote::format_ident!("{}Props", ident);

    let idents = Idents {
        component_ident: ident,
        props_ident,
    };

    let mut original_inputs: syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]> =
        syn::parse_quote!();
    std::mem::swap(&mut update_fn.sig.inputs, &mut original_inputs);

    let props_struct = create_props_struct(&original_inputs, &idents);

    update_fn.sig.inputs.push(syn::parse_quote! {
        &mut self
    });

    update_fn
        .sig
        .inputs
        .push(create_props_destructuring(&original_inputs, &idents));

    let stmts = &mut update_fn.block.stmts;

    for var in vars.iter() {
        let ident = &var.variable.ident;
        let field_ident = &var.field_ident;
        let node_ident = &var.node_ident;

        let stmt: syn::Stmt = syn::parse_quote! {
            if let Some(v) = self.#field_ident.update(#ident) {
                Y::set_text(&self.#node_ident, v);
            }
        };

        stmts.push(stmt);
    }

    stmts.extend(component_updates);

    AppliedTemplate {
        idents,
        update_fn,
        props_struct,
        node_fields,
        static_initializers,
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

            pub __phantom: std::marker::PhantomData<&'p PhantomProp>
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
    update_fn: syn::ItemFn,
    props_struct: syn::ItemStruct,
    node_fields: Vec<template::NodeField>,
    static_initializers: Vec<TokenStream>,
    vars: Vec<template::TemplateVar>,
}

struct Idents {
    component_ident: syn::Ident,
    props_ident: syn::Ident,
}
