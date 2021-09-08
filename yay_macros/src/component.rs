use proc_macro2::TokenStream;
use quote::quote;

use crate::template;

pub fn generate_component(template: template::Template, input_fn: syn::ItemFn) -> TokenStream {
    let RootBlock {
        root_idents,
        props_struct,
        variant_enums,
        struct_fields,
        block_stmts:
            BlockStmts {
                props_destructuring,
                constructor_stmts,
                update_stmts,
                vars,
                matches,
            },
        fn_stmts,
    } = apply_root_block(template.root_block, input_fn);

    let node_var_params = vars.iter().map(|var| {
        let ident = &var.field_ident;
        let ty = &var.variable.ty;

        quote! {
            #ident: Var<#ty>,
        }
    });

    let component_ident = &root_idents.component_ident;
    let props_ident = &root_idents.props_ident;

    let struct_field_defs = struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&root_idents));

    let var_struct_params = vars.iter().map(|var| {
        let ident = &var.field_ident;
        quote! {
            #ident: Var::new(),
        }
    });

    let node_struct_params = struct_fields
        .iter()
        .map(template::StructField::struct_param_tokens);

    let matches = matches.iter().map(template::Match::to_match_stmt);

    quote! {
        #props_struct

        #(#variant_enums)*

        pub struct #component_ident<A: Awe> {
            #(#node_var_params)*
            #(#struct_field_defs)*

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
                #(#matches)*
            }

            fn unmount(&mut self, __vm: &mut dyn DomVM<A>) {
                // TODO: we actually want to panic here if things are not OK?
                panic!("implement unmount")
            }
        }
    }
}

fn apply_root_block(
    template::Block {
        struct_fields,
        constructor_stmts,
        vars,
        component_updates,
        matches,
    }: template::Block,
    input_fn: syn::ItemFn,
) -> RootBlock {
    let ident = input_fn.sig.ident;
    let props_ident = quote::format_ident!("{}Props", ident);

    let root_idents = RootIdents {
        component_ident: ident,
        props_ident,
    };

    let props_struct = create_props_struct(&input_fn.sig.inputs, &root_idents);
    let props_destructuring = create_props_destructuring(&input_fn.sig.inputs, &root_idents);

    let mut variant_enums = vec![];
    collect_variant_enums(&matches, &root_idents, &mut variant_enums);

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

    RootBlock {
        variant_enums,
        root_idents,
        props_struct,
        struct_fields,
        block_stmts: BlockStmts {
            props_destructuring,
            constructor_stmts,
            update_stmts,
            vars,
            matches,
        },
        fn_stmts: input_fn.block.stmts,
    }
}

fn create_props_struct(
    inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
    root_idents: &RootIdents,
) -> syn::ItemStruct {
    let props_ident = &root_idents.props_ident;

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
    root_idents: &RootIdents,
) -> syn::FnArg {
    let props_ident = &root_idents.props_ident;

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

fn collect_variant_enums(
    matches: &[template::Match],
    root_idents: &RootIdents,
    output: &mut Vec<TokenStream>,
) {
    for the_match in matches {
        let enum_ident = the_match.enum_type.to_tokens(root_idents);

        let variants = the_match.arms.iter().map(|arm| {
            let ident = &arm.enum_variant_ident;
            let struct_params = arm
                .block
                .struct_fields
                .iter()
                .map(template::StructField::struct_param_tokens);

            quote! {
                #ident { #(#struct_params)* },
            }
        });

        output.push(quote! {
            enum #enum_ident {
                #(#variants)*
            }
        });

        for arm in &the_match.arms {
            collect_variant_enums(&arm.block.matches, root_idents, output);
        }
    }
}

struct RootBlock {
    root_idents: RootIdents,
    props_struct: syn::ItemStruct,
    variant_enums: Vec<TokenStream>,
    struct_fields: Vec<template::StructField>,
    block_stmts: BlockStmts,
    fn_stmts: Vec<syn::Stmt>,
}

struct RootIdents {
    component_ident: syn::Ident,
    props_ident: syn::Ident,
}

impl RootIdents {
    fn format_enum_ident(&self, enum_index: usize) -> syn::Ident {
        quote::format_ident!("{}Cond{}", self.component_ident, enum_index)
    }
}

struct BlockStmts {
    props_destructuring: syn::FnArg,
    constructor_stmts: Vec<TokenStream>,
    update_stmts: Vec<syn::Stmt>,
    vars: Vec<template::TemplateVar>,
    matches: Vec<template::Match>,
}

impl template::StructField {
    fn field_def_tokens(&self, root_idents: &RootIdents) -> TokenStream {
        let ident = &self.ident;
        let ty = self.ty.to_tokens(root_idents);

        quote! {
            #ident: #ty,
        }
    }

    fn struct_param_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        quote! {
            #ident,
        }
    }
}

impl template::Match {
    fn to_match_stmt(&self) -> TokenStream {
        /*
        let variant_field_ident = &self.variant_field_ident;
        let expr = &self.expr;

        quote! {
            match (&mut self.#variant_field_ident, #expr) {
            }
        }
        */

        quote! {}
    }
}

impl template::StructFieldType {
    fn to_tokens(&self, root_idents: &RootIdents) -> TokenStream {
        match self {
            Self::DomText => quote! { A::Text },
            Self::Component(path) => {
                let type_path = &path.type_path;
                quote! { #type_path }
            }
            Self::Enum(enum_index) => {
                let ident =
                    quote::format_ident!("{}Enum{}", root_idents.component_ident, enum_index);
                quote! { #ident }
            }
        }
    }
}
