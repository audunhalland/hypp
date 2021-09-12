use proc_macro2::TokenStream;
use quote::quote;

use crate::ast;
use crate::ir;

use crate::codegen::*;

struct Component {
    dom_programs: Vec<TokenStream>,
    root_idents: RootIdents,
    props_struct: syn::ItemStruct,
    variant_enums: Vec<TokenStream>,
    struct_fields: Vec<ir::StructField>,
    props_destructuring: syn::FnArg,
    fn_stmts: Vec<syn::Stmt>,
    statements: Vec<ir::Statement>,
}

pub fn generate_component(root_block: ir::Block, input_fn: syn::ItemFn) -> TokenStream {
    let Component {
        dom_programs,
        root_idents,
        props_struct,
        variant_enums,
        struct_fields,
        props_destructuring,
        fn_stmts,
        statements,
    } = analyze_component(root_block, input_fn);

    let component_ident = &root_idents.component_ident;
    let props_ident = &root_idents.props_ident;

    let struct_field_defs = struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&root_idents, Scope::Component));

    let struct_params = struct_fields
        .iter()
        .map(ir::StructField::struct_param_tokens);

    let mount_stmts = statements.iter().map(|statement| {
        statement.gen_mount(
            &root_idents,
            CodegenCtx {
                lifecycle: Lifecycle::Mount,
                scope: Scope::Component,
            },
        )
    });

    let patch_stmts = statements.iter().map(|statement| {
        statement.gen_patch(
            &root_idents,
            CodegenCtx {
                lifecycle: Lifecycle::Patch,
                scope: Scope::Component,
            },
        )
    });

    let unmount_stmts = gen_unmount(
        &statements,
        CodegenCtx {
            lifecycle: Lifecycle::Unmount,
            scope: Scope::Component,
        },
    );

    quote! {
        #props_struct

        #(#dom_programs)*

        #(#variant_enums)*

        pub struct #component_ident<H: Hypp> {
            #(#struct_field_defs)*

            __phantom: PhantomField<H>
        }

        impl<H: Hypp> #component_ident<H> {
            pub fn mount(#props_destructuring, __vm: &mut dyn DomVM<H>) -> Result<Self, Error> {
                #(#fn_stmts)*
                #(#mount_stmts)*

                Ok(Self {
                    #(#struct_params)*

                    __phantom: std::marker::PhantomData
                })
            }
        }

        impl<'p, H: Hypp> Component<'p, H> for #component_ident<H> {
            type Props = #props_ident<'p>;

            fn patch(&mut self, #props_destructuring, __vm: &mut dyn DomVM<H>) {
                #(#fn_stmts)*
                #(#patch_stmts)*
            }

            fn unmount(&mut self, __vm: &mut dyn DomVM<H>) {
                #unmount_stmts
            }
        }
    }
}

fn analyze_component(
    ir::Block {
        struct_fields,
        statements,
    }: ir::Block,
    input_fn: syn::ItemFn,
) -> Component {
    let ident = input_fn.sig.ident;
    let props_ident = quote::format_ident!("{}Props", ident);
    let uppercase_prefix = ident.clone().to_string().to_uppercase();

    let root_idents = RootIdents {
        component_ident: ident,
        props_ident,
        uppercase_prefix,
    };

    let props_struct = create_props_struct(&input_fn.sig.inputs, &root_idents);
    let props_destructuring = create_props_destructuring(&input_fn.sig.inputs, &root_idents);

    let mut dom_programs = vec![];
    collect_dom_programs(&statements, &root_idents, &mut dom_programs);

    let mut variant_enums = vec![];
    collect_variant_enums(&statements, &root_idents, &mut variant_enums);

    Component {
        dom_programs,
        variant_enums,
        root_idents,
        props_struct,
        struct_fields,
        props_destructuring,
        fn_stmts: input_fn.block.stmts,
        statements,
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
