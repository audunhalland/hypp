use proc_macro2::TokenStream;
use quote::quote;

use crate::component_ast;
use crate::ir;
use crate::lowering;
use crate::param;

use crate::codegen::*;

struct Component {
    dom_programs: Vec<TokenStream>,
    root_idents: RootIdents,
    props_struct: TokenStream,
    variant_enums: Vec<TokenStream>,
    struct_fields: Vec<ir::StructField>,
    props_destructuring: syn::FnArg,
    fn_stmts: Vec<syn::Stmt>,
    statements: Vec<ir::Statement>,
}

pub fn generate_component(ast: component_ast::Component) -> TokenStream {
    let Component {
        dom_programs,
        root_idents,
        props_struct,
        variant_enums,
        struct_fields,
        props_destructuring,
        fn_stmts,
        statements,
    } = analyze_ast(ast);

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

            fn unmount(&self, __vm: &mut dyn DomVM<H>) {
                #unmount_stmts
            }
        }
    }
}

fn analyze_ast(
    component_ast::Component {
        ident,
        params,
        fns: _fns,
        template,
    }: component_ast::Component,
) -> Component {
    let root_idents = RootIdents::from_component_ident(ident);

    let props_struct = create_props_struct(&params, &root_idents);
    let props_destructuring = create_props_destructuring(&params, &root_idents);

    let ir::Block {
        struct_fields,
        statements,
    } = lowering::lower_root_node(template, &params);

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
        fn_stmts: vec![],
        statements,
    }
}

fn create_props_struct(params: &[param::Param], root_idents: &RootIdents) -> TokenStream {
    let props_ident = &root_idents.props_ident;

    let fields = params.iter().map(|param| {
        let ident = &param.ident;
        let ty = &param.ty;

        quote! {
            pub #ident: #ty,
        }
    });

    quote! {
        pub struct #props_ident<'p> {
            #(#fields)*

            pub __phantom: PhantomProp<'p>
        }
    }
}

fn create_props_destructuring(params: &[param::Param], root_idents: &RootIdents) -> syn::FnArg {
    let props_ident = &root_idents.props_ident;

    let fields = params.iter().map(|param| {
        let ident = &param.ident;

        quote! {
            #ident,
        }
    });

    syn::parse_quote! {
        #props_ident {
            #(#fields)*
            ..
        }: #props_ident
    }
}
