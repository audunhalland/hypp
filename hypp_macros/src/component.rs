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
    handle_kind: component_ast::HandleKind,
    variant_enums: Vec<TokenStream>,
    struct_fields: Vec<ir::StructField>,
    fn_props_destructuring: syn::FnArg,
    self_props_bindings: TokenStream,
    props_updater: TokenStream,
    fn_stmts: Vec<syn::Stmt>,
    statements: Vec<ir::Statement>,
    methods: Vec<syn::ItemFn>,
}

pub fn generate_component(ast: component_ast::Component) -> TokenStream {
    let Component {
        dom_programs,
        root_idents,
        props_struct,
        handle_kind,
        variant_enums,
        struct_fields,
        fn_props_destructuring,
        self_props_bindings,
        props_updater,
        fn_stmts,
        statements,
        methods,
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
        ir::DomDepth(0),
        CodegenCtx {
            lifecycle: Lifecycle::Unmount,
            scope: Scope::Component,
        },
    );

    let handle_path = handle_kind.handle_path();

    quote! {
        #props_struct

        #(#dom_programs)*

        #(#variant_enums)*

        #[allow(dead_code)]
        pub struct #component_ident<H: Hypp> {
            #(#struct_field_defs)*

            __phantom: PhantomField<H>,
        }

        impl<H: Hypp> #component_ident<H> {
            pub fn mount(#fn_props_destructuring, __vm: &mut dyn DomVM<H>) -> Result<#handle_path<Self>, Error> {
                #(#fn_stmts)*
                #(#mount_stmts)*

                Ok(#handle_path::new(Self {
                    #(#struct_params)*

                    __phantom: std::marker::PhantomData
                }))
            }

            #[allow(unused_variables)]
            fn patch(&mut self, __updates: &[bool], __vm: &mut dyn DomVM<H>) {
                #self_props_bindings

                #(#fn_stmts)*
                #(#patch_stmts)*
            }

            #(#methods)*
        }

        impl<H: Hypp> handle::ToHandle for #component_ident<H> {
            type Handle = #handle_path<Self>;
        }

        impl<'p, H: Hypp> Component<'p, H> for #component_ident<H> {
            type Props = #props_ident<'p>;

            fn set_props(&mut self, #fn_props_destructuring, __vm: &mut dyn DomVM<H>) {
                #props_updater

                self.patch(&__updates, __vm);
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
        methods,
        handle_kind,
        template,
    }: component_ast::Component,
) -> Component {
    let root_idents = RootIdents::from_component_ident(ident);

    let props_struct = create_props_struct(&params, &root_idents);
    let fn_props_destructuring = create_fn_props_destructuring(&params, &root_idents);
    let self_props_bindings = create_self_props_bindings(&params);
    let props_updater = create_props_updater(&params);

    let ir::Block {
        struct_fields,
        statements,
        ..
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
        handle_kind,
        struct_fields,
        fn_props_destructuring,
        self_props_bindings,
        props_updater,
        fn_stmts: vec![],
        statements,
        methods,
    }
}

fn create_props_struct(params: &[param::Param], root_idents: &RootIdents) -> TokenStream {
    let props_ident = &root_idents.props_ident;

    let fields = params.iter().map(|param| {
        let ident = &param.ident;
        let ty = match &param.ty {
            param::ParamRootType::One(ty) => match ty {
                param::ParamLeafType::Owned(ty) => quote! { #ty },
                param::ParamLeafType::Ref(ty) => quote! { &'p #ty },
            },
            param::ParamRootType::Option(ty) => match ty {
                param::ParamLeafType::Owned(ty) => quote! { Option<#ty> },
                param::ParamLeafType::Ref(ty) => quote! { Option<&'p #ty> },
            },
        };

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

fn create_fn_props_destructuring(params: &[param::Param], root_idents: &RootIdents) -> syn::FnArg {
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

fn create_self_props_bindings(params: &[param::Param]) -> TokenStream {
    let bindings = params.iter().map(|param| {
        let ident = &param.ident;

        match &param.ty {
            param::ParamRootType::One(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { let #ident = self.#ident; },
                param::ParamLeafType::Ref(_) => {
                    // Remember, when storing a _reference_ prop inside self,
                    // we use the `<T as ToOwned>::Owned` type to store it..
                    // so we can just take the reference to that again!
                    quote! {
                        let #ident = &self.#ident;
                    }
                }
            },
            param::ParamRootType::Option(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { let #ident = self.#ident; },
                param::ParamLeafType::Ref(_) => quote! { let #ident = self.#ident.as_deref(); },
            },
        }
    });

    quote! {
        #(#bindings)*
    }
}

fn create_props_updater(params: &[param::Param]) -> TokenStream {
    let n_props = params
        .iter()
        .filter(|param| match &param.kind {
            param::ParamKind::Prop => true,
            _ => false,
        })
        .count();

    let checks = params.iter().map(|param| {
        let id = param.id as usize;
        let ident = &param.ident;

        let self_prop_as_ref = match &param.ty {
            param::ParamRootType::One(_) => quote! { self.#ident },
            param::ParamRootType::Option(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { self.#ident },
                param::ParamLeafType::Ref(_) => quote! {self.#ident.as_deref() },
            },
        };

        let write = match &param.ty {
            param::ParamRootType::One(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { self.#ident = #ident; },
                param::ParamLeafType::Ref(_) => quote! {
                    self.#ident = #ident.to_owned();
                },
            },
            param::ParamRootType::Option(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { self.#ident = #ident; },
                param::ParamLeafType::Ref(_) => quote! {
                    self.#ident = #ident.map(|val| val.to_owned());
                },
            },
        };

        quote! {
            if #self_prop_as_ref != #ident {
                #write
                __updates[#id] = true;
            }
        }
    });

    quote! {
        // TODO: use bitvec?
        let mut __updates: [bool; #n_props] = [false; #n_props];

        #(#checks)*
    }
}
