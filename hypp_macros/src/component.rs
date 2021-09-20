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
    params: Vec<param::Param>,
    prop_fields: Vec<ir::StructField>,
    root_block: ir::Block,
    variant_enums: Vec<TokenStream>,
    fn_stmts: Vec<syn::Stmt>,
    has_self_shim: HasSelfShim,
    methods: Vec<syn::ItemFn>,
}

pub fn generate_component(ast: component_ast::Component) -> TokenStream {
    let Component {
        dom_programs,
        root_idents,
        params,
        prop_fields,
        root_block,
        variant_enums,
        fn_stmts,
        has_self_shim,
        methods,
    } = analyze_ast(ast);

    let component_ident = &root_idents.component_ident;
    let props_ident = &root_idents.public_props_ident;

    let public_props_struct = create_public_props_struct(&prop_fields, &root_idents);
    let owned_props_struct = create_owned_props_struct(&prop_fields, &root_idents);
    let fn_props_destructuring = create_fn_props_destructuring(&params, &root_idents);
    let self_props_bindings = create_self_props_bindings(&params);
    let props_updater = create_props_updater(&params);
    let self_shim = if has_self_shim.0 {
        create_self_shim(&root_block.struct_fields, &root_idents, methods)
    } else {
        quote! {}
    };
    let shim_updater_trampoline = if has_self_shim.0 {
        create_shim_updater_trampoline(&root_block.struct_fields, &root_idents, &params)
    } else {
        quote! {}
    };

    let struct_field_defs = root_block
        .struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&root_idents, Scope::Component));

    let mount = root_block.gen_mount(
        ConstructorKind::Component,
        &root_idents,
        OwnedProps(Some(&prop_fields)),
        CodegenCtx {
            lifecycle: Lifecycle::Mount,
            scope: Scope::Component,
        },
    );

    let patch_stmts = root_block.statements.iter().map(|statement| {
        statement.gen_patch(
            &root_idents,
            CodegenCtx {
                lifecycle: Lifecycle::Patch,
                scope: Scope::Component,
            },
        )
    });

    let unmount_stmts = gen_unmount(
        &root_block.statements,
        ir::DomDepth(0),
        CodegenCtx {
            lifecycle: Lifecycle::Unmount,
            scope: Scope::Component,
        },
    );

    let handle_path = root_block.handle_kind.handle_path();

    quote! {
        #public_props_struct
        #owned_props_struct
        #self_shim

        #(#dom_programs)*

        #(#variant_enums)*

        #[allow(dead_code)]
        pub struct #component_ident<H: ::hypp::Hypp> {
            #(#struct_field_defs)*

            __phantom: ::std::marker::PhantomData<H>,
        }

        impl<H: ::hypp::Hypp + 'static> #component_ident<H> {
            pub fn mount(#fn_props_destructuring, __cursor: &mut dyn ::hypp::Cursor<H>) -> Result<#handle_path<Self>, ::hypp::Error> {
                #(#fn_stmts)*
                #mount
                Ok(#handle_path::new(__mounted))
            }

            #[allow(unused_variables)]
            fn patch(&mut self, __updates: &[bool], __cursor: &mut dyn ::hypp::Cursor<H>) {
                #self_props_bindings

                #(#fn_stmts)*
                #(#patch_stmts)*
            }

            #shim_updater_trampoline
        }

        impl<H: ::hypp::Hypp> ::hypp::handle::ToHandle for #component_ident<H> {
            type Handle = #handle_path<Self>;
        }

        impl<'p, H: ::hypp::Hypp + 'static> ::hypp::Component<'p, H> for #component_ident<H> {
            type Props = #props_ident<'p>;

            fn set_props(&mut self, #fn_props_destructuring, __cursor: &mut dyn ::hypp::Cursor<H>) {
                #props_updater

                self.patch(&__updates, __cursor);
            }

            fn pass_over(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) {
                unimplemented!()
            }

            fn unmount(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) {
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
        template,
    }: component_ast::Component,
) -> Component {
    let root_block =
        lowering::lower_root_node(template, lowering::TraversalDirection::LastToFirst, &params)
            .expect("Compile error: Lowering problem");

    // Current heuristic for determining if we need self shim: component is stored
    // with a shared handle:
    let has_self_shim = match root_block.handle_kind {
        ir::HandleKind::Shared => HasSelfShim(true),
        ir::HandleKind::Unique => HasSelfShim(false),
    };

    // Root idents which are used as prefixes for every global ident generated:
    let root_idents = RootIdents::from_component_ident(ident);

    let prop_fields = params
        .iter()
        .filter(|param| param.is_prop())
        .map(|param| ir::StructField {
            ident: ir::FieldIdent::Param(param.ident.clone()),
            ty: ir::StructFieldType::Param(param.clone()),
        })
        .collect::<Vec<_>>();

    let mut dom_programs = vec![];
    collect_dom_programs(&root_block.statements, &root_idents, &mut dom_programs);

    let mut variant_enums = vec![];
    collect_variant_enums(&root_block.statements, &root_idents, &mut variant_enums);

    Component {
        dom_programs,
        root_idents,
        params,
        prop_fields,
        root_block,
        variant_enums,
        fn_stmts: vec![],
        has_self_shim,
        methods,
    }
}

/// Create the props `struct` that callees will use to instantiate the component
fn create_public_props_struct(fields: &[ir::StructField], root_idents: &RootIdents) -> TokenStream {
    let props_ident = &root_idents.public_props_ident;

    let fields = fields.iter().map(|field| {
        let ident = &field.ident;

        let ty = match &field.ty {
            ir::StructFieldType::Param(ty) => match &ty.ty {
                param::ParamRootType::One(ty) => match ty {
                    param::ParamLeafType::Owned(ty) => quote! { #ty },
                    param::ParamLeafType::Ref(ty) => quote! { &'p #ty },
                },
                param::ParamRootType::Option(ty) => match ty {
                    param::ParamLeafType::Owned(ty) => quote! { Option<#ty> },
                    param::ParamLeafType::Ref(ty) => quote! { Option<&'p #ty> },
                },
            },
            _ => quote! {},
        };

        quote! {
            pub #ident: #ty,
        }
    });

    quote! {
        pub struct #props_ident<'p> {
            #(#fields)*

            pub __phantom: ::std::marker::PhantomData<&'p ()>
        }
    }
}

/// Create the props `struct` that the component uses to store the props internally
fn create_owned_props_struct(fields: &[ir::StructField], root_idents: &RootIdents) -> TokenStream {
    let props_ident = &root_idents.owned_props_ident;

    let fields = fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = field
            .ty
            .to_tokens(root_idents, Scope::Component, WithGenerics(true));

        quote! {
            pub #ident: #ty,
        }
    });

    quote! {
        pub struct #props_ident {
            #(#fields)*
        }
    }
}

fn create_fn_props_destructuring(params: &[param::Param], root_idents: &RootIdents) -> syn::FnArg {
    let props_ident = &root_idents.public_props_ident;

    let fields = params.iter().filter_map(|param| match &param.kind {
        param::ParamKind::Prop => {
            let ident = &param.ident;

            Some(quote! {
                #ident,
            })
        }
        param::ParamKind::State => None,
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

        let substruct = if param.is_prop() {
            quote! { .props }
        } else {
            quote! {}
        };

        match &param.ty {
            param::ParamRootType::One(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { let #ident = self #substruct.#ident; },
                param::ParamLeafType::Ref(_) => {
                    // Remember, when storing a _reference_ prop inside self,
                    // we use the `<T as ToOwned>::Owned` type to store it..
                    // so we can just take the reference to that again!
                    quote! {
                        let #ident = &self #substruct.#ident;
                    }
                }
            },
            param::ParamRootType::Option(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { let #ident = self #substruct.#ident; },
                param::ParamLeafType::Ref(_) => {
                    quote! { let #ident = self #substruct.#ident.as_deref(); }
                }
            },
        }
    });

    quote! {
        #(#bindings)*
    }
}

fn create_props_updater(params: &[param::Param]) -> TokenStream {
    let n_params = params.iter().count();

    let checks = params.iter().filter(|param| param.is_prop()).map(|param| {
        let id = param.id as usize;
        let ident = &param.ident;

        let self_prop_as_ref = match &param.ty {
            param::ParamRootType::One(_) => quote! { self.props.#ident },
            param::ParamRootType::Option(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { self.props.#ident },
                param::ParamLeafType::Ref(_) => quote! {self.props.#ident.as_deref() },
            },
        };

        let write = match &param.ty {
            param::ParamRootType::One(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { self.props.#ident = #ident; },
                param::ParamLeafType::Ref(_) => quote! {
                    self.props.#ident = #ident.to_owned();
                },
            },
            param::ParamRootType::Option(ty) => match ty {
                param::ParamLeafType::Owned(_) => quote! { self.props.#ident = #ident; },
                param::ParamLeafType::Ref(_) => quote! {
                    self.props.#ident = #ident.map(|val| val.to_owned());
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
        let mut __updates: [bool; #n_params] = [false; #n_params];

        #(#checks)*
    }
}

fn create_self_shim(
    struct_fields: &[ir::StructField],
    root_idents: &RootIdents,
    methods: Vec<syn::ItemFn>,
) -> TokenStream {
    let shim_ident = &root_idents.self_shim_ident;
    let owned_props_ident = &root_idents.owned_props_ident;

    let state_fields = struct_fields
        .iter()
        .filter_map(|struct_field| match &struct_field.ty {
            ir::StructFieldType::Param(param) if param.is_state() => {
                let ident = &struct_field.ident;
                let inner_ty =
                    struct_field
                        .ty
                        .to_tokens(&root_idents, Scope::Component, WithGenerics(true));

                Some(quote! {
                    #ident: ::hypp::state_ref::StateRef<'c, #inner_ty>,
                })
            }
            _ => None,
        });

    quote! {
        struct #shim_ident<'c> {
            props: &'c #owned_props_ident,
            #(#state_fields)*
        }

        impl<'c> #shim_ident<'c> {
            #(#methods)*
        }
    }
}

fn create_shim_updater_trampoline(
    struct_fields: &[ir::StructField],
    root_idents: &RootIdents,
    params: &[param::Param],
) -> TokenStream {
    let n_params = params.iter().count();
    let shim_ident = &root_idents.self_shim_ident;

    let state_fields = struct_fields
        .iter()
        .filter_map(|struct_field| match &struct_field.ty {
            ir::StructFieldType::Param(param) if param.is_state() => {
                let ident = &struct_field.ident;
                let id = param.id as usize;

                Some(quote! {
                    #ident: ::hypp::state_ref::StateRef::new(&mut self.#ident, &mut __updates[#id]),
                })
            }
            _ => None,
        });

    quote! {
        fn shim_updater_trampoline(&mut self, cb: impl Fn(&mut #shim_ident)) {
            let mut __updates: [bool; #n_params] = [false; #n_params];

            let mut shim = #shim_ident {
                props: &self.props,

                #(#state_fields)*
            };

            cb(&mut shim);

            // FIXME: The only thing missing now, is the call to self.patch().
            // But then we need to instantiate a Cursor builder positioned at the first DOM node.
        }
    }
}
