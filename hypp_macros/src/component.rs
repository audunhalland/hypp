use proc_macro2::TokenStream;
use quote::quote;

use crate::component_ast;
use crate::ir;
use crate::lowering;
use crate::param;

use crate::codegen::*;

struct Component {
    component_kind: ir::ComponentKind,
    dom_programs: Vec<TokenStream>,
    root_idents: RootIdents,
    params: Vec<param::Param>,
    root_block: ir::Block,
    dynamic_span_enums: Vec<TokenStream>,
    fn_stmts: Vec<syn::Stmt>,
    has_self_shim: HasSelfShim,
    methods: Vec<syn::ItemFn>,
}

struct PublicPropsStruct {
    tokens: TokenStream,
    has_p_lifetime: bool,
}

pub fn generate_component(ast: component_ast::Component) -> TokenStream {
    let Component {
        component_kind,
        dom_programs,
        root_idents,
        params,
        root_block,
        dynamic_span_enums,
        fn_stmts,
        has_self_shim,
        methods,
    } = analyze_ast(ast);

    let component_ident = &root_idents.component_ident;
    let props_ident = &root_idents.public_props_ident;
    let env_ident = &root_idents.env_ident;
    let root_span_ident = &root_idents.root_span_ident;

    let PublicPropsStruct {
        tokens: public_props_struct,
        has_p_lifetime,
    } = create_public_props_struct(&params, &root_idents);
    let env_struct = create_env_struct(&params, &root_idents);
    let root_span_struct = create_root_span_struct(&root_block, &root_idents);
    let fn_props_destructuring = create_fn_props_destructuring(&params, &root_idents);
    let self_env_locals = create_env_locals(&params, quote! { self.env });
    let env_locals = create_env_locals(&params, quote! { __env });
    let props_updater = create_props_updater(&params);
    let self_shim = if has_self_shim.0 {
        create_self_shim(&params, &root_idents, methods)
    } else {
        quote! {}
    };
    let shim_impls = if has_self_shim.0 {
        Some(create_shim_impls(&params, &root_idents))
    } else {
        None
    };

    let public_props_generics = if has_p_lifetime {
        Some(quote! { <'p> })
    } else {
        None
    };

    let struct_field_defs = root_block
        .struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&root_idents, Scope::Component));

    let mount_state_locals = create_mount_state_locals(&params);
    let mount = root_block.gen_mount(
        ConstructorKind::Component,
        &root_idents,
        Params(Some(&params)),
        CodegenCtx {
            component_kind,
            function: Function::Mount,
            scope: Scope::Component,
        },
    );

    let patch_binder_local = match component_kind {
        ir::ComponentKind::SelfUpdatable => Some(quote! {
            let mut __binder: ::hypp::shim::Binder<H, Self> = ::hypp::shim::Binder::from_opt_weak(&self.__weak_self);
        }),
        ir::ComponentKind::Basic => None,
    };

    let patch_stmts = root_block.statements.iter().map(|statement| {
        statement.gen_patch(
            &root_idents,
            CodegenCtx {
                component_kind,
                function: Function::Patch,
                scope: Scope::Component,
            },
        )
    });

    let span_pass = root_block.gen_span_pass(
        ir::DomDepth(0),
        &root_idents,
        CodegenCtx {
            component_kind,
            function: Function::SpanPass,
            scope: Scope::Component,
        },
    );

    let fn_span_pass_over = match component_kind {
        ir::ComponentKind::SelfUpdatable => Some(quote! {
            fn pass_over(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
                // Self updating! This component needs to store the updated anchor.
                self.__anchor = __cursor.anchor();
                self.pass(__cursor, ::hypp::SpanOp::PassOver)
            }
        }),
        ir::ComponentKind::Basic => None,
    };

    let fn_span_erase = root_block
        .gen_span_erase(CodegenCtx {
            component_kind,
            function: Function::Erase,
            scope: Scope::Component,
        })
        .map(|stmts| {
            quote! {
                fn erase(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
                    #stmts
                    self.pass(__cursor, ::hypp::SpanOp::Erase)
                }
            }
        });

    let handle_path = root_block.handle_kind.handle_path();

    quote! {
        #public_props_struct
        #env_struct
        #root_span_struct
        #self_shim

        #(#dom_programs)*

        #(#dynamic_span_enums)*

        #[allow(dead_code)]
        pub struct #component_ident<H: ::hypp::Hypp> {
            #(#struct_field_defs)*
        }

        impl<H: ::hypp::Hypp + 'static> #component_ident<H> {
            pub fn mount(#fn_props_destructuring, __cursor: &mut dyn ::hypp::Cursor<H>) -> Result<#handle_path<Self>, ::hypp::Error> {
                #mount_state_locals
                #(#fn_stmts)*
                #mount
                Ok(#handle_path::new(__mounted))
            }

            #[allow(unused_variables)]
            fn patch(&mut self, __updates: &[bool], __cursor: &mut dyn ::hypp::Cursor<H>) {
                #self_env_locals
                #(#fn_stmts)*
                #patch_binder_local
                #(#patch_stmts)*
            }

            #[allow(unused_variables)]
            fn patch2(
                __root: ::hypp::InputOrOutput<#root_span_ident<H>>,
                __env: &#env_ident,
                __updates: &[bool],
                __cursor: &mut dyn ::hypp::Cursor<H>,
                __bind: &mut dyn ::hypp::BindCallback<H, Self>,
            ) -> Result<(), ::hypp::Error> {
                #env_locals

                match __root {
                    ::hypp::InputOrOutput::Output(span) => {
                        *span = Some(#root_span_ident {
                            __phantom: ::std::marker::PhantomData,
                        });
                    }
                    ::hypp::InputOrOutput::Input(span) => {
                        // just patch!
                        // patch_span0(&mut span.__f0, __cursor)?;
                    }
                }
                Ok(())
            }
        }

        impl<H: ::hypp::Hypp> ::hypp::handle::ToHandle for #component_ident<H> {
            type Handle = #handle_path<Self>;
        }

        impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for #component_ident<H> {
            fn is_anchored(&self) -> bool {
                unimplemented!()
            }

            fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
                #span_pass
            }

            #fn_span_pass_over
            #fn_span_erase
        }

        impl<'p, H: ::hypp::Hypp + 'static> ::hypp::Component<'p, H> for #component_ident<H> {
            type Props = #props_ident #public_props_generics;

            fn pass_props(&mut self, #fn_props_destructuring, __cursor: &mut dyn ::hypp::Cursor<H>) {
                #props_updater

                self.patch(&__updates, __cursor);
            }
        }

        #shim_impls

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
    let (component_kind, root_block) =
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

    let mut dom_programs = vec![];
    collect_dom_programs(&root_block.statements, &root_idents, &mut dom_programs);

    let mut dynamic_span_enums = vec![];
    collect_dynamic_span_enums(
        &root_block.statements,
        component_kind,
        &root_idents,
        &mut dynamic_span_enums,
    );

    Component {
        component_kind,
        dom_programs,
        root_idents,
        params,
        root_block,
        dynamic_span_enums,
        fn_stmts: vec![],
        has_self_shim,
        methods,
    }
}

/// Create the props `struct` that callees will use to instantiate the component
fn create_public_props_struct(
    params: &[param::Param],
    root_idents: &RootIdents,
) -> PublicPropsStruct {
    let props_ident = &root_idents.public_props_ident;

    let mut has_p_lifetime = false;

    let fields = params
        .iter()
        .filter_map(|param| match &param.kind {
            param::ParamKind::Prop(root_ty) => Some((&param.ident, root_ty)),
            param::ParamKind::State(_) => None,
        })
        .map(|(ident, root_ty)| {
            let ty = match root_ty {
                param::ParamRootType::One(ty) => match ty {
                    param::ParamLeafType::Owned(ty) => quote! { #ty },
                    param::ParamLeafType::Ref(ty) => {
                        has_p_lifetime = true;
                        quote! { &'p #ty }
                    }
                },
                param::ParamRootType::Option(ty) => match ty {
                    param::ParamLeafType::Owned(ty) => quote! { Option<#ty> },
                    param::ParamLeafType::Ref(ty) => {
                        has_p_lifetime = true;
                        quote! { Option<&'p #ty> }
                    }
                },
            };

            quote! {
                pub #ident: #ty,
            }
        })
        .collect::<Vec<_>>();

    let generics = if has_p_lifetime {
        Some(quote! { <'p> })
    } else {
        None
    };

    PublicPropsStruct {
        tokens: quote! {
            pub struct #props_ident #generics {
                #(#fields)*
            }
        },
        has_p_lifetime,
    }
}

/// Create the env struct that the component uses to cache internal data
fn create_env_struct(params: &[param::Param], root_idents: &RootIdents) -> TokenStream {
    let env_ident = &root_idents.env_ident;

    let fields = params.iter().map(|param| {
        let ident = &param.ident;
        let ty = param.owned_ty_tokens();

        quote! {
            pub #ident: #ty,
        }
    });

    quote! {
        pub struct #env_ident {
            #(#fields)*
        }
    }
}

fn create_root_span_struct(block: &ir::Block, root_idents: &RootIdents) -> TokenStream {
    let root_span_ident = &root_idents.root_span_ident;

    quote! {
        struct #root_span_ident<H: ::hypp::Hypp> {
            __phantom: ::std::marker::PhantomData<H>
        }
    }
}

fn create_fn_props_destructuring(params: &[param::Param], root_idents: &RootIdents) -> syn::FnArg {
    let props_ident = &root_idents.public_props_ident;

    let fields = params.iter().filter_map(|param| match &param.kind {
        param::ParamKind::Prop(_) => {
            let ident = &param.ident;

            Some(quote! {
                #ident,
            })
        }
        param::ParamKind::State(_) => None,
    });

    syn::parse_quote! {
        #props_ident {
            #(#fields)*
            ..
        }: #props_ident
    }
}

// Temporary while still using separate 'mount' template code
fn create_mount_state_locals(params: &[param::Param]) -> TokenStream {
    let locals = params.iter().filter_map(|param| match &param.kind {
        param::ParamKind::State(_) => {
            let ident = &param.ident;

            Some(quote! {
                let #ident = Default::default();
            })
        }
        param::ParamKind::Prop(_) => None,
    });

    quote! {
        #(#locals)*
    }
}

fn create_env_locals(params: &[param::Param], prefix: TokenStream) -> TokenStream {
    let bindings = params.iter().map(|param| {
        let ident = &param.ident;

        match &param.kind {
            param::ParamKind::Prop(root_ty) => match root_ty {
                param::ParamRootType::One(ty) => match ty {
                    param::ParamLeafType::Owned(_) => {
                        quote! { let #ident = #prefix.#ident; }
                    }
                    param::ParamLeafType::Ref(_) => {
                        // Remember, when storing a _reference_ prop inside self,
                        // we use the `<T as ToOwned>::Owned` type to store it..
                        // so we can just take the reference to that again!
                        quote! {
                            let #ident = &#prefix.#ident;
                        }
                    }
                },
                param::ParamRootType::Option(ty) => match ty {
                    param::ParamLeafType::Owned(_) => {
                        quote! { let #ident = #prefix.#ident; }
                    }
                    param::ParamLeafType::Ref(_) => {
                        quote! { let #ident = #prefix.#ident.as_deref(); }
                    }
                },
            },
            param::ParamKind::State(_) => quote! {
                // err... take reference maybe?
                let #ident = #prefix.#ident;
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

        match &param.kind {
            param::ParamKind::Prop(root_ty) => {
                let self_prop_as_ref = match root_ty {
                    param::ParamRootType::One(_) => quote! { self.env.#ident },
                    param::ParamRootType::Option(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { self.env.#ident },
                        param::ParamLeafType::Ref(_) => quote! {self.env.#ident.as_deref() },
                    },
                };

                let write = match root_ty {
                    param::ParamRootType::One(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { self.env.#ident = #ident; },
                        param::ParamLeafType::Ref(_) => quote! {
                            self.env.#ident = #ident.to_owned();
                        },
                    },
                    param::ParamRootType::Option(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { self.env.#ident = #ident; },
                        param::ParamLeafType::Ref(_) => quote! {
                            self.env.#ident = #ident.map(|val| val.to_owned());
                        },
                    },
                };

                Some(quote! {
                    if #self_prop_as_ref != #ident {
                        #write
                        __updates[#id] = true;
                    }
                })
            }
            param::ParamKind::State(_) => None,
        }
    });

    quote! {
        // TODO: use bitvec?
        let mut __updates: [bool; #n_params] = [false; #n_params];

        #(#checks)*
    }
}

fn create_self_shim(
    params: &[param::Param],
    root_idents: &RootIdents,
    methods: Vec<syn::ItemFn>,
) -> TokenStream {
    let shim_ident = &root_idents.self_shim_ident;

    let fields = params.iter().map(|param| {
        let ident = &param.ident;
        let ty = match &param.kind {
            param::ParamKind::Prop(root_ty) => match root_ty {
                param::ParamRootType::One(ty) => match ty {
                    param::ParamLeafType::Owned(ty) => quote! { &'c #ty },
                    param::ParamLeafType::Ref(ty) => quote! { &'c #ty },
                },
                param::ParamRootType::Option(ty) => match ty {
                    param::ParamLeafType::Owned(ty) => quote! { Option<&'p #ty> },
                    param::ParamLeafType::Ref(ty) => quote! { Option<&'p #ty> },
                },
            },
            param::ParamKind::State(ty) => quote! {
                ::hypp::state_ref::StateRef<'c, #ty>
            },
        };

        quote! {
            #ident: #ty,
        }
    });

    quote! {
        pub struct #shim_ident<'c> {
            #(#fields)*
        }

        impl<'c> #shim_ident<'c> {
            #(#methods)*
        }
    }
}

fn create_shim_impls(params: &[param::Param], root_idents: &RootIdents) -> TokenStream {
    let n_params = params.iter().count();
    let component_ident = &root_idents.component_ident;
    let shim_ident = &root_idents.self_shim_ident;

    let env_fields = params.iter().map(|param| {
        let ident = &param.ident;
        match &param.kind {
            param::ParamKind::Prop(_) => quote! {
                #ident: &self.env.#ident,
            },
            param::ParamKind::State(_) => {
                let id = param.id as usize;

                quote! {
                    #ident: ::hypp::state_ref::StateRef::new(&mut self.env.#ident, &mut __updates[#id]),
                }
            }
        }
    });

    quote! {
        impl<'p, H: ::hypp::Hypp + 'static> ::hypp::ShimTrampoline for #component_ident<H> {
            type Shim<'s> = #shim_ident<'s>;

            fn shim_trampoline(&mut self, method: ::hypp::ShimMethod<Self>)
            {
                let mut __updates: [bool; #n_params] = [false; #n_params];

                let mut shim = #shim_ident {
                    #(#env_fields)*
                };

                method.0(&mut shim);

                let mut cursor = self.__anchor.create_builder();
                self.patch(&__updates, &mut cursor);
            }
        }
    }
}
