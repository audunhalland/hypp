//!
//! Main code generator for one component
//!

use proc_macro2::TokenStream;
use quote::quote;

use crate::component_ast;
use crate::ir;
use crate::lowering;
use crate::param;

use crate::misc_codegen::*;

struct Component {
    comp_ctx: CompCtx,
    dom_programs: Vec<TokenStream>,
    params: Vec<param::Param>,
    root_block: ir::Block,
    dynamic_span_enums: Vec<TokenStream>,
    fn_stmts: Vec<syn::Stmt>,
    methods: Vec<syn::ItemFn>,
}

struct PublicPropsStruct {
    tokens: TokenStream,
    has_p_lifetime: bool,
}

pub fn generate_component(ast: component_ast::Component) -> TokenStream {
    let Component {
        comp_ctx,
        dom_programs,
        params,
        root_block,
        dynamic_span_enums,
        fn_stmts,
        methods,
    } = analyze_ast(ast);

    let component_ident = &comp_ctx.component_ident;
    let props_ident = &comp_ctx.public_props_ident;
    let env_ident = &comp_ctx.env_ident;
    let root_span_ident = &comp_ctx.root_span_ident;

    let PublicPropsStruct {
        tokens: public_props_struct,
        has_p_lifetime,
    } = gen_public_props_struct(&params, &comp_ctx);
    let env_struct = gen_env_struct(&params, &comp_ctx);
    let root_span_struct = gen_root_span(&root_block, &comp_ctx);
    let fn_props_destructuring = gen_fn_props_destructuring(&params, &comp_ctx);
    let mount_body = gen_mount_body(&params, &comp_ctx);
    let env_locals = gen_env_locals(&params);
    let props_updater = gen_props_updater(&params);
    let self_shim = if comp_ctx.kind.is_self_updatable() {
        gen_self_shim(&params, &comp_ctx, methods)
    } else {
        quote! {}
    };
    let shim_impls = if comp_ctx.kind.is_self_updatable() {
        Some(gen_shim_impls(&params, &comp_ctx))
    } else {
        None
    };

    let public_props_generics = if has_p_lifetime {
        Some(quote! { <'p> })
    } else {
        None
    };

    let component_field_defs = match comp_ctx.kind {
        ir::ComponentKind::Basic => quote! {
            env: #env_ident,
            root_span: #root_span_ident<H>,
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            env: #env_ident,
            root_span: #root_span_ident<H>,
            anchor: H::Anchor,
            weak_self: Option<::std::rc::Weak<::std::cell::RefCell<Self>>>
        },
    };

    let patch_fn = crate::patch::gen_patch_fn(
        &root_block,
        &comp_ctx,
        env_locals,
        fn_stmts,
        CodegenCtx {
            component_kind: comp_ctx.kind,
            function: Function::Patch,
            scope: Scope::Component,
        },
    );

    let fn_span_pass_over = match comp_ctx.kind {
        ir::ComponentKind::SelfUpdatable => Some(quote! {
            fn pass_over(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
                // Self updating! This component needs to store the updated anchor.
                self.anchor = __cursor.anchor();
                self.pass(__cursor, ::hypp::SpanOp::PassOver)
            }
        }),
        ir::ComponentKind::Basic => None,
    };

    let fn_span_erase = match comp_ctx.kind {
        ir::ComponentKind::SelfUpdatable => Some(quote! {
            fn erase(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
                self.weak_self = None;
                self.pass(__cursor, ::hypp::SpanOp::Erase)
            }
        }),
        ir::ComponentKind::Basic => None,
    };

    let pass_props_patch_call = match comp_ctx.kind {
        ir::ComponentKind::Basic => quote! {
            Self::patch(
                ::hypp::InputOrOutput::Input(&mut self.root_span),
                &self.env,
                &__updates,
                &mut ::hypp::PatchCtx {
                    cur: __cursor,
                }
            ).unwrap();
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            let mut binder = ::hypp::shim::Binder::from_opt_weak(&self.weak_self);
            Self::patch(
                ::hypp::InputOrOutput::Input(&mut self.root_span),
                &self.env,
                &__updates,
                &mut ::hypp::PatchBindCtx {
                    cur: __cursor,
                    bind: &mut binder,
                }
            ).unwrap();
        },
    };

    let handle_path = root_block.handle_kind.handle_path();

    quote! {
        #public_props_struct
        #env_struct
        #self_shim
        #root_span_struct

        #(#dom_programs)*

        #(#dynamic_span_enums)*

        pub struct #component_ident<H: ::hypp::Hypp> {
            #component_field_defs
        }

        impl<H: ::hypp::Hypp + 'static> #component_ident<H> {
            pub fn mount(#fn_props_destructuring, __cursor: &mut dyn ::hypp::Cursor<H>) -> Result<#handle_path<Self>, ::hypp::Error> {
                #mount_body
            }

            #patch_fn
        }

        impl<H: ::hypp::Hypp> ::hypp::handle::ToHandle for #component_ident<H> {
            type Handle = #handle_path<Self>;
        }

        impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for #component_ident<H> {
            fn is_anchored(&self) -> bool {
                unimplemented!()
            }

            fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
                ::hypp::span::pass(&mut [&mut self.root_span], __cursor, op)
            }

            #fn_span_pass_over
            #fn_span_erase
        }

        impl<'p, H: ::hypp::Hypp + 'static> ::hypp::Component<'p, H> for #component_ident<H> {
            type Props = #props_ident #public_props_generics;

            fn pass_props(&mut self, #fn_props_destructuring, __cursor: &mut dyn ::hypp::Cursor<H>) {
                #props_updater
                #pass_props_patch_call
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

    // Root idents which are used as prefixes for every global ident generated:
    let comp_ctx = CompCtx::new(ident, component_kind);

    let mut dom_programs = vec![];
    collect_dom_programs(&root_block.statements, &comp_ctx, &mut dom_programs);

    let mut dynamic_span_enums = vec![];
    collect_dynamic_span_enums(&root_block.statements, &comp_ctx, &mut dynamic_span_enums);

    Component {
        comp_ctx,
        dom_programs,
        params,
        root_block,
        dynamic_span_enums,
        fn_stmts: vec![],
        methods,
    }
}

/// Create the props `struct` that callees will use to instantiate the component
fn gen_public_props_struct(params: &[param::Param], comp_ctx: &CompCtx) -> PublicPropsStruct {
    let props_ident = &comp_ctx.public_props_ident;

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
fn gen_env_struct(params: &[param::Param], comp_ctx: &CompCtx) -> TokenStream {
    let env_ident = &comp_ctx.env_ident;

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

// TODO: A lot in this function is very similar for a lot
// of components. Find a way to make a lib generic function.
fn gen_mount_body(params: &[param::Param], comp_ctx: &CompCtx) -> TokenStream {
    let n_params = params.iter().count();
    let component_ident = &comp_ctx.component_ident;
    let env_ident = &comp_ctx.env_ident;

    let env_params = params.iter().map(param::Param::owned_struct_param_tokens);

    let anchor_binder_locals = if comp_ctx.kind.is_self_updatable() {
        Some(quote! {
            let anchor = __cursor.anchor();
            let mut binder: ::hypp::shim::LazyBinder<H, Self> = ::hypp::shim::LazyBinder::new();
        })
    } else {
        None
    };

    let patch_ctx_arg = if comp_ctx.kind.is_self_updatable() {
        quote! {
            ::hypp::PatchBindCtx {
                cur: __cursor,
                bind: &mut binder,
            }
        }
    } else {
        quote! {
            ::hypp::PatchCtx { cur: __cursor }
        }
    };

    let component_constructor = if comp_ctx.kind.is_self_updatable() {
        quote! {
            let mounted = ::std::rc::Rc::new(::std::cell::RefCell::new(#component_ident {
                env,
                root_span: root_span.unwrap(),
                anchor,
                weak_self: None,
            }));

            mounted.borrow_mut().weak_self = Some(::std::rc::Rc::downgrade(&mounted));
            binder.bind_all(&mounted.borrow().weak_self);

            Ok(::hypp::handle::Shared::new(mounted))
        }
    } else {
        quote! {
            Ok(::hypp::handle::Unique::new(#component_ident {
                env,
                root_span: root_span.unwrap(),
            }))
        }
    };

    quote! {
        let env = #env_ident {
            #(#env_params)*
        };

        // When mounting, everything is "out of date"!
        let updates: [bool; #n_params] = [true; #n_params];

        #anchor_binder_locals
        let mut root_span = None;
        Self::patch(
            ::hypp::InputOrOutput::Output(&mut root_span),
            &env,
            &updates,
            &mut #patch_ctx_arg
        )?;

        #component_constructor
    }
}

fn gen_root_span(block: &ir::Block, comp_ctx: &CompCtx) -> TokenStream {
    let root_span_ident = &comp_ctx.root_span_ident;

    let struct_field_defs = block
        .struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&comp_ctx, Scope::Component));

    let span_pass = block.gen_span_pass(
        ir::DomDepth(0),
        &comp_ctx,
        CodegenCtx {
            component_kind: comp_ctx.kind,
            function: Function::SpanPass,
            scope: Scope::Component,
        },
    );

    let fn_span_erase = block
        .gen_span_erase(CodegenCtx {
            component_kind: comp_ctx.kind,
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

    quote! {
        struct #root_span_ident<H: ::hypp::Hypp> {
            #(#struct_field_defs)*
            __phantom: ::std::marker::PhantomData<H>
        }

        impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for #root_span_ident<H> {
            fn is_anchored(&self) -> bool {
                unimplemented!()
            }

            fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
                #span_pass
            }

            #fn_span_erase
        }
    }
}

fn gen_fn_props_destructuring(params: &[param::Param], comp_ctx: &CompCtx) -> syn::FnArg {
    let props_ident = &comp_ctx.public_props_ident;

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

fn gen_env_locals(params: &[param::Param]) -> TokenStream {
    let bindings = params.iter().map(|param| {
        let ident = &param.ident;

        match &param.kind {
            param::ParamKind::Prop(root_ty) => match root_ty {
                param::ParamRootType::One(ty) => match ty {
                    param::ParamLeafType::Owned(_) => {
                        quote! { let #ident = __env.#ident; }
                    }
                    param::ParamLeafType::Ref(_) => {
                        // Remember, when storing a _reference_ prop inside self,
                        // we use the `<T as ToOwned>::Owned` type to store it..
                        // so we can just take the reference to that again!
                        quote! {
                            let #ident = &__env.#ident;
                        }
                    }
                },
                param::ParamRootType::Option(ty) => match ty {
                    param::ParamLeafType::Owned(_) => {
                        quote! { let #ident = __env.#ident; }
                    }
                    param::ParamLeafType::Ref(_) => {
                        quote! { let #ident = __env.#ident.as_deref(); }
                    }
                },
            },
            param::ParamKind::State(_) => quote! {
                // err... take reference maybe?
                let #ident = __env.#ident;
            },
        }
    });

    quote! {
        #(#bindings)*
    }
}

fn gen_props_updater(params: &[param::Param]) -> TokenStream {
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

fn gen_self_shim(
    params: &[param::Param],
    comp_ctx: &CompCtx,
    methods: Vec<syn::ItemFn>,
) -> TokenStream {
    let shim_ident = &comp_ctx.self_shim_ident;

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
        #[allow(dead_code)]
        pub struct #shim_ident<'c> {
            #(#fields)*
        }

        impl<'c> #shim_ident<'c> {
            #(#methods)*
        }
    }
}

fn gen_shim_impls(params: &[param::Param], comp_ctx: &CompCtx) -> TokenStream {
    let n_params = params.iter().count();
    let component_ident = &comp_ctx.component_ident;
    let shim_ident = &comp_ctx.self_shim_ident;

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

                let mut cursor = self.anchor.create_builder();
                let mut binder = ::hypp::shim::Binder::from_opt_weak(&self.weak_self);
                Self::patch(
                    ::hypp::InputOrOutput::Input(&mut self.root_span),
                    &self.env,
                    &__updates,
                    &mut ::hypp::PatchBindCtx {
                        cur: &mut cursor,
                        bind: &mut binder,
                    }
                ).unwrap();
            }
        }
    }
}
