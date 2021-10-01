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

use crate::misc_codegen::*;

///
/// Fully analyzed component, ready for code generation
///
pub struct Component {
    comp_ctx: CompCtx,
    dom_programs: Vec<TokenStream>,
    params: Vec<param::Param>,
    root_block: ir::Block,
    span_typedefs: Vec<TokenStream>,
    fn_stmts: Vec<syn::Stmt>,
    methods: Vec<syn::ItemFn>,
}

impl Parse for Component {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let component_ast::Component {
            ident,
            namespace,
            params,
            methods,
            template,
        } = input.parse()?;

        let (component_kind, root_block) =
            lowering::lower_root_node(template, namespace.traversal_direction(), &params)?;

        // Root idents which are used as prefixes for every global ident generated:
        let comp_ctx = CompCtx::new(ident, component_kind, namespace);

        let mut dom_programs = vec![];
        collect_dom_programs(&root_block.statements, &mut dom_programs);

        let mut span_typedefs = vec![];
        collect_span_typedefs(&root_block, None, &comp_ctx, &mut span_typedefs);

        Ok(Self {
            comp_ctx,
            dom_programs,
            params,
            root_block,
            span_typedefs,
            fn_stmts: vec![],
            methods,
        })
    }
}

struct PublicPropsStruct {
    tokens: TokenStream,
    has_p_lifetime: bool,
}

pub fn generate_component(
    Component {
        comp_ctx,
        dom_programs,
        params,
        root_block,
        span_typedefs,
        fn_stmts,
        methods,
    }: Component,
) -> TokenStream {
    let component_ident = &comp_ctx.component_ident;
    let props_ident = &comp_ctx.public_props_ident;
    let mod_ident = &comp_ctx.mod_ident;
    let hypp_ns = comp_ctx.namespace.hypp_ns();

    let PublicPropsStruct {
        tokens: public_props_struct,
        has_p_lifetime,
    } = gen_public_props_struct(&params, &comp_ctx);
    let env_struct = gen_env_struct(&params);
    let fn_props_destructuring = gen_fn_props_destructuring(&params, &comp_ctx);
    let env_locals = gen_env_locals(&params);
    let props_updater = gen_props_updater(&params);
    let shim = if comp_ctx.kind.is_self_updatable() {
        gen_shim_struct_and_impl(&params, methods)
    } else {
        quote! {}
    };

    let component_inner_ty = match comp_ctx.kind {
        ir::ComponentKind::Basic => quote! {
            ::hypp::comp::UniqueInner<#mod_ident::__Env, #mod_ident::__RootSpan<H>>
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            ::hypp::comp::SharedInner<H, Self, #mod_ident::__Env, #mod_ident::__RootSpan<H>>,
        },
    };

    let env_expr = gen_env_expr(&params);
    let n_params = params.len();
    let mount_body = match comp_ctx.kind {
        ir::ComponentKind::Basic => quote! {
            ::hypp::comp::UniqueInner::mount::<_, _, _, _, _, #n_params>(
                #env_expr,
                cursor,
                __patch,
                |inner| Self(inner),
            )
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            ::hypp::comp::SharedInner::mount::<_, _, _, _, #n_params>(
                #env_expr,
                cursor,
                __patch,
                |inner| Self(inner),
                |outer, weak_self| outer.0.weak_self = Some(weak_self),
            )
        },
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

    let pass_props_patch_call = match comp_ctx.kind {
        ir::ComponentKind::Basic => quote! {
            __patch(
                ::hypp::Duplex::In(&mut self.0.root_span),
                &self.0.env,
                &__updates,
                &mut ::hypp::patch::PatchCtx {
                    cur: __cursor,
                }
            ).unwrap();
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            let mut binder = ::hypp::shim::SelfBinder::from_opt_weak(&self.0.weak_self);
            __patch(
                ::hypp::Duplex::In(&mut self.0.root_span),
                &self.0.env,
                &__updates,
                &mut ::hypp::patch::PatchBindCtx {
                    cur: __cursor,
                    bind: &mut binder,
                }
            ).unwrap();
        },
    };

    let handle_path = root_block.handle_kind.handle_path();

    let mount_impl = if params.iter().filter(|param| param.is_prop()).count() == 0 {
        Some(quote! {
            impl<'p, H: ::hypp::Hypp + 'static> ::hypp::Mount<'p, H> for #component_ident<H> {
                fn mount(cursor: &mut H::Cursor<__NS>) -> Result<#handle_path<Self>, ::hypp::Error> {
                    Self::mount(#props_ident {}, cursor)
                }
            }
        })
    } else {
        None
    };

    quote! {
        #public_props_struct

        pub struct #component_ident<H: ::hypp::Hypp + 'static>(#component_inner_ty);

        mod #mod_ident {
            use super::*;

            // Probably shouldn't do this, because it could shadow users' types
            type __NS = #hypp_ns;

            // Also the simple names of these may shadow.. Should use underscore
            #env_struct
            #shim

            impl<H: ::hypp::Hypp + 'static> #component_ident<H> {
                pub fn mount(#fn_props_destructuring, cursor: &mut H::Cursor<__NS>) -> Result<#handle_path<Self>, ::hypp::Error> {
                    #mount_body
                }
            }

            impl<H: ::hypp::Hypp + 'static> ::hypp::handle::ToHandle for #component_ident<H> {
                type Handle = #handle_path<Self>;
            }

            impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for #component_ident<H> {
                fn is_anchored(&self) -> bool {
                    self.0.is_anchored()
                }

                fn pass(&mut self, cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
                    self.0.pass(cursor, op)
                }
            }

            impl<'p, H: ::hypp::Hypp + 'static> ::hypp::Component<'p, H> for #component_ident<H> {
                type Props = #props_ident #public_props_generics;
                type NS = __NS;

                fn pass_props(&mut self, #fn_props_destructuring, __cursor: &mut H::Cursor<Self::NS>) {
                    #props_updater
                    #pass_props_patch_call
                }
            }

            #(#dom_programs)*
            #(#span_typedefs)*

            #shim_impls
            #mount_impl

            #patch_fn
        }
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
fn gen_env_struct(params: &[param::Param]) -> TokenStream {
    let fields = params.iter().map(|param| {
        let ident = &param.ident;
        let ty = param.owned_ty_tokens();

        quote! {
            pub #ident: #ty,
        }
    });

    quote! {
        pub struct __Env {
            #(#fields)*
        }
    }
}

fn gen_env_expr(params: &[param::Param]) -> TokenStream {
    let env_params = params.iter().map(param::Param::owned_struct_param_tokens);
    quote! {
        __Env {
            #(#env_params)*
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
                let #ident = &__env.#ident;
            },
        }
    });

    quote! {
        #(#bindings)*
    }
}

fn gen_props_updater(params: &[param::Param]) -> TokenStream {
    let n_params = params.len();

    let checks = params.iter().filter(|param| param.is_prop()).map(|param| {
        let id = param.id as usize;
        let ident = &param.ident;

        match &param.kind {
            param::ParamKind::Prop(root_ty) => {
                let self_prop_as_ref = match root_ty {
                    param::ParamRootType::One(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { self.0.env.#ident },
                        param::ParamLeafType::Ref(_) => quote! { &self.0.env.#ident },
                    },
                    param::ParamRootType::Option(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { self.0.env.#ident },
                        param::ParamLeafType::Ref(_) => quote! {self.0.env.#ident.as_deref() },
                    },
                };

                let write = match root_ty {
                    param::ParamRootType::One(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { self.0.env.#ident = #ident; },
                        param::ParamLeafType::Ref(_) => quote! {
                            self.0.env.#ident = #ident.to_owned();
                        },
                    },
                    param::ParamRootType::Option(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { self.0.env.#ident = #ident; },
                        param::ParamLeafType::Ref(_) => quote! {
                            self.0.env.#ident = #ident.map(|val| val.to_owned());
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

fn gen_shim_struct_and_impl(params: &[param::Param], methods: Vec<syn::ItemFn>) -> TokenStream {
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
        pub struct __Shim<'c> {
            #(#fields)*
        }

        impl<'c> __Shim<'c> {
            #(#methods)*
        }
    }
}

fn gen_shim_impls(params: &[param::Param], comp_ctx: &CompCtx) -> TokenStream {
    let component_ident = &comp_ctx.component_ident;

    let state_update_idents = params
        .iter()
        .map(|param| match &param.kind {
            param::ParamKind::Prop(_) => None,
            param::ParamKind::State(_) => Some(quote::format_ident!("update_{}", param.id)),
        })
        .collect::<Vec<_>>();

    let updates_locals = state_update_idents.iter().map(|opt_ident| {
        opt_ident
            .as_ref()
            .map(|ident| quote! { let mut #ident = false; })
    });

    let env_fields = params.iter().map(|param| {
        let ident = &param.ident;
        match &param.kind {
            param::ParamKind::Prop(_) => quote! {
                #ident: &self.0.env.#ident,
            },
            param::ParamKind::State(_) => {
                let update_ident = state_update_idents[param.id as usize].as_ref().unwrap();

                quote! {
                    #ident: ::hypp::state_ref::StateRef::new(&mut self.0.env.#ident, &mut #update_ident),
                }
            }
        }
    });

    let updates_array_items = state_update_idents.iter().map(|opt_ident| match opt_ident {
        Some(ident) => quote! { #ident },
        None => quote! { false },
    });

    quote! {
        impl<'p, H: ::hypp::Hypp + 'static> ::hypp::shim::ShimTrampoline for #component_ident<H> {
            type Shim<'s> = __Shim<'s>;

            fn shim_trampoline(&mut self, method: ::hypp::shim::ShimMethod<Self>)
            {
                #(#updates_locals)*

                let mut shim = __Shim {
                    #(#env_fields)*
                };

                method.0(&mut shim);

                let mut cursor = self.0.anchor.create_cursor();
                let mut binder = ::hypp::shim::SelfBinder::from_opt_weak(&self.0.weak_self);
                __patch(
                    ::hypp::Duplex::In(&mut self.0.root_span),
                    &self.0.env,
                    &[#(#updates_array_items),*],
                    &mut ::hypp::patch::PatchBindCtx {
                        cur: &mut cursor,
                        bind: &mut binder,
                    }
                ).unwrap();
            }
        }
    }
}
