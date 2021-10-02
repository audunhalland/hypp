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
            generics,
            params,
            methods,
            template,
        } = input.parse()?;

        let ns = generics.ns;

        let (component_kind, root_block) =
            lowering::lower_root_node(template, generics.ns.traversal_direction(), &params)?;

        // Root idents which are used as prefixes for every global ident generated:
        let comp_ctx = CompCtx::new(ident, generics, component_kind, ns);

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

struct PropsEnvStructs {
    props_struct: TokenStream,
    // Generic arguments, with lifetime for props
    props_gen_args: Option<TokenStream>,

    env_struct: TokenStream,
    // Generic arguments for env, no lifetime
    env_gen_args: Option<TokenStream>,
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

    let PropsEnvStructs {
        props_struct,
        props_gen_args,
        env_struct,
        env_gen_args,
    } = gen_props_env_structs(&params, &comp_ctx);
    let fn_props_destructuring = gen_fn_props_destructuring(&params, &env_gen_args, &comp_ctx);
    let env_locals = gen_env_locals(&params);
    let props_updater = gen_props_updater(&params);
    let shim = if comp_ctx.kind.is_self_updatable() {
        gen_shim_struct_and_impl(&params, methods)
    } else {
        quote! {}
    };

    // Ident `H` in `H: ::hypp::Hypp`
    let hypp_ident = &comp_ctx.generics.hypp_ident;
    let public_generic_params = &comp_ctx.generics.public.params;
    let public_generic_arguments = &comp_ctx.generics.public.arguments;

    let component_inner_ty = match comp_ctx.kind {
        ir::ComponentKind::Basic => quote! {
            ::hypp::comp::UniqueInner<#mod_ident::__Env #env_gen_args, #mod_ident::__RootSpan<#hypp_ident>>
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            ::hypp::comp::SharedInner<#hypp_ident, Self, #mod_ident::__Env #env_gen_args, #mod_ident::__RootSpan<#hypp_ident>>,
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

    let patch_fn = crate::patch::gen_patch_fn(
        &root_block,
        &comp_ctx,
        env_locals,
        &env_gen_args,
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
                false, // invalidated
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
                false, // invalidated,
                &__updates,
                &mut ::hypp::patch::PatchBindCtx {
                    cur: __cursor,
                    bind: &mut binder,
                }
            ).unwrap();
        },
    };

    let handle_path = root_block.handle_kind.handle_path(&comp_ctx);

    let mount_impl = if params.iter().filter(|param| param.is_prop()).count() == 0 {
        Some(quote! {
            impl<'p, #(#public_generic_params),*> ::hypp::Mount<'p, #hypp_ident> for #component_ident<#(#public_generic_arguments),*> {
                fn mount(cursor: &mut #hypp_ident::Cursor<__NS>) -> Result<#handle_path<Self>, ::hypp::Error> {
                    Self::mount(#props_ident {}, cursor)
                }
            }
        })
    } else {
        None
    };

    quote! {
        #props_struct

        pub struct #component_ident<#(#public_generic_params),*>(#component_inner_ty);

        mod #mod_ident {
            use super::*;

            // Probably shouldn't do this, because it could shadow users' types
            type __NS = #hypp_ns;

            // Also the simple names of these may shadow.. Should use underscore
            #env_struct
            #shim

            impl<#(#public_generic_params),*> #component_ident<#(#public_generic_arguments),*> {
                pub fn mount(#fn_props_destructuring, cursor: &mut #hypp_ident::Cursor<__NS>) -> Result<#handle_path<Self>, ::hypp::Error> {
                    #mount_body
                }
            }

            impl<#(#public_generic_params),*> ::hypp::handle::ToHandle for #component_ident<#(#public_generic_arguments),*> {
                type Handle = #handle_path<Self>;
            }

            impl<#(#public_generic_params),*> ::hypp::Span<#hypp_ident> for #component_ident<#(#public_generic_arguments),*> {
                fn is_anchored(&self) -> bool {
                    self.0.is_anchored()
                }

                fn pass(&mut self, cursor: &mut dyn ::hypp::Cursor<#hypp_ident>, op: ::hypp::SpanOp) -> bool {
                    self.0.pass(cursor, op)
                }
            }

            impl<'p, #(#public_generic_params),*> ::hypp::Component<'p, #hypp_ident> for #component_ident<#(#public_generic_arguments),*> {
                type Props = #props_ident #props_gen_args;
                type NS = __NS;

                fn pass_props(&mut self, #fn_props_destructuring, __cursor: &mut #hypp_ident::Cursor<Self::NS>) {
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

fn gen_props_env_structs(params: &[param::Param], comp_ctx: &CompCtx) -> PropsEnvStructs {
    let props_ident = &comp_ctx.public_props_ident;

    let mut has_p_lifetime = false;

    let props_fields = params
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

    let env_fields = params.iter().map(|param| {
        let ident = &param.ident;
        let ty = param.owned_ty_tokens();

        quote! {
            pub #ident: #ty,
        }
    });

    let props_lifetime = if has_p_lifetime {
        Some(quote! { 'p, })
    } else {
        None
    };

    let item_generics = &comp_ctx.generics.internal;

    match (props_lifetime, item_generics) {
        // Simple case, no generics at all
        (None, None) => PropsEnvStructs {
            props_struct: quote! {
                pub struct #props_ident {
                    #(#props_fields)*
                }
            },
            props_gen_args: None,
            env_struct: quote! {
                pub struct __Env {
                    #(#env_fields)*
                }
            },
            env_gen_args: None,
        },
        (props_lifetime, generics) => {
            let generic_params = generics.as_ref().map(|item_generics| {
                let params = &item_generics.params;
                quote! { #(#params),* }
            });
            let generic_arguments = generics.as_ref().map(|item_generics| {
                let arguments = &item_generics.arguments;
                quote! { #(#arguments),* }
            });

            PropsEnvStructs {
                props_struct: quote! {
                    pub struct #props_ident<#props_lifetime #generic_params> {
                        #(#props_fields)*
                    }
                },
                props_gen_args: Some(quote! { <#props_lifetime #generic_arguments> }),

                env_struct: quote! {
                    pub struct __Env<#generic_params> {
                        #(#env_fields)*
                    }
                },
                env_gen_args: Some(quote! { <#generic_arguments> }),
            }
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

fn gen_fn_props_destructuring(
    params: &[param::Param],
    env_gen_args: &Option<TokenStream>,
    comp_ctx: &CompCtx,
) -> syn::FnArg {
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
        }: #props_ident #env_gen_args
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

    let public_generic_params = &comp_ctx.generics.public.params;
    let public_generic_arguments = &comp_ctx.generics.public.arguments;

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
        impl<'p, #(#public_generic_params),*> ::hypp::shim::ShimTrampoline for #component_ident<#(#public_generic_arguments),*> {
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
                    false, // invalidated
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
