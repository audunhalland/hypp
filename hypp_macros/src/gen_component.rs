//!
//! Main code generator for one component
//!

use proc_macro2::TokenStream;
use quote::quote;

use crate::component::Component;
use crate::ir;
use crate::param;

use crate::gen_misc::*;

struct PropsEnvStructs {
    props_struct: TokenStream,
    // Generic arguments, with lifetime for props
    props_gen_args: Option<TokenStream>,

    env_struct: TokenStream,
    // Generic arguments for env, no lifetime
    env_gen_args: Option<TokenStream>,
}

pub fn generate_component(comp: Component) -> TokenStream {
    let component_ident = &comp.component_ident;
    let props_ident = &comp.public_props_ident;
    let mod_ident = &comp.mod_ident;
    let hypp_ns = comp.namespace.hypp_ns();

    let mut dom_programs = vec![];
    collect_dom_programs(&comp.root_block.statements, &mut dom_programs);

    let mut span_typedefs = vec![];
    collect_span_typedefs(&comp.root_block, None, &comp, &mut span_typedefs);

    let PropsEnvStructs {
        props_struct,
        props_gen_args,
        env_struct,
        env_gen_args,
    } = gen_props_env_structs(&comp);
    let fn_props_destructuring = gen_fn_props_destructuring(&env_gen_args, &comp);
    let env_locals = gen_env_locals(&comp.params);
    let props_updater = gen_props_updater(&comp.params);
    let shim = if comp.kind.is_self_updatable() {
        gen_shim_struct_and_impl(&comp)
    } else {
        quote! {}
    };

    // Ident `H` in `H: ::hypp::Hypp`
    let hypp_ident = &comp.generics.hypp_ident;
    let public_generic_params = &comp.generics.public.params;
    let public_generic_arguments = &comp.generics.public.arguments;

    let component_inner_ty = match comp.kind {
        ir::ComponentKind::Basic => quote! {
            ::hypp::comp::UniqueInner<#mod_ident::__Env #env_gen_args, #mod_ident::__RootSpan<#hypp_ident>>
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            ::hypp::comp::SharedInner<#hypp_ident, Self, #mod_ident::__Env #env_gen_args, #mod_ident::__RootSpan<#hypp_ident>>,
        },
    };

    let env_expr = gen_env_expr(&comp.params);
    let mount_body = match comp.kind {
        ir::ComponentKind::Basic => quote! {
            ::hypp::comp::UniqueInner::mount::<_, _, _, _, _>(
                #env_expr,
                cursor,
                __patch,
                |inner| Self(inner),
            )
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            ::hypp::comp::SharedInner::mount::<_, _, _>(
                #env_expr,
                cursor,
                __patch,
                |inner| Self(inner),
            )
        },
    };

    let shim_impls = if comp.kind.is_self_updatable() {
        Some(gen_shim_impls(&comp))
    } else {
        None
    };

    let patch_fn = crate::gen_patch::gen_patch_fn(
        &comp.root_block,
        &comp,
        env_locals,
        &env_gen_args,
        CodegenCtx {
            component_kind: comp.kind,
            function: Function::Patch,
            scope: Scope::Component,
        },
    );

    let pass_props_patch_call = match comp.kind {
        ir::ComponentKind::Basic => quote! {
            __patch(
                ::hypp::Duplex::In(&mut self.0.root_span),
                &self.0.env,
                ::hypp::Deviation::Partial(&__refresh_params),
                &mut ::hypp::patch::PatchCtx {
                    cur: __cursor,
                }
            ).unwrap();
        },
        ir::ComponentKind::SelfUpdatable => quote! {
            __patch(
                ::hypp::Duplex::In(&mut self.0.root_span),
                &self.0.env,
                ::hypp::Deviation::Partial(&__refresh_params),
                &mut ::hypp::patch::PatchBindCtx {
                    cur: __cursor,
                    closure_env: self.0.closure_env.clone().unwrap(),
                }
            ).unwrap();
        },
    };

    let handle_path = comp.root_block.handle_kind.handle_path(&comp);

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

            impl<#(#public_generic_params),*> ::hypp::handle::ToHandle<#hypp_ident> for #component_ident<#(#public_generic_arguments),*> {
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

            impl<'a, #(#public_generic_params),*> ::hypp::Component<'a, #hypp_ident> for #component_ident<#(#public_generic_arguments),*> {
                type Props = #props_ident #props_gen_args;
                type NS = __NS;

                fn mount(
                    #fn_props_destructuring,
                    cursor: &mut #hypp_ident::Cursor<__NS>
                ) -> Result<#handle_path<Self>, ::hypp::Error> {
                    #mount_body
                }

                fn pass_props(
                    &mut self,
                    #fn_props_destructuring,
                    __cursor: &mut #hypp_ident::Cursor<Self::NS>
                ) {
                    #props_updater
                    #pass_props_patch_call
                }
            }

            #(#dom_programs)*
            #(#span_typedefs)*

            #shim_impls

            #patch_fn
        }
    }
}

fn gen_props_env_structs(comp: &Component) -> PropsEnvStructs {
    let props_ident = &comp.public_props_ident;

    let mut has_p_lifetime = false;

    let props_fields = comp
        .params
        .iter()
        .filter_map(|param| match &param.triple.0 {
            param::ParamKind::Prop => Some((&param.ident, &param.triple)),
            param::ParamKind::State => None,
        })
        .map(|(ident, triple)| {
            use param::*;

            let ty = match triple {
                (_, Quantifier::Unit, Ty::Owned(ty)) => quote! { #ty },
                (_, Quantifier::Option(option), Ty::Owned(ty)) => quote! { #option<#ty> },

                (_, Quantifier::Unit, Ty::Reference(r)) => {
                    has_p_lifetime = true;
                    quote! { #r }
                }
                (_, Quantifier::Option(option), Ty::Reference(r)) => {
                    has_p_lifetime = true;
                    quote! { #option<#r> }
                }
            };

            quote! {
                pub #ident: (#ty, ::hypp::Refresh),
            }
        })
        .collect::<Vec<_>>();

    let props_default_impl = if props_fields.is_empty() {
        Some(quote! {
            impl Default for #props_ident {
                fn default() -> Self {
                    Self {}
                }
            }
        })
    } else {
        None
    };

    let env_fields = comp.params.iter().map(|param| {
        let ident = &param.ident;
        let ty = param.owned_ty_tokens();

        quote! {
            pub #ident: #ty,
        }
    });

    let props_lifetime = if has_p_lifetime {
        Some(quote! { 'a, })
    } else {
        None
    };

    let item_generics = &comp.generics.internal;

    match (props_lifetime, item_generics) {
        // Simple case, no generics at all
        (None, None) => PropsEnvStructs {
            props_struct: quote! {
                pub struct #props_ident {
                    #(#props_fields)*
                }
                #props_default_impl
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
                    #props_default_impl
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
    let env_params = params
        .iter()
        .map(param::Param::props_to_owned_struct_param_tokens);
    quote! {
        __Env {
            #(#env_params)*
        }
    }
}

fn gen_fn_props_destructuring(env_gen_args: &Option<TokenStream>, comp: &Component) -> syn::FnArg {
    let props_ident = &comp.public_props_ident;

    let fields = comp
        .params
        .iter()
        .filter_map(|param| match &param.triple.0 {
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
        }: #props_ident #env_gen_args
    }
}

fn gen_env_locals(params: &[param::Param]) -> TokenStream {
    let bindings = params.iter().map(|param| {
        let ident = &param.ident;

        use param::*;

        match &param.triple {
            (ParamKind::State, _, _) => quote! {
                // err... take reference maybe?
                let #ident = &__env.#ident;
            },

            (ParamKind::Prop, Quantifier::Unit, Ty::Owned(_)) => quote! {
                let #ident = __env.#ident;
            },
            (ParamKind::Prop, Quantifier::Unit, Ty::Reference(_)) => {
                // Remember, when storing a _reference_ prop inside self,
                // we use the `<T as ToOwned>::Owned` type to store it..
                // so we can just take the reference to that again!
                quote! {
                    let #ident = &__env.#ident;
                }
            }
            (ParamKind::Prop, Quantifier::Option(_), Ty::Owned(_)) => quote! {
                let #ident = __env.#ident;
            },
            (ParamKind::Prop, Quantifier::Option(_), Ty::Reference(_)) => quote! {
                let #ident = __env.#ident.as_deref();
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

        use param::*;

        let opt_write = match &param.triple {
            (ParamKind::State, _, _) => None,
            (_, _, Ty::Owned(_)) => Some(quote! {
                self.0.env.#ident = #ident.0;
            }),
            (_, Quantifier::Unit, Ty::Reference(_)) => Some(quote! {
                self.0.env.#ident = #ident.0.to_owned();
            }),
            (_, Quantifier::Option(_), Ty::Reference(_)) => Some(quote! {
                self.0.env.#ident = #ident.0.map(|val| val.to_owned());
            }),
        };

        opt_write.map(|write| {
            quote! {
                if #ident.1.0 {
                    #write
                    __refresh_params[#id] = true;
                }
            }
        })
    });

    quote! {
        // TODO: use bitvec?
        let mut __refresh_params: [bool; #n_params] = [false; #n_params];

        #(#checks)*
    }
}

fn gen_shim_struct_and_impl(comp: &Component) -> TokenStream {
    let methods = &comp.methods;
    let fields = comp.params.iter().map(|param| {
        let ident = &param.ident;

        use param::*;

        let ty = match &param.triple {
            (ParamKind::State, _, Ty::Owned(ty)) => quote! {
                ::hypp::state_ref::StateRef<'a, #ty>
            },
            (ParamKind::State, _, Ty::Reference(_)) => unimplemented!("reference in state"),

            (ParamKind::Prop, Quantifier::Unit, Ty::Owned(ty)) => quote! { &'a #ty },
            (ParamKind::Prop, Quantifier::Unit, Ty::Reference(r)) => quote! { #r },
            (ParamKind::Prop, Quantifier::Option(option), Ty::Owned(ty)) => {
                quote! { #option<&'a #ty> }
            }
            (ParamKind::Prop, Quantifier::Option(option), Ty::Reference(r)) => {
                quote! { #option<#r> }
            }
        };

        quote! {
            #ident: #ty,
        }
    });

    let hypp_ident = &comp.generics.hypp_ident;

    quote! {
        #[allow(dead_code)]
        pub struct __Shim<'a, #hypp_ident: ::hypp::Hypp> {
            #(#fields)*
            __phantom: std::marker::PhantomData<#hypp_ident>,
        }

        impl<'a, #hypp_ident: ::hypp::Hypp> __Shim<'a, #hypp_ident> {
            #(#methods)*
        }
    }
}

fn gen_shim_impls(comp: &Component) -> TokenStream {
    let component_ident = &comp.component_ident;
    let hypp_ident = &comp.generics.hypp_ident;

    let public_generic_params = &comp.generics.public.params;
    let public_generic_arguments = &comp.generics.public.arguments;

    let state_update_idents = comp
        .params
        .iter()
        .map(|param| match &param.triple.0 {
            param::ParamKind::Prop => None,
            param::ParamKind::State => Some(quote::format_ident!("update_{}", param.id)),
        })
        .collect::<Vec<_>>();

    let updates_locals = state_update_idents.iter().map(|opt_ident| {
        opt_ident
            .as_ref()
            .map(|ident| quote! { let mut #ident = false; })
    });

    let env_fields = comp.params.iter().map(|param| {
        let ident = &param.ident;
        match &param.triple.0 {
            param::ParamKind::Prop => quote! {
                #ident: &self.0.env.#ident,
            },
            param::ParamKind::State => {
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
        impl<'a, #(#public_generic_params),*> ::hypp::shim::ShimTrampoline for #component_ident<#(#public_generic_arguments),*> {
            type Shim<'s> = __Shim<'s, #hypp_ident>;

            fn shim_trampoline(&mut self, method: &mut dyn for<'s> FnMut(&'s mut Self::Shim<'s>))
            {
                #(#updates_locals)*

                let mut shim = __Shim {
                    #(#env_fields)*
                    __phantom: std::marker::PhantomData
                };

                method(&mut shim);

                let mut cursor = self.0.anchor.create_cursor();
                __patch(
                    ::hypp::Duplex::In(&mut self.0.root_span),
                    &self.0.env,
                    ::hypp::Deviation::Partial(&[#(#updates_array_items),*]),
                    &mut ::hypp::patch::PatchBindCtx {
                        cur: &mut cursor,
                        closure_env: self.0.closure_env.clone().unwrap(),
                    }
                ).unwrap();
            }
        }
    }
}
