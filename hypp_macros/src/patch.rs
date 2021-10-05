//!
//! Code for generating the `patch` function,
//! basically the final Rust output of a root template_ast::Node.
//!

use proc_macro2::TokenStream;
use quote::quote;

use crate::ir;
use crate::misc_codegen::*;
use crate::template_ast;

pub fn gen_patch_fn(
    block: &ir::Block,
    comp_ctx: &CompCtx,
    env_locals: TokenStream,
    env_gen_args: &Option<TokenStream>,
    fn_stmts: Vec<syn::Stmt>,
    ctx: CodegenCtx,
) -> TokenStream {
    let patch_ctx_ty_root = &comp_ctx.patch_ctx_ty_root;

    let hypp_ident = &comp_ctx.generics.hypp_ident;
    let public_generic_params = &comp_ctx.generics.public.params;

    let Body {
        closures,
        mount_locals,
        mount_expr,
        patch,
    } = compile_body(block, comp_ctx, ctx, SpanConstructorKind::FixedSpan(None));

    let fields = block
        .struct_fields
        .iter()
        .map(ir::StructField::mut_pattern_tokens);

    quote! {
        pub fn __patch<#(#public_generic_params),*>(
            __root: ::hypp::Duplex<__RootSpan<#hypp_ident>>,
            __env: &__Env #env_gen_args,
            __deviation: ::hypp::Deviation<'_>,
            __ctx: &mut #patch_ctx_ty_root,
        ) -> Result<(), ::hypp::Error> {
            #env_locals

            #(#fn_stmts)*

            #(#closures)*

            match __root {
                ::hypp::Duplex::Out(__root) => {
                    #mount_locals
                    *__root = Some(#mount_expr);
                }
                ::hypp::Duplex::In(__RootSpan { #(#fields)* .. }) => {
                    #patch
                }
            }
            Ok(())
        }
    }
}

// body of a block before it's 'serialized'.
struct Body<'c> {
    /// helper functions used by 'mount' and 'patch',
    /// to avoid code duplication.
    closures: Vec<Closure<'c>>,

    mount_locals: TokenStream,

    mount_expr: TokenStream,

    // code used when the body span already exists, and should
    // be selectively updated
    patch: TokenStream,
}

struct Closure<'c> {
    comp_ctx: &'c CompCtx,
    sig: ClosureSig,
    args: TokenStream,
    body: TokenStream,
}

struct ClosureSig {
    ident: syn::Ident,
    kind: ClosureKind,
}

impl ClosureSig {
    fn from_stmt(stmt: &ir::Statement, kind: ClosureKind) -> Self {
        let field = stmt.field.unwrap();

        let ident = match kind {
            ClosureKind::Match => quote::format_ident!("__patch_f{}", field.0),
            ClosureKind::ListSpan => quote::format_ident!("__list_f{}", field.0),
            ClosureKind::String => quote::format_ident!("__string_f{}", field.0),
        };

        Self { ident, kind }
    }
}

#[derive(Clone, Copy)]
enum ClosureKind {
    // Closure for patching a `Match` statement
    Match,

    // Closure for patching a list span using an iteration
    ListSpan,

    // Closure for producing a string value (that may use env variables)
    String,
}

fn compile_body<'c>(
    block: &ir::Block,
    comp_ctx: &'c CompCtx,
    ctx: CodegenCtx,
    constructor_kind: SpanConstructorKind<'_>,
) -> Body<'c> {
    enum Mutability {
        Let,
        LetMut,
    }

    let hypp_ident = &comp_ctx.generics.hypp_ident;

    struct FieldInit<'b> {
        mutability: Mutability,
        field_ident: Option<&'b ir::FieldIdent>,
        init: TokenStream,
        post_init: Option<TokenStream>,
    }

    let mut closures: Vec<Closure> = vec![];
    let mut field_inits: Vec<FieldInit> = vec![];
    let mut patch_stmts: Vec<TokenStream> = vec![];

    for stmt in &block.statements {
        match &stmt.expression {
            ir::Expression::ConstDom(program) => {
                let program_ident = program.get_ident();
                let last_node_opcode = program.last_node_opcode();

                field_inits.push(FieldInit {
                    mutability: Mutability::Let,
                    field_ident: stmt.field.as_ref(),
                    init: match last_node_opcode {
                        Some((ir::DomOpCodeKind::Enter(_), _) | (ir::DomOpCodeKind::Exit, _)) => {
                            quote! {
                                __ctx.cur.const_exec_element(&#program_ident)?;
                            }
                        }
                        Some((ir::DomOpCodeKind::Text(_), _)) => quote! {
                            __ctx.cur.const_exec_text(&#program_ident)?;
                        },
                        _ => panic!(),
                    },
                    post_init: None,
                });

                patch_stmts.push(match &stmt.field {
                    Some(field) => {
                        // OPTIMIZATION: Can advance the cursor directly!

                        let field_expr = FieldExpr(*field, ctx);
                        match last_node_opcode {
                            Some((ir::DomOpCodeKind::Enter(_), _)) => quote! {
                                __ctx.cur.move_to_children_of(&#field_expr);
                            },
                            Some((ir::DomOpCodeKind::Exit | ir::DomOpCodeKind::Text(_), _)) => {
                                quote! {
                                    __ctx.cur.move_to_following_sibling_of(#field_expr.as_node());
                                }
                            }
                            _ => panic!(),
                        }
                    }
                    None => {
                        quote! {
                            __ctx.cur.skip_const_program(&#program_ident);
                        }
                    }
                });
            }
            ir::Expression::AttributeCallback(callback) => match callback {
                ir::Callback::Expr(expr) => {
                    let field = stmt.field.as_ref().unwrap();

                    field_inits.push(FieldInit {
                        mutability: Mutability::LetMut,
                        field_ident: stmt.field.as_ref(),
                        init: quote! {
                            __ctx.cur.attribute_slot::<::hypp::html::HtmlEventKind>()?;
                        },
                        post_init: Some(quote! {
                            <
                                ::hypp::Slot<#hypp_ident, __NS, ::hypp::html::HtmlEventKind>
                                as
                                ::hypp::Subscribe<#hypp_ident, _>
                            >::subscribe(&mut #field, #expr.clone());
                        }),
                    });
                }
                ir::Callback::SelfMethod {
                    local_field: closure_field,
                    method_ident,
                } => {
                    let field = stmt.field.as_ref().unwrap();

                    // Closure:
                    field_inits.push(FieldInit {
                        mutability: Mutability::Let,
                        field_ident: Some(closure_field),
                        init: quote! {
                            __ctx.closure_env.new_closure(&|shim: &mut __Shim| {
                                shim.#method_ident();
                            });
                        },
                        post_init: None,
                    });

                    // Slot:
                    field_inits.push(FieldInit {
                        mutability: Mutability::LetMut,
                        field_ident: stmt.field.as_ref(),
                        init: quote! {
                            __ctx.cur.attribute_slot::<::hypp::html::HtmlEventKind>()?;
                        },
                        // Associate the slot with the closure:
                        post_init: Some(quote! {
                            <
                                ::hypp::Slot<#hypp_ident, __NS, ::hypp::html::HtmlEventKind>
                                as
                                ::hypp::Subscribe<#hypp_ident, _>
                            >::subscribe(&mut #field, #closure_field);
                        }),
                    });
                }
            },
            ir::Expression::Text(expr) => {
                let field_expr = FieldExpr(stmt.field.unwrap(), ctx);
                let refresh_expr = stmt.param_deps.param_refresh_expr();

                let closure = Closure {
                    comp_ctx,
                    sig: ClosureSig::from_stmt(stmt, ClosureKind::String),
                    args: quote! {},
                    body: quote! { #expr },
                };
                let closure_ident = &closure.sig.ident;

                field_inits.push(FieldInit {
                    mutability: Mutability::Let,
                    field_ident: stmt.field.as_ref(),
                    init: quote! {
                        __ctx.cur.text(#closure_ident().as_ref())?;
                    },
                    post_init: None,
                });

                let text_update = if stmt.param_deps.is_variable() {
                    Some(quote! {
                        if #refresh_expr.0 {
                            #hypp_ident::set_text(#field_expr, #closure_ident().as_ref());
                        }
                    })
                } else {
                    None
                };

                patch_stmts.push(quote! {
                    #text_update
                    __ctx.cur.move_to_following_sibling_of(#field_expr.as_node());
                });

                closures.push(closure);
            }
            ir::Expression::Component { path, props } => {
                let mut props_exprs: Vec<(&ir::ComponentPropArg, TokenStream)> = vec![];

                for prop_arg in props.iter() {
                    match &prop_arg.value {
                        template_ast::AttrValue::ImplicitTrue => {
                            props_exprs.push((prop_arg, quote! { true }));
                        }
                        template_ast::AttrValue::Literal(lit) => {
                            props_exprs.push((prop_arg, quote! { #lit }));
                        }
                        template_ast::AttrValue::Expr(expr) => {
                            props_exprs.push((prop_arg, quote! { #expr }));
                        }
                        template_ast::AttrValue::SelfMethod(method_ident) => {
                            // Construct a callback wrapping the self method

                            let closure_field = prop_arg
                                .local_field
                                .as_ref()
                                .expect("Expected a local for closure creation");

                            // Push field init for the creation of the closure
                            field_inits.push(FieldInit {
                                mutability: Mutability::Let,
                                field_ident: Some(closure_field),
                                init: quote! {
                                    __ctx.closure_env.new_closure(&|shim: &mut __Shim| {
                                        shim.#method_ident();
                                    });
                                },
                                post_init: None,
                            });

                            props_exprs.push((prop_arg, quote! { &#closure_field }));
                        }
                    }
                }

                let component_path = &path.type_path;
                let props_path = path.props_path();

                let mount_props = props_exprs.iter().map(|(prop_arg, value)| {
                    let ident = &prop_arg.ident;
                    quote! { #ident: (#value, ::hypp::Refresh(true)) }
                });

                let mount_expr = quote! {
                    #component_path::mount(
                        #props_path {
                            #(#mount_props),*
                        },
                        __ctx.cur
                    )?
                };

                field_inits.push(FieldInit {
                    mutability: Mutability::Let,
                    field_ident: stmt.field.as_ref(),
                    init: match &ctx.scope {
                        Scope::Component | Scope::Iter => quote! { #mount_expr; },
                        Scope::DynamicSpan => quote! {
                            #mount_expr.into_boxed();
                        },
                    },
                    post_init: None,
                });

                // Only need a patch statement if the component actually depends on
                // variable parameters. If not, it is a constant.
                if stmt.param_deps.is_variable() {
                    let field_expr = FieldExpr(stmt.field.unwrap(), ctx);
                    let test = stmt.param_deps.param_refresh_expr();

                    let props_with_refresh = props_exprs.into_iter().map(|(prop_arg, value)| {
                        let ident = &prop_arg.ident;
                        let refresh_expr = prop_arg.param_deps.param_refresh_expr();
                        quote! { #ident: (#value, #refresh_expr) }
                    });

                    patch_stmts.push(quote! {
                        if #test.0 {
                            #field_expr.get_mut().pass_props(
                                #props_path {
                                    #(#props_with_refresh),*
                                },
                                __ctx.cur
                            );
                        } else {
                            // Nothing has changed, but the cursor must pass
                            // over the component. This should be very cheap.
                            #field_expr.get_mut().pass_over(__ctx.cur);
                        }
                    });
                }
            }
            ir::Expression::Match {
                span_type,
                expr,
                arms,
            } => {
                let closure_sig = ClosureSig::from_stmt(stmt, ClosureKind::Match);
                let closure =
                    gen_match_closure(stmt, span_type, closure_sig, expr, arms, comp_ctx, ctx);

                let field = stmt.field.as_ref().unwrap();
                let closure_ident = &closure.sig.ident;

                field_inits.push(FieldInit {
                    mutability: Mutability::LetMut,
                    field_ident: stmt.field.as_ref(),
                    init: quote! { None; },
                    post_init: Some(quote! {
                        #closure_ident(&mut #field, __ctx)?;
                    }),
                });

                patch_stmts.push(quote! {
                    #closure_ident(#field, __ctx)?;
                });
                closures.push(closure);
            }
            ir::Expression::Iter {
                span_type,
                expr,
                variable,
                inner_block,
                ..
            } => {
                let closure_sig = ClosureSig::from_stmt(stmt, ClosureKind::ListSpan);
                let closure = gen_list_span_closure(
                    span_type,
                    closure_sig,
                    expr,
                    variable,
                    inner_block,
                    comp_ctx,
                    CodegenCtx {
                        scope: Scope::Iter,
                        ..ctx
                    },
                );

                let field = stmt.field.as_ref().unwrap();
                let closure_ident = &closure.sig.ident;

                field_inits.push(FieldInit {
                    mutability: Mutability::LetMut,
                    field_ident: stmt.field.as_ref(),
                    init: quote! { ::hypp::list::SimpleListSpan::new(); },
                    post_init: Some(quote! {
                        #closure_ident(&mut #field, __ctx)?;
                    }),
                });
                patch_stmts.push(quote! {
                    #closure_ident(#field, __ctx)?;
                });
                closures.push(closure);
            }
        }
    }

    let mount_field_stmts = field_inits.into_iter().map(|field_init| {
        let init = field_init.init;
        let post_init = field_init.post_init;
        match &field_init.field_ident {
            None => quote! {
                #init
                #post_init
            },
            Some(field_ident) => match field_init.mutability {
                Mutability::Let => quote! {
                    let #field_ident = #init
                    #post_init
                },
                Mutability::LetMut => quote! {
                    let mut #field_ident = #init
                    #post_init
                },
            },
        }
    });

    let constructor_path = match constructor_kind {
        SpanConstructorKind::FixedSpan(opt_span_type) => match opt_span_type {
            Some(span_type) => {
                let path_segment =
                    span_type.to_tokens(ctx.scope, StructFieldFormat::PathSegment, comp_ctx);
                quote! {
                    #path_segment
                }
            }
            None => {
                quote! { __RootSpan }
            }
        },
        SpanConstructorKind::DynamicSpan { span_type, variant } => {
            let span_ident =
                span_type.to_tokens(ctx.scope, StructFieldFormat::PathSegment, comp_ctx);

            quote! {
                #span_ident::#variant
            }
        }
    };

    let struct_params = block
        .struct_fields
        .iter()
        .map(ir::StructField::struct_param_tokens);

    let phantom = match constructor_kind {
        SpanConstructorKind::FixedSpan(_) => {
            Some(quote! { __phantom: ::std::marker::PhantomData, })
        }
        SpanConstructorKind::DynamicSpan { .. } => None,
    };

    Body {
        closures,
        mount_locals: quote! {
            #(#mount_field_stmts)*
        },
        mount_expr: quote! {
            #constructor_path {
                #(#struct_params)* #phantom
            }
        },
        patch: quote! {
            #(#patch_stmts)*
        },
    }
}

fn gen_match_closure<'c>(
    statement: &ir::Statement,
    span_type: &ir::StructFieldType,
    closure_sig: ClosureSig,
    expr: &syn::Expr,
    arms: &[ir::Arm],
    comp_ctx: &'c CompCtx,
    ctx: CodegenCtx,
) -> Closure<'c> {
    let field = statement.field.unwrap();
    let field_expr = FieldExpr(field, ctx);
    let dynamic_span_path_segment =
        span_type.to_tokens(ctx.scope, StructFieldFormat::PathSegment, comp_ctx);

    let pattern_arms = arms.iter().map(
        |ir::Arm {
             variant,
             pattern,
             block,
             ..
         }| {
            let Body {
                closures,
                mount_locals,
                mount_expr,
                patch,
            } = compile_body(
                &block,
                comp_ctx,
                CodegenCtx {
                    scope: Scope::DynamicSpan,
                    ..ctx
                },
                SpanConstructorKind::DynamicSpan { span_type, variant },
            );

            // Don't generate patch code if the expression is constant
            let non_const_patch = if statement.param_deps.is_variable() {
                Some(patch)
            } else {
                None
            };

            let fields = block
                .struct_fields
                .iter()
                .map(ir::StructField::mut_pattern_tokens);

            quote! {
                #pattern => {
                    #(#closures)*

                    match &mut #field {
                        Some(#dynamic_span_path_segment::#variant { #(#fields)* .. }) => {
                            // The matching arm (this variant corresponds to 'expr')
                            #non_const_patch
                        }
                        _ => {
                            // The non-matching arm, erase and then re-mount
                            #field_expr.erase(__ctx.cur);
                            #mount_locals
                            *#field = Some(#mount_expr);
                        }
                    }
                },
            }
        },
    );

    let body = quote! {
        match #expr {
            #(#pattern_arms)*
        };
        Ok(())
    };

    let dynamic_span_full_type =
        span_type.to_tokens(ctx.scope, StructFieldFormat::TypeInStruct, comp_ctx);

    Closure {
        comp_ctx,
        sig: closure_sig,
        args: quote! {
            mut #field: &mut #dynamic_span_full_type,
        },
        body,
    }
}

fn gen_list_span_closure<'c>(
    span_type: &ir::StructFieldType,
    closure_sig: ClosureSig,
    iter_expr: &syn::Expr,
    iter_variable: &syn::Ident,
    inner_block: &ir::Block,
    comp_ctx: &'c CompCtx,
    ctx: CodegenCtx,
) -> Closure<'c> {
    let Body {
        closures,
        mount_locals,
        mount_expr,
        patch,
    } = compile_body(
        inner_block,
        comp_ctx,
        ctx,
        SpanConstructorKind::FixedSpan(Some(span_type)),
    );

    let hypp_ident = &comp_ctx.generics.hypp_ident;
    let fixed_span_path_segment =
        span_type.to_tokens(ctx.scope, StructFieldFormat::PathSegment, comp_ctx);

    let fields = inner_block
        .struct_fields
        .iter()
        .map(ir::StructField::mut_pattern_tokens);

    let body = quote! {
        __span.patch(#iter_expr.iter(), __deviation, __ctx, |#iter_variable, __span, __deviation, __ctx| {
            #(#closures)*

            match __span {
                ::hypp::Duplex::Out(__span) => {
                    #mount_locals
                    *__span = Some(#mount_expr);
                }
                ::hypp::Duplex::In(#fixed_span_path_segment { #(#fields)* .. }) => {
                    #patch
                }
            }

            Ok(())
        })
    };

    let fixed_span_full_type =
        span_type.to_tokens(ctx.scope, StructFieldFormat::InnerType, comp_ctx);

    Closure {
        comp_ctx,
        sig: closure_sig,
        args: quote! {
            __span: &mut ::hypp::list::SimpleListSpan<#hypp_ident, __NS, #fixed_span_full_type>,
        },
        body,
    }
}

impl<'c> quote::ToTokens for Closure<'c> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = &self.sig.ident;
        let args = &self.args;
        let patch_ctx_ty_inner = &self.comp_ctx.patch_ctx_ty_inner;
        let body = &self.body;

        let ctx_arg = match self.sig.kind {
            ClosureKind::Match | ClosureKind::ListSpan => Some(quote! {
                __ctx: &mut #patch_ctx_ty_inner
            }),
            ClosureKind::String => None,
        };

        let return_type = match self.sig.kind {
            ClosureKind::Match | ClosureKind::ListSpan => Some(quote! { -> ::hypp::Void }),
            ClosureKind::String => None,
        };

        let output = quote! {
            let #ident = |#args #ctx_arg| #return_type {
                #body
            };
        };
        tokens.extend(output);
    }
}
