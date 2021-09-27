//!
//! Code for generating the `patch` function,
//! basically the final Rust output of a root template_ast::Node.
//!

use proc_macro2::TokenStream;
use quote::quote;

use crate::callback;
use crate::ir;
use crate::misc_codegen::*;
use crate::template_ast;

pub fn gen_patch_fn(
    block: &ir::Block,
    comp_ctx: &CompCtx,
    env_locals: TokenStream,
    fn_stmts: Vec<syn::Stmt>,
    ctx: CodegenCtx,
) -> TokenStream {
    let patch_ctx_ty_root = &comp_ctx.patch_ctx_ty_root;

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
        pub fn patch<H: ::hypp::Hypp>(
            __root: ::hypp::InputOrOutput<RootSpan<H>>,
            __env: &Env,
            __updates: &[bool],
            __ctx: &mut #patch_ctx_ty_root,
        ) -> Result<(), ::hypp::Error> {
            #env_locals

            #(#fn_stmts)*

            // closures may shadow this variable
            let __invalidated = false;

            #(#closures)*

            match __root {
                ::hypp::InputOrOutput::Output(__root) => {
                    #mount_locals
                    *__root = Some(#mount_expr);
                }
                ::hypp::InputOrOutput::Input(RootSpan { #(#fields)* .. }) => {
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
            ClosureKind::IterItem => quote::format_ident!("__elem_f{}", field.0),
            ClosureKind::String => quote::format_ident!("__string_f{}", field.0),
        };

        Self { ident, kind }
    }
}

#[derive(Clone, Copy)]
enum ClosureKind {
    // Closure for patching a `Match` statement
    Match,

    // Closure for patching one span in an iteration
    IterItem,

    // Closure for producing a string value (that may use env variables)
    String,
}

fn compile_body<'c>(
    block: &ir::Block,
    comp_ctx: &'c CompCtx,
    ctx: CodegenCtx,
    constructor_kind: SpanConstructorKind<'_>,
) -> Body<'c> {
    enum FieldLocal {
        Let,
        LetMut,
    }

    struct FieldInit<'b> {
        local: FieldLocal,
        field_ident: &'b Option<ir::FieldIdent>,
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
                    local: FieldLocal::Let,
                    field_ident: &stmt.field,
                    init: match last_node_opcode {
                        Some(ir::DomOpCode::EnterElement(_) | ir::DomOpCode::ExitElement) => {
                            quote! {
                                __ctx.cur.const_exec_element(&#program_ident)?;
                            }
                        }
                        Some(ir::DomOpCode::Text(_)) => quote! {
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
                            Some(ir::DomOpCode::EnterElement(_)) => quote! {
                                __ctx.cur.move_to_children_of(&#field_expr);
                            },
                            Some(ir::DomOpCode::ExitElement | ir::DomOpCode::Text(_)) => quote! {
                                __ctx.cur.move_to_following_sibling_of(#field_expr.as_node());
                            },
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
            ir::Expression::AttributeCallback(callback::Callback { ident }) => {
                let field = stmt.field.as_ref().unwrap();
                let component_ident = &comp_ctx.component_ident;

                field_inits.push(FieldInit {
                    local: FieldLocal::Let,
                    field_ident: &stmt.field,
                    init: quote! {
                        __ctx.cur.attribute_value_callback()?;
                    },
                    post_init: Some(quote! {
                        __ctx.bind.bind(
                            #field.clone(),
                            ::hypp::ShimMethod::<#component_ident<H>>(&|shim| {
                                shim.#ident();
                            }),
                        );
                    }),
                });
            }
            ir::Expression::Text(expr) => {
                let field_expr = FieldExpr(stmt.field.unwrap(), ctx);
                let test = stmt.param_deps.update_test_tokens();

                let closure = Closure {
                    comp_ctx,
                    sig: ClosureSig::from_stmt(stmt, ClosureKind::String),
                    args: quote! {},
                    body: quote! { #expr },
                };
                let closure_ident = &closure.sig.ident;

                field_inits.push(FieldInit {
                    local: FieldLocal::Let,
                    field_ident: &stmt.field,
                    init: quote! {
                        __ctx.cur.text(#closure_ident().as_ref())?;
                    },
                    post_init: None,
                });

                let text_update = if stmt.param_deps.is_variable() {
                    Some(quote! {
                        if #test {
                            H::set_text(#field_expr, #closure_ident().as_ref());
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
                let prop_list: Vec<TokenStream> = props
                    .iter()
                    .map(|attr| {
                        let ident = &attr.ident;
                        match &attr.value {
                            template_ast::AttrValue::ImplicitTrue => quote! {
                                #ident: true,
                            },
                            template_ast::AttrValue::Literal(lit) => quote! {
                                #ident: #lit,
                            },
                            template_ast::AttrValue::Expr(expr) => quote! {
                                #ident: #expr,
                            },
                        }
                    })
                    .collect();

                let component_path = &path.type_path;
                let props_path = path.props_path();

                let mount_expr = quote! {
                    #component_path::mount(
                        #props_path {
                            #(#prop_list)*
                        },
                        __ctx.cur
                    )?
                };

                field_inits.push(FieldInit {
                    local: FieldLocal::Let,
                    field_ident: &stmt.field,
                    init: match &ctx.scope {
                        Scope::Component => quote! { #mount_expr; },
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
                    let test = stmt.param_deps.update_test_tokens();

                    patch_stmts.push(quote! {
                        if #test {
                            #field_expr.get_mut().pass_props(
                                #props_path {
                                    #(#prop_list)*
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
                    local: FieldLocal::LetMut,
                    field_ident: &stmt.field,
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
                let closure_sig = ClosureSig::from_stmt(stmt, ClosureKind::IterItem);
                let closure = gen_iter_item_closure(
                    span_type,
                    closure_sig,
                    variable,
                    inner_block,
                    comp_ctx,
                    ctx,
                );

                let field = stmt.field.as_ref().unwrap();
                let closure_ident = &closure.sig.ident;

                field_inits.push(FieldInit {
                    local: FieldLocal::LetMut,
                    field_ident: &stmt.field,
                    init: quote! { ::hypp::list::SimpleListSpan::new(); },
                    post_init: Some(quote! {
                        #field.patch(#expr.iter(), #closure_ident, __ctx)?;
                    }),
                });
                patch_stmts.push(quote! {
                    #field.patch(#expr.iter(), #closure_ident, __ctx)?;
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
            Some(field_ident) => match field_init.local {
                FieldLocal::Let => quote! {
                    let #field_ident = #init
                    #post_init
                },
                FieldLocal::LetMut => quote! {
                    let mut #field_ident = #init
                    #post_init
                },
            },
        }
    });

    let constructor_path = match constructor_kind {
        SpanConstructorKind::FixedSpan(opt_span_type) => match opt_span_type {
            Some(span_type) => {
                let path_segment = span_type.to_tokens(ctx.scope, StructFieldFormat::PathSegment);
                quote! {
                    #path_segment
                }
            }
            None => {
                quote! { RootSpan }
            }
        },
        SpanConstructorKind::DynamicSpan { span_type, variant } => {
            let span_ident = span_type.to_tokens(ctx.scope, StructFieldFormat::PathSegment);

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
    let dynamic_span_path_segment = span_type.to_tokens(ctx.scope, StructFieldFormat::PathSegment);

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

    let dynamic_span_full_type = span_type.to_tokens(ctx.scope, StructFieldFormat::TypeInStruct);

    Closure {
        comp_ctx,
        sig: closure_sig,
        args: quote! {
            mut #field: &mut #dynamic_span_full_type,
        },
        body,
    }
}

fn gen_iter_item_closure<'c>(
    span_type: &ir::StructFieldType,
    closure_sig: ClosureSig,
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

    let fixed_span_path_segment = span_type.to_tokens(ctx.scope, StructFieldFormat::PathSegment);

    let fields = inner_block
        .struct_fields
        .iter()
        .map(ir::StructField::mut_pattern_tokens);

    let body = quote! {
        #(#closures)*

        match __span {
            ::hypp::InputOrOutput::Output(__span) => {
                #mount_locals
                *__span = Some(#mount_expr);
            }
            ::hypp::InputOrOutput::Input(#fixed_span_path_segment { #(#fields)* .. }) => {
                #patch
            }
        }

        Ok(())
    };

    let fixed_span_full_type = span_type.to_tokens(ctx.scope, StructFieldFormat::InnerType);

    Closure {
        comp_ctx,
        sig: closure_sig,
        args: quote! {
            __span: ::hypp::InputOrOutput<#fixed_span_full_type>,
            #iter_variable: &String,
            __invalidated: bool,
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
            ClosureKind::Match | ClosureKind::IterItem => Some(quote! {
                __ctx: &mut #patch_ctx_ty_inner
            }),
            ClosureKind::String => None,
        };

        let return_type = match self.sig.kind {
            ClosureKind::Match | ClosureKind::IterItem => Some(quote! { -> ::hypp::Void }),
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
