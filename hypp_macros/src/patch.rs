use proc_macro2::TokenStream;
use quote::quote;

use crate::callback;
use crate::codegen::*;
use crate::ir;
use crate::template_ast;

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
    ident: syn::Ident,
    args: TokenStream,
    body: TokenStream,
}

pub fn gen_patch2_fn(
    block: &ir::Block,
    comp_ctx: &CompCtx,
    env_locals: TokenStream,
    fn_stmts: Vec<syn::Stmt>,
    ctx: CodegenCtx,
) -> TokenStream {
    let env_ident = &comp_ctx.env_ident;
    let root_span_ident = &comp_ctx.root_span_ident;
    let patch_ctx_ty = &comp_ctx.patch_ctx_ty;

    let Body {
        closures,
        mount_locals,
        mount_expr,
        patch,
    } = compile_body(block, comp_ctx, ctx, ConstructorKind::RootSpan);

    let fields = block
        .struct_fields
        .iter()
        .map(ir::StructField::mut_pattern_tokens);

    quote! {
        #[allow(unused_variables)]
        fn patch2(
            __root: ::hypp::InputOrOutput<#root_span_ident<H>>,
            __env: &#env_ident,
            __updates: &[bool],
            __ctx: &mut #patch_ctx_ty,
        ) -> Result<(), ::hypp::Error> {
            #env_locals

            #(#fn_stmts)*

            #(#closures)*

            match __root {
                ::hypp::InputOrOutput::Output(__root) => {
                    #mount_locals
                    *__root = Some(#mount_expr);
                }
                // Destructure here:
                ::hypp::InputOrOutput::Input(#root_span_ident { #(#fields)* .. }) => {
                    #patch
                }
            }
            Ok(())
        }
    }
}

fn compile_body<'c>(
    block: &ir::Block,
    comp_ctx: &'c CompCtx,
    ctx: CodegenCtx,
    constructor_kind: ConstructorKind<'_>,
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

    fn next_closure_ident(closures: &[Closure]) -> syn::Ident {
        let index = closures.len();
        quote::format_ident!("__patch_{}", index)
    }

    for stmt in &block.statements {
        match &stmt.expression {
            ir::Expression::ConstDom(program) => {
                let program_ident = program.get_ident(comp_ctx);
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

                        let field_expr = FieldExpr(field, ctx);
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
                let field_expr = FieldExpr(stmt.field.as_ref().unwrap(), ctx);
                let test = stmt.param_deps.update_test_tokens();

                field_inits.push(FieldInit {
                    local: FieldLocal::Let,
                    field_ident: &stmt.field,
                    init: quote! {
                        __ctx.cur.text(#expr.as_ref())?;
                    },
                    post_init: None,
                });

                patch_stmts.push(quote! {
                    if #test {
                        H::set_text(#field_expr, #expr.as_ref());
                    }
                    __ctx.cur.move_to_following_sibling_of(#field_expr.as_node());
                });
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
                    let field_expr = FieldExpr(stmt.field.as_ref().unwrap(), ctx);
                    let test = stmt.param_deps.update_test_tokens();

                    patch_stmts.push(quote! {
                        if #test {
                            #field_expr.borrow_mut().pass_props(
                                #props_path {
                                    #(#prop_list)*
                                },
                                __ctx.cur
                            );
                        } else {
                            // Nothing has changed, but the cursor must pass
                            // over the component. This should be very cheap.
                            #field_expr.borrow_mut().pass_over(__ctx.cur);
                        }
                    });
                }
            }
            ir::Expression::Match {
                dynamic_span_type,
                expr,
                arms,
            } => match &stmt.param_deps {
                ir::ParamDeps::Const => {}
                _ => {
                    let dynamic_span_ident =
                        dynamic_span_type.to_tokens(comp_ctx, ctx.scope, WithGenerics(false));

                    let closure_ident = next_closure_ident(&closures);
                    let closure = gen_match_closure(
                        stmt,
                        dynamic_span_type,
                        &dynamic_span_ident,
                        closure_ident.clone(),
                        expr,
                        arms,
                        comp_ctx,
                        ctx,
                    );

                    let field = stmt.field.as_ref().unwrap();

                    closures.push(closure);
                    field_inits.push(FieldInit {
                        local: FieldLocal::LetMut,
                        field_ident: &stmt.field,
                        init: quote! {
                            #dynamic_span_ident::Erased;
                        },
                        post_init: Some(quote! {
                            #closure_ident(&mut #field, __ctx)?;
                        }),
                    });

                    patch_stmts.push(quote! {
                        #closure_ident(#field, __ctx)?;
                    });
                }
            },
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
        ConstructorKind::Component => panic!(),
        ConstructorKind::RootSpan => {
            let root_span_ident = &comp_ctx.root_span_ident;
            quote! {
                #root_span_ident
            }
        }
        ConstructorKind::DynamicSpan {
            dynamic_span_type,
            variant,
        } => {
            let dynamic_span_ident =
                dynamic_span_type.to_tokens(comp_ctx, ctx.scope, WithGenerics(false));

            quote! {
                #dynamic_span_ident::#variant
            }
        }
    };

    let struct_params = block
        .struct_fields
        .iter()
        .map(ir::StructField::struct_param_tokens);

    let phantom = match constructor_kind {
        ConstructorKind::Component => panic!(),
        ConstructorKind::RootSpan => Some(quote! { __phantom: ::std::marker::PhantomData, }),
        ConstructorKind::DynamicSpan { .. } => None,
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
    dynamic_span_type: &ir::StructFieldType,
    dynamic_span_ident: &TokenStream,
    closure_ident: syn::Ident,
    expr: &syn::Expr,
    arms: &[ir::Arm],
    comp_ctx: &'c CompCtx,
    ctx: CodegenCtx,
) -> Closure<'c> {
    let field = statement.field.as_ref().unwrap();
    let field_expr = FieldExpr(field, ctx);

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
                ConstructorKind::DynamicSpan {
                    dynamic_span_type,
                    variant,
                },
            );

            let fields = block
                .struct_fields
                .iter()
                .map(ir::StructField::mut_pattern_tokens);

            quote! {
                #pattern => {
                    #(#closures)*

                    match &mut #field {
                        #dynamic_span_ident::#variant { #(#fields)* .. } => {
                            // The matching arm (this variant corresponds to 'expr')
                            #patch
                        }
                        _ => {
                            // The non-matching arm, erase and then re-mount
                            #field_expr.erase(__ctx.cur);
                            #mount_locals
                            *#field = #mount_expr;
                        }
                    }
                },
            }
        },
    );

    let body = quote! {
        match #expr {
            #(#pattern_arms)*
        }
    };

    Closure {
        comp_ctx,
        ident: closure_ident,
        args: quote! {
            mut #field: &mut #dynamic_span_ident<H>,
        },
        body,
    }
}

impl<'c> quote::ToTokens for Closure<'c> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = &self.ident;
        let args = &self.args;
        let patch_ctx_ty = &self.comp_ctx.patch_ctx_ty;
        let body = &self.body;

        let output = quote! {
            let #ident = |#args __ctx: &mut #patch_ctx_ty| -> ::hypp::Void {
                #body
                Ok(())
            };
        };
        tokens.extend(output);
    }
}
