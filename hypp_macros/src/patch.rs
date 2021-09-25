use proc_macro2::TokenStream;
use quote::quote;

use crate::callback;
use crate::codegen::*;
use crate::ir;
use crate::param;
use crate::template_ast;

// body of a block before it's 'serialized'.
struct Body {
    /// helper functions used by 'mount' and 'patch',
    /// to avoid code duplication.
    closures: Vec<Closure>,

    // code used when the block must be built from scratch
    mount: TokenStream,

    // code used when the body span already exists, and should
    // be selectively updated
    patch: TokenStream,
}

struct Closure {
    ident: syn::Ident,
    arg_ty: TokenStream,
    body: TokenStream,
}

pub fn gen_patch(block: &ir::Block, root_idents: &RootIdents, ctx: CodegenCtx) -> TokenStream {
    let Body { closures, .. } = compile_body(block, root_idents, ctx);

    quote! {
        #(#closures)*
    }
}

fn compile_body(block: &ir::Block, root_idents: &RootIdents, ctx: CodegenCtx) -> Body {
    let mut closures: Vec<Closure> = vec![];
    let mut mount_stmts: Vec<TokenStream> = vec![];
    let mut patch_stmts: Vec<TokenStream> = vec![];

    fn next_closure_ident(closures: &[Closure]) -> syn::Ident {
        let index = closures.len();
        quote::format_ident!("__patch_{}", index)
    }

    for stmt in &block.statements {
        match &stmt.expression {
            ir::Expression::ConstDom(program) => {
                let program_ident = program.get_ident(root_idents);
                let last_node_opcode = program.last_node_opcode();

                mount_stmts.push(match last_node_opcode {
                    Some(ir::DomOpCode::EnterElement(_) | ir::DomOpCode::ExitElement) => {
                        quote! {
                            __cursor.const_exec_element(&#program_ident)?
                        }
                    }
                    Some(ir::DomOpCode::Text(_)) => quote! {
                        __cursor.const_exec_text(&#program_ident)?
                    },
                    _ => panic!(),
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
                            __cursor.skip_const_program(&#program_ident);
                        }
                    }
                });
            }
            ir::Expression::AttributeCallback(callback::Callback { ident }) => {
                let field = stmt.field.as_ref().unwrap();
                let component_ident = &root_idents.component_ident;

                mount_stmts.push(quote! {
                    let #field = __ctx.cur.attribute_value_callback()?;

                    __ctx.bind.bind(
                        #field.clone(),
                        ::hypp::ShimMethod::<#component_ident<H>>(&|shim| {
                            shim.#ident();
                        }),
                    );
                });
            }
            ir::Expression::Text(expr) => {
                let field_expr = FieldExpr(stmt.field.as_ref().unwrap(), ctx);
                let test = stmt.param_deps.update_test_tokens();

                mount_stmts.push(quote! {
                    __ctx.cur.text(#expr.as_ref())?
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
                        __cursor
                    )?
                };

                mount_stmts.push(match (&stmt.field, ctx.scope) {
                    (Some(field), Scope::Component) => quote! {
                        let #field = #mount_expr;
                    },
                    (Some(field), Scope::DynamicSpan) => quote! {
                        let #field = #mount_expr.into_boxed();
                    },
                    (None, _) => quote! {
                        #mount_expr;
                    },
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
                            #field_expr.borrow_mut().pass_over(__cursor);
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
                        dynamic_span_type.to_tokens(root_idents, ctx.scope, WithGenerics(false));

                    let closure_ident = next_closure_ident(&closures);
                    let closure = gen_match_closure(
                        stmt,
                        &dynamic_span_ident,
                        closure_ident.clone(),
                        expr,
                        arms,
                        root_idents,
                        ctx,
                    );

                    let field = stmt.field.as_ref().unwrap();

                    closures.push(closure);
                    mount_stmts.push(quote! {
                        let mut #field = #dynamic_span_ident::Erased;
                        #closure_ident(&mut #field, __ctx);
                    });

                    patch_stmts.push(quote! {
                        #closure_ident(&mut #field, __ctx);
                    });
                }
            },
        }
    }

    Body {
        closures,
        mount: quote! {},
        patch: quote! {},
    }
}

fn gen_match_closure(
    statement: &ir::Statement,
    dynamic_span_ident: &TokenStream,
    closure_ident: syn::Ident,
    expr: &syn::Expr,
    arms: &[ir::Arm],
    root_idents: &RootIdents,
    ctx: CodegenCtx,
) -> Closure {
    let field = statement.field.as_ref().unwrap();
    let field_expr = FieldExpr(field, ctx);
    let field_assign = FieldAssign(field, ctx);
    let mut_field_pat = MutFieldPat(field, ctx);

    let pattern_arms = arms.iter().map(
        |ir::Arm {
             variant,
             pattern,
             block,
             ..
         }| {
            let Body {
                closures,
                mount,
                patch,
            } = compile_body(
                &block,
                root_idents,
                CodegenCtx {
                    scope: Scope::DynamicSpan,
                    ..ctx
                },
            );

            let fields = block
                .struct_fields
                .iter()
                .map(ir::StructField::mut_pattern_tokens);

            quote! {
                #pattern => {
                    #(#closures)*

                    match #mut_field_pat {
                        #dynamic_span_ident::#variant { #(#fields)* .. } => {
                            // The matching arm (this variant corresponds to 'expr')
                            #patch
                        }
                        _ => {
                            // The non-matching arm, erase and then re-mount
                            #field_expr.erase(__ctx.cur);
                            #mount
                            #field_assign = __mounted;
                        }
                    }
                }
            }
        },
    );

    let body = quote! {
        match #expr {
            #(#pattern_arms)*
        }
    };

    Closure {
        ident: closure_ident,
        arg_ty: quote! { () },
        body,
    }
}

impl quote::ToTokens for Closure {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = &self.ident;

        let output = quote! {
            let #ident = || -> ::hypp::Void {
                Ok(())
            }
        };
        tokens.extend(output);
    }
}
