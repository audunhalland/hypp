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
    closures: TokenStream,

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
        #closures
    }
}

fn gen_match_closure(
    statement: &ir::Statement,
    dynamic_span_type: &ir::StructFieldType,
    expr: &syn::Expr,
    arms: &[ir::Arm],
    root_idents: &RootIdents,
    ctx: CodegenCtx,
) -> Closure {
    let dynamic_span_ident =
        dynamic_span_type.to_tokens(root_idents, ctx.scope, WithGenerics(false));
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
            quote! {
                #pattern => {

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
        ident: quote::format_ident!("lol"),
        arg_ty: quote! { () },
        body,
    }
}

fn patch_closure_ident(index: usize) -> syn::Ident {
    quote::format_ident!("__patch_{}", index)
}

fn compile_body(block: &ir::Block, root_idents: &RootIdents, ctx: CodegenCtx) -> Body {
    let mut closures: Vec<Closure> = vec![];
    let mut mount_stmts: Vec<TokenStream> = vec![];
    let mut patch_stmts: Vec<TokenStream> = vec![];

    let next_closure_ident = || -> syn::Ident {
        let index = closures.len();
        quote::format_ident!("mount_{}", index)
    };

    for stmt in &block.statements {
        match &stmt.expression {
            ir::Expression::ConstDom(_) => {}
            ir::Expression::AttributeCallback(_) => {}
            ir::Expression::Text(expr) => {
                let field_expr = FieldExpr(stmt.field.as_ref().unwrap(), ctx);
                let test = stmt.param_deps.update_test_tokens();

                mount_stmts.push(quote! {
                    __cursor.text(#expr.as_ref())?
                });

                patch_stmts.push(quote! {
                    if #test {
                        H::set_text(#field_expr, #expr.as_ref());
                    }
                    __cursor.move_to_following_sibling_of(#field_expr.as_node());
                });
            }
            ir::Expression::Component { path, props } => match &stmt.param_deps {
                // Const components never need to update!
                // (all their parameters are constants)
                ir::ParamDeps::Const => {}
                _ => {
                    let prop_list = props.iter().map(|attr| {
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
                    });
                    let props_path = path.props_path();

                    // let field_expr = FieldExpr(stmt.field.as_ref().unwrap(), ctx);
                    let test = stmt.param_deps.update_test_tokens();
                    // let ident = path.ident();

                    /*
                    Some(Closure {
                        arg_ty: quote! { #ident },
                        body: quote! {
                            if #test {
                                __arg.borrow_mut().pass_props(
                                    #props_path {
                                        #(#prop_list)*
                                    },
                                    __cursor
                                );
                            } else {
                                // Nothing has changed, but the cursor must pass
                                // over the component. This should be very cheap.
                                __arg.borrow_mut().pass_over(__cursor);
                            }
                        },
                    })
                    */
                }
            },
            ir::Expression::Match {
                dynamic_span_type,
                expr,
                arms,
            } => match &stmt.param_deps {
                ir::ParamDeps::Const => {}
                _ => {
                    let _ = Some(gen_match_closure(
                        stmt,
                        dynamic_span_type,
                        expr,
                        arms,
                        root_idents,
                        ctx,
                    ));
                }
            },
        }
    }

    /*
    let sub_patch_closures = block
    .statements
    .iter()
    .enumerate()
    .filter_map(|(index, stmt)| {
        let result: Option<Closure> =

        if let Some(Closure { arg_ty, body }) = result {
            let closure_ident = patch_closure_ident(index);
            Some(quote! {
                let mut #closure_ident = |__arg: #arg_ty, __cursor: &mut dyn ::hypp::Cursor<H>| {
                    #body
                };
            })
        } else {
            None
        }
    });
    */

    Body {
        closures: quote! {
            // #(#closures)*
        },
        mount: quote! {},
        patch: quote! {},
    }
}
