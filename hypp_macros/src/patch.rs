use proc_macro2::TokenStream;
use quote::quote;

use crate::callback;
use crate::codegen::*;
use crate::ir;
use crate::param;
use crate::template_ast;

struct Closure {
    arg_ty: TokenStream,
    body: TokenStream,
}

pub fn gen_patch(block: &ir::Block, root_idents: &RootIdents, ctx: CodegenCtx) -> TokenStream {
    let sub_patch_closures = block
        .statements
        .iter()
        .enumerate()
        .filter_map(|(index, stmt)| {
            let result: Option<Closure> = match &stmt.expression {
                ir::Expression::ConstDom(_) => None,
                ir::Expression::AttributeCallback(_) => None,
                ir::Expression::Text(expr) => {
                    let test = stmt.param_deps.update_test_tokens();

                    Some(Closure {
                        arg_ty: quote! { &H::Text },
                        body: quote! {
                            if #test {
                                H::set_text(__arg, #expr.as_ref());
                            }
                            __cursor.move_to_following_sibling_of(__arg.as_node());
                        },
                    })
                }
                ir::Expression::Component { path, props } => match &stmt.param_deps {
                    // Const components never need to update!
                    // (all their parameters are constants)
                    ir::ParamDeps::Const => None,
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
                        let ident = path.ident();

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
                    }
                },
                ir::Expression::Match {
                    dynamic_span_type,
                    expr,
                    arms,
                } => match &stmt.param_deps {
                    ir::ParamDeps::Const => None,
                    _ => Some(gen_match_closure(
                        stmt,
                        dynamic_span_type,
                        expr,
                        arms,
                        root_idents,
                        ctx,
                    )),
                },
            };

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

    quote! {
        #(#sub_patch_closures)*
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
    Closure {
        arg_ty: quote! { () },
        body: quote! {},
    }
}

fn patch_closure_ident(index: usize) -> syn::Ident {
    quote::format_ident!("__patch_{}", index)
}
