//!
//! Lowering / "flattening" of template_ast::Node, into a simple
//! control-flow "tree" (not a graph, because there are no joins).
//!
//! The tree consists of `ir::Block`, which contains code the _must_ run
//! together with other code. The only reason to split a block would the
//! presence of conditionals.
//!

use crate::flow;
use crate::ir;
use crate::namespace::*;
use crate::param;
use crate::template_ast;

// Minimum size of programs to directly skip when
// generating patch code.
// For longer programs, it may be worth it to store a reference
// to a DOM node within the component, in order to directly skip to it.
const SKIP_PROGRAM_THRESHOLD: usize = 0;

pub fn lower_root_node(
    root: template_ast::Node,
    direction: TraversalDirection,
    params: &[param::Param],
) -> Result<(ir::ComponentKind, ir::Block), syn::Error> {
    let mut root_builder = BlockBuilder::default();

    let mut ctx = Context {
        dom_program_count: 0,
        field_count: 0,
        dynamic_span_count: 0,
        used_method_count: 0,
        current_dom_depth: 0,
        direction,
        errors: vec![],
    };

    let scope = flow::FlowScope::from_params(params);

    root_builder.lower_ast(root, &scope, &mut ctx)?;

    if !ctx.errors.is_empty() {
        let mut errors = ctx.errors.into_iter();
        let mut error = errors.next().unwrap();
        while let Some(next_error) = errors.next() {
            error.combine(next_error);
        }
        return Err(error);
    }

    let mut root_block = root_builder.to_block(&mut ctx);

    let mut component_kind = ir::ComponentKind::Basic;

    if ctx.used_method_count > 0 {
        component_kind = ir::ComponentKind::SelfUpdatable;
        root_block.handle_kind = ir::HandleKind::Shared;
    }

    Ok((component_kind, root_block))
}

/// Context used for the whole template
pub struct Context {
    dom_program_count: u16,
    field_count: u16,
    dynamic_span_count: u16,
    used_method_count: u16,

    current_dom_depth: u16,

    direction: TraversalDirection,

    errors: Vec<syn::Error>,
}

impl Context {
    fn next_dom_program_id(&mut self) -> u16 {
        let index = self.dom_program_count;
        self.dom_program_count += 1;
        index
    }

    fn next_field_id(&mut self) -> ir::FieldIdent {
        let index = self.field_count;
        self.field_count += 1;
        ir::FieldIdent(index)
    }

    fn next_span_index(&mut self) -> u16 {
        let index = self.dynamic_span_count;
        self.dynamic_span_count += 1;
        index
    }
}

#[derive(Default)]
struct BlockBuilder {
    pub struct_fields: Vec<ir::StructField>,
    pub statements: Vec<ir::Statement>,

    // Param deps for the whole block
    param_deps: ir::ParamDeps,

    current_dom_program_depth: u16,
    current_dom_opcodes: Vec<ir::DomOpCode>,
}

impl BlockBuilder {
    fn to_block(mut self, ctx: &mut Context) -> ir::Block {
        self.flush_dom_program(ctx);

        ir::Block {
            handle_kind: ir::HandleKind::Unique,
            struct_fields: self.struct_fields,
            statements: self.statements,
            param_deps: self.param_deps,
        }
    }

    fn push_dom_opcode(&mut self, opcode: ir::DomOpCode, ctx: &Context) {
        if self.current_dom_opcodes.is_empty() {
            self.current_dom_program_depth = ctx.current_dom_depth;
        }

        self.current_dom_opcodes.push(opcode);
    }

    fn flush_dom_program(&mut self, ctx: &mut Context) {
        if self.current_dom_opcodes.is_empty() {
            return;
        }

        let opcodes = std::mem::replace(&mut self.current_dom_opcodes, vec![]);

        let program = ir::ConstDomProgram {
            id: ctx.next_dom_program_id(),
            opcodes,
        };

        // Determine the resulting type of running the program (the last opcode)
        let program_ty = match program.last_node_type() {
            Some(ir::NodeType::Element) => Some(ir::StructFieldType::DomElement),
            Some(ir::NodeType::Text) => Some(ir::StructFieldType::DomText),
            None => None,
        };

        // TODO: this is a tradeoff.
        // Storing an extra reference to a dom node within the component
        // is not worth it when the program it skips is very short.
        // so we should have some kind of threshold for when to store the element.
        let opt_field = if program.opcodes.len() >= SKIP_PROGRAM_THRESHOLD {
            let field = ctx.next_field_id();
            if let Some(program_ty) = program_ty {
                self.struct_fields.push(ir::StructField {
                    ident: field.clone(),
                    ty: program_ty,
                });
                Some(field)
            } else {
                None
            }
        } else {
            None
        };

        self.statements.push(ir::Statement {
            field: opt_field,
            dom_depth: ir::DomDepth(self.current_dom_program_depth),
            param_deps: ir::ParamDeps::Const,
            expression: ir::Expression::ConstDom(program),
        });
    }

    fn push_statement(&mut self, statement: ir::Statement, ctx: &mut Context) {
        self.flush_dom_program(ctx);

        self.statements.push(statement);
    }

    fn lower_ast<'p>(
        &mut self,
        node: template_ast::Node,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) -> Result<(), syn::Error> {
        match node {
            template_ast::Node::Element(element) => self.lower_element(element, scope, ctx)?,
            template_ast::Node::Fragment(nodes) => match ctx.direction {
                TraversalDirection::FirstToLast => {
                    for node in nodes {
                        if let Err(error) = self.lower_ast(node, scope, ctx) {
                            ctx.errors.push(error);
                        }
                    }
                }
                TraversalDirection::LastToFirst => {
                    for node in nodes.into_iter().rev() {
                        if let Err(error) = self.lower_ast(node, scope, ctx) {
                            ctx.errors.push(error);
                        }
                    }
                }
            },
            template_ast::Node::Text(text) => self.lower_text_const(text, ctx)?,
            template_ast::Node::TextExpr(expr) => self.lower_text_expr(expr, scope, ctx)?,
            template_ast::Node::Component(component) => {
                self.lower_component(component, scope, ctx)?
            }
            template_ast::Node::Match(the_match) => self.lower_match(the_match, scope, ctx)?,
            template_ast::Node::For(the_for) => self.lower_for(the_for, scope, ctx)?,
        }

        Ok(())
    }

    fn lower_element<'p>(
        &mut self,
        element: template_ast::Element,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) -> Result<(), syn::Error> {
        self.push_dom_opcode(
            (
                ir::DomOpCodeKind::Enter(element.ns_name.opcode_value()),
                element.ns_name.ident.span(),
            ),
            ctx,
        );

        for attr in element.attrs {
            if let Err(error) = self.lower_element_attr(attr, ctx) {
                ctx.errors.push(error);
            }
        }

        ctx.current_dom_depth += 1;

        match ctx.direction {
            TraversalDirection::FirstToLast => {
                for child in element.children {
                    if let Err(error) = self.lower_ast(child, scope, ctx) {
                        ctx.errors.push(error);
                    }
                }
            }
            TraversalDirection::LastToFirst => {
                for child in element.children.into_iter().rev() {
                    if let Err(error) = self.lower_ast(child, scope, ctx) {
                        ctx.errors.push(error);
                    }
                }
            }
        }

        self.push_dom_opcode((ir::DomOpCodeKind::Exit, element.ns_name.ident.span()), ctx);
        ctx.current_dom_depth -= 1;

        Ok(())
    }

    fn lower_element_attr<'p>(
        &mut self,
        attr: template_ast::Attr<NSName<NSAttrName>>,
        ctx: &mut Context,
    ) -> Result<(), syn::Error> {
        let attr_opcode_value = attr.name.opcode_value();
        let span = attr.name.ident.span();

        match (attr.name.name.kind(), attr.value) {
            (NSAttrKind::Callback, template_ast::AttrValue::SelfMethod(method_ident)) => {
                // First push the attribute name into const program:
                self.push_dom_opcode((ir::DomOpCodeKind::Attr(attr_opcode_value), span), ctx);

                ctx.used_method_count += 1;

                // Slot must be stored, because it must release ref counts on unmount.
                let slot_field = ctx.next_field_id();

                self.struct_fields.push(ir::StructField {
                    ident: slot_field.clone(),
                    ty: ir::StructFieldType::EventSlot,
                });

                // Break the DOM program to produce a callback here:
                self.push_statement(
                    ir::Statement {
                        field: Some(slot_field),
                        dom_depth: ir::DomDepth(ctx.current_dom_depth),
                        param_deps: ir::ParamDeps::Const,
                        expression: ir::Expression::AttributeCallback(ir::Callback::SelfMethod {
                            local_field: ctx.next_field_id(),
                            method_ident,
                        }),
                    },
                    ctx,
                );
                Ok(())
            }
            (NSAttrKind::Callback, template_ast::AttrValue::Expr(expr)) => {
                // First push the attribute name into const program:
                self.push_dom_opcode((ir::DomOpCodeKind::Attr(attr_opcode_value), span), ctx);

                // Slot must be stored, because it must release ref counts on unmount.
                let slot_field = ctx.next_field_id();

                self.struct_fields.push(ir::StructField {
                    ident: slot_field.clone(),
                    ty: ir::StructFieldType::EventSlot,
                });

                self.push_statement(
                    ir::Statement {
                        field: Some(slot_field),
                        dom_depth: ir::DomDepth(ctx.current_dom_depth),
                        param_deps: ir::ParamDeps::Const,
                        expression: ir::Expression::AttributeCallback(ir::Callback::Expr(expr)),
                    },
                    ctx,
                );
                Ok(())
            }
            (_, template_ast::AttrValue::ImplicitTrue) => {
                let attr_value = syn::LitStr::new("true", proc_macro2::Span::mixed_site());
                self.push_dom_opcode((ir::DomOpCodeKind::Attr(attr_opcode_value), span), ctx);
                self.push_dom_opcode((ir::DomOpCodeKind::AttrText(attr_value), span), ctx);
                Ok(())
            }
            (_, template_ast::AttrValue::Literal(lit)) => {
                let value_lit = match lit {
                    syn::Lit::Str(lit_str) => lit_str,
                    // BUG: Debug formatting
                    lit => syn::LitStr::new(&format!("{:?}", lit), proc_macro2::Span::mixed_site()),
                };
                self.push_dom_opcode((ir::DomOpCodeKind::Attr(attr_opcode_value), span), ctx);
                self.push_dom_opcode((ir::DomOpCodeKind::AttrText(value_lit), span), ctx);
                Ok(())
            }
            (_, template_ast::AttrValue::Expr(_)) => {
                panic!("lol")
            }
            (_, template_ast::AttrValue::SelfMethod(_)) => Err(syn::Error::new(
                span,
                "Callback not applicable to this attribute",
            )),
        }
    }

    fn lower_text_const(
        &mut self,
        text: template_ast::Text,
        ctx: &mut Context,
    ) -> Result<(), syn::Error> {
        let span = text.0.span();
        self.push_dom_opcode((ir::DomOpCodeKind::Text(text.0), span), ctx);
        Ok(())
    }

    fn lower_text_expr<'p>(
        &mut self,
        expr: syn::Expr,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) -> Result<(), syn::Error> {
        let node_field = ctx.next_field_id();

        self.struct_fields.push(ir::StructField {
            ident: node_field.clone(),
            ty: ir::StructFieldType::DomText,
        });

        let param_deps = scope.lookup_params_deps_for_expr(&expr);
        self.param_deps.extend(&param_deps);

        self.push_statement(
            ir::Statement {
                field: Some(node_field),
                dom_depth: ir::DomDepth(ctx.current_dom_depth),
                param_deps,
                expression: ir::Expression::Text(expr),
            },
            ctx,
        );

        Ok(())
    }

    fn lower_component<'p>(
        &mut self,
        component: template_ast::Component,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) -> Result<(), syn::Error> {
        let component_path = ir::ComponentPath::new(component.type_path);

        let prop_args = component
            .attrs
            .into_iter()
            .map(|attr| self.lower_component_attr(attr, scope, ctx))
            .collect::<syn::Result<Vec<_>>>()?;

        let field = ctx.next_field_id();
        self.struct_fields.push(ir::StructField {
            ident: field.clone(),
            ty: ir::StructFieldType::Component(component_path.clone()),
        });

        let mut param_deps = ir::ParamDeps::Const;
        for prop_arg in &prop_args {
            param_deps.extend(&prop_arg.param_deps)
        }

        self.param_deps.extend(&param_deps);

        self.push_statement(
            ir::Statement {
                field: Some(field),
                dom_depth: ir::DomDepth(ctx.current_dom_depth),
                param_deps,
                expression: ir::Expression::Component {
                    path: component_path,
                    props: prop_args,
                },
            },
            ctx,
        );

        Ok(())
    }

    fn lower_component_attr<'p>(
        &mut self,
        attr: template_ast::Attr<syn::Ident>,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) -> Result<ir::ComponentPropArg, syn::Error> {
        match &attr.value {
            template_ast::AttrValue::ImplicitTrue | template_ast::AttrValue::Literal(_) => {
                Ok(ir::ComponentPropArg {
                    ident: attr.name,
                    local_field: None,
                    value: attr.value,
                    param_deps: ir::ParamDeps::Const,
                })
            }
            template_ast::AttrValue::Expr(expr) => Ok(ir::ComponentPropArg {
                ident: attr.name,
                local_field: None,
                param_deps: scope.lookup_params_deps_for_expr(expr),
                value: attr.value,
            }),
            template_ast::AttrValue::SelfMethod(method_ident) => {
                ctx.used_method_count += 1;

                // This must be stored because the component might be patched
                let field = ctx.next_field_id();

                self.struct_fields.push(ir::StructField {
                    ident: field.clone(),
                    ty: ir::StructFieldType::SelfClosure(method_ident.clone()),
                });

                Ok(ir::ComponentPropArg {
                    ident: attr.name,
                    local_field: Some(field),
                    value: attr.value,
                    param_deps: ir::ParamDeps::Const,
                })
            }
        }
    }

    fn lower_match<'p>(
        &mut self,
        the_match: template_ast::Match,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) -> Result<(), syn::Error> {
        let expr = the_match.expr;
        let field = ctx.next_field_id();
        let span_type = ir::StructFieldType::Span(ctx.next_span_index(), ir::SpanKind::Enum);

        let arms: Vec<_> = the_match
            .arms
            .into_iter()
            .enumerate()
            .map(|(index, arm)| {
                // Help dataflow analysis inside the new variable scope
                let mut arm_scope = flow::FlowScope::with_parent(scope);
                // expr flows into arm.pat
                arm_scope.bind_expr(&arm.pat, &expr);

                // Compile the arm
                let mut arm_builder = BlockBuilder::default();
                arm_builder.lower_ast(arm.node, &arm_scope, ctx)?;

                Ok(ir::Arm {
                    variant: quote::format_ident!("V{}", index),
                    pattern: arm.pat,
                    block: arm_builder.to_block(ctx),
                })
            })
            .collect::<syn::Result<Vec<_>>>()?;

        // Param deps for the whole match expression
        let mut param_deps = scope.lookup_params_deps_for_expr(&expr);
        for arm in arms.iter() {
            param_deps.extend(&arm.block.param_deps);
        }

        self.param_deps.extend(&param_deps);

        self.push_statement(
            ir::Statement {
                field: Some(field.clone()),
                dom_depth: ir::DomDepth(ctx.current_dom_depth),
                param_deps,
                expression: ir::Expression::Match {
                    span_type: span_type.clone(),
                    expr,
                    arms,
                },
            },
            ctx,
        );

        self.struct_fields.push(ir::StructField {
            ident: field,
            ty: span_type,
        });

        Ok(())
    }

    fn lower_for<'p>(
        &mut self,
        the_for: template_ast::For,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) -> Result<(), syn::Error> {
        // let expr = the_for.expr;
        let field = ctx.next_field_id();
        let span_type =
            ir::StructFieldType::Span(ctx.next_span_index(), ir::SpanKind::RepeatedStruct);

        let expr = the_for.expression;
        let variable = the_for.binding;

        // Help dataflow analysis inside the new variable scope
        let mut inner_scope = flow::FlowScope::with_parent(scope);

        {
            let pattern = syn::Pat::Ident(syn::PatIdent {
                attrs: vec![],
                by_ref: None,
                mutability: None,
                ident: variable.clone(),
                subpat: None,
            });

            // expr flows into the pattern
            inner_scope.bind_expr(&pattern, &expr);
        }

        // Compile inner code
        let inner_block = {
            let mut inner_builder = BlockBuilder::default();
            inner_builder.lower_ast(*the_for.repeating_node, &inner_scope, ctx)?;
            inner_builder.to_block(ctx)
        };

        // Param deps for the whole match expression
        let mut param_deps = scope.lookup_params_deps_for_expr(&expr);
        param_deps.extend(&inner_block.param_deps);
        self.param_deps.extend(&param_deps);

        self.push_statement(
            ir::Statement {
                field: Some(field),
                dom_depth: ir::DomDepth(ctx.current_dom_depth),
                param_deps,
                expression: ir::Expression::Iter {
                    span_type: span_type.clone(),
                    expr,
                    variable,
                    inner_block: Box::new(inner_block),
                },
            },
            ctx,
        );

        self.struct_fields.push(ir::StructField {
            ident: field,
            ty: span_type,
        });

        Ok(())
    }
}
