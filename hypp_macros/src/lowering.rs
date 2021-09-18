use crate::flow;
use crate::ir;
use crate::param;
use crate::template_ast;

// Minimum size of programs to directly skip when
// generating patch code.
// For longer programs, it may be worth it to store a reference
// to a DOM node within the component, in order to directly skip to it.
const SKIP_PROGRAM_THRESHOLD: usize = 0;

pub fn lower_root_node(root: template_ast::Node, params: &[param::Param]) -> ir::Block {
    let mut root_builder = BlockBuilder::default();

    let mut ctx = Context {
        dom_program_count: 0,
        field_count: 0,
        enum_count: 0,
        current_dom_depth: 0,
    };

    let scope = flow::FlowScope::from_params(params);

    root_builder.lower_ast(root, &scope, &mut ctx);

    for param in params {
        root_builder.struct_fields.push(ir::StructField {
            field: ir::FieldIdent::Param(param.ident.clone()),
            ty: ir::StructFieldType::Param(param.clone()),
        });
    }

    root_builder.to_block(&mut ctx)
}

/// Context used for the whole template
pub struct Context {
    dom_program_count: u16,
    field_count: u16,
    enum_count: u16,

    current_dom_depth: u16,
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
        ir::FieldIdent::Id(index)
    }

    fn next_enum_index(&mut self) -> u16 {
        let index = self.enum_count;
        self.enum_count += 1;
        index
    }
}

#[derive(Default)]
struct BlockBuilder {
    pub struct_fields: Vec<ir::StructField>,
    pub statements: Vec<ir::Statement>,

    current_dom_program_depth: u16,
    current_dom_opcodes: Vec<ir::DomOpCode>,
}

impl BlockBuilder {
    fn to_block(mut self, ctx: &mut Context) -> ir::Block {
        self.flush_dom_program(ctx);

        ir::Block {
            struct_fields: self.struct_fields,
            statements: self.statements,
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

        let program_id = ctx.next_dom_program_id();

        // Determine the resulting type of running the program (the last opcode)
        let program_ty = match opcodes.last().unwrap() {
            ir::DomOpCode::EnterElement(_) | ir::DomOpCode::ExitElement => {
                ir::StructFieldType::DomElement
            }
            ir::DomOpCode::Text(_) => ir::StructFieldType::DomText,
        };

        // TODO: this is a tradeoff.
        // Storing an extra reference to a dom node within the component
        // is not worth it when the program it skips is very short.
        // so we should have some kind of threshold for when to store the element.
        let opt_field = if opcodes.len() >= SKIP_PROGRAM_THRESHOLD {
            let field = ctx.next_field_id();
            self.struct_fields.push(ir::StructField {
                field: field.clone(),
                ty: program_ty,
            });
            Some(field)
        } else {
            None
        };

        self.statements.push(ir::Statement {
            field: opt_field,
            dom_depth: ir::DomDepth(self.current_dom_program_depth),
            param_deps: ir::ParamDeps::Const,
            expression: ir::Expression::ConstDom(ir::ConstDomProgram {
                id: program_id,
                opcodes,
            }),
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
    ) {
        match node {
            template_ast::Node::Element(element) => self.lower_element(element, scope, ctx),
            template_ast::Node::Fragment(nodes) => {
                for node in nodes {
                    self.lower_ast(node, scope, ctx);
                }
            }
            template_ast::Node::Text(text) => self.lower_text_const(text, ctx),
            template_ast::Node::TextExpr(expr) => self.lower_text_expr(expr, scope, ctx),
            template_ast::Node::Component(component) => self.lower_component(component, scope, ctx),
            template_ast::Node::Match(the_match) => self.lower_match(the_match, scope, ctx),
            template_ast::Node::For(_the_for) => {}
        }
    }

    fn lower_element<'p>(
        &mut self,
        element: template_ast::Element,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) {
        let tag_name = syn::LitStr::new(&element.tag_name.to_string(), element.tag_name.span());

        self.push_dom_opcode(ir::DomOpCode::EnterElement(tag_name.clone()), ctx);
        ctx.current_dom_depth += 1;

        for child in element.children {
            self.lower_ast(child, scope, ctx);
        }

        self.push_dom_opcode(ir::DomOpCode::ExitElement, ctx);
        ctx.current_dom_depth -= 1;
    }

    fn lower_text_const(&mut self, text: template_ast::Text, ctx: &mut Context) {
        self.push_dom_opcode(ir::DomOpCode::Text(text.0), ctx);
    }

    fn lower_text_expr<'p>(
        &mut self,
        expr: syn::Expr,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) {
        let node_field = ctx.next_field_id();
        let variable_field = ctx.next_field_id();

        self.struct_fields.push(ir::StructField {
            field: node_field.clone(),
            ty: ir::StructFieldType::DomText,
        });

        self.push_statement(
            ir::Statement {
                field: Some(node_field),
                dom_depth: ir::DomDepth(ctx.current_dom_depth),
                param_deps: scope.lookup_params_deps_for_expr(&expr),
                expression: ir::Expression::Text {
                    variable_field: variable_field.clone(),
                    expr,
                },
            },
            ctx,
        );
    }

    fn lower_component<'p>(
        &mut self,
        component: template_ast::Component,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) {
        let field = ctx.next_field_id();

        let component_path = ir::ComponentPath::new(component.type_path);

        self.struct_fields.push(ir::StructField {
            field: field.clone(),
            ty: ir::StructFieldType::Component(component_path.clone()),
        });

        let mut param_deps = ir::ParamDeps::Const;
        for attr in &component.attrs {
            param_deps = param_deps.union(scope.lookup_param_deps_for_ident(&attr.0));
        }

        self.push_statement(
            ir::Statement {
                field: Some(field),
                dom_depth: ir::DomDepth(ctx.current_dom_depth),
                param_deps,
                expression: ir::Expression::Component {
                    path: component_path,
                    props: component.attrs,
                },
            },
            ctx,
        );
    }

    fn lower_match<'p>(
        &mut self,
        the_match: template_ast::Match,
        scope: &flow::FlowScope<'p>,
        ctx: &mut Context,
    ) {
        let expr = the_match.expr;
        let field = ctx.next_field_id();
        let enum_type = ir::StructFieldType::Enum(ctx.next_enum_index());

        let arms = the_match
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
                arm_builder.lower_ast(arm.node, &arm_scope, ctx);

                ir::Arm {
                    enum_variant_ident: quote::format_ident!("V{}", index),
                    mount_fn_ident: quote::format_ident!("mount_v{}", index),
                    pattern: arm.pat,
                    block: arm_builder.to_block(ctx),
                }
            })
            .collect();

        // FIXME: Should we take into account the deps inside the arms HERE?
        // because we have to execute this Match update if any of the params
        // used inside the match get touched.
        let param_deps = scope.lookup_params_deps_for_expr(&expr);

        self.push_statement(
            ir::Statement {
                field: Some(field.clone()),
                dom_depth: ir::DomDepth(ctx.current_dom_depth),
                param_deps,
                expression: ir::Expression::Match {
                    enum_type: enum_type.clone(),
                    expr,
                    arms,
                },
            },
            ctx,
        );

        self.struct_fields.push(ir::StructField {
            field,
            ty: enum_type,
        });
    }
}
