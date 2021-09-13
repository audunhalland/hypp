use crate::ir;
use crate::template_ast;
use crate::variable;

// Minimum size of programs to directly skip when
// generating patch code.
// For longer programs, it may be worth it to store a reference
// to a DOM node within the component, in order to directly skip to it.
const SKIP_PROGRAM_THRESHOLD: usize = 0;

pub fn lower_root_node(root: template_ast::Node) -> ir::Block {
    let mut root_builder = BlockBuilder::default();

    let mut ctx = Context {
        dom_program_count: 0,
        field_count: 0,
        enum_count: 0,
        current_dom_depth: 0,
    };

    root_builder.lower_ast(root, &mut ctx);
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

    fn next_field_id(&mut self) -> ir::FieldId {
        let index = self.field_count;
        self.field_count += 1;
        ir::FieldId(index)
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

    fn flush_dom_program(&mut self, ctx: &mut Context) {
        if self.current_dom_opcodes.is_empty() {
            return;
        }

        let opcodes = std::mem::replace(&mut self.current_dom_opcodes, vec![]);

        let program_id = ctx.next_dom_program_id();

        enum NodeType {
            Element,
            Text,
        }

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
                field,
                ty: program_ty,
            });
            Some(field)
        } else {
            None
        };

        self.statements.push(ir::Statement {
            field: opt_field,
            dom_depth: self.current_dom_program_depth,
            expression: ir::Expression::ConstDom(ir::ConstDomProgram {
                id: program_id,
                opcodes,
            }),
        });

        self.current_dom_program_depth = ctx.current_dom_depth;
    }

    fn push_statement(&mut self, statement: ir::Statement, ctx: &mut Context) {
        self.flush_dom_program(ctx);

        self.statements.push(statement);
    }

    fn lower_ast(&mut self, node: template_ast::Node, ctx: &mut Context) {
        match node {
            template_ast::Node::Element(element) => self.lower_element(element, ctx),
            template_ast::Node::Fragment(nodes) => {
                for node in nodes {
                    self.lower_ast(node, ctx);
                }
            }
            template_ast::Node::Text(text) => self.lower_text(text),
            template_ast::Node::Variable(variable) => self.lower_variable(variable, ctx),
            template_ast::Node::Component(component) => self.lower_component(component, ctx),
            template_ast::Node::If(the_if) => self.lower_if(the_if, ctx),
            template_ast::Node::For(the_for) => {}
        }
    }

    fn lower_element(&mut self, element: template_ast::Element, ctx: &mut Context) {
        let tag_name = syn::LitStr::new(&element.tag_name.to_string(), element.tag_name.span());

        self.current_dom_opcodes
            .push(ir::DomOpCode::EnterElement(tag_name.clone()));

        ctx.current_dom_depth += 1;

        for child in element.children {
            self.lower_ast(child, ctx);
        }

        ctx.current_dom_depth -= 1;

        self.current_dom_opcodes.push(ir::DomOpCode::ExitElement);
    }

    fn lower_text(&mut self, text: template_ast::Text) {
        self.current_dom_opcodes.push(ir::DomOpCode::Text(text.0));
    }

    fn lower_variable(&mut self, variable: variable::Variable, ctx: &mut Context) {
        let node_field = ctx.next_field_id();
        let variable_field = ctx.next_field_id();

        self.struct_fields.push(ir::StructField {
            field: node_field,
            ty: ir::StructFieldType::DomText,
        });

        self.struct_fields.push(ir::StructField {
            field: variable_field,
            ty: ir::StructFieldType::Variable(variable.ty),
        });

        self.push_statement(
            ir::Statement {
                field: Some(node_field),
                dom_depth: ctx.current_dom_depth,
                expression: ir::Expression::VariableText {
                    variable_field,
                    expr: variable.ident.clone(),
                },
            },
            ctx,
        );
        self.push_statement(
            ir::Statement {
                field: Some(variable_field),
                dom_depth: ctx.current_dom_depth,
                expression: ir::Expression::LocalVar,
            },
            ctx,
        );
    }

    fn lower_component(&mut self, component: template_ast::Component, ctx: &mut Context) {
        let field = ctx.next_field_id();

        let component_path = ir::ComponentPath::new(component.type_path);

        self.struct_fields.push(ir::StructField {
            field,
            ty: ir::StructFieldType::Component(component_path.clone()),
        });

        self.push_statement(
            ir::Statement {
                field: Some(field),
                dom_depth: ctx.current_dom_depth,
                expression: ir::Expression::Component {
                    path: component_path,
                    props: component.attrs,
                },
            },
            ctx,
        );
    }

    fn lower_if(&mut self, the_if: template_ast::If, ctx: &mut Context) {
        let test = the_if.test;
        let field = ctx.next_field_id();
        let enum_type = ir::StructFieldType::Enum(ctx.next_enum_index());

        let mut then_builder = BlockBuilder::default();
        then_builder.lower_ast(*the_if.then_branch, ctx);

        let mut else_builder = BlockBuilder::default();

        match the_if.else_branch {
            Some(template_ast::Else::If(_, else_if)) => {
                else_builder.lower_if(*else_if, ctx);
            }
            Some(template_ast::Else::Node(_, node)) => {
                else_builder.lower_ast(*node, ctx);
            }
            None => {}
        }

        self.push_statement(
            ir::Statement {
                field: Some(field),
                dom_depth: ctx.current_dom_depth,
                expression: ir::Expression::Match {
                    enum_type: enum_type.clone(),
                    expr: test,
                    arms: vec![
                        ir::Arm {
                            enum_variant_ident: quote::format_ident!("True"),
                            mount_fn_ident: quote::format_ident!("mount_true"),
                            pattern: syn::parse_quote! { true },
                            block: then_builder.to_block(ctx),
                        },
                        ir::Arm {
                            enum_variant_ident: quote::format_ident!("False"),
                            mount_fn_ident: quote::format_ident!("mount_false"),
                            pattern: syn::parse_quote! { false },
                            block: else_builder.to_block(ctx),
                        },
                    ],
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
