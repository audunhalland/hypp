use crate::ast;
use crate::ir;
use crate::variable;

pub fn lower_root_node(root: ast::Node) -> ir::Block {
    let mut root_builder = BlockBuilder::default();

    let mut ctx = Context {
        dom_program_count: 0,
        field_count: 0,
        enum_count: 0,
    };

    root_builder.lower_ast(root, None, &mut ctx);
    root_builder.to_block(&mut ctx)
}

/// Context used for the whole template
pub struct Context {
    dom_program_count: u16,
    field_count: u16,
    enum_count: u16,
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

#[derive(Copy, Clone)]
enum Constness {
    Const,
    Variable,
}

impl Constness {
    fn and(self, other: Self) -> Self {
        match self {
            Self::Const => other,
            Self::Variable => self,
        }
    }
}

#[derive(Default)]
struct BlockBuilder {
    pub struct_fields: Vec<ir::StructField>,
    pub statements: Vec<ir::Statement>,

    current_dom_opcodes: Vec<ir::DomOpCode>,
}

impl BlockBuilder {
    fn to_block(mut self, ctx: &mut Context) -> ir::Block {
        self.flush_dom_program(None, ctx);

        ir::Block {
            struct_fields: self.struct_fields,
            statements: self.statements,
        }
    }

    fn flush_dom_program(&mut self, field: Option<ir::FieldId>, ctx: &mut Context) {
        if self.current_dom_opcodes.is_empty() {
            return;
        }

        let mut opcodes = vec![];
        std::mem::swap(&mut self.current_dom_opcodes, &mut opcodes);

        let program_id = ctx.next_dom_program_id();

        match opcodes.last() {
            None => {}
            Some(ir::DomOpCode::EnterElement(_) | ir::DomOpCode::ExitElement) => {
                self.statements.push(ir::Statement {
                    field,
                    expression: ir::Expression::ConstDom(ir::ConstDomProgram {
                        id: program_id,
                        ty: ir::ConstDomProgramTy::Element,
                        opcodes,
                    }),
                });
            }
            Some(ir::DomOpCode::Text(_)) => {
                if field.is_some() {
                    panic!("There is no point storing a constant text node (I think)");
                }

                self.statements.push(ir::Statement {
                    field,
                    expression: ir::Expression::ConstDom(ir::ConstDomProgram {
                        id: program_id,
                        ty: ir::ConstDomProgramTy::Text,
                        opcodes,
                    }),
                });
            }
        }
    }

    fn push_statement(&mut self, statement: ir::Statement, ctx: &mut Context) {
        self.flush_dom_program(None, ctx);

        self.statements.push(statement);
    }

    fn lower_ast(
        &mut self,
        node: ast::Node,
        parent: Option<ir::FieldId>,
        ctx: &mut Context,
    ) -> Constness {
        match node {
            ast::Node::Element(element) => self.lower_element(element, ctx),
            ast::Node::Fragment(nodes) => {
                let mut constness = Constness::Const;
                for node in nodes {
                    constness = constness.and(self.lower_ast(node, parent, ctx));
                }

                constness
            }
            ast::Node::Text(text) => self.lower_text(text),
            ast::Node::Variable(variable) => self.lower_variable(variable, ctx),
            ast::Node::Component(component) => self.lower_component(component, parent, ctx),
            ast::Node::If(the_if) => self.lower_if(the_if, parent, ctx),
        }
    }

    fn lower_element(&mut self, element: ast::Element, ctx: &mut Context) -> Constness {
        let tag_name = syn::LitStr::new(&element.tag_name.to_string(), element.tag_name.span());

        // We don't know if we should allocate this field yet
        let field = ctx.next_field_id();

        self.current_dom_opcodes
            .push(ir::DomOpCode::EnterElement(tag_name.clone()));

        let mut constness = Constness::Const;

        for child in element.children {
            constness = constness.and(self.lower_ast(child, Some(field), ctx));
        }

        self.current_dom_opcodes.push(ir::DomOpCode::ExitElement);

        match constness {
            Constness::Variable => {
                // Need to store this field
                self.flush_dom_program(Some(field), ctx);

                self.struct_fields.push(ir::StructField {
                    field,
                    ty: ir::StructFieldType::DomElement,
                });
            }
            _ => {}
        }

        Constness::Const
    }

    fn lower_text(&mut self, text: ast::Text) -> Constness {
        self.current_dom_opcodes.push(ir::DomOpCode::Text(text.0));
        Constness::Const
    }

    fn lower_variable(&mut self, variable: variable::Variable, ctx: &mut Context) -> Constness {
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
                expression: ir::Expression::LocalVar,
            },
            ctx,
        );

        Constness::Variable
    }

    fn lower_component(
        &mut self,
        component: ast::Component,
        parent: Option<ir::FieldId>,
        ctx: &mut Context,
    ) -> Constness {
        let field = ctx.next_field_id();

        let component_path = ir::ComponentPath::new(component.type_path);

        self.struct_fields.push(ir::StructField {
            field,
            ty: ir::StructFieldType::Component(component_path.clone()),
        });

        self.push_statement(
            ir::Statement {
                field: Some(field),
                expression: ir::Expression::Component {
                    parent,
                    path: component_path,
                    props: component.attrs,
                },
            },
            ctx,
        );

        Constness::Variable
    }

    fn lower_if(
        &mut self,
        the_if: ast::If,
        parent: Option<ir::FieldId>,
        ctx: &mut Context,
    ) -> Constness {
        let test = the_if.test;
        let field = ctx.next_field_id();
        let enum_type = ir::StructFieldType::Enum(ctx.next_enum_index());

        let mut then_builder = BlockBuilder::default();
        then_builder.lower_ast(*the_if.then_branch, None, ctx);

        let mut else_builder = BlockBuilder::default();

        match the_if.else_branch {
            Some(ast::Else::If(_, else_if)) => {
                else_builder.lower_if(*else_if, None, ctx);
            }
            Some(ast::Else::Node(_, node)) => {
                else_builder.lower_ast(*node, None, ctx);
            }
            None => {}
        }

        self.push_statement(
            ir::Statement {
                field: Some(field),
                expression: ir::Expression::Match {
                    parent,
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

        Constness::Variable
    }
}