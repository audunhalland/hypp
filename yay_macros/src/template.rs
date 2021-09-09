use crate::ir;
use crate::markup;
use crate::variable;

pub struct Template {
    pub root_block: ir::Block,
}

impl Template {
    pub fn analyze(root: markup::Node) -> Self {
        let mut root_block = ir::Block::default();

        let mut ctx = Context { enum_count: 0 };

        root_block.analyze_node(root, None, &mut ctx);

        Self { root_block }
    }
}

/// Context used for the whole template
pub struct Context {
    enum_count: usize,
}

impl Context {
    fn next_enum_index(&mut self) -> usize {
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

impl ir::Block {
    fn next_field_id(&mut self) -> ir::FieldId {
        let index = self.variable_count;
        self.variable_count += 1;
        ir::FieldId(index)
    }

    fn analyze_node(
        &mut self,
        node: markup::Node,
        parent: Option<ir::FieldId>,
        ctx: &mut Context,
    ) -> Constness {
        match node {
            markup::Node::Element(element) => self.analyze_element(element, ctx),
            markup::Node::Fragment(nodes) => {
                let mut constness = Constness::Const;
                for node in nodes {
                    constness = constness.and(self.analyze_node(node, parent, ctx));
                }

                constness
            }
            markup::Node::Text(text) => self.analyze_text(text),
            markup::Node::Variable(variable) => self.analyze_variable(variable),
            markup::Node::Component(component) => self.analyze_component(component, parent),
            markup::Node::If(the_if) => self.analyze_if(the_if, parent, ctx),
        }
    }

    fn analyze_element(&mut self, element: markup::Element, ctx: &mut Context) -> Constness {
        let tag_name = syn::LitStr::new(&element.tag_name.to_string(), element.tag_name.span());

        // We don't know if we should allocate this field yet
        let field = self.next_field_id();

        self.program
            .push(ir::OpCode::EnterElement { field, tag_name });

        let mut constness = Constness::Const;

        for child in element.children {
            constness = constness.and(self.analyze_node(child, Some(field), ctx));
        }

        self.program.push(ir::OpCode::ExitElement);

        match constness {
            Constness::Variable => {
                // Need to store this field
                self.struct_fields.push(ir::StructField {
                    field,
                    ty: ir::StructFieldType::DomElement,
                });
            }
            _ => {}
        }

        constness
    }

    fn analyze_text(&mut self, text: markup::Text) -> Constness {
        self.program.push(ir::OpCode::TextConst { text: text.0 });
        Constness::Const
    }

    fn analyze_variable(&mut self, variable: variable::Variable) -> Constness {
        let node_field = self.next_field_id();
        self.struct_fields.push(ir::StructField {
            field: node_field,
            ty: ir::StructFieldType::DomText,
        });

        let variable_field = self.next_field_id();
        self.struct_fields.push(ir::StructField {
            field: variable_field,
            ty: ir::StructFieldType::Variable(variable.ty),
        });

        self.program.push(ir::OpCode::TextVar {
            node_field,
            variable_field,
            expr: variable.ident.clone(),
        });

        Constness::Variable
    }

    fn analyze_component(
        &mut self,
        component: markup::Component,
        parent: Option<ir::FieldId>,
    ) -> Constness {
        let field = self.next_field_id();

        let component_path = ir::ComponentPath::new(component.type_path);

        self.program.push(ir::OpCode::Component {
            parent,
            field,
            path: component_path.clone(),
            props: component.attrs.clone(),
        });

        self.struct_fields.push(ir::StructField {
            field,
            ty: ir::StructFieldType::Component(component_path),
        });

        Constness::Variable
    }

    fn analyze_if(
        &mut self,
        the_if: markup::If,
        parent: Option<ir::FieldId>,
        ctx: &mut Context,
    ) -> Constness {
        let test = the_if.test;
        let field = self.next_field_id();
        let enum_type = ir::StructFieldType::Enum(ctx.next_enum_index());

        let mut then_block = ir::Block::default();
        then_block.analyze_node(*the_if.then_branch, None, ctx);

        let mut else_block = ir::Block::default();

        match the_if.else_branch {
            Some(markup::Else::If(_, else_if)) => {
                else_block.analyze_if(*else_if, None, ctx);
            }
            Some(markup::Else::Node(_, node)) => {
                else_block.analyze_node(*node, None, ctx);
            }
            None => {}
        }

        self.program.push(ir::OpCode::Match {
            parent,
            field,
            enum_type: enum_type.clone(),
            expr: test,
            arms: vec![
                ir::Arm {
                    enum_variant_ident: quote::format_ident!("True"),
                    pattern: syn::parse_quote! { true },
                    block: then_block,
                },
                ir::Arm {
                    enum_variant_ident: quote::format_ident!("False"),
                    pattern: syn::parse_quote! { false },
                    block: else_block,
                },
            ],
        });

        self.struct_fields.push(ir::StructField {
            field,
            ty: enum_type,
        });

        Constness::Variable
    }
}
