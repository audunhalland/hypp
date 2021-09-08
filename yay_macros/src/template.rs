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

        root_block.analyze_node(root, &mut ctx);

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

impl ir::Block {
    fn next_variable_index(&mut self) -> usize {
        let index = self.variable_count;
        self.variable_count += 1;
        index
    }

    fn gen_node_ident(&mut self) -> syn::Ident {
        quote::format_ident!("node{}", self.next_variable_index())
    }

    fn gen_var_field_ident(&mut self) -> syn::Ident {
        quote::format_ident!("var{}", self.next_variable_index())
    }

    fn gen_comp_field_ident(&mut self) -> syn::Ident {
        quote::format_ident!("comp{}", self.next_variable_index())
    }

    fn analyze_node(&mut self, node: markup::Node, ctx: &mut Context) {
        match node {
            markup::Node::Element(element) => self.analyze_element(element, ctx),
            markup::Node::Fragment(nodes) => {
                for node in nodes {
                    self.analyze_node(node, ctx);
                }
                panic!("TODO: implement fragment");
            }
            markup::Node::Text(text) => self.analyze_text(text),
            markup::Node::Variable(variable) => self.analyze_variable(variable),
            markup::Node::Component(component) => self.analyze_component(component),
            markup::Node::If(the_if) => self.analyze_if(the_if, ctx),
        };
    }

    fn analyze_element(&mut self, element: markup::Element, ctx: &mut Context) {
        let tag_name = syn::LitStr::new(&element.tag_name.to_string(), element.tag_name.span());

        self.program.push(ir::OpCode::EnterElement { tag_name });

        for child in element.children {
            self.analyze_node(child, ctx);
        }

        self.program.push(ir::OpCode::ExitElement);
    }

    fn analyze_text(&mut self, text: markup::Text) {
        self.program.push(ir::OpCode::TextConst { text: text.0 });
    }

    fn analyze_variable(&mut self, variable: variable::Variable) {
        let node_ident = self.gen_node_ident();
        self.struct_fields.push(ir::StructField {
            ident: node_ident.clone(),
            ty: ir::StructFieldType::DomText,
        });

        let variable_ident = self.gen_var_field_ident();
        self.struct_fields.push(ir::StructField {
            ident: variable_ident.clone(),
            ty: ir::StructFieldType::Variable(variable.ty),
        });

        self.program.push(ir::OpCode::TextVar {
            node_binding: node_ident.clone(),
            variable_binding: variable_ident,
            expr: variable.ident.clone(),
        });
    }

    fn analyze_component(&mut self, component: markup::Component) {
        let ident = self.gen_comp_field_ident();

        let component_path = ir::ComponentPath::new(component.type_path);

        self.program.push(ir::OpCode::Component {
            binding: ident.clone(),
            path: component_path.clone(),
            props: component.attrs.clone(),
        });

        self.struct_fields.push(ir::StructField {
            ident,
            ty: ir::StructFieldType::Component(component_path),
        });
    }

    fn analyze_if(&mut self, the_if: markup::If, ctx: &mut Context) {
        let test = the_if.test;
        let variant_field_ident = self.gen_var_field_ident();
        let enum_type = ir::StructFieldType::Enum(ctx.next_enum_index());

        let mut then_block = ir::Block::default();
        then_block.analyze_node(*the_if.then_branch, ctx);

        let mut else_block = ir::Block::default();

        match the_if.else_branch {
            Some(markup::Else::If(_, else_if)) => {
                else_block.analyze_if(*else_if, ctx);
            }
            Some(markup::Else::Node(_, node)) => {
                else_block.analyze_node(*node, ctx);
            }
            None => {}
        }

        self.program.push(ir::OpCode::Match {
            binding: variant_field_ident.clone(),
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
            ident: variant_field_ident,
            ty: enum_type,
        });
    }
}
