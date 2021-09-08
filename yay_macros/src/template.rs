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
    fn gen_node_ident(&self) -> syn::Ident {
        let index = self.constructor_stmts.len();
        quote::format_ident!("node{}", index)
    }

    fn gen_var_field_ident(&self) -> syn::Ident {
        let index = self.vars.len();
        quote::format_ident!("var{}", index)
    }

    fn gen_comp_field_ident(&self) -> syn::Ident {
        let index = self.component_updates.len();
        quote::format_ident!("comp{}", index)
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

        self.constructor_stmts
            .push(ir::Statement::EnterElement { tag_name });

        for child in element.children {
            self.analyze_node(child, ctx);
        }

        self.constructor_stmts.push(ir::Statement::ExitElement);
    }

    fn analyze_text(&mut self, text: markup::Text) {
        self.constructor_stmts
            .push(ir::Statement::TextConst { text: text.0 });
    }

    fn analyze_variable(&mut self, variable: variable::Variable) {
        let node_ident = self.gen_node_ident();

        self.constructor_stmts.push(ir::Statement::LetTextVar {
            binding: node_ident.clone(),
            var: variable.ident.clone(),
        });

        self.struct_fields.push(ir::StructField {
            ident: node_ident.clone(),
            ty: ir::StructFieldType::DomText,
        });

        let field_ident = self.gen_var_field_ident();

        self.vars.push(ir::TemplateVar {
            variable,
            node_ident: node_ident.clone(),
            field_ident,
        });
    }

    fn analyze_component(&mut self, component: markup::Component) {
        let ident = self.gen_comp_field_ident();

        let component_path = ir::ComponentPath::new(component.type_path);

        self.constructor_stmts
            .push(ir::Statement::LetInstantiateComponent {
                binding: ident.clone(),
                path: component_path.clone(),
                props: component.attrs.clone(),
            });

        self.component_updates.push(ir::Statement::UpdateComponent {
            in_self: true,
            ident: ident.clone(),
            path: component_path.clone(),
            props: component.attrs,
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

        self.struct_fields.push(ir::StructField {
            ident: variant_field_ident.clone(),
            ty: enum_type.clone(),
        });

        self.matches.push(ir::Match {
            enum_type,
            variant_field_ident,
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
    }
}
