use quote::quote;

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

        self.constructor_stmts.push(quote! {
            __vm.enter_element(#tag_name)?;
        });

        for child in element.children {
            self.analyze_node(child, ctx);
        }

        self.constructor_stmts.push(quote! {
            __vm.exit_element()?;
        });
    }

    fn analyze_text(&mut self, text: markup::Text) {
        let lit_str = text.0;

        self.constructor_stmts.push(quote! {
            __vm.text(#lit_str)?;
        });
    }

    fn analyze_variable(&mut self, variable: variable::Variable) {
        let node_ident = self.gen_node_ident();
        let variable_ident = &variable.ident;

        self.constructor_stmts.push(quote! {
            let #node_ident = __vm.text(#variable_ident)?;
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
        let type_path = component.type_path.clone();

        let ident = self.gen_comp_field_ident();

        let component_path = ir::ComponentPath::new(component.type_path);
        let component_ident = component_path.ident();

        let prop_list: Vec<_> = component
            .attrs
            .into_iter()
            .map(|(name, value)| match value {
                markup::AttrValue::ImplicitTrue => quote! {
                    #name: true,
                },
                markup::AttrValue::Literal(lit) => quote! {
                    #name: #lit,
                },
                markup::AttrValue::Eval(ident) => quote! {
                    #name: #ident,
                },
            })
            .collect();

        let mut props_path = type_path.path.clone();
        if let Some(last_props_path_segment) = props_path.segments.last_mut() {
            last_props_path_segment.ident = quote::format_ident!("{}Props", component_ident);
            last_props_path_segment.arguments = syn::PathArguments::None;
        }

        self.constructor_stmts.push(quote! {
            let #ident = #type_path::new(
                #props_path {
                    #(#prop_list)*
                    __phantom: std::marker::PhantomData
                },
                __vm
            )?;
        });

        let stmt: syn::Stmt = syn::parse_quote! {
            self.#ident.update(#props_path {
                #(#prop_list)*
                __phantom: std::marker::PhantomData
            }, __vm);
        };

        self.component_updates.push(stmt);

        self.struct_fields.push(ir::StructField {
            ident: ident.clone(),
            ty: ir::StructFieldType::Component(component_path),
        });
    }

    fn analyze_if(&mut self, mut the_if: markup::If, ctx: &mut Context) {
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
