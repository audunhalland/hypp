use proc_macro2::TokenStream;
use quote::quote;

use crate::markup;
use crate::variable;

pub struct Template {
    pub node_fields: Vec<NodeField>,
    pub static_initializers: Vec<TokenStream>,
    pub vars: Vec<TemplateVar>,
    pub nodes: Vec<TemplateNode>,
}

enum Ref {
    Node(syn::Ident),
    Component(syn::Ident),
}

impl Template {
    pub fn analyze(root: markup::Node) -> Self {
        let mut template = Template {
            node_fields: vec![],
            static_initializers: vec![],
            vars: vec![],
            nodes: vec![],
        };

        template.analyze_node(root, None);
        template
    }

    fn gen_node_ident(&self) -> syn::Ident {
        let index = self.static_initializers.len();
        quote::format_ident!("node{}", index)
    }

    fn gen_var_field_ident(&self) -> syn::Ident {
        let index = self.vars.len();
        quote::format_ident!("var{}", index)
    }

    fn analyze_node(&mut self, node: markup::Node, parent: Option<&syn::Ident>) {
        let result_ref: Ref = match node {
            markup::Node::Element(element) => self.analyze_element(element),
            markup::Node::Fragment(nodes) => {
                for node in nodes {
                    self.analyze_node(node, None);
                }
                panic!("TODO: implement fragment");
            }
            markup::Node::Text(text) => self.analyze_text(text),
            markup::Node::Variable(variable) => self.analyze_variable(variable),
            markup::Node::Component(component) => self.analyze_component(component),
        };

        if let Some(parent_ident) = parent {
            match result_ref {
                Ref::Node(ident) => {
                    self.static_initializers.push(quote! {
                        Y::append_child(& #parent_ident, #ident.as_node())?;
                    });
                }
                Ref::Component(ident) => self.static_initializers.push(quote! {
                    #ident.mount(MountPoint {
                        yay: y,
                        parent: & #parent_ident,
                    })?;
                }),
            }
        }
    }

    fn analyze_element(&mut self, element: markup::Element) -> Ref {
        let ident = self.gen_node_ident();
        let tag_name = syn::LitStr::new(&element.tag_name.to_string(), element.tag_name.span());

        if self.static_initializers.is_empty() {
            // Root field
            self.node_fields.push(NodeField {
                ident: ident.clone(),
                ty: syn::parse_quote! {
                    Y::Element
                },
            })
        }

        self.static_initializers.push(quote! {
            let #ident = y.create_element(#tag_name);
        });

        for child in element.children {
            self.analyze_node(child, Some(&ident));
        }
        Ref::Node(ident)
    }

    fn analyze_text(&mut self, text: markup::Text) -> Ref {
        let ident = self.gen_node_ident();
        let lit_str = text.0;

        if self.static_initializers.is_empty() {
            // Root field
            self.node_fields.push(NodeField {
                ident: ident.clone(),
                ty: syn::parse_quote! {
                    Y::Text
                },
            })
        }

        self.static_initializers.push(quote! {
            let #ident = y.create_empty_text();
            Y::set_text(&#ident, #lit_str);
        });
        Ref::Node(ident)
    }

    fn analyze_variable(&mut self, variable: variable::Variable) -> Ref {
        let node_ident = self.gen_node_ident();

        self.static_initializers.push(quote! {
            let #node_ident = y.create_empty_text();
        });

        self.node_fields.push(NodeField {
            ident: node_ident.clone(),
            ty: syn::parse_quote! {
                Y::Text
            },
        });

        let field_ident = self.gen_var_field_ident();

        self.vars.push(TemplateVar {
            variable,
            node_ident: node_ident.clone(),
            field_ident,
        });

        Ref::Node(node_ident)
    }

    fn analyze_component(&mut self, component: markup::Component) -> Ref {
        let mut type_path = component.type_path;

        let ident = quote::format_ident!("todo_fixme_component");

        self.static_initializers.push(quote! {
            let #ident = #type_path::new(y)?;
        });

        if let Some(last_segment) = type_path.path.segments.last_mut() {
            last_segment.arguments = syn::PathArguments::AngleBracketed(syn::parse_quote! {
                <Y>
            });
        }

        self.node_fields.push(NodeField {
            ident: ident.clone(),
            ty: syn::Type::Path(type_path),
        });

        Ref::Component(ident)
    }
}

pub struct NodeField {
    pub ident: syn::Ident,
    pub ty: syn::Type,
}

pub struct TemplateNode {
    pub ty: syn::Type,
    pub must_store: bool,
}

pub struct TemplateVar {
    pub variable: variable::Variable,
    pub node_ident: syn::Ident,
    pub field_ident: syn::Ident,
}
