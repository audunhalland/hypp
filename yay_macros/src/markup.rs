use std::fmt::Display;

use syn::parse::{Parse, ParseStream};

use crate::variable::Variable;

#[derive(Debug, Eq, PartialEq)]
pub enum Node {
    Element(Element),
    Fragment(Vec<Node>),
    Text(Text),
    Variable(Variable),
    Component(Component),
}

type Attr = (syn::Ident, AttrValue);

#[derive(Debug, Eq, PartialEq)]
pub enum AttrValue {
    ImplicitTrue,
    Literal(syn::Lit),
    Eval(syn::Ident),
}

impl Parse for Node {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Token!(<)) {
            return Ok(parse_element_or_fragment(input)?);
        }

        if let Ok(text) = input.parse::<Text>() {
            return Ok(Self::Text(text));
        }

        // Fallback: evaluate expression in {}
        let content;
        let _brace_token = syn::braced!(content in input);

        let ident: syn::Ident = content.parse()?;

        Ok(Self::Variable(Variable::with_ident(ident)))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Text(pub syn::LitStr);

impl Parse for Text {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lit_str = input.parse::<syn::LitStr>()?;
        Ok(Self(lit_str))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Element {
    pub tag_name: syn::Ident,
    pub attrs: Vec<Attr>,
    pub children: Vec<Node>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Component {
    pub type_path: syn::TypePath,
    pub attrs: Vec<Attr>,
}

fn parse_element_or_fragment(input: ParseStream) -> syn::Result<Node> {
    // Opening:
    input.parse::<syn::Token!(<)>()?;

    if input.peek(syn::Token!(>)) {
        input.parse::<syn::Token!(>)>()?;
        let nodes = parse_children(input)?;
        input.parse::<syn::Token!(<)>()?;
        input.parse::<syn::Token!(/)>()?;
        input.parse::<syn::Token!(>)>()?;

        return Ok(Node::Fragment(nodes));
    }

    let name = parse_name(input)?;

    let attrs = parse_attrs(input)?;

    if input.peek(syn::Token!(/)) {
        input.parse::<syn::Token!(/)>()?;
        input.parse::<syn::Token!(>)>()?;

        return Ok(element_or_component(name, attrs, vec![]));
    }

    input.parse::<syn::Token!(>)>()?;

    let children = parse_children(input)?;

    // Closing:
    input.parse::<syn::Token!(<)>()?;
    input.parse::<syn::Token!(/)>()?;

    let end_name = parse_name(input)?;
    if end_name != name {
        return Err(syn::Error::new(
            input.span(),
            format!(
                "Unexpected closing name `{}`. Expected `{}`.",
                end_name, name
            ),
        ));
    }
    input.parse::<syn::Token!(>)>()?;

    Ok(element_or_component(name, attrs, children))
}

#[derive(Eq, PartialEq)]
enum Name {
    Tag(syn::Ident),
    Component(syn::TypePath),
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Tag(ident) => ident.fmt(f),
            Self::Component(type_path) => {
                let tokens = quote::quote! {
                    #type_path
                };

                tokens.fmt(f)
            }
        }
    }
}

fn element_or_component(name: Name, attrs: Vec<Attr>, children: Vec<Node>) -> Node {
    match name {
        Name::Tag(ident) => Node::Element(Element {
            tag_name: ident,
            attrs,
            children,
        }),
        Name::Component(type_path) => Node::Component(Component { type_path, attrs }),
    }
}

fn parse_name(input: ParseStream) -> syn::Result<Name> {
    let type_path: syn::TypePath = input.parse()?;

    Ok(if type_path.path.segments.len() == 1 {
        let ident = &type_path.path.segments[0].ident;

        if ident < &quote::format_ident!("a") {
            Name::Component(type_path)
        } else {
            Name::Tag(type_path.path.segments.into_iter().next().unwrap().ident)
        }
    } else {
        Name::Component(type_path)
    })
}

fn parse_attrs(input: ParseStream) -> syn::Result<Vec<Attr>> {
    let mut attrs = vec![];

    loop {
        if input.peek(syn::Token!(/)) || input.peek(syn::Token!(>)) {
            break;
        }

        let name: syn::Ident = input.parse()?;

        let value = if input.peek(syn::Token!(=)) {
            let _eq: syn::Token!(=) = input.parse()?;

            if input.peek(syn::Lit) {
                AttrValue::Literal(input.parse()?)
            } else {
                let content;
                let _brace_token = syn::braced!(content in input);

                let ident: syn::Ident = content.parse()?;

                AttrValue::Eval(ident)
            }
        } else {
            AttrValue::ImplicitTrue
        };

        attrs.push((name, value));
    }

    Ok(attrs)
}

fn parse_children(input: ParseStream) -> syn::Result<Vec<Node>> {
    let mut children = vec![];
    while !input.is_empty() {
        if input.peek(syn::Token!(<)) && input.peek2(syn::Token!(/)) {
            break;
        }

        children.push(input.parse()?);
    }

    Ok(children)
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    fn element(tag_name: &str, attrs: Vec<Attr>, children: Vec<Node>) -> Node {
        Node::Element(Element {
            tag_name: syn::Ident::new(tag_name, proc_macro2::Span::mixed_site()),
            attrs,
            children,
        })
    }

    fn fragment(nodes: Vec<Node>) -> Node {
        Node::Fragment(nodes)
    }

    fn text(text: &str) -> Node {
        Node::Text(Text(syn::LitStr::new(
            text,
            proc_macro2::Span::mixed_site(),
        )))
    }

    fn var(name: &str) -> Node {
        Node::Variable(Variable::with_ident(quote::format_ident!("{}", name)))
    }

    fn component(type_path: syn::TypePath, attrs: Vec<Attr>) -> Node {
        Node::Component(Component { type_path, attrs })
    }

    fn attr(name: &str, value: AttrValue) -> Attr {
        (
            syn::Ident::new(name, proc_macro2::Span::mixed_site()),
            value,
        )
    }

    #[test]
    fn parse_empty_element_no_children_not_self_closing() {
        let node: Node = syn::parse2(quote! {
            <p></p>
        })
        .unwrap();
        assert_eq!(node, element("p", vec![], vec![]));
    }

    #[test]
    fn parse_unmatched_closing_tag_fails() {
        let result: syn::Result<Node> = syn::parse2(quote! {
            <p></q>
        });
        assert!(result.is_err());
    }

    #[test]
    fn parse_empty_element_self_closing() {
        let node: Node = syn::parse2(quote! {
            <p/>
        })
        .unwrap();
        assert_eq!(node, element("p", vec![], vec![]));
    }

    #[test]
    fn parse_empty_component_self_closing() {
        let node: Node = syn::parse2(quote! {
            <P/>
        })
        .unwrap();

        assert_eq!(
            node,
            component(
                syn::parse_quote! {
                    P
                },
                vec![]
            )
        );
    }

    #[test]
    fn parse_empty_component_with_path_self_closing() {
        let node: Node = syn::parse2(quote! {
            <module::P/>
        })
        .unwrap();
        assert_eq!(
            node,
            component(
                syn::parse_quote! {
                    module::P
                },
                vec![]
            )
        );
    }

    #[test]
    fn parse_element_with_children() {
        let node: Node = syn::parse2(quote! {
            <p>
                <strong>"Strong"</strong>
                "not strong"
            </p>
        })
        .unwrap();
        assert_eq!(
            node,
            element(
                "p",
                vec![],
                vec![
                    element("strong", vec![], vec![text("Strong")]),
                    text("not strong")
                ]
            )
        );
    }

    #[test]
    fn parse_fragment() {
        let node: Node = syn::parse2(quote! {
            <>
                <p/>
                <div/>
            </>
        })
        .unwrap();
        assert_eq!(
            node,
            fragment(vec![
                element("p", vec![], vec![]),
                element("div", vec![], vec![])
            ])
        );
    }

    #[test]
    fn parse_element_with_variable() {
        let node: Node = syn::parse2(quote! {
            <p>
                {variable}
            </p>
        })
        .unwrap();
        assert_eq!(node, element("p", vec![], vec![var("variable")]));
    }

    #[test]
    fn parse_element_with_attrs() {
        let node: Node = syn::parse2(quote! {
            <p a b="b" c=42 d={foo} />
        })
        .unwrap();
        assert_eq!(
            node,
            element(
                "p",
                vec![
                    attr("a", AttrValue::ImplicitTrue),
                    attr("b", AttrValue::Literal(syn::parse_quote! { "b" })),
                    attr("c", AttrValue::Literal(syn::parse_quote! { 42 })),
                    attr("d", AttrValue::Eval(syn::parse_quote! { foo })),
                ],
                vec![]
            )
        );
    }
}
