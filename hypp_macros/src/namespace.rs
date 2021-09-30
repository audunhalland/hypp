use proc_macro2::TokenStream;
use quote::quote;
use web_ns::{AttrByLocalName, TagByLocalName};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Namespace {
    Html,
}

// Which direction lowering will lower children,
// affecting the generated code.
// It will be constant for a specific implementation of Hypp.
#[allow(unused)]
pub enum TraversalDirection {
    FirstToLast,
    LastToFirst,
}

#[derive(Debug, Eq, PartialEq)]
pub struct NSName<N> {
    pub input: String,
    pub name: N,
}

#[derive(Debug, Eq, PartialEq)]
pub enum NSTagName {
    Html(web_ns::html5::HtmlTag),
}

#[derive(Debug, Eq, PartialEq)]
pub enum NSAttrName {
    Html(web_ns::html5::HtmlAttr),
}

pub trait OpcodeValue {
    fn opcode_value(&self) -> syn::Expr;
}

impl Namespace {
    pub fn traversal_direction(&self) -> TraversalDirection {
        match self {
            Self::Html => TraversalDirection::LastToFirst,
        }
    }

    pub fn hypp_ns(&self) -> TokenStream {
        quote! {
            ::hypp::ns::Html
        }
    }

    pub fn parse_tag_name(
        &self,
        input: String,
        span: proc_macro2::Span,
    ) -> syn::Result<NSName<NSTagName>> {
        match self {
            Self::Html => {
                let tag_name = web_ns::html5::HTML5_NS
                    .tag_by_local_name(&input)
                    .map_err(|error| self.web_ns_error_to_syn_error(&input, error, span))?;

                Ok(NSName {
                    input,
                    name: NSTagName::Html(tag_name),
                })
            }
        }
    }

    fn web_ns_error_to_syn_error(
        &self,
        local_name: &str,
        _error: web_ns::Error,
        span: proc_macro2::Span,
    ) -> syn::Error {
        syn::Error::new(
            span,
            format!(
                "invalid local name for namespace {:?}: {}",
                self, local_name
            ),
        )
    }
}

impl NSTagName {
    pub fn parse_attr_name(
        &self,
        input: String,
        span: proc_macro2::Span,
    ) -> syn::Result<NSName<NSAttrName>> {
        match self {
            Self::Html(tag_name) => {
                let attr_name = tag_name.attr_by_local_name(&input).map_err(|error| {
                    Namespace::Html.web_ns_error_to_syn_error(&input, error, span)
                })?;

                Ok(NSName {
                    input,
                    name: NSAttrName::Html(attr_name),
                })
            }
        }
    }
}

impl OpcodeValue for NSTagName {
    fn opcode_value(&self) -> syn::Expr {
        match self {
            Self::Html(tag_name) => {
                let ident = quote::format_ident!("{}", format!("{:?}", tag_name));

                syn::parse_quote! {
                    ::web_ns::html5::HtmlTag::#ident
                }
            }
        }
    }
}

impl OpcodeValue for NSAttrName {
    fn opcode_value(&self) -> syn::Expr {
        match self {
            Self::Html(attr_name) => {
                let ident = quote::format_ident!("{}", format!("{:?}", attr_name));

                syn::parse_quote! {
                    ::web_ns::html5::HtmlAttr::#ident
                }
            }
        }
    }
}
