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
    pub ident: syn::Ident,
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

pub enum NSAttrKind {
    Callback,
    Misc,
}

pub trait OpcodeValue {
    fn opcode_value(&self) -> syn::Expr;
}

impl Default for Namespace {
    fn default() -> Self {
        Self::Html
    }
}

impl Namespace {
    pub fn traversal_direction(&self) -> TraversalDirection {
        match self {
            Self::Html => TraversalDirection::LastToFirst,
        }
    }

    pub fn hypp_ns(&self) -> TokenStream {
        quote! {
            ::hypp::html::Html
        }
    }

    pub fn parse_tag_name(
        &self,
        ident: syn::Ident,
        ident_string: String,
        span: proc_macro2::Span,
    ) -> syn::Result<NSName<NSTagName>> {
        match self {
            Self::Html => {
                let tag_name = web_ns::html5::HTML5_NS
                    .tag_by_local_name(&ident_string)
                    .map_err(|error| self.web_ns_error_to_syn_error(&ident_string, error, span))?;

                Ok(NSName {
                    ident,
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
        ident: syn::Ident,
        ident_string: String,
        span: proc_macro2::Span,
    ) -> syn::Result<NSName<NSAttrName>> {
        match self {
            Self::Html(tag_name) => {
                let attr_name = tag_name
                    .attr_by_local_name(&ident_string)
                    .map_err(|error| {
                        Namespace::Html.web_ns_error_to_syn_error(&ident_string, error, span)
                    })?;

                Ok(NSName {
                    ident,
                    name: NSAttrName::Html(attr_name),
                })
            }
        }
    }
}

impl NSAttrName {
    pub fn kind(&self) -> NSAttrKind {
        use web_ns::html5::HtmlAttr;
        match self {
            NSAttrName::Html(attr) => match attr {
                HtmlAttr::Onclick
                | HtmlAttr::Onclose
                | HtmlAttr::Oncontextmenu
                | HtmlAttr::Oncopy
                | HtmlAttr::Oncuechange
                | HtmlAttr::Oncut
                | HtmlAttr::Ondblclick
                | HtmlAttr::Ondrag
                | HtmlAttr::Ondragend
                | HtmlAttr::Ondragenter
                | HtmlAttr::Ondragexit
                | HtmlAttr::Ondragleave
                | HtmlAttr::Ondragover
                | HtmlAttr::Ondragstart
                | HtmlAttr::Ondrop
                | HtmlAttr::Ondurationchange
                | HtmlAttr::Onemptied
                | HtmlAttr::Onended
                | HtmlAttr::Onerror
                | HtmlAttr::Onfocus
                | HtmlAttr::Onformdata
                | HtmlAttr::Onhashchange
                | HtmlAttr::Oninput
                | HtmlAttr::Oninvalid
                | HtmlAttr::Onkeydown
                | HtmlAttr::Onkeypress
                | HtmlAttr::Onkeyup
                | HtmlAttr::Onlanguagechange
                | HtmlAttr::Onload
                | HtmlAttr::Onloadeddata
                | HtmlAttr::Onloadedmetadata
                | HtmlAttr::Onloadend
                | HtmlAttr::Onloadstart
                | HtmlAttr::Onmessage
                | HtmlAttr::Onmessageerror
                | HtmlAttr::Onmousedown
                | HtmlAttr::Onmouseenter
                | HtmlAttr::Onmouseleave
                | HtmlAttr::Onmousemove
                | HtmlAttr::Onmouseout
                | HtmlAttr::Onmouseover
                | HtmlAttr::Onmouseup
                | HtmlAttr::Onoffline
                | HtmlAttr::Ononline
                | HtmlAttr::Onpagehide
                | HtmlAttr::Onpageshow
                | HtmlAttr::Onpaste
                | HtmlAttr::Onpause
                | HtmlAttr::Onplay
                | HtmlAttr::Onplaying
                | HtmlAttr::Onpopstate
                | HtmlAttr::Onprogress
                | HtmlAttr::Onratechange
                | HtmlAttr::Onrejectionhandled
                | HtmlAttr::Onreset
                | HtmlAttr::Onresize
                | HtmlAttr::Onscroll
                | HtmlAttr::Onsecuritypolicyviolation
                | HtmlAttr::Onseeked
                | HtmlAttr::Onseeking
                | HtmlAttr::Onselect
                | HtmlAttr::Onslotchange
                | HtmlAttr::Onstalled
                | HtmlAttr::Onstorage
                | HtmlAttr::Onsubmit
                | HtmlAttr::Onsuspend
                | HtmlAttr::Ontimeupdate
                | HtmlAttr::Ontoggle
                | HtmlAttr::Onunhandledrejection
                | HtmlAttr::Onunload
                | HtmlAttr::Onvolumechange
                | HtmlAttr::Onwaiting
                | HtmlAttr::Onwheel => NSAttrKind::Callback,
                _ => NSAttrKind::Misc,
            },
        }
    }
}

impl OpcodeValue for NSName<NSTagName> {
    fn opcode_value(&self) -> syn::Expr {
        match &self.name {
            NSTagName::Html(tag_name) => {
                let ident = quote::format_ident!("{}", format!("{:?}", tag_name));

                syn::parse_quote! {
                    ::hypp::html::HtmlTag::#ident
                }
            }
        }
    }
}

impl OpcodeValue for NSName<NSAttrName> {
    fn opcode_value(&self) -> syn::Expr {
        match &self.name {
            NSAttrName::Html(attr_name) => {
                let ident = quote::format_ident!("{}", format!("{:?}", attr_name));

                syn::parse_quote! {
                    ::hypp::html::HtmlAttr::#ident
                }
            }
        }
    }
}
