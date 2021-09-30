use std::fmt::Display;
use syn::parse::ParseStream;

use crate::namespace::*;

#[derive(Eq, PartialEq)]
pub enum TagName {
    NS(NSName<NSTagName>),
    Component(syn::TypePath),
}

impl Display for TagName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::NS(ns_name) => ns_name.input.fmt(f),
            Self::Component(type_path) => {
                let tokens = quote::quote! {
                    #type_path
                };

                tokens.fmt(f)
            }
        }
    }
}

pub fn parse_tag_name(input: ParseStream, namespace: Namespace) -> Result<TagName, syn::Error> {
    let type_path: syn::TypePath = input.parse()?;

    Ok(if type_path.path.segments.len() == 1 {
        let ident = &type_path.path.segments[0].ident;
        let ident_string = ident.to_string();

        // Component names start with an uppercase letter
        if ident_string.as_str() < "a" {
            TagName::Component(type_path)
        } else {
            TagName::NS(namespace.parse_tag_name(ident_string, ident.span())?)
        }
    } else {
        TagName::Component(type_path)
    })
}
