use syn::parse::{Parse, ParseStream};

use crate::template_ast;

pub struct Component {
    ident: syn::Ident,
    props: syn::punctuated::Punctuated<NamedField, syn::Token![,]>,
    state: syn::punctuated::Punctuated<NamedField, syn::Token![,]>,
    fns: Vec<syn::ItemFn>,
    template: template_ast::Node,
}

struct NamedField {
    ident: syn::Ident,
    colon_token: syn::Token![:],
    ty: syn::Type,
}

impl Parse for Component {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;

        let props_content;
        syn::parenthesized!(props_content in input);
        let props = props_content.parse_terminated(parse_named_field)?;

        let state_content;
        syn::braced!(state_content in input);
        let state = state_content.parse_terminated(parse_named_field)?;

        let mut fns = vec![];

        while input.peek(syn::Token!(fn)) {
            // TODO: ignore for now
            fns.push(input.parse::<syn::ItemFn>()?);
        }

        let template = template_ast::parse_at_least_one(input)?;

        Ok(Self {
            ident,
            props,
            state,
            fns,
            template,
        })
    }
}

fn parse_named_field(input: ParseStream) -> syn::Result<NamedField> {
    let ident = input.parse()?;
    let colon_token = input.parse()?;
    let ty = input.parse()?;

    Ok(NamedField {
        ident,
        colon_token,
        ty,
    })
}
