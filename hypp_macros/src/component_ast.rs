use syn::parse::{Parse, ParseStream};

use crate::param;
use crate::template_ast;

pub struct Component {
    pub ident: syn::Ident,
    pub params: Vec<param::Param>,
    pub fns: Vec<syn::ItemFn>,
    pub template: template_ast::Node,
}

impl Parse for Component {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;

        let props_content;
        syn::parenthesized!(props_content in input);
        let props: syn::punctuated::Punctuated<param::Param, syn::token::Comma> =
            props_content.parse_terminated(parse_prop)?;

        let state_content;
        syn::braced!(state_content in input);
        let state: syn::punctuated::Punctuated<param::Param, syn::token::Comma> =
            state_content.parse_terminated(parse_state)?;

        let mut params = vec![];
        params.extend(props.into_iter());
        params.extend(state.into_iter());

        // assign IDs
        for (id, param) in params.iter_mut().enumerate() {
            param.id = id as u16;
        }

        let mut fns = vec![];

        while input.peek(syn::token::Fn) {
            // TODO: ignore for now
            fns.push(input.parse::<syn::ItemFn>()?);
        }

        let template = template_ast::parse_at_least_one(input)?;

        Ok(Self {
            ident,
            params,
            fns,
            template,
        })
    }
}

fn parse_prop(input: ParseStream) -> syn::Result<param::Param> {
    parse_param(param::ParamKind::Prop, input)
}

fn parse_state(input: ParseStream) -> syn::Result<param::Param> {
    parse_param(param::ParamKind::State, input)
}

fn parse_param(kind: param::ParamKind, input: ParseStream) -> syn::Result<param::Param> {
    let ident = input.parse()?;
    let _: syn::token::Colon = input.parse()?;
    let ty = input.parse()?;

    Ok(param::Param {
        id: 0,
        kind,
        ident,
        ty,
    })
}