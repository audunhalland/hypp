//!
//! Parser for the complete component syntax
//!

use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;

use crate::namespace::Namespace;
use crate::param;
use crate::template_ast;

pub struct Component {
    pub ident: syn::Ident,
    pub generics: ComponentGenerics,
    pub params: Vec<param::Param>,
    pub methods: Vec<syn::ItemFn>,
    pub template: template_ast::Node,
}

pub struct ComponentGenerics {
    pub has_explicit_hypp: bool,

    pub hypp_ident: syn::Ident,

    // Public generics, for the whole component structs
    pub public: ItemGenerics,

    // Internal generics, used for props and env structs
    pub internal: Option<ItemGenerics>,

    pub ns: Namespace,
}

pub struct ItemGenerics {
    // 'incoming' generics, e.g. `H: ::hypp::Hypp` in `impl<H: ::hypp::Hypp> ...`
    pub params: Vec<syn::TypeParam>,
    // 'outgoing' generics, e.g. `H` in `SomeStruct<H>`
    pub arguments: Vec<syn::GenericArgument>,
}

impl Parse for Component {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let generics = ComponentGenerics::try_from_generics(input.parse()?)?;

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

        let mut methods = vec![];

        while input.peek(syn::token::Fn) {
            methods.push(input.parse::<syn::ItemFn>()?);
        }

        let template =
            template_ast::TemplateParser::for_namespace(generics.ns).parse_at_least_one(input)?;

        Ok(Self {
            ident,
            generics,
            params,
            methods,
            template,
        })
    }
}

enum GenericComponentParam {
    Hypp(syn::TypeParam),
    Generic(syn::TypeParam),
}

impl ComponentGenerics {
    fn try_from_generics(generics: syn::Generics) -> syn::Result<Self> {
        let mut explicit_hypp: Option<syn::TypeParam> = None;
        let mut type_params = vec![];

        for ty_param in generics.params.into_iter() {
            let component_ty_param = Self::analyze_generic(ty_param)?;
            match component_ty_param {
                GenericComponentParam::Hypp(ty_param) => {
                    explicit_hypp = Some(ty_param);
                }
                GenericComponentParam::Generic(ty_param) => {
                    type_params.push(ty_param);
                }
            }
        }

        if let Some(explicit_hypp) = explicit_hypp {
            let hypp_ident = explicit_hypp.ident.clone();

            let mut final_type_params = vec![explicit_hypp];
            final_type_params.extend(type_params.into_iter());

            Ok(Self {
                has_explicit_hypp: true,
                hypp_ident,
                public: ItemGenerics::from_type_params(final_type_params.clone()),
                internal: Some(ItemGenerics::from_type_params(final_type_params.clone())),
                ns: Namespace::default(),
            })
        } else {
            let hypp_ident = quote::format_ident!("H");
            let hypp_type_param: syn::TypeParam = syn::parse_quote! {
                H: ::hypp::Hypp + 'static
            };

            let mut public_type_params = vec![hypp_type_param];
            public_type_params.extend(type_params.clone().into_iter());

            Ok(Self {
                has_explicit_hypp: true,
                hypp_ident,
                public: ItemGenerics::from_type_params(public_type_params),
                internal: if type_params.is_empty() {
                    None
                } else {
                    Some(ItemGenerics::from_type_params(type_params))
                },
                ns: Namespace::default(),
            })
        }
    }

    fn analyze_generic(param: syn::GenericParam) -> syn::Result<GenericComponentParam> {
        match param {
            syn::GenericParam::Type(ty_param) => {
                let mut is_hypp = false;
                for bound in &ty_param.bounds {
                    match bound {
                        syn::TypeParamBound::Trait(tr) => {
                            if Self::path_is_hypp(&tr.path) {
                                is_hypp = true;
                            }
                        }
                        syn::TypeParamBound::Lifetime(_) => {}
                    }
                }

                if is_hypp {
                    Ok(GenericComponentParam::Hypp(ty_param))
                } else {
                    Ok(GenericComponentParam::Generic(ty_param))
                }
            }
            syn::GenericParam::Lifetime(_) => {
                return Err(syn::Error::new(
                    param.span(),
                    "Lifetime params are not supported",
                ));
            }
            syn::GenericParam::Const(_) => {
                return Err(syn::Error::new(
                    param.span(),
                    "Const params are not supported",
                ));
            }
        }
    }

    fn path_is_hypp(_path: &syn::Path) -> bool {
        // BUG: determine for real
        true
    }
}

impl ItemGenerics {
    fn from_type_params(type_params: Vec<syn::TypeParam>) -> Self {
        let arguments = type_params
            .iter()
            .map(|param| {
                let mut segments = syn::punctuated::Punctuated::new();
                segments.push(syn::PathSegment {
                    ident: param.ident.clone(),
                    arguments: syn::PathArguments::None,
                });

                syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments,
                    },
                }))
            })
            .collect();

        Self {
            params: type_params,
            arguments,
        }
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

    let triple = param::ParamParser::new(kind)
        .parse_triple(input.parse()?)
        .map_err(|param_error| match param_error {
            param::ParamError::UnparseableType(span) => {
                syn::Error::new(span, "Incomprehensible type")
            }
        })?;

    Ok(param::Param {
        id: 0,
        ident,
        triple,
    })
}
