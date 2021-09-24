use syn::spanned::Spanned;

#[derive(Debug)]
pub enum ParamError {
    UnparseableType(proc_macro2::Span),
}

/// A parameter to a component
#[derive(Clone)]
pub struct Param {
    pub id: u16,
    pub ident: syn::Ident,
    pub kind: ParamKind,
}

impl Param {
    pub fn is_prop(&self) -> bool {
        match &self.kind {
            ParamKind::Prop(_) => true,
            ParamKind::State(_) => false,
        }
    }

    pub fn is_state(&self) -> bool {
        !self.is_prop()
    }
}

#[derive(Clone)]
pub enum ParamKind {
    Prop(ParamRootType),
    State(syn::Type),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParamRootType {
    One(ParamLeafType),
    Option(ParamLeafType),
    // Slice(ParamLeafType),
}

impl ParamRootType {
    pub fn try_from_type(ty: syn::Type) -> Result<Self, ParamError> {
        match ty {
            syn::Type::Slice(_) => {
                unimplemented!()
                // Ok(Self::Slice(ParamLeafType::try_from_type(*slice.elem)?)),
            }
            syn::Type::Path(path) => Self::from_path(path),
            ty => Ok(Self::One(ParamLeafType::try_from_type(ty)?)),
        }
    }

    fn from_path(path: syn::TypePath) -> Result<Self, ParamError> {
        if let Some(_) = &path.qself {
            Ok(Self::One(ParamLeafType::try_from_type(syn::Type::Path(
                path,
            ))?))
        } else if path.path.segments.len() > 1 {
            Ok(Self::One(ParamLeafType::try_from_type(syn::Type::Path(
                path,
            ))?))
        } else if path.path.segments.is_empty() {
            Err(ParamError::UnparseableType(path.span()))
        } else if Self::peek_is_option(&path) {
            let first_segment = path.path.segments.into_iter().next().unwrap();
            let generics = parse_path_segment_generics(first_segment.arguments, 1)?;
            Ok(Self::Option(generics.into_iter().next().unwrap()))
        } else {
            Ok(Self::One(ParamLeafType::Owned(syn::Type::Path(path))))
        }
    }

    fn peek_is_option(path: &syn::TypePath) -> bool {
        path.path.segments.len() == 1
            && path
                .path
                .segments
                .first()
                .map(|first| first.ident == "Option")
                .unwrap_or(false)
    }
}

fn parse_path_segment_generics(
    path_arguments: syn::PathArguments,
    expected_count: usize,
) -> Result<Vec<ParamLeafType>, ParamError> {
    let span = path_arguments.span();

    match path_arguments {
        syn::PathArguments::AngleBracketed(angle_bracketed) => {
            if let Some(_) = angle_bracketed.colon2_token {
                // "turbofish": `::<`
                Err(ParamError::UnparseableType(span))
            } else {
                let arguments: Vec<ParamLeafType> = angle_bracketed
                    .args
                    .into_iter()
                    .map(|arg| ParamLeafType::from_generic_argument(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                if arguments.len() == expected_count {
                    Ok(arguments)
                } else {
                    Err(ParamError::UnparseableType(span))
                }
            }
        }
        _ => Err(ParamError::UnparseableType(span)),
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParamLeafType {
    Ref(syn::Type),
    Owned(syn::Type),
}

impl ParamLeafType {
    fn try_from_type(ty: syn::Type) -> Result<Self, ParamError> {
        match ty {
            syn::Type::Reference(reference) => Ok(Self::Ref(*reference.elem)),
            ty => Ok(Self::Owned(ty)),
        }
    }

    fn from_generic_argument(ga: syn::GenericArgument) -> Result<Self, ParamError> {
        match ga {
            syn::GenericArgument::Type(ty) => Self::try_from_type(ty),
            ga => Err(ParamError::UnparseableType(ga.span())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_owned() {
        assert_eq!(
            ParamRootType::try_from_type(syn::parse_quote! { String }).unwrap(),
            ParamRootType::One(ParamLeafType::Owned(syn::parse_quote! { String }))
        )
    }

    #[test]
    fn parse_ref() {
        assert_eq!(
            ParamRootType::try_from_type(syn::parse_quote! { &str }).unwrap(),
            ParamRootType::One(ParamLeafType::Ref(syn::parse_quote! { str }))
        )
    }

    #[test]
    fn parse_option() {
        assert_eq!(
            ParamRootType::try_from_type(syn::parse_quote! { Option<&str> }).unwrap(),
            ParamRootType::Option(ParamLeafType::Ref(syn::parse_quote! { str }))
        )
    }
}
