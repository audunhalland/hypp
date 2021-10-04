use syn::spanned::Spanned;

#[derive(Debug)]
pub enum ParamError {
    UnparseableType(proc_macro2::Span),
}

pub struct IsProp(pub bool);

pub type Triple = (ParamKind, Quantifier, Ty);

/// A parameter to a component
#[derive(Clone)]
pub struct Param {
    pub id: u16,
    pub ident: syn::Ident,
    pub triple: Triple,
}

impl Param {
    pub fn is_prop(&self) -> bool {
        match &self.triple.0 {
            ParamKind::Prop => true,
            ParamKind::State => false,
        }
    }
}

#[derive(Clone, Copy)]
pub enum ParamKind {
    Prop,
    State,
}

#[derive(Clone)]
pub enum Quantifier {
    Unit,
    Option(syn::Ident),
}

#[derive(Clone)]
pub enum Ty {
    Reference(syn::TypeReference),
    Owned(syn::Type),
}

pub struct ParamParser {
    kind: ParamKind,
    prop_lifetime: syn::Lifetime,
}

impl ParamParser {
    pub fn new(kind: ParamKind) -> Self {
        Self {
            kind,
            prop_lifetime: syn::parse_quote! { 'a },
        }
    }

    pub fn parse_triple(&self, ty: syn::Type) -> Result<Triple, ParamError> {
        match ty {
            syn::Type::Slice(_) => {
                unimplemented!()
                // Ok(Self::Slice(self.parse_leaf(*slice.elem)?)),
            }
            syn::Type::Path(path) => self.parse_path(path),
            ty => Ok((self.kind, Quantifier::Unit, self.parse_ty(ty)?)),
        }
    }

    fn parse_path(&self, path: syn::TypePath) -> Result<Triple, ParamError> {
        if let Some(_) = &path.qself {
            Ok((
                self.kind,
                Quantifier::Unit,
                self.parse_ty(syn::Type::Path(path))?,
            ))
        } else if path.path.segments.len() > 1 {
            Ok((
                self.kind,
                Quantifier::Unit,
                self.parse_ty(syn::Type::Path(path))?,
            ))
        } else if path.path.segments.is_empty() {
            Err(ParamError::UnparseableType(path.span()))
        } else if Self::peek_is_option(&path) {
            let first_segment = path.path.segments.into_iter().next().unwrap();
            let generics = self.parse_path_segment_generics(first_segment.arguments, 1)?;
            Ok((
                self.kind,
                Quantifier::Option(first_segment.ident),
                generics.into_iter().next().unwrap(),
            ))
        } else {
            Ok((
                self.kind,
                Quantifier::Unit,
                Ty::Owned(syn::Type::Path(path)),
            ))
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

    fn parse_ty(&self, ty: syn::Type) -> Result<Ty, ParamError> {
        match ty {
            syn::Type::Reference(mut reference) => {
                match &self.kind {
                    ParamKind::Prop => {
                        reference.lifetime = Some(self.prop_lifetime.clone());
                    }
                    _ => {}
                }
                Ok(Ty::Reference(reference))
            }
            ty => Ok(Ty::Owned(ty)),
        }
    }

    fn parse_leaf_from_generic_argument(&self, ga: syn::GenericArgument) -> Result<Ty, ParamError> {
        match ga {
            syn::GenericArgument::Type(ty) => self.parse_ty(ty),
            ga => Err(ParamError::UnparseableType(ga.span())),
        }
    }

    fn parse_path_segment_generics(
        &self,
        path_arguments: syn::PathArguments,
        expected_count: usize,
    ) -> Result<Vec<Ty>, ParamError> {
        let span = path_arguments.span();

        match path_arguments {
            syn::PathArguments::AngleBracketed(angle_bracketed) => {
                if let Some(_) = angle_bracketed.colon2_token {
                    // "turbofish": `::<`
                    Err(ParamError::UnparseableType(span))
                } else {
                    let arguments: Vec<Ty> = angle_bracketed
                        .args
                        .into_iter()
                        .map(|arg| self.parse_leaf_from_generic_argument(arg))
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_owned() {
        assert_eq!(
            ParamParser::new(IsProp(true))
                .parse(syn::parse_quote! { String })
                .unwrap(),
            ParamRootType::One(ParamLeafType::Owned(syn::parse_quote! { String }))
        )
    }

    #[test]
    fn parse_ref() {
        assert_eq!(
            ParamParser::new(IsProp(true))
                .parse(syn::parse_quote! { &str })
                .unwrap(),
            ParamRootType::One(ParamLeafType::Ref(syn::parse_quote! { str }))
        )
    }

    #[test]
    fn parse_option() {
        assert_eq!(
            ParamParser::new(IsProp(true))
                .parse(syn::parse_quote! { Option<&str> })
                .unwrap(),
            ParamRootType::Option(ParamLeafType::Ref(syn::parse_quote! { str }))
        )
    }
}
