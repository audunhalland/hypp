#[derive(Debug, Eq, PartialEq)]
pub struct Variable {
    pub ident: syn::Ident,
    pub ty: syn::Type,
}

impl Variable {
    pub fn with_ident(ident: syn::Ident) -> Self {
        Self {
            ident,
            ty: syn::parse_quote! {
                &'static str
            },
        }
    }
}
