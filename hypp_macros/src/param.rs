#[derive(Clone, Copy)]
pub enum ParamKind {
    Prop,
    State,
}

/// A parameter to a component
#[derive(Clone)]
pub struct Param {
    pub kind: ParamKind,
    pub ident: syn::Ident,
    pub ty: syn::Type,
}
