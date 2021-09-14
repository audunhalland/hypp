/// A parameter to a component
#[derive(Clone)]
pub struct Param {
    pub id: u16,
    pub kind: ParamKind,
    pub ident: syn::Ident,
    pub ty: syn::Type,
}

#[derive(Clone, Copy)]
pub enum ParamKind {
    Prop,
    State,
}
