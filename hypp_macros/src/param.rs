/// A parameter to a component
#[derive(Clone)]
pub struct Param {
    pub id: u16,
    pub kind: ParamKind,
    pub ident: syn::Ident,
    pub ty: syn::Type,
}

impl Param {
    pub fn is_reference(&self) -> bool {
        match &self.ty {
            syn::Type::Reference(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy)]
pub enum ParamKind {
    Prop,
    State,
}
