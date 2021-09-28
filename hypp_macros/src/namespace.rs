use proc_macro2::TokenStream;
use quote::quote;

pub enum Namespace {
    Html,
}

// Which direction lowering will lower children,
// affecting the generated code.
// It will be constant for a specific implementation of Hypp.
#[allow(unused)]
pub enum TraversalDirection {
    FirstToLast,
    LastToFirst,
}

impl Namespace {
    pub fn traversal_direction(&self) -> TraversalDirection {
        match self {
            Self::Html => TraversalDirection::LastToFirst,
        }
    }

    pub fn hypp_ns(&self) -> TokenStream {
        quote! {
            ::hypp::ns::Html
        }
    }
}
