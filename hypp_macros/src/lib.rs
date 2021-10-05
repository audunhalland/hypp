#![forbid(unsafe_code)]

extern crate proc_macro;

use proc_macro::TokenStream;

mod component;
mod component_ast;
mod flow;
mod gen_component;
mod gen_misc;
mod gen_patch;
mod ir;
mod lowering;
mod name;
mod namespace;
mod param;
mod template_ast;

///
/// HTML component.
///
#[proc_macro]
pub fn component(input: TokenStream) -> proc_macro::TokenStream {
    let output =
        gen_component::generate_component(syn::parse_macro_input!(input as component::Component));

    TokenStream::from(output)
}

///
/// HTML component, with a debug-printout of the generated code.
///
#[proc_macro]
pub fn component_dbg(input: TokenStream) -> proc_macro::TokenStream {
    let output =
        gen_component::generate_component(syn::parse_macro_input!(input as component::Component));

    println!("// macro output: \n{}", output);

    TokenStream::from(output)
}
