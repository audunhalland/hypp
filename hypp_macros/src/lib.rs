extern crate proc_macro;

use proc_macro::TokenStream;

mod codegen;
mod component;
mod component_ast;
mod ir;
mod lowering;
mod template_ast;
mod variable;

#[proc_macro]
pub fn component(input: TokenStream) -> proc_macro::TokenStream {
    let output =
        component::generate_component(syn::parse_macro_input!(input as component_ast::Component));

    TokenStream::from(output)
}

#[proc_macro]
pub fn component_dbg(input: TokenStream) -> proc_macro::TokenStream {
    let output =
        component::generate_component(syn::parse_macro_input!(input as component_ast::Component));

    println!("// macro output: \n{}", output);

    TokenStream::from(output)
}
