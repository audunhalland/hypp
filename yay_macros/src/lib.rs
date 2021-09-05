extern crate proc_macro;

use proc_macro::TokenStream;

mod component;
mod markup;
mod template;
mod variable;

#[proc_macro_attribute]
pub fn component(attr: TokenStream, input: TokenStream) -> TokenStream {
    let markup = syn::parse_macro_input!(attr as markup::Node);
    let update_fn = syn::parse_macro_input!(input as syn::ItemFn);

    let template = template::Template::analyze(markup);
    let output = component::generate_component(template, update_fn);

    TokenStream::from(output)
}

#[proc_macro_attribute]
pub fn component_dbg(attr: TokenStream, input: TokenStream) -> TokenStream {
    let markup = syn::parse_macro_input!(attr as markup::Node);
    let update_fn = syn::parse_macro_input!(input as syn::ItemFn);

    let template = template::Template::analyze(markup);
    let output = component::generate_component(template, update_fn);

    println!("// macro output: \n{}", output);

    TokenStream::from(output)
}
