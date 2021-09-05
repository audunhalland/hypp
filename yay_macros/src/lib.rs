extern crate proc_macro;

use proc_macro::TokenStream;

mod component;
mod markup;
mod template;
mod variable;

#[proc_macro_attribute]
pub fn component(attr: TokenStream, input: TokenStream) -> TokenStream {
    let component = syn::parse_macro_input!(attr as component::Component);
    let update_fn = syn::parse_macro_input!(input as syn::ItemFn);

    let output = component.to_token_stream(update_fn);

    println!("macro output: {}", output);

    TokenStream::from(output)
}
