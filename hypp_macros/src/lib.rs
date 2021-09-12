extern crate proc_macro;

use proc_macro::TokenStream;

mod ast;
mod component;
mod ir;
mod lowering;
mod variable;

#[proc_macro_attribute]
pub fn component(attr: TokenStream, input: TokenStream) -> proc_macro::TokenStream {
    let output = compile_component(
        syn::parse_macro_input!(attr as ast::Node),
        syn::parse_macro_input!(input as syn::ItemFn),
    );

    TokenStream::from(output)
}

#[proc_macro_attribute]
pub fn component_dbg(attr: TokenStream, input: TokenStream) -> TokenStream {
    let output = compile_component(
        syn::parse_macro_input!(attr as ast::Node),
        syn::parse_macro_input!(input as syn::ItemFn),
    );

    println!("// macro output: \n{}", output);

    TokenStream::from(output)
}

fn compile_component(root_node: ast::Node, update_fn: syn::ItemFn) -> proc_macro2::TokenStream {
    let block = lowering::lower_root_node(root_node);
    component::generate_component(block, update_fn)
}

#[proc_macro]
pub fn component2(input: TokenStream) -> proc_macro::TokenStream {
    let output = quote::quote! {
        fn it_works() {
        }
    };

    TokenStream::from(output)
}
