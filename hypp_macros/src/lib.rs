extern crate proc_macro;

use proc_macro::TokenStream;

mod codegen;
mod component;
mod component_ast;
mod component_old;
mod ir;
mod lowering;
mod template_ast;
mod variable;

#[proc_macro]
pub fn component2(input: TokenStream) -> proc_macro::TokenStream {
    let output =
        component::generate_component(syn::parse_macro_input!(input as component_ast::Component));

    TokenStream::from(output)
}

#[proc_macro_attribute]
pub fn component(attr: TokenStream, input: TokenStream) -> proc_macro::TokenStream {
    let output = compile_component_old(
        syn::parse_macro_input!(attr as template_ast::Node),
        syn::parse_macro_input!(input as syn::ItemFn),
    );

    TokenStream::from(output)
}

#[proc_macro_attribute]
pub fn component_dbg(attr: TokenStream, input: TokenStream) -> TokenStream {
    let output = compile_component_old(
        syn::parse_macro_input!(attr as template_ast::Node),
        syn::parse_macro_input!(input as syn::ItemFn),
    );

    println!("// macro output: \n{}", output);

    TokenStream::from(output)
}

fn compile_component_old(
    root_node: template_ast::Node,
    update_fn: syn::ItemFn,
) -> proc_macro2::TokenStream {
    let block = lowering::lower_root_node(root_node);
    component_old::generate_component(block, update_fn)
}
