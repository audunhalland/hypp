use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};

use crate::markup;
use crate::template;

pub struct Component {
    struct_ident: syn::Ident,
    props_ident: syn::Ident,
    template: template::Template,
}

impl Component {
    pub fn to_token_stream(self, input_fn: syn::ItemFn) -> TokenStream {
        let UpdateFn {
            update_fn,
            props_struct,
        } = self.create_update_fn(input_fn);

        let node_var_params = self.template.vars.iter().map(|var| {
            let ident = &var.field_ident;
            let ty = &var.variable.ty;

            quote! {
                #ident: Var<#ty>,
            }
        });

        let node_field_defs = self.template.node_fields.iter().map(|node_field| {
            let ident = &node_field.ident;
            let ty = &node_field.ty;

            quote! {
                #ident: #ty,
            }
        });

        let static_initializers = self.template.static_initializers;

        let var_struct_params = self.template.vars.iter().map(|var| {
            let ident = &var.field_ident;
            quote! {
                #ident: Var::new(),
            }
        });

        let node_struct_params = self.template.node_fields.iter().map(|node_field| {
            let ident = &node_field.ident;

            quote! {
                #ident,
            }
        });

        let mount_impl = if let Some(root) = self.template.node_fields.first() {
            let root_ident = &root.ident;
            quote! {
                Y::append_child(mount_point.parent, &self.#root_ident.as_node())
            }
        } else {
            quote! {}
        };

        let struct_ident = &self.struct_ident;
        let props_ident = &self.props_ident;

        quote! {
            #props_struct

            pub struct #struct_ident<Y: Yay> {
                #(#node_var_params)*
                #(#node_field_defs)*
            }

            impl<Y: Yay> #struct_ident<Y> {
                pub fn new(y: &Y) -> Result<Self, Error> {
                    #(#static_initializers)*

                    Ok(Self {
                        #(#var_struct_params)*
                        #(#node_struct_params)*
                    })
                }
            }

            impl<Y: Yay> Component<Y> for #struct_ident<Y> {
                type Props = #props_ident;

                fn mount(&self, mount_point: MountPoint<Y>) -> Result<(), Error> {
                    #mount_impl
                }

                #update_fn
            }
        }
    }

    fn create_update_fn(&self, mut update_fn: syn::ItemFn) -> UpdateFn {
        update_fn.sig.ident = syn::parse_quote! { update };

        let mut original_inputs: syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]> =
            syn::parse_quote!();
        std::mem::swap(&mut update_fn.sig.inputs, &mut original_inputs);

        let props_struct = self.create_props_struct(&original_inputs);

        update_fn.sig.inputs.push(syn::parse_quote! {
            &mut self
        });

        update_fn
            .sig
            .inputs
            .push(self.create_props_destructuring(&original_inputs));

        let stmts = &mut update_fn.block.stmts;

        for var in self.template.vars.iter() {
            let ident = &var.variable.ident;
            let field_ident = &var.field_ident;
            let node_ident = &var.node_ident;

            let stmt: syn::Stmt = syn::parse_quote! {
                if let Some(v) = self.#field_ident.update(#ident) {
                    Y::set_text(&self.#node_ident, v);
                }
            };

            stmts.push(stmt);
        }

        UpdateFn {
            update_fn,
            props_struct,
        }
    }

    fn create_props_struct(
        &self,
        inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
    ) -> syn::ItemStruct {
        let props_ident = &self.props_ident;

        let fields = inputs.iter().filter_map(|input| match input {
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => Some(quote! {
                #pat: #ty
            }),
            _ => None,
        });

        syn::parse_quote! {
            pub struct #props_ident {
                #(#fields)*
            }
        }
    }

    fn create_props_destructuring(
        &self,
        inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
    ) -> syn::FnArg {
        let props_ident = &self.props_ident;

        let fields = inputs.iter().filter_map(|input| match input {
            syn::FnArg::Typed(syn::PatType { pat, .. }) => Some(quote! {
                #pat,
            }),
            _ => None,
        });

        syn::parse_quote! {
            #props_ident {
                #(#fields)*
            }: #props_ident
        }
    }
}

impl Parse for Component {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let struct_ident = input.parse::<syn::Ident>()?;
        input.parse::<syn::Token![,]>()?;
        let root = input.parse::<markup::Node>()?;

        let template = template::Template::analyze(root);

        let props_ident = quote::format_ident!("{}Props", struct_ident);

        Ok(Self {
            struct_ident,
            props_ident,
            template,
        })
    }
}

struct UpdateFn {
    update_fn: syn::ItemFn,
    props_struct: syn::ItemStruct,
}
