use proc_macro2::TokenStream;
use quote::quote;

use crate::ir;
use crate::markup;
use crate::template;

pub fn generate_component(template: template::Template, input_fn: syn::ItemFn) -> TokenStream {
    let RootBlock {
        root_idents,
        props_struct,
        variant_enums,
        struct_fields,
        block_stmts:
            BlockStmts {
                props_destructuring,
                constructor_stmts,
                update_stmts,
            },
        fn_stmts,
    } = apply_root_block(template.root_block, input_fn);

    let component_ident = &root_idents.component_ident;
    let props_ident = &root_idents.props_ident;

    let struct_field_defs = struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&root_idents));

    let struct_params = struct_fields
        .iter()
        .map(ir::StructField::struct_param_tokens);

    quote! {
        #props_struct

        #(#variant_enums)*

        pub struct #component_ident<A: Awe> {
            #(#struct_field_defs)*

            __phantom: PhantomField<A>
        }

        impl<A: Awe> #component_ident<A> {
            pub fn new(#props_destructuring, __vm: &mut dyn DomVM<A>) -> Result<Self, Error> {
                #(#fn_stmts)*
                #(#constructor_stmts)*

                Ok(Self {
                    #(#struct_params)*

                    __phantom: std::marker::PhantomData
                })
            }
        }

        impl<'p, A: Awe> Component<'p, A> for #component_ident<A> {
            type Props = #props_ident<'p>;

            fn update(&mut self, #props_destructuring, __vm: &mut dyn DomVM<A>) {
                #(#fn_stmts)*
                #(#update_stmts)*
            }

            fn unmount(&mut self, __vm: &mut dyn DomVM<A>) {
                // TODO: we actually want to panic here if things are not OK?
                panic!("implement unmount")
            }
        }
    }
}

fn apply_root_block(
    ir::Block {
        variable_count: _,
        struct_fields,
        program,
    }: ir::Block,
    input_fn: syn::ItemFn,
) -> RootBlock {
    let ident = input_fn.sig.ident;
    let props_ident = quote::format_ident!("{}Props", ident);

    let root_idents = RootIdents {
        component_ident: ident,
        props_ident,
    };

    let props_struct = create_props_struct(&input_fn.sig.inputs, &root_idents);
    let props_destructuring = create_props_destructuring(&input_fn.sig.inputs, &root_idents);

    let mut variant_enums = vec![];
    collect_variant_enums(&program, &root_idents, &mut variant_enums);

    let constructor_stmts = program
        .iter()
        .map(|opcode| opcode.to_constructor_tokens(&root_idents))
        .collect();

    let update_stmts = program
        .iter()
        .filter_map(|opcode| opcode.to_update_tokens(&root_idents, 0))
        .collect();

    RootBlock {
        variant_enums,
        root_idents,
        props_struct,
        struct_fields,
        block_stmts: BlockStmts {
            props_destructuring,
            constructor_stmts,
            update_stmts,
        },
        fn_stmts: input_fn.block.stmts,
    }
}

fn create_props_struct(
    inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
    root_idents: &RootIdents,
) -> syn::ItemStruct {
    let props_ident = &root_idents.props_ident;

    let fields = inputs.iter().filter_map(|input| match input {
        syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => Some(quote! {
            pub #pat: #ty,
        }),
        _ => None,
    });

    syn::parse_quote! {
        pub struct #props_ident<'p> {
            #(#fields)*

            pub __phantom: PhantomProp<'p>
        }
    }
}

fn create_props_destructuring(
    inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::Token![,]>,
    root_idents: &RootIdents,
) -> syn::FnArg {
    let props_ident = &root_idents.props_ident;

    let fields = inputs.iter().filter_map(|input| match input {
        syn::FnArg::Typed(syn::PatType { pat, .. }) => Some(quote! {
            #pat,
        }),
        _ => None,
    });

    syn::parse_quote! {
        #props_ident {
            #(#fields)*
            ..
        }: #props_ident
    }
}

fn collect_variant_enums(
    program: &[ir::OpCode],
    root_idents: &RootIdents,
    output: &mut Vec<TokenStream>,
) {
    for opcode in program {
        match opcode {
            ir::OpCode::Match {
                enum_type, arms, ..
            } => {
                let enum_ident = enum_type.to_tokens(root_idents);

                let variants = arms.iter().map(|arm| {
                    let ident = &arm.enum_variant_ident;
                    let struct_params = arm
                        .block
                        .struct_fields
                        .iter()
                        .map(ir::StructField::struct_param_tokens);

                    quote! {
                        #ident { #(#struct_params)* },
                    }
                });

                output.push(quote! {
                    enum #enum_ident {
                        #(#variants)*
                    }
                });

                for arm in arms {
                    collect_variant_enums(&arm.block.program, root_idents, output);
                }
            }
            _ => {}
        }
    }
}

struct RootBlock {
    root_idents: RootIdents,
    props_struct: syn::ItemStruct,
    variant_enums: Vec<TokenStream>,
    struct_fields: Vec<ir::StructField>,
    block_stmts: BlockStmts,
    fn_stmts: Vec<syn::Stmt>,
}

struct RootIdents {
    component_ident: syn::Ident,
    props_ident: syn::Ident,
}

struct BlockStmts {
    props_destructuring: syn::FnArg,
    constructor_stmts: Vec<TokenStream>,
    update_stmts: Vec<TokenStream>,
}

impl ir::StructField {
    fn field_def_tokens(&self, root_idents: &RootIdents) -> TokenStream {
        let ident = &self.ident;
        let ty = self.ty.to_tokens(root_idents);

        quote! {
            #ident: #ty,
        }
    }

    fn struct_param_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        quote! {
            #ident,
        }
    }
}

impl ir::OpCode {
    fn to_constructor_tokens(&self, root_idents: &RootIdents) -> TokenStream {
        match self {
            Self::EnterElement { tag_name } => quote! {
                __vm.enter_element(#tag_name)?;
            },
            Self::TextConst { text } => quote! {
                __vm.text(#text)?;
            },
            Self::TextVar {
                node_binding,
                variable_binding,
                expr,
            } => quote! {
                let #node_binding = __vm.text(#expr)?;
                let #variable_binding = Var::new();
            },
            Self::ExitElement => quote! {
                __vm.exit_element()?;
            },
            Self::Component {
                binding,
                path,
                props,
            } => {
                let prop_list = props.iter().map(|(name, value)| match value {
                    markup::AttrValue::ImplicitTrue => quote! {
                        #name: true,
                    },
                    markup::AttrValue::Literal(lit) => quote! {
                        #name: #lit,
                    },
                    markup::AttrValue::Eval(ident) => quote! {
                        #name: #ident,
                    },
                });
                let component_path = &path.type_path;
                let props_path = path.props_path();

                quote! {
                    let #binding = #component_path::new(
                        #props_path {
                            #(#prop_list)*
                            __phantom: std::marker::PhantomData
                        },
                        __vm
                    )?;
                }
            }
            Self::Match {
                binding,
                enum_type,
                expr,
                arms,
            } => {
                let enum_ident = enum_type.to_tokens(root_idents);

                let arms = arms.iter().map(
                    |ir::Arm {
                         enum_variant_ident,
                         pattern,
                         block,
                     }| {
                        let program_tokens = block
                            .program
                            .iter()
                            .map(|opcode| opcode.to_constructor_tokens(&root_idents));
                        let struct_params = block
                            .struct_fields
                            .iter()
                            .map(ir::StructField::struct_param_tokens);

                        quote! {
                            #pattern => {
                                #(#program_tokens)*

                                #enum_ident::#enum_variant_ident {
                                    #(#struct_params)*
                                }
                            },
                        }
                    },
                );

                quote! {
                    let #binding = match #expr {
                        #(#arms)*
                    };
                }
            }
        }
    }

    fn to_update_tokens(&self, root_idents: &RootIdents, nesting: usize) -> Option<TokenStream> {
        match self {
            Self::TextVar {
                node_binding,
                variable_binding,
                expr,
            } => Some(quote! {
                if let Some(v) = self.#variable_binding.update(#expr) {
                    A::set_text(&self.#node_binding, v);
                }
            }),
            Self::Component {
                binding,
                path,
                props,
            } => {
                let prop_list = props.iter().map(|(name, value)| match value {
                    markup::AttrValue::ImplicitTrue => quote! {
                        #name: true,
                    },
                    markup::AttrValue::Literal(lit) => quote! {
                        #name: #lit,
                    },
                    markup::AttrValue::Eval(ident) => quote! {
                        #name: #ident,
                    },
                });
                let component_path = &path.type_path;
                let props_path = path.props_path();

                let field_path = if nesting == 0 {
                    quote! { self.#binding }
                } else {
                    quote! { #binding }
                };

                Some(quote! {
                    #field_path.update(#props_path {
                        #(#prop_list)*
                        __phantom: std::marker::PhantomData,
                    }, __vm);
                })
            }
            Self::Match { .. } => None,
            _ => None,
        }
    }
}

impl ir::StructFieldType {
    fn to_tokens(&self, root_idents: &RootIdents) -> TokenStream {
        match self {
            Self::DomText => quote! { A::Text },
            Self::Component(path) => {
                let type_path = &path.type_path;
                quote! { #type_path<A> }
            }
            Self::Enum(enum_index) => {
                let ident =
                    quote::format_ident!("{}Enum{}", root_idents.component_ident, enum_index);
                quote! { #ident }
            }
            Self::Variable(ty) => {
                quote! { Var<#ty> }
            }
        }
    }
}
