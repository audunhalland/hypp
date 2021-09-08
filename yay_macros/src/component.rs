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
        props_destructuring,
        fn_stmts,
        program,
    } = apply_root_block(template.root_block, input_fn);

    let component_ident = &root_idents.component_ident;
    let props_ident = &root_idents.props_ident;

    let struct_field_defs = struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&root_idents));

    let struct_params = struct_fields
        .iter()
        .map(ir::StructField::struct_param_tokens);

    let mount_stmts = program
        .iter()
        .map(|opcode| opcode.mount_stmts(&root_idents, Lifecycle::Mount));

    let update_stmts = program
        .iter()
        .map(|opcode| opcode.update_stmts(&root_idents, 0));

    let unmount_stmts = unmount_stmts(&program);

    quote! {
        #props_struct

        #(#variant_enums)*

        pub struct #component_ident<A: Awe> {
            #(#struct_field_defs)*

            __phantom: PhantomField<A>
        }

        impl<A: Awe> #component_ident<A> {
            pub fn mount(#props_destructuring, __vm: &mut dyn DomVM<A>) -> Result<Self, Error> {
                #(#fn_stmts)*
                #(#mount_stmts)*

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
                #unmount_stmts
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

    RootBlock {
        variant_enums,
        root_idents,
        props_struct,
        struct_fields,
        props_destructuring,
        fn_stmts: input_fn.block.stmts,
        program,
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

                let variant_defs = arms.iter().map(|arm| {
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

                let unmount_arms = arms.iter().map(
                    |ir::Arm {
                         enum_variant_ident,
                         block,
                         ..
                     }| {
                        let fields = block.struct_fields.iter().map(|field| {
                            let ident = &field.ident;

                            quote! {
                                #ident,
                            }
                        });
                        let unmount_stmts = unmount_stmts(&block.program);

                        quote! {
                            Self::#enum_variant_ident { #(#fields)* } => {
                                #unmount_stmts
                            },
                        }
                    },
                );

                output.push(quote! {
                    enum #enum_ident {
                        #(#variant_defs)*
                    }

                    impl #enum_ident {
                        pub fn unmount<A: Awe>(&mut self, __vm: &mut dyn DomVM<A>) {
                            match self {
                                #(#unmount_arms)*
                            }
                        }
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
    props_destructuring: syn::FnArg,
    fn_stmts: Vec<syn::Stmt>,
    program: Vec<ir::OpCode>,
}

struct RootIdents {
    component_ident: syn::Ident,
    props_ident: syn::Ident,
}

enum Lifecycle {
    Mount,
    Update,
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
    /// Map an opcode into the rust statements used when mounting
    fn mount_stmts(&self, root_idents: &RootIdents, lifecycle: Lifecycle) -> TokenStream {
        // Mount is error-tolerant:
        let err_handler = match lifecycle {
            Lifecycle::Mount => quote! { ? },
            Lifecycle::Update => quote! { .unwrap() },
        };

        match self {
            Self::EnterElement { tag_name } => quote! {
                __vm.enter_element(#tag_name)#err_handler;
            },
            Self::TextConst { text } => quote! {
                __vm.text(#text)#err_handler;
            },
            Self::TextVar {
                node_binding,
                variable_binding,
                expr,
            } => quote! {
                let #node_binding = __vm.text(#expr)#err_handler;
                let #variable_binding = Var::new();
            },
            Self::ExitElement => quote! {
                __vm.exit_element()#err_handler;
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
                    let #binding = #component_path::mount(
                        #props_path {
                            #(#prop_list)*
                            __phantom: std::marker::PhantomData
                        },
                        __vm
                    )#err_handler;
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
                        let mount_stmts = block
                            .program
                            .iter()
                            .map(|opcode| opcode.mount_stmts(&root_idents, Lifecycle::Mount));
                        let struct_params = block
                            .struct_fields
                            .iter()
                            .map(ir::StructField::struct_param_tokens);

                        quote! {
                            #pattern => {
                                #(#mount_stmts)*

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

    /// Map an opcode into the rust tokens used when updating
    fn update_stmts(&self, root_idents: &RootIdents, nesting: usize) -> TokenStream {
        match self {
            Self::TextVar {
                node_binding,
                variable_binding,
                expr,
            } => quote! {
                if let Some(v) = self.#variable_binding.update(#expr) {
                    A::set_text(&self.#node_binding, v);
                }
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
                let props_path = path.props_path();

                let field_path = if nesting == 0 {
                    quote! { self.#binding }
                } else {
                    quote! { #binding }
                };

                quote! {
                    #field_path.update(#props_path {
                        #(#prop_list)*
                        __phantom: std::marker::PhantomData,
                    }, __vm);
                }
            }
            Self::Match {
                binding,
                enum_type,
                expr,
                arms,
            } => {
                // The match arms generated here should be twice as many as in the mounter.
                // We match a tuple of two things: the existing value and the new value.
                // The first match arms are for when the existing value matches the old value.
                // The rest of the matches are for mismatches, one for each new value.

                let enum_ident = enum_type.to_tokens(root_idents);

                let matching_arms = arms.iter().map(
                    |ir::Arm {
                         enum_variant_ident,
                         pattern,
                         block,
                     }| {
                        let fields = block.struct_fields.iter().map(|field| {
                            let ident = &field.ident;

                            quote! {
                                #ident,
                            }
                        });

                        let update_stmts = block
                            .program
                            .iter()
                            .map(|opcode| opcode.update_stmts(root_idents, nesting + 1));

                        quote! {
                           (#enum_ident::#enum_variant_ident { #(#fields)* }, #pattern) => {
                               #(#update_stmts)*
                           },
                        }
                    },
                );

                let nonmatching_arms = arms.iter().map(
                    |ir::Arm {
                         enum_variant_ident,
                         pattern,
                         block,
                     }| {
                        // BUG: Code duplication with constructor.
                        // These are constant and could be moved to separate function?
                        // But the problem then will be to formalize another param struct
                        // to pass into that function.. :(
                        // But it's better to have less runtime code.
                        let mount_stmts = block
                            .program
                            .iter()
                            .map(|opcode| opcode.mount_stmts(&root_idents, Lifecycle::Update));
                        let struct_params = block
                            .struct_fields
                            .iter()
                            .map(ir::StructField::struct_param_tokens);

                        quote! {
                            (_, #pattern) => {
                                self.#binding.unmount(__vm);
                                #(#mount_stmts)*

                                self.#binding = #enum_ident::#enum_variant_ident {
                                    #(#struct_params)*
                                }
                           },
                        }
                    },
                );

                quote! {
                    match (&mut self.#binding, #expr) {
                        #(#matching_arms)*
                        #(#nonmatching_arms)*

                        _ => {}
                    }
                }
            }
            _ => TokenStream::new(),
        }
    }
}

///
/// Generate the rust program required to appropriately unmount
///
fn unmount_stmts(program: &[ir::OpCode]) -> TokenStream {
    let mut element_level = 0;

    let mut unmount_calls = vec![];
    let mut node_removals = vec![];

    for opcode in program {
        match opcode {
            ir::OpCode::EnterElement { tag_name } => {
                if element_level == 0 {
                    node_removals.push(quote! {
                        __vm.remove_element(#tag_name).unwrap();
                    });
                }
                element_level += 1;
            }
            ir::OpCode::ExitElement => {
                element_level -= 1;
            }
            ir::OpCode::TextConst { .. } | ir::OpCode::TextVar { .. } => {
                if element_level == 0 {
                    node_removals.push(quote! {
                        __vm.remove_text().unwrap();
                    });
                }
            }
            ir::OpCode::Component { binding, .. } => {
                unmount_calls.push(quote! {
                    self.#binding.unmount(__vm);
                });
            }
            ir::OpCode::Match { binding, .. } => {
                unmount_calls.push(quote! {
                    self.#binding.unmount(__vm);
                });
            }
        }
    }

    quote! {
        #(#unmount_calls)*
        #(#node_removals)*
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
