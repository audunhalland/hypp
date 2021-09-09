use proc_macro2::TokenStream;
use quote::quote;

use crate::ast;
use crate::ir;

pub fn generate_component(root_block: ir::Block, input_fn: syn::ItemFn) -> TokenStream {
    let RootBlock {
        root_idents,
        props_struct,
        variant_enums,
        struct_fields,
        props_destructuring,
        fn_stmts,
        program,
    } = process_root_block(root_block, input_fn);

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
        .map(|opcode| opcode.update_stmts(&root_idents, Scope::Component));

    let unmount_stmts = unmount_stmts(&program, Scope::Component);

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

fn process_root_block(
    ir::Block {
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
                    let struct_field_defs = arm
                        .block
                        .struct_fields
                        .iter()
                        .map(|field| field.field_def_tokens(root_idents));

                    quote! {
                        #ident { #(#struct_field_defs)* },
                    }
                });

                let unmount_arms = arms.iter().map(
                    |ir::Arm {
                         enum_variant_ident,
                         block,
                         ..
                     }| {
                        let fields = block
                            .struct_fields
                            .iter()
                            .map(ir::StructField::struct_param_tokens);
                        let unmount_stmts = unmount_stmts(&block.program, Scope::Enum);

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

#[derive(Copy, Clone)]
enum Lifecycle {
    Mount,
    Update,
}

#[derive(Copy, Clone)]
enum Scope {
    Component,
    Enum,
}

impl ir::StructField {
    fn field_def_tokens(&self, root_idents: &RootIdents) -> TokenStream {
        let field = &self.field;
        let ty = self.ty.to_tokens(root_idents);

        quote! { #field: #ty, }
    }

    fn struct_param_tokens(&self) -> TokenStream {
        let field = &self.field;

        quote! { #field, }
    }

    fn mut_pattern_tokens(&self) -> TokenStream {
        let field = &self.field;

        quote! { ref mut #field, }
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
            Self::EnterElement { field, tag_name } => quote! {
                let #field = __vm.enter_element(#tag_name)#err_handler;
            },
            Self::TextConst { text } => quote! {
                __vm.text(#text)#err_handler;
            },
            Self::TextVar {
                node_field,
                variable_field,
                expr,
            } => {
                quote! {
                    let #node_field = __vm.text(#expr)#err_handler;
                    let #variable_field = Var::new();
                }
            }
            Self::ExitElement => quote! {
                __vm.exit_element()#err_handler;
            },
            Self::Component {
                parent,
                field,
                path,
                props,
            } => {
                let prop_list = props.iter().map(|(name, value)| match value {
                    ast::AttrValue::ImplicitTrue => quote! {
                        #name: true,
                    },
                    ast::AttrValue::Literal(lit) => quote! {
                        #name: #lit,
                    },
                    ast::AttrValue::Eval(ident) => quote! {
                        #name: #ident,
                    },
                });
                let component_path = &path.type_path;
                let props_path = path.props_path();

                quote! {
                    let #field = #component_path::mount(
                        #props_path {
                            #(#prop_list)*
                            __phantom: std::marker::PhantomData
                        },
                        __vm
                    )#err_handler;
                }
            }
            Self::Match {
                parent,
                field,
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
                            .map(|opcode| opcode.mount_stmts(&root_idents, lifecycle));
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
                    let #field = match #expr {
                        #(#arms)*
                    };
                }
            }
        }
    }

    /// Map an opcode into the rust tokens used when updating
    fn update_stmts(&self, root_idents: &RootIdents, scope: Scope) -> TokenStream {
        match self {
            Self::TextVar {
                node_field,
                variable_field,
                expr,
            } => quote! {
                if let Some(v) = self.#variable_field.update(#expr) {
                    A::set_text(&self.#node_field, v);
                }
            },
            Self::Component {
                parent,
                field,
                path,
                props,
            } => {
                let prop_list = props.iter().map(|(name, value)| match value {
                    ast::AttrValue::ImplicitTrue => quote! {
                        #name: true,
                    },
                    ast::AttrValue::Literal(lit) => quote! {
                        #name: #lit,
                    },
                    ast::AttrValue::Eval(ident) => quote! {
                        #name: #ident,
                    },
                });
                let props_path = path.props_path();

                let field_ref = FieldRef(*field, scope);

                quote! {
                    #field_ref.update(#props_path {
                        #(#prop_list)*
                        __phantom: std::marker::PhantomData,
                    }, __vm);
                }
            }
            Self::Match {
                parent,
                field,
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
                        let fields = block
                            .struct_fields
                            .iter()
                            .map(ir::StructField::mut_pattern_tokens);

                        let update_stmts = block
                            .program
                            .iter()
                            .map(|opcode| opcode.update_stmts(root_idents, Scope::Enum));

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

                        let field_ref = FieldRef(*field, scope);
                        let field_assign = FieldAssign(*field, scope);

                        quote! {
                            (_, #pattern) => {
                                #field_ref.unmount(__vm);
                                #(#mount_stmts)*

                                #field_assign = #enum_ident::#enum_variant_ident {
                                    #(#struct_params)*
                                };
                           },
                        }
                    },
                );

                let mut_field_ref = MutFieldRef(*field, scope);
                let push = PushElementContext(*parent, scope);
                let pop = PopElementContext(*parent, scope);

                quote! {
                    #push
                    match (#mut_field_ref, #expr) {
                        #(#matching_arms)*
                        #(#nonmatching_arms)*

                        _ => {}
                    }
                    #pop
                }
            }
            _ => TokenStream::new(),
        }
    }
}

///
/// Generate the rust program required to appropriately unmount
///
fn unmount_stmts(program: &[ir::OpCode], scope: Scope) -> TokenStream {
    let mut element_level = 0;

    let mut stmts = vec![];

    for opcode in program {
        match opcode {
            ir::OpCode::EnterElement { tag_name, .. } => {
                if element_level == 0 {
                    stmts.push(quote! {
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
                    stmts.push(quote! {
                        __vm.remove_text().unwrap();
                    });
                }
            }
            ir::OpCode::Component { field, .. } => {
                let field_ref = FieldRef(*field, scope);
                stmts.push(quote! {
                    #field_ref.unmount(__vm);
                });
            }
            ir::OpCode::Match { field, .. } => {
                let field_ref = FieldRef(*field, scope);
                stmts.push(quote! {
                    #field_ref.unmount(__vm);
                });
            }
        }
    }

    quote! {
        #(#stmts)*
    }
}

impl ir::StructFieldType {
    fn to_tokens(&self, root_idents: &RootIdents) -> TokenStream {
        match self {
            Self::DomElement => quote! { A::Element },
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

struct PushElementContext(Option<ir::FieldId>, Scope);

impl quote::ToTokens for PushElementContext {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if let Some(parent) = &self.0 {
            let parent = FieldRef(*parent, self.1);
            tokens.extend(quote! {
                __vm.push_element_context(#parent.clone());
            });
        }
    }
}

struct PopElementContext(Option<ir::FieldId>, Scope);

impl quote::ToTokens for PopElementContext {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if let Some(parent) = &self.0 {
            tokens.extend(quote! {
                __vm.pop_element_context();
            });
        }
    }
}

/// A field we want to reference (read)
struct FieldRef(ir::FieldId, Scope);

impl quote::ToTokens for FieldRef {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1 {
            Scope::Component => quote! { self.#field },
            Scope::Enum => quote! { #field },
        });
    }
}

struct MutFieldRef(ir::FieldId, Scope);

impl quote::ToTokens for MutFieldRef {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1 {
            Scope::Component => quote! { &mut self.#field },
            // Field has already been marked as a `ref mut` in the pattern binding:
            Scope::Enum => quote! { &#field },
        });
    }
}

/// A field we want to assign to (mut)
struct FieldAssign(ir::FieldId, Scope);

impl quote::ToTokens for FieldAssign {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1 {
            Scope::Component => quote! { self.#field },
            Scope::Enum => quote! { *#field },
        });
    }
}
