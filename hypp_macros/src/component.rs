use proc_macro2::TokenStream;
use quote::quote;

use crate::ast;
use crate::ir;

#[derive(Copy, Clone)]
enum Lifecycle {
    Mount,
    Patch,
    Unmount,
}

#[derive(Copy, Clone)]
enum Scope {
    Component,
    Enum,
}

#[derive(Clone, Copy)]
struct CodegenCtx {
    lifecycle: Lifecycle,
    scope: Scope,
}

pub fn generate_component(root_block: ir::Block, input_fn: syn::ItemFn) -> TokenStream {
    let Component {
        dom_programs,
        root_idents,
        props_struct,
        variant_enums,
        struct_fields,
        props_destructuring,
        fn_stmts,
        statements,
    } = analyze_component(root_block, input_fn);

    let component_ident = &root_idents.component_ident;
    let props_ident = &root_idents.props_ident;

    let struct_field_defs = struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&root_idents, Scope::Component));

    let struct_params = struct_fields
        .iter()
        .map(ir::StructField::struct_param_tokens);

    let mount_stmts = statements.iter().map(|statement| {
        statement.gen_mount(
            &root_idents,
            CodegenCtx {
                lifecycle: Lifecycle::Mount,
                scope: Scope::Component,
            },
        )
    });

    let patch_stmts = statements.iter().map(|statement| {
        statement.gen_patch(
            &root_idents,
            CodegenCtx {
                lifecycle: Lifecycle::Patch,
                scope: Scope::Component,
            },
        )
    });

    let unmount_stmts = gen_unmount(
        &statements,
        CodegenCtx {
            lifecycle: Lifecycle::Unmount,
            scope: Scope::Component,
        },
    );

    quote! {
        #props_struct

        #(#dom_programs)*

        #(#variant_enums)*

        pub struct #component_ident<H: Hypp> {
            #(#struct_field_defs)*

            __phantom: PhantomField<H>
        }

        impl<H: Hypp> #component_ident<H> {
            pub fn mount(#props_destructuring, __vm: &mut dyn DomVM<H>) -> Result<Self, Error> {
                #(#fn_stmts)*
                #(#mount_stmts)*

                Ok(Self {
                    #(#struct_params)*

                    __phantom: std::marker::PhantomData
                })
            }
        }

        impl<'p, H: Hypp> Component<'p, H> for #component_ident<H> {
            type Props = #props_ident<'p>;

            fn patch(&mut self, #props_destructuring, __vm: &mut dyn DomVM<H>) {
                #(#fn_stmts)*
                #(#patch_stmts)*
            }

            fn unmount(&mut self, __vm: &mut dyn DomVM<H>) {
                #unmount_stmts
            }
        }
    }
}

fn analyze_component(
    ir::Block {
        struct_fields,
        statements,
    }: ir::Block,
    input_fn: syn::ItemFn,
) -> Component {
    let ident = input_fn.sig.ident;
    let props_ident = quote::format_ident!("{}Props", ident);
    let uppercase_prefix = ident.clone().to_string().to_uppercase();

    let root_idents = RootIdents {
        component_ident: ident,
        props_ident,
        uppercase_prefix,
    };

    let props_struct = create_props_struct(&input_fn.sig.inputs, &root_idents);
    let props_destructuring = create_props_destructuring(&input_fn.sig.inputs, &root_idents);

    let mut dom_programs = vec![];
    collect_dom_programs(&statements, &root_idents, &mut dom_programs);

    let mut variant_enums = vec![];
    collect_variant_enums(&statements, &root_idents, &mut variant_enums);

    Component {
        dom_programs,
        variant_enums,
        root_idents,
        props_struct,
        struct_fields,
        props_destructuring,
        fn_stmts: input_fn.block.stmts,
        statements,
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

fn collect_dom_programs(
    statements: &[ir::Statement],
    root_idents: &RootIdents,
    output: &mut Vec<TokenStream>,
) {
    for statement in statements {
        match &statement.expression {
            ir::Expression::ConstDom(program) => {
                output.push(generate_dom_program(program, root_idents));
            }
            ir::Expression::Match { arms, .. } => {
                for arm in arms {
                    collect_dom_programs(&arm.block.statements, root_idents, output);
                }
            }
            _ => {}
        }
    }
}

fn generate_dom_program(program: &ir::ConstDomProgram, root_idents: &RootIdents) -> TokenStream {
    let opcodes = program.opcodes.iter().map(|opcode| match opcode {
        ir::DomOpCode::EnterElement(lit_str) => quote! {
            ConstOpCode::EnterElement(#lit_str),
        },
        ir::DomOpCode::Text(lit_str) => quote! {
            ConstOpCode::Text(#lit_str),
        },
        ir::DomOpCode::ExitElement => quote! {
            ConstOpCode::ExitElement,
        },
    });

    let len = program.opcodes.len();
    let ident = program.get_ident(root_idents);

    quote! {
        static #ident: [ConstOpCode; #len] = [
            #(#opcodes)*
        ];
    }
}

fn collect_variant_enums(
    statements: &[ir::Statement],
    root_idents: &RootIdents,
    output: &mut Vec<TokenStream>,
) {
    for statement in statements {
        match &statement.expression {
            ir::Expression::Match {
                enum_type, arms, ..
            } => {
                output.push(generate_variant_enum(enum_type, arms, root_idents));
                for arm in arms {
                    collect_variant_enums(&arm.block.statements, root_idents, output);
                }
            }
            _ => {}
        }
    }
}

fn generate_variant_enum(
    enum_type: &ir::StructFieldType,
    arms: &[ir::Arm],
    root_idents: &RootIdents,
) -> TokenStream {
    let enum_ident = enum_type.to_tokens(root_idents, Scope::Enum, WithGenerics(false));

    let variant_defs = arms.iter().map(|arm| {
        let ident = &arm.enum_variant_ident;
        let struct_field_defs = arm
            .block
            .struct_fields
            .iter()
            .map(|field| field.field_def_tokens(root_idents, Scope::Enum));

        quote! {
            #ident {
                #(#struct_field_defs)*

                __phantom: PhantomField<H>
            },
        }
    });

    let unmount_arms = arms.iter().map(
        |ir::Arm {
             enum_variant_ident,
             block,
             ..
         }| {
            let field_defs = block
                .struct_fields
                .iter()
                .map(ir::StructField::struct_param_tokens);
            let unmount_stmts = gen_unmount(
                &block.statements,
                CodegenCtx {
                    lifecycle: Lifecycle::Unmount,
                    scope: Scope::Enum,
                },
            );

            quote! {
                Self::#enum_variant_ident { #(#field_defs)* .. } => {
                    #unmount_stmts
                },
            }
        },
    );

    quote! {
        enum #enum_ident<H: Hypp> {
            #(#variant_defs)*
        }

        impl<H: Hypp> #enum_ident<H> {
            pub fn unmount(&mut self, __vm: &mut dyn DomVM<H>) {
                match self {
                    #(#unmount_arms)*
                }
            }
        }
    }
}

struct Component {
    dom_programs: Vec<TokenStream>,
    root_idents: RootIdents,
    props_struct: syn::ItemStruct,
    variant_enums: Vec<TokenStream>,
    struct_fields: Vec<ir::StructField>,
    props_destructuring: syn::FnArg,
    fn_stmts: Vec<syn::Stmt>,
    statements: Vec<ir::Statement>,
}

struct RootIdents {
    component_ident: syn::Ident,
    props_ident: syn::Ident,
    uppercase_prefix: String,
}

impl ir::StructField {
    fn field_def_tokens(&self, root_idents: &RootIdents, scope: Scope) -> TokenStream {
        let field = &self.field;
        let ty = self.ty.to_tokens(root_idents, scope, WithGenerics(true));

        quote! { #field: #ty, }
    }

    fn struct_param_tokens(&self) -> TokenStream {
        let field = &self.field;

        quote! { #field, }
    }

    fn mut_pattern_tokens(&self) -> TokenStream {
        let field = &self.field;

        match &self.ty {
            // mutable types
            ir::StructFieldType::Component(_)
            | ir::StructFieldType::Enum(_)
            | ir::StructFieldType::Variable(_) => quote! {
                ref mut #field,
            },
            // immutable types
            ir::StructFieldType::DomElement | ir::StructFieldType::DomText => quote! {
                ref #field,
            },
        }
    }
}

impl ir::Statement {
    /// Rust statement used when mounting
    fn gen_mount(&self, root_idents: &RootIdents, ctx: CodegenCtx) -> TokenStream {
        // Mount is error-tolerant:
        let err_handler = match &ctx.lifecycle {
            Lifecycle::Mount => quote! { ? },
            Lifecycle::Patch | Lifecycle::Unmount => quote! { .unwrap() },
        };

        let expr = match &self.expression {
            ir::Expression::ConstDom(program) => {
                let program_ident = program.get_ident(root_idents);

                match program.opcodes.last().unwrap() {
                    ir::DomOpCode::EnterElement(_) | ir::DomOpCode::ExitElement => quote! {
                        __vm.const_exec_element(&#program_ident) #err_handler
                    },
                    ir::DomOpCode::Text(_) => quote! {
                        __vm.const_exec_text(&#program_ident) #err_handler
                    },
                }
            }
            ir::Expression::VariableText { expr, .. } => quote! {
                __vm.text(#expr) #err_handler
            },
            ir::Expression::LocalVar => quote! {
                Var::new()
            },
            ir::Expression::Component { path, props, .. } => {
                let prop_list = props.iter().map(|(name, value)| match value {
                    ast::AttrValue::ImplicitTrue => quote! {
                        #name: true,
                    },
                    ast::AttrValue::Literal(lit) => quote! {
                        #name: #lit,
                    },
                    ast::AttrValue::Expr(expr) => quote! {
                        #name: #expr,
                    },
                });

                let component_path = &path.type_path;
                let props_path = path.props_path();

                let mount_expr = quote! {
                    #component_path::mount(
                        #props_path {
                            #(#prop_list)*
                            __phantom: std::marker::PhantomData
                        },
                        __vm
                    ) #err_handler
                };

                match ctx.scope {
                    Scope::Component => mount_expr,
                    Scope::Enum => quote! { Box::new(#mount_expr) },
                }
            }
            ir::Expression::Match {
                enum_type,
                expr,
                arms,
                ..
            } => {
                let enum_ident = enum_type.to_tokens(root_idents, ctx.scope, WithGenerics(false));

                let arms = arms.iter().map(
                    |ir::Arm {
                         enum_variant_ident,
                         pattern,
                         block,
                         ..
                     }| {
                        let mount_stmts = block.statements.iter().map(|statement| {
                            statement.gen_mount(
                                &root_idents,
                                CodegenCtx {
                                    scope: Scope::Enum,
                                    ..ctx
                                },
                            )
                        });
                        let struct_params = block
                            .struct_fields
                            .iter()
                            .map(ir::StructField::struct_param_tokens);

                        quote! {
                            #pattern => {
                                #(#mount_stmts)*

                                #enum_ident::#enum_variant_ident {
                                    #(#struct_params)*
                                    __phantom: std::marker::PhantomData
                                }
                            },
                        }
                    },
                );

                quote! {
                    match #expr { #(#arms)* }
                }
            }
        };

        if let Some(field) = &self.field {
            quote! { let #field = #expr; }
        } else {
            quote! { #expr; }
        }
    }

    fn gen_patch(&self, root_idents: &RootIdents, ctx: CodegenCtx) -> TokenStream {
        match &self.expression {
            ir::Expression::ConstDom(program) => {
                if let Some(field) = self.field {
                    // OPTIMIZATION: Can advance the cursor directly!

                    let field_ref = FieldRef(field, ctx);

                    match program.opcodes.last().unwrap() {
                        ir::DomOpCode::EnterElement(_) => quote! {
                            __vm.advance_to_first_child_of(&#field_ref);
                        },
                        ir::DomOpCode::Text(_) | ir::DomOpCode::ExitElement => quote! {
                            __vm.advance_to_next_sibling_of(#field_ref.as_node());
                        },
                    }
                } else {
                    let program_ident = program.get_ident(root_idents);

                    quote! {
                        __vm.skip_const_program(&#program_ident);
                    }
                }
            }
            ir::Expression::VariableText {
                expr,
                variable_field,
            } => {
                let field_ref = FieldRef(self.field.clone().unwrap(), ctx);
                let variable_field_ref = FieldRef(*variable_field, ctx);
                quote! {
                    if let Some(v) = #variable_field_ref.update(#expr) {
                        H::set_text(&#field_ref, v);
                    }
                }
            }
            ir::Expression::Component { path, props } => {
                let prop_list = props.iter().map(|(name, value)| match value {
                    ast::AttrValue::ImplicitTrue => quote! {
                        #name: true,
                    },
                    ast::AttrValue::Literal(lit) => quote! {
                        #name: #lit,
                    },
                    ast::AttrValue::Expr(expr) => quote! {
                        #name: #expr,
                    },
                });
                let props_path = path.props_path();

                let field_ref = FieldRef(self.field.clone().unwrap(), ctx);

                quote! {
                    #field_ref.patch(#props_path {
                        #(#prop_list)*
                        __phantom: std::marker::PhantomData,
                    }, __vm);
                }
            }
            ir::Expression::Match {
                enum_type,
                expr,
                arms,
            } => {
                // The match arms generated here should be twice as many as in the mounter.
                // We match a tuple of two things: the existing value and the new value.
                // The first match arms are for when the existing value matches the old value.
                // The rest of the matches are for mismatches, one for each new value.

                let enum_ident = enum_type.to_tokens(root_idents, ctx.scope, WithGenerics(false));
                let field = self.field.clone().unwrap();
                let field_ref = FieldRef(field, ctx);
                let field_assign = FieldAssign(field, ctx);
                let mut_field_ref = MutFieldRef(field, ctx);

                let matching_arms = arms.iter().map(
                    |ir::Arm {
                         enum_variant_ident,
                         pattern,
                         block,
                         ..
                     }| {
                        let fields = block
                            .struct_fields
                            .iter()
                            .map(ir::StructField::mut_pattern_tokens);

                        let patch_stmts = block.statements.iter().map(|statement| {
                            statement.gen_patch(
                                root_idents,
                                CodegenCtx {
                                    scope: Scope::Enum,
                                    ..ctx
                                },
                            )
                        });

                        quote! {
                           (#enum_ident::#enum_variant_ident { #(#fields)* .. }, #pattern) => {
                               #(#patch_stmts)*
                           },
                        }
                    },
                );

                let nonmatching_arms = arms.iter().map(
                    |ir::Arm {
                         enum_variant_ident,
                         pattern,
                         block,
                         ..
                     }| {
                        let mount_stmts = block.statements.iter().map(|statement| {
                            statement.gen_mount(
                                &root_idents,
                                CodegenCtx {
                                    scope: Scope::Enum,
                                    ..ctx
                                },
                            )
                        });
                        let struct_params = block
                            .struct_fields
                            .iter()
                            .map(ir::StructField::struct_param_tokens);

                        quote! {
                            (_, #pattern) => {
                                #field_ref.unmount(__vm);
                                #(#mount_stmts)*

                                #field_assign = #enum_ident::#enum_variant_ident {
                                    #(#struct_params)*
                                    __phantom: std::marker::PhantomData
                                };
                           },
                        }
                    },
                );

                quote! {
                    match (#mut_field_ref, #expr) {
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

impl ir::ConstDomProgram {
    fn get_ident(&self, root_idents: &RootIdents) -> syn::Ident {
        quote::format_ident!("{}_PRG{}", root_idents.uppercase_prefix, self.id)
    }
}

///
/// Generate the rust program required to appropriately unmount
///
fn gen_unmount(statements: &[ir::Statement], ctx: CodegenCtx) -> TokenStream {
    let mut stmts: Vec<TokenStream> = vec![];

    for statement in statements {
        let mut dom_depth = statement.dom_depth;

        match &statement.expression {
            ir::Expression::ConstDom(ir::ConstDomProgram { opcodes, .. }) => {
                for opcode in opcodes {
                    match opcode {
                        ir::DomOpCode::EnterElement(tag_name) => {
                            if dom_depth == 0 {
                                stmts.push(quote! {
                                    __vm.remove_element(#tag_name).unwrap();
                                });
                            }
                            dom_depth += 1;
                        }
                        ir::DomOpCode::Text(_) => {
                            if dom_depth == 0 {
                                stmts.push(quote! {
                                    __vm.remove_text().unwrap();
                                });
                            }
                        }
                        ir::DomOpCode::ExitElement => {
                            dom_depth -= 1;
                        }
                    }
                }
            }
            ir::Expression::VariableText { .. } => {
                if dom_depth == 0 {
                    stmts.push(quote! {
                        __vm.remove_text().unwrap();
                    });
                }
            }
            ir::Expression::Component { .. } => {
                if dom_depth == 0 {
                    let field_ref = FieldRef(statement.field.clone().unwrap(), ctx);
                    stmts.push(quote! {
                        #field_ref.unmount(__vm);
                    });
                }
            }
            ir::Expression::Match { .. } => {
                if dom_depth == 0 {
                    let field_ref = FieldRef(statement.field.clone().unwrap(), ctx);
                    stmts.push(quote! {
                        #field_ref.unmount(__vm);
                    });
                }
            }
            ir::Expression::LocalVar { .. } => {}
        }
    }

    quote! {
        #(#stmts)*
    }
}

struct WithGenerics(bool);

impl ir::StructFieldType {
    fn to_tokens(
        &self,
        root_idents: &RootIdents,
        scope: Scope,
        with_generics: WithGenerics,
    ) -> TokenStream {
        let generics = match with_generics.0 {
            true => quote! { <H> },
            false => quote! {},
        };

        match self {
            Self::DomElement => quote! { H::Element },
            Self::DomText => quote! { H::Text },
            Self::Component(path) => {
                let type_path = &path.type_path;
                match scope {
                    Scope::Enum => quote! { Box<#type_path #generics> },
                    Scope::Component => quote! { #type_path #generics },
                }
            }
            Self::Enum(enum_index) => {
                let ident =
                    quote::format_ident!("{}Enum{}", root_idents.component_ident, enum_index);
                quote! { #ident #generics }
            }
            Self::Variable(ty) => {
                quote! { Var<#ty> }
            }
        }
    }
}

/// A field we want to reference (read)
struct FieldRef(ir::FieldId, CodegenCtx);

impl quote::ToTokens for FieldRef {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { self.#field },
            Scope::Enum => quote! { #field },
        });
    }
}

struct MutFieldRef(ir::FieldId, CodegenCtx);

impl quote::ToTokens for MutFieldRef {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { &mut self.#field },
            // Field has already been marked as a `ref mut` in the pattern binding:
            Scope::Enum => quote! { &#field },
        });
    }
}

/// A field we want to assign to (mut)
struct FieldAssign(ir::FieldId, CodegenCtx);

impl quote::ToTokens for FieldAssign {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { self.#field },
            Scope::Enum => quote! { *#field },
        });
    }
}
