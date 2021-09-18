use proc_macro2::TokenStream;
use quote::quote;

use crate::ir;
use crate::param;
use crate::template_ast;

#[derive(Copy, Clone)]
pub enum Lifecycle {
    Mount,
    Patch,
    Unmount,
}

#[derive(Copy, Clone)]
pub enum Scope {
    Component,
    Enum,
}

#[derive(Clone, Copy)]
pub struct CodegenCtx {
    pub lifecycle: Lifecycle,
    pub scope: Scope,
}

pub struct RootIdents {
    pub component_ident: syn::Ident,
    pub props_ident: syn::Ident,
    pub uppercase_prefix: String,
}

impl RootIdents {
    pub fn from_component_ident(component_ident: syn::Ident) -> Self {
        let props_ident = quote::format_ident!("{}Props", component_ident);
        let uppercase_prefix = component_ident.clone().to_string().to_uppercase();

        Self {
            component_ident,
            props_ident,
            uppercase_prefix,
        }
    }
}

impl quote::ToTokens for ir::FieldIdent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Id(id) => {
                let ident = quote::format_ident!("__f{}", id);
                tokens.extend(quote::quote! { #ident });
            }
            Self::Param(ident) => {
                tokens.extend(quote::quote! { #ident });
            }
        }
    }
}

/// A field we want to reference (read)
pub struct FieldRef<'f>(&'f ir::FieldIdent, CodegenCtx);

impl<'f> quote::ToTokens for FieldRef<'f> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { self.#field },
            Scope::Enum => quote! { #field },
        });
    }
}

pub struct MutFieldRef<'f>(&'f ir::FieldIdent, CodegenCtx);

impl<'f> quote::ToTokens for MutFieldRef<'f> {
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
pub struct FieldAssign<'f>(&'f ir::FieldIdent, CodegenCtx);

impl<'f> quote::ToTokens for FieldAssign<'f> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { self.#field },
            Scope::Enum => quote! { *#field },
        });
    }
}

pub fn collect_dom_programs(
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

pub fn generate_dom_program(
    program: &ir::ConstDomProgram,
    root_idents: &RootIdents,
) -> TokenStream {
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

pub fn collect_variant_enums(
    statements: &[ir::Statement],
    root_idents: &RootIdents,
    output: &mut Vec<TokenStream>,
) {
    for statement in statements {
        match &statement.expression {
            ir::Expression::Match {
                enum_type, arms, ..
            } => {
                output.push(generate_variant_enum(
                    enum_type,
                    arms,
                    statement.dom_depth,
                    root_idents,
                ));
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
    dom_depth: ir::DomDepth,
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
                dom_depth,
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
            pub fn unmount(&self, __vm: &mut dyn DomVM<H>) {
                match self {
                    #(#unmount_arms)*
                }
            }
        }
    }
}

///
/// Generate the rust program required to appropriately unmount
///
pub fn gen_unmount(
    statements: &[ir::Statement],
    base_dom_depth: ir::DomDepth,
    ctx: CodegenCtx,
) -> TokenStream {
    let mut stmts: Vec<TokenStream> = vec![];

    for statement in statements {
        let mut dom_depth = statement.dom_depth.0 - base_dom_depth.0;

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
            ir::Expression::Text { .. } => {
                if dom_depth == 0 {
                    stmts.push(quote! {
                        __vm.remove_text().unwrap();
                    });
                }
            }
            ir::Expression::Component { .. } => {
                if dom_depth == 0 {
                    let field_ref = FieldRef(&statement.field.as_ref().unwrap(), ctx);
                    stmts.push(quote! {
                        #field_ref.unmount(__vm);
                    });
                }
            }
            ir::Expression::Match { .. } => {
                if dom_depth == 0 {
                    let field_ref = FieldRef(&statement.field.as_ref().unwrap(), ctx);
                    stmts.push(quote! {
                        #field_ref.unmount(__vm);
                    });
                }
            }
        }
    }

    quote! {
        #(#stmts)*
    }
}

impl ir::StructField {
    pub fn field_def_tokens(&self, root_idents: &RootIdents, scope: Scope) -> TokenStream {
        let field = &self.field;
        let ty = self.ty.to_tokens(root_idents, scope, WithGenerics(true));

        quote! { #field: #ty, }
    }

    pub fn struct_param_tokens(&self) -> TokenStream {
        let field = &self.field;

        match &self.ty {
            ir::StructFieldType::Param(param) => match &param.ty {
                param::ParamRootType::One(ty) => match ty {
                    param::ParamLeafType::Owned(_) => {
                        quote! { #field, }
                    }
                    param::ParamLeafType::Ref(_) => {
                        quote! { #field: #field.to_owned(), }
                    }
                },
                param::ParamRootType::Option(ty) => match ty {
                    param::ParamLeafType::Owned(_) => {
                        quote! { #field, }
                    }
                    param::ParamLeafType::Ref(_) => {
                        quote! { #field: #field.map(|val| val.to_owned()), }
                    }
                },
            },
            _ => quote! {#field, },
        }
    }

    pub fn mut_pattern_tokens(&self) -> TokenStream {
        let field = &self.field;

        match &self.ty {
            // mutable types
            ir::StructFieldType::Param(_)
            | ir::StructFieldType::Component(_)
            | ir::StructFieldType::Enum(_) => quote! {
                ref mut #field,
            },
            // immutable types
            ir::StructFieldType::DomElement | ir::StructFieldType::DomText => quote! {
                ref #field,
            },
        }
    }
}

impl ir::ParamDeps {
    pub fn update_test_tokens(&self) -> TokenStream {
        match self {
            Self::Const => quote! { false },
            Self::Some(ids) => {
                let clauses = ids.iter().map(|id| {
                    let id = *id as usize;
                    quote! {
                        __updates[#id]
                    }
                });

                quote! {
                    #(#clauses)||*
                }
            }
            Self::All => quote! { true },
        }
    }
}

impl ir::Statement {
    /// Rust statement used when mounting
    pub fn gen_mount(&self, root_idents: &RootIdents, ctx: CodegenCtx) -> TokenStream {
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
            ir::Expression::Text { expr, .. } => quote! {
                __vm.text(#expr.as_ref()) #err_handler
            },
            ir::Expression::Component { path, props, .. } => {
                let prop_list = props.iter().map(|(name, value)| match value {
                    template_ast::AttrValue::ImplicitTrue => quote! {
                        #name: true,
                    },
                    template_ast::AttrValue::Literal(lit) => quote! {
                        #name: #lit,
                    },
                    template_ast::AttrValue::Expr(expr) => quote! {
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

    pub fn gen_patch(&self, root_idents: &RootIdents, ctx: CodegenCtx) -> TokenStream {
        match &self.expression {
            ir::Expression::ConstDom(program) => {
                if let Some(field) = &self.field {
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
            ir::Expression::Text { expr, .. } => {
                let field_ref = FieldRef(self.field.as_ref().unwrap(), ctx);
                let test = self.param_deps.update_test_tokens();

                quote! {
                    if #test {
                        H::set_text(&#field_ref, #expr);
                    }
                }
            }
            ir::Expression::Component { path, props } => {
                let prop_list = props.iter().map(|(name, value)| match value {
                    template_ast::AttrValue::ImplicitTrue => quote! {
                        #name: true,
                    },
                    template_ast::AttrValue::Literal(lit) => quote! {
                        #name: #lit,
                    },
                    template_ast::AttrValue::Expr(expr) => quote! {
                        #name: #expr,
                    },
                });
                let props_path = path.props_path();

                let field_ref = FieldRef(self.field.as_ref().unwrap(), ctx);

                quote! {
                    #field_ref.set_props(#props_path {
                        #(#prop_list)*
                        __phantom: std::marker::PhantomData,
                    }, __vm);
                }
            }
            ir::Expression::Match {
                enum_type,
                expr,
                arms,
            } => gen_match_patch(self, enum_type, expr, arms, root_idents, ctx),
            _ => TokenStream::new(),
        }
    }
}

/// The match arms generated here should be twice as many as in the mounter.
/// We match a tuple of two things: the existing value and the new value.
/// Algorithm/arm order:
/// First, take every "new" value, and generate match arms for that.
///
/// match (old, new) {
///    (A, A) => { update A; },
///    (B | C, A) => { unmount B|C; mount A; }
///    (B, B) => { update B; },
///    (A | C, B) => { unmount A|C; mount B; },
///    ...etc
/// }
/// Always be explicit about every enum.
/// This is because the last match arm is sometimes a wildcard, e.g. when
/// generated from `if let Some(stuff) = stuff`.
fn gen_match_patch(
    statement: &ir::Statement,
    enum_type: &ir::StructFieldType,
    expr: &syn::Expr,
    arms: &[ir::Arm],
    root_idents: &RootIdents,
    ctx: CodegenCtx,
) -> TokenStream {
    let enum_ident = enum_type.to_tokens(root_idents, ctx.scope, WithGenerics(false));
    let field = statement.field.as_ref().unwrap();
    let field_ref = FieldRef(field, ctx);
    let field_assign = FieldAssign(field, ctx);
    let mut_field_ref = MutFieldRef(field, ctx);

    let patterns_without_variables: Vec<_> = arms
        .iter()
        .map(
            |ir::Arm {
                 enum_variant_ident, ..
             }| {
                quote! {
                    #enum_ident::#enum_variant_ident { .. }
                }
            },
        )
        .collect();

    // make "arm pairs" for every variant, a pair consists of one match and one mismatch.
    let arm_pairs = arms.iter().enumerate().map(
        |(
            arm_index,
            ir::Arm {
                enum_variant_ident,
                pattern,
                block,
                ..
            },
        )| {
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

            let nonmatching_enum_patterns = patterns_without_variables
                .iter()
                .enumerate()
                .filter_map(
                    |(i, pattern)| {
                        if i == arm_index {
                            None
                        } else {
                            Some(pattern)
                        }
                    },
                );

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
                // Matching variant:
                (#enum_ident::#enum_variant_ident { #(#fields)* .. }, #pattern) => {
                    #(#patch_stmts)*
                },

                // Non-matching variant:
                (#(#nonmatching_enum_patterns)|*, #pattern) => {
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
            #(#arm_pairs)*

            _ => {}
        }
    }
}

impl ir::ConstDomProgram {
    fn get_ident(&self, root_idents: &RootIdents) -> syn::Ident {
        quote::format_ident!("{}_PRG{}", root_idents.uppercase_prefix, self.id)
    }
}

pub struct WithGenerics(bool);

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
            Self::Param(param) => match &param.ty {
                param::ParamRootType::One(ty) => match ty {
                    param::ParamLeafType::Owned(ty) => {
                        quote! { #ty }
                    }
                    param::ParamLeafType::Ref(ty) => {
                        quote! { <#ty as ToOwned>::Owned }
                    }
                },
                param::ParamRootType::Option(ty) => match ty {
                    param::ParamLeafType::Owned(ty) => {
                        quote! { Option<#ty> }
                    }
                    param::ParamLeafType::Ref(ty) => {
                        quote! { Option<<#ty as ToOwned>::Owned> }
                    }
                },
            },
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
        }
    }
}
