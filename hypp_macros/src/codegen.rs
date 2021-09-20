use proc_macro2::TokenStream;
use quote::quote;

use crate::callback;
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
    pub public_props_ident: syn::Ident,
    pub owned_props_ident: syn::Ident,
    pub self_shim_ident: syn::Ident,
    pub uppercase_prefix: String,
}

pub enum ConstructorKind<'a> {
    Component,
    Enum {
        enum_type: &'a ir::StructFieldType,
        variant: &'a syn::Ident,
    },
}

/// A "self shim" is used when the client needs access to the component
/// internals (e.g. callback functions). Not every component needs that.
#[derive(Clone, Copy)]
pub struct HasSelfShim(pub bool);

pub struct OwnedProps<'p>(pub Option<&'p [ir::StructField]>);

impl RootIdents {
    pub fn from_component_ident(component_ident: syn::Ident) -> Self {
        let public_props_ident = quote::format_ident!("__{}Props", component_ident);
        let owned_props_ident = quote::format_ident!("__{}OwnedProps", component_ident);
        let self_shim_ident = quote::format_ident!("__{}Shim", component_ident);
        let uppercase_prefix = format!("__{}", component_ident.clone().to_string().to_uppercase());

        Self {
            component_ident,
            public_props_ident,
            owned_props_ident,
            self_shim_ident,
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
            Self::Props => {
                tokens.extend(quote::quote! { props });
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
            ::hypp::ConstOpCode::EnterElement(#lit_str),
        },
        ir::DomOpCode::AttrName(name) => quote! {
            ::hypp::ConstOpCode::AttributeName(#name),
        },
        ir::DomOpCode::AttrTextValue(value) => quote! {
            ::hypp::ConstOpCode::AttributeTextValue(#value),
        },
        ir::DomOpCode::Text(lit_str) => quote! {
            ::hypp::ConstOpCode::Text(#lit_str),
        },
        ir::DomOpCode::ExitElement => quote! {
            ::hypp::ConstOpCode::ExitElement,
        },
    });

    let len = program.opcodes.len();
    let ident = program.get_ident(root_idents);

    quote! {
        static #ident: [::hypp::ConstOpCode; #len] = [
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

                __phantom: ::std::marker::PhantomData<H>
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
        enum #enum_ident<H: ::hypp::Hypp> {
            #(#variant_defs)*
        }

        impl<H: ::hypp::Hypp + 'static> #enum_ident<H> {
            pub fn unmount(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) {
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
                                    __cursor.remove_element(#tag_name).unwrap();
                                });
                            }
                            dom_depth += 1;
                        }
                        ir::DomOpCode::Text(_) => {
                            if dom_depth == 0 {
                                stmts.push(quote! {
                                    __cursor.remove_text().unwrap();
                                });
                            }
                        }
                        ir::DomOpCode::ExitElement => {
                            dom_depth -= 1;
                        }
                        _ => {}
                    }
                }
            }
            ir::Expression::AttributeCallback(_) => {
                let field_ref = FieldRef(&statement.field.as_ref().unwrap(), ctx);
                stmts.push(quote! {
                    #field_ref.release();
                });
            }
            ir::Expression::Text { .. } => {
                if dom_depth == 0 {
                    stmts.push(quote! {
                        __cursor.remove_text().unwrap();
                    });
                }
            }
            ir::Expression::Component { .. } => {
                if dom_depth == 0 {
                    let field_ref = FieldRef(&statement.field.as_ref().unwrap(), ctx);
                    stmts.push(quote! {
                        #field_ref.borrow_mut().unmount(__cursor);
                    });
                }
            }
            ir::Expression::Match { .. } => {
                if dom_depth == 0 {
                    let field_ref = FieldRef(&statement.field.as_ref().unwrap(), ctx);
                    stmts.push(quote! {
                        #field_ref.unmount(__cursor);
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
        let ident = &self.ident;
        let ty = self.ty.to_tokens(root_idents, scope, WithGenerics(true));

        quote! { #ident: #ty, }
    }

    pub fn struct_param_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        match &self.ty {
            ir::StructFieldType::Param(param) => match &param.kind {
                // State variable types must implement default:
                param::ParamKind::State => quote! { #ident: Default::default(), },
                // Convert from generally borrowed props:
                param::ParamKind::Prop => match &param.ty {
                    param::ParamRootType::One(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { #ident, },
                        param::ParamLeafType::Ref(_) => quote! { #ident: #ident.to_owned(), },
                    },
                    param::ParamRootType::Option(ty) => match ty {
                        param::ParamLeafType::Owned(_) => quote! { #ident, },
                        param::ParamLeafType::Ref(_) => {
                            quote! { #ident: #ident.map(|val| val.to_owned()), }
                        }
                    },
                },
            },
            ir::StructFieldType::Callback => quote! { #ident: #ident.clone(), },
            // Handled separately:
            ir::StructFieldType::Props => quote! {},
            _ => quote! { #ident, },
        }
    }

    pub fn mut_pattern_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        match &self.ty {
            // mutable types
            ir::StructFieldType::Param(_)
            | ir::StructFieldType::Component(_)
            | ir::StructFieldType::Enum(_) => quote! {
                ref mut #ident,
            },
            // immutable types
            ir::StructFieldType::DomElement
            | ir::StructFieldType::DomText
            | ir::StructFieldType::Props
            | ir::StructFieldType::Callback => quote! {
                ref #ident,
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

impl ir::Block {
    /// Generate code that will have a local called `__mounted` at the end.
    pub fn gen_mount(
        &self,
        constructor_kind: ConstructorKind<'_>,
        root_idents: &RootIdents,
        owned_props: OwnedProps<'_>,
        ctx: CodegenCtx,
    ) -> TokenStream {
        // Mount is error-tolerant:
        let err_handler = match &ctx.lifecycle {
            Lifecycle::Mount => quote! { ? },
            Lifecycle::Patch | Lifecycle::Unmount => quote! { .unwrap() },
        };

        struct FieldInit {
            field_mut: bool,
            init: TokenStream,
        }

        let field_inits = self.statements.iter().map(|statement| {
            let FieldInit { field_mut, init } = match &statement.expression {
                ir::Expression::ConstDom(program) => {
                    let program_ident = program.get_ident(root_idents);

                    FieldInit {
                        field_mut: false,
                        init: match program.last_node_opcode() {
                            Some(ir::DomOpCode::EnterElement(_) | ir::DomOpCode::ExitElement) => {
                                quote! {
                                    __cursor.const_exec_element(&#program_ident) #err_handler
                                }
                            }
                            Some(ir::DomOpCode::Text(_)) => quote! {
                                __cursor.const_exec_text(&#program_ident) #err_handler
                            },
                            _ => panic!(),
                        },
                    }
                }
                ir::Expression::AttributeCallback(_) => FieldInit {
                    field_mut: false,
                    init: quote! {
                        ::std::rc::Rc::new(__cursor.attribute_value_callback() #err_handler)
                    },
                },
                ir::Expression::Text(expr) => FieldInit {
                    field_mut: true,
                    init: quote! {
                        __cursor.text(#expr.as_ref()) #err_handler
                    },
                },
                ir::Expression::Component { path, props, .. } => {
                    let prop_list = props.iter().map(|attr| {
                        let ident = &attr.ident;
                        match &attr.value {
                            template_ast::AttrValue::ImplicitTrue => quote! {
                                #ident: true,
                            },
                            template_ast::AttrValue::Literal(lit) => quote! {
                                #ident: #lit,
                            },
                            template_ast::AttrValue::Expr(expr) => quote! {
                                #ident: #expr,
                            },
                        }
                    });

                    let component_path = &path.type_path;
                    let props_path = path.props_path();

                    let mount_expr = quote! {
                        #component_path::mount(
                            #props_path {
                                #(#prop_list)*
                                __phantom: ::std::marker::PhantomData
                            },
                            __cursor
                        ) #err_handler
                    };

                    FieldInit {
                        field_mut: false,
                        init: match ctx.scope {
                            Scope::Component => mount_expr,
                            // If inside an enum, the component is _conditional_,
                            // and might be used for recursion:
                            Scope::Enum => {
                                quote! { #mount_expr.into_boxed() }
                            }
                        },
                    }
                }
                ir::Expression::Match {
                    enum_type,
                    expr,
                    arms,
                    ..
                } => {
                    let arms = arms.iter().map(
                        |ir::Arm {
                             enum_variant_ident,
                             pattern,
                             block,
                             ..
                         }| {
                            let mount = block.gen_mount(
                                ConstructorKind::Enum {
                                    enum_type,
                                    variant: enum_variant_ident,
                                },
                                &root_idents,
                                OwnedProps(None),
                                CodegenCtx {
                                    scope: Scope::Enum,
                                    ..ctx
                                },
                            );

                            quote! {
                                #pattern => {
                                    #mount
                                    __mounted
                                },
                            }
                        },
                    );

                    FieldInit {
                        field_mut: false,
                        init: quote! {
                            match #expr { #(#arms)* }
                        },
                    }
                }
            };

            match (field_mut, &statement.field) {
                (true, Some(field)) => quote! {
                    let mut #field = #init;
                },
                (false, Some(field)) => quote! {
                    let #field = #init;
                },
                _ => quote! {
                    #init;
                },
            }
        });

        let struct_params = self
            .struct_fields
            .iter()
            .map(ir::StructField::struct_param_tokens);

        let constructor_path = match constructor_kind {
            ConstructorKind::Component => quote! { Self },
            ConstructorKind::Enum { enum_type, variant } => {
                let enum_ident = enum_type.to_tokens(root_idents, ctx.scope, WithGenerics(false));

                quote! {
                    #enum_ident::#variant
                }
            }
        };

        let owned_props = if let Some(struct_fields) = owned_props.0 {
            let owned_props_ident = &root_idents.owned_props_ident;
            let struct_params = struct_fields
                .iter()
                .map(ir::StructField::struct_param_tokens);

            quote! {
                props: #owned_props_ident {
                    #(#struct_params)*
                },
            }
        } else {
            quote! {}
        };

        let callback_setups = self
            .statements
            .iter()
            .map(|statement| match &statement.expression {
                ir::Expression::AttributeCallback(callback::Callback { ident }) => {
                    if let Some(field) = &statement.field {
                        quote! {
                            {
                                let __mounted = __mounted.clone();
                                #field.bind(Box::new(move || {
                                    __mounted.borrow_mut().shim_updater_trampoline(|shim| {
                                        shim.#ident();
                                    })
                                }));
                            }
                        }
                    } else {
                        quote! {}
                    }
                }
                _ => quote! {},
            });

        match self.handle_kind {
            ir::HandleKind::Unique => quote! {
                #(#field_inits)*

                let __mounted = #constructor_path {
                    #(#struct_params)*
                    #owned_props

                    __phantom: ::std::marker::PhantomData
                };
            },
            ir::HandleKind::Shared => {
                quote! {
                    #(#field_inits)*

                    let __mounted = ::std::rc::Rc::new(
                        ::std::cell::RefCell::new(
                            #constructor_path {
                                #(#struct_params)*
                                #owned_props

                                __phantom: ::std::marker::PhantomData
                            }
                        )
                    );

                    #(#callback_setups)*
                }
            }
        }
    }
}

impl ir::Statement {
    pub fn gen_patch(&self, root_idents: &RootIdents, ctx: CodegenCtx) -> TokenStream {
        // FIXME: Do some "peephole optimization" here.
        // E.g. there is no reason to output "advance to next"
        // if the next instruction does not require the cursor
        // to be at the correct location:
        // Examples:
        // 1. H::set_text
        // 2. the next statement is constant, and therefore is never updated

        match &self.expression {
            ir::Expression::ConstDom(program) => {
                if let Some(field) = &self.field {
                    // OPTIMIZATION: Can advance the cursor directly!

                    let field_ref = FieldRef(field, ctx);

                    match program.last_node_opcode() {
                        Some(ir::DomOpCode::EnterElement(_)) => quote! {
                            __cursor.move_to_children_of(&#field_ref);
                        },
                        Some(ir::DomOpCode::ExitElement | ir::DomOpCode::Text(_)) => quote! {
                            __cursor.move_to_following_sibling_of(#field_ref.as_node());
                        },
                        _ => panic!(),
                    }
                } else {
                    let program_ident = program.get_ident(root_idents);

                    quote! {
                        __cursor.skip_const_program(&#program_ident);
                    }
                }
            }
            ir::Expression::AttributeCallback(_) => quote! {},
            ir::Expression::Text(expr) => {
                let field_ref = FieldRef(self.field.as_ref().unwrap(), ctx);
                let test = self.param_deps.update_test_tokens();

                quote! {
                    if #test {
                        H::set_text(&#field_ref, #expr.as_ref());
                    }
                    __cursor.move_to_following_sibling_of(#field_ref.as_node());
                }
            }
            ir::Expression::Component { path, props } => match &self.param_deps {
                // Const components never need to update!
                // (all their parameters are constants)
                ir::ParamDeps::Const => quote! {},
                _ => {
                    let prop_list = props.iter().map(|attr| {
                        let ident = &attr.ident;
                        match &attr.value {
                            template_ast::AttrValue::ImplicitTrue => quote! {
                                #ident: true,
                            },
                            template_ast::AttrValue::Literal(lit) => quote! {
                                #ident: #lit,
                            },
                            template_ast::AttrValue::Expr(expr) => quote! {
                                #ident: #expr,
                            },
                        }
                    });
                    let props_path = path.props_path();

                    let field_ref = FieldRef(self.field.as_ref().unwrap(), ctx);
                    let test = self.param_deps.update_test_tokens();

                    quote! {
                        if #test {
                            #field_ref.borrow_mut().set_props(
                                #props_path {
                                    #(#prop_list)*
                                    __phantom: ::std::marker::PhantomData,
                                },
                                __cursor
                            );
                        }
                    }
                }
            },
            ir::Expression::Match {
                enum_type,
                expr,
                arms,
            } => match &self.param_deps {
                ir::ParamDeps::Const => quote! {},
                _ => gen_match_patch(self, enum_type, expr, arms, root_idents, ctx),
            },
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

            let mount = block.gen_mount(
                ConstructorKind::Enum {
                    enum_type,
                    variant: enum_variant_ident,
                },
                &root_idents,
                OwnedProps(None),
                CodegenCtx {
                    scope: Scope::Enum,
                    ..ctx
                },
            );

            quote! {
                // Matching variant:
                (#enum_ident::#enum_variant_ident { #(#fields)* .. }, #pattern) => {
                    #(#patch_stmts)*
                },

                // Non-matching variant:
                (#(#nonmatching_enum_patterns)|*, #pattern) => {
                    #field_ref.unmount(__cursor);
                    #mount

                    #field_assign = __mounted;
                },
            }
        },
    );

    quote! {
        match (#mut_field_ref, #expr) {
            #(#arm_pairs)*
        }
    }
}

impl ir::ConstDomProgram {
    fn get_ident(&self, root_idents: &RootIdents) -> syn::Ident {
        quote::format_ident!("{}_PRG{}", root_idents.uppercase_prefix, self.id)
    }
}

pub struct WithGenerics(pub bool);

impl ir::StructFieldType {
    pub fn to_tokens(
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
            Self::Props => {
                let ident = &root_idents.owned_props_ident;
                quote! { #ident }
            }
            Self::Callback => quote! { ::std::rc::Rc<H::Callback> },
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
                    Scope::Enum => quote! {
                        <<#type_path #generics as ::hypp::handle::ToHandle>::Handle as ::hypp::handle::Handle<#type_path #generics>>::Boxed
                    },
                    Scope::Component => {
                        quote! { <#type_path #generics as ::hypp::handle::ToHandle>::Handle }
                    }
                }
            }
            Self::Enum(enum_index) => {
                let ident =
                    quote::format_ident!("__{}Enum{}", root_idents.component_ident, enum_index);
                quote! { #ident #generics }
            }
        }
    }
}

impl ir::HandleKind {
    pub fn handle_path(&self) -> TokenStream {
        match self {
            Self::Unique => quote! { ::hypp::handle::Unique },
            Self::Shared => quote! { ::hypp::handle::Shared },
        }
    }
}
