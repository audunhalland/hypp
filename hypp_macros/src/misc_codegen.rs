//!
//! Miscellaenous helper structs and various code generators generating rust code.
//!

use proc_macro2::TokenStream;
use quote::quote;

use crate::ir;
use crate::param;

#[derive(Copy, Clone)]
pub enum Function {
    Patch,
    SpanPass,
    Erase,
}

#[derive(Copy, Clone)]
pub enum Scope {
    Component,
    DynamicSpan,
}

#[derive(Clone, Copy)]
pub struct CodegenCtx {
    pub component_kind: ir::ComponentKind,
    pub function: Function,
    pub scope: Scope,
}

/// Context for one whole component
pub struct CompCtx {
    pub kind: ir::ComponentKind,
    pub component_ident: syn::Ident,
    pub public_props_ident: syn::Ident,
    pub env_ident: syn::Ident,
    pub root_span_ident: syn::Ident,
    pub self_shim_ident: syn::Ident,
    pub uppercase_prefix: String,

    pub patch_ctx_ty: TokenStream,
}

impl CompCtx {
    pub fn new(component_ident: syn::Ident, kind: ir::ComponentKind) -> Self {
        let public_props_ident = quote::format_ident!("__{}Props", component_ident);
        let env_ident = quote::format_ident!("__{}Env", component_ident);
        let root_span_ident = quote::format_ident!("__{}RootSpan", component_ident);
        let self_shim_ident = quote::format_ident!("__{}Shim", component_ident);
        let uppercase_prefix = format!("__{}", component_ident.clone().to_string().to_uppercase());

        let patch_ctx_ty = match kind {
            ir::ComponentKind::Basic => quote! {
                ::hypp::PatchCtx<H>
            },
            ir::ComponentKind::SelfUpdatable => quote! {
                ::hypp::PatchBindCtx<H, Self>
            },
        };

        Self {
            kind,
            component_ident,
            public_props_ident,
            env_ident,
            root_span_ident,
            self_shim_ident,
            uppercase_prefix,
            patch_ctx_ty,
        }
    }
}

pub enum SpanConstructorKind<'a> {
    FixedSpan(Option<&'a ir::StructFieldType>),
    DynamicSpan {
        span_type: &'a ir::StructFieldType,
        variant: &'a syn::Ident,
    },
}

/// Code associated with a certain field
pub struct FieldCode {
    field: Option<ir::FieldIdent>,
    code: TokenStream,
}

impl quote::ToTokens for FieldCode {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(self.code.clone());
    }
}

/// Code associated with a set of fields
pub struct JoinedFieldCode {
    fields: std::collections::BTreeSet<u16>,
    code: TokenStream,
}

impl quote::ToTokens for JoinedFieldCode {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(self.code.clone());
    }
}

impl quote::ToTokens for ir::FieldIdent {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = quote::format_ident!("__f{}", self.0);
        tokens.extend(quote::quote! { #ident });
    }
}

/// A field we want to reference (read)
pub struct FieldExpr(pub ir::FieldIdent, pub CodegenCtx);

impl quote::ToTokens for FieldExpr {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match (self.1.scope, self.1.function) {
            (Scope::Component, Function::Patch) => quote! { #field },
            (Scope::Component, _) => quote! { self.#field },
            (Scope::DynamicSpan, _) => quote! { #field },
        });
    }
}

/// A field we want to pass directly to a function as a reference
pub struct FieldRef<'f>(&'f ir::FieldIdent, CodegenCtx);

impl<'f> quote::ToTokens for FieldRef<'f> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { &self.#field },
            Scope::DynamicSpan => quote! { #field },
        });
    }
}

pub struct MutFieldRef(ir::FieldIdent, CodegenCtx);

impl<'f> quote::ToTokens for MutFieldRef {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { &mut self.#field },
            Scope::DynamicSpan => quote! { #field },
        });
    }
}

pub struct MutFieldPat<'f>(pub &'f ir::FieldIdent, pub CodegenCtx);

impl<'f> quote::ToTokens for MutFieldPat<'f> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { &mut self.#field },
            // Field has already been marked as a `ref mut` in the pattern binding:
            Scope::DynamicSpan => quote! { &#field },
        });
    }
}

/// A field we want to assign to (mut)
pub struct FieldAssign<'f>(pub &'f ir::FieldIdent, pub CodegenCtx);

impl<'f> quote::ToTokens for FieldAssign<'f> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { self.#field },
            Scope::DynamicSpan => quote! { *#field },
        });
    }
}

pub fn collect_dom_programs(
    statements: &[ir::Statement],
    comp_ctx: &CompCtx,
    output: &mut Vec<TokenStream>,
) {
    for statement in statements {
        match &statement.expression {
            ir::Expression::ConstDom(program) => {
                output.push(generate_dom_program(program, comp_ctx));
            }
            ir::Expression::Match { arms, .. } => {
                for arm in arms {
                    collect_dom_programs(&arm.block.statements, comp_ctx, output);
                }
            }
            ir::Expression::Iter { inner_block, .. } => {
                collect_dom_programs(&inner_block.statements, comp_ctx, output);
            }
            _ => {}
        }
    }
}

pub fn generate_dom_program(program: &ir::ConstDomProgram, comp_ctx: &CompCtx) -> TokenStream {
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
    let ident = program.get_ident(comp_ctx);

    quote! {
        static #ident: [::hypp::ConstOpCode; #len] = [
            #(#opcodes)*
        ];
    }
}

pub fn collect_span_typedefs(
    block: &ir::Block,
    span_type: Option<&ir::StructFieldType>,
    comp_ctx: &CompCtx,
    output: &mut Vec<TokenStream>,
) {
    output.push(gen_fixed_span_struct(block, span_type, comp_ctx));
    collect_stmt_span_typedefs(&block.statements, comp_ctx, output);
}

fn collect_stmt_span_typedefs(
    statements: &[ir::Statement],
    comp_ctx: &CompCtx,
    output: &mut Vec<TokenStream>,
) {
    for statement in statements {
        match &statement.expression {
            ir::Expression::Match {
                span_type, arms, ..
            } => {
                output.push(gen_dynamic_span_enum(
                    span_type,
                    arms,
                    statement.dom_depth,
                    comp_ctx,
                ));
                for arm in arms {
                    collect_stmt_span_typedefs(&arm.block.statements, comp_ctx, output);
                }
            }
            ir::Expression::Iter {
                span_type,
                inner_block,
                ..
            } => {
                collect_span_typedefs(inner_block, Some(span_type), comp_ctx, output);
            }
            _ => {}
        }
    }
}

fn gen_fixed_span_struct(
    block: &ir::Block,
    span_type: Option<&ir::StructFieldType>,
    comp_ctx: &CompCtx,
) -> TokenStream {
    let span_ident = if let Some(span_type) = span_type {
        span_type.to_tokens(comp_ctx, Scope::DynamicSpan, StructFieldFormat::PathSegment)
    } else {
        let root_span_ident = &comp_ctx.root_span_ident;
        quote! { #root_span_ident }
    };

    let struct_field_defs = block
        .struct_fields
        .iter()
        .map(|field| field.field_def_tokens(&comp_ctx, Scope::Component));

    let span_pass = block.gen_span_pass(
        ir::DomDepth(0),
        &comp_ctx,
        CodegenCtx {
            component_kind: comp_ctx.kind,
            function: Function::SpanPass,
            scope: Scope::Component,
        },
    );

    let fn_span_erase = block
        .gen_span_erase(CodegenCtx {
            component_kind: comp_ctx.kind,
            function: Function::Erase,
            scope: Scope::Component,
        })
        .map(|stmts| {
            quote! {
                fn erase(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
                    #stmts
                    self.pass(__cursor, ::hypp::SpanOp::Erase)
                }
            }
        });

    quote! {
        struct #span_ident<H: ::hypp::Hypp + 'static> {
            #(#struct_field_defs)*
            __phantom: ::std::marker::PhantomData<H>
        }

        impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for #span_ident<H> {
            fn is_anchored(&self) -> bool {
                unimplemented!()
            }

            fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
                #span_pass
            }

            #fn_span_erase
        }
    }
}

fn gen_dynamic_span_enum(
    span_type: &ir::StructFieldType,
    arms: &[ir::Arm],
    dom_depth: ir::DomDepth,
    comp_ctx: &CompCtx,
) -> TokenStream {
    let span_ident =
        span_type.to_tokens(comp_ctx, Scope::DynamicSpan, StructFieldFormat::PathSegment);

    let variant_defs = arms.iter().map(|arm| {
        let variant = &arm.variant;
        let struct_field_defs = arm
            .block
            .struct_fields
            .iter()
            .map(|field| field.field_def_tokens(comp_ctx, Scope::DynamicSpan));

        quote! {
            #variant {
                #(#struct_field_defs)*
            },
        }
    });

    let span_pass_arms = arms.iter().map(|ir::Arm { variant, block, .. }| {
        let span_pass = block.gen_span_pass(
            dom_depth,
            comp_ctx,
            CodegenCtx {
                component_kind: comp_ctx.kind,
                function: Function::SpanPass,
                scope: Scope::DynamicSpan,
            },
        );
        let pat_fields = block
            .struct_fields
            .iter()
            .filter(|field| span_pass.fields.contains(&field.ident.0))
            .map(ir::StructField::mut_pattern_tokens);

        quote! {
            Self::#variant { #(#pat_fields)* .. } => {
                #span_pass
            },
        }
    });

    struct ActuallyDoesErase(bool);

    let span_erase_arms = arms
        .iter()
        .map(|ir::Arm { variant, block, .. }| {
            let opt_span_erase = block.gen_span_erase(CodegenCtx {
                component_kind: comp_ctx.kind,
                function: Function::SpanPass,
                scope: Scope::DynamicSpan,
            });

            let pat_fields = block
                .struct_fields
                .iter()
                .filter(|field| {
                    opt_span_erase
                        .as_ref()
                        .map(|span_erase| span_erase.fields.contains(&field.ident.0))
                        .unwrap_or(false)
                })
                .map(ir::StructField::mut_pattern_tokens);

            (
                ActuallyDoesErase(opt_span_erase.is_some()),
                quote! {
                    Self::#variant { #(#pat_fields)* .. } => {
                        #opt_span_erase
                    },
                },
            )
        })
        .collect::<Vec<_>>();

    let span_erase_fn = if span_erase_arms
        .iter()
        .any(|(actually_does_erase, _)| actually_does_erase.0)
    {
        let arms = span_erase_arms.iter().map(|(_, arm)| arm);
        quote! {
            fn erase(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
                match self {
                    #(#arms)*
                }
                self.pass(__cursor, ::hypp::SpanOp::Erase)
            }
        }
    } else {
        quote! {}
    };

    quote! {
        enum #span_ident<H: ::hypp::Hypp + 'static> {
            #(#variant_defs)*
        }

        impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for #span_ident<H> {
            fn is_anchored(&self) -> bool {
                // not anchored, by definition
                false
            }

            fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
                match self {
                    #(#span_pass_arms)*
                }
            }

            #span_erase_fn
        }
    }
}

impl ir::StructField {
    pub fn field_def_tokens(&self, comp_ctx: &CompCtx, scope: Scope) -> TokenStream {
        let ident = &self.ident;
        let ty = self
            .ty
            .to_tokens(comp_ctx, scope, StructFieldFormat::TypeInStruct);

        quote! { #ident: #ty, }
    }

    // Tokens for initializing a specific field in a struct
    pub fn struct_param_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        quote! { #ident, }
    }

    pub fn mut_pattern_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        match &self.ty {
            // mutable types
            ir::StructFieldType::Component(_)
            | ir::StructFieldType::CallbackSlot
            | ir::StructFieldType::Span(_, _) => quote! {
                ref mut #ident,
            },
            // immutable types
            ir::StructFieldType::DomElement | ir::StructFieldType::DomText => quote! {
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
    ///
    /// Generate code for the Span::pass method
    ///
    pub fn gen_span_pass(
        &self,
        base_dom_depth: ir::DomDepth,
        comp_ctx: &CompCtx,
        ctx: CodegenCtx,
    ) -> JoinedFieldCode {
        let mut sub_spans: Vec<FieldCode> = vec![];

        for statement in &self.statements {
            let mut dom_depth = statement.dom_depth.0 - base_dom_depth.0;

            match &statement.expression {
                ir::Expression::ConstDom(program) => {
                    let program_ident = program.get_ident(comp_ctx);
                    for (index, opcode) in program.opcodes.iter().enumerate() {
                        match opcode {
                            ir::DomOpCode::EnterElement(_) => {
                                if dom_depth == 0 {
                                    sub_spans.push(FieldCode {
                                        field: None,
                                        code: quote! {
                                            &mut #program_ident[#index].as_span()
                                        },
                                    });
                                }
                                dom_depth += 1;
                            }
                            ir::DomOpCode::Text(_) => {
                                if dom_depth == 0 {
                                    sub_spans.push(FieldCode {
                                        field: None,
                                        code: quote! {
                                            &mut #program_ident[#index].as_span()
                                        },
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
                ir::Expression::AttributeCallback(_) => {}
                ir::Expression::Text { .. } => {
                    if dom_depth == 0 {
                        sub_spans.push(FieldCode {
                            field: None,
                            code: quote! {
                            &mut ::hypp::span::SingleTextSpan
                            },
                        });
                    }
                }
                ir::Expression::Component { .. } => {
                    if dom_depth == 0 {
                        let field = statement.field.unwrap();
                        let field_expr = FieldExpr(field, ctx);
                        sub_spans.push(FieldCode {
                            field: Some(field),
                            code: quote! {
                                #field_expr.get_mut()
                            },
                        });
                    }
                }
                ir::Expression::Match { .. } | ir::Expression::Iter { .. } => {
                    if dom_depth == 0 {
                        let field = statement.field.unwrap();
                        let mut_field_ref = MutFieldRef(field, ctx);
                        sub_spans.push(FieldCode {
                            field: Some(field),
                            code: quote! {
                                #mut_field_ref
                            },
                        });
                    }
                }
            }
        }

        JoinedFieldCode {
            fields: sub_spans
                .iter()
                .filter_map(|span| Some(span.field?.0))
                .collect(),
            code: quote! {
                ::hypp::span::pass(
                    &mut [#(#sub_spans),*],
                    __cursor,
                    op
                )
            },
        }
    }

    ///
    /// Generate code for the Span::erase method.
    /// Will return None if no specializion for Erase is needed.
    /// (i.e. nothing needs to be released)
    ///
    pub fn gen_span_erase(&self, ctx: CodegenCtx) -> Option<JoinedFieldCode> {
        let stmts = self
            .statements
            .iter()
            .filter_map(|statement| match &statement.expression {
                ir::Expression::AttributeCallback(_) => {
                    let field = statement.field.unwrap();
                    let field_expr = FieldExpr(field, ctx);

                    Some(FieldCode {
                        field: Some(field),
                        code: quote! {
                            #field_expr.get_mut().release();
                        },
                    })
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        let code = JoinedFieldCode {
            fields: stmts
                .iter()
                .filter_map(|field_code| Some(field_code.field?.0))
                .collect(),
            code: quote! {
                #(#stmts)*
            },
        };

        match self.handle_kind {
            ir::HandleKind::Unique if stmts.is_empty() => None,
            ir::HandleKind::Unique => Some(code),
            ir::HandleKind::Shared => Some(code),
        }
    }
}

impl ir::ConstDomProgram {
    pub fn get_ident(&self, comp_ctx: &CompCtx) -> syn::Ident {
        quote::format_ident!("{}_PRG{}", comp_ctx.uppercase_prefix, self.id)
    }
}

pub enum StructFieldFormat {
    TypeInStruct,
    InnerType,
    PathSegment,
}

impl ir::StructFieldType {
    pub fn to_tokens(
        &self,
        comp_ctx: &CompCtx,
        scope: Scope,
        format: StructFieldFormat,
    ) -> TokenStream {
        let generics = match format {
            StructFieldFormat::TypeInStruct | StructFieldFormat::InnerType => quote! { <H> },
            StructFieldFormat::PathSegment => quote! {},
        };

        match self {
            Self::DomElement => quote! { H::Element },
            Self::DomText => quote! { H::Text },
            Self::CallbackSlot => quote! { H::Shared<H::CallbackSlot> },
            Self::Component(path) => {
                let type_path = &path.type_path;
                match scope {
                    Scope::Component => {
                        quote! { <#type_path #generics as ::hypp::handle::ToHandle>::Handle }
                    }
                    Scope::DynamicSpan => quote! {
                        <<#type_path #generics as ::hypp::handle::ToHandle>::Handle as ::hypp::handle::Handle<#type_path #generics>>::Boxed
                    },
                }
            }
            Self::Span(span_index, span_kind) => {
                let ident =
                    quote::format_ident!("__{}Span{}", comp_ctx.component_ident, span_index);
                match (span_kind, format) {
                    (ir::SpanKind::Enum, StructFieldFormat::TypeInStruct) => {
                        quote! { Option<#ident #generics> }
                    }
                    (ir::SpanKind::RepeatedStruct, StructFieldFormat::TypeInStruct) => {
                        quote! { hypp::list::SimpleListSpan<H, #ident #generics> }
                    }
                    (_, StructFieldFormat::InnerType) => quote! { #ident #generics },
                    (_, StructFieldFormat::PathSegment) => quote! { #ident },
                }
            }
        }
    }
}

impl param::Param {
    // Struct _fields_ for the param, owned version
    pub fn owned_ty_tokens(&self) -> TokenStream {
        match &self.kind {
            param::ParamKind::Prop(root_ty) => match root_ty {
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
            param::ParamKind::State(ty) => {
                quote! { #ty }
            }
        }
    }

    pub fn owned_struct_param_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        match &self.kind {
            // State variables should already be in scope
            param::ParamKind::State(_) => quote! { #ident: Default::default(), },
            // Convert from generally borrowed props:
            param::ParamKind::Prop(root_ty) => match root_ty {
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
        }
    }
}

impl ir::HandleKind {
    pub fn handle_path(&self) -> TokenStream {
        match self {
            Self::Unique => quote! { ::hypp::handle::Unique },
            Self::Shared => quote! { H::Shared },
        }
    }
}
