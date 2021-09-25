use proc_macro2::TokenStream;
use quote::quote;

use crate::callback;
use crate::ir;
use crate::param;
use crate::template_ast;

#[derive(Copy, Clone)]
pub enum Function {
    Mount,
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

pub struct RootIdents {
    pub component_ident: syn::Ident,
    pub public_props_ident: syn::Ident,
    pub env_ident: syn::Ident,
    pub root_span_ident: syn::Ident,
    pub self_shim_ident: syn::Ident,
    pub uppercase_prefix: String,
}

pub enum ConstructorKind<'a> {
    Component,
    DynamicSpan {
        dynamic_span_type: &'a ir::StructFieldType,
        variant: &'a syn::Ident,
    },
}

/// A "self shim" is used when the client needs access to the component
/// internals (e.g. callback functions). Not every component needs that.
#[derive(Clone, Copy)]
pub struct HasSelfShim(pub bool);

pub struct Params<'p>(pub Option<&'p [param::Param]>);

impl RootIdents {
    pub fn from_component_ident(component_ident: syn::Ident) -> Self {
        let public_props_ident = quote::format_ident!("__{}Props", component_ident);
        let env_ident = quote::format_ident!("__{}Env", component_ident);
        let root_span_ident = quote::format_ident!("__{}RootSpan", component_ident);
        let self_shim_ident = quote::format_ident!("__{}Shim", component_ident);
        let uppercase_prefix = format!("__{}", component_ident.clone().to_string().to_uppercase());

        Self {
            component_ident,
            public_props_ident,
            env_ident,
            root_span_ident,
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
            Self::Env => {
                tokens.extend(quote::quote! { env });
            }
        }
    }
}

/// A field we want to reference (read)
pub struct FieldExpr<'f>(&'f ir::FieldIdent, CodegenCtx);

impl<'f> quote::ToTokens for FieldExpr<'f> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { self.#field },
            Scope::DynamicSpan => quote! { #field },
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

pub struct MutFieldRef<'f>(&'f ir::FieldIdent, CodegenCtx);

impl<'f> quote::ToTokens for MutFieldRef<'f> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let field = &self.0;

        tokens.extend(match self.1.scope {
            Scope::Component => quote! { &mut self.#field },
            Scope::DynamicSpan => quote! { #field },
        });
    }
}

pub struct MutFieldPat<'f>(&'f ir::FieldIdent, CodegenCtx);

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
pub struct FieldAssign<'f>(&'f ir::FieldIdent, CodegenCtx);

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

pub fn collect_dynamic_span_enums(
    statements: &[ir::Statement],
    component_kind: ir::ComponentKind,
    root_idents: &RootIdents,
    output: &mut Vec<TokenStream>,
) {
    for statement in statements {
        match &statement.expression {
            ir::Expression::Match {
                dynamic_span_type,
                arms,
                ..
            } => {
                output.push(generate_dynamic_span_enum(
                    dynamic_span_type,
                    arms,
                    statement.dom_depth,
                    component_kind,
                    root_idents,
                ));
                for arm in arms {
                    collect_dynamic_span_enums(
                        &arm.block.statements,
                        component_kind,
                        root_idents,
                        output,
                    );
                }
            }
            _ => {}
        }
    }
}

fn generate_dynamic_span_enum(
    dynamic_span_type: &ir::StructFieldType,
    arms: &[ir::Arm],
    dom_depth: ir::DomDepth,
    component_kind: ir::ComponentKind,
    root_idents: &RootIdents,
) -> TokenStream {
    let dynamic_span_ident =
        dynamic_span_type.to_tokens(root_idents, Scope::DynamicSpan, WithGenerics(false));

    let variant_defs = arms.iter().map(|arm| {
        let variant = &arm.variant;
        let struct_field_defs = arm
            .block
            .struct_fields
            .iter()
            .map(|field| field.field_def_tokens(root_idents, Scope::DynamicSpan));

        quote! {
            #variant {
                #(#struct_field_defs)*
            },
        }
    });

    let span_pass_arms = arms.iter().map(|ir::Arm { variant, block, .. }| {
        let pat_fields = block
            .struct_fields
            .iter()
            .map(ir::StructField::mut_pattern_tokens);

        let span_pass = block.gen_span_pass(
            dom_depth,
            root_idents,
            CodegenCtx {
                component_kind,
                function: Function::SpanPass,
                scope: Scope::DynamicSpan,
            },
        );

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
            let pat_fields = block
                .struct_fields
                .iter()
                .map(ir::StructField::mut_pattern_tokens);

            let span_pass = block.gen_span_erase(CodegenCtx {
                component_kind,
                function: Function::SpanPass,
                scope: Scope::DynamicSpan,
            });

            (
                ActuallyDoesErase(span_pass.is_some()),
                quote! {
                    Self::#variant { #(#pat_fields)* .. } => {
                        #span_pass
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
        enum #dynamic_span_ident<H: ::hypp::Hypp> {
            #(#variant_defs)*
        }

        impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for #dynamic_span_ident<H> {
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
    pub fn field_def_tokens(&self, root_idents: &RootIdents, scope: Scope) -> TokenStream {
        let ident = &self.ident;
        let ty = self.ty.to_tokens(root_idents, scope, WithGenerics(true));

        quote! { #ident: #ty, }
    }

    // Tokens for initializing a specific field in a struct
    pub fn struct_param_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        match &self.ty {
            // Handled separately:
            ir::StructFieldType::Env => quote! {},
            _ => quote! { #ident, },
        }
    }

    pub fn mut_pattern_tokens(&self) -> TokenStream {
        let ident = &self.ident;

        match &self.ty {
            // mutable types
            ir::StructFieldType::Component(_) | ir::StructFieldType::DynamicSpan(_) => quote! {
                ref mut #ident,
            },
            // immutable types
            ir::StructFieldType::DomElement
            | ir::StructFieldType::DomText
            | ir::StructFieldType::Env
            | ir::StructFieldType::WeakSelf
            | ir::StructFieldType::Anchor
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
        params: Params,
        ctx: CodegenCtx,
    ) -> TokenStream {
        // Mount is error-tolerant:
        let err_handler = match &ctx.function {
            Function::Mount => quote! { ? },
            _ => quote! { .unwrap() },
        };

        struct FieldInit {
            field_mut: bool,
            init: TokenStream,
        }

        // Anchor needs to be taken before anything else
        let anchor_init = match (ctx.component_kind, ctx.scope) {
            (ir::ComponentKind::SelfUpdatable, Scope::Component) => Some(quote! {
                let __anchor = __cursor.anchor();
            }),
            _ => None,
        };

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
                        __cursor.attribute_value_callback() #err_handler
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
                            Scope::DynamicSpan => {
                                quote! { #mount_expr.into_boxed() }
                            }
                        },
                    }
                }
                ir::Expression::Match {
                    dynamic_span_type,
                    expr,
                    arms,
                    ..
                } => {
                    let arms = arms.iter().map(
                        |ir::Arm {
                             variant,
                             pattern,
                             block,
                             ..
                         }| {
                            let mount = block.gen_mount(
                                ConstructorKind::DynamicSpan {
                                    dynamic_span_type,
                                    variant,
                                },
                                &root_idents,
                                Params(None),
                                CodegenCtx {
                                    scope: Scope::DynamicSpan,
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
            ConstructorKind::DynamicSpan {
                dynamic_span_type,
                variant,
            } => {
                let dynamic_span_ident =
                    dynamic_span_type.to_tokens(root_idents, ctx.scope, WithGenerics(false));

                quote! {
                    #dynamic_span_ident::#variant
                }
            }
        };

        let env = if let Some(params) = params.0 {
            let env_ident = &root_idents.env_ident;
            let struct_params = params.iter().map(param::Param::owned_struct_param_tokens);

            quote! {
                env: #env_ident {
                    #(#struct_params)*
                },
            }
        } else {
            quote! {}
        };

        let callback_collectors =
            self.statements
                .iter()
                .map(|statement| match &statement.expression {
                    ir::Expression::AttributeCallback(callback::Callback { ident }) => {
                        if let Some(field) = &statement.field {
                            let component_ident = &root_idents.component_ident;

                            quote! {
                                __binder.bind(
                                    #field.clone(),
                                    ::hypp::ShimMethod::<#component_ident<H>>(&|shim| {
                                        shim.#ident();
                                    }),
                                );
                            }
                        } else {
                            quote! {}
                        }
                    }
                    _ => quote! {},
                });

        // Things we have to do after the constructor
        let shared_ref_statements = match (self.handle_kind, ctx.function) {
            (ir::HandleKind::Shared, Function::Mount) => {
                quote! {
                    __mounted.borrow_mut().__weak_self = Some(::std::rc::Rc::downgrade(&__mounted));
                    __binder.bind_all(&__mounted.borrow().__weak_self);
                }
            }
            _ => quote! {},
        };

        match (self.handle_kind, ctx.function) {
            (ir::HandleKind::Shared, Function::Mount) => {
                quote! {
                    #anchor_init
                    let mut __binder: ::hypp::shim::LazyBinder<H, Self> = ::hypp::shim::LazyBinder::new();

                    #(#field_inits)*
                    #(#callback_collectors)*

                    let __weak_self = None;
                    let __mounted = ::std::rc::Rc::new(
                        ::std::cell::RefCell::new(
                            #constructor_path {
                                #(#struct_params)*
                                #env
                            }
                        )
                    );

                    #shared_ref_statements
                }
            }
            _ => quote! {
                #anchor_init
                #(#field_inits)*
                #(#callback_collectors)*

                let __mounted = #constructor_path {
                    #(#struct_params)*
                    #env
                };
            },
        }
    }

    ///
    /// Generate code for the Span::pass method
    ///
    pub fn gen_span_pass(
        &self,
        base_dom_depth: ir::DomDepth,
        root_idents: &RootIdents,
        ctx: CodegenCtx,
    ) -> TokenStream {
        let mut sub_spans: Vec<TokenStream> = vec![];

        for statement in &self.statements {
            let mut dom_depth = statement.dom_depth.0 - base_dom_depth.0;

            match &statement.expression {
                ir::Expression::ConstDom(program) => {
                    let program_ident = program.get_ident(root_idents);
                    for (index, opcode) in program.opcodes.iter().enumerate() {
                        match opcode {
                            ir::DomOpCode::EnterElement(_) => {
                                if dom_depth == 0 {
                                    sub_spans.push(quote! {
                                        &mut #program_ident[#index].as_span()
                                    });
                                }
                                dom_depth += 1;
                            }
                            ir::DomOpCode::Text(_) => {
                                if dom_depth == 0 {
                                    sub_spans.push(quote! {
                                        &mut #program_ident[#index].as_span()
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
                        sub_spans.push(quote! {
                            &mut ::hypp::span::SingleTextSpan
                        });
                    }
                }
                ir::Expression::Component { .. } => {
                    if dom_depth == 0 {
                        let field_expr = FieldExpr(&statement.field.as_ref().unwrap(), ctx);
                        sub_spans.push(quote! {
                            #field_expr.borrow_mut()
                        });
                    }
                }
                ir::Expression::Match { .. } => {
                    if dom_depth == 0 {
                        let mut_field_ref = MutFieldRef(&statement.field.as_ref().unwrap(), ctx);
                        sub_spans.push(quote! {
                            #mut_field_ref
                        });
                    }
                }
            }
        }

        quote! {
            ::hypp::span::pass(
                &mut [#(#sub_spans),*],
                __cursor,
                op
            )
        }
    }

    ///
    /// Generate code for the Span::erase method.
    /// Will return None if no specializion for Erase is needed.
    /// (i.e. nothing needs to be released)
    ///
    pub fn gen_span_erase(&self, ctx: CodegenCtx) -> Option<TokenStream> {
        let stmts = self
            .statements
            .iter()
            .filter_map(|statement| match &statement.expression {
                ir::Expression::AttributeCallback(_) => {
                    let field_expr = FieldExpr(statement.field.as_ref().unwrap(), ctx);

                    Some(quote! {
                        #field_expr.borrow_mut().release();
                    })
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        match self.handle_kind {
            ir::HandleKind::Unique if stmts.is_empty() => None,
            ir::HandleKind::Unique => Some(quote! {
                #(#stmts)*
            }),
            ir::HandleKind::Shared => Some(quote! {
                self.__weak_self = None;
                #(#stmts)*
            }),
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

                    let field_expr = FieldExpr(field, ctx);

                    match program.last_node_opcode() {
                        Some(ir::DomOpCode::EnterElement(_)) => quote! {
                            __cursor.move_to_children_of(&#field_expr);
                        },
                        Some(ir::DomOpCode::ExitElement | ir::DomOpCode::Text(_)) => quote! {
                            __cursor.move_to_following_sibling_of(#field_expr.as_node());
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
                let field_expr = FieldExpr(self.field.as_ref().unwrap(), ctx);
                let test = self.param_deps.update_test_tokens();

                quote! {
                    if #test {
                        H::set_text(&#field_expr, #expr.as_ref());
                    }
                    __cursor.move_to_following_sibling_of(#field_expr.as_node());
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

                    let field_expr = FieldExpr(self.field.as_ref().unwrap(), ctx);
                    let test = self.param_deps.update_test_tokens();

                    quote! {
                        if #test {
                            #field_expr.borrow_mut().pass_props(
                                #props_path {
                                    #(#prop_list)*
                                },
                                __cursor
                            );
                        } else {
                            // Nothing has changed, but the cursor must pass
                            // over the component. This should be very cheap.
                            #field_expr.borrow_mut().pass_over(__cursor);
                        }
                    }
                }
            },
            ir::Expression::Match {
                dynamic_span_type,
                expr,
                arms,
            } => match &self.param_deps {
                ir::ParamDeps::Const => quote! {},
                _ => gen_match_patch(self, dynamic_span_type, expr, arms, root_idents, ctx),
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
    dynamic_span_type: &ir::StructFieldType,
    expr: &syn::Expr,
    arms: &[ir::Arm],
    root_idents: &RootIdents,
    ctx: CodegenCtx,
) -> TokenStream {
    let dynamic_span_ident =
        dynamic_span_type.to_tokens(root_idents, ctx.scope, WithGenerics(false));
    let field = statement.field.as_ref().unwrap();
    let field_expr = FieldExpr(field, ctx);
    let field_assign = FieldAssign(field, ctx);
    let mut_field_pat = MutFieldPat(field, ctx);

    let patterns_without_variables: Vec<_> = arms
        .iter()
        .map(|ir::Arm { variant, .. }| {
            quote! {
                #dynamic_span_ident::#variant { .. }
            }
        })
        .collect();

    // make "arm pairs" for every variant, a pair consists of one match and one mismatch.
    let arm_pairs = arms.iter().enumerate().map(
        |(
            arm_index,
            ir::Arm {
                variant,
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
                        scope: Scope::DynamicSpan,
                        ..ctx
                    },
                )
            });

            let nonmatching_patterns =
                patterns_without_variables
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
                ConstructorKind::DynamicSpan {
                    dynamic_span_type,
                    variant,
                },
                &root_idents,
                Params(None),
                CodegenCtx {
                    scope: Scope::DynamicSpan,
                    ..ctx
                },
            );

            quote! {
                // Matching variant:
                (#dynamic_span_ident::#variant { #(#fields)* .. }, #pattern) => {
                    #(#patch_stmts)*
                },

                // Non-matching variant:
                (#(#nonmatching_patterns)|*, #pattern) => {
                    #field_expr.erase(__cursor);
                    #mount

                    #field_assign = __mounted;
                },
            }
        },
    );

    quote! {
        match (#mut_field_pat, #expr) {
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
            Self::Env => {
                let ident = &root_idents.env_ident;
                quote! { #ident }
            }
            Self::WeakSelf => quote! {
                Option<::std::rc::Weak<::std::cell::RefCell<Self>>>
            },
            Self::Anchor => quote! { H::Anchor },
            Self::Callback => quote! { ::hypp::SharedCallback<H> },
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
            Self::DynamicSpan(span_index) => {
                let ident =
                    quote::format_ident!("__{}Span{}", root_idents.component_ident, span_index);
                quote! { #ident #generics }
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
            param::ParamKind::State(_) => quote! { #ident, },
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
            Self::Shared => quote! { ::hypp::handle::Shared },
        }
    }
}
