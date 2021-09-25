#![feature(generic_associated_types)]

/// For pasting the output of component_dbg! .. and debugging it!
#[allow(unused_imports)]
use hypp::prelude::*;

/// NEW CODE: Env for storing props+state
struct Env {
    show_button: bool,
    toggled: bool,
    list: Vec<u32>,
}

// macro output:
pub struct __ConditionalCallbackProps {
    pub show_button: bool,
}
pub struct __ConditionalCallbackShim<'c> {
    // just do this instead of exposing 'props' member:
    show_button: &'c bool,
    toggled: ::hypp::state_ref::StateRef<'c, bool>,
}
impl<'c> __ConditionalCallbackShim<'c> {
    fn handle_click(&mut self) {
        *self.toggled = !*self.toggled;
    }
}
static __CONDITIONALCALLBACK_PRG0: [::hypp::ConstOpCode; 2usize] = [
    ::hypp::ConstOpCode::EnterElement("button"),
    ::hypp::ConstOpCode::AttributeName("on_click"),
];
static __CONDITIONALCALLBACK_PRG1: [::hypp::ConstOpCode; 1usize] =
    [::hypp::ConstOpCode::Text("Toggled")];
static __CONDITIONALCALLBACK_PRG2: [::hypp::ConstOpCode; 1usize] =
    [::hypp::ConstOpCode::Text("Not toggled")];
static __CONDITIONALCALLBACK_PRG3: [::hypp::ConstOpCode; 1usize] =
    [::hypp::ConstOpCode::ExitElement];

struct __ConditionalCallbackSpanRoot<H: ::hypp::Hypp> {
    __f0: __ConditionalCallbackSpan0<H>,
}

impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for __ConditionalCallbackSpanRoot<H> {
    fn is_anchored(&self) -> bool {
        unimplemented!()
    }
    fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
        ::hypp::span::pass(&mut [&mut self.__f0], __cursor, op)
    }
}

enum __ConditionalCallbackSpan0<H: ::hypp::Hypp> {
    Erased,
    V0 {
        // Callback should actually implement Span..
        __f1: ::hypp::SharedCallback<H>,
        __f2: H::Element,
        __f3: __ConditionalCallbackSpan1<H>,
        __f6: H::Element,
    },
    V1 {},
}
impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for __ConditionalCallbackSpan0<H> {
    fn is_anchored(&self) -> bool {
        false
    }
    fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
        match self {
            Self::Erased => false,
            Self::V0 {
                ref __f1,
                ref __f2,
                ref mut __f3,
                ref __f6,
                ..
            } => ::hypp::span::pass(
                &mut [&mut __CONDITIONALCALLBACK_PRG0[0usize].as_span()],
                __cursor,
                op,
            ),
            Self::V1 { .. } => ::hypp::span::pass(&mut [], __cursor, op),
        }
    }
    fn erase(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
        match self {
            Self::Erased => {}
            Self::V0 {
                ref __f1,
                ref __f2,
                ref mut __f3,
                ref __f6,
                ..
            } => {
                __f1.borrow_mut().release();
            }
            Self::V1 { .. } => {}
        }
        let res = self.pass(__cursor, ::hypp::SpanOp::Erase);
        *self = Self::Erased;
        res
    }
}
enum __ConditionalCallbackSpan1<H: ::hypp::Hypp> {
    Erased,
    V0 { __f4: H::Text },
    V1 { __f5: H::Text },
}
impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for __ConditionalCallbackSpan1<H> {
    fn is_anchored(&self) -> bool {
        false
    }
    fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
        match self {
            Self::Erased => false,
            Self::V0 { ref __f4, .. } => ::hypp::span::pass(
                &mut [&mut __CONDITIONALCALLBACK_PRG1[0usize].as_span()],
                __cursor,
                op,
            ),
            Self::V1 { ref __f5, .. } => ::hypp::span::pass(
                &mut [&mut __CONDITIONALCALLBACK_PRG2[0usize].as_span()],
                __cursor,
                op,
            ),
        }
    }
    fn erase(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
        let res = self.pass(__cursor, ::hypp::SpanOp::Erase);
        *self = Self::Erased;
        res
    }
}

#[allow(dead_code)]
pub struct ConditionalCallback<H: ::hypp::Hypp> {
    env: Env,
    span: __ConditionalCallbackSpanRoot<H>,
    __anchor: H::Anchor,
    __weak_self: Option<::std::rc::Weak<::std::cell::RefCell<Self>>>,
}
impl<H: ::hypp::Hypp + 'static> ConditionalCallback<H> {
    pub fn mount(
        __ConditionalCallbackProps { show_button, .. }: __ConditionalCallbackProps,
        __cursor: &mut dyn ::hypp::Cursor<H>,
    ) -> Result<::hypp::handle::Shared<Self>, ::hypp::Error> {
        let __updates: [bool; 2usize] = [true; 2usize];
        let __anchor = __cursor.anchor();
        let mut __binder: ::hypp::shim::LazyBinder<H, Self> = ::hypp::shim::LazyBinder::new();

        let env = Env {
            show_button,
            toggled: Default::default(),
            list: vec![],
        };

        let mut span = None;
        Self::patch2(
            ::hypp::InputOrOutput::Output(&mut span),
            &env,
            &__updates,
            &mut ::hypp::PatchBindCtx {
                cur: __cursor,
                bind: &mut __binder,
            },
        )?;

        let __weak_self = None;
        let __mounted = ::std::rc::Rc::new(::std::cell::RefCell::new(ConditionalCallback {
            env,
            span: span.unwrap(),
            __anchor,
            __weak_self,
        }));
        __mounted.borrow_mut().__weak_self = Some(::std::rc::Rc::downgrade(&__mounted));
        __binder.bind_all(&__mounted.borrow().__weak_self);

        Ok(::hypp::handle::Shared::new(__mounted))
    }

    // Patch only one root conditional.
    // can be used by initial mount.
    fn patch2(
        __root: ::hypp::InputOrOutput<__ConditionalCallbackSpanRoot<H>>,
        __env: &Env,
        __updates: &[bool],
        __ctx: &mut ::hypp::PatchBindCtx<H, Self>,
    ) -> ::hypp::Void {
        let show_button = __env.show_button;
        let toggled = __env.toggled;
        let list = &__env.list;

        // test code
        let do_with_list =
            |__ctx: &mut ::hypp::PatchBindCtx<H, Self>| -> Result<(), ::hypp::Error> {
                let cb = __ctx.cur.attribute_value_callback()?;
                __ctx.bind.bind(
                    cb,
                    ::hypp::ShimMethod::<ConditionalCallback<H>>(&|shim| {
                        shim.handle_click();
                    }),
                );
                for _item in list.iter() {}
                Ok(())
            };

        let do_with_list2 =
            |__ctx: &mut ::hypp::PatchBindCtx<H, Self>| -> Result<(), ::hypp::Error> {
                for _item in list.iter() {}
                Ok(())
            };

        let __patch_span0 = |mut __f0: &mut __ConditionalCallbackSpan0<H>,
                             __ctx: &mut ::hypp::PatchBindCtx<H, Self>|
         -> ::hypp::Void {
            match show_button {
                // Single pattern here!
                true => {
                    let __patch_span1 = |mut __f3: &mut __ConditionalCallbackSpan1<H>,
                                         __ctx: &mut ::hypp::PatchBindCtx<H, Self>|
                     -> ::hypp::Void {
                        // Single pattern here!
                        match toggled {
                            true => {
                                match &__f3 {
                                    __ConditionalCallbackSpan1::V0 { ref __f4, .. } => {
                                        // Can do this
                                        __f3.pass_over(__ctx.cur);
                                        // Instead of this:
                                        // __cursor.move_to_following_sibling_of(__f4.as_node());
                                    }
                                    _ => {
                                        __f3.erase(__ctx.cur);
                                        let __f4 = __ctx.cur.const_exec_text(&[
                                            ::hypp::ConstOpCode::Text("Toggled"),
                                        ])?;
                                        *__f3 = __ConditionalCallbackSpan1::V0 { __f4 };
                                    }
                                }
                            }
                            false => match &__f3 {
                                __ConditionalCallbackSpan1::V1 { ref __f5, .. } => {
                                    __f3.pass_over(__ctx.cur);
                                }
                                _ => {
                                    __f3.erase(__ctx.cur);
                                    let __f5 =
                                        __ctx.cur.const_exec_text(&[::hypp::ConstOpCode::Text(
                                            "Not toggled",
                                        )])?;
                                    *__f3 = __ConditionalCallbackSpan1::V1 { __f5 };
                                }
                            },
                        }
                        Ok(())
                    };

                    match &mut __f0 {
                        __ConditionalCallbackSpan0::V0 {
                            ref __f1,
                            ref __f2,
                            ref mut __f3,
                            ref __f6,
                            ..
                        } => {
                            // Patch internals of V0
                            __ctx.cur.move_to_children_of(&__f2);
                            __patch_span1(__f3, __ctx)?;
                            // Just pass the nodes instead? We know statically what they will be
                            __ctx.cur.move_to_following_sibling_of(__f6.as_node());
                        }
                        _ => {
                            __f0.erase(__ctx.cur);

                            let __f2 = __ctx.cur.const_exec_element(&[
                                ::hypp::ConstOpCode::EnterElement("button"),
                                ::hypp::ConstOpCode::AttributeName("on_click"),
                            ])?;
                            let __f1 = __ctx.cur.attribute_value_callback()?;
                            let mut __f3 = __ConditionalCallbackSpan1::Erased;
                            __patch_span1(&mut __f3, __ctx)?;
                            let __f6 = __ctx
                                .cur
                                .const_exec_element(&[::hypp::ConstOpCode::ExitElement])?;

                            __ctx.bind.bind(
                                __f1.clone(),
                                ::hypp::ShimMethod::<ConditionalCallback<H>>(&|shim| {
                                    shim.handle_click();
                                }),
                            );

                            *__f0 = __ConditionalCallbackSpan0::V0 {
                                __f1,
                                __f2,
                                __f3,
                                __f6,
                            };
                        }
                    }
                }
                false => match &mut __f0 {
                    __ConditionalCallbackSpan0::V1 { .. } => {}
                    _ => {
                        __f0.erase(__ctx.cur);
                        let __mounted = __ConditionalCallbackSpan0::V1 {};
                        *__f0 = __mounted;
                    }
                },
            }
            Ok(())
        };

        match __root {
            ::hypp::InputOrOutput::Input(span) => {
                __patch_span0(&mut span.__f0, __ctx)?;
            }
            ::hypp::InputOrOutput::Output(span) => {
                do_with_list(__ctx)?;
                do_with_list2(__ctx)?;
                let mut __f0 = __ConditionalCallbackSpan0::Erased;
                __patch_span0(&mut __f0, __ctx)?;
                *span = Some(__ConditionalCallbackSpanRoot { __f0 });
            }
        }

        Ok(())
    }
}
impl<H: ::hypp::Hypp> ::hypp::handle::ToHandle for ConditionalCallback<H> {
    type Handle = ::hypp::handle::Shared<Self>;
}
impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for ConditionalCallback<H> {
    fn is_anchored(&self) -> bool {
        unimplemented!()
    }
    fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
        self.span.pass(__cursor, op)
    }
    fn pass_over(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
        self.__anchor = __cursor.anchor();
        self.pass(__cursor, ::hypp::SpanOp::PassOver)
    }
    fn erase(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>) -> bool {
        self.__weak_self = None;
        self.pass(__cursor, ::hypp::SpanOp::Erase)
    }
}
impl<'p, H: ::hypp::Hypp + 'static> ::hypp::Component<'p, H> for ConditionalCallback<H> {
    type Props = __ConditionalCallbackProps;
    fn pass_props(
        &mut self,
        __ConditionalCallbackProps { show_button, .. }: __ConditionalCallbackProps,
        __cursor: &mut dyn ::hypp::Cursor<H>,
    ) {
        let mut __updates: [bool; 2usize] = [false; 2usize];
        if self.env.show_button != show_button {
            self.env.show_button = show_button;
            __updates[0usize] = true;
        }

        let mut binder = ::hypp::shim::Binder::from_opt_weak(&self.__weak_self);
        Self::patch2(
            ::hypp::InputOrOutput::Input(&mut self.span),
            &self.env,
            &__updates,
            &mut ::hypp::PatchBindCtx {
                cur: __cursor,
                bind: &mut binder,
            },
        )
        .unwrap();
    }
}
impl<'p, H: ::hypp::Hypp + 'static> ::hypp::ShimTrampoline for ConditionalCallback<H> {
    type Shim<'s> = __ConditionalCallbackShim<'s>;
    fn shim_trampoline(&mut self, method: ::hypp::ShimMethod<Self>) {
        let mut __updates: [bool; 2usize] = [false; 2usize];
        let mut shim = __ConditionalCallbackShim {
            show_button: &self.env.show_button,
            toggled: ::hypp::state_ref::StateRef::new(
                &mut self.env.toggled,
                &mut __updates[1usize],
            ),
        };
        method.0(&mut shim);

        let mut cursor = self.__anchor.create_builder();
        let mut binder = ::hypp::shim::Binder::from_opt_weak(&self.__weak_self);
        Self::patch2(
            ::hypp::InputOrOutput::Input(&mut self.span),
            &self.env,
            &__updates,
            &mut ::hypp::PatchBindCtx {
                cur: &mut cursor,
                bind: &mut binder,
            },
        )
        .unwrap();
    }
}

#[test]
fn run() {}
