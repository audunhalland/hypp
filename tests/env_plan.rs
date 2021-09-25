#![feature(generic_associated_types)]

/// For pasting the output of component_dbg! .. and debugging it!
#[allow(unused_imports)]
use hypp::prelude::*;

/// NEW CODE: Env for storing props+state
struct Env<H: ::hypp::Hypp> {
    show_button: bool,
    toggled: bool,
    list: Vec<u32>,
    __phantom: std::marker::PhantomData<H>,
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
    Init,
    V0 {
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
            Self::Init => false,
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
            Self::Init => {}
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
        self.pass(__cursor, ::hypp::SpanOp::Erase)
    }
}
enum __ConditionalCallbackSpan1<H: ::hypp::Hypp> {
    Init,
    V0 { __f4: H::Text },
    V1 { __f5: H::Text },
}
impl<H: ::hypp::Hypp + 'static> ::hypp::Span<H> for __ConditionalCallbackSpan1<H> {
    fn is_anchored(&self) -> bool {
        false
    }
    fn pass(&mut self, __cursor: &mut dyn ::hypp::Cursor<H>, op: ::hypp::SpanOp) -> bool {
        match self {
            Self::Init => false,
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
}

#[allow(dead_code)]
pub struct ConditionalCallback<H: ::hypp::Hypp> {
    env: Env<H>,
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
            __phantom: std::marker::PhantomData,
        };

        let mut span = None;
        Self::patch2(
            ::hypp::InputOrOutput::Output(&mut span),
            &env,
            &__updates,
            __cursor,
            &mut __binder,
        );

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
        __env: &Env<H>,
        __updates: &[bool],
        __cursor: &mut dyn ::hypp::Cursor<H>,
        __bind: &mut dyn ::hypp::BindCallback<H, ConditionalCallback<H>>,
    ) -> Result<(), ::hypp::Error> {
        let show_button = __env.show_button;
        let toggled = __env.toggled;
        let list = &__env.list;

        let mut do_with_list = |__cursor: &mut dyn ::hypp::Cursor<H>| -> Result<(), ::hypp::Error> {
            for item in list.iter() {}
            Ok(())
        };

        let mut do_with_list2 =
            |__cursor: &mut dyn ::hypp::Cursor<H>| -> Result<(), ::hypp::Error> {
                for item in list.iter() {}
                Ok(())
            };

        let mut patch_span0 = |mut __f0: &mut __ConditionalCallbackSpan0<H>,
                               __cursor: &mut dyn ::hypp::Cursor<H>|
         -> Result<(), ::hypp::Error> {
            match show_button {
                // Single pattern here!
                true => {
                    let mount_span1_v0 = |__cursor: &mut dyn ::hypp::Cursor<H>| -> Result<
                        __ConditionalCallbackSpan1<H>,
                        ::hypp::Error,
                    > {
                        let __f4 = __cursor.const_exec_text(&__CONDITIONALCALLBACK_PRG1)?;
                        Ok(__ConditionalCallbackSpan1::V0 { __f4 })
                    };

                    let mount_span1_v1 = |__cursor: &mut dyn ::hypp::Cursor<H>| -> Result<
                        __ConditionalCallbackSpan1<H>,
                        ::hypp::Error,
                    > {
                        let __f5 = __cursor.const_exec_text(&__CONDITIONALCALLBACK_PRG2)?;
                        Ok(__ConditionalCallbackSpan1::V1 { __f5 })
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
                            __cursor.move_to_children_of(&__f2);
                            match toggled {
                                true => {
                                    match &__f3 {
                                        __ConditionalCallbackSpan1::V0 { ref __f4, .. } => {
                                            // Can do this
                                            __f3.pass_over(__cursor);
                                            // Instead of this:
                                            // __cursor.move_to_following_sibling_of(__f4.as_node());
                                        }
                                        __ConditionalCallbackSpan1::Init
                                        | __ConditionalCallbackSpan1::V1 { .. } => {
                                            __f3.erase(__cursor);
                                            *__f3 = mount_span1_v0(__cursor)?;
                                        }
                                    }
                                }
                                false => match &__f3 {
                                    __ConditionalCallbackSpan1::V1 { ref __f5, .. } => {
                                        __f3.pass_over(__cursor);
                                    }
                                    __ConditionalCallbackSpan1::Init
                                    | __ConditionalCallbackSpan1::V0 { .. } => {
                                        __f3.erase(__cursor);
                                        *__f3 = mount_span1_v1(__cursor)?;
                                    }
                                },
                            }
                            // Just pass the nodes instead? We know statically what they will be
                            __cursor.move_to_following_sibling_of(__f6.as_node());
                        }
                        __ConditionalCallbackSpan0::Init
                        | __ConditionalCallbackSpan0::V1 { .. } => {
                            __f0.erase(__cursor);

                            let __f2 = __cursor.const_exec_element(&__CONDITIONALCALLBACK_PRG0)?;
                            let __f1 = __cursor.attribute_value_callback().unwrap();
                            let __f3 = match toggled {
                                true => mount_span1_v0(__cursor)?,
                                false => mount_span1_v1(__cursor)?,
                            };
                            let __f6 = __cursor.const_exec_element(&__CONDITIONALCALLBACK_PRG3)?;

                            __bind.bind(
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
                    __ConditionalCallbackSpan0::Init | __ConditionalCallbackSpan0::V0 { .. } => {
                        __f0.erase(__cursor);
                        let __mounted = __ConditionalCallbackSpan0::V1 {};
                        *__f0 = __mounted;
                    }
                },
            }
            Ok(())
        };

        match __root {
            ::hypp::InputOrOutput::Output(span) => {
                let mut __f0 = __ConditionalCallbackSpan0::Init;
                do_with_list(__cursor);
                do_with_list2(__cursor);
                patch_span0(&mut __f0, __cursor)?;
                *span = Some(__ConditionalCallbackSpanRoot { __f0 });
            }
            ::hypp::InputOrOutput::Input(span) => {
                patch_span0(&mut span.__f0, __cursor)?;
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
            __cursor,
            &mut binder,
        );
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
            &mut cursor,
            &mut binder,
        );
    }
}

#[test]
fn run() {}
