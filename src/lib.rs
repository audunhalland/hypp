use std::marker::PhantomData;

// pub mod dom_index;
pub mod error;
pub mod server;
pub mod web;

use error::Error;

pub trait AsNode<Y: Awe> {
    fn as_node(&self) -> &Y::Node;
}

pub trait Awe: Sized {
    type Node: Clone;
    type Element: Clone + AsNode<Self>;
    type Text: Clone + AsNode<Self>;

    fn remove_child(parent: &Self::Element, child: &Self::Node) -> Result<(), Error>;

    fn set_text(node: &Self::Text, text: &str);
}

///
/// DomVM
///
/// An abstract cursor at some DOM position, allowing users of the trait
/// to do various operations to mutate it.
///
pub trait DomVM<'doc, A: Awe> {
    /// Enter an element at the current location, and return it.
    /// The cursor position moves into that element's first child.
    fn enter_element(&mut self, tag_name: &'static str) -> Result<A::Element, Error>;

    /// Define an attribute on the current element (and return it perhaps?)
    fn attribute(&mut self, name: &'static str, value: &'static str) -> Result<(), Error>;

    /// Define a text. The cursor moves past this text.
    fn text(&mut self, text: &str) -> Result<A::Text, Error>;

    /// Exit the current element, advancing the cursor position to
    /// the element's next sibling.
    fn exit_element(&mut self) -> Result<(), Error>;

    /// Remove the element at the current cursor position.
    /// The element's name must match the tag name passed.
    /// The cursor moves to point at the next sibling.
    fn remove_element(&mut self, tag_name: &'static str) -> Result<(), Error>;

    /// Push context.
    /// Does not mutate the DOM.
    /// This means we can skip parts of the DOM tree, moving the cursor directly
    /// into this element's first child.
    fn push_element_context(&mut self, element: A::Element);

    /// Pop element context,
    /// Restoring the state to what it was before `push_element_context`.
    fn pop_element_context(&mut self);
}

pub struct State<T: Default> {
    pub value: T,
}

pub struct Var<T: Eq> {
    value: Option<T>,
}

impl<T: Eq> Var<T> {
    pub fn new() -> Self {
        Self { value: None }
    }

    pub fn update<V>(&mut self, value: V) -> Option<&T>
    where
        T: From<V> + PartialEq<V>,
    {
        match &self.value {
            None => {
                self.value = Some(T::from(value));
                self.value.as_ref()
            }
            Some(old_value) => {
                if old_value != &value {
                    self.value = Some(T::from(value));
                    self.value.as_ref()
                } else {
                    None
                }
            }
        }
    }
}

pub trait Component<'p, A: Awe> {
    type Props: 'p;

    /// Update the component, synchronizing its resulting
    /// state using the DomVM.
    fn update(&mut self, _props: Self::Props, __vm: &mut dyn DomVM<A>) {}

    /// Unmount the component, removing all its nodes
    /// from under its mount point in the tree, using the DomVM.
    fn unmount(&mut self, __vm: &mut dyn DomVM<A>);
}

pub type PhantomProp<'p> = PhantomData<&'p ()>;
pub type PhantomField<A> = PhantomData<A>;

mod debugging {}

#[cfg(test)]
mod tests {
    use super::*;

    use wasm_bindgen_test::*;

    use yay_macros::*;

    #[component(
        <div>
            <p>
                <span>{label}</span>
            </p>
        </div>
    )]
    fn Foo(is_cool: bool) {
        let label = if is_cool { "cool" } else { "dull" };
    }

    #[wasm_bindgen_test]
    fn render_foo_web() {
        let awe = web::WebAwe::new();
        let _comp = Foo::new(
            FooProps {
                is_cool: true,
                __phantom: std::marker::PhantomData,
            },
            &mut awe.builder_at_body(),
        )
        .unwrap();
    }

    #[test]
    fn render_foo_server() {
        let awe = server::ServerAwe::new();
        let mut comp = Foo::new(
            FooProps {
                is_cool: true,
                __phantom: std::marker::PhantomData,
            },
            &mut awe.builder_at_body(),
        )
        .unwrap();

        assert_eq!(
            &awe.render(),
            "<body><div><p><span>cool</span></p></div></body>"
        );

        comp.update(
            FooProps {
                is_cool: false,
                __phantom: std::marker::PhantomData,
            },
            &mut awe.builder_at_body(),
        );

        assert_eq!(
            &awe.render(),
            "<body><div><p><span>dull</span></p></div></body>"
        );
    }

    mod inside {
        use super::*;

        #[component(<p>{text}</p>)]
        fn P1(text: &'p str) {}

        #[component(<p>"static"</p>)]
        fn P2() {}
    }

    #[component(
        <div>
            // En kommentar
            <inside::P1 text="variable"/>
            <inside::P2/>
        </div>
    )]
    fn Baz() {}

    #[test]
    fn render_baz_server() {
        let awe = server::ServerAwe::new();
        Baz::new(
            BazProps {
                __phantom: std::marker::PhantomData,
            },
            &mut awe.builder_at_body(),
        )
        .unwrap();

        assert_eq!(
            &awe.render(),
            "<body><div><p>variable</p><p>static</p></div></body>"
        );
    }

    #[component(
        <div>
            /*
            if draw_stuff {
                <div>
                </div>
            }
            */
        </div>
    )]
    fn Conditional(draw_stuff: bool) {}

    mod expr_conditional {
        use super::*;

        mod inner {
            use super::*;

            pub struct InnerProps<'p> {
                pub str: &'p str,
            }

            pub struct Inner<A: Awe> {
                element: A::Element,
            }

            impl<A: Awe> Inner<A> {
                pub fn new(props: InnerProps, __vm: &mut dyn DomVM<A>) -> Result<Self, Error> {
                    let element = __vm.enter_element("span")?;
                    __vm.exit_element()?;
                    Ok(Self { element })
                }
            }

            impl<'p, A: Awe> Component<'p, A> for Inner<A> {
                type Props = InnerProps<'p>;

                fn unmount(&mut self, __vm: &mut dyn DomVM<A>) {
                    __vm.remove_element("span").unwrap();
                }
            }

            pub enum InnerVariant<A: Awe> {
                V0(inner::Inner<A>),
                V1,
            }

            impl<A: Awe> InnerVariant<A> {
                pub fn reset(&mut self, variant: Self, __vm: &mut dyn DomVM<A>) {
                    match self {
                        Self::V0(comp) => comp.unmount(__vm),
                        Self::V1 => {}
                    }
                    *self = variant;
                }
            }
        }

        struct CondProps<'p> {
            maybe_str: Option<&'p str>,
        }

        struct Cond<A: Awe> {
            inner_variant: inner::InnerVariant<A>,
        }

        impl<'p, A: Awe> Component<'p, A> for Cond<A> {
            type Props = CondProps<'p>;

            fn update(&mut self, CondProps { maybe_str, .. }: CondProps, __vm: &mut dyn DomVM<A>) {
                match (&mut self.inner_variant, maybe_str) {
                    // First variants where props and instance correlate:
                    (inner::InnerVariant::V0(variant), Some(str)) => {
                        // FIXME: Add the doc method
                        variant.update(inner::InnerProps { str }, __vm);
                    }
                    // Then the "reset" variants
                    (_, Some(str)) => {
                        self.inner_variant.reset(
                            inner::InnerVariant::V0(
                                inner::Inner::new(inner::InnerProps { str }, __vm).unwrap(),
                            ),
                            __vm,
                        );
                    }
                    (_, None) => {
                        self.inner_variant.reset(inner::InnerVariant::V1, __vm);
                    }
                    _ => {}
                }
            }

            fn unmount(&mut self, __vm: &mut dyn DomVM<A>) {
                panic!();
            }
        }
    }
}
