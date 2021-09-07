use std::marker::PhantomData;

pub mod dom_index;
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

    fn body(&self) -> &Self::Element;

    fn remove_child(parent: &Self::Element, child: &Self::Node) -> Result<(), Error>;

    fn set_text(node: &Self::Text, text: &str);
}

pub trait DomVM<A: Awe> {
    fn enter_element(&mut self, tag_name: &'static str) -> Result<A::Element, Error>;
    fn attribute(&mut self, name: &'static str, value: &'static str) -> Result<(), Error>;
    fn text(&mut self, text: &str) -> Result<A::Text, Error>;
    fn leave_element(&mut self) -> Result<(), Error>;
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

    fn update(&mut self, props: Self::Props);
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
            &awe.body().to_string(),
            "<body><div><p><span>cool</span></p></div></body>"
        );

        comp.update(FooProps {
            is_cool: false,
            __phantom: std::marker::PhantomData,
        });

        assert_eq!(
            &awe.body().to_string(),
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
            &awe.body().to_string(),
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

            pub struct Inner<Y: Awe> {
                root: Y::Element,
            }

            impl<Y: Awe> Inner<Y> {
                pub fn new() -> Self {
                    unimplemented!()
                }
            }

            impl<'p, A: Awe> Component<'p, A> for Inner<A> {
                type Props = InnerProps<'p>;

                fn update(&mut self, Self::Props { str, .. }: Self::Props) {}
            }

            pub enum InnerVariant<Y: Awe> {
                One(inner::Inner<Y>),
                Nothing,
            }

            impl<'p, A: Awe> InnerVariant<A> {
                pub fn update_variant_one(&mut self, props: inner::InnerProps<'p>) {
                    let instance = match self {
                        Self::One(instance) => instance,
                        _ => {
                            self.unmount();
                            *self = Self::One(inner::Inner::new());
                            match self {
                                Self::One(instance) => instance,
                                _ => panic!(),
                            }
                        }
                    };

                    instance.update(props);
                }

                pub fn update_variant_nothing(&mut self) {
                    match self {
                        Self::Nothing => {}
                        _ => {
                            // TODO: Unmount
                            self.unmount();
                            *self = Self::Nothing;
                        }
                    }
                }

                fn unmount(&mut self) {}
            }
        }

        struct CondProps<'p> {
            maybe_str: Option<&'p str>,
        }

        struct Cond<Y: Awe> {
            inner_variant: inner::InnerVariant<Y>,
        }

        impl<'p, A: Awe> Component<'p, A> for Cond<A> {
            type Props = CondProps<'p>;

            fn update(&mut self, Self::Props { maybe_str, .. }: Self::Props) {
                if let Some(str) = maybe_str {
                    self.inner_variant
                        .update_variant_one(inner::InnerProps { str });
                } else {
                    self.inner_variant.update_variant_nothing();
                }
            }
        }
    }
}
