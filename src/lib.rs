pub mod dom_index;
pub mod dom_vm;
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

    fn mount_at_body<'p>(&self, component: &impl Component<'p, Self>) -> Result<(), Error> {
        component.mount(MountPoint {
            awe: &self,
            parent: self.body(),
        })
    }

    fn create_element(&self, tag_name: &'static str) -> Self::Element;
    fn create_empty_text(&self) -> Self::Text;

    fn insert_child_before(
        parent: &Self::Element,
        node: &Self::Node,
        before: Option<&Self::Node>,
    ) -> Result<(), Error>;
    fn remove_child(parent: &Self::Element, child: &Self::Node) -> Result<(), Error>;

    fn set_text(node: &Self::Text, text: &str);
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

pub struct MountPoint<'a, 'p, A: Awe> {
    awe: &'a A,
    parent: &'p A::Element,
}

pub struct DomCursor<'n, A: Awe> {
    parent: &'n A::Element,
    child: Option<&'n A::Node>,
}

// BUG: Should be able to know whether something is in th DOM? So more of a "Slot"-like design?
impl<'n, A: Awe> DomCursor<'n, A> {
    /// Insert a node at this cursor position,
    /// then advancing the cursor to look past the inserted node.
    pub fn mount(self, node: &'n A::Node) -> Self {
        // TODO: Should not unwrap()? Error propagation?
        A::insert_child_before(self.parent, node, self.child).unwrap();
        self
    }

    /// Unmount the node at this cursor position,
    /// Then advance the cursor position to look past the inserted node.
    pub fn unmount(self) -> Self {
        if let Some(child) = self.child {
            A::remove_child(self.parent, child).unwrap();
        }
        self
    }

    pub fn next_sibling(self) -> Self {
        Self {
            parent: self.parent,
            child: panic!(),
        }
    }
}

pub trait Component<'p, A: Awe> {
    type Props: 'p;

    fn mount(&self, mount_point: MountPoint<A>) -> Result<(), Error>;

    fn update(&mut self, props: Self::Props);
}

pub struct PhantomProp;

mod debugging {
    //use super::*;
}

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
        let mut comp = Foo::new(&awe).unwrap();
        awe.mount_at_body(&comp).unwrap();
        comp.update(FooProps {
            is_cool: true,
            __phantom: std::marker::PhantomData,
        });
    }

    #[test]
    fn render_foo_server() {
        let awe = server::ServerAwe::new();
        let mut comp = Foo::new(&awe).unwrap();
        awe.mount_at_body(&comp).unwrap();

        comp.update(FooProps {
            is_cool: true,
            __phantom: std::marker::PhantomData,
        });

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

    #[component_dbg(
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
        let mut comp = Baz::new(&awe).unwrap();
        awe.mount_at_body(&comp).unwrap();

        comp.update(BazProps {
            __phantom: std::marker::PhantomData,
        });

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

                fn mount(&self, mount_point: MountPoint<A>) -> Result<(), Error> {
                    panic!()
                }

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

            fn mount(&self, mount_point: MountPoint<A>) -> Result<(), Error> {
                panic!()
            }

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
