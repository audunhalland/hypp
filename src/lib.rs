pub mod error;
pub mod server;
pub mod web;

use error::Error;

pub trait AsNode<Y: Yay> {
    fn as_node(&self) -> &Y::Node;
}

pub trait Yay: Sized {
    type Node: Clone;
    type Element: Clone + AsNode<Self>;
    type Text: Clone + AsNode<Self>;

    fn body(&self) -> &Self::Element;

    fn mount_at_body<'p>(&self, component: &impl Component<'p, Self>) -> Result<(), Error> {
        component.mount(MountPoint {
            yay: &self,
            parent: self.body(),
        })
    }

    fn create_element(&self, tag_name: &'static str) -> Self::Element;
    fn create_empty_text(&self) -> Self::Text;

    fn append_child(parent: &Self::Element, child: &Self::Node) -> Result<(), Error>;
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

pub struct MountPoint<'y, 'p, Y: Yay> {
    yay: &'y Y,
    parent: &'p Y::Element,
}

pub trait Component<'p, Y: Yay> {
    type Props: 'p;

    fn mount(&self, mount_point: MountPoint<Y>) -> Result<(), Error>;

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
        let yay = web::WebYay::new();
        let mut comp = Foo::new(&yay).unwrap();
        yay.mount_at_body(&comp).unwrap();
        comp.update(FooProps {
            is_cool: true,
            __phantom: std::marker::PhantomData,
        });
    }

    #[test]
    fn render_foo_server() {
        let yay = server::ServerYay::new();
        let mut comp = Foo::new(&yay).unwrap();
        yay.mount_at_body(&comp).unwrap();

        comp.update(FooProps {
            is_cool: true,
            __phantom: std::marker::PhantomData,
        });

        assert_eq!(
            &yay.body().to_string(),
            "<body><div><p><span>cool</span></p></div></body>"
        );

        comp.update(FooProps {
            is_cool: false,
            __phantom: std::marker::PhantomData,
        });

        assert_eq!(
            &yay.body().to_string(),
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
            <inside::P1 text="variable"/>
            <inside::P2/>
        </div>
    )]
    fn Baz() {}

    #[test]
    fn render_baz_server() {
        let yay = server::ServerYay::new();
        let mut comp = Baz::new(&yay).unwrap();
        yay.mount_at_body(&comp).unwrap();

        comp.update(BazProps {
            __phantom: std::marker::PhantomData,
        });

        assert_eq!(
            &yay.body().to_string(),
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
}
