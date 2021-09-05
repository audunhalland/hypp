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

    fn mount_at_body(&self, component: &impl Component<Self>) -> Result<(), Error> {
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

    pub fn update(&mut self, value: T) -> Option<&T> {
        match &self.value {
            None => {
                self.value = Some(value);
                self.value.as_ref()
            }
            Some(old_value) => {
                if old_value != &value {
                    self.value = Some(value);
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

pub trait Component<Y: Yay> {
    type Props;

    fn mount(&self, mount_point: MountPoint<Y>) -> Result<(), Error>;

    fn update(&mut self, props: Self::Props);
}

#[cfg(test)]
mod tests {
    use super::*;

    use wasm_bindgen_test::*;

    use yay_macros::component;

    #[component(
        Foo,
        <div>
            <p>
                <span>{label}</span>
            </p>
        </div>
    )]
    fn foo(is_cool: bool) {
        let label = if is_cool { "cool" } else { "dull" };
    }

    mod inside {
        use super::*;

        #[component(Bar, <p>"Bar!"</p>)]
        fn bar() {}
    }

    #[component(
        Baz,
        <div>
            <inside::Bar/>
        </div>
    )]
    fn baz() {}

    #[wasm_bindgen_test]
    fn render_foo_web() {
        let yay = web::WebYay::new();
        let mut comp = Foo::new(&yay).unwrap();
        yay.mount_at_body(&comp).unwrap();
        comp.update(FooProps { is_cool: true });
    }

    #[test]
    fn render_foobar_server() {
        let yay = server::ServerYay::new();
        let mut comp = Foo::new(&yay).unwrap();
        yay.mount_at_body(&comp).unwrap();

        comp.update(FooProps { is_cool: true });

        assert_eq!(
            &yay.body().to_string(),
            "<body><div><p><span>cool</span></p></div></body>"
        );

        comp.update(FooProps { is_cool: false });

        assert_eq!(
            &yay.body().to_string(),
            "<body><div><p><span>dull</span></p></div></body>"
        );
    }

    #[test]
    fn render_baz_server() {
        let yay = server::ServerYay::new();
        let mut comp = Baz::new(&yay).unwrap();
        yay.mount_at_body(&comp).unwrap();

        comp.update(BazProps {});

        assert_eq!(
            &yay.body().to_string(),
            "<body><div><p>Bar!</p></div></body>"
        );
    }
}
