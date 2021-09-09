use std::marker::PhantomData;

// pub mod dom_index;
pub mod error;
pub mod server;
pub mod web;

use error::Error;

pub trait AsNode<H: Hypp> {
    fn as_node(&self) -> &H::Node;
}

pub trait Hypp: Sized {
    type Node: Clone;
    type Element: Clone + AsNode<Self>;
    type Text: Clone + AsNode<Self>;

    fn remove_child(parent: &Self::Element, child: &Self::Node) -> Result<(), Error>;

    fn set_text(node: &Self::Text, text: &str);
}

pub enum ConstOpCode {
    EnterElement(&'static str),
    TextAttribute(&'static str, &'static str),
    Text(&'static str),
    ExitElement,
    RemoveElement(&'static str),
}

///
/// DomVM
///
/// An abstract cursor at some DOM position, allowing users of the trait
/// to do various operations to mutate it.
///
pub trait DomVM<'doc, H: Hypp> {
    /// Execute a series of opcodes.
    /// The last opcode should produce an element.
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<H::Element, Error>;

    /// Execute a series of opcodes.
    /// The last opcode should produce a text node.
    fn const_exec_text(&mut self, program: &[ConstOpCode]) -> Result<H::Text, Error>;

    /// Enter an element at the current location, and return it.
    /// The cursor position moves into that element's first child.
    fn enter_element(&mut self, tag_name: &'static str) -> Result<H::Element, Error>;

    /// Define an attribute on the current element (and return it perhaps?)
    fn attribute(&mut self, name: &'static str, value: &'static str) -> Result<(), Error>;

    /// Define a text. The cursor moves past this text.
    fn text(&mut self, text: &str) -> Result<H::Text, Error>;

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
    fn push_element_context(&mut self, element: H::Element);

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

pub trait Component<'p, H: Hypp> {
    type Props: 'p;

    /// Update the component, synchronizing its resulting
    /// state using the DomVM.
    fn update(&mut self, _props: Self::Props, __vm: &mut dyn DomVM<H>) {}

    /// Unmount the component, removing all its nodes
    /// from under its mount point in the tree, using the DomVM.
    fn unmount(&mut self, __vm: &mut dyn DomVM<H>);
}

pub type PhantomProp<'p> = PhantomData<&'p ()>;
pub type PhantomField<A> = PhantomData<A>;

mod debugging {}

#[cfg(test)]
mod tests {
    use super::*;

    use wasm_bindgen_test::*;

    use hypp_macros::*;

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
        let awe = web::WebHypp::new();
        let _comp = Foo::mount(
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
        let awe = server::ServerHypp::new();
        let mut comp = Foo::mount(
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
        let awe = server::ServerHypp::new();
        Baz::mount(
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
            if hello {
                <span>"Hello"</span>
                if world {
                    <a>"World"</a>
                } else {
                    <span>"Universe"</span>
                }
            }
        </div>
    )]
    fn Conditional(hello: bool, world: bool) {}

    #[test]
    fn render_conditional_server() {
        let awe = server::ServerHypp::new();
        let mut builder = awe.builder_at_body();
        let mut comp = Conditional::mount(
            ConditionalProps {
                hello: false,
                world: false,
                __phantom: std::marker::PhantomData,
            },
            &mut builder,
        )
        .unwrap();

        assert_eq!(&awe.render(), "<body><div/></body>");

        comp.update(
            ConditionalProps {
                hello: false,
                world: true,
                __phantom: std::marker::PhantomData,
            },
            &mut builder,
        );

        // No change:
        assert_eq!(&awe.render(), "<body><div/></body>");

        comp.update(
            ConditionalProps {
                hello: true,
                world: false,
                __phantom: std::marker::PhantomData,
            },
            &mut builder,
        );

        assert_eq!(&awe.render(), "<body><div><span>Hello</span></div></body>");

        comp.update(
            ConditionalProps {
                hello: true,
                world: true,
                __phantom: std::marker::PhantomData,
            },
            &mut builder,
        );

        assert_eq!(
            &awe.render(),
            "<body><div><span>Hello</span><a>World</a></div></body>"
        );

        comp.unmount(&mut builder);

        assert_eq!(&awe.render(), "<body/>");
    }

    mod test_code {}
}
