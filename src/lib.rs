#![forbid(unsafe_code)]

//!
//! The hypp crate allows expressing a reactive DOM.
//!
//! The philosophy is to precompile as much as we can.
//!

use std::marker::PhantomData;

pub mod error;
pub mod server;
pub mod web;

pub use error::Error;
pub use hypp_macros::component;

///
/// "upcast" a DOM node of a specific type to its generic type
///
pub trait AsNode<H: Hypp> {
    fn as_node(&self) -> &H::Node;
}

///
/// Main abstraction for hypp (as of writing)
///
/// We abstract over the type of DOM we are targeting.
///
pub trait Hypp: Sized {
    type Node: Clone;
    type Element: Clone + AsNode<Self>;
    type Text: Clone + AsNode<Self>;

    fn set_text(node: &Self::Text, text: &str);
}

///
/// Small program instructions for manipulating DOM.
///
/// The const opcodes are not lifetime constrained, and thus
/// may be allocated in static memory.
///
pub enum ConstOpCode {
    /// Enter an element at the current location, and return it.
    /// The cursor position moves into that element's first child.
    EnterElement(&'static str),

    /// Define an attribute on the current element,
    Attribute(&'static str, &'static str),

    /// Define a text. The cursor moves past this text.
    Text(&'static str),

    /// Exit the current element, advancing the cursor position to
    /// the element's next sibling.
    ExitElement,

    /// Remove the element at the current cursor position.
    /// The element's name must match the tag name passed.
    /// The cursor moves to point at the next sibling.
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
    /// The last opcode must produce an element.
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<H::Element, Error>;

    /// Execute a series of opcodes.
    /// The last opcode must produce a text node.
    fn const_exec_text(&mut self, program: &[ConstOpCode]) -> Result<H::Text, Error>;

    /// Define a text. The cursor moves past this text.
    fn text(&mut self, text: &str) -> Result<H::Text, Error>;

    /// Advance cursor directly to a location
    fn advance_to_first_child_of(&mut self, element: &H::Element);

    /// Advance cursor directly to a location
    fn advance_to_next_sibling_of(&mut self, node: &H::Node);

    /// Remove the element at the current cursor position.
    /// The element's name must match the tag name passed.
    /// The cursor moves to point at the next sibling.
    fn remove_element(&mut self, tag_name: &'static str) -> Result<H::Element, Error>;

    fn remove_text(&mut self) -> Result<H::Text, Error>;

    /// Advance the cursor, according to the const program passed.
    /// Don't mutate anything.
    /// The DOM described by the program must match the actual DOM.
    fn skip_const_program(&mut self, program: &[ConstOpCode]);
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

    ///
    /// Set new props on the component instance, causing an immediate, synchronous
    /// DOM update if the component determines that anything changes.
    ///
    /// The __vm cursor __must__ point to the component start.
    /// After the patch is finished, the __vm cursor __must__
    /// point to the position after the component.
    ///
    fn set_props(&mut self, props: Self::Props, __vm: &mut dyn DomVM<H>);

    /// Unmount the component, removing all its nodes
    /// from under its mount point in the tree, using the DomVM.
    /// The only purpose of this call is to clean up nodes in the DOM.
    fn unmount(&self, __vm: &mut dyn DomVM<H>);
}

pub type PhantomProp<'p> = PhantomData<&'p ()>;
pub type PhantomField<A> = PhantomData<A>;

#[allow(unused_imports)]
mod debugging {
    use super::*;
}

#[cfg(test)]
mod tests {
    use super::*;

    use wasm_bindgen_test::*;

    #[allow(unused_imports)]
    use hypp_macros::component_dbg;

    component! {
        Foo(is_cool: bool) {}

        // let label = if is_cool { "cool" } else { "dull" };

        <div>
            <p>
                <span>
                    // FIXME: Need some way to define this as a temporary variable?
                    // maybe let bindings before the template?
                    if is_cool {
                        "cool"
                    } else {
                        "dull"
                    }
                </span>
            </p>
        </div>
    }

    #[wasm_bindgen_test]
    fn render_foo_web() {
        let hypp = web::WebHypp::new();
        let _comp = Foo::mount(
            FooProps {
                is_cool: true,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        )
        .unwrap();
    }

    #[test]
    fn render_foo_server() {
        let hypp = server::ServerHypp::new();
        let mut c = Foo::mount(
            FooProps {
                is_cool: true,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        )
        .unwrap();

        assert_eq!(
            hypp.render(),
            "<body><div><p><span>cool</span></p></div></body>"
        );

        c.set_props(
            FooProps {
                is_cool: false,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(
            &hypp.render(),
            "<body><div><p><span>dull</span></p></div></body>"
        );
    }

    mod inside {
        use super::*;

        component! {
            P1(text: &str) {}

            <p>{text}</p>
        }

        component! {
            P2() {}

            <p>"static"</p>
        }
    }

    component! {
        Baz() {}

        <div>
            // En kommentar
            <inside::P1 text="variable"/>
            <inside::P2/>
        </div>
    }

    #[test]
    fn render_baz_server() {
        let hypp = server::ServerHypp::new();
        Baz::mount(
            BazProps {
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        )
        .unwrap();

        assert_eq!(
            &hypp.render(),
            "<body><div><p>variable</p><p>static</p></div></body>"
        );
    }

    component! {
        Conditional(hello: bool, world: bool) {}

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
    }

    #[test]
    fn render_conditional_server() {
        let hypp = server::ServerHypp::new();
        let mut c = Conditional::mount(
            ConditionalProps {
                hello: false,
                world: false,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        )
        .unwrap();

        assert_eq!(hypp.render(), "<body><div/></body>");

        c.set_props(
            ConditionalProps {
                hello: false,
                world: true,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        // No change:
        assert_eq!(hypp.render(), "<body><div/></body>");

        c.set_props(
            ConditionalProps {
                hello: true,
                world: false,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(
            hypp.render(),
            "<body><div><span>Hello</span><span>Universe</span></div></body>"
        );

        c.set_props(
            ConditionalProps {
                hello: true,
                world: true,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(
            hypp.render(),
            "<body><div><span>Hello</span><a>World</a></div></body>"
        );

        c.unmount(&mut hypp.builder_at_body());

        assert_eq!(hypp.render(), "<body/>");
    }

    component! {
        ConditionalWithVariableText(hello: bool, yo: &str) {}

        <div>
        if hello {
            {yo}
        }
        </div>
    }

    component! {
        ConditionalWithComponent(hello: bool) {}

        <div>
        if hello {
            <Baz />
        }
        </div>
    }

    component! {
        ITakeANumber(number: u32) {}

        "num"
    }

    component! {
        IfLet(opt_number: Option<u32>) {}

        <article>
            if let Some(number) = opt_number {
                <ITakeANumber number={number} />
            }
        </article>
    }

    #[test]
    fn render_iflet_server() {
        let hypp = server::ServerHypp::new();
        let mut c = IfLet::mount(
            IfLetProps {
                opt_number: None,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        )
        .unwrap();

        assert_eq!(hypp.render(), "<body><article/></body>");

        c.set_props(
            IfLetProps {
                opt_number: Some(42),
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(hypp.render(), "<body><article>num</article></body>");

        c.set_props(
            IfLetProps {
                opt_number: None,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(hypp.render(), "<body><article/></body>");

        c.unmount(&mut hypp.builder_at_body());

        assert_eq!(hypp.render(), "<body/>");
    }

    component! {
        OptionString(opt_str: Option<&str>) {}

        <article>
            if let Some(str) = opt_str {
                <p>{str}</p>
            }
        </article>
    }

    component! {
        Fragment1(perhaps: bool) {}

        <div>"first"</div>
        if perhaps {
            <span>"second"</span>
        }
        <p>"third"</p>
    }

    #[test]
    fn render_fragment1() {
        let hypp = server::ServerHypp::new();
        let mut c = Fragment1::mount(
            Fragment1Props {
                perhaps: true,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        )
        .unwrap();

        assert_eq!(
            hypp.render(),
            "<body><div>first</div><span>second</span><p>third</p></body>"
        );

        c.set_props(
            Fragment1Props {
                perhaps: false,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(hypp.render(), "<body><div>first</div><p>third</p></body>");

        c.unmount(&mut hypp.builder_at_body());

        assert_eq!(hypp.render(), "<body/>");
    }

    component! {
        Recursive(depth: usize) {}

        <span>
        if depth > 1 {
            <Recursive depth={depth - 1} />
        }
        </span>
    }

    #[test]
    fn render_recursive_server() {
        let hypp = server::ServerHypp::new();
        let mut c = Recursive::mount(
            RecursiveProps {
                depth: 3,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        )
        .unwrap();

        assert_eq!(
            hypp.render(),
            "<body><span><span><span/></span></span></body>"
        );

        c.set_props(
            RecursiveProps {
                depth: 2,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(hypp.render(), "<body><span><span/></span></body>");

        c.unmount(&mut hypp.builder_at_body());

        assert_eq!(hypp.render(), "<body/>");
    }

    // doesn't work yet
    component! {
        List(_items: &[String]) {}

        <ul>
            for item in items {
                <li>{item}</li>
            }
        </ul>
    }

    component! {
        TopLevelConditional(lol: bool, text: &str) {}

        if lol {
            {text}
        } else {
            <p>"goo"</p>
        }
    }

    component! {
        StringProp1(arg: &str) {}
        <p>{arg}</p>
    }

    component! {
        StringProp2(arg: &str) {}
        <div>{arg}</div>
    }

    component! {
        ConditionalStringProp(arg: &str, draw_one: bool) {}

        if draw_one {
            <StringProp1 arg={arg} />
        } else {
            <StringProp2 arg={arg} />
        }
    }

    // Experimentation with new surface syntax
    component! {
        Stuff(prop1: bool, prop2: &str) {
            state: bool,
        }

        fn update(&mut self) {
            if self.prop1 {
                self.state = false;
            }
        }

        fn handle_click(&mut self, event: SomeType) {
        }

        <div>
            if prop1 {
                <p>"yep"</p>
            }
        </div>
        <div>
            <button on_click={Self::handle_click}>
                "A button"
            </button>
        </div>
    }
}
