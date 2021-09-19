#![forbid(unsafe_code)]
#![feature(generic_associated_types)]

//!
//! The hypp crate allows expressing a reactive DOM.
//!
//! The philosophy is to precompile as much as we can.
//!

use std::marker::PhantomData;

pub mod error;
pub mod handle;
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

    type Callback: Callback;

    fn set_text(node: &Self::Text, text: &str);
}

pub trait Callback {
    fn bind(&mut self, function: Box<dyn Fn()>);

    fn release(&mut self);
}

///
/// Small program instructions for manipulating DOM.
///
/// The const opcodes are not lifetime constrained, and thus
/// may be "allocated" in static memory.
///
pub enum ConstOpCode {
    /// Enter an element at the current location, and return it.
    /// The cursor position moves into that element's first child.
    /// The "return value" of this opcode is the element produced.
    EnterElement(&'static str),

    /// Moves the cursor to a specific attribute name within the current element.
    /// This opcode has no return value, and has no effect on the return value of the whole program.
    AttributeName(&'static str),

    /// Define text-based attribute value on the current attribute.
    /// This opcode has no return value, and has no effect on the return value of the whole program.
    AttributeTextValue(&'static str),

    /// Define a text node. The cursor moves past this text.
    /// The "return value" of this opcode is the text node produced.
    Text(&'static str),

    /// Exit the current element, advancing the cursor position to
    /// the element's next sibling.
    /// The "return value" of this opcode is the element just exited.
    ExitElement,

    /// Remove the element at the current cursor position.
    /// The element's name must match the tag name passed.
    /// The cursor moves to point at the next sibling.
    /// The "return value" of this opcode is the element that has been removed.
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
    /// The last node opcode must produce an element.
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<H::Element, Error>;

    /// Execute a series of opcodes.
    /// The last node opcode must produce a text node.
    fn const_exec_text(&mut self, program: &[ConstOpCode]) -> Result<H::Text, Error>;

    /// Set up a callback bound to the current attribute.
    fn attribute_value_callback(&mut self) -> Result<H::Callback, Error>;

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

pub trait Component<'p, H: Hypp>: Sized + handle::ToHandle {
    /// The type of properties this component recieves
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
    fn unmount(&mut self, __vm: &mut dyn DomVM<H>);
}

pub type PhantomProp<'p> = PhantomData<&'p ()>;
pub type PhantomField<A> = PhantomData<A>;

/*
    CALLBACK HANDLING:

            let __f2 = __vm.const_exec_element(&STUFF_PRG1)?;
            let mut on_click = __vm.attribute_value_callback()?;
            let __f0 = match prop1 {
                true => {
                    let __f1 = __vm.const_exec_element(&STUFF_PRG0)?;
                    StuffEnum0::V0 {
                        __f1,
                        __phantom: std::marker::PhantomData,
                    }
                }
                false => StuffEnum0::V1 {
                    __phantom: std::marker::PhantomData,
                },
            };
            let __f3 = __vm.const_exec_element(&STUFF_PRG2)?;
            let __self = std::rc::Rc::new(std::cell::RefCell::new(Self {
                __f2,
                __f0,
                prop1,
                prop2: prop2.to_owned(),
                state,
                __f3,
                __phantom: std::marker::PhantomData,
            }));

            {
                let __self = __self.clone();
                on_click.bind(Box::new(move || {
                    __self.borrow_mut().handle_click();
                }));
            }

            Ok(handle::Shared::new(__self))
*/

#[allow(unused_imports)]
mod debugging {
    use super::handle::Handle;
    use super::*;
}

#[cfg(test)]
mod tests {
    use super::handle::Handle;
    use super::*;

    use wasm_bindgen_test::*;

    #[allow(unused_imports)]
    use hypp_macros::component_dbg;

    component! {
        Foo(is_cool: bool) {}

        // let label = if is_cool { "cool" } else { "dull" };

        <div>
            <p class="css">
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
            "<body><div><p class=\"css\"><span>cool</span></p></div></body>"
        );

        c.borrow_mut().set_props(
            FooProps {
                is_cool: false,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(
            &hypp.render(),
            "<body><div><p class=\"css\"><span>dull</span></p></div></body>"
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

        c.borrow_mut().set_props(
            ConditionalProps {
                hello: false,
                world: true,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        // No change:
        assert_eq!(hypp.render(), "<body><div/></body>");

        c.borrow_mut().set_props(
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

        c.borrow_mut().set_props(
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

        c.borrow_mut().unmount(&mut hypp.builder_at_body());

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

        c.borrow_mut().set_props(
            IfLetProps {
                opt_number: Some(42),
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(hypp.render(), "<body><article>num</article></body>");

        c.borrow_mut().set_props(
            IfLetProps {
                opt_number: None,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(hypp.render(), "<body><article/></body>");

        c.borrow_mut().unmount(&mut hypp.builder_at_body());

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

        c.borrow_mut().set_props(
            Fragment1Props {
                perhaps: false,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(hypp.render(), "<body><div>first</div><p>third</p></body>");

        c.borrow_mut().unmount(&mut hypp.builder_at_body());

        assert_eq!(hypp.render(), "<body/>");
    }

    component! {
        Recursive(depth: usize) {}

        <span>
            {format!("{}", depth)}
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
            "<body><span>3<span>2<span>1</span></span></span></body>"
        );

        c.borrow_mut().set_props(
            RecursiveProps {
                depth: 2,
                __phantom: std::marker::PhantomData,
            },
            &mut hypp.builder_at_body(),
        );

        assert_eq!(hypp.render(), "<body><span>2<span>1</span></span></body>");

        c.borrow_mut().unmount(&mut hypp.builder_at_body());

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
            /*
            if self.prop1 {
                self.state = false;
            }
            */
        }

        fn handle_click(&mut self) {
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

mod test_owning {}
