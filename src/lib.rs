#![forbid(unsafe_code)]
#![feature(generic_associated_types)]

//!
//! The hypp crate allows expressing a reactive DOM.
//!
//! The philosophy is to precompile as much as we can.
//!

pub mod error;
pub mod handle;
pub mod server;
pub mod slot;
pub mod state_ref;
pub mod web;

pub mod prelude;

pub use error::Error;
pub use hypp_macros::component;

pub enum TraversalDirection {
    FirstToLast,
    LastToFirst,
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

    fn traversal_direction() -> TraversalDirection;
}

///
/// "upcast" a DOM node of a specific type to its generic type
///
pub trait AsNode<H: Hypp> {
    fn as_node(&self) -> &H::Node;
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
/// Cursor
///
/// An abstract cursor at some DOM position, allowing users of the trait
/// to do various operations to mutate it.
///
/// The cursor has a specific traversal direction for a Hypp implementation.
///
/// For web DOMs, the traversal direction of children, should be last to first.
/// This is because the basic call to add a child is `Element::insert_before(element, next)`,
/// and `next` will then be item that item that has been most recently produced.
///
pub trait Cursor<H: Hypp> {
    /// Execute a series of opcodes.
    /// The last node opcode must produce an element.
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<H::Element, Error>;

    /// Execute a series of opcodes.
    /// The last node opcode must produce a text node.
    fn const_exec_text(&mut self, program: &[ConstOpCode]) -> Result<H::Text, Error>;

    /// Set up a callback bound to the current attribute.
    /// The way this works is that we bind the H::Callback to the element,
    /// but that callback doesn't do anything yet. Later, using the Callback
    /// handle, the callback is set up to do its work.
    fn attribute_value_callback(&mut self) -> Result<H::Callback, Error>;

    /// Define a text. The cursor moves past this text.
    fn text(&mut self, text: &str) -> Result<H::Text, Error>;

    /// Advance cursor directly to a location.
    /// This sets up the cursor to traverse the element's child list.
    fn move_to_children_of(&mut self, element: &H::Element);

    /// Advance cursor directly to a location in a child list.
    fn move_to_following_sibling_of(&mut self, node: &H::Node);

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

/// The component trait
pub trait Component<'p, H: Hypp>: Sized + handle::ToHandle {
    /// The type of properties this component recieves
    type Props: 'p;

    ///
    /// Set new props on the component instance, causing an immediate, synchronous
    /// DOM update if the component determines that anything changes.
    ///
    /// The cursor must point at the component start before calling this method.
    /// When the method returns, the cursor must point to the end of the component,
    /// what direction to take is determined by H.
    ///
    fn set_props(&mut self, props: Self::Props, cursor: &mut dyn Cursor<H>);

    /// Move the cursor from the start to the end of the component,
    /// according to the traversal direction in H.
    /// The component should not update.
    /// But it gets a possibility to store the new cursor position in case it needs to store it.
    fn pass_over(&mut self, cursor: &mut dyn Cursor<H>);

    /// Unmount the component, removing all its nodes
    /// from under its mount point in the tree, using the cursor.
    /// The only purpose of this call is to remove nodes in the DOM.
    fn unmount(&mut self, cursor: &mut dyn Cursor<H>);
}

///
/// Callback
///
/// Ownership structure:
///
/// [Parent component]
///    |
///    v                           ****************
///  [Rc] ------> [RefCell] -----> *THIS COMPONENT*
///    ^                           *****|**********
///    |                                v
///    |                               [Rc]
///  [Fn]                               |
///    ^                                v
///    |                           [Callback]         [DOM Node]
///    |                                |                 |
///  [Box]                              v                 v
///    ^                               [Rc] <------- [wasm closure]
///    |                                |
///    |                                v
/// [Option] <--- [CallbackCell] <-- [RefCell]
///
/// When a component is unmounted, it must release() all its callbacks.
/// releasing the callback means setting the `Option` within `CallbackCell` to `None`,
/// so that the Fn no longer owns the component.
///
pub trait Callback {
    fn bind(&self, function: Box<dyn Fn()>);

    fn release(&self);
}
