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
pub mod span;
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
    type Node: Clone + span::AsSpan;
    type Element: Clone + span::AsSpan + AsNode<Self>;
    type Text: Clone + span::AsSpan + AsNode<Self>;

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
#[derive(Debug)]
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

    fn move_to_following_sibling(&mut self) -> Result<(), Error>;

    /// Advance cursor directly to a location in a child list.
    fn move_to_following_sibling_of(&mut self, node: &H::Node);

    /// Remove the node at the current cursor position.
    fn remove_node(&mut self) -> Result<(), Error>;

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

#[derive(Debug, Clone, Copy)]
pub enum SpanOp {
    PassOver,
    Erase,
}

/// Something that spans nodes in a DOM-like tree.
///
/// A Span only considers siblings in a sibling list.
///
/// Between any two consecutive siblings, there is an empty span.
///
/// Example:
/// <a>
///   <b/>________
///   <c/>       |
///   <d>        | The span of elements `c` and `d`.
///     <e/>     | This span has a length of 2.
///   </d>_______|
///   <e/>
///   <f/>________ The empty span after `f` has a length of 0.
///  </a>
pub trait Span<H: Hypp> {
    /// Whether the start of the span is anchored,
    /// which means that it is associated with a constant node
    /// that never changes.
    fn is_anchored(&self) -> bool;

    /// "Generic" pass, that can be implemented by passing
    /// this span's sub spans.
    /// This method is reserved for the specific pass methods below.
    /// It exists so that a Span impl can implement pass generically,
    /// if it constists only of sub spans.
    fn pass(&mut self, _cursor: &mut dyn Cursor<H>, _op: SpanOp) -> bool {
        false
    }

    /// Make the cursor _pass over_ the this span,
    /// i.e. jump directly from the beginning to the end,
    /// without doing any modifications to the cursor.
    /// The direction of the pass must in accordance with Hypp implementation.
    ///
    /// The method must return whether it was able to pass anything.
    fn pass_over(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        self.pass(cursor, SpanOp::PassOver)
    }

    ///
    /// Delete this span, which must make it zero sized,
    /// e.g. contain no nodes. All the nodes that where within
    /// the span have to be removed from the tree.
    /// The location of the cursor must be unchanged when the method returns.
    ///
    fn erase(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        self.pass(cursor, SpanOp::Erase)
    }
}

/// The component trait.
///
/// The term 'pass' is used when there's a cursor method parameter,
/// and that method implementation must 'pass' the cursor over
/// the component's owned DOM.
///
pub trait Component<'p, H: Hypp>: Sized + Span<H> + handle::ToHandle {
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
    fn pass_props(&mut self, props: Self::Props, cursor: &mut dyn Cursor<H>);

    ///
    /// Must be called before dropping a component.
    /// Used to unnest circular references, etc.
    ///
    fn cleanup(&mut self);
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
