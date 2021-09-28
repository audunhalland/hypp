#![feature(generic_associated_types)]

//!
//! The hypp crate allows expressing a reactive DOM.
//!
//! The philosophy is to precompile as much as we can.
//!

pub mod prelude;

pub mod comp;
pub mod error;
pub mod handle;
pub mod list;
pub mod ns;
pub mod shim;
pub mod span;
pub mod state_ref;

#[cfg(feature = "web")]
pub mod web;

#[cfg(feature = "server")]
pub mod server;

pub use error::Error;
pub use hypp_macros::component;
use prelude::ToHandle;

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

    /// An immutable variant of the cursor:
    type Anchor: Anchor<Self>;
    type Cursor<NS: TemplNS>: NSCursor<Self, NS> + Cursor<Self>;

    ///
    /// Type of
    type CallbackSlot: CallbackSlot + 'static;

    /// How to share something.
    /// Different hypp implementations may have different thread-safety requirements.
    type Shared<T>: handle::SharedHandle<T>
    where
        T: 'static;

    fn make_shared<T: 'static>(value: T) -> Self::Shared<T>;

    /// Mount something at the root.
    fn mount<'p, M: Mount<'p, Self> + 'static>(&mut self) -> Result<(), Error>;

    fn set_text(node: &Self::Text, text: &str);

    fn traversal_direction() -> TraversalDirection;
}

///
/// A namespace in which a component template can pick names
///
pub trait TemplNS: Sized + 'static {
    type EType: StaticName;
}

pub trait StaticName {
    fn static_name(&self) -> &'static str;
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
    /// Create a storable anchor at the current position.
    fn anchor(&self) -> H::Anchor;

    /// Set up a callback bound to the current attribute.
    /// The way this works is that we bind the H::Callback to the element,
    /// but that callback doesn't do anything yet. Later, using the Callback
    /// handle, the callback is set up to do its work.
    fn attribute_value_callback(&mut self) -> Result<H::Shared<H::CallbackSlot>, Error>;

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

pub trait NSCursor<H: Hypp, NS: TemplNS>: Cursor<H> {
    /// Execute a series of opcodes.
    /// The last node opcode must produce an element.
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<H::Element, Error>;

    /// Execute a series of opcodes.
    /// The last node opcode must produce a text node.
    fn const_exec_text(&mut self, program: &[ConstOpCode]) -> Result<H::Text, Error>;
}

pub trait Anchor<H: Hypp> {
    /// Create a builder at the anchor position
    fn create_cursor<NS: TemplNS>(&self) -> H::Cursor<NS>;
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
    /// the span must be removed from the tree.
    /// The location of the cursor must be unchanged when the method returns.
    ///
    /// This method is also the opportunity for components to unnest allocated
    /// resources leading to circular references.
    ///
    fn erase(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        self.pass(cursor, SpanOp::Erase)
    }
}

/// The Component trait.
///
/// The term 'pass' is used when there's a cursor method parameter,
/// and that method implementation must 'pass' the cursor over
/// the component's current span.
///
pub trait Component<'p, H: Hypp>: Sized + Span<H> + ToHandle {
    /// The type of properties this component receives
    type Props: 'p;

    type NS: TemplNS;

    ///
    /// Set new props on the component instance, causing an immediate, synchronous
    /// DOM update if the component determines that anything changes.
    ///
    /// The cursor must point at the component start before calling this method.
    /// When the method returns, the cursor must point to the end of the component,
    /// what direction to take is determined by H.
    ///
    fn pass_props(&mut self, props: Self::Props, cursor: &mut H::Cursor<Self::NS>);
}

///
/// Anything that can be mounted without parameters
///
pub trait Mount<'p, H: Hypp>: handle::ToHandle + Component<'p, H> {
    fn mount(cursor: &mut H::Cursor<Self::NS>) -> Result<<Self as ToHandle>::Handle, Error>;
}

///
/// Callback Slot
///
/// Ownership structure:
///
/// ```text
///
///                       [Handle::Weak] <------------- [Option A]
///                             |                           ^
///                             |                           |
/// [  Parent   ]               v                       ****************
/// [ Component ] -> [H::Shared] --> [RefCell/Mutex] --> *THIS COMPONENT*
///                             ^                       ****************
///                             |                           |
///                       [Handle::Weak]                    v
///                             ^                       [H::Shared] <------ [wasm/JS closure] <-- [DOM node]
///                             |                           |
///                           [Fn] (method)                 v
///                             ^                      [RefCell/Mutex]
///                             |                           |
///                             |                           v
///                           [Box] <-- [Option B] <-- [CallbackSlot]
/// ```
///
///
/// [Option A] is used when setting up a new callback during patch.
/// [Option B] is used within an existing callback.
///
/// When a component is unmounted, it must release() all its circular
/// references.
/// Releasing the references involves setting those `Option` values in the
/// diagram to None.
///
pub trait CallbackSlot {
    /// Bind the slot to an actual method
    fn bind(&mut self, method: Box<dyn Fn()>);

    /// Release the bound method from the slot
    fn release(&mut self);
}

// Bug: derive(Clone) broken
// https://github.com/rust-lang/rust/issues/89188
// #[derive(Clone)]
pub struct ShimMethod<T: ShimTrampoline + 'static>(
    pub &'static dyn for<'s> Fn(&'s mut T::Shim<'s>),
);

impl<T: ShimTrampoline + 'static> ShimMethod<T> {
    fn clone(&self) -> Self {
        ShimMethod(self.0)
    }
}

///
/// Inject a "shim" type as the argument of a closure and call that closure.
///
pub trait ShimTrampoline: Sized {
    type Shim<'s>
    where
        Self: 's;

    ///
    /// Set up the shim, and call the given closure
    /// with that shim as its only argument.
    ///
    fn shim_trampoline(&mut self, method: ShimMethod<Self>);
}

///
/// Bind a callback.
///
/// The intention is to associate a callback with a method.
///
pub trait BindCallback<H: Hypp, T: ShimTrampoline> {
    fn bind(&mut self, slot: H::Shared<H::CallbackSlot>, method: ShimMethod<T>);
}

///
/// A function parameter that can function dynamically as either an input or an output parameter
///
pub enum Duplex<'a, T> {
    In(&'a mut T),
    Out(&'a mut Option<T>),
}

///
/// Something from which to acquire a cursor
///
pub trait GetCursor<H: Hypp, NS: TemplNS> {
    fn get_cursor(&mut self) -> &mut H::Cursor<NS>;
}

///
/// Patching context without `bind` functionality
///
pub struct PatchCtx<'a, H: Hypp, NS: TemplNS> {
    pub cur: &'a mut H::Cursor<NS>,
}

impl<'a, H: Hypp, NS: TemplNS> GetCursor<H, NS> for PatchCtx<'a, H, NS> {
    fn get_cursor(&mut self) -> &mut H::Cursor<NS> {
        self.cur
    }
}

///
/// Patching context _with_ `bind` functionality
///
pub struct PatchBindCtx<'a, H: Hypp, NS: TemplNS, T: ShimTrampoline> {
    pub cur: &'a mut H::Cursor<NS>,
    pub bind: &'a mut dyn BindCallback<H, T>,
}

impl<'a, H: Hypp, NS: TemplNS, T: ShimTrampoline> GetCursor<H, NS> for PatchBindCtx<'a, H, NS, T> {
    fn get_cursor(&mut self) -> &mut H::Cursor<NS> {
        self.cur
    }
}

pub type Void = Result<(), Error>;
