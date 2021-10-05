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
pub mod patch;
pub mod shim;
pub mod span;
pub mod state_ref;

#[cfg(feature = "web")]
pub mod web;

#[cfg(feature = "server")]
pub mod server;

#[cfg(feature = "html")]
pub mod html;

use std::ops::Deref;

pub use error::Error;
pub use hypp_macros::component;
pub use hypp_macros::component_dbg;
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
    /// Type of event slot used for connecting with functions
    ///
    // type EventSlot<Args: 'static>: Listen<Self, Args> + 'static;

    /// How to share something, without mutation ability.
    type Shared<T>: Clone + Deref<Target = T> + 'static
    where
        T: ?Sized + 'static;

    /// How to share something, with mutation ability.
    /// Different hypp implementations may have different thread-safety requirements.
    type SharedMut<T>: handle::SharedHandle<T>
    where
        T: 'static;

    fn make_shared<T: 'static>(value: T) -> Self::Shared<T>;
    fn make_box_shared<T: ?Sized + 'static>(value: Box<T>) -> Self::Shared<T>;
    fn make_shared_mut<T: 'static>(value: T) -> Self::SharedMut<T>;

    /// Mount a component accepting default props exclusively at the root.
    fn mount<'p, C>(&mut self) -> Result<(), Error>
    where
        C: Component<'p, Self>,
        C::Props: Default;

    fn set_text(node: &Self::Text, text: &str);

    fn traversal_direction() -> TraversalDirection;
}

///
/// A namespace in which a component template can pick names
///
pub trait TemplNS: Sized + 'static {
    /// Type of an 'element' or container in this namespace
    type EType: Name + 'static;

    /// Type of an 'attribute' - a property sent to an element in this namespace
    type AType: Name + 'static;
}

pub trait Name {
    fn name(&self) -> &str;
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
pub enum ConstOpCode<NS: TemplNS> {
    /// Enter an element at the current location, and return it.
    /// The cursor position moves into that element's first child.
    /// The "return value" of this opcode is the element produced.
    Enter(NS::EType),

    /// Moves the cursor to a specific attribute name within the current element.
    /// This opcode has no return value, and has no effect on the return value of the whole program.
    Attr(NS::AType),

    /// Define text-based attribute value on the current attribute.
    /// This opcode has no return value, and has no effect on the return value of the whole program.
    AttrText(&'static str),

    /// Define a text node. The cursor moves past this text.
    /// The "return value" of this opcode is the text node produced.
    Text(&'static str),

    /// Exit the current element, advancing the cursor position to
    /// the element's next sibling.
    /// The "return value" of this opcode is the element just exited.
    Exit,

    /// Remove the element at the current cursor position.
    /// The element's name must match the tag name passed.
    /// The cursor moves to point at the next sibling.
    /// The "return value" of this opcode is the element that has been removed.
    Erase(NS::EType),
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
}

pub trait NSCursor<H: Hypp, NS: TemplNS>: Cursor<H> {
    /// Execute a series of opcodes.
    /// The last node opcode must produce an element.
    fn const_exec_element(
        &mut self,
        program: &'static [ConstOpCode<NS>],
    ) -> Result<H::Element, Error>;

    /// Execute a series of opcodes.
    /// The last node opcode must produce a text node.
    fn const_exec_text(&mut self, program: &'static [ConstOpCode<NS>]) -> Result<H::Text, Error>;

    /// Set up an event slot connected to the current attribute.
    fn attribute_slot<EK: 'static>(&mut self) -> Result<Slot<H, NS, EK>, Error>
    where
        EK: EventKind<NS>;

    /// Advance the cursor, according to the const program passed.
    /// Don't mutate anything.
    /// The DOM described by the program must match the actual DOM.
    fn skip_const_program(&mut self, program: &'static [ConstOpCode<NS>]);
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

///
/// Deviation model (encodes which parameters must be refreshed)
///
#[derive(Clone, Copy)]
pub enum Deviation<'b> {
    Full,
    Partial(&'b [bool]),
    None,
}

impl<'b> Deviation<'b> {
    pub fn refresh_at(&self, param: usize) -> Refresh {
        match self {
            Self::Full => Refresh(true),
            Self::Partial(slice) => Refresh(slice[param]),
            Self::None => Refresh(false),
        }
    }

    pub fn refresh_at_any(&self, params: &[usize]) -> Refresh {
        match self {
            Self::Full => Refresh(true),
            Self::Partial(slice) => {
                for param in params {
                    if slice[*param] {
                        return Refresh(true);
                    }
                }
                Refresh(false)
            }
            Self::None => Refresh(false),
        }
    }
}

///
/// Refresh; determines whether something atomically needs to be refreshed
///
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Refresh(pub bool);

impl Default for Refresh {
    fn default() -> Self {
        Refresh(true)
    }
}

/// The Component trait.
///
/// The term 'pass' is used when there's a cursor method parameter,
/// and that method implementation must 'pass' the cursor over
/// the component's current span.
///
pub trait Component<'p, H: Hypp>: Sized + Span<H> + ToHandle<H> {
    /// The type of properties this component receives
    type Props: 'p;

    type NS: TemplNS;

    fn mount(
        props: Self::Props,
        cursor: &mut H::Cursor<Self::NS>,
    ) -> Result<<Self as ToHandle<H>>::Handle, Error>;

    ///
    /// Set new props on the component instance, causing an immediate, synchronous
    /// DOM update if the component determines that anything changes.
    ///
    /// The cursor must point at the component start before calling this method.
    /// When the method returns, the cursor must point to the end of the component,
    /// what direction to take is determined by H.
    ///
    /// # Arguments
    /// * `props` - new set of props
    /// * `cursor` - mutable cursor poining to the component. After return of method,
    ///   it must point _after_ the component (depending on traversal direction)
    ///
    fn pass_props(&mut self, props: Self::Props, cursor: &mut H::Cursor<Self::NS>);

    fn as_dyn_span_mut(&mut self) -> &mut dyn Span<H> {
        self
    }
}

pub trait EventKind<NS: TemplNS> {
    type Event;

    #[cfg(feature = "web")]
    fn from_web_event<W: wasm_bindgen::convert::FromWasmAbi>(web_event: &W) -> Self::Event;
}

///
/// # Listen
///
/// Ownership diagram of a component with one registered event function in a web context:
///
/// ```text
///
///  [Parent component]
///        |
///        v                    ####################################
///  [H::SharedMut] ----------> #         THIS COMPONENT           #
///                ^            ####################################
///                |                |                          |
///                |                v                          v
///              [Weak]          [Option]             [Slot(Box<dyn Listen>)]
///                ^                |                          |
///                |                v                          v
///                |           [ClosureEnv]              [H::SharedMut] <--- [wasm/JS closure] <-- [DOM node]
///                |                |                          |
///                |                v                          v
///             [Option] <---- [H::SharedMut]             [WebCallback]
///                                 ^                          |
///                                 |                          v
///                            [ClosureEnv]                 [Option]
///                                 ^                          |
///                                 |                          v
///                                [Fn] <---------------- [H::Shared]
///                                                            ^
///                                                            |
///                                         {callbacks shared with child components, etc}
/// ```
///
/// When an event is fired from the DOM, it notifies the `WebCallback`.
/// The `WebCallback` is owned through `Slot`, to prevent the JS closure from being garbage collected.
/// `WebCallback`, which implements `Listen`, has an optional shared pointer to a registered shared `Fn`.
/// The shared `Fn` owns a `ClosureEnv`, which contains the weak reference back to the component.
/// The event invokes the `Fn`, the weak reference is upgraded to a strong one, and the component shim method
/// is executed.
///
/// New closures are made from the `Option -> ClosureEnv` owned by the component.
///
/// When the component is unmounted, it must release() all its circular
/// references.
/// Releasing the references involves setting those `Option` values in the
/// diagram to None.
///
pub trait Listen<H: Hypp, Event> {
    /// Listen to when some event is fired
    fn listen(&mut self, function: H::Shared<dyn Fn(Event)>);

    /// Forget the listener
    fn forget(&mut self);
}

///
/// Subscribe to something given a shared function.
///
pub trait Subscribe<H: Hypp, F: ?Sized + 'static> {
    fn subscribe(&mut self, f: H::Shared<F>);
}

///
/// # Slot
///
/// Facade for listening to events.
///
///
pub struct Slot<H: Hypp, NS: TemplNS, EK: EventKind<NS>>(Box<dyn Listen<H, EK::Event>>);

impl<H: Hypp, NS: TemplNS, EK: EventKind<NS>> Slot<H, NS, EK> {
    pub fn new(listen: Box<dyn Listen<H, EK::Event>>) -> Self {
        Self(listen)
    }

    pub fn forget(&mut self) {
        self.0.forget();
    }
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

pub type Void = Result<(), Error>;
