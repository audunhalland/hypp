/// Trait for converting a type into a Handle.
pub trait ToHandle: Sized {
    /// The type of handle that must be used when owning an instance of this type.
    type Handle: Handle<Self>;
}

///
/// Handle
///
/// Sometimes a type wants to be "owned" in a certain way:
/// E.g. it may want to be reference counted using Rc.
/// Or it could fine being just normally owned/unboxed.
///
/// The Handle is an abstract way through which to "own" that type, in
/// the way the type wants, using `ToHandle`.
///
/// The handle may be borrowed/dereffed mutably and immutably, and
/// boxed, if not already boxed.
///
/// Being boxed is not something that the type needs to decide itself,
/// but rather the _owner_ type. So boxing is optional, using `type Boxed`.
///
/// This feature requires GAT support in the compiler.
pub trait Handle<T>: Sized {
    /// Shared reference to the wrapped type; any type
    /// that implements Deref to that type.
    type Ref<'a>: std::ops::Deref<Target = T>
    where
        Self: 'a;

    /// Mutable reference to the wrapped type; any type
    /// that implements DerefMut to that type.
    type RefMut<'a>: std::ops::DerefMut<Target = T>
    where
        Self: 'a;

    type Boxed;

    fn borrow<'a>(&'a self) -> Self::Ref<'a>;

    fn borrow_mut<'a>(&'a mut self) -> Self::RefMut<'a>;

    fn into_boxed(self) -> Self::Boxed;
}

/// A unique (non-shared) handle.
pub struct Unique<T>(T);

impl<T> Unique<T> {
    pub fn new(val: T) -> Self {
        Self(val)
    }
}

impl<T> Handle<T> for Unique<T> {
    type Boxed = UniqueBoxed<T>;

    type Ref<'a>
    where
        Self: 'a,
    = &'a T;

    type RefMut<'a>
    where
        Self: 'a,
    = &'a mut T;

    fn into_boxed(self) -> Self::Boxed {
        UniqueBoxed(Box::new(self.0))
    }

    fn borrow<'a>(&'a self) -> &'a T {
        &self.0
    }

    fn borrow_mut<'a>(&'a mut self) -> &'a mut T {
        &mut self.0
    }
}

pub struct UniqueBoxed<C>(Box<C>);

impl<T> Handle<T> for UniqueBoxed<T> {
    type Boxed = Self;

    type Ref<'a>
    where
        Self: 'a,
    = &'a T;

    type RefMut<'a>
    where
        Self: 'a,
    = &'a mut T;

    fn into_boxed(self) -> Self::Boxed {
        self
    }

    fn borrow<'a>(&'a self) -> &'a T {
        &self.0
    }

    fn borrow_mut<'a>(&'a mut self) -> &'a mut T {
        &mut self.0
    }
}

/// A shared handle.
pub struct Shared<T>(std::rc::Rc<std::cell::RefCell<T>>);

impl<T> Shared<T> {
    pub fn new(val: std::rc::Rc<std::cell::RefCell<T>>) -> Self {
        Self(val)
    }
}

impl<T> Handle<T> for Shared<T> {
    type Boxed = Self;

    type Ref<'a>
    where
        Self: 'a,
    = std::cell::Ref<'a, T>;

    type RefMut<'a>
    where
        Self: 'a,
    = std::cell::RefMut<'a, T>;

    fn into_boxed(self) -> Self::Boxed {
        // already boxed :)
        self
    }

    fn borrow<'a>(&'a self) -> Self::Ref<'a> {
        self.0.borrow()
    }

    fn borrow_mut<'a>(&'a mut self) -> Self::RefMut<'a> {
        self.0.borrow_mut()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Component, Cursor, Hypp, Span};

    struct LolProps<'p> {
        _prop: &'p str,
    }

    struct CompA {}

    impl CompA {
        fn mount() -> Unique<Self> {
            Unique::new(CompA {})
        }
    }

    impl ToHandle for CompA {
        type Handle = Unique<Self>;
    }

    impl<H: Hypp> Span<H> for CompA {
        fn is_anchored(&self) -> bool {
            true
        }
    }

    impl<'p, H: Hypp> Component<'p, H> for CompA {
        type Props = LolProps<'p>;

        fn pass_props(&mut self, _props: Self::Props, _: &mut dyn Cursor<H>) {}
    }

    struct CompB {
        _comp_a: <CompA as ToHandle>::Handle,
    }

    impl CompB {
        fn mount() -> Shared<Self> {
            Shared::new(std::rc::Rc::new(std::cell::RefCell::new(CompB {
                _comp_a: CompA::mount(),
            })))
        }
    }

    impl ToHandle for CompB {
        type Handle = Shared<Self>;
    }

    impl<H: Hypp> Span<H> for CompB {
        fn is_anchored(&self) -> bool {
            true
        }
    }

    impl<'p, H: Hypp> Component<'p, H> for CompB {
        type Props = LolProps<'p>;

        fn pass_props(&mut self, _props: Self::Props, _: &mut dyn Cursor<H>) {}
    }

    #[test]
    fn test_handles_compile() {
        let mut a: <CompA as ToHandle>::Handle = CompA::mount();
        a.borrow_mut();

        let mut a2: <<CompA as ToHandle>::Handle as Handle<CompA>>::Boxed =
            CompA::mount().into_boxed();
        a2.borrow_mut();

        let mut b: <CompB as ToHandle>::Handle = CompB::mount();
        b.borrow_mut();

        let mut b2: <<CompB as ToHandle>::Handle as Handle<CompB>>::Boxed =
            CompB::mount().into_boxed();

        b2.borrow_mut();
    }
}
