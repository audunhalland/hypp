/// Trait for converting a type into a Handle.
pub trait ToHandle: Sized {
    /// The type of handle that must be used when owning an instance of this type.
    type Handle: Handle<Self>;

    fn to_handle(self) -> Self::Handle {
        Self::Handle::new(self)
    }
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

    /// Boxed version of this handle
    type Boxed;

    fn new(val: T) -> Self;

    fn get<'a>(&'a self) -> Self::Ref<'a>;
    fn get_mut<'a>(&'a mut self) -> Self::RefMut<'a>;

    fn into_boxed(self) -> Self::Boxed;
}

pub trait SharedHandle<T>: Handle<T> + Clone {
    type Weak: WeakHandle<T> + 'static;

    fn downgrade(&self) -> Self::Weak;
}

pub trait WeakHandle<T>: Clone + Sized {
    type Strong: SharedHandle<T> + 'static;

    fn upgrade(&self) -> Option<Self::Strong>;
}

/// A unique (non-shared) handle.
pub struct Unique<T>(T);

impl<T> Handle<T> for Unique<T> {
    type Boxed = Box<T>;

    type Ref<'a>
    where
        Self: 'a,
    = &'a T;

    type RefMut<'a>
    where
        Self: 'a,
    = &'a mut T;

    fn new(val: T) -> Self {
        Self(val)
    }

    fn get<'a>(&'a self) -> &'a T {
        &self.0
    }

    fn get_mut<'a>(&'a mut self) -> &'a mut T {
        &mut self.0
    }

    fn into_boxed(self) -> Self::Boxed {
        Box::new(self.0)
    }
}

impl<T> Handle<T> for Box<T> {
    type Boxed = Self;

    type Ref<'a>
    where
        Self: 'a,
    = &'a T;

    type RefMut<'a>
    where
        Self: 'a,
    = &'a mut T;

    fn new(val: T) -> Self {
        Box::new(val)
    }

    fn into_boxed(self) -> Self::Boxed {
        self
    }

    fn get<'a>(&'a self) -> &'a T {
        self
    }

    fn get_mut<'a>(&'a mut self) -> &'a mut T {
        self
    }
}

type Shared<T> = std::rc::Rc<std::cell::RefCell<T>>;

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

    fn new(val: T) -> Self {
        std::rc::Rc::new(std::cell::RefCell::new(val))
    }

    fn into_boxed(self) -> Self::Boxed {
        // already boxed :)
        self
    }

    fn get<'a>(&'a self) -> Self::Ref<'a> {
        self.borrow()
    }

    fn get_mut<'a>(&'a mut self) -> Self::RefMut<'a> {
        self.borrow_mut()
    }
}

impl<T> SharedHandle<T> for Shared<T>
where
    T: 'static,
{
    type Weak = std::rc::Weak<std::cell::RefCell<T>>;

    fn downgrade(&self) -> Self::Weak {
        std::rc::Rc::downgrade(self)
    }
}

impl<T> WeakHandle<T> for std::rc::Weak<std::cell::RefCell<T>>
where
    T: 'static,
{
    type Strong = Shared<T>;

    fn upgrade(&self) -> Option<Self::Strong> {
        self.upgrade()
    }
}

#[cfg(feature = "server")]
pub mod sync {
    use parking_lot::{Mutex, MutexGuard};
    use std::sync::{Arc, Weak};

    type SyncShared<T> = Arc<Mutex<T>>;
    type SyncWeak<T> = Weak<Mutex<T>>;

    impl<T> super::Handle<T> for SyncShared<T> {
        type Boxed = Self;

        type Ref<'a>
        where
            Self: 'a,
        = MutexGuard<'a, T>;

        type RefMut<'a>
        where
            Self: 'a,
        = MutexGuard<'a, T>;

        fn new(val: T) -> Self {
            Arc::new(Mutex::new(val))
        }

        fn get<'a>(&'a self) -> Self::Ref<'a> {
            self.lock()
        }

        fn get_mut<'a>(&'a mut self) -> Self::RefMut<'a> {
            self.lock()
        }

        fn into_boxed(self) -> Self::Boxed {
            // already boxed :)
            self
        }
    }

    impl<T> super::SharedHandle<T> for SyncShared<T>
    where
        T: 'static,
    {
        type Weak = SyncWeak<T>;

        fn downgrade(&self) -> Self::Weak {
            std::sync::Arc::downgrade(self)
        }
    }

    impl<T> super::WeakHandle<T> for SyncWeak<T>
    where
        T: 'static,
    {
        type Strong = SyncShared<T>;

        fn upgrade(&self) -> Option<Self::Strong> {
            self.upgrade()
        }
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
            CompA {}.to_handle()
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
            CompB {
                _comp_a: CompA::mount(),
            }
            .to_handle()
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
        a.get_mut();

        let mut a2: <<CompA as ToHandle>::Handle as Handle<CompA>>::Boxed =
            CompA::mount().into_boxed();
        a2.get_mut();

        let mut b: <CompB as ToHandle>::Handle = CompB::mount();
        b.get_mut();

        let mut b2: <<CompB as ToHandle>::Handle as Handle<CompB>>::Boxed =
            CompB::mount().into_boxed();

        b2.get_mut();
    }
}
