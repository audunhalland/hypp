use super::*;
use crate::handle::{Handle, SharedHandle, WeakHandle};

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

pub struct BoundShimMethod<H: Hypp, T: ShimTrampoline + 'static> {
    weak: <H::Shared<T> as handle::SharedHandle<T>>::Weak,
    method: ShimMethod<T>,
}

impl<H: Hypp, T: ShimTrampoline + 'static> Call for BoundShimMethod<H, T> {
    fn call(&self) {
        let mut rc = self
            .weak
            .upgrade()
            .expect("Callback invoked, but component was destroyed");
        rc.get_mut().shim_trampoline(self.method.clone());
    }
}

///
/// Bind a callback to 'self'.
///
/// The intention is to associate a callback with a self method.
///
/// But need to do so in an abstract manner, because 'self' may not exist yet (LazySelfBinder)
///
pub trait BindSelf<H: Hypp, T: ShimTrampoline> {
    fn bind_self(&mut self, slot: H::Shared<H::CallbackSlot>, method: ShimMethod<T>);
}

pub struct SelfBinder<H: Hypp, T: 'static> {
    weak: <H::Shared<T> as SharedHandle<T>>::Weak,
    __phantom: std::marker::PhantomData<H>,
}

impl<H: Hypp, T> SelfBinder<H, T> {
    pub fn from_weak(weak: <H::Shared<T> as SharedHandle<T>>::Weak) -> Self {
        Self {
            weak,
            __phantom: std::marker::PhantomData,
        }
    }

    pub fn from_opt_weak(opt_weak: &Option<<H::Shared<T> as SharedHandle<T>>::Weak>) -> Self {
        let weak = opt_weak.as_ref().map(|weak| weak.clone()).unwrap();
        Self {
            weak,
            __phantom: std::marker::PhantomData,
        }
    }
}

impl<H: Hypp, T: ShimTrampoline> BindSelf<H, T> for SelfBinder<H, T> {
    fn bind_self(&mut self, mut callback: H::Shared<H::CallbackSlot>, method: ShimMethod<T>) {
        let weak = self.weak.clone();

        callback.get_mut().bind(Box::new(move || {
            let mut rc = weak
                .upgrade()
                .expect("Callback invoked, but component was destroyed");
            rc.get_mut().shim_trampoline(method.clone());
        }));
    }
}

pub struct LazySelfBinder<H: Hypp, T: ShimTrampoline + 'static> {
    bound_slots: Vec<(H::Shared<H::CallbackSlot>, ShimMethod<T>)>,
}

impl<H: Hypp, T: ShimTrampoline + 'static> LazySelfBinder<H, T> {
    pub fn new() -> Self {
        Self {
            bound_slots: vec![],
        }
    }

    pub fn bind_all(self, weak: <H::Shared<T> as SharedHandle<T>>::Weak) {
        if !self.bound_slots.is_empty() {
            let mut binder: SelfBinder<H, T> = SelfBinder::from_weak(weak);

            for (slot, method) in self.bound_slots.into_iter() {
                binder.bind_self(slot, method);
            }
        }
    }
}

impl<H: Hypp, T: ShimTrampoline> BindSelf<H, T> for LazySelfBinder<H, T> {
    fn bind_self(&mut self, slot: H::Shared<H::CallbackSlot>, method: ShimMethod<T>) {
        self.bound_slots.push((slot, method));
    }
}
