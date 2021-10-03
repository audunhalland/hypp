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

#[derive(Clone)]
struct DeferrableInstance<H: Hypp, T: ShimTrampoline + 'static>(
    H::Shared<Option<<H::Shared<T> as handle::SharedHandle<T>>::Weak>>,
);

///
/// Shim closure - a weak ShimTrampoline instance paired with a ShimMethod
///
pub struct ShimClosure<H: Hypp, T: ShimTrampoline + 'static> {
    instance: DeferrableInstance<H, T>,
    method: ShimMethod<T>,
}

impl<H: Hypp, T: ShimTrampoline + 'static> Call for ShimClosure<H, T> {
    fn call(&self) {
        let mut rc = self
            .instance
            .0
            .get()
            .as_ref()
            .expect("Tried to call uninitialized closure")
            .upgrade()
            .expect("Callback invoked, but instance was dropped");
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
pub trait MakeClosure<H: Hypp, T: ShimTrampoline> {
    fn make_closure(&mut self, method: ShimMethod<T>) -> Function<H>;
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

impl<H: Hypp + 'static, T: ShimTrampoline> MakeClosure<H, T> for SelfBinder<H, T> {
    fn make_closure(&mut self, method: ShimMethod<T>) -> Function<H> {
        let closure = ShimClosure::<H, T> {
            instance: DeferrableInstance(H::make_shared(Some(self.weak.clone()))),
            method,
        };
        Function::from_call(Box::new(closure))
    }
}

pub struct DeferredSelfBinder<H: Hypp + 'static, T: ShimTrampoline + 'static> {
    deferred_instances: Vec<DeferrableInstance<H, T>>,
}

impl<H: Hypp, T: ShimTrampoline + 'static> DeferredSelfBinder<H, T> {
    pub fn new() -> Self {
        Self {
            deferred_instances: vec![],
        }
    }

    pub fn register_instance(self, weak: <H::Shared<T> as SharedHandle<T>>::Weak) {
        for mut deferred_instance in self.deferred_instances.into_iter() {
            let mut opt_instance = deferred_instance.0.get_mut();
            *opt_instance = Some(weak.clone());
        }
    }
}

impl<H: Hypp, T: ShimTrampoline> MakeClosure<H, T> for DeferredSelfBinder<H, T> {
    fn make_closure(&mut self, method: ShimMethod<T>) -> Function<H> {
        let deferred_instance = DeferrableInstance::<H, T>(H::make_shared(None));

        let closure = ShimClosure::<H, T> {
            instance: DeferrableInstance(deferred_instance.0.clone()),
            method,
        };
        self.deferred_instances.push(deferred_instance);

        let shared_closure: H::Shared<Box<dyn Call>> = H::make_shared(Box::new(closure));
        Function::from_shared(shared_closure)
    }
}
