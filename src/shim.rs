use super::*;
use crate::handle::{Handle, WeakHandle};

///
/// Inject a "shim" type as the argument of a closure and call that closure.
///
pub trait ShimTrampoline: Sized + 'static {
    type Shim<'s>
    where
        Self: 's;

    ///
    /// Set up the shim, and call the given closure
    /// with that shim as its only argument.
    ///
    fn shim_trampoline(&mut self, method: &mut dyn for<'s> FnMut(&'s mut Self::Shim<'s>));
}

///
/// Type representing the environment for a ShimTrampoline closure.
///
/// The weak reference pointing back to the instance is itself inside a
/// H::SharedMut reference. This is because of the chicken-and-egg problem
/// of creating new objects: The 'things' the object contains has to store
/// closures bound to the object, which cannot exist before everything it
/// is composed of exists. Therefore the ClosureEnv is created with `::deferred()`,
/// and later `::finalize()`d after the object instance is available.
///
pub struct ClosureEnv<H: Hypp, T: ShimTrampoline + 'static>(
    H::SharedMut<Option<<H::SharedMut<T> as handle::SharedHandle<T>>::Weak>>,
);

///
/// How to wrap any ShimTrampoline method to obtain a shared closure.
///
pub trait NewClosure<H: Hypp, T: ShimTrampoline, F, Args> {
    type Output: ?Sized;

    fn new_closure(&self, func: &'static F) -> H::Shared<Self::Output>;
}

impl<H: Hypp + 'static, T: ShimTrampoline + 'static> ClosureEnv<H, T> {
    pub fn deferred() -> Self {
        Self(H::make_shared_mut(None))
    }

    pub fn finalize(&mut self, instance: <H::SharedMut<T> as handle::SharedHandle<T>>::Weak) {
        *self.0.get_mut() = Some(instance);
    }

    fn as_strong(
        &self,
    ) -> <<H::SharedMut<T> as handle::SharedHandle<T>>::Weak as WeakHandle<T>>::Strong {
        self.0
            .get()
            .as_ref()
            .expect("Tried to call uninitialized closure")
            .upgrade()
            .expect("Closure invoked, but instance was dropped")
    }
}

impl<H: Hypp, T: ShimTrampoline + 'static> Clone for ClosureEnv<H, T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<H, T, F> NewClosure<H, T, F, ()> for ClosureEnv<H, T>
where
    H: Hypp + 'static,
    T: ShimTrampoline,
    F: for<'s> Fn(&'s mut T::Shim<'s>) + 'static,
{
    type Output = dyn Fn();

    fn new_closure(&self, func: &'static F) -> H::Shared<Self::Output> {
        let env = self.clone();
        H::make_box_shared(Box::new(move || {
            let mut instance = env.as_strong();

            instance.get_mut().shim_trampoline(&mut |shim| {
                func(shim);
            });
        }))
    }
}

impl<H, T, F, A0> NewClosure<H, T, F, (A0,)> for ClosureEnv<H, T>
where
    H: Hypp + 'static,
    T: ShimTrampoline,
    F: for<'s> Fn(&'s mut T::Shim<'s>, A0) + 'static,
{
    type Output = dyn Fn(A0);

    fn new_closure(&self, func: &'static F) -> H::Shared<Self::Output> {
        let env = self.clone();
        H::make_box_shared(Box::new(move |a0| {
            let mut instance = env.as_strong();
            let mut a0 = Some(a0);

            instance.get_mut().shim_trampoline(&mut |shim| {
                func(shim, a0.take().unwrap());
            });
        }))
    }
}
