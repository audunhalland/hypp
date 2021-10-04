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

pub struct ClosureEnv<H: Hypp, T: ShimTrampoline + 'static>(
    H::Shared<Option<<H::Shared<T> as handle::SharedHandle<T>>::Weak>>,
);

impl<H: Hypp + 'static, T: ShimTrampoline + 'static> ClosureEnv<H, T> {
    pub fn deferred() -> Self {
        Self(H::make_shared(None))
    }

    pub fn finalize(&mut self, instance: <H::Shared<T> as handle::SharedHandle<T>>::Weak) {
        *self.0.get_mut() = Some(instance);
    }

    pub fn bind<Args>(
        &self,
        method: &'static dyn for<'s> Fn(&'s mut T::Shim<'s>, Args),
    ) -> H::Function<Args> {
        H::make_function(Box::new(ShimClosure::<H, T, Args> {
            env: self.clone(),
            method,
        }))
    }

    fn as_strong(
        &self,
    ) -> <<H::Shared<T> as handle::SharedHandle<T>>::Weak as WeakHandle<T>>::Strong {
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

///
/// Shim closure - a weak ShimTrampoline instance paired with a ShimMethod
///
pub struct ShimClosure<H: Hypp, T: ShimTrampoline + 'static, Args: 'static> {
    env: ClosureEnv<H, T>,
    method: &'static dyn for<'s> Fn(&'s mut T::Shim<'s>, Args),
}

impl<H, T, Args> Call<Args> for ShimClosure<H, T, Args>
where
    H: Hypp + 'static,
    T: ShimTrampoline + 'static,
    Args: 'static,
{
    fn call(&self, args: Args) {
        let mut instance = self.env.as_strong();
        let method = &self.method;
        let mut args = Some(args);

        instance.get_mut().shim_trampoline(&mut |shim| {
            let args = args.take().unwrap();
            method(shim, args);
        });
    }
}
