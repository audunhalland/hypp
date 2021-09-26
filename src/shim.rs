use super::*;
use crate::handle::{Handle, SharedHandle, WeakHandle};

pub struct Binder<H: Hypp, T: 'static> {
    weak: <H::Shared<T> as SharedHandle<T>>::Weak,
    __phantom: std::marker::PhantomData<H>,
}

impl<H: Hypp, T> Binder<H, T> {
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

impl<H: Hypp, T: ShimTrampoline> BindCallback<H, T> for Binder<H, T> {
    fn bind(&mut self, mut callback: H::Shared<H::Callback>, method: ShimMethod<T>) {
        let weak = self.weak.clone();

        callback.get_mut().bind(Box::new(move || {
            let mut rc = weak
                .upgrade()
                .expect("Callback invoked, but component was destroyed");
            rc.get_mut().shim_trampoline(method.clone());
        }));
    }
}

pub struct LazyBinder<H: Hypp, T: ShimTrampoline + 'static> {
    callbacks: Vec<(H::Shared<H::Callback>, ShimMethod<T>)>,
}

impl<H: Hypp, T: ShimTrampoline + 'static> LazyBinder<H, T> {
    pub fn new() -> Self {
        Self { callbacks: vec![] }
    }

    pub fn bind_all(self, weak: <H::Shared<T> as SharedHandle<T>>::Weak) {
        if !self.callbacks.is_empty() {
            let mut binder: Binder<H, T> = Binder::from_weak(weak);

            for (callback, method) in self.callbacks.into_iter() {
                binder.bind(callback, method);
            }
        }
    }
}

impl<H: Hypp, T: ShimTrampoline> BindCallback<H, T> for LazyBinder<H, T> {
    fn bind(&mut self, callback: H::Shared<H::Callback>, method: ShimMethod<T>) {
        self.callbacks.push((callback, method));
    }
}
