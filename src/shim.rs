use super::*;

pub struct Binder<H: Hypp, T> {
    rc: std::rc::Rc<std::cell::RefCell<T>>,
    __phantom: std::marker::PhantomData<H>,
}

impl<H: Hypp, T> Binder<H, T> {
    pub fn from_rc(rc: std::rc::Rc<std::cell::RefCell<T>>) -> Self {
        Self {
            rc,
            __phantom: std::marker::PhantomData,
        }
    }

    pub fn from_opt_weak(weak: &Option<std::rc::Weak<std::cell::RefCell<T>>>) -> Self {
        let rc = weak.as_ref().and_then(|weak| weak.upgrade()).unwrap();
        Self {
            rc,
            __phantom: std::marker::PhantomData,
        }
    }
}

impl<H: Hypp, T: ShimTrampoline> BindCallback<H, T> for Binder<H, T> {
    fn bind(&mut self, callback: SharedCallback<H>, method: ShimMethod<T>) {
        let rc = self.rc.clone();

        callback.borrow_mut().bind(Box::new(move || {
            rc.borrow_mut().shim_trampoline(method.clone());
        }));
    }
}

pub struct LazyBinder<H: Hypp, T: ShimTrampoline + 'static> {
    callbacks: Vec<(SharedCallback<H>, ShimMethod<T>)>,
}

impl<H: Hypp, T: ShimTrampoline + 'static> LazyBinder<H, T> {
    pub fn new() -> Self {
        Self { callbacks: vec![] }
    }

    pub fn bind_all(self, weak: &Option<std::rc::Weak<std::cell::RefCell<T>>>) {
        if !self.callbacks.is_empty() {
            let mut binder: Binder<H, T> = Binder::from_opt_weak(weak);

            for (callback, method) in self.callbacks.into_iter() {
                binder.bind(callback, method);
            }
        }
    }
}

impl<H: Hypp, T: ShimTrampoline> BindCallback<H, T> for LazyBinder<H, T> {
    fn bind(&mut self, callback: SharedCallback<H>, method: ShimMethod<T>) {
        self.callbacks.push((callback, method));
    }
}
