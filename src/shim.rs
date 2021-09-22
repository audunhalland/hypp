use super::{Callback, Hypp, SharedCallback, ShimTrampoline};

///
/// Bind a list of (callback, method) pairs to a weak instance
///
pub fn bind_callbacks_weak<H: Hypp, T: ShimTrampoline + 'static, F>(
    instance: &Option<std::rc::Weak<std::cell::RefCell<T>>>,
    callbacks: Vec<(SharedCallback<H>, F)>,
) where
    for<'s> F: Fn(&'s mut T::Shim<'s>) + Copy + 'static,
{
    if !callbacks.is_empty() {
        let rc = instance.as_ref().and_then(|weak| weak.upgrade()).unwrap();

        for (callback, method) in callbacks.into_iter() {
            bind_callback::<H, _, _>(callback, rc.clone(), method);
        }
    }
}

///
/// Bind one callback to a weak instance and a method
///
pub fn bind_callback_weak<H: Hypp, T: ShimTrampoline + 'static, F>(
    callback: SharedCallback<H>,
    instance: &Option<std::rc::Weak<std::cell::RefCell<T>>>,
    method: F,
) where
    for<'s> F: Fn(&'s mut T::Shim<'s>) + Copy + 'static,
{
    let rc = instance.as_ref().and_then(|weak| weak.upgrade()).unwrap();
    bind_callback::<H, _, _>(callback, rc, method);
}

///
/// Bind one callback to a strong instance and a method
///
pub fn bind_callback<H: Hypp, T: ShimTrampoline + 'static, F>(
    callback: SharedCallback<H>,
    instance: std::rc::Rc<std::cell::RefCell<T>>,
    method: F,
) where
    for<'s> F: Fn(&'s mut T::Shim<'s>) + Copy + 'static,
{
    callback.borrow_mut().bind(Box::new(move || {
        instance.borrow_mut().shim_trampoline(method);
    }));
}
