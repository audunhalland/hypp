use super::{Callback, Hypp, SharedCallback, ShimTrampoline};

pub fn bind_callbacks<H: Hypp, T: ShimTrampoline + 'static, F>(
    weak_self: &Option<std::rc::Weak<std::cell::RefCell<T>>>,
    callbacks: Vec<(SharedCallback<H>, F)>,
) where
    for<'s> F: Fn(&'s mut T::Shim<'s>) + Copy + 'static,
{
    if !callbacks.is_empty() {
        let rc = weak_self.as_ref().and_then(|weak| weak.upgrade()).unwrap();

        for (callback, method) in callbacks.into_iter() {
            let rc = rc.clone();

            callback.borrow_mut().bind(Box::new(move || {
                rc.borrow_mut().shim_trampoline(method);
            }));
        }
    }
}
