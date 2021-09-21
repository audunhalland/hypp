use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::closure::Closure;

use crate::Callback;

pub struct WebCallback {
    rust_function: Option<Box<dyn Fn()>>,
}

impl WebCallback {
    fn call(&self) {
        let rust_function = self
            .rust_function
            .as_ref()
            .expect("No Rust function defined in callback");

        rust_function();
    }
}

pub fn new_callback() -> (Rc<RefCell<WebCallback>>, Closure<dyn Fn()>) {
    let callback = Rc::new(RefCell::new(WebCallback {
        rust_function: None,
    }));
    let js_ref = callback.clone();

    let web_closure = Closure::wrap(Box::new(move || {
        js_ref.borrow().call();
    }) as Box<dyn Fn()>);

    (callback, web_closure)
}

impl Callback for WebCallback {
    fn bind(&mut self, function: Box<dyn Fn()>) {
        self.rust_function = Some(function);
    }

    fn release(&mut self) {
        self.rust_function = None;
    }
}
