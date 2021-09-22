use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use crate::Callback;

pub struct WebCallback {
    web_closure: Option<Closure<dyn Fn()>>,
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

    pub fn web_closure(&self) -> &Closure<dyn Fn()> {
        self.web_closure.as_ref().unwrap()
    }
}

impl Drop for WebCallback {
    fn drop(&mut self) {
        // Logging..
        // console::log_1(&"Dropping callback!".into());
    }
}

pub fn new_callback() -> Rc<RefCell<WebCallback>> {
    let callback = Rc::new(RefCell::new(WebCallback {
        web_closure: None,
        rust_function: None,
    }));
    let js_ref = callback.clone();

    let web_closure = Closure::wrap(Box::new(move || {
        js_ref.borrow().call();
    }) as Box<dyn Fn()>);

    callback.borrow_mut().web_closure = Some(web_closure);

    callback
}

impl Callback for WebCallback {
    fn bind(&mut self, function: Box<dyn Fn()>) {
        self.rust_function = Some(function);
    }

    fn release(&mut self) {
        self.web_closure = None;
        self.rust_function = None;
    }
}
