use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use super::WebHypp;
use crate::{Call, Callback, CallbackSlot};

pub struct WebCallbackSlot {
    web_closure: Option<Closure<dyn Fn()>>,
    callback: Option<Callback<WebHypp>>,
}

impl WebCallbackSlot {
    fn call(&self) {
        let callback = self
            .callback
            .as_ref()
            .expect("No Rust function defined in callback");

        callback.call();
    }

    pub fn web_closure(&self) -> &Closure<dyn Fn()> {
        self.web_closure.as_ref().unwrap()
    }
}

impl Drop for WebCallbackSlot {
    fn drop(&mut self) {
        // Logging..
        // console::log_1(&"Dropping callback!".into());
    }
}

pub fn new_slot() -> Rc<RefCell<WebCallbackSlot>> {
    let callback = Rc::new(RefCell::new(WebCallbackSlot {
        web_closure: None,
        callback: None,
    }));
    let js_ref = callback.clone();

    let web_closure = Closure::wrap(Box::new(move || {
        js_ref.borrow().call();
    }) as Box<dyn Fn()>);

    callback.borrow_mut().web_closure = Some(web_closure);

    callback
}

impl CallbackSlot<WebHypp> for WebCallbackSlot {
    fn bind(&mut self, callback: Callback<WebHypp>) {
        self.callback = Some(callback);
    }

    fn release(&mut self) {
        self.web_closure = None;
        self.callback = None;
    }
}
