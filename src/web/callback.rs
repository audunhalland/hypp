use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use super::WebHypp;
use crate::CallbackSlot;

pub struct WebCallbackSlot {
    web_closure: Option<Closure<dyn Fn()>>,
    function: Option<Rc<dyn Fn()>>,
}

impl WebCallbackSlot {
    fn call(&self) {
        let function = self
            .function
            .as_ref()
            .expect("No Rust function defined in callback");

        function();
    }

    pub fn web_closure(&self) -> &Closure<dyn Fn()> {
        self.web_closure.as_ref().unwrap()
    }
}

impl Drop for WebCallbackSlot {
    fn drop(&mut self) {
        // Logging..
        // console::log_1(&"Dropping callback slot!".into());
    }
}

pub fn new_slot() -> Rc<RefCell<WebCallbackSlot>> {
    let callback = Rc::new(RefCell::new(WebCallbackSlot {
        web_closure: None,
        function: None,
    }));
    let js_ref = callback.clone();

    let web_closure = Closure::wrap(Box::new(move || {
        //unimplemented!("Figure out how to use 'dynamic' types here");
        js_ref.borrow().call();
    }) as Box<dyn Fn()>);

    callback.borrow_mut().web_closure = Some(web_closure);

    callback
}

impl<Args> CallbackSlot<WebHypp, Args> for WebCallbackSlot {
    fn bind(&mut self, function: Rc<dyn Fn()>) {
        self.function = Some(function);
    }

    fn release(&mut self) {
        self.web_closure = None;
        self.function = None;
    }
}
