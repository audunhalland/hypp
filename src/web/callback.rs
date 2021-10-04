use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;

use super::{WebFunction, WebHypp};
use crate::{Call, CallbackSlot};

pub struct WebCallbackSlot<Args: 'static> {
    web_closure: Option<Closure<dyn Fn()>>,
    function: Option<WebFunction<Args>>,
}

impl<Args> WebCallbackSlot<Args> {
    fn call(&self, args: Args) {
        let function = self
            .function
            .as_ref()
            .expect("No Rust function defined in callback");

        function.call(args);
    }

    pub fn web_closure(&self) -> &Closure<dyn Fn()> {
        self.web_closure.as_ref().unwrap()
    }
}

impl<Args> Drop for WebCallbackSlot<Args> {
    fn drop(&mut self) {
        // Logging..
        // console::log_1(&"Dropping callback slot!".into());
    }
}

pub fn new_slot<Args>() -> Rc<RefCell<WebCallbackSlot<Args>>> {
    let callback = Rc::new(RefCell::new(WebCallbackSlot {
        web_closure: None,
        function: None,
    }));
    let js_ref = callback.clone();

    let web_closure = Closure::wrap(Box::new(move || {
        unimplemented!("Figure out how to use 'dynamic' types here");
        //js_ref.borrow().call();
    }) as Box<dyn Fn()>);

    callback.borrow_mut().web_closure = Some(web_closure);

    callback
}

impl<Args> CallbackSlot<WebHypp, Args> for WebCallbackSlot<Args> {
    fn bind(&mut self, function: WebFunction<Args>) {
        self.function = Some(function);
    }

    fn release(&mut self) {
        self.web_closure = None;
        self.function = None;
    }
}
