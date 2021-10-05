use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::convert::FromWasmAbi;
use wasm_bindgen::prelude::*;

use super::WebHypp;
use crate::{EventKind, Listen, TemplNS};

pub struct WebCallback<NS: TemplNS, EK: EventKind<NS>, W> {
    web_closure: Option<Closure<dyn Fn(W)>>,
    function: Option<Rc<dyn Fn(EK::Event)>>,
}

impl<NS: TemplNS, EK: EventKind<NS>, W> WebCallback<NS, EK, W>
where
    W: FromWasmAbi + 'static,
{
    fn call(&self, web_event: &W) {
        let function = self
            .function
            .as_ref()
            .expect("No Rust function defined in callback");

        let event = EK::from_web_event(web_event);

        function(event);
    }

    pub fn web_closure(&self) -> &Closure<dyn Fn(W)> {
        self.web_closure.as_ref().unwrap()
    }
}

impl<NS: TemplNS, EK: EventKind<NS>, W> Drop for WebCallback<NS, EK, W> {
    fn drop(&mut self) {
        // Logging..
        // console::log_1(&"Dropping callback slot!".into());
    }
}

pub fn new_callback<NS: TemplNS, EK: EventKind<NS> + 'static, W: FromWasmAbi + 'static>(
) -> Rc<RefCell<WebCallback<NS, EK, W>>> {
    let callback = Rc::new(RefCell::new(WebCallback {
        web_closure: None,
        function: None,
    }));
    let js_ref = callback.clone();

    let web_closure = Closure::wrap(Box::new(move |web_event| {
        js_ref.borrow().call(&web_event);
    }) as Box<dyn Fn(W)>);

    callback.borrow_mut().web_closure = Some(web_closure);

    callback
}

impl<NS: TemplNS, EK: EventKind<NS>, W> Listen<WebHypp, EK::Event> for WebCallback<NS, EK, W> {
    fn listen(&mut self, function: Rc<dyn Fn(EK::Event)>) {
        self.function = Some(function);
    }

    fn forget(&mut self) {
        self.web_closure = None;
        self.function = None;
    }
}

impl<NS: TemplNS, EK: EventKind<NS>, W> Listen<WebHypp, EK::Event>
    for Rc<RefCell<WebCallback<NS, EK, W>>>
{
    fn listen(&mut self, function: Rc<dyn Fn(EK::Event)>) {
        self.borrow_mut().function = Some(function);
    }

    fn forget(&mut self) {
        let mut borrow = self.borrow_mut();

        borrow.web_closure = None;
        borrow.function = None;
    }
}
