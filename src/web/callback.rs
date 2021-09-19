use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::closure::Closure;

use crate::Callback;

pub struct WebCallback {
    cell: Rc<RefCell<CallbackCell>>,
    pub web_closure: Closure<dyn Fn()>,
}

impl WebCallback {
    pub fn new() -> Self {
        let local_cell = Rc::new(RefCell::new(CallbackCell {
            rust_function: None,
        }));
        let js_cell = local_cell.clone();

        let web_closure = Closure::wrap(Box::new(move || {
            js_cell.borrow().call();
        }) as Box<dyn Fn()>);

        Self {
            cell: local_cell,
            web_closure: web_closure,
        }
    }
}

impl Callback for WebCallback {
    fn bind(&self, function: Box<dyn Fn()>) {
        let mut cell = self.cell.borrow_mut();
        cell.rust_function = Some(function);
    }

    fn release(&self) {
        let mut cell = self.cell.borrow_mut();
        cell.rust_function = None;
    }
}

struct CallbackCell {
    rust_function: Option<Box<dyn Fn()>>,
}

impl CallbackCell {
    fn call(&self) {
        let rust_function = self
            .rust_function
            .as_ref()
            .expect("No Rust function defined in callback");

        rust_function();
    }
}
