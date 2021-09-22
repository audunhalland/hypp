#![feature(generic_associated_types)]

use hypp::prelude::*;

use wasm_bindgen::prelude::*;

use web_sys::console;

hypp::component! {
    App() {
        toggled: bool,
    }

    fn handle_click(&mut self) {
        console::log_1(&"Toggling!".into());
        *self.toggled = !*self.toggled;
    }

    <div>
        "Hypp: The perfect framework for "
        <strong>
            "Button toggling!"
        </strong>
    </div>
    <div>
        <button on_click={Self::handle_click}>
        if toggled {
            "Toggled"
        } else {
            "Not toggled"
        }
        </button>
    </div>
}

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    let hypp = hypp::web::WebHypp::new();
    App::mount(__AppProps {}, &mut hypp.builder_at_body()).unwrap();

    Ok(())
}
