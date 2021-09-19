use hypp::prelude::*;

use wasm_bindgen::prelude::*;

use web_sys::console;

hypp::component! {
    App() {
        toggled: bool,
    }

    fn handle_click(&mut self) {
        console::log_1(&"Toggling!".into());
        self.toggled = !self.toggled;
    }

    <button on_click={Self::handle_click}>
        if self.toggled {
            "Toggled"
        } else {
            "Not toggled"
        }
    </button>
}

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    let hypp = web::WebHypp::new();
    App::mount(
        AppProps {
            __phantom: std::marker::PhantomData,
        },
        &mut hypp.builder_at_body(),
    )
    .unwrap();

    Ok(())
}
