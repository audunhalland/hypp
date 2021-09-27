#![feature(generic_associated_types)]

use hypp::prelude::*;

use wasm_bindgen::prelude::*;

hypp::component! {
    App<Html>() {
        toggled: bool,
    }

    fn handle_click(&mut self) {
        tracing::info!("Toggling!");
        *self.toggled = !*self.toggled;
    }

    <Headline toggled={*toggled} />
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

hypp::component! {
    Headline<Html>(toggled: bool) {}

    <div>
        if toggled {
            "Now try to turn it off again:"
        } else {
            "Hypp: The perfect framework for "<strong>"Button toggling!"</strong>
        }
    </div>
}

// Called when the wasm module is instantiated
#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    tracing_wasm::set_as_global_default();

    let mut hypp = hypp::web::WebHypp::new();
    hypp.mount::<App<_>>().unwrap();
    hypp.store_global();

    Ok(())
}
