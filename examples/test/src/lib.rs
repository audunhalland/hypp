use hypp::*;

use wasm_bindgen::prelude::*;

use web_sys::console;

hypp::component! {
    App() {}

    fn handle_click(&mut self) {
        console::log_1(&"Inside handle_click!".into());
    }

    <button on_click={Self::handle_click}>
        "Some text to test!"
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
