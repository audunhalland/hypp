use hypp::*;

use wasm_bindgen::prelude::*;

hypp::component! {
    App() {}

    <div>
        "Some text to test!"
    </div>
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
