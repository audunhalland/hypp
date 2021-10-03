#![feature(generic_associated_types)]

use hypp::prelude::*;

use wasm_bindgen::prelude::*;

hypp::component! {
    App() {
        todo_items: Vec<String>,
        id_counter: usize,
    }

    fn add_item(&mut self) {
        *self.id_counter += 1;
        self.todo_items.push(format!("Item {}", *self.id_counter));
    }

    <div>
        <Headline n_items={todo_items.len()} />
        <button onClick={Self::add_item}>
            "Add item"
        </button>
    </div>
    <div>
        if todo_items.is_empty() {
            "No items"
        } else {
            <ul>
                for item in todo_items {
                    <li>{item}</li>
                }
            </ul>
        }
    </div>
}

hypp::component! {
    Headline(n_items: usize) {}

    <div>
        match n_items {
            0 => "Welcome to Hypp TODO. You have no items."
            1 => "Nice, you now have 1 item."
            other => {format!("Good, you seem to get the hang of it. You now have {} items.", other)}
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
