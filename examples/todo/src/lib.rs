#![feature(generic_associated_types)]

use hypp::prelude::*;

use wasm_bindgen::prelude::*;

pub struct TodoModel {
    id: usize,
    description: String,
    resolved: bool,
}

hypp::component_dbg! {
    App() {
        todo_items: Vec<TodoModel>,
        id_counter: usize,
    }

    fn add_item(&mut self) {
        *self.id_counter += 1;
        self.todo_items.push(TodoModel {
            id: *self.id_counter,
            description: format!("Item {}", *self.id_counter),
            resolved: false,
        });
    }

    fn on_resolve(&mut self, id: usize) {
        tracing::debug!("App::on_resolve");

        let todo_item = self.todo_items
            .iter_mut()
            .find(|todo_item| todo_item.id == id);

        if let Some(todo_item) = todo_item {
            todo_item.resolved = true;
        }

        tracing::debug!("App::on_resolve DONE");
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
                    <li>
                        <div>
                            {&item.description}
                            if item.resolved {
                                <span style="color:green;">" Resolved"</span>
                            } else {
                                <>
                                    <span style="color:red;">" Pending"</span>
                                    <ResolveButton todo_id={item.id} on_resolve={Self::on_resolve} />
                                </>
                            }
                        </div>
                    </li>
                }
            </ul>
        }
    </div>
}

hypp::component_dbg! {
    ResolveButton<H: ::hypp::Hypp + 'static>(
        todo_id: usize,
        on_resolve: &H::Shared<dyn Fn(usize) + 'static>
    ) {}

    fn handle_click(&mut self) {
        tracing::debug!("ResolveButton::handle_click");
        (self.on_resolve)(*self.todo_id);
    }

    <button onClick={Self::handle_click}>
        "Set done!"
    </button>
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
