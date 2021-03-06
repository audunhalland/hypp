#![feature(generic_associated_types)]

use hypp::prelude::*;

hypp::component! {
    IdButton<H: ::hypp::Hypp + 'static>(
        id: usize,
        callback: &H::Shared<dyn Fn(usize) + 'static>
    ) {}

    fn handle_click(&mut self) {
        (self.callback)(*self.id);
    }

    <button onClick={Self::handle_click}>
    </button>
}

hypp::component! {
    DoWithId() {}

    fn handle_id_click(&mut self, id: usize) {}

    <IdButton id={42} callback={Self::handle_id_click} />
}

component! {
    Toggle(prop1: bool, prop2: &str) {
        toggled: bool,
    }

    fn handle_click(&mut self) {
        *self.toggled = !*self.toggled;
    }

    <div>
        if prop1 {
            <p>"yep"</p>
        }
    </div>
    <div>
        <button onClick={Self::handle_click}>
            if toggled {
                "Toggled"
            } else {
                "Not toggled"
            }
        </button>
    </div>
}

component! {
    ConditionalCallback(show_button: bool) {
        toggled: bool
    }

    fn handle_click(&mut self) {
        *self.toggled = !*self.toggled;
    }

    if show_button {
        <button onClick={Self::handle_click}>
            if toggled {
                "Toggled"
            } else {
                "Not toggled"
            }
        </button>
    }
}

component! {
    BasicCallbackWithParameterAsProp<H: ::hypp::Hypp + 'static>(
        id: usize,
        callback: &H::Shared<dyn Fn(usize) + 'static>
    ) {}

    fn handle_click(&mut self) {
        (self.callback)(*self.id);
    }

    <button onClick={Self::handle_click}>
    </button>
}

component! {
    BasicCallbackAsProp<H: ::hypp::Hypp + 'static>(
        callback: &H::Shared<dyn Fn() + 'static>
    ) {}

    <button onClick={callback}>
    </button>
}

component! {
    PassCallbackToChildComponent() {}

    fn do_stuff(&mut self) {}

    <BasicCallbackAsProp callback={Self::do_stuff} />
}

component! {
    ListItem(text: &str) {}

    <li>{text}</li>
}

component! {
    AddToList() {
        todo_items: Vec<String>,
        unused: bool,
    }

    fn add_item(&mut self) {
        let count = self.todo_items.len();
        self.todo_items.push(format!("Item {}", count));
    }

    <div>
        <button onClick={Self::add_item}>"Add item"</button>
        if todo_items.is_empty() {
            "No items"
        } else {
            <ul>
                for item in todo_items {
                    <li>{item}</li>
                    <ListItem text={item}/>
                }
            </ul>
        }
    </div>
}

pub struct TodoModel {
    id: usize,
    description: String,
    resolved: bool,
}

hypp::component! {
    PatchChildWithCallback() {
        todo_items: Vec<TodoModel>,
    }

    fn on_resolve(&mut self, id: usize) {
    }

    for item in todo_items {
        <div>
            <ResolveButton todo_id={item.id} on_resolve={Self::on_resolve} />
        </div>
    }
}

hypp::component! {
    // This time the ResolveButton will be ::Boxed, because of the conditional
    ConditionalCallbackToChildComponent() {}

    fn on_resolve(&mut self, id: usize) {}

    if true {
        <ResolveButton todo_id={42} on_resolve={Self::on_resolve} />
    }
}

hypp::component! {
    ResolveButton<H: ::hypp::Hypp + 'static>(
        todo_id: usize,
        on_resolve: &H::Shared<dyn Fn(usize) + 'static>
    ) {}

    fn handle_click(&mut self) {
        (self.on_resolve)(*self.todo_id);
    }

    <button onClick={Self::handle_click}>
    </button>
}
