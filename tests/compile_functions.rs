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

hypp::component_dbg! {
    DoWithId() {
        // BUG: need to have a field
        dummy: usize,
    }

    fn handle_id_click(&mut self, id: usize) {}

    <IdButton id={42} callback={Self::handle_id_click} />
}
