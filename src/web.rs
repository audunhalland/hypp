use crate::error::Error;

use super::{AsNode, Awe};

pub struct WebAwe {
    _window: web_sys::Window,
    document: web_sys::Document,
    body: web_sys::HtmlElement,
}

impl WebAwe {
    pub fn new() -> Self {
        let window = web_sys::window().unwrap();
        let document = window.document().unwrap();
        let body = document.body().unwrap();

        Self {
            _window: window,
            document,
            body,
        }
    }

    pub fn document(&self) -> &web_sys::Document {
        &self.document
    }
}

impl AsNode<WebAwe> for web_sys::Element {
    #[inline]
    fn as_node(&self) -> &web_sys::Node {
        self
    }
}

impl AsNode<WebAwe> for web_sys::Text {
    fn as_node(&self) -> &web_sys::Node {
        self
    }
}

impl Awe for WebAwe {
    type Node = web_sys::Node;
    type Element = web_sys::Element;
    type Text = web_sys::Text;

    fn body(&self) -> &Self::Element {
        &self.body
    }

    fn create_element(&self, tag_name: &'static str) -> Self::Element {
        self.document.create_element(tag_name).unwrap()
    }

    fn create_empty_text(&self) -> Self::Text {
        self.document.create_text_node("")
    }

    fn insert_child_before(
        parent: &Self::Element,
        child: &Self::Node,
        before: Option<&Self::Node>,
    ) -> Result<(), Error> {
        match parent.insert_before(child, before) {
            Ok(_) => Ok(()),
            Err(_) => Err(Error::AddChild),
        }
    }

    fn remove_child(parent: &Self::Element, child: &Self::Node) -> Result<(), Error> {
        match parent.remove_child(child) {
            Ok(_) => Ok(()),
            Err(_) => Err(Error::AddChild), // FIXME
        }
    }

    fn set_text(node: &Self::Text, text: &str) {
        node.set_data(text);
    }
}
