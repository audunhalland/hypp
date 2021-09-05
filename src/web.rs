use crate::error::Error;

use super::{AsNode, Yay};

pub struct WebYay {
    _window: web_sys::Window,
    document: web_sys::Document,
    body: web_sys::HtmlElement,
}

impl WebYay {
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

impl AsNode<WebYay> for web_sys::Element {
    #[inline]
    fn as_node(&self) -> &web_sys::Node {
        self
    }
}

impl AsNode<WebYay> for web_sys::Text {
    fn as_node(&self) -> &web_sys::Node {
        self
    }
}

impl Yay for WebYay {
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

    fn append_child(parent: &Self::Element, child: &Self::Node) -> Result<(), Error> {
        match parent.append_child(child) {
            Ok(_) => Ok(()),
            Err(_) => Err(Error::AppendChild),
        }
    }

    fn set_text(node: &Self::Text, text: &str) {
        node.set_data(text);
    }
}
