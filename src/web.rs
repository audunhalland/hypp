use crate::error::Error;

use super::{AsNode, Awe, DomVM};

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

    pub fn builder_at_body<'a>(&'a self) -> WebBuilder<'a> {
        WebBuilder {
            awe: self,
            element_stack: vec![],
        }
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

pub struct WebBuilder<'a> {
    awe: &'a WebAwe,
    element_stack: Vec<web_sys::Element>,
}

impl<'a> WebBuilder<'a> {
    fn parent(&self) -> &web_sys::Element {
        self.element_stack.last().unwrap_or_else(|| &self.awe.body)
    }

    fn append_child(&mut self, child: &web_sys::Node) -> Result<(), Error> {
        self.parent()
            .append_child(child)
            .map(|_| ())
            .map_err(|_| Error::AddChild)
    }
}

impl<'doc> DomVM<'doc, WebAwe> for WebBuilder<'doc> {
    fn enter_element(&mut self, tag_name: &'static str) -> Result<web_sys::Element, Error> {
        let element = self.awe.document.create_element(tag_name).unwrap();
        self.append_child(element.as_node())?;
        self.element_stack.push(element.clone());
        Ok(element)
    }

    fn attribute(&mut self, _name: &'static str, _value: &'static str) -> Result<(), Error> {
        Err(Error::SetAttribute)
    }

    fn text(&mut self, text: &str) -> Result<web_sys::Text, Error> {
        let text_node = self.awe.document.create_text_node(text);
        self.append_child(text_node.as_node())?;
        Ok(text_node)
    }

    fn exit_element(&mut self) -> Result<(), Error> {
        self.element_stack.pop();
        Ok(())
    }

    fn remove_element(&mut self, _tag_name: &'static str) -> Result<(), Error> {
        unimplemented!()
    }

    fn push_element_context(&mut self, element: web_sys::Element) {
        self.element_stack.push(element);
    }

    fn pop_element_context(&mut self) {
        self.element_stack.pop();
    }
}
