use crate::error::Error;

use super::{AsNode, ConstOpCode, CursorCmd, DomVM, Hypp};

pub struct WebHypp {
    _window: web_sys::Window,
    document: web_sys::Document,
    body: web_sys::HtmlElement,
}

impl WebHypp {
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
            hypp: self,
            element_stack: vec![],
        }
    }
}

impl AsNode<WebHypp> for web_sys::Element {
    #[inline]
    fn as_node(&self) -> &web_sys::Node {
        self
    }
}

impl AsNode<WebHypp> for web_sys::Text {
    fn as_node(&self) -> &web_sys::Node {
        self
    }
}

impl Hypp for WebHypp {
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
    hypp: &'a WebHypp,
    element_stack: Vec<web_sys::Element>,
}

impl<'a> WebBuilder<'a> {
    fn parent(&self) -> &web_sys::Element {
        self.element_stack.last().unwrap_or_else(|| &self.hypp.body)
    }

    fn append_child(&mut self, child: &web_sys::Node) -> Result<(), Error> {
        self.parent()
            .append_child(child)
            .map(|_| ())
            .map_err(|_| Error::AddChild)
    }

    /*
    fn enter_element(&mut self, tag_name: &'static str) -> Result<web_sys::Element, Error> {
        let element = self.hypp.document.create_element(tag_name).unwrap();
        self.append_child(element.as_node())?;
        self.element_stack.push(element.clone());
        Ok(element)
    }

    fn exit_element(&mut self) -> Result<web_sys::Element, Error> {
        match self.element_stack.pop() {
            Some(element) => Ok(element),
            None => Err(Error::ExitElement),
        }
    }
    */
}

impl<'doc> DomVM<'doc, WebHypp> for WebBuilder<'doc> {
    fn const_exec_element(&mut self, _program: &[ConstOpCode]) -> Result<web_sys::Element, Error> {
        panic!();
    }

    fn const_exec_text(&mut self, _program: &[ConstOpCode]) -> Result<web_sys::Text, Error> {
        panic!()
    }

    fn text(&mut self, text: &str) -> Result<web_sys::Text, Error> {
        let text_node = self.hypp.document.create_text_node(text);
        self.append_child(text_node.as_node())?;
        Ok(text_node)
    }

    fn remove_element(&mut self, _tag_name: &'static str) -> Result<web_sys::Element, Error> {
        unimplemented!()
    }

    fn remove_text(&mut self) -> Result<web_sys::Element, Error> {
        unimplemented!()
    }

    fn advance(&mut self, _commands: &[CursorCmd]) {}

    fn skip_const_program(&mut self, _program: &[ConstOpCode]) {
        panic!()
    }
}
