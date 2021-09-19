use crate::error::Error;

use super::{AsNode, ConstOpCode, DomVM, Hypp};

use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;

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

    pub fn builder_at_body<'a>(&'a self) -> WebBuilder {
        WebBuilder {
            document: self.document.clone(),
            element: self.body.clone().dyn_into().unwrap(),
            next_child: self.body.first_child(),
            loaded_attribute_name: None,
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

    fn set_text(node: &Self::Text, text: &str) {
        node.set_data(text);
    }
}

pub struct WebBuilder {
    document: web_sys::Document,
    element: web_sys::Element,
    next_child: Option<web_sys::Node>,
    loaded_attribute_name: Option<&'static str>,
}

impl WebBuilder {
    fn enter_element(&mut self, tag_name: &str) -> Result<web_sys::Element, Error> {
        let element = self.document.create_element(tag_name).unwrap();
        self.element
            .insert_before(element.as_ref(), self.next_child.as_ref())?;

        self.element = element.clone();
        self.next_child = None;

        Ok(element)
    }

    fn set_attribute(&mut self, value: &str) -> Result<(), Error> {
        let attribute_name = self
            .loaded_attribute_name
            .expect("needs an attribute name loaded");
        self.element.set_attribute(attribute_name, value)?;

        Ok(())
    }

    fn text(&mut self, text: &str) -> Result<web_sys::Text, Error> {
        let node = self.document.create_text_node(text);
        self.element
            .insert_before(node.as_ref(), self.next_child.as_ref())?;

        Ok(node)
    }

    fn exit_element(&mut self) -> Result<web_sys::Element, Error> {
        let parent_node = self
            .element
            .parent_node()
            .expect("Failed to exit: no parent node");

        self.next_child = self.element.next_sibling();

        Ok(std::mem::replace(
            &mut self.element,
            parent_node.try_into_element()?,
        ))
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<web_sys::Element, Error> {
        let next_element = self
            .next_child
            .take()
            .ok_or(Error::RemoveElement)?
            .try_into_element()?;

        if next_element.tag_name() != tag_name {
            Err(Error::RemoveElement)
        } else {
            let next_sibling = next_element.next_sibling();
            self.element.remove_child(&next_element)?;
            self.next_child = next_sibling;

            Ok(next_element)
        }
    }
}

impl<'doc> DomVM<'doc, WebHypp> for WebBuilder {
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<web_sys::Element, Error> {
        let mut result = Err(Error::NoProgram);

        for opcode in program {
            match opcode {
                ConstOpCode::EnterElement(tag_name) => {
                    result = Ok(self.enter_element(tag_name)?);
                }
                ConstOpCode::AttributeName(name) => {
                    self.loaded_attribute_name = Some(name);
                }
                ConstOpCode::AttributeTextValue(value) => {
                    self.set_attribute(value)?;
                }
                ConstOpCode::Text(text) => {
                    self.text(text)?;
                }
                ConstOpCode::ExitElement => {
                    result = Ok(self.exit_element()?);
                }
                ConstOpCode::RemoveElement(tag_name) => {
                    result = Ok(self.remove_element(tag_name)?);
                }
            };
        }

        result
    }

    fn const_exec_text(&mut self, program: &[ConstOpCode]) -> Result<web_sys::Text, Error> {
        let mut result = Err(Error::NoProgram);

        for opcode in program {
            match opcode {
                ConstOpCode::EnterElement(tag_name) => {
                    self.enter_element(tag_name)?;
                }
                ConstOpCode::AttributeName(name) => {
                    self.loaded_attribute_name = Some(name);
                }
                ConstOpCode::AttributeTextValue(value) => {
                    self.set_attribute(value)?;
                }
                ConstOpCode::Text(text) => {
                    result = Ok(self.text(text)?);
                }
                ConstOpCode::ExitElement => {
                    self.exit_element()?;
                }
                ConstOpCode::RemoveElement(tag_name) => {
                    self.remove_element(tag_name)?;
                }
            };
        }

        result
    }

    fn text(&mut self, text: &str) -> Result<web_sys::Text, Error> {
        Ok(self.text(text)?)
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<web_sys::Element, Error> {
        let child = self
            .next_child
            .take()
            .expect("Expected an element to remove");
        child.assert_is_element_with_tag_name(tag_name);

        self.next_child = child.next_sibling();

        child.try_into_element()
    }

    fn remove_text(&mut self) -> Result<web_sys::Text, Error> {
        let child = self
            .next_child
            .take()
            .expect("Expected an element to remove");

        self.next_child = child.next_sibling();

        child.try_into_text()
    }

    fn advance_to_first_child_of(&mut self, element: &web_sys::Element) {
        self.element = element.clone();
        self.next_child = element.first_child();
    }

    fn advance_to_next_sibling_of(&mut self, node: &web_sys::Node) {
        self.element = node.parent_node().unwrap().try_into_element().unwrap();
        self.next_child = node.next_sibling();
    }

    fn skip_const_program(&mut self, program: &[ConstOpCode]) {
        for opcode in program {
            match opcode {
                ConstOpCode::EnterElement(tag_name) => {
                    self.element = self
                        .next_child
                        .take()
                        .expect("Expected a node to enter")
                        .try_into_element()
                        .expect("Expected an element to enter");
                    self.element.assert_is_element_with_tag_name(tag_name);

                    self.next_child = self.element.first_child();
                }
                ConstOpCode::AttributeName(_) => {}
                ConstOpCode::AttributeTextValue(_) => {}
                ConstOpCode::Text(_) => {
                    let node = self.next_child.take().expect("Expected a text");
                    self.next_child = node.next_sibling();
                }
                ConstOpCode::ExitElement => {
                    self.exit_element().expect("expecten an element to exit");
                }
                ConstOpCode::RemoveElement(tag_name) => {
                    panic!(
                        "Cannot remove an element \"{}\" in a skip setting?",
                        tag_name
                    );
                }
            };
        }
    }
}

impl From<wasm_bindgen::JsValue> for Error {
    fn from(_js_error: wasm_bindgen::JsValue) -> Self {
        Error::JsError
    }
}

trait NodeExt {
    fn assert_is_element_with_tag_name(&self, tag_name: &str);

    fn try_into_element(self) -> Result<web_sys::Element, Error>;
    fn try_into_text(self) -> Result<web_sys::Text, Error>;
}

impl NodeExt for web_sys::Node {
    fn assert_is_element_with_tag_name(&self, tag_name: &str) {
        match self.dyn_ref::<web_sys::Element>() {
            Some(element) => {
                assert_eq!(element.tag_name(), tag_name)
            }
            None => {
                panic!(
                    "Expected an element with tag name {}, got something else",
                    tag_name
                );
            }
        }
    }

    fn try_into_element(self) -> Result<web_sys::Element, Error> {
        match self.dyn_into::<web_sys::Element>() {
            Ok(element) => Ok(element),
            Err(_) => Err(Error::NotAnElement),
        }
    }

    fn try_into_text(self) -> Result<web_sys::Text, Error> {
        match self.dyn_into::<web_sys::Text>() {
            Ok(text) => Ok(text),
            Err(_) => Err(Error::NotAText),
        }
    }
}
