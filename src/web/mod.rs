use crate::error::Error;
use crate::span::{AsSpan, SpanAdapter};
use crate::{AsNode, ConstOpCode, Cursor, Hypp, Mount, NSCursor, Span, TemplNS};

use std::cell::RefCell;
use std::rc::Rc;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

mod callback;

#[wasm_bindgen]
pub struct WebHypp {
    _window: web_sys::Window,
    document: web_sys::Document,
    body: web_sys::HtmlElement,
    mounts: Vec<Box<dyn std::any::Any>>,
}

impl Drop for WebHypp {
    fn drop(&mut self) {
        tracing::warn!("WebHypp dropped!");
    }
}

#[wasm_bindgen(inline_js = "export function store_global_hypp(hypp) { window.hypp = hypp; }")]
extern "C" {
    fn store_global_hypp(hypp: JsValue);
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
            mounts: vec![],
        }
    }

    pub fn store_global(self) {
        let js_value: JsValue = self.into();

        unsafe { store_global_hypp(js_value) }
    }

    pub fn document(&self) -> &web_sys::Document {
        &self.document
    }

    pub fn builder_at_body(&self) -> WebBuilder {
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

    type Anchor = WebBuilder;
    type Cursor<NS: TemplNS> = WebBuilder;

    type Shared<T>
    where
        T: 'static,
    = std::rc::Rc<std::cell::RefCell<T>>;

    type CallbackSlot = callback::WebCallbackSlot;

    fn make_shared<T: 'static>(value: T) -> Self::Shared<T> {
        Rc::new(RefCell::new(value))
    }

    fn mount<'p, M: Mount<'p, WebHypp> + 'static>(&mut self) -> Result<(), Error> {
        let mut builder = self.builder_at_body();
        let mounted = M::mount(&mut builder)?;

        self.mounts.push(Box::new(mounted));

        Ok(())
    }

    fn set_text(node: &Self::Text, text: &str) {
        node.set_data(text);
    }

    fn traversal_direction() -> crate::TraversalDirection {
        crate::TraversalDirection::LastToFirst
    }
}

impl AsSpan for web_sys::Node {
    type Target = web_sys::Node;

    fn as_span<'a>(&'a self) -> SpanAdapter<'a, web_sys::Node> {
        SpanAdapter(self)
    }
}

impl AsSpan for web_sys::Element {
    type Target = web_sys::Node;

    fn as_span<'a>(&'a self) -> SpanAdapter<'a, web_sys::Node> {
        SpanAdapter(self)
    }
}

impl AsSpan for web_sys::Text {
    type Target = web_sys::Node;

    fn as_span<'a>(&'a self) -> SpanAdapter<'a, web_sys::Node> {
        SpanAdapter(self)
    }
}

impl<'a> Span<WebHypp> for SpanAdapter<'a, web_sys::Node> {
    fn is_anchored(&self) -> bool {
        true
    }

    fn pass_over(&mut self, cursor: &mut dyn Cursor<WebHypp>) -> bool {
        cursor.move_to_following_sibling_of(&self.0);
        true
    }

    fn erase(&mut self, cursor: &mut dyn Cursor<WebHypp>) -> bool {
        cursor.remove_node().unwrap();
        true
    }
}

#[derive(Clone)]
pub struct WebBuilder {
    document: web_sys::Document,
    element: web_sys::Element,
    next_child: Option<web_sys::Node>,
    loaded_attribute_name: Option<&'static str>,
}

impl WebBuilder {
    fn current_child(&self) -> Option<web_sys::Node> {
        match &self.next_child {
            Some(next_child) => next_child.previous_sibling(),
            None => self.element.last_child(),
        }
    }

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
        self.next_child = Some(node.as_node().clone());

        Ok(node)
    }

    fn exit_element(&mut self) -> Result<web_sys::Element, Error> {
        let parent_node = self
            .element
            .parent_node()
            .expect("Failed to exit: no parent node");

        self.next_child = Some(self.element.as_node().clone());

        Ok(std::mem::replace(
            &mut self.element,
            parent_node.try_into_element()?,
        ))
    }

    fn html_element(&self) -> &web_sys::HtmlElement {
        self.element
            .dyn_ref::<web_sys::HtmlElement>()
            .expect("must be a HtmlElement")
    }
}

impl Cursor<WebHypp> for WebBuilder {
    fn anchor(&self) -> WebBuilder {
        self.clone()
    }

    fn attribute_value_callback(
        &mut self,
    ) -> Result<Rc<RefCell<callback::WebCallbackSlot>>, Error> {
        let attribute_name = self
            .loaded_attribute_name
            .expect("needs an attribute name loaded");

        match attribute_name {
            "on_click" => {
                let callback = callback::new_callback();

                {
                    let borrow = callback.borrow();

                    self.html_element()
                        .set_onclick(Some(borrow.web_closure().as_ref().unchecked_ref()));
                }

                Ok(callback)
            }
            _ => Err(Error::SetAttribute),
        }
    }

    fn text(&mut self, text: &str) -> Result<web_sys::Text, Error> {
        Ok(self.text(text)?)
    }

    fn remove_node(&mut self) -> Result<(), Error> {
        let child = self.current_child().expect("Expected a node to remove");
        self.element.remove_child(&child)?;

        Ok(())
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<web_sys::Element, Error> {
        let child = self
            .current_child()
            .expect("Expected an element to remove")
            .try_into_element()?;
        child.assert_is_element_with_tag_name(tag_name);

        self.element.remove_child(&child)?;

        Ok(child)
    }

    fn remove_text(&mut self) -> Result<web_sys::Text, Error> {
        let child = self
            .current_child()
            .expect("Expected an element to remove")
            .try_into_text()?;

        self.element.remove_child(&child)?;

        Ok(child)
    }

    fn move_to_children_of(&mut self, element: &web_sys::Element) {
        self.element = element.clone();
        // Start at the end of the child list:
        self.next_child = None;
    }

    fn move_to_following_sibling(&mut self) -> Result<(), Error> {
        self.next_child = Some(self.current_child().expect("Moved past the first child"));
        Ok(())
    }

    fn move_to_following_sibling_of(&mut self, node: &web_sys::Node) {
        self.element = node.parent_element().unwrap();
        // The next child is the node itself (so the cursor points before that node)
        self.next_child = Some(node.clone());
    }

    fn skip_const_program(&mut self, _program: &[ConstOpCode]) {
        /*
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
        */
        unimplemented!("Not needed?");
    }
}

impl<NS: crate::TemplNS> NSCursor<WebHypp, NS> for WebBuilder {
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

impl crate::Anchor<WebHypp> for WebBuilder {
    fn create_cursor<NS>(&self) -> WebBuilder {
        self.clone()
    }
}
