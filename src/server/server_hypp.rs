use crate::error::Error;

use super::server_dom::{AttributeValue, Node, NodeKind, RcNode};
use crate::{AsNode, Callback, ConstOpCode, Cursor, Hypp, Span};

impl AsNode<ServerHypp> for RcNode {
    #[inline]
    fn as_node(&self) -> &RcNode {
        self
    }
}

pub struct ServerHypp {
    body: RcNode,
}

impl ServerHypp {
    pub fn new() -> Self {
        Self {
            body: Node::create_element("body"),
        }
    }

    pub fn builder_at_body(&self) -> ServerBuilder {
        ServerBuilder {
            element: self.body.clone(),
            // Start at the _end_ of the child list:
            next_child: None,
            loaded_attribute_name: None,
        }
    }

    pub fn render(&self) -> String {
        self.body.to_string()
    }
}

impl Hypp for ServerHypp {
    type Node = RcNode;
    type Element = RcNode;
    type Text = RcNode;

    type Callback = ();

    fn set_text(node: &Self::Text, text: &str) {
        node.set_text(text);
    }

    fn traversal_direction() -> crate::TraversalDirection {
        crate::TraversalDirection::LastToFirst
    }
}

impl Span<ServerHypp> for RcNode {
    fn is_anchored(&self) -> bool {
        true
    }

    fn pass_over(&self, cursor: &mut dyn Cursor<ServerHypp>) -> bool {
        cursor.move_to_following_sibling_of(&self);
        true
    }

    fn erase(&self, cursor: &mut dyn Cursor<ServerHypp>) -> bool {
        cursor.remove_node().unwrap();
        true
    }
}

impl Callback for () {
    fn bind(&self, _function: Box<dyn Fn()>) {}

    fn release(&self) {}
}

pub struct ServerBuilder {
    element: RcNode,
    next_child: Option<RcNode>,
    loaded_attribute_name: Option<&'static str>,
}

impl ServerBuilder {
    fn current_child(&self) -> Option<RcNode> {
        match &self.next_child {
            Some(next_child) => next_child.prev_sibling(),
            None => self.element.last_child(),
        }
    }

    fn enter_element(&mut self, tag_name: &'static str) -> RcNode {
        let element = Node::create_element(tag_name);
        self.element
            .insert_before(element.clone(), self.next_child.clone());

        self.element = element.clone();
        // Start at "last child" (the new element has no children)
        self.next_child = None;

        element
    }

    fn text(&mut self, text: &str) -> RcNode {
        let node = Node::create_text(text.to_string());
        self.element
            .insert_before(node.clone(), self.next_child.clone());

        // The next child is the newly created node
        self.next_child = Some(node.clone());

        node
    }

    fn exit_element(&mut self) -> RcNode {
        let parent_element = self
            .element
            .parent()
            .expect("Failed to exit: no parent element");

        // The next child is the element just exited
        self.next_child = Some(self.element.clone());

        std::mem::replace(&mut self.element, parent_element)
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<RcNode, Error> {
        let next_child = self.next_child.take();

        match next_child {
            Some(next_child) => match &next_child.kind {
                NodeKind::Element {
                    tag_name: node_tag_name,
                    ..
                } => {
                    if *node_tag_name != tag_name {
                        Err(Error::RemoveElement)
                    } else {
                        let next_sibling = next_child.next_sibling();
                        self.element.remove_child(next_child.clone());
                        self.next_child = next_sibling;

                        Ok(next_child)
                    }
                }
                _ => Err(Error::RemoveElement),
            },
            None => Err(Error::RemoveElement),
        }
    }
}

impl Cursor<ServerHypp> for ServerBuilder {
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<RcNode, Error> {
        let mut result = Err(Error::NoProgram);

        for opcode in program {
            match opcode {
                ConstOpCode::EnterElement(tag_name) => {
                    result = Ok(self.enter_element(tag_name));
                }
                ConstOpCode::AttributeName(name) => {
                    self.loaded_attribute_name = Some(name);
                }
                ConstOpCode::AttributeTextValue(value) => {
                    let attribute_name = self
                        .loaded_attribute_name
                        .expect("Should call AttributeName before setting attribute value");

                    match &self.element.kind {
                        NodeKind::Element { attributes, .. } => {
                            attributes
                                .borrow_mut()
                                .map
                                .insert(attribute_name, AttributeValue::Static(value));
                            self.loaded_attribute_name = None;
                        }
                        _ => return Err(Error::SetAttribute),
                    };
                }
                ConstOpCode::Text(text) => {
                    result = Ok(self.text(text));
                }
                ConstOpCode::ExitElement => {
                    result = Ok(self.exit_element());
                }
                ConstOpCode::RemoveElement(tag_name) => {
                    result = Ok(self.remove_element(tag_name)?);
                }
            };
        }

        result
    }

    fn const_exec_text(&mut self, program: &[ConstOpCode]) -> Result<RcNode, Error> {
        self.const_exec_element(program)
    }

    fn attribute_value_callback(&mut self) -> Result<(), Error> {
        // Callbacks do nothing on the server
        Ok(())
    }

    fn text(&mut self, text: &str) -> Result<RcNode, Error> {
        Ok(self.text(text))
    }

    fn remove_node(&mut self) -> Result<(), Error> {
        let child = self.current_child().expect("Expected a node to remove");
        self.element.remove_child(child);

        Ok(())
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<RcNode, Error> {
        let child = self.current_child().expect("Expected an element to remove");
        child.assert_is_element_with_tag_name(tag_name);

        self.element.remove_child(child.clone());

        Ok(child)
    }

    fn remove_text(&mut self) -> Result<RcNode, Error> {
        let child = self.current_child().expect("Expected an element to remove");
        self.element.remove_child(child.clone());

        Ok(child)
    }

    fn move_to_children_of(&mut self, element: &RcNode) {
        self.element = element.clone();
        // Start at the end of the child list:
        self.next_child = None;
    }

    fn move_to_following_sibling(&mut self) -> Result<(), Error> {
        self.next_child = Some(self.current_child().expect("Moved past the first childa"));
        Ok(())
    }

    fn move_to_following_sibling_of(&mut self, node: &RcNode) {
        self.element = node.parent().unwrap();
        // The next child is the node itself (so the cursor points before that node)
        self.next_child = Some(node.clone());
    }

    fn skip_const_program(&mut self, program: &[ConstOpCode]) {
        for opcode in program {
            match opcode {
                ConstOpCode::EnterElement(tag_name) => {
                    self.element = self.next_child.take().expect("Expected an element");
                    self.element.assert_is_element_with_tag_name(tag_name);

                    self.next_child = self.element.first_child();
                }
                ConstOpCode::AttributeName(_) => {}
                ConstOpCode::AttributeTextValue(_) => {}
                ConstOpCode::Text(text) => {
                    let node = self.next_child.take().expect("Expected a text");
                    node.assert_is_text(text);
                    self.next_child = node.next_sibling();
                }
                ConstOpCode::ExitElement => {
                    self.exit_element();
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

#[cfg(test)]
mod tests {}
