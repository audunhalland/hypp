use crate::error::Error;

use super::server_dom::{Node, NodeKind, RcNode};
use crate::{AsNode, ConstOpCode, DomVM, Hypp};

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
            next_child: self.body.first_child(),
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

    fn set_text(node: &Self::Text, text: &str) {
        node.set_text(text);
    }
}

pub struct ServerBuilder {
    element: RcNode,
    next_child: Option<RcNode>,
}

impl ServerBuilder {
    fn enter_element(&mut self, tag_name: &'static str) -> RcNode {
        let element = Node::create_element(tag_name);
        self.element
            .insert_before(element.clone(), self.next_child.clone());

        self.element = element.clone();
        self.next_child = None;

        element
    }

    fn text(&mut self, text: &str) -> RcNode {
        let node = Node::create_text(text.to_string());
        self.element
            .insert_before(node.clone(), self.next_child.clone());

        node
    }

    fn exit_element(&mut self) -> RcNode {
        let parent_element = self
            .element
            .parent()
            .expect("Failed to exit: no parent element");

        self.next_child = self.element.next_sibling();

        std::mem::replace(&mut self.element, parent_element)
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<RcNode, Error> {
        let next_child = self.next_child.take();

        match next_child {
            Some(next_child) => match &next_child.kind {
                NodeKind::Element {
                    tag_name: node_tag_name,
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

impl<'doc> DomVM<'doc, ServerHypp> for ServerBuilder {
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<RcNode, Error> {
        let mut result = Err(Error::NoProgram);

        for opcode in program {
            match opcode {
                ConstOpCode::EnterElement(tag_name) => {
                    result = Ok(self.enter_element(tag_name));
                }
                ConstOpCode::Attribute(_name, _value) => {
                    return Err(Error::SetAttribute);
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

    fn text(&mut self, text: &str) -> Result<RcNode, Error> {
        Ok(self.text(text))
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<RcNode, Error> {
        let child = self
            .next_child
            .take()
            .expect("Expected an element to remove");
        child.assert_is_element_with_tag_name(tag_name);

        self.next_child = child.next_sibling();

        self.element.remove_child(child.clone());

        Ok(child)
    }

    fn remove_text(&mut self) -> Result<RcNode, Error> {
        let child = self.next_child.take().expect("Expected a node to remove");

        self.next_child = child.next_sibling();

        self.element.remove_child(child.clone());

        Ok(child)
    }

    fn advance_to_first_child_of(&mut self, element: &RcNode) {
        self.element = element.clone();
        self.next_child = element.first_child();
    }

    fn advance_to_next_sibling_of(&mut self, node: &RcNode) {
        self.element = node.parent().unwrap();
        self.next_child = node.next_sibling();
    }

    fn skip_const_program(&mut self, program: &[ConstOpCode]) {
        for opcode in program {
            match opcode {
                ConstOpCode::EnterElement(tag_name) => {
                    self.element = self.next_child.take().expect("Expected an element");
                    self.element.assert_is_element_with_tag_name(tag_name);

                    self.next_child = self.element.first_child();
                }
                ConstOpCode::Attribute(_name, _value) => {}
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
