use std::cell::RefCell;
use std::rc::Rc;

use crate::error::Error;

use crate::{AsNode, ConstOpCode, CursorCmd, DomVM, Hypp};

type Node = Rc<ServerNode>;

impl AsNode<ServerHypp> for Node {
    #[inline]
    fn as_node(&self) -> &Node {
        self
    }
}

pub struct ServerHypp {
    body: Node,
}

impl ServerHypp {
    pub fn new() -> Self {
        Self {
            body: Rc::new(ServerNode::Element(Element {
                tag_name: "body",
                children: RefCell::new(vec![]),
            })),
        }
    }

    pub fn builder_at_body(&self) -> ServerBuilder {
        ServerBuilder {
            stack: vec![Cursor {
                parent: self.body.clone(),
                position: 0,
            }],
        }
    }

    pub fn render(&self) -> String {
        self.body.to_string()
    }
}

impl Hypp for ServerHypp {
    type Node = Node;
    type Element = Node;
    type Text = Node;

    fn remove_child(parent: &Self::Element, child: &Self::Node) -> Result<(), Error> {
        match parent.as_ref() {
            ServerNode::Element(element) => element.remove_child(child),
            _ => Err(Error::AddChild),
        }
    }

    fn set_text(node: &Self::Text, text: &str) {
        match node.as_ref() {
            ServerNode::Text(string) => {
                *string.borrow_mut() = text.to_string();
            }
            _ => panic!(""),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ServerNode {
    Element(Element),
    Fragment(RefCell<Vec<Node>>),
    Text(RefCell<String>),
}

impl ServerNode {
    fn is(&self, other: &Self) -> bool {
        let a = self as *const _;
        let b = other as *const _;

        a == b
    }
}

impl ToString for ServerNode {
    fn to_string(&self) -> String {
        fn recurse(node: &ServerNode, buf: &mut String) {
            match node {
                ServerNode::Element(element) => {
                    buf.push('<');
                    buf.push_str(element.tag_name);

                    if element.children.borrow().is_empty() {
                        buf.push_str("/>");
                    } else {
                        buf.push('>');

                        for child in element.children.borrow().iter() {
                            recurse(child, buf);
                        }

                        buf.push_str("</");
                        buf.push_str(element.tag_name);
                        buf.push('>');
                    }
                }
                ServerNode::Fragment(nodes) => {
                    for child in nodes.borrow().iter() {
                        recurse(child, buf);
                    }
                }
                ServerNode::Text(text) => {
                    buf.push_str(text.borrow().as_str());
                }
            }
        }

        let mut buf = String::new();
        recurse(self, &mut buf);
        buf
    }
}

#[derive(Clone, Debug)]
pub struct Element {
    tag_name: &'static str,
    children: RefCell<Vec<Node>>,
}

impl Element {
    fn remove_child(&self, child: &Node) -> Result<(), Error> {
        if let Some(index) = self.children.borrow().iter().position(|it| it.is(child)) {
            self.children.borrow_mut().remove(index);
            Ok(())
        } else {
            Err(Error::RemoveChild)
        }
    }
}

#[derive(Clone)]
struct Cursor {
    parent: Node,
    position: usize,
}

impl Cursor {
    fn parent_element_ref(&self) -> Result<&Element, Error> {
        match self.parent.as_ref() {
            ServerNode::Element(element) => Ok(element),
            _ => Err(Error::DomCorruption),
        }
    }

    fn next_node(&self) -> Option<Node> {
        let parent_element = self.parent_element_ref().unwrap();
        parent_element
            .children
            .borrow()
            .get(self.position as usize)
            .cloned()
    }

    fn nth_child(&self, index: usize) -> Node {
        let parent_element = self.parent_element_ref().unwrap();
        parent_element.children.borrow()[index].clone()
    }

    fn append_child(&mut self, child: &Node) -> Result<(), Error> {
        self.parent_element_ref()?
            .children
            .borrow_mut()
            .push(child.clone());
        self.position += 1;
        Ok(())
    }
}

pub struct ServerBuilder {
    // TODO: Distinguish between parent/child stack nodes and "context" stack nodes,
    // for better stack traces
    stack: Vec<Cursor>,
}

impl ServerBuilder {
    fn cursor(&self) -> &Cursor {
        self.stack.last().unwrap()
    }

    fn cursor_mut(&mut self) -> &mut Cursor {
        self.stack.last_mut().unwrap()
    }

    fn enter_element(&mut self, tag_name: &'static str) -> Result<Node, Error> {
        let element = Rc::new(ServerNode::Element(Element {
            tag_name,
            children: RefCell::new(vec![]),
        }));
        self.cursor_mut().append_child(&element)?;
        self.stack.push(Cursor {
            parent: element.clone(),
            position: 0,
        });
        Ok(element)
    }

    fn exit_element(&mut self) -> Result<Node, Error> {
        match self.stack.pop() {
            Some(cursor) => match cursor.parent.as_ref() {
                &ServerNode::Element(_) => Ok(cursor.parent),
                _ => Err(Error::ExitElement),
            },
            None => Err(Error::ExitElement),
        }
    }
}

impl<'doc> DomVM<'doc, ServerHypp> for ServerBuilder {
    fn const_exec_element(&mut self, program: &[ConstOpCode]) -> Result<Node, Error> {
        let mut iterator = program.iter();

        if let Some(mut opcode) = iterator.next() {
            loop {
                let node = match opcode {
                    ConstOpCode::EnterElement(tag_name) => self.enter_element(tag_name)?,
                    ConstOpCode::Attribute(_name, _value) => {
                        return Err(Error::SetAttribute);
                    }
                    ConstOpCode::Text(text) => self.text(text)?,
                    ConstOpCode::ExitElement => self.exit_element()?,
                    ConstOpCode::RemoveElement(tag_name) => self.remove_element(tag_name)?,
                };

                if let Some(next) = iterator.next() {
                    opcode = next;
                } else {
                    return Ok(node);
                }
            }
        } else {
            return Err(Error::NoProgram);
        }
    }

    fn const_exec_text(&mut self, _program: &[ConstOpCode]) -> Result<Node, Error> {
        panic!()
    }

    fn text(&mut self, text: &str) -> Result<Node, Error> {
        let text_node = Rc::new(ServerNode::Text(RefCell::new(text.to_string())));
        self.cursor_mut().append_child(&text_node)?;

        Ok(text_node)
    }

    fn remove_element(&mut self, _tag_name: &'static str) -> Result<Node, Error> {
        let parent = self.cursor().parent_element_ref()?;
        let child = parent.children.borrow_mut().remove(0);

        match child.as_ref() {
            ServerNode::Element(_) => Ok(child),
            _ => Err(Error::RemoveElement),
        }
    }

    fn advance(&mut self, commands: &[CursorCmd]) {}

    fn skip_const_program(&mut self, _program: &[ConstOpCode]) -> Result<(), Error> {
        panic!()
    }

    fn push_navigation(&mut self, path: &[u16], child_offset: u16) {
        let mut cursor = self.stack.last().unwrap().clone();

        for child_index in path {
            cursor.parent = cursor.nth_child(*child_index as usize);
            cursor.position = 0;
        }

        cursor.position += child_offset as usize;
        self.stack.push(cursor);
    }

    fn pop_navigation(&mut self) {
        self.stack.pop();
    }

    fn push_element_context(&mut self, element: Node) {
        self.stack.push(Cursor {
            parent: element,
            position: 0,
        });
    }

    fn pop_element_context(&mut self) {
        self.stack.pop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node_text(node: &Node) -> String {
        match node.as_ref() {
            ServerNode::Text(string) => string.borrow().clone(),
            _ => {
                panic!("Was not a text, but {}", node.to_string())
            }
        }
    }

    #[test]
    fn build_and_navigate() {
        let hypp = ServerHypp::new();

        {
            let mut vm = hypp.builder_at_body();

            vm.const_exec_element(&[
                ConstOpCode::EnterElement("p"),
                ConstOpCode::Text("foo"),
                ConstOpCode::Text("bar"),
                ConstOpCode::Text("baz"),
                ConstOpCode::ExitElement,
            ])
            .unwrap();

            assert!(vm.cursor().next_node().is_none());
        }

        assert!(hypp.builder_at_body().cursor().next_node().is_some());

        let mut vm = hypp.builder_at_body();

        // Navigate into <p>, "bar":
        vm.push_navigation(&[0], 1);
        assert_eq!(node_text(&vm.cursor().next_node().unwrap()), "bar");

        // Advance to the next sibling
        vm.push_navigation(&[], 1);
        assert_eq!(node_text(&vm.cursor().next_node().unwrap()), "baz");
    }
}
