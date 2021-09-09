use std::cell::RefCell;
use std::rc::Rc;

use crate::error::Error;

use super::{AsNode, DomVM, Hypp};

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
            stack: vec![self.body.clone()],
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

pub struct ServerBuilder {
    // TODO: Distinguish between parent/child stack nodes and "context" stack nodes,
    // for better stack traces
    stack: Vec<Node>,
}

impl ServerBuilder {
    fn parent_element(&self) -> Result<&Element, Error> {
        let parent = self.stack.last().unwrap();

        match parent.as_ref() {
            ServerNode::Element(element) => Ok(element),
            _ => Err(Error::DomCorruption),
        }
    }

    fn append_child(&mut self, child: &Node) -> Result<(), Error> {
        self.parent_element()?
            .children
            .borrow_mut()
            .push(child.clone());
        Ok(())
    }
}

impl<'doc> DomVM<'doc, ServerHypp> for ServerBuilder {
    fn enter_element(&mut self, tag_name: &'static str) -> Result<Node, Error> {
        let element = Rc::new(ServerNode::Element(Element {
            tag_name,
            children: RefCell::new(vec![]),
        }));
        self.append_child(&element)?;
        self.stack.push(element.clone());
        Ok(element)
    }

    fn attribute(&mut self, _name: &'static str, _value: &'static str) -> Result<(), Error> {
        Err(Error::SetAttribute)
    }

    fn text(&mut self, text: &str) -> Result<Node, Error> {
        let text_node = Rc::new(ServerNode::Text(RefCell::new(text.to_string())));
        self.append_child(&text_node)?;

        Ok(text_node)
    }

    fn exit_element(&mut self) -> Result<(), Error> {
        match self.stack.pop() {
            Some(node) => match node.as_ref() {
                &ServerNode::Element(_) => Ok(()),
                _ => Err(Error::ExitElement),
            },
            None => Err(Error::ExitElement),
        }
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<(), Error> {
        println!("remove element: {}", tag_name);
        let parent = self.parent_element()?;
        let child = parent.children.borrow_mut().remove(0);

        match child.as_ref() {
            ServerNode::Element(_) => Ok(()),
            _ => Err(Error::RemoveElement),
        }
    }

    fn push_element_context(&mut self, element: Node) {
        self.stack.push(element);
    }

    fn pop_element_context(&mut self) {
        self.stack.pop();
    }
}
