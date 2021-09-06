use std::cell::RefCell;
use std::rc::Rc;

use crate::error::Error;

use super::{AsNode, Awe};

type Node = Rc<ServerNode>;

impl AsNode<ServerAwe> for Node {
    #[inline]
    fn as_node(&self) -> &Node {
        self
    }
}

pub struct ServerAwe {
    body: Node,
}

impl ServerAwe {
    pub fn new() -> Self {
        Self {
            body: Rc::new(ServerNode::Element(Element {
                tag_name: "body",
                children: RefCell::new(vec![]),
            })),
        }
    }
}

impl Awe for ServerAwe {
    type Node = Node;
    type Element = Node;
    type Text = Node;

    fn body(&self) -> &Self::Element {
        &self.body
    }

    fn create_element(&self, tag_name: &'static str) -> Self::Element {
        Rc::new(ServerNode::Element(Element {
            tag_name,
            children: RefCell::new(vec![]),
        }))
    }

    fn create_empty_text(&self) -> Self::Text {
        Rc::new(ServerNode::Text(RefCell::new(String::new())))
    }

    fn insert_child_before(
        parent: &Self::Element,
        child: &Self::Node,
        before: Option<&Self::Node>,
    ) -> Result<(), Error> {
        match parent.as_ref() {
            ServerNode::Element(element) => {
                element.children.borrow_mut().push(child.clone());
                Ok(())
            }
            _ => Err(Error::AddChild),
        }
    }

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

#[derive(Clone)]
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
                    buf.push('>');

                    for child in element.children.borrow().iter() {
                        recurse(child, buf);
                    }

                    buf.push_str("</");
                    buf.push_str(element.tag_name);
                    buf.push('>');
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

#[derive(Clone)]
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
