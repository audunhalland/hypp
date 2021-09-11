use crate::error::Error;

use super::server_dom::{Node, NodeKind, RcNode};
use crate::{AsNode, ConstOpCode, CursorCmd, DomVM, Hypp};

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
            parent: self.body.clone(),
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

    fn remove_child(_parent: &Self::Element, _child: &Self::Node) -> Result<(), Error> {
        panic!();
    }

    fn set_text(_node: &Self::Text, _text: &str) {
        panic!();
    }
}

///
/// Cursor implmentation
/// A cursor points *between* nodes or *at* nodes?
///
/// <body> <-- start
///   <div>  <-- enter element (attributes apply to the previously entered element)
///     *
///     "foo"    <-- text (where does the cursor go now?)
///     *
///     <span>   <-- enter element
///     </span>  <-- exit element
///   </div> <-- exit element
/// </body>
///

pub struct ServerBuilder {
    parent: RcNode,
    next_child: Option<RcNode>,
}

impl ServerBuilder {
    fn enter_element(&mut self, tag_name: &'static str) -> RcNode {
        let element = Node::create_element(tag_name);
        self.parent
            .insert_before(element.clone(), self.next_child.clone());

        self.parent = element.clone();
        self.next_child = None;

        element
    }

    fn text(&mut self, text: &str) -> RcNode {
        let node = Node::create_text(text.to_string());
        self.parent
            .insert_before(node.clone(), self.next_child.clone());

        node
    }

    fn exit_element(&mut self) -> RcNode {
        let parent = self.parent.clone();
        self.next_child = parent.next_sibling();

        self.parent = parent.parent().expect("Failed to exit: no parent node");

        parent
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
                        self.parent.remove_child(next_child.clone());
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

    fn const_exec_text(&mut self, _program: &[ConstOpCode]) -> Result<RcNode, Error> {
        panic!()
    }

    fn text(&mut self, text: &str) -> Result<RcNode, Error> {
        Ok(self.text(text))
    }

    fn remove_element(&mut self, _tag_name: &'static str) -> Result<RcNode, Error> {
        panic!()
    }

    fn advance(&mut self, _commands: &[CursorCmd]) {}

    fn skip_const_program(&mut self, _program: &[ConstOpCode]) -> Result<(), Error> {
        panic!()
    }

    fn push_navigation(&mut self, _path: &[u16], _child_offset: u16) {
        panic!();
    }

    fn pop_navigation(&mut self) {
        panic!();
    }

    fn push_element_context(&mut self, _element: RcNode) {
        panic!();
    }

    fn pop_element_context(&mut self) {
        panic!();
    }
}

#[cfg(test)]
mod tests {}
