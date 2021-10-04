use crate::error::Error;

use super::server_dom::{ArcNode, AttributeValue, Node, NodeKind};
use crate::span::{AsSpan, SpanAdapter};
use crate::{
    AsNode, CallbackSlot, Component, ConstOpCode, Cursor, Hypp, NSCursor, Name, Span, TemplNS,
};

use parking_lot::Mutex;
use std::sync::Arc;

impl AsNode<ServerHypp> for ArcNode {
    #[inline]
    fn as_node(&self) -> &ArcNode {
        self
    }
}

pub struct ServerHypp {
    body: ArcNode,
    mounts: Vec<Box<dyn std::any::Any>>,
}

impl ServerHypp {
    pub fn new() -> Self {
        Self {
            body: Node::create_element("body"),
            mounts: vec![],
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
    type Node = ArcNode;
    type Element = ArcNode;
    type Text = ArcNode;

    type Anchor = ServerBuilder;
    type Cursor<NS: TemplNS> = ServerBuilder;

    type Shared<T>
    where
        T: ?Sized + 'static,
    = Arc<T>;

    type SharedMut<T>
    where
        T: 'static,
    = Arc<Mutex<T>>;

    /// Server has no real callback slots
    type CallbackSlot<Args: 'static> = ();

    fn make_shared<T: 'static>(value: T) -> Self::Shared<T> {
        Arc::new(value)
    }

    fn make_box_shared<T: ?Sized + 'static>(value: Box<T>) -> Self::Shared<T> {
        value.into()
    }

    fn make_shared_mut<T: 'static>(value: T) -> Self::SharedMut<T> {
        Arc::new(Mutex::new(value))
    }

    fn mount<'p, C>(&mut self) -> Result<(), Error>
    where
        C: Component<'p, ServerHypp>,
        C::Props: Default,
    {
        let mut builder = self.builder_at_body();
        let handle = C::mount(Default::default(), &mut builder)?;

        self.mounts.push(Box::new(handle));

        Ok(())
    }

    fn set_text(node: &Self::Text, text: &str) {
        node.set_text(text);
    }

    fn traversal_direction() -> crate::TraversalDirection {
        crate::TraversalDirection::LastToFirst
    }
}

impl AsSpan for ArcNode {
    type Target = Self;

    fn as_span<'a>(&'a self) -> SpanAdapter<'a, Self> {
        SpanAdapter(self)
    }
}

impl<'a> Span<ServerHypp> for SpanAdapter<'a, ArcNode> {
    fn is_anchored(&self) -> bool {
        true
    }

    fn pass_over(&mut self, cursor: &mut dyn Cursor<ServerHypp>) -> bool {
        cursor.move_to_following_sibling_of(&self.0);
        true
    }

    fn erase(&mut self, cursor: &mut dyn Cursor<ServerHypp>) -> bool {
        cursor.remove_node().unwrap();
        true
    }
}

impl<Args> CallbackSlot<ServerHypp, Args> for () {
    fn bind(&mut self, _function: Arc<dyn Fn()>) {}
    fn release(&mut self) {}
}

#[derive(Clone)]
pub struct ServerBuilder {
    element: ArcNode,
    next_child: Option<ArcNode>,
    loaded_attribute_name: Option<&'static str>,
}

impl ServerBuilder {
    fn current_child(&self) -> Option<ArcNode> {
        match &self.next_child {
            Some(next_child) => next_child.prev_sibling(),
            None => self.element.last_child(),
        }
    }

    fn enter_element(&mut self, tag_name: &'static str) -> ArcNode {
        let element = Node::create_element(tag_name);
        self.element
            .insert_before(element.clone(), self.next_child.clone());

        self.element = element.clone();
        // Start at "last child" (the new element has no children)
        self.next_child = None;

        element
    }

    fn text(&mut self, text: &str) -> ArcNode {
        let node = Node::create_text(text.to_string());
        self.element
            .insert_before(node.clone(), self.next_child.clone());

        // The next child is the newly created node
        self.next_child = Some(node.clone());

        node
    }

    fn exit_element(&mut self) -> ArcNode {
        let parent_element = self
            .element
            .parent()
            .expect("Failed to exit: no parent element");

        // The next child is the element just exited
        self.next_child = Some(self.element.clone());

        std::mem::replace(&mut self.element, parent_element)
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<ArcNode, Error> {
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
    fn anchor(&self) -> ServerBuilder {
        self.clone()
    }

    fn text(&mut self, text: &str) -> Result<ArcNode, Error> {
        Ok(self.text(text))
    }

    fn remove_node(&mut self) -> Result<(), Error> {
        let child = self.current_child().expect("Expected a node to remove");
        self.element.remove_child(child);

        Ok(())
    }

    fn remove_element(&mut self, tag_name: &'static str) -> Result<ArcNode, Error> {
        let child = self.current_child().expect("Expected an element to remove");
        child.assert_is_element_with_tag_name(tag_name);

        self.element.remove_child(child.clone());

        Ok(child)
    }

    fn remove_text(&mut self) -> Result<ArcNode, Error> {
        let child = self.current_child().expect("Expected an element to remove");
        self.element.remove_child(child.clone());

        Ok(child)
    }

    fn move_to_children_of(&mut self, element: &ArcNode) {
        self.element = element.clone();
        // Start at the end of the child list:
        self.next_child = None;
    }

    fn move_to_following_sibling(&mut self) -> Result<(), Error> {
        self.next_child = Some(self.current_child().expect("Moved past the first child"));
        Ok(())
    }

    fn move_to_following_sibling_of(&mut self, node: &ArcNode) {
        self.element = node.parent().unwrap();
        // The next child is the node itself (so the cursor points before that node)
        self.next_child = Some(node.clone());
    }
}

impl<NS: TemplNS> NSCursor<ServerHypp, NS> for ServerBuilder {
    fn const_exec_element(
        &mut self,
        program: &'static [ConstOpCode<NS>],
    ) -> Result<ArcNode, Error> {
        let mut result = Err(Error::NoProgram);

        for opcode in program {
            match opcode {
                ConstOpCode::Enter(etype) => {
                    result = Ok(self.enter_element(etype.name()));
                }
                ConstOpCode::Attr(atype) => {
                    self.loaded_attribute_name = Some(atype.name());
                }
                ConstOpCode::AttrText(value) => {
                    let attribute_name = self
                        .loaded_attribute_name
                        .expect("Should call AttributeName before setting attribute value");

                    match &self.element.kind {
                        NodeKind::Element { attributes, .. } => {
                            attributes
                                .lock()
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
                ConstOpCode::Exit => {
                    result = Ok(self.exit_element());
                }
                ConstOpCode::Erase(etype) => {
                    result = Ok(self.remove_element(etype.name())?);
                }
            };
        }

        result
    }

    fn const_exec_text(&mut self, program: &'static [ConstOpCode<NS>]) -> Result<ArcNode, Error> {
        <ServerBuilder as NSCursor<ServerHypp, NS>>::const_exec_element(self, program)
    }

    fn attribute_slot<Args>(&mut self) -> Result<Arc<Mutex<()>>, Error> {
        // Callbacks do nothing on the server
        Ok(Arc::new(Mutex::new(())))
    }

    fn skip_const_program(&mut self, _program: &[ConstOpCode<NS>]) {
        /*
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
        */
        unimplemented!("Not needed");
    }
}

impl crate::Anchor<ServerHypp> for ServerBuilder {
    fn create_cursor<NS>(&self) -> ServerBuilder {
        self.clone()
    }
}

#[cfg(test)]
mod tests {}
