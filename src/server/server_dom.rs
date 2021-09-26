use parking_lot::Mutex;
use std::collections::BTreeMap;
use std::sync::{Arc, Weak};

pub type ArcNode = Arc<Node>;

pub struct Node {
    pub kind: NodeKind,

    links: Mutex<Links>,
}

pub enum NodeKind {
    Text(Mutex<String>),
    Element {
        tag_name: &'static str,
        attributes: Mutex<Attributes>,
    },
    Fragment,
}

#[derive(Default)]
struct Links {
    pub parent: Option<Weak<Node>>,

    pub next_sibling: Option<ArcNode>,
    pub prev_sibling: Option<Weak<Node>>,

    pub first_child: Option<ArcNode>,
    pub last_child: Option<Weak<Node>>,
}

#[derive(Default)]
pub struct Attributes {
    pub map: BTreeMap<&'static str, AttributeValue>,
}

pub enum AttributeValue {
    Static(&'static str),
    Dynamic(String),
}

impl Node {
    pub fn is(&self, other: &Node) -> bool {
        self as *const _ == other as *const _
    }

    pub fn assert_is_element_with_tag_name(&self, tag_name: &'static str) {
        match &self.kind {
            NodeKind::Element {
                tag_name: self_tag_name,
                ..
            } => {
                assert_eq!(*self_tag_name, tag_name);
            }
            NodeKind::Fragment => {
                panic!("Expected an element, but was fragment {}", self.to_string());
            }
            NodeKind::Text(_) => {
                panic!("Expected an element, but was text {}", self.to_string());
            }
        }
    }

    pub fn assert_is_text(&self, text: &str) {
        match &self.kind {
            NodeKind::Element { .. } => {
                panic!("Expected text \"{}\", but was {}", text, self.to_string());
            }
            NodeKind::Fragment => {
                panic!("Expected an element, but was fragment {}", self.to_string());
            }
            NodeKind::Text(self_text) => {
                assert_eq!(self_text.lock().as_str(), text);
            }
        }
    }

    pub fn create_element(tag_name: &'static str) -> ArcNode {
        Arc::new(Node {
            kind: NodeKind::Element {
                tag_name,
                attributes: Mutex::new(Attributes::default()),
            },
            links: Mutex::new(Links::default()),
        })
    }

    pub fn create_text(text: String) -> ArcNode {
        Arc::new(Node {
            kind: NodeKind::Text(Mutex::new(text)),
            links: Mutex::new(Links::default()),
        })
    }

    pub fn set_text(&self, text: &str) {
        match &self.kind {
            NodeKind::Text(self_text) => {
                *self_text.lock() = text.to_string();
            }
            _ => {
                panic!("tried to set text no something which is not a text node");
            }
        }
    }

    pub fn parent(&self) -> Option<ArcNode> {
        self.links
            .lock()
            .parent
            .as_ref()
            .and_then(|parent| parent.upgrade())
    }

    pub fn first_child(&self) -> Option<ArcNode> {
        self.links.lock().first_child.clone()
    }

    pub fn last_child(&self) -> Option<ArcNode> {
        self.links
            .lock()
            .last_child
            .as_ref()
            .and_then(|weak| weak.upgrade())
    }

    pub fn next_sibling(&self) -> Option<ArcNode> {
        self.links.lock().next_sibling.clone()
    }

    pub fn prev_sibling(&self) -> Option<ArcNode> {
        self.links
            .lock()
            .prev_sibling
            .as_ref()
            .and_then(|weak| weak.upgrade())
    }

    pub fn append_child(self: &Arc<Self>, child: ArcNode) -> ArcNode {
        self.insert_before(child, None)
    }

    pub fn insert_before(self: &Arc<Self>, child: ArcNode, reference: Option<ArcNode>) -> ArcNode {
        child.unlink();

        {
            let mut parent_links = self.links.lock();
            let mut child_links = child.links.lock();

            child_links.parent = Some(Arc::downgrade(self));

            if let Some(reference) = reference {
                assert!(reference.parent().unwrap().is(self));

                // Set up next sibling owned pointer
                child_links.next_sibling = Some(reference.clone());

                let mut next_links = reference.links.lock();

                match next_links.prev_sibling.take() {
                    // first child
                    None => {
                        next_links.prev_sibling = Some(Arc::downgrade(&child));
                        parent_links.first_child = Some(child.clone());
                    }
                    // not first
                    Some(old_prev) => {
                        let old_prev = old_prev.upgrade().unwrap();
                        let mut old_prev_links = old_prev.links.lock();

                        old_prev_links.next_sibling = Some(child.clone());
                        next_links.prev_sibling = Some(Arc::downgrade(&child));
                        child_links.prev_sibling = Some(Arc::downgrade(&old_prev));
                    }
                }
            } else {
                // Append

                if let Some(last_child) = parent_links
                    .last_child
                    .take()
                    .and_then(|child| child.upgrade())
                {
                    assert!(last_child.links.lock().next_sibling.is_none());

                    child_links.prev_sibling = Some(Arc::downgrade(&last_child));
                    last_child.links.lock().next_sibling = Some(child.clone());
                } else {
                    // Add the first child
                    parent_links.first_child = Some(child.clone());
                    parent_links.last_child = Some(Arc::downgrade(&child));
                }

                parent_links.last_child = Some(Arc::downgrade(&child));
            }
        }

        child
    }

    pub fn remove_child(&self, child: ArcNode) -> ArcNode {
        child.unlink();
        child
    }

    /// Unlink from current child list
    fn unlink(&self) {
        let mut self_links = self.links.lock();
        if let Some(parent) = self_links.parent.take().and_then(|parent| parent.upgrade()) {
            let mut parent_links = parent.links.lock();

            match (
                self_links.prev_sibling.take(),
                self_links.next_sibling.take(),
            ) {
                // only child:
                (None, None) => {
                    parent_links.last_child = None;
                    parent_links.first_child = None;
                }
                // first child:
                (None, Some(next)) => {
                    next.links.lock().prev_sibling = None;
                    parent_links.first_child = Some(next);
                }
                // last child:
                (Some(prev), None) => {
                    let prev = prev.upgrade().unwrap();

                    prev.links.lock().next_sibling = None;
                    parent_links.last_child = Some(Arc::downgrade(&prev));
                }
                // internal child:
                (Some(prev), Some(next)) => {
                    let prev = prev.upgrade().unwrap();

                    next.links.lock().prev_sibling = Some(Arc::downgrade(&prev));
                    prev.links.lock().next_sibling = Some(next);
                }
            }

            self_links.parent = None;
        } else {
            assert!(self_links.prev_sibling.is_none());
            assert!(self_links.next_sibling.is_none());
        }
    }
}

impl Drop for Node {
    // Unlink all the nodes in the direct child list
    fn drop(&mut self) {
        let mut next_sibling = {
            let self_links = self.links.lock();
            self_links.first_child.clone()
        };

        while let Some(sibling) = next_sibling.take() {
            next_sibling = {
                let mut links = sibling.links.lock();

                links.parent = None;
                links.prev_sibling = None;
                links.next_sibling.take()
            }
        }
    }
}

impl ToString for Node {
    fn to_string(&self) -> String {
        fn recurse(node: &Node, buf: &mut String) {
            match &node.kind {
                NodeKind::Element {
                    tag_name,
                    attributes,
                } => {
                    buf.push('<');
                    buf.push_str(tag_name);

                    for (name, value) in attributes.lock().map.iter() {
                        buf.push(' ');
                        buf.push_str(name);
                        buf.push_str("=\"");
                        match value {
                            AttributeValue::Static(str) => {
                                buf.push_str(str);
                            }
                            AttributeValue::Dynamic(string) => {
                                buf.push_str(&string);
                            }
                        }
                        buf.push('"');
                    }

                    if let Some(first_child) = node.first_child() {
                        buf.push('>');

                        let mut child = first_child;

                        loop {
                            recurse(&child, buf);

                            if let Some(next_sibling) = child.next_sibling() {
                                child = next_sibling;
                            } else {
                                break;
                            }
                        }

                        buf.push_str("</");
                        buf.push_str(tag_name);
                        buf.push('>');
                    } else {
                        buf.push_str("/>");
                    }
                }
                NodeKind::Text(text) => {
                    buf.push_str(text.lock().as_str());
                }
                NodeKind::Fragment => {
                    panic!("implement");
                }
            }
        }

        let mut buf = String::new();
        recurse(self, &mut buf);
        buf
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tree_stuff() {
        let parent = Node::create_element("div");
        let child0 = Node::create_text("Foo".into());
        let child1 = Node::create_element("a");
        let child2 = Node::create_text("Baz".into());

        child1.append_child(Node::create_text("Bar".into()));

        parent.append_child(child0.clone());
        parent.append_child(child1.clone());
        parent.append_child(child2.clone());

        let test_original = || {
            assert_eq!(parent.to_string(), "<div>Foo<a>Bar</a>Baz</div>");

            assert!(child0.next_sibling().unwrap().is(&child1));
            assert!(child1.next_sibling().unwrap().is(&child2));
            assert!(child2.next_sibling().is_none());

            assert!(child2.prev_sibling().unwrap().is(&child1));
            assert!(child1.prev_sibling().unwrap().is(&child0));
            assert!(child0.prev_sibling().is_none());

            assert!(child0.parent().unwrap().is(&parent));
            assert!(child1.parent().unwrap().is(&parent));
            assert!(child2.parent().unwrap().is(&parent));
        };

        test_original();

        child0.unlink();
        assert_eq!(parent.to_string(), "<div><a>Bar</a>Baz</div>");
        parent.insert_before(child0.clone(), Some(child1.clone()));
        test_original();

        child1.unlink();
        assert_eq!(parent.to_string(), "<div>FooBaz</div>");
        parent.insert_before(child1.clone(), Some(child2.clone()));
        test_original();

        child2.unlink();
        assert_eq!(parent.to_string(), "<div>Foo<a>Bar</a></div>");
        parent.insert_before(child2.clone(), None);
        test_original();

        // Dropping the parent should "unnest" its child list
        drop(parent);

        assert!(child0.next_sibling().is_none());
        assert!(child1.next_sibling().is_none());

        assert!(child0.parent().is_none());
        assert!(child1.parent().is_none());
        assert!(child2.parent().is_none());

        // child1 should still render as before
        assert_eq!(child1.to_string(), "<a>Bar</a>");
    }

    #[test]
    fn build_tree() {
        let html = Node::create_element("html");
        let div = Node::create_element("div");

        div.append_child(Node::create_text("Foo".into()));
        div.append_child(Node::create_element("a"));

        html.append_child(div);

        assert_eq!(html.to_string(), "<html><div>Foo<a/></div></html>");
    }
}
