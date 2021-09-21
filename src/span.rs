use crate::{ConstOpCode, Cursor, Hypp, Span, SpanOp};

pub struct SingleTextSpan;

pub static TEXT_SPAN: SingleTextSpan = SingleTextSpan;

pub fn pass<H: Hypp>(spans: &[&dyn Span<H>], cursor: &mut dyn Cursor<H>, op: SpanOp) -> bool {
    println!("enter {:?} pass for {} spans", op, spans.len());
    let result = match op {
        SpanOp::PassOver => {
            let mut result = false;
            for span in spans {
                if span.pass_over(cursor) {
                    result = true;
                }
            }
            result
        }
        SpanOp::Erase => {
            let mut result = false;
            for span in spans {
                if span.erase(cursor) {
                    result = true;
                }
            }
            result
        }
    };
    println!("exit  {:?} pass", op);
    result
}

impl ConstOpCode {
    fn is_node(&self) -> bool {
        match self {
            ConstOpCode::EnterElement(_) | ConstOpCode::ExitElement | ConstOpCode::Text(_) => true,
            _ => false,
        }
    }
}

impl<H: Hypp> Span<H> for ConstOpCode {
    fn is_anchored(&self) -> bool {
        self.is_node()
    }

    fn pass_over(&self, cursor: &mut dyn Cursor<H>) -> bool {
        if self.is_node() {
            cursor.move_to_following_sibling().unwrap();
            true
        } else {
            false
        }
    }

    fn erase(&self, cursor: &mut dyn Cursor<H>) -> bool {
        println!("Erase {:?}", self);
        if self.is_node() {
            cursor.remove_node().unwrap();
            true
        } else {
            false
        }
    }
}

impl<H: Hypp> Span<H> for SingleTextSpan {
    fn is_anchored(&self) -> bool {
        true
    }

    fn pass_over(&self, cursor: &mut dyn Cursor<H>) -> bool {
        println!("SingleTextSpan::pass_over");
        cursor.move_to_following_sibling().unwrap();
        true
    }

    fn erase(&self, cursor: &mut dyn Cursor<H>) -> bool {
        println!("SingleTextSpan::erase");
        cursor.remove_node().unwrap();
        true
    }
}
