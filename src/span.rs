use crate::{ConstOpCode, Cursor, Hypp, Span, SpanOp};

pub trait AsSpan: Sized {
    type Target;

    fn as_span<'a>(&'a self) -> SpanAdapter<'a, Self::Target>;
}

pub struct SingleTextSpan;

#[tracing::instrument(skip(spans, cursor), fields(len = spans.len()))]
pub fn pass<H: Hypp>(
    spans: &mut [&mut dyn Span<H>],
    cursor: &mut dyn Cursor<H>,
    op: SpanOp,
) -> bool {
    match op {
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
    }
}

#[derive(Debug)]
pub struct SpanAdapter<'a, T>(pub &'a T);

impl AsSpan for ConstOpCode {
    type Target = Self;

    fn as_span<'a>(&'a self) -> SpanAdapter<'a, Self> {
        SpanAdapter(self)
    }
}

impl<'c> SpanAdapter<'c, ConstOpCode> {
    fn is_node(&self) -> bool {
        match &self.0 {
            ConstOpCode::EnterElement(_) | ConstOpCode::ExitElement | ConstOpCode::Text(_) => true,
            _ => false,
        }
    }
}

impl<'a, H: Hypp> Span<H> for SpanAdapter<'a, ConstOpCode> {
    fn is_anchored(&self) -> bool {
        self.is_node()
    }

    #[tracing::instrument(skip(cursor))]
    fn pass_over(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        if self.is_node() {
            cursor.move_to_following_sibling().unwrap();
            true
        } else {
            false
        }
    }

    #[tracing::instrument(skip(cursor))]
    fn erase(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
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

    #[tracing::instrument(skip(self, cursor))]
    fn pass_over(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        cursor.move_to_following_sibling().unwrap();
        true
    }

    #[tracing::instrument(skip(self, cursor))]
    fn erase(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        cursor.remove_node().unwrap();
        true
    }
}
