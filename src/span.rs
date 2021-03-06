use crate::{ConstOpCode, Cursor, Hypp, Span, SpanOp, TemplNS};

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

impl<NS: TemplNS> AsSpan for ConstOpCode<NS> {
    type Target = Self;

    fn as_span<'a>(&'a self) -> SpanAdapter<'a, Self> {
        SpanAdapter(self)
    }
}

impl<'c, NS: TemplNS> SpanAdapter<'c, ConstOpCode<NS>> {
    fn is_node(&self) -> bool {
        match &self.0 {
            ConstOpCode::Enter(_) | ConstOpCode::Exit | ConstOpCode::Text(_) => true,
            _ => false,
        }
    }
}

impl<'a, H: Hypp, NS: TemplNS> Span<H> for SpanAdapter<'a, ConstOpCode<NS>> {
    fn is_anchored(&self) -> bool {
        self.is_node()
    }

    // #[tracing::instrument(skip(cursor))]
    fn pass_over(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        if self.is_node() {
            cursor.move_to_following_sibling().unwrap();
            true
        } else {
            false
        }
    }

    // #[tracing::instrument(skip(cursor))]
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

/// A span which is either erased or "something"
impl<H: Hypp, T> Span<H> for Option<T>
where
    T: Span<H>,
{
    fn is_anchored(&self) -> bool {
        match self {
            Some(span) => span.is_anchored(),
            None => false,
        }
    }

    #[tracing::instrument(skip(self, cursor))]
    fn pass_over(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        match self {
            Some(span) => span.pass_over(cursor),
            None => false,
        }
    }

    #[tracing::instrument(skip(self, cursor))]
    fn erase(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        match self {
            Some(span) => {
                let result = span.erase(cursor);
                *self = None;
                result
            }
            None => false,
        }
    }
}
