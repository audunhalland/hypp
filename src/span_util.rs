use crate::{Cursor, Hypp, Span, SpanOp};

pub fn span_pass<H: Hypp>(spans: &mut [&mut dyn Span<H>], cursor: &mut dyn Cursor<H>, op: SpanOp) {
    for span in spans {
        span.unmount(cursor);
    }
}
