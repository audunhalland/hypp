use crate::*;

///
/// A dynamic span which is a dynamically repeating list of a given sub span.
/// This implementation does not use keyeing for moving elements around.
///
pub struct SimpleListSpan<H: Hypp, S> {
    spans: Vec<S>,
    phantom: std::marker::PhantomData<H>,
}

impl<H: Hypp + 'static, S> SimpleListSpan<H, S>
where
    S: Span<H>,
{
    fn new() -> Self {
        Self {
            spans: vec![],
            phantom: std::marker::PhantomData,
        }
    }

    fn patch<'a, I, D, F, C>(
        &mut self,
        mut data_iterator: I,
        mut inner_patch_fn: F,
        mut ctx: C,
    ) -> Result<(), crate::Error>
    where
        I: DoubleEndedIterator<Item = D>,
        F: FnMut(InputOrOutput<S>, D) -> Result<(), crate::Error>,
        C: GetCursor<H> + 'a,
    {
        let next_data_item = match H::traversal_direction() {
            TraversalDirection::FirstToLast => I::next,
            TraversalDirection::LastToFirst => I::next_back,
        };
        let mut index = 0;

        while let Some(data_item) = next_data_item(&mut data_iterator) {
            if index < self.spans.len() {
                inner_patch_fn(InputOrOutput::Input(&mut self.spans[index]), data_item)?;
            } else {
                let mut new_inner = None;
                inner_patch_fn(InputOrOutput::Output(&mut new_inner), data_item)?;
                self.spans.push(new_inner.unwrap());
            }
            index += 1;
        }

        let cursor = ctx.get_cursor();

        while index < self.spans.len() {
            let mut popped = self.spans.pop().unwrap();
            popped.erase(cursor);
        }

        Ok(())
    }
}

impl<H, S> Span<H> for SimpleListSpan<H, S>
where
    H: Hypp,
    S: Span<H>,
{
    fn is_anchored(&self) -> bool {
        false
    }

    fn pass(&mut self, cursor: &mut dyn Cursor<H>, op: SpanOp) -> bool {
        if self.spans.is_empty() {
            false
        } else {
            let mut result = false;
            for span in self.spans.iter_mut() {
                if span.pass(cursor, op) {
                    result = true;
                }
            }
            result
        }
    }

    fn pass_over(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        self.pass(cursor, SpanOp::PassOver)
    }

    fn erase(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        let result = self.pass(cursor, SpanOp::Erase);
        self.spans.clear();
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::server::ServerHypp;

    #[derive(Eq, PartialEq, Debug)]
    struct FakeSpan {
        generation: usize,
        value: &'static str,
    }

    impl<H: Hypp> Span<H> for FakeSpan {
        fn is_anchored(&self) -> bool {
            false
        }

        fn pass(&mut self, _cursor: &mut dyn Cursor<H>, _op: SpanOp) -> bool {
            false
        }
    }

    struct FakePatcher {
        gen_counter: usize,
    }

    fn fake_spans(values: &[(usize, &'static str)]) -> Vec<FakeSpan> {
        values
            .iter()
            .map(|(generation, value)| FakeSpan {
                generation: *generation,
                value,
            })
            .collect()
    }

    fn patch_fake(
        list_span: &mut SimpleListSpan<ServerHypp, FakeSpan>,
        cursor: &mut dyn Cursor<ServerHypp>,
        fake_patcher: &mut FakePatcher,
        data: Vec<&'static str>,
    ) {
        let patch_fake_span_inner =
            |inout: InputOrOutput<FakeSpan>, data: &'static str| -> Result<(), crate::Error> {
                match inout {
                    InputOrOutput::Input(span) => {
                        span.value = data;
                    }
                    InputOrOutput::Output(span) => {
                        let generation = fake_patcher.gen_counter;
                        fake_patcher.gen_counter += 1;
                        *span = Some(FakeSpan {
                            generation,
                            value: data,
                        });
                    }
                }

                Ok(())
            };

        let mut patch_ctx = PatchCtx { cur: cursor };
        list_span
            .patch(data.into_iter(), patch_fake_span_inner, &mut patch_ctx)
            .unwrap();
    }

    #[test]
    fn simple_list_span() {
        let hypp = ServerHypp::new();
        let mut builder = hypp.builder_at_body();
        let mut list_span: SimpleListSpan<ServerHypp, FakeSpan> = SimpleListSpan::new();
        let mut fake_patcher = FakePatcher { gen_counter: 0 };

        patch_fake(
            &mut list_span,
            &mut builder,
            &mut fake_patcher,
            vec!["a", "b", "c"],
        );
        assert_eq!(
            &list_span.spans,
            &fake_spans(&[(0, "c"), (1, "b"), (2, "a")])
        );

        patch_fake(
            &mut list_span,
            &mut builder,
            &mut fake_patcher,
            vec!["a", "FOO", "c"],
        );
        assert_eq!(
            &list_span.spans,
            &fake_spans(&[(0, "c"), (1, "FOO"), (2, "a")])
        );

        patch_fake(
            &mut list_span,
            &mut builder,
            &mut fake_patcher,
            vec!["a", "FOO"],
        );
        // "FOO" is not "moved" in the list, the old item "c" is just repatched
        // to prepresent FOO
        assert_eq!(&list_span.spans, &fake_spans(&[(0, "FOO"), (1, "a")]));

        patch_fake(
            &mut list_span,
            &mut builder,
            &mut fake_patcher,
            vec!["a", "FOO", "BAZ"],
        );
        // extending the list works
        assert_eq!(
            &list_span.spans,
            &fake_spans(&[(0, "BAZ"), (1, "FOO"), (3, "a")])
        );
    }
}
