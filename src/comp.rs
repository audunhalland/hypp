use crate::handle::*;
use crate::patch::{PatchBindCtx, PatchCtx};
use crate::shim::ShimTrampoline;
use crate::*;

pub struct UniqueInner<Env, Span> {
    pub env: Env,
    pub root_span: Span,
}

pub struct SharedInner<H: crate::Hypp, C: ShimTrampoline + 'static, Env, Span> {
    pub env: Env,
    pub root_span: Span,
    pub anchor: H::Anchor,
    pub closure_env: Option<shim::ClosureEnv<H, C>>,
}

impl<Env, Span> UniqueInner<Env, Span> {
    pub fn mount<H: Hypp, NS: TemplNS, C, Patch, Wrap>(
        env: Env,
        cursor: &mut H::Cursor<NS>,
        patch: Patch,
        wrap: Wrap,
    ) -> Result<Unique<C>, crate::Error>
    where
        Patch:
            Fn(Duplex<Span>, &Env, Deviation<'_>, &mut PatchCtx<H, NS>) -> Result<(), crate::Error>,
        Wrap: Fn(Self) -> C,
    {
        let mut root_span = None;
        patch(
            Duplex::Out(&mut root_span),
            &env,
            Deviation::Full,
            &mut PatchCtx { cur: cursor },
        )?;
        Ok(Unique::new(wrap(Self {
            env,
            root_span: root_span.unwrap(),
        })))
    }
}

impl<H: crate::Hypp + 'static, C: ShimTrampoline + 'static, Env, Span>
    SharedInner<H, C, Env, Span>
{
    pub fn mount<NS: TemplNS, Patch, Wrap>(
        env: Env,
        cursor: &mut H::Cursor<NS>,
        patch: Patch,
        wrap: Wrap,
    ) -> Result<H::Shared<C>, crate::Error>
    where
        Patch: Fn(
            Duplex<Span>,
            &Env,
            Deviation<'_>,
            &mut PatchBindCtx<H, NS, C>,
        ) -> Result<(), crate::Error>,
        Wrap: Fn(Self) -> C,
    {
        let anchor = cursor.anchor();
        let mut closure_env = crate::shim::ClosureEnv::deferred();
        let mut root_span = None;
        patch(
            Duplex::Out(&mut root_span),
            &env,
            Deviation::Full,
            &mut PatchBindCtx {
                cur: cursor,
                closure_env: closure_env.clone(),
            },
        )?;
        let handle = H::make_shared(wrap(Self {
            env,
            root_span: root_span.unwrap(),
            anchor,
            closure_env: Some(closure_env.clone()),
        }));
        closure_env.finalize(handle.downgrade());
        Ok(handle)
    }
}

impl<H: Hypp + 'static, Env, S: Span<H>> Span<H> for UniqueInner<Env, S> {
    fn is_anchored(&self) -> bool {
        self.root_span.is_anchored()
    }

    fn pass(&mut self, cursor: &mut dyn Cursor<H>, op: SpanOp) -> bool {
        self.root_span.pass(cursor, op)
    }
}

impl<H: crate::Hypp, C: ShimTrampoline + 'static, Env, S: Span<H>> Span<H>
    for SharedInner<H, C, Env, S>
{
    fn is_anchored(&self) -> bool {
        self.root_span.is_anchored()
    }

    fn pass(&mut self, cursor: &mut dyn Cursor<H>, op: SpanOp) -> bool {
        self.root_span.pass(cursor, op)
    }

    fn pass_over(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        // Self updating! This component needs to store the updated anchor.
        self.anchor = cursor.anchor();
        self.pass(cursor, SpanOp::PassOver)
    }

    fn erase(&mut self, cursor: &mut dyn Cursor<H>) -> bool {
        self.closure_env = None;
        self.pass(cursor, SpanOp::Erase)
    }
}
