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
    pub weak_self: Option<<H::Shared<C> as SharedHandle<C>>::Weak>,
}

impl<Env, Span> UniqueInner<Env, Span> {
    pub fn mount<H: Hypp, NS: TemplNS, C, Patch, Wrap, const U: usize>(
        env: Env,
        cursor: &mut H::Cursor<NS>,
        patch: Patch,
        wrap: Wrap,
    ) -> Result<Unique<C>, crate::Error>
    where
        Patch: Fn(
            Duplex<Span>,
            &Env,
            Invalidated,
            &[bool],
            &mut PatchCtx<H, NS>,
        ) -> Result<(), crate::Error>,
        Wrap: Fn(Self) -> C,
    {
        let updates: [bool; 1usize] = [true; 1usize];
        let mut root_span = None;
        patch(
            Duplex::Out(&mut root_span),
            &env,
            Invalidated(true),
            &updates,
            &mut PatchCtx { cur: cursor },
        )?;
        Ok(Unique::new(wrap(Self {
            env,
            root_span: root_span.unwrap(),
        })))
    }
}

impl<H: crate::Hypp, C: ShimTrampoline + 'static, Env, Span> SharedInner<H, C, Env, Span> {
    pub fn mount<NS: TemplNS, Patch, Wrap, SaveWeak, const U: usize>(
        env: Env,
        cursor: &mut H::Cursor<NS>,
        patch: Patch,
        wrap: Wrap,
        save_weak: SaveWeak,
    ) -> Result<H::Shared<C>, crate::Error>
    where
        Patch: Fn(
            Duplex<Span>,
            &Env,
            Invalidated,
            &[bool],
            &mut PatchBindCtx<H, NS, C>,
        ) -> Result<(), crate::Error>,
        Wrap: Fn(Self) -> C,
        SaveWeak: Fn(&mut C, <H::Shared<C> as SharedHandle<C>>::Weak),
    {
        let updates = [false; U];
        let anchor = cursor.anchor();
        let mut binder: crate::shim::LazySelfBinder<H, C> = crate::shim::LazySelfBinder::new();
        let mut root_span = None;
        patch(
            Duplex::Out(&mut root_span),
            &env,
            Invalidated(true),
            &updates,
            &mut PatchBindCtx {
                cur: cursor,
                bind: &mut binder,
            },
        )?;
        let mut handle = H::make_shared(wrap(Self {
            env,
            root_span: root_span.unwrap(),
            anchor,
            weak_self: None,
        }));
        let weak = handle.downgrade();
        binder.bind_all(weak.clone());
        save_weak(&mut handle.get_mut(), weak);
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
        self.weak_self = None;
        self.pass(cursor, SpanOp::Erase)
    }
}
