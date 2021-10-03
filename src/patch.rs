use super::shim::{MakeClosure, ShimTrampoline};
use super::{GetCursor, Hypp, TemplNS};

///
/// Patching context without `bind` functionality
///
pub struct PatchCtx<'a, H: Hypp, NS: TemplNS> {
    pub cur: &'a mut H::Cursor<NS>,
}

impl<'a, H: Hypp, NS: TemplNS> GetCursor<H, NS> for PatchCtx<'a, H, NS> {
    fn get_cursor(&mut self) -> &mut H::Cursor<NS> {
        self.cur
    }
}

///
/// Patching context _with_ `bind` functionality
///
pub struct PatchBindCtx<'a, H: Hypp, NS: TemplNS, T: ShimTrampoline> {
    pub cur: &'a mut H::Cursor<NS>,
    pub bind: &'a mut dyn MakeClosure<H, T>,
}

impl<'a, H: Hypp, NS: TemplNS, T: ShimTrampoline> GetCursor<H, NS> for PatchBindCtx<'a, H, NS, T> {
    fn get_cursor(&mut self) -> &mut H::Cursor<NS> {
        self.cur
    }
}
