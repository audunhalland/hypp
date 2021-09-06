use super::Awe;

///
/// Problem with initializing DomSiblingIndex from existing DOM (SSR mode):
/// upon initial "reconciliation", there may be sibling _fragments_ which will result in ambiguity:
///
/// {
///   slot0:
///   <>
///     <div/>
///     <p/>
///   </>
///   slot1:
///   <>
///     <span/>
///     <a/>
///   </>
/// }
///
/// There's basically no way to know(???) which slot "owns" different nodes, when the number
/// of actual DOM nodes per slot is unknown. In fact it is also problematic when seeing
/// optional nodes, where the count is either 0 or 1:
///
/// {
///    slot0:
///    <div/> (optional)
///    slot1:
///    <div/> (optional)
/// }
///
/// It *should* be able to be resolved automatically. The algorithm should be to inspect
/// the current DOM and perform a comparison node-by-node to determine which actual node
/// represents the "wanted" node..? But how to do that when the goal is to have no shadow DOM?
///
/// Perhaps generate slot-index attributes:
///
/// {
///   slot0:
///   <div slot="0"/> (optional)
///   slot1:
///   <div slot="1"/> (optional)
/// }
///
///
/// The idea is to have a simplified DOM projection representing only
/// the points where the tree may change. Constant parts of the DOM
/// does not need to be represented in the application, it's implicitly
/// stored in the DOM.
///
/// Example:
///
///
///
pub struct StaticSiblingIndex<Y: Awe, const SIZE: usize> {
    slots: [DomSlot<Y>; SIZE],
}

struct DomSlot<Y: Awe> {
    next_sibling: Y::Node,
}
