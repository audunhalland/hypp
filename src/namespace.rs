use crate::Namespace;

trait Cur<N: Namespace> {
    fn enter_ns(&mut self);
}

struct MyCur;

impl<N: Namespace> Cur<N> for MyCur {
    fn enter_ns(&mut self) {}
}

struct A;
struct B;

impl Namespace for A {}
impl Namespace for B {}

trait Process<N: Namespace> {
    fn process(&self, cur: &mut dyn Cur<N>);

    fn process_any_ns<O: Namespace>(&self, _cur: &mut dyn Cur<O>) {}
}

struct Comp1 {
    comp2: Comp2,
}
struct Comp2;

impl Process<A> for Comp1 {
    fn process(&self, cur: &mut dyn Cur<A>) {
        // now try to process comp2...
        self.comp2.process_any_ns(cur);
    }
}

impl Process<B> for Comp2 {
    fn process(&self, _cur: &mut dyn Cur<B>) {}
}
