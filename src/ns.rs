pub struct Html;

impl crate::TemplNS for Html {
    type EType = &'static str;
}

impl crate::StaticName for &'static str {
    fn static_name(&self) -> &'static str {
        self
    }
}

#[cfg(test)]
mod experiment {
    use crate::TemplNS;
    use std::any::TypeId;

    trait Cur<N: TemplNS> {
        fn add(&mut self, element: N::EType);
    }

    trait Sys: Sized {
        type Cursor<N: TemplNS>: Cur<N>;

        fn enter_ns<N1, N2, F>(cur: &mut Self::Cursor<N1>, func: F)
        where
            N1: TemplNS,
            N2: TemplNS,
            F: Fn(&mut Self::Cursor<N2>);
    }

    struct LORD;

    impl Sys for LORD {
        type Cursor<N: TemplNS> = MyCur;

        fn enter_ns<N1, N2, F>(cur: &mut Self::Cursor<N1>, func: F)
        where
            N1: TemplNS,
            N2: TemplNS,
            F: Fn(&mut Self::Cursor<N2>),
        {
            let old_id = std::mem::replace(&mut cur.current_ns, TypeId::of::<N2>());
            func(cur);
            cur.current_ns = old_id;
        }
    }

    struct MyCur {
        current_ns: std::any::TypeId,
    }

    impl<N: TemplNS> Cur<N> for MyCur {
        fn add(&mut self, element: N::EType) {}
    }

    struct A;
    struct B;

    impl TemplNS for A {
        type EType = &'static str;
    }
    impl TemplNS for B {
        type EType = &'static str;
    }

    trait Process<S: Sys> {
        type NS: TemplNS;

        fn process(&self, cur: &mut S::Cursor<Self::NS>);
    }

    struct Comp1 {
        comp2: Comp2,
    }
    struct Comp2;

    impl<S: Sys> Process<S> for Comp1 {
        type NS = A;

        fn process(&self, cur: &mut S::Cursor<Self::NS>) {
            cur.add("lol");
            // now try to process comp2...
            S::enter_ns::<A, <Comp2 as Process<S>>::NS, _>(cur, |cur| {
                <Comp2 as Process<S>>::process(&self.comp2, cur)
            });
        }
    }

    impl<S: Sys> Process<S> for Comp2 {
        type NS = B;

        fn process(&self, cur: &mut S::Cursor<Self::NS>) {
            cur.add("bar");
        }
    }
}
