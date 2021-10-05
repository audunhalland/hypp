use web_ns::LocalName;

pub struct Html;

impl crate::TemplNS for Html {
    type EType = web_ns::html5::HtmlTag;
    type AType = web_ns::html5::HtmlAttr;
}

impl crate::Name for web_ns::html5::HtmlTag {
    fn name(&self) -> &str {
        self.local_name()
    }
}

impl crate::Name for web_ns::html5::HtmlAttr {
    fn name(&self) -> &str {
        self.local_name()
    }
}

impl crate::Name for &'static str {
    fn name(&self) -> &'static str {
        self
    }
}

pub struct HtmlEventKind;

impl crate::EventKind<Html> for HtmlEventKind {
    type Event = &'static str;

    #[cfg(feature = "web")]
    #[cfg(feature = "web")]
    fn from_web_event<W: wasm_bindgen::convert::FromWasmAbi>(_web_event: &W) -> &'static str {
        "TODO: make a real event :)"
    }
}

impl<H: crate::Hypp> crate::Subscribe<H, dyn Fn() + 'static>
    for crate::Slot<H, Html, HtmlEventKind>
{
    fn subscribe(&mut self, func: H::Shared<dyn Fn() + 'static>) {
        self.0.listen(H::make_box_shared(Box::new(move |_| {
            func();
        })));
    }
}

impl<H: crate::Hypp> crate::Subscribe<H, dyn Fn(&'static str) + 'static>
    for crate::Slot<H, Html, HtmlEventKind>
{
    fn subscribe(&mut self, func: H::Shared<dyn Fn(&'static str) + 'static>) {
        self.0.listen(func);
    }
}

// TODO: if cfg html
pub mod html {
    pub use ::web_ns::html5::HtmlAttr;
    pub use ::web_ns::html5::HtmlTag;
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
        fn add(&mut self, _element: N::EType) {}
    }

    struct A;
    struct B;

    impl TemplNS for A {
        type EType = &'static str;
        type AType = &'static str;
    }
    impl TemplNS for B {
        type EType = &'static str;
        type AType = &'static str;
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
