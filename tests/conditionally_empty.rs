use hypp::prelude::*;

component! {
    ConditionalString(string: Option<&str>) {}

    if let Some(string) = string {
        {string}
    }
}

// This represents a corner case:
// A component rendered before a conditional node.
// the component could potentially be empty,
// and could instantiate its DOM "after-the-fact".
// In that case it has to know where to anchor.
component! {
    ComponentBeforeConditional(
        foo: Option<&str>,
        bar: Option<&str>,
        baz: Option<&str>
    ) {}

    if let Some(foo) = foo {
        {foo}
    }
    <ConditionalString string={bar} />
    if let Some(baz) = baz {
        {baz}
    }
}

struct Test {
    hypp: hypp::server::ServerHypp,
    comp: <ComponentBeforeConditional<hypp::server::ServerHypp> as ToHandle>::Handle,
}

impl Test {
    fn new() -> Self {
        let hypp = hypp::server::ServerHypp::new();
        let comp = ComponentBeforeConditional::mount(
            __ComponentBeforeConditionalProps {
                foo: None,
                bar: None,
                baz: None,
            },
            &mut hypp.builder_at_body(),
        )
        .unwrap();

        Self { hypp, comp }
    }

    fn verify(&mut self, props: (u8, u8, u8)) {
        let foo = props.0 > 0;
        let bar = props.1 > 0;
        let baz = props.2 > 0;

        let expected = format!(
            "<body>{}{}{}</body>",
            if foo { "foo" } else { "" },
            if bar { "bar" } else { "" },
            if baz { "baz" } else { "" },
        );

        self.comp.borrow_mut().pass_props(
            __ComponentBeforeConditionalProps {
                foo: if foo { Some("foo") } else { None },
                bar: if bar { Some("bar") } else { None },
                baz: if baz { Some("baz") } else { None },
            },
            &mut self.hypp.builder_at_body(),
        );

        assert_eq!(self.hypp.render(), expected);
    }
}

#[test]
fn stress_test_sequential_conditionals() {
    let mut test = Test::new();

    // basics 1: turning on and off in isolation
    test.verify((0, 0, 0));
    test.verify((1, 0, 0));
    test.verify((0, 0, 0));
    test.verify((0, 1, 0));
    test.verify((0, 0, 0));
    test.verify((0, 0, 1));
    test.verify((0, 0, 0));

    // basics 2: directly transitioning from one "1" to another
    test.verify((1, 0, 0));
    test.verify((0, 1, 0));
    test.verify((0, 0, 1));
    test.verify((0, 1, 0));
    test.verify((1, 0, 0));
    test.verify((0, 0, 1));

    // with siblings:
    test.verify((0, 0, 0));
    test.verify((1, 0, 0));
    test.verify((1, 1, 0));
    test.verify((0, 1, 1));
    test.verify((0, 0, 1));
    test.verify((0, 1, 1));
    test.verify((1, 1, 0));
    test.verify((1, 0, 0));
    test.verify((0, 0, 0));

    // symmetry:
    test.verify((0, 1, 0));
    test.verify((1, 0, 1));
    test.verify((0, 1, 0));
    test.verify((0, 0, 0));
    test.verify((1, 0, 1));
    test.verify((0, 0, 0));
    test.verify((1, 0, 1));
    test.verify((1, 1, 1));
    test.verify((0, 1, 0));
    test.verify((0, 0, 0));
}