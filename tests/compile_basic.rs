#![feature(generic_associated_types)]

use hypp::prelude::*;
use hypp::server::ServerHypp;

component! {
    Foo(is_cool: bool) {}

    // let label = if is_cool { "cool" } else { "dull" };

    <div>
        <p class="css">
            <span>
                // FIXME: Need some way to define this as a temporary variable?
                // maybe let bindings before the template?
                if is_cool {
                    "cool"
                } else {
                    "dull"
                }
            </span>
        </p>
    </div>
}

#[test]
fn render_foo_server() {
    let hypp = ServerHypp::new();
    let mut c = Foo::<ServerHypp>::mount(
        __FooProps {
            is_cool: (true, hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    )
    .unwrap();

    assert_eq!(
        hypp.render(),
        "<body><div><p class=\"css\"><span>cool</span></p></div></body>"
    );

    c.get_mut().pass_props(
        __FooProps {
            is_cool: (false, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    assert_eq!(
        &hypp.render(),
        "<body><div><p class=\"css\"><span>dull</span></p></div></body>"
    );
}

mod inside {
    use super::*;

    component! {
        P1(text: &str) {}

        <p>{text}</p>
    }

    component! {
        P2() {}

        <p>"static"</p>
    }
}

component! {
    Baz() {}

    <div>
        // En kommentar
        <inside::P1 text="variable"/>
        <inside::P2/>
    </div>
}

#[test]
fn render_baz_server() {
    let hypp = ServerHypp::new();
    Baz::<ServerHypp>::mount(__BazProps {}, &mut hypp.builder_at_body()).unwrap();

    assert_eq!(
        &hypp.render(),
        "<body><div><p>variable</p><p>static</p></div></body>"
    );
}

component! {
    Conditional(hello: bool, world: bool) {}

    <div>
        if hello {
            <span>"Hello"</span>
            if world {
                <a>"World"</a>
            } else {
                <span>"Universe"</span>
            }
        }
    </div>
}

#[test]
fn render_conditional_server() {
    let hypp = ServerHypp::new();
    let mut c = Conditional::<ServerHypp>::mount(
        __ConditionalProps {
            hello: (false, ::hypp::Refresh(true)),
            world: (false, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    )
    .unwrap();

    assert_eq!(hypp.render(), "<body><div/></body>");

    c.get_mut().pass_props(
        __ConditionalProps {
            hello: (false, ::hypp::Refresh(true)),
            world: (true, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    // No change:
    assert_eq!(hypp.render(), "<body><div/></body>");

    c.get_mut().pass_props(
        __ConditionalProps {
            hello: (true, ::hypp::Refresh(true)),
            world: (false, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    assert_eq!(
        hypp.render(),
        "<body><div><span>Hello</span><span>Universe</span></div></body>"
    );

    c.get_mut().pass_props(
        __ConditionalProps {
            hello: (true, ::hypp::Refresh(true)),
            world: (true, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    assert_eq!(
        hypp.render(),
        "<body><div><span>Hello</span><a>World</a></div></body>"
    );

    c.get_mut().erase(&mut hypp.builder_at_body());

    assert_eq!(hypp.render(), "<body/>");
}

component! {
    ConditionalWithVariableText(hello: bool, yo: &str) {}

    <div>
    if hello {
        {yo}
    }
    </div>
}

component! {
    ConditionalWithComponent(hello: bool) {}

    <div>
    if hello {
        <Baz />
    }
    </div>
}

component! {
    ITakeANumber(number: u32) {}

    "num"
}

component! {
    IfLet(opt_number: Option<u32>) {}

    <article>
        if let Some(number) = opt_number {
            <ITakeANumber number={number} />
        }
    </article>
}

#[test]
fn render_iflet_server() {
    use hypp::*;

    let hypp = hypp::server::ServerHypp::new();
    let mut c = IfLet::<ServerHypp>::mount(
        __IfLetProps {
            opt_number: (None, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    )
    .unwrap();

    assert_eq!(hypp.render(), "<body><article/></body>");

    c.get_mut().pass_props(
        __IfLetProps {
            opt_number: (Some(42), ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    assert_eq!(hypp.render(), "<body><article>num</article></body>");

    c.get_mut().pass_props(
        __IfLetProps {
            opt_number: (None, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    assert_eq!(hypp.render(), "<body><article/></body>");

    c.get_mut().erase(&mut hypp.builder_at_body());

    assert_eq!(hypp.render(), "<body/>");
}

component! {
    OptionString(opt_str: Option<&str>) {}

    <article>
        if let Some(str) = opt_str {
            <p>{str}</p>
        }
    </article>
}

component! {
    Fragment1(perhaps: bool) {}

    <div>"first"</div>
    if perhaps {
        <span>"second"</span>
    }
    <p>"third"</p>
}

#[test]
fn render_fragment1() {
    use hypp::*;

    let hypp = hypp::server::ServerHypp::new();
    let mut c = Fragment1::<ServerHypp>::mount(
        __Fragment1Props {
            perhaps: (true, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    )
    .unwrap();

    assert_eq!(
        hypp.render(),
        "<body><div>first</div><span>second</span><p>third</p></body>"
    );

    c.get_mut().pass_props(
        __Fragment1Props {
            perhaps: (false, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    assert_eq!(hypp.render(), "<body><div>first</div><p>third</p></body>");

    c.get_mut().erase(&mut hypp.builder_at_body());

    assert_eq!(hypp.render(), "<body/>");
}

component! {
    Recursive(depth: usize) {}

    <span>
        {format!("{}", depth)}
        if depth > 1 {
            <Recursive depth={depth - 1} />
        }
    </span>
}

#[test]
fn render_recursive_server() {
    let hypp = hypp::server::ServerHypp::new();
    let mut c = Recursive::<ServerHypp>::mount(
        __RecursiveProps {
            depth: (3, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    )
    .unwrap();

    assert_eq!(
        hypp.render(),
        "<body><span>3<span>2<span>1</span></span></span></body>"
    );

    c.get_mut().pass_props(
        __RecursiveProps {
            depth: (2, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    assert_eq!(hypp.render(), "<body><span>2<span>1</span></span></body>");

    c.get_mut().erase(&mut hypp.builder_at_body());

    assert_eq!(hypp.render(), "<body/>");
}

component! {
    List(items: &[String]) {}

    <ul>
    for item in items {
        <li>{item}</li>
    }
    </ul>
}

#[test]
fn render_list() {
    let hypp = hypp::server::ServerHypp::new();
    let strings1 = ["foo".to_string(), "bar".to_string()];
    let strings2 = ["bar".to_string(), "baz".to_string()];

    let mut c = List::<ServerHypp>::mount(
        __ListProps {
            items: (&strings1, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    )
    .unwrap();

    assert_eq!(
        hypp.render(),
        "<body><ul><li>foo</li><li>bar</li></ul></body>"
    );

    c.get_mut().pass_props(
        __ListProps {
            items: (&strings2, ::hypp::Refresh(true)),
        },
        &mut hypp.builder_at_body(),
    );

    assert_eq!(
        hypp.render(),
        "<body><ul><li>bar</li><li>baz</li></ul></body>"
    );

    c.get_mut().erase(&mut hypp.builder_at_body());

    assert_eq!(hypp.render(), "<body/>");
}

component! {
    TopLevelConditional(lol: bool, text: &str) {}

    if lol {
        {text}
    } else {
        <p>"goo"</p>
    }
}

component! {
    StringProp1(arg: &str) {}
    <p>{arg}</p>
}

component! {
    StringProp2(arg: &str) {}
    <div>{arg}</div>
}

component! {
    ConditionalStringProp(arg: &str, draw_one: bool) {}

    if draw_one {
        <StringProp1 arg={arg} />
    } else {
        <StringProp2 arg={arg} />
    }
}

pub struct SomeStruct {
    text: String,
}

component! {
    StructInList() {
        items: Vec<SomeStruct>
    }

    for item in items {
        <div>{&item.text}</div>
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum E {
    Foo(String),
    Bar(&'static str),
    Baz,
}

component! {
    Enum(e: &E) {}

    <div>
        match e {
            E::Foo(arg) => { format!("Foo({})", arg) }
            E::Bar(arg) =>
                <p>
                    {format!("Bar({})", arg)}
                </p>
            E::Baz => <button>"Baz"</button>
        }
    </div>
}
