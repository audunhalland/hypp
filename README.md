# Hypp - a web-enabled GUI framework

## Declarative
Hypp enables you to create GUI applications using a declarative programming style drawing inspiration from other popular web frameworks.

The core abstraction is the `Component`, which usually represents some kind
of rectangle on the screen.

## Efficient
Hypp tries to analyze as much as possible from your template declarations - the ultimate goal being to produce the most efficient DOM updating code possible.

Hypp doesn't use a "virtual DOM". Instead it performs inline patching of the DOM exactly where a change happens, and does no DOM work at all if there are no changes.

## Ergonomic
Hypp strives to keep the amount of boilerplate code to an absolute minimum. Rust is a fantastic language, but nonetheless infamous for its verbosity. Our feeling
is that when writing GUI code, you want to concentrating on the logic that decides
what will be shown on the screen.

Hypp provides an advanced code generator that enables both ergonomic templates
and efficient target code at the same time.

## Multi-target
Hypp is not designed towards a specific target. Thus, using it in a web server works equally well as inside a browser. There are plans for targeting other GUI environments that use a DOM-like representation like widget trees.

## Work in progress
Hypp is a work in progress. The surface syntax is not written in stone. Examples on how to use it currently can be found inside the code base.

## To do
- [x] Self-update
- [x] For loops in template
- [x] Reduce amount of generated code using closures in patch
- [x] Passing callbacks as props to other components
- [ ] typed argument(s) in callback slots
- [ ] use any type for lists
- [ ] Avoid copying into env for non-self-updatable components
- [ ] Support namespace with `type` syntax
