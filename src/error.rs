#[derive(Debug)]
pub enum Error {
    AddChild,
    RemoveChild,
    RemoveNode,
    SetAttribute,
    Unmount,
    ExitElement,
    RemoveElement,
    DomCorruption,
}
