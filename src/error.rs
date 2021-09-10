#[derive(Debug)]
pub enum Error {
    NoProgram,
    AddChild,
    RemoveChild,
    RemoveNode,
    SetAttribute,
    Unmount,
    ExitElement,
    RemoveElement,
    DomCorruption,
}
