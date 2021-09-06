enum ProgramError {}

trait DomVM {
    fn push_element(&mut self, tag_name: &'static str) -> Result<(), ProgramError>;
    fn set_attribute(
        &mut self,
        name: &'static str,
        value: &'static str,
    ) -> Result<(), ProgramError>;
    fn append_text(&mut self, text: &'static str) -> Result<(), ProgramError>;
    fn pop_element(&mut self) -> Result<(), ProgramError>;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_program(vm: &mut dyn DomVM) -> Result<(), ProgramError> {
        vm.push_element("p")?;
        vm.set_attribute("className", "yo")?;
        vm.append_text("test")?;
        vm.pop_element()?;

        Ok(())
    }
}
