pub struct StateRef<'a, T> {
    val: &'a mut T,
    changed: &'a mut bool,
}

impl<'a, T> StateRef<'a, T> {
    pub fn new(val: &'a mut T, changed: &'a mut bool) -> Self {
        Self { val, changed }
    }
}

impl<'a, T> std::ops::Deref for StateRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.val
    }
}

impl<'a, T> std::ops::DerefMut for StateRef<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        *self.changed = true;
        &mut self.val
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bool() {
        pub struct Mutation<'a> {
            state: StateRef<'a, bool>,
        }

        impl<'a> Mutation<'a> {
            fn immutable_access(&mut self) {
                println!("state: {}", *self.state);
            }

            fn mutable_access(&mut self) {
                *self.state = !*self.state;
            }
        }

        let mut state = false;
        let mut changed = false;

        {
            let mut mutation = Mutation {
                state: StateRef::new(&mut state, &mut changed),
            };

            mutation.immutable_access();
        }

        assert!(!changed);

        {
            let mut mutation = Mutation {
                state: StateRef::new(&mut state, &mut changed),
            };

            mutation.mutable_access();
        }

        assert!(changed);
    }

    #[test]
    fn test_struct() {
        pub struct Struct {}

        impl Struct {
            fn immutable_method(&self) {}
            fn mutable_method(&mut self) {}
        }

        pub struct Mutation<'a> {
            state: StateRef<'a, Struct>,
        }

        impl<'a> Mutation<'a> {
            fn immutable_access(&mut self) {
                self.state.immutable_method();
            }

            fn mutable_access(&mut self) {
                self.state.mutable_method();
            }
        }

        let mut state = Struct {};
        let mut changed = false;

        {
            let mut mutation = Mutation {
                state: StateRef::new(&mut state, &mut changed),
            };

            mutation.immutable_access();
        }

        assert!(!changed);

        {
            let mut mutation = Mutation {
                state: StateRef::new(&mut state, &mut changed),
            };

            mutation.mutable_access();
        }

        assert!(changed);
    }
}
