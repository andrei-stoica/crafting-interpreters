use std::collections::HashMap;

use crate::lox::type_system::LoxType;

use super::{Error, Result};

pub struct Environment {
    pub state: HashMap<String, LoxType>,
    pub enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            state: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_sub_envoronment(enclosing: Environment) -> Self {
        Self {
            state: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    pub fn exit(self) -> Result<Self> {
        match self.enclosing {
            Some(env_box) => Ok(*env_box),
            None => Err(Error::ExitingGlobalScope),
        }
    }

    pub fn put(&mut self, name: String, value: LoxType) {
        self.state.insert(name, value);
    }

    pub fn get(&mut self, name: &str) -> Result<LoxType> {
        match self.state.get(name) {
            Some(value) => Ok(value.clone()),
            None => Err(Error::UndifinedVariable(name.to_string())),
        }
    }
}
