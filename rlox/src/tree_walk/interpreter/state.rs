use std::collections::HashMap;

use crate::lox::type_system::LoxType;

use super::{Error, Result};

#[derive(Debug, Clone)]
pub struct Environment {
    pub state: HashMap<Box<str>, LoxType>,
    pub enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            state: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_sub_envoronment(enclosing: &Environment) -> Self {
        // NOTE: there's a lot of cloning when I enter and exit environemnts,
        // need to figure out how to do it without cloning
        Self {
            state: HashMap::new(),
            enclosing: Some(Box::new(enclosing.clone())),
        }
    }

    pub fn exit(&mut self) -> Result<Self> {
        match &mut self.enclosing {
            Some(env_box) => Ok(*env_box.clone()),
            None => Err(Error::ExitingGlobalScope),
        }
    }

    pub fn put(&mut self, name: Box<str>, value: LoxType) {
        self.state.insert(name, value);
    }

    pub fn get(&mut self, name: &str) -> Result<LoxType> {
        match self.state.get(name) {
            Some(value) => Ok(value.clone()),
            None => match &mut self.enclosing {
                Some(enclosing_env) => enclosing_env.get(name),
                None => Err(Error::UndifinedVariable(name.to_string())),
            },
        }
    }

    pub fn assign(&mut self, name: &str, value: LoxType) -> Result<LoxType> {
        match self.state.contains_key(name.into()) {
            true => {
                self.state.insert(name.into(), value.clone());
                Ok(value)
            }
            _ => match &mut self.enclosing {
                Some(enclosing_env) => enclosing_env.assign(name.into(), value.clone()),
                _ => Err(Error::UndifinedVariable(name.into())),
            },
        }
    }
}
