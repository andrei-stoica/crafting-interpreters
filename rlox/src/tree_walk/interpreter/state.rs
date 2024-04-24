use std::collections::HashMap;

use crate::lox::type_system::LoxType;

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

    pub fn exit(self) -> Option<Self> {
        match self.enclosing {
            Some(env_box) => Some(*env_box),
            None => None,
        }
    }
}
