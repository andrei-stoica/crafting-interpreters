use std::collections::HashMap;
use std::rc::Rc;
use std::{borrow::BorrowMut, cell::RefCell};

use crate::lox::LoxType;

use super::{Error, Result};

#[derive(Debug, Clone)]
pub struct Environment {
    pub state: HashMap<Box<str>, LoxType>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            state: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn new_sub_envoronment(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            state: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn put(&mut self, name: Box<str>, value: LoxType) {
        self.state.insert(name, value);
    }

    pub fn get(&mut self, name: &str) -> Result<LoxType> {
        match self.state.get(name) {
            Some(value) => Ok(value.clone()),
            None => match &mut self.enclosing {
                Some(enclosing_env) => {
                    // This is ugly but I can't think of another way to have
                    // multiple pointers to an env
                    let env_ref = &mut *enclosing_env.borrow_mut();
                    let mut env = env_ref.as_ref().borrow_mut();
                    env.get(name)
                }
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
                Some(enclosing_env) => {
                    eprintln!("getting from enclosing");
                    let env_ref = &mut *enclosing_env.borrow_mut();
                    let mut env = env_ref.as_ref().borrow_mut();
                    env.assign(name.into(), value)
                }
                _ => Err(Error::UndifinedVariable(name.into())),
            },
        }
    }
}
