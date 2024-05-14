use super::{Error, Result};
use crate::lox::LoxType;
use crate::tree_walk::interpreter::{Environment, Interpreter};
use std::{cell::RefCell, iter::zip, rc::Rc};

pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, arguments: Box<[LoxType]>) -> Result<LoxType>;
    fn arity(&self) -> usize;
}

impl Callable for LoxType {
    fn arity(&self) -> usize {
        match self {
            Self::Function { parameters, .. } => parameters.len(),
            _ => 0,
        }
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Box<[LoxType]>) -> Result<LoxType> {
        match self {
            Self::Function { parameters, body } => {
                if arguments.len() != self.arity() {
                    return Err(Error::InvalidArity {
                        line: None,
                        expected: self.arity(),
                        received: arguments.len(),
                    })?;
                }

                let mut env = Environment::new_sub_envoronment(interpreter.get_globals());
                zip(parameters.iter(), arguments.iter())
                    .for_each(|(param, arg)| env.put(param.clone().into_boxed_str(), arg.clone()));

                Ok(interpreter.evaluate_with_env(&[body.clone()], Rc::new(RefCell::new(env)))?)
            }
            Self::Builtin(builtin) => builtin.call(interpreter, arguments),
            _ => Err(Error::NotCallable { line: None }),
        }
    }
}
