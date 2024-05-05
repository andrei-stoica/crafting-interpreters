use super::{Error, LoxType, Result};
use crate::tree_walk::interpreter::Interpreter;

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
                        expected: self.arity(),
                        received: arguments.len(),
                    });
                }

                todo!("run fuction")
            }
            Self::Builtin(builtin) => builtin.call(interpreter, arguments),
            _ => Err(Error::NotCallable),
        }
    }
}
