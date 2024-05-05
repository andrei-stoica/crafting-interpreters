use std::fmt::Display;
use std::time::{SystemTime, UNIX_EPOCH};

use super::Callable;
use super::Error;
use super::LoxType;

#[derive(Debug, Clone, PartialEq)]
pub enum Builtin {
    Time,
}

fn time() -> LoxType {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");

    LoxType::Number(since_the_epoch.as_secs_f64())
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Time => write!(f, "<builtin-fn time()>"),
            builtin => unimplemented!("Display not implemented for builtin {builtin}"),
        }
    }
}

impl Callable for Builtin {
    fn arity(&self) -> usize {
        match self {
            Self::Time => 0,
            builtin => unimplemented!("arity not implemented for builtin {builtin}"),
        }
    }

    fn call(
        &self,
        interpreter: &mut crate::tree_walk::interpreter::Interpreter,
        arguments: Box<[super::LoxType]>,
    ) -> super::Result<super::LoxType> {
        if arguments.len() != self.arity() {
            return Err(Error::InvalidArity {
                expected: self.arity(),
                received: arguments.len(),
            });
        }

        match self {
            Self::Time => Ok(time()),
        }
    }
}
