use std::fmt::Display;

use crate::lox::{Error as LoxTypeError, LoxType};
use crate::tree_walk::token::Token;

#[derive(Debug, PartialEq)]
pub enum Error {
    OperationNotSuported {
        operator: Token,
        values: (LoxType, Option<LoxType>),
    },
    TokenIsNotAnIdenifier(Token),
    AssignTargetNotVariable,
    ExitingGlobalScope,
    UndifinedVariable(String),
    ConditionNotBool,
    NotCallable {
        line: u32,
        err: LoxTypeError,
    },
    InvalidArity {
        line: u32,
        err: LoxTypeError,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OperationNotSuported { operator, values } => {
                let second_value = match &values.1 {
                    Some(value) => format!("and {:?}", value),
                    None => "".into(),
                };
                write!(
                    f,
                    "[Line {}] {:?} not supported for {:?} {}",
                    operator.line, operator.token_type, values.0, second_value
                )
            }
            Self::TokenIsNotAnIdenifier(token) => {
                write!(f, "[Line {}] Token expect to be an identifier.", token.line)
            }
            Self::UndifinedVariable(name) => {
                write!(f, "Undifined variable '{}'.", name)
            }
            Self::NotCallable { line, err } | Self::InvalidArity { line, err } => {
                write!(f, "[Line {line}] {err}")
            }
            _ => todo!(),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
