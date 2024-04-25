use std::fmt::Display;

use crate::lox::type_system::LoxType;
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
            _ => todo!(),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
