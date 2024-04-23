use std::fmt::Display;

use super::RetVal;
use crate::tree_walk::token::Token;

#[derive(Debug, PartialEq)]
pub enum Error {
    OperationNotSuported {
        operator: Token,
        values: (RetVal, Option<RetVal>),
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
            _ => todo!(),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
