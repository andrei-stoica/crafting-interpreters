use std::fmt::Display;

use crate::lox::LoxType;
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
        line: Option<usize>,
    },
    InvalidArity {
        // HACK: call funcion doesn't have knowledge of the line.
        // returning none and enriching the error furtur up the call stack
        // for the moment
        line: Option<usize>,
        expected: usize,
        received: usize,
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
            Self::NotCallable { line } => {
                let line_text = optional_line_to_string(line);
                write!(f, "[Line {line_text}] Only functions are callable")
            }
            Self::InvalidArity {
                line,
                expected,
                received,
            } => {
                let line_text = optional_line_to_string(line);
                write!(
                    f,
                    "[Line {line_text}] Expected {expected} arguments but got {received}."
                )
            }
            _ => todo!(),
        }
    }
}

fn optional_line_to_string(line: &Option<usize>) -> String {
    match line {
        Some(l) => l.to_string(),
        None => "N/A".into(),
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
