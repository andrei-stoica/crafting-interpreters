use super::Builtin;
use crate::tree_walk::parser::{AstNode, LiteralExpr}; // This might need to come out of

use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum LoxType {
    Number(f64),
    Bool(bool),
    String(Box<str>),
    Function {
        parameters: Box<[LoxType]>,
        body: AstNode,
    },
    Builtin(Builtin),
    Nil,
}

impl From<&LiteralExpr> for LoxType {
    fn from(value: &LiteralExpr) -> Self {
        match value {
            LiteralExpr::False => LoxType::Bool(false),
            LiteralExpr::True => LoxType::Bool(true),
            LiteralExpr::StringLit(s) => LoxType::String(s.clone()),
            LiteralExpr::Number(n) => LoxType::Number(n.clone()),
            LiteralExpr::Nil => LoxType::Nil,
        }
    }
}

impl Into<bool> for LoxType {
    fn into(self) -> bool {
        match self {
            LoxType::Nil => false,
            LoxType::Bool(v) => v,
            _ => true,
        }
    }
}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(value) => write!(f, "{}", value),
            Self::String(value) => write!(f, "{}", value),
            Self::Bool(value) => write!(f, "{}", value),
            Self::Function { parameters, .. } => {
                write!(
                    f,
                    "fn ({})",
                    parameters
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::Builtin(builtin) => write!(f, "{builtin}"),
            Self::Nil => write!(f, "nil"),
        }
    }
}
