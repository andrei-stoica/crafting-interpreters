use crate::tree_walk::parser::LiteralExpr; // This might need to come out of
                                           // tree_walk to make more sense

use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum LoxType {
    Number(f64),
    Bool(bool),
    String(std::string::String),
    Nil,
}

impl From<LiteralExpr> for LoxType {
    fn from(value: LiteralExpr) -> Self {
        match value {
            LiteralExpr::False => LoxType::Bool(false),
            LiteralExpr::True => LoxType::Bool(true),
            LiteralExpr::StringLit(s) => LoxType::String(s),
            LiteralExpr::Number(n) => LoxType::Number(n),
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
            Self::Nil => write!(f, "nil"),
        }
    }
}
