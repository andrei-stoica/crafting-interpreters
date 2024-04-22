#![allow(dead_code)]

use core::slice;
use std::fmt::Display;

use super::parser::{
    AstNode::{self, *},
    LiteralExpr,
};
use super::token::{Token, TokenType};

#[derive(Debug, PartialEq)]
pub enum Error {
    OperationNotSuported { operator: Token },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OperationNotSuported { operator } => write!(
                f,
                "[Line {}] {:?} not supported for types",
                operator.line, operator.token_type
            ),
            _ => todo!(),
        }
    }
}

impl std::error::Error for Error {}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum RetVal {
    Number(f64),
    Bool(bool),
    String(std::string::String),
    Nil,
}

impl From<LiteralExpr> for RetVal {
    fn from(value: LiteralExpr) -> Self {
        match value {
            LiteralExpr::False => RetVal::Bool(false),
            LiteralExpr::True => RetVal::Bool(true),
            LiteralExpr::StringLit(s) => RetVal::String(s),
            LiteralExpr::Number(n) => RetVal::Number(n),
            LiteralExpr::Nil => RetVal::Nil,
        }
    }
}

impl Display for RetVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(value) => write!(f, "{}", value),
            Self::String(value) => write!(f, "{}", value),
            Self::Bool(value) => write!(f, "{}", value),
            Self::Nil => write!(f, "nil"),
        }
    }
}

pub fn evaluate(node: AstNode) -> Result<RetVal> {
    match node {
        Prog(stmts) => evaluate_prog(stmts),
        ExprStmt(stmt) => evaluate(*stmt),
        PrintStmt(expr) => evaluate_print_stmt(*expr),
        BinaryExpr {
            left,
            operator,
            right,
        } => evaluate_binary_expr(*left, operator, *right),
        Literal(expr) => Ok(expr.into()),
        _ => {
            eprintln!("{node:#?}");
            unimplemented!();
        }
    }
}

fn evaluate_prog(stmts: Vec<AstNode>) -> Result<RetVal> {
    for stmt in stmts {
        evaluate(stmt)?;
    }
    Ok(RetVal::Nil)
}

fn evaluate_print_stmt(expr: AstNode) -> Result<RetVal> {
    println!("{}", evaluate(expr)?);
    Ok(RetVal::Nil)
}

fn evaluate_binary_expr(left: AstNode, operator: Token, right: AstNode) -> Result<RetVal> {
    let left_val = evaluate(left)?;
    let right_val = evaluate(right)?;
    match operator.token_type {
        TokenType::Plus => match (left_val, right_val) {
            (RetVal::Number(left), RetVal::Number(right)) => Ok(RetVal::Number(left + right)),
            (RetVal::String(left), RetVal::String(right)) => {
                let val = [left.as_str(), right.as_str()].concat();
                Ok(RetVal::String(val))
            }
            _ => Err(Error::OperationNotSuported {
                operator: operator.clone(),
            }),
        },
        TokenType::Minus => match (left_val, right_val) {
            (RetVal::Number(left), RetVal::Number(right)) => Ok(RetVal::Number(left - right)),
            _ => Err(Error::OperationNotSuported {
                operator: operator.clone(),
            }),
        },
        TokenType::Slash => match (left_val, right_val) {
            (RetVal::Number(left), RetVal::Number(right)) => Ok(RetVal::Number(left / right)),
            _ => Err(Error::OperationNotSuported {
                operator: operator.clone(),
            }),
        },
        TokenType::Star => match (left_val, right_val) {
            (RetVal::Number(left), RetVal::Number(right)) => Ok(RetVal::Number(left * right)),
            _ => Err(Error::OperationNotSuported {
                operator: operator.clone(),
            }),
        },
        _ => {
            eprintln!("{operator:#?}");
            unimplemented!();
        }
    }
}

#[cfg(test)]
mod test {
    use crate::tree_walk::{
        parser::LiteralExpr,
        token::{Token, TokenType},
    };

    use super::*;

    #[test]
    fn test_eval() {
        let prog = AstNode::Prog(vec![]);
        let _ = evaluate(prog);
        // TODO: add assertion to verfy results

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Plus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = evaluate(prog);
        assert_eq!(Ok(RetVal::Number(3.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Minus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = evaluate(prog);
        assert_eq!(Ok(RetVal::Number(-1.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Slash,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = evaluate(prog);
        assert_eq!(Ok(RetVal::Number(0.5)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(-1.0))),
            operator: Token {
                token_type: TokenType::Slash,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = evaluate(prog);
        assert_eq!(Ok(RetVal::Number(-0.5)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Star,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = evaluate(prog);
        assert_eq!(Ok(RetVal::Number(4.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Plus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        }));
        let res = evaluate(prog);
        assert_eq!(Ok(RetVal::String("AB".into())), res);
    }
}
