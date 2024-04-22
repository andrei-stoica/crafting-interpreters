#![allow(dead_code)]

use std::{borrow::BorrowMut, fmt::Display, io::Write};

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
                "[Line {}] {:?} not supported for given type(s)",
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

pub struct Interpreter<'a> {
    // these enable redirecting output so that print can be tested
    output: Box<(dyn Write + 'a)>,
    err_output: Box<(dyn Write + 'a)>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            output: Box::new(std::io::stdout()),
            err_output: Box::new(std::io::stderr()),
        }
    }

    pub fn new_with_output(out: Box<dyn Write + 'a>, err_out: Box<dyn Write + 'a>) -> Self {
        Self {
            output: out,
            err_output: err_out,
        }
    }

    pub fn evaluate(&mut self, node: AstNode) -> Result<RetVal> {
        match node {
            // NOTE: statements returning values does not make sense, but I
            // will need to rewrite test to change
            Prog(stmts) => self.evaluate_prog(stmts),
            ExprStmt(stmt) => self.evaluate(*stmt),
            PrintStmt(expr) => self.evaluate_print_stmt(*expr),
            BinaryExpr {
                left,
                operator,
                right,
            } => self.evaluate_binary_expr(*left, operator, *right),
            UnaryExpr { operator, right } => self.evaluate_unary_expr(operator, *right),
            Grouping(expr) => self.evaluate(*expr),
            Literal(expr) => Ok(expr.into()),
            _ => {
                eprintln!("{node:#?}");
                unimplemented!();
            }
        }
    }

    fn evaluate_prog(&mut self, stmts: Vec<AstNode>) -> Result<RetVal> {
        for stmt in stmts {
            self.evaluate(stmt)?;
        }
        Ok(RetVal::Nil)
    }

    fn evaluate_print_stmt(&mut self, expr: AstNode) -> Result<RetVal> {
        let output = format!("{}", self.evaluate(expr)?);
        self.output.write(output.as_bytes()); // TODO: need to see what I do with this return value
        Ok(RetVal::Nil)
    }

    fn evaluate_binary_expr(
        &mut self,
        left: AstNode,
        operator: Token,
        right: AstNode,
    ) -> Result<RetVal> {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;
        match operator.token_type {
            TokenType::Plus => match (left_val, right_val) {
                (RetVal::Number(l), RetVal::Number(r)) => Ok(RetVal::Number(l + r)),
                (RetVal::String(l), RetVal::String(r)) => {
                    let val = [l.as_str(), r.as_str()].concat();
                    Ok(RetVal::String(val))
                }
                _ => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                }),
            },
            TokenType::Minus => match (left_val, right_val) {
                (RetVal::Number(l), RetVal::Number(r)) => Ok(RetVal::Number(l - r)),
                _ => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                }),
            },
            TokenType::Slash => match (left_val, right_val) {
                (RetVal::Number(l), RetVal::Number(r)) => Ok(RetVal::Number(l / r)),
                _ => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                }),
            },
            TokenType::Star => match (left_val, right_val) {
                (RetVal::Number(l), RetVal::Number(r)) => Ok(RetVal::Number(l * r)),
                _ => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                }),
            },
            TokenType::Greater => match (left_val, right_val) {
                (RetVal::Number(l), RetVal::Number(r)) => Ok(RetVal::Bool(l > r)),
                (RetVal::String(l), RetVal::String(r)) => Ok(RetVal::Bool(l > r)),
                _ => Err(Error::OperationNotSuported { operator }),
            },
            TokenType::GreaterEqual => match (left_val, right_val) {
                (RetVal::Number(l), RetVal::Number(r)) => Ok(RetVal::Bool(l >= r)),
                (RetVal::String(l), RetVal::String(r)) => Ok(RetVal::Bool(l >= r)),
                _ => Err(Error::OperationNotSuported { operator }),
            },
            TokenType::Less => match (left_val, right_val) {
                (RetVal::Number(l), RetVal::Number(r)) => Ok(RetVal::Bool(l < r)),
                (RetVal::String(l), RetVal::String(r)) => Ok(RetVal::Bool(l < r)),
                _ => Err(Error::OperationNotSuported { operator }),
            },
            TokenType::LessEqual => match (left_val, right_val) {
                (RetVal::Number(l), RetVal::Number(r)) => Ok(RetVal::Bool(l <= r)),
                (RetVal::String(l), RetVal::String(r)) => Ok(RetVal::Bool(l <= r)),
                _ => Err(Error::OperationNotSuported { operator }),
            },
            TokenType::EqualEqual => Ok(self.is_equal(left_val, right_val)),
            TokenType::BangEqual => Ok(self.is_not_truthy(self.is_equal(left_val, right_val))),
            _ => {
                eprintln!("{operator:#?}");
                unimplemented!();
            }
        }
    }

    fn is_equal(&self, left: RetVal, right: RetVal) -> RetVal {
        match (left, right) {
            (RetVal::Nil, RetVal::Nil) => RetVal::Bool(true),
            (RetVal::Nil, _) => RetVal::Bool(false),

            (RetVal::Number(left_val), RetVal::Number(right_val)) => {
                RetVal::Bool(left_val == right_val)
            }
            (RetVal::String(left_val), RetVal::String(right_val)) => {
                RetVal::Bool(left_val == right_val)
            }
            (RetVal::Bool(left_val), RetVal::Bool(right_val)) => {
                RetVal::Bool(left_val == right_val)
            }
            (_, _) => unimplemented!(),
        }
    }

    fn evaluate_unary_expr(&mut self, operator: Token, right: AstNode) -> Result<RetVal> {
        let right_val = self.evaluate(right)?;
        match operator.token_type {
            TokenType::Minus => {
                if let RetVal::Number(n) = right_val {
                    Ok(RetVal::Number(-n))
                } else {
                    Err(Error::OperationNotSuported { operator })
                }
            }
            TokenType::Bang => Ok(self.is_not_truthy(right_val)),
            _ => {
                eprintln!("{operator:#?}");
                unimplemented!();
            }
        }
    }

    fn is_not_truthy(&self, value: RetVal) -> RetVal {
        let v = match value {
            RetVal::Bool(v) => v,
            _ => {
                if let RetVal::Bool(v) = self.is_truthy(value) {
                    v
                } else {
                    unreachable!("All RetVal should be converted into Bool by this point")
                }
            }
        };

        RetVal::Bool(!v)
    }

    fn is_truthy(&self, value: RetVal) -> RetVal {
        match value {
            RetVal::Nil => RetVal::Bool(false),
            RetVal::Bool(v) => RetVal::Bool(v),
            _ => RetVal::Bool(true),
        }
    }
}

#[cfg(test)]
mod test {
    use std::{borrow::BorrowMut, cell::RefCell};

    use super::*;

    // NOTE: this is to test output of print statements in unit tests
    #[derive(Clone, Debug)]
    struct SharedBuff(RefCell<Vec<u8>>);

    impl Write for &mut SharedBuff {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.borrow_mut().extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    #[test]
    fn test_arithmitic_expr() {
        let mut interpreter = Interpreter::new();
        let prog = AstNode::Prog(vec![]);
        let _ = interpreter.evaluate(prog);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Plus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Number(3.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Minus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Number(-1.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Slash,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Number(0.5)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::UnaryExpr {
                operator: Token {
                    token_type: TokenType::Minus,
                    line: 0,
                },
                right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            }),
            operator: Token {
                token_type: TokenType::Slash,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Number(-0.5)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Star,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Number(4.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Plus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::String("AB".into())), res);
    }

    #[test]
    fn test_logic_expr() {
        let mut interpreter = Interpreter::new();

        // EqualEqual
        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::False)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::False)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::True)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::False)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Nil)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::False)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(0.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit(
                "this is a string".into(),
            ))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        // BangEqual
        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::False)),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::False)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::True)),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::False)),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        // Greater
        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        // GreaterEqual
        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        // Less
        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        // LessEqual
        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        // Unary negate
        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::False)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Nil)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(true)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(0.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit(
                "This is a string".into(),
            ))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(RetVal::Bool(false)), res);
    }

    #[test]
    fn test_print() {
        let mut out_buf = SharedBuff(RefCell::new(Vec::<u8>::new()));
        let mut err_buf = SharedBuff(RefCell::new(Vec::<u8>::new()));
        {
            let mut interpreter = Interpreter::new_with_output(
                Box::new(out_buf.borrow_mut()),
                Box::new(err_buf.borrow_mut()),
            );

            let prog = AstNode::PrintStmt(Box::new(AstNode::Literal(LiteralExpr::StringLit(
                "This is a string".into(),
            ))));
            let res = interpreter.evaluate(prog);
            assert_eq!(Ok(RetVal::Nil), res);
        }
        let output = String::from_utf8(out_buf.0.borrow().to_vec()).expect("should be string");
        assert_eq!("This is a string".to_string(), output);
    }
}
