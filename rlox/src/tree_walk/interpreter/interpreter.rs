#![allow(dead_code)]

use std::io::Write;

use crate::lox::type_system::LoxType;
use crate::tree_walk::parser::AstNode::{self, *};
use crate::tree_walk::token::{Token, TokenType};

use super::{Error, Result};

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

    pub fn evaluate(&mut self, node: AstNode) -> Result<LoxType> {
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

    fn evaluate_prog(&mut self, stmts: Vec<AstNode>) -> Result<LoxType> {
        for stmt in stmts {
            match self.evaluate(stmt) {
                Err(err) => {
                    let msg = format!("{}", err);
                    let write_res = self.err_output.write(msg.as_bytes());
                    assert!(write_res.is_ok());
                    break;
                }
                Ok(_) => (),
            };
        }
        Ok(LoxType::Nil)
    }

    fn evaluate_print_stmt(&mut self, expr: AstNode) -> Result<LoxType> {
        let output = format!("{}", self.evaluate(expr)?);
        let write_res = self.output.write(output.as_bytes());
        assert!(write_res.is_ok());
        Ok(LoxType::Nil)
    }

    fn evaluate_binary_expr(
        &mut self,
        left: AstNode,
        operator: Token,
        right: AstNode,
    ) -> Result<LoxType> {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;
        match operator.token_type {
            TokenType::Plus => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l + r)),
                (LoxType::String(l), LoxType::String(r)) => {
                    let val = [l.as_str(), r.as_str()].concat();
                    Ok(LoxType::String(val))
                }
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::Minus => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l - r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::Slash => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l / r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::Star => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l * r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::Greater => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Bool(l > r)),
                (LoxType::String(l), LoxType::String(r)) => Ok(LoxType::Bool(l > r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator,
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::GreaterEqual => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Bool(l >= r)),
                (LoxType::String(l), LoxType::String(r)) => Ok(LoxType::Bool(l >= r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator,
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::Less => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Bool(l < r)),
                (LoxType::String(l), LoxType::String(r)) => Ok(LoxType::Bool(l < r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator,
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::LessEqual => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Bool(l <= r)),
                (LoxType::String(l), LoxType::String(r)) => Ok(LoxType::Bool(l <= r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator,
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::EqualEqual => Ok(self.is_equal(left_val, right_val)),
            TokenType::BangEqual => Ok(self.is_not_truthy(self.is_equal(left_val, right_val))),
            _ => {
                eprintln!("{operator:#?}");
                unimplemented!();
            }
        }
    }

    fn is_equal(&self, left: LoxType, right: LoxType) -> LoxType {
        match (left, right) {
            (LoxType::Nil, LoxType::Nil) => LoxType::Bool(true),
            (LoxType::Nil, _) => LoxType::Bool(false),

            (LoxType::Number(left_val), LoxType::Number(right_val)) => {
                LoxType::Bool(left_val == right_val)
            }
            (LoxType::String(left_val), LoxType::String(right_val)) => {
                LoxType::Bool(left_val == right_val)
            }
            (LoxType::Bool(left_val), LoxType::Bool(right_val)) => {
                LoxType::Bool(left_val == right_val)
            }
            (_, _) => unimplemented!(),
        }
    }

    fn evaluate_unary_expr(&mut self, operator: Token, right: AstNode) -> Result<LoxType> {
        let right_val = self.evaluate(right)?;
        match operator.token_type {
            TokenType::Minus => {
                if let LoxType::Number(n) = right_val {
                    Ok(LoxType::Number(-n))
                } else {
                    Err(Error::OperationNotSuported {
                        operator,
                        values: (right_val, None),
                    })
                }
            }
            TokenType::Bang => Ok(self.is_not_truthy(right_val)),
            _ => {
                eprintln!("{operator:#?}");
                unimplemented!();
            }
        }
    }

    fn is_not_truthy(&self, value: LoxType) -> LoxType {
        let v = match value {
            LoxType::Bool(v) => v,
            _ => {
                if let LoxType::Bool(v) = self.is_truthy(value) {
                    v
                } else {
                    unreachable!("All RetVal should be converted into Bool by this point")
                }
            }
        };

        LoxType::Bool(!v)
    }

    fn is_truthy(&self, value: LoxType) -> LoxType {
        match value {
            LoxType::Nil => LoxType::Bool(false),
            LoxType::Bool(v) => LoxType::Bool(v),
            _ => LoxType::Bool(true),
        }
    }
}

#[cfg(test)]
mod test {
    // TODO: add testing of errors
    use std::{borrow::BorrowMut, cell::RefCell};

    use super::*;
    use crate::tree_walk::parser::LiteralExpr;

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
        assert_eq!(Ok(LoxType::Number(3.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Minus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Number(-1.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Slash,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Number(0.5)), res);

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
        assert_eq!(Ok(LoxType::Number(-0.5)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Star,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Number(4.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Plus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        }));
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::String("AB".into())), res);
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
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::True)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::False)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Nil)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::False)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(0.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

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
        assert_eq!(Ok(LoxType::Bool(false)), res);

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
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::True)),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::False)),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

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
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

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
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

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
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

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
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        // Unary negate
        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::False)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Nil)),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(0.0))),
        };
        let res = interpreter.evaluate(prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

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
        assert_eq!(Ok(LoxType::Bool(false)), res);
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
            assert_eq!(Ok(LoxType::Nil), res);
        }
        let output = String::from_utf8(out_buf.0.borrow().to_vec()).expect("should be string");
        assert_eq!("This is a string".to_string(), output);
    }
}
