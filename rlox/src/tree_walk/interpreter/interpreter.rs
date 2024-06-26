use std::borrow::BorrowMut;
use std::io::Write;

use crate::lox::LoxType;
use crate::tree_walk::parser::AstNode::{self, *};
use crate::tree_walk::token::{Token, TokenType};

use super::state::Environment;
use super::{Builtin, Callable, Error, Result};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter<'a> {
    // these enable redirecting output so that print can be tested
    output: Box<(dyn Write + 'a)>,
    err_output: Box<(dyn Write + 'a)>,

    environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self::new_with_output(Box::new(std::io::stdout()), Box::new(std::io::stderr()))
    }

    pub fn new_with_output(out: Box<dyn Write + 'a>, err_out: Box<dyn Write + 'a>) -> Self {
        let mut global_env = Environment::new();
        global_env
            .borrow_mut()
            .put("time".into(), LoxType::Builtin(Builtin::Time));
        let globals = Rc::new(RefCell::new(global_env));

        Self {
            output: out,
            err_output: err_out,
            environment: globals.clone(),
            globals,
        }
    }

    pub fn evaluate(&mut self, node: &AstNode) -> Result<LoxType> {
        match node {
            // NOTE: statements returning values does not make sense, but I
            // will need to rewrite test to change
            Prog(stmts) => self.evaluate_prog(&stmts),
            DeclStmt { identifier, expr } => {
                self.evaluate_var_decl_stmt(identifier, expr.as_deref())
            }
            IfStmt {
                condition,
                then_stmt,
                else_stmt,
            } => self.evaluate_if_stmt(condition, then_stmt, else_stmt.as_deref()),
            FuncStmt {
                name,
                parameters,
                body,
            } => self.evaluate_fun_decl(name, parameters.as_ref(), body),
            PrintStmt(expr) => self.evaluate_print_stmt(expr),
            WhileStmt { condition, body } => self.evaluate_while_stmt(condition, body),
            Block(exprs) => self.evaluate_block(exprs),
            ExprStmt(stmt) => self.evaluate(stmt),
            Assign { target, value } => self.evaluate_assign_expr(target, value),
            LogicalExpr {
                left,
                operator,
                right,
            } => self.evaluate_logical_expr(left, operator, right),
            BinaryExpr {
                left,
                operator,
                right,
            } => self.evaluate_binary_expr(left, operator, right),
            UnaryExpr { operator, right } => self.evaluate_unary_expr(operator, right),
            CallExpr {
                calle,
                paren,
                arguments,
            } => self.evaluate_call_expr(calle, paren, &arguments),
            Literal(expr) => Ok(expr.into()),
            Variable(token) => self.evaluate_variable_expr(token),
            Grouping(expr) => self.evaluate(expr),
            _ => {
                unimplemented!("Evaluating AstNode: {node:#?}");
            }
        }
    }

    pub fn get_globals(&self) -> Rc<RefCell<Environment>> {
        self.globals.clone()
    }

    pub fn evaluate_with_env(
        &mut self,
        exprs: &[AstNode],
        env: Rc<RefCell<Environment>>,
    ) -> Result<LoxType> {
        let previous = self.environment.clone();
        self.environment = env;

        let mut last_expr_res = Ok(LoxType::Nil);
        for expr in exprs.iter() {
            last_expr_res = self.evaluate(expr);
            if matches!(last_expr_res, Err(_)) {
                break;
            }
        }

        self.environment = previous;
        match last_expr_res {
            Err(e) => Err(e),
            _ => Ok(LoxType::Nil),
        }
    }

    fn evaluate_prog(&mut self, stmts: &[AstNode]) -> Result<LoxType> {
        for stmt in stmts.iter() {
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

    fn evaluate_var_decl_stmt(
        &mut self,
        identifier: &Token,
        expr: Option<&AstNode>,
    ) -> Result<LoxType> {
        let name = match &identifier.token_type {
            TokenType::Identifier(name) => name,
            // this should only happen if I'm an idot
            _ => return Err(Error::TokenIsNotAnIdenifier(identifier.clone())),
        };
        let expr_val = match expr {
            Some(expr) => self.evaluate(expr)?,
            _ => LoxType::Nil,
        };
        let env_ref = &mut *self.environment.borrow_mut();
        let mut env = env_ref.as_ref().borrow_mut();
        env.put(name.clone(), expr_val.clone());
        Ok(expr_val)
    }

    fn evaluate_if_stmt(
        &mut self,
        condition: &AstNode,
        then_stmt: &AstNode,
        else_stmt: Option<&AstNode>,
    ) -> Result<LoxType> {
        let cond = self.evaluate(condition)?;
        match self.is_truthy(cond) {
            LoxType::Bool(cond) => match cond {
                true => self.evaluate(then_stmt),
                false => match else_stmt {
                    Some(stmt) => self.evaluate(stmt),
                    _ => Ok(LoxType::Nil),
                },
            },
            _ => unreachable!("is_truthy() should only return LoxType::Bool()"),
        }
    }

    fn evaluate_fun_decl(
        &mut self,
        identifier: &Token,
        parameters: &[Token],
        body: &AstNode,
    ) -> Result<LoxType> {
        let name = match &identifier.token_type {
            TokenType::Identifier(name) => Ok(name.clone()),
            _ => Err(Error::TokenIsNotAnIdenifier(identifier.clone())),
        }?;
        let params = parameters
            .iter()
            .map(|param| match &param.token_type {
                TokenType::Identifier(name) => Ok(name.to_string()),
                _ => Err(Error::TokenIsNotAnIdenifier(param.clone())),
            })
            .collect::<Result<Vec<_>>>()?;
        let func = LoxType::Function {
            parameters: params.into(),
            body: body.clone(),
        };

        let env_ref = &mut *self.environment.borrow_mut();
        let mut env = env_ref.as_ref().borrow_mut();
        env.put(name, func.clone());
        Ok(LoxType::Nil)
    }

    fn evaluate_print_stmt(&mut self, expr: &AstNode) -> Result<LoxType> {
        let output = format!("{}\n", self.evaluate(expr)?);
        let write_res = self.output.write(output.as_bytes());
        assert!(write_res.is_ok());
        Ok(LoxType::Nil)
    }

    fn evaluate_while_stmt(&mut self, condition: &AstNode, stmt: &AstNode) -> Result<LoxType> {
        while self.evaluate(condition)?.into() {
            self.evaluate(stmt)?;
        }
        Ok(LoxType::Nil)
    }

    fn evaluate_block(&mut self, exprs: &[AstNode]) -> Result<LoxType> {
        let env = Rc::new(RefCell::new(Environment::new_sub_envoronment(
            self.environment.clone(),
        )));

        self.evaluate_with_env(exprs, env)
    }

    fn evaluate_assign_expr(&mut self, target: &AstNode, value: &AstNode) -> Result<LoxType> {
        let res = self.evaluate(value)?;
        match target {
            Variable(name_token) => match &name_token.token_type {
                TokenType::Identifier(name) => {
                    let env_ref = &mut *self.environment.borrow_mut();
                    let mut env = env_ref.as_ref().borrow_mut();

                    env.assign(&name, res.clone())?;
                    Ok(res)
                }
                _ => Err(Error::TokenIsNotAnIdenifier(name_token.clone())),
            },
            _ => unreachable!("Assign should only exist with a variable as target"),
        }
    }

    fn evaluate_logical_expr(
        &mut self,
        left: &AstNode,
        operator: &Token,
        right: &AstNode,
    ) -> Result<LoxType> {
        let left_val = self.evaluate(left)?;
        let truthy = left_val.clone().into();

        match operator.token_type {
            TokenType::Or if truthy => Ok(left_val),
            TokenType::And if !truthy => Ok(left_val),
            _ => Ok(self.evaluate(right)?),
        }
    }

    fn evaluate_binary_expr(
        &mut self,
        left: &AstNode,
        operator: &Token,
        right: &AstNode,
    ) -> Result<LoxType> {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;
        match operator.token_type {
            TokenType::Plus => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Number(l + r)),
                (LoxType::String(l), LoxType::String(r)) => {
                    let val = [l, r].concat();
                    Ok(LoxType::String(val.into()))
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
                    operator: operator.clone(),
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::GreaterEqual => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Bool(l >= r)),
                (LoxType::String(l), LoxType::String(r)) => Ok(LoxType::Bool(l >= r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::Less => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Bool(l < r)),
                (LoxType::String(l), LoxType::String(r)) => Ok(LoxType::Bool(l < r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
                    values: (left_val, Some(right_val)),
                }),
            },
            TokenType::LessEqual => match (left_val, right_val) {
                (LoxType::Number(l), LoxType::Number(r)) => Ok(LoxType::Bool(l <= r)),
                (LoxType::String(l), LoxType::String(r)) => Ok(LoxType::Bool(l <= r)),
                (left_val, right_val) => Err(Error::OperationNotSuported {
                    operator: operator.clone(),
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

    fn is_not_truthy(&self, value: LoxType) -> LoxType {
        let v: bool = value.into();
        LoxType::Bool(!v)
    }

    fn is_truthy(&self, value: LoxType) -> LoxType {
        LoxType::Bool(value.into())
    }

    fn evaluate_unary_expr(&mut self, operator: &Token, right: &AstNode) -> Result<LoxType> {
        let right_val = self.evaluate(right)?;
        match operator.token_type {
            TokenType::Minus => {
                if let LoxType::Number(n) = right_val {
                    Ok(LoxType::Number(-n))
                } else {
                    Err(Error::OperationNotSuported {
                        operator: operator.clone(),
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

    fn evaluate_call_expr(
        &mut self,
        callee: &AstNode,
        paren: &Token,
        argumnets: &[AstNode],
    ) -> Result<LoxType> {
        let callee_val = self.evaluate(callee)?;

        let args = argumnets
            .iter()
            .map(|arg| self.evaluate(arg))
            .collect::<Result<Vec<_>>>()?;

        match callee_val.call(self, args.into()) {
            Err(Error::InvalidArity {
                line: None,
                expected,
                received,
            }) => Err(Error::InvalidArity {
                // NOTE: I SHOULD HAVE USED usize FROM THE START!
                line: Some(paren.line.try_into().unwrap()),
                expected,
                received,
            }),
            Err(Error::NotCallable { line: None }) => Err(Error::NotCallable {
                line: Some(paren.line.try_into().unwrap()),
            }),
            ret => ret,
        }
    }

    fn evaluate_variable_expr(&mut self, token: &Token) -> Result<LoxType> {
        match &token.token_type {
            TokenType::Identifier(name) => {
                let env_ref = &mut *self.environment.borrow_mut();
                let mut env = env_ref.as_ref().borrow_mut();
                env.get(&name)
            }
            _ => unreachable!("Variable expresion should always be an identifier."),
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
        let prog = AstNode::Prog(Box::new([]));
        let _ = interpreter.evaluate(&prog);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Plus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Number(3.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Minus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Number(-1.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Slash,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Number(-0.5)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Star,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Number(4.0)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Plus,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        }));
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::True)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::False)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Nil)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::False)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            operator: Token {
                token_type: TokenType::EqualEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(0.0))),
        };
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::True)),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::False)),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::BangEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Greater,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::GreaterEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::Less,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::StringLit("B".into()))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::StringLit("A".into()))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::BinaryExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            operator: Token {
                token_type: TokenType::LessEqual,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        // Unary negate
        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::False)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(false)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Nil)),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::UnaryExpr {
            operator: Token {
                token_type: TokenType::Bang,
                line: 0,
            },
            right: Box::new(AstNode::Literal(LiteralExpr::Number(0.0))),
        };
        let res = interpreter.evaluate(&prog);
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
        let res = interpreter.evaluate(&prog);
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
            let res = interpreter.evaluate(&prog);
            assert_eq!(Ok(LoxType::Nil), res);
        }
        let output = String::from_utf8(out_buf.0.borrow().to_vec()).expect("should be string");
        assert_eq!("This is a string\n".to_string(), output);
    }

    #[test]
    fn test_var_decl_stmt() {
        let mut interpreter = Interpreter::new();

        let prog = AstNode::DeclStmt {
            identifier: Token {
                token_type: TokenType::Identifier("test".into()),
                line: 0,
            },
            expr: Some(Box::new(Literal(LiteralExpr::Number(1.0)))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Number(1.0)), res);
        assert_eq!(Some(LoxType::Number(1.0)), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("test").cloned()
        });

        let prog = AstNode::DeclStmt {
            identifier: Token {
                token_type: TokenType::Identifier("test".into()),
                line: 0,
            },
            expr: None,
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);
        assert_eq!(Some(LoxType::Nil), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("test").cloned()
        });
    }

    #[test]
    fn test_assign() {
        let mut interpreter = Interpreter::new();

        let prog = AstNode::DeclStmt {
            identifier: Token {
                token_type: TokenType::Identifier("test".into()),
                line: 0,
            },
            expr: None,
        };
        let _ = interpreter.evaluate(&prog);
        assert_eq!(Some(LoxType::Nil), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("test").cloned()
        });

        let prog = AstNode::Assign {
            target: Box::new(AstNode::Variable(Token {
                token_type: TokenType::Identifier("not_valid".into()),
                line: 0,
            })),
            value: Box::new(Literal(LiteralExpr::Number(1.0))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Err(Error::UndifinedVariable("not_valid".into())), res);

        let prog = AstNode::Assign {
            target: Box::new(AstNode::Variable(Token {
                token_type: TokenType::Identifier("test".into()),
                line: 0,
            })),
            value: Box::new(Literal(LiteralExpr::Number(1.0))),
        };
        let _ = interpreter.evaluate(&prog);
        assert_eq!(Some(LoxType::Number(1.0)), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("test").cloned()
        });
    }

    #[test]
    fn test_variable_expr() {
        let mut out_buf = SharedBuff(RefCell::new(Vec::<u8>::new()));
        let mut err_buf = SharedBuff(RefCell::new(Vec::<u8>::new()));
        {
            let mut interpreter = Interpreter::new_with_output(
                Box::new(out_buf.borrow_mut()),
                Box::new(err_buf.borrow_mut()),
            );

            let prog = AstNode::PrintStmt(Box::new(AstNode::Variable(Token {
                token_type: TokenType::Identifier("test".into()),
                line: 0,
            })));
            let res = interpreter.evaluate(&prog);
            assert_eq!(Err(Error::UndifinedVariable("test".into())), res);
            assert_eq!(None, {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("test").cloned()
            });

            let prog = AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    token_type: TokenType::Identifier("test".into()),
                    line: 0,
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::StringLit(
                    "should be a string".into(),
                ))),
            };
            let res = interpreter.evaluate(&prog);
            assert_eq!(Err(Error::UndifinedVariable("test".into())), res);
            assert_eq!(None, {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("test").cloned()
            });

            let prog = AstNode::DeclStmt {
                identifier: Token {
                    token_type: TokenType::Identifier("test".into()),
                    line: 0,
                },
                expr: Some(Box::new(AstNode::Literal(LiteralExpr::StringLit(
                    "should be a string".into(),
                )))),
            };
            let res = interpreter.evaluate(&prog);
            assert_eq!(Ok(LoxType::String("should be a string".into())), res);
            assert_eq!(Some(LoxType::String("should be a string".into())), {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("test").cloned()
            });

            let prog = AstNode::PrintStmt(Box::new(AstNode::Variable(Token {
                token_type: TokenType::Identifier("test".into()),
                line: 0,
            })));
            let _res = interpreter.evaluate(&prog);
        }
        let output = String::from_utf8(out_buf.0.borrow().to_vec()).expect("should be string");
        assert_eq!("should be a string\n".to_string(), output);

        out_buf.0.borrow_mut().clear();

        {
            let mut interpreter = Interpreter::new_with_output(
                Box::new(out_buf.borrow_mut()),
                Box::new(err_buf.borrow_mut()),
            );
            let prog = AstNode::Prog(Box::new([
                AstNode::DeclStmt {
                    identifier: Token {
                        token_type: TokenType::Identifier("a".into()),
                        line: 0,
                    },
                    expr: None,
                },
                AstNode::DeclStmt {
                    identifier: Token {
                        token_type: TokenType::Identifier("b".into()),
                        line: 1,
                    },
                    expr: None,
                },
            ]));
            let _res = interpreter.evaluate(&prog);
            assert_eq!(Some(LoxType::Nil), {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("a").cloned()
            });
            assert_eq!(Some(LoxType::Nil), {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("b").cloned()
            });

            let prog = PrintStmt(Box::new(Assign {
                target: Box::new(Variable(Token {
                    token_type: TokenType::Identifier("a".into()),
                    line: 0,
                })),
                value: Box::new(Assign {
                    target: Box::new(Variable(Token {
                        token_type: TokenType::Identifier("b".into()),
                        line: 0,
                    })),
                    value: Box::new(Literal(LiteralExpr::Number(1.0))),
                }),
            }));
            let _res = interpreter.evaluate(&prog);
            assert_eq!(Some(LoxType::Number(1.0)), {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("a").cloned()
            });
            assert_eq!(Some(LoxType::Number(1.0)), {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("b").cloned()
            });
        }
        let output = String::from_utf8(out_buf.0.borrow().to_vec()).expect("should be string");
        assert_eq!("1\n".to_string(), output);
    }

    #[test]
    fn test_block_stmt() {
        let mut interpreter = Interpreter::new();

        let prog = AstNode::Block(Box::new([]));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);

        let prog = AstNode::Block(Box::new([AstNode::DeclStmt {
            identifier: Token {
                line: 1,
                token_type: TokenType::Identifier("a".into()),
            },
            expr: Some(Box::new(AstNode::Literal(LiteralExpr::Number(1.0)))),
        }]));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);
        assert_eq!(None, {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("a").cloned()
        });

        let prog = AstNode::DeclStmt {
            identifier: Token {
                line: 0,
                token_type: TokenType::Identifier("a".into()),
            },
            expr: Some(Box::new(AstNode::Literal(LiteralExpr::Number(1.0)))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Number(1.0)), res);
        assert_eq!(Some(LoxType::Number(1.0)), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("a").cloned()
        });

        let prog = AstNode::Block(Box::new([AstNode::Assign {
            target: Box::new(AstNode::Variable(Token {
                token_type: TokenType::Identifier("a".into()),
                line: 2,
            })),
            value: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
        }]));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);
        assert_eq!(Some(LoxType::Number(2.0)), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("a").cloned()
        });

        let prog = AstNode::Block(Box::new([
            AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    token_type: TokenType::Identifier("b".into()),
                    line: 2,
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::Number(3.0))),
            },
            AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    token_type: TokenType::Identifier("a".into()),
                    line: 3,
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::Number(3.0))),
            },
        ]));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Err(Error::UndifinedVariable("b".into())), res);
        assert_eq!(Some(LoxType::Number(2.0)), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("a").cloned()
        });
    }

    #[test]
    fn test_if_stmt() {
        let mut interpreter = Interpreter::new();

        let prog = AstNode::Prog(Box::new([AstNode::DeclStmt {
            identifier: Token {
                line: 0,
                token_type: TokenType::Identifier("a".into()),
            },
            expr: None,
        }]));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);
        assert_eq!(Some(LoxType::Nil), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("a").cloned()
        });

        let prog = AstNode::Prog(Box::new([AstNode::IfStmt {
            condition: Box::new(AstNode::Literal(LiteralExpr::True)),
            then_stmt: Box::new(AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    line: 1,
                    token_type: TokenType::Identifier("a".into()),
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            }),
            else_stmt: Some(Box::new(AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    line: 1,
                    token_type: TokenType::Identifier("a".into()),
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            })),
        }]));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);
        assert_eq!(Some(LoxType::Number(1.0)), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("a").cloned()
        });

        let prog = AstNode::Prog(Box::new([AstNode::IfStmt {
            condition: Box::new(AstNode::Literal(LiteralExpr::False)),
            then_stmt: Box::new(AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    line: 1,
                    token_type: TokenType::Identifier("a".into()),
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
            }),
            else_stmt: Some(Box::new(AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    line: 1,
                    token_type: TokenType::Identifier("a".into()),
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
            })),
        }]));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);
        assert_eq!(Some(LoxType::Number(2.0)), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("a").cloned()
        });

        let prog = AstNode::Prog(Box::new([
            AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    line: 1,
                    token_type: TokenType::Identifier("a".into()),
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::Nil)),
            },
            AstNode::IfStmt {
                condition: Box::new(AstNode::Literal(LiteralExpr::False)),
                then_stmt: Box::new(AstNode::Assign {
                    target: Box::new(AstNode::Variable(Token {
                        line: 1,
                        token_type: TokenType::Identifier("a".into()),
                    })),
                    value: Box::new(AstNode::Literal(LiteralExpr::Number(1.0))),
                }),
                else_stmt: None,
            },
        ]));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);
        assert_eq!(Some(LoxType::Nil), {
            let env_ref = &mut *interpreter.environment.borrow_mut();
            let env = env_ref.as_ref().borrow_mut();
            env.state.get("a").cloned()
        });
    }

    #[test]
    fn test_logical_expr() {
        let mut interpreter = Interpreter::new();

        let prog = AstNode::ExprStmt(Box::new(AstNode::LogicalExpr {
            left: Box::new(AstNode::Literal(LiteralExpr::True)),
            operator: Token {
                line: 0,
                token_type: TokenType::Or,
            },
            right: Box::new(AstNode::LogicalExpr {
                left: Box::new(AstNode::Literal(LiteralExpr::True)),
                operator: Token {
                    line: 0,
                    token_type: TokenType::And,
                },
                right: Box::new(AstNode::Literal(LiteralExpr::True)),
            }),
        }));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);

        let prog = AstNode::ExprStmt(Box::new(AstNode::LogicalExpr {
            right: Box::new(AstNode::Literal(LiteralExpr::True)),
            operator: Token {
                line: 0,
                token_type: TokenType::Or,
            },
            left: Box::new(AstNode::LogicalExpr {
                left: Box::new(AstNode::Literal(LiteralExpr::True)),
                operator: Token {
                    line: 0,
                    token_type: TokenType::And,
                },
                right: Box::new(AstNode::Literal(LiteralExpr::True)),
            }),
        }));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Bool(true)), res);
    }

    #[test]
    fn test_while_stmt() {
        let mut out_buf = SharedBuff(RefCell::new(Vec::<u8>::new()));
        let mut err_buf = SharedBuff(RefCell::new(Vec::<u8>::new()));
        {
            let mut interpreter = Interpreter::new_with_output(
                Box::new(out_buf.borrow_mut()),
                Box::new(err_buf.borrow_mut()),
            );

            let prog = DeclStmt {
                identifier: Token {
                    token_type: TokenType::Identifier("i".into()),
                    line: 0,
                },
                expr: Some(Box::new(Literal(LiteralExpr::Number(0.0)))),
            };
            let _res = interpreter.evaluate(&prog);
            assert_eq!(Some(LoxType::Number(0.0)), {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("i").cloned()
            });

            let prog = WhileStmt {
                condition: Box::new(BinaryExpr {
                    left: Box::new(Variable(Token {
                        token_type: TokenType::Identifier("i".into()),
                        line: 0,
                    })),
                    operator: Token {
                        token_type: TokenType::Less,
                        line: 0,
                    },
                    right: Box::new(Literal(LiteralExpr::Number(4.0))),
                }),
                body: Box::new(Block(Box::new([
                    PrintStmt(Box::new(Variable(Token {
                        token_type: TokenType::Identifier("i".into()),
                        line: 1,
                    }))),
                    Assign {
                        target: Box::new(Variable(Token {
                            token_type: TokenType::Identifier("i".into()),
                            line: 1,
                        })),
                        value: Box::new(BinaryExpr {
                            left: Box::new(Variable(Token {
                                token_type: TokenType::Identifier("i".into()),
                                line: 1,
                            })),
                            operator: Token {
                                token_type: TokenType::Plus,
                                line: 1,
                            },
                            right: Box::new(Literal(LiteralExpr::Number(1.0))),
                        }),
                    },
                ]))),
            };
            let _res = interpreter.evaluate(&prog);
            assert_eq!(Some(LoxType::Number(4.0)), {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("i").cloned()
            });
        }
        let output = String::from_utf8(out_buf.0.borrow().to_vec()).expect("should be string");
        assert_eq!("0\n1\n2\n3\n".to_string(), output);
    }

    #[test]
    fn test_call() {
        let mut interpreter = Interpreter::new();

        let prog = ExprStmt(Box::new(CallExpr {
            calle: Box::new(Variable(Token {
                token_type: TokenType::Identifier("f".into()),
                line: 0,
            })),
            paren: Token {
                token_type: TokenType::LeftParen,
                line: 0,
            },
            arguments: Box::new([]),
        }));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Err(Error::UndifinedVariable("f".into())), res);

        let prog = DeclStmt {
            identifier: Token {
                token_type: TokenType::Identifier("f".into()),
                line: 0,
            },
            expr: Some(Box::new(Literal(LiteralExpr::Number(1.0)))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Number(1.0)), res);

        let prog = ExprStmt(Box::new(CallExpr {
            calle: Box::new(Variable(Token {
                token_type: TokenType::Identifier("f".into()),
                line: 0,
            })),
            paren: Token {
                token_type: TokenType::LeftParen,
                line: 0,
            },
            arguments: Box::new([]),
        }));
        let res = interpreter.evaluate(&prog);
        assert_eq!(Err(Error::NotCallable { line: Some(0) }), res);

        let prog = ExprStmt(Box::new(CallExpr {
            calle: Box::new(Variable(Token {
                token_type: TokenType::Identifier("time".into()),
                line: 0,
            })),
            paren: Token {
                token_type: TokenType::LeftParen,
                line: 0,
            },
            arguments: Box::new([]),
        }));
        let res = interpreter.evaluate(&prog);
        assert!(res.is_ok());
        assert!(matches!(res.as_ref().unwrap(), LoxType::Number(_)));
        if let Ok(LoxType::Number(x)) = res {
            assert!(x > 0.0);
        }
    }

    #[test]
    fn test_function_declaration() {
        let mut interpreter = Interpreter::new();

        let prog = FuncStmt {
            name: Token {
                token_type: TokenType::Identifier("f".into()),
                line: 0,
            },
            parameters: Box::new([Token {
                token_type: TokenType::Identifier("x".into()),
                line: 0,
            }]),
            body: Box::new(Block(Box::new([]))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(Ok(LoxType::Nil), res);
        assert_eq!(
            Some(LoxType::Function {
                parameters: Box::new(["x".into()]),
                body: Block(Box::new([])),
            }),
            {
                let env_ref = &mut *interpreter.environment.borrow_mut();
                let env = env_ref.as_ref().borrow_mut();
                env.state.get("f").cloned()
            }
        );

        let prog = FuncStmt {
            name: Token {
                token_type: TokenType::Comma,
                line: 0,
            },
            parameters: Box::new([Token {
                token_type: TokenType::Identifier("x".into()),
                line: 0,
            }]),
            body: Box::new(Block(Box::new([]))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(
            Err(Error::TokenIsNotAnIdenifier(Token {
                token_type: TokenType::Comma,
                line: 0,
            })),
            res
        );

        let prog = FuncStmt {
            name: Token {
                token_type: TokenType::Identifier("x".into()),
                line: 0,
            },
            parameters: Box::new([Token {
                token_type: TokenType::For,
                line: 0,
            }]),
            body: Box::new(Block(Box::new([]))),
        };
        let res = interpreter.evaluate(&prog);
        assert_eq!(
            Err(Error::TokenIsNotAnIdenifier(Token {
                token_type: TokenType::For,
                line: 0,
            })),
            res
        );
    }
}
