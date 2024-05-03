use super::{Error, Result};
use crate::tree_walk::token::{Token, TokenType::*};

macro_rules! expect {
    ($self:expr, $($token:pat)+, $err:expr) => {{
        let token = $self.peek().ok_or(Error::RanOutOfTokens)?;
        match token.token_type {
            $($token => Ok($self.advance()),)+
            _ => Err($err),
        }?
    }};
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Prog(Box<[AstNode]>),
    ProgInvalid {
        stmts: Box<[AstNode]>,
        errors: Box<[Error]>,
    },
    DeclStmt {
        identifier: Token,
        expr: Option<Box<AstNode>>,
    },
    IfStmt {
        condition: Box<AstNode>,
        then_stmt: Box<AstNode>,
        else_stmt: Option<Box<AstNode>>,
    },
    PrintStmt(Box<AstNode>),
    WhileStmt {
        condition: Box<AstNode>,
        body: Box<AstNode>,
    },
    Block(Box<[AstNode]>),
    ExprStmt(Box<AstNode>),
    Assign {
        target: Box<AstNode>,
        value: Box<AstNode>,
    },
    LogicalExpr {
        left: Box<AstNode>,
        operator: Token,
        right: Box<AstNode>,
    },
    BinaryExpr {
        left: Box<AstNode>,
        operator: Token,
        right: Box<AstNode>,
    },
    UnaryExpr {
        operator: Token,
        right: Box<AstNode>,
    },
    CallExpr {
        calle: Box<AstNode>,
        paren: Token,
        arguments: Option<Box<[AstNode]>>,
    },
    Literal(LiteralExpr),
    Variable(Token),
    Grouping(Box<AstNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    False,
    True,
    Nil,
    Number(f64),
    StringLit(Box<str>),
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> Result<Token> {
        self.current += 1;
        self.tokens
            .get(self.current - 1)
            .ok_or(Error::RanOutOfTokens)
            .cloned()
    }

    fn previous(&mut self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn report_error(error: &Error) {
        eprintln!("{}", error);
    }

    fn expect_semicolon(
        &mut self,
        token: Token,
        preceding: Option<std::string::String>,
        ret_val: Result<AstNode>,
    ) -> Result<AstNode> {
        let next = self.advance()?;
        match next.token_type {
            Semicolon => ret_val,
            _ => Err(Error::ExpectedSemicolon {
                line: token.line,
                preceding: if let Some(text) = preceding {
                    text
                } else {
                    token.token_type.to_string()
                },
            }),
        }
    }

    fn syncronize(&mut self) {
        while let Some(token) = self.peek() {
            match token.token_type {
                Class | For | Fun | If | Print | Return | Var | While | EOF => break,
                Semicolon => {
                    let _ = self.advance();
                    break;
                }
                _ => {
                    let _ = self.advance();
                }
            }
        }
    }

    pub fn parse(&mut self) -> AstNode {
        self.prog()
    }

    fn prog(&mut self) -> AstNode {
        let mut stmts = Vec::new();
        let mut errors = Vec::new();
        let mut invalid = false;

        while let Some(token) = self.peek() {
            match token.token_type {
                EOF => break,
                _ => match self.decleration() {
                    Err(e) => {
                        Self::report_error(&e);
                        errors.push(e);
                        self.syncronize();
                        invalid = true;
                    }
                    Ok(stmt) => stmts.push(stmt),
                },
            }
        }

        match invalid {
            false => AstNode::Prog(stmts.into()),
            true => AstNode::ProgInvalid {
                stmts: stmts.into(),
                errors: errors.into(),
            },
        }
    }

    fn decleration(&mut self) -> Result<AstNode> {
        let token = self.peek().ok_or(Error::RanOutOfTokens)?;
        match token.token_type {
            Var => self.var_decleration(),
            _ => self.statement(),
        }
    }

    fn var_decleration(&mut self) -> Result<AstNode> {
        let var_token = self.advance()?;
        let identifier = expect!(
            self,
            Identifier(_),
            Error::VarExpectedIdentifer(var_token.clone())
        )?;
        let next = self.peek();
        let expr = match next {
            Some(token) if token.token_type == Equal => {
                let _ = self.advance();
                Some(Box::new(self.expression()?))
            }
            _ => None,
        };

        self.expect_semicolon(var_token, None, Ok(AstNode::DeclStmt { identifier, expr }))
    }

    fn statement(&mut self) -> Result<AstNode> {
        let token = self.peek().ok_or(Error::RanOutOfTokens)?;
        match token.token_type {
            If => self.if_statemtnt(),
            For => self.for_statement(),
            Print => self.print_statement(),
            While => self.while_statement(),
            LeftBrace => self.block(),
            _ => self.expr_statement(),
        }
    }

    fn print_statement(&mut self) -> Result<AstNode> {
        let print_token = self.advance()?;
        let expr = self.expression()?;

        let _ = expect!(
            self,
            Semicolon,
            Error::ExpectedSemicolon {
                line: print_token.line,
                preceding: print_token.token_type.to_string()
            }
        )?;
        Ok(AstNode::PrintStmt(Box::new(expr)))
    }

    fn while_statement(&mut self) -> Result<AstNode> {
        let while_token = self.advance()?;

        let _ = expect!(
            self,
            LeftParen,
            Error::ExpectedOpeningParen(while_token.clone())
        )?;
        let condition = Box::new(self.expression()?);
        let _ = expect!(
            self,
            RightParen,
            Error::ExpectedClosingParen(while_token.clone())
        )?;

        let body = Box::new(self.statement()?);

        Ok(AstNode::WhileStmt { condition, body })
    }

    fn block(&mut self) -> Result<AstNode> {
        let open_brace = self.advance()?;
        let mut stmts = Vec::new();

        while let Some(next) = self.peek() {
            match next.token_type {
                RightBrace | EOF => break,
                _ => stmts.push(self.decleration()?),
            }
        }

        let _ = expect!(self, RightBrace, Error::ExpectedClosingBrace(open_brace))?;
        Ok(AstNode::Block(stmts.into()))
    }

    fn if_statemtnt(&mut self) -> Result<AstNode> {
        let if_token = self.advance()?;

        let _ = expect!(
            self,
            LeftParen,
            Error::ExpectedOpeningParen(if_token.clone())
        )?;
        let condition = Box::new(self.expression()?);
        let _ = expect!(self, RightParen, Error::ExpectedClosingParen(if_token))?;

        let then_stmt = Box::new(self.statement()?);

        let next = self.peek().ok_or(Error::RanOutOfTokens)?;
        let else_stmt = match next.token_type {
            Else => {
                let _ = self.advance();
                Some(Box::new(self.statement()?))
            }
            _ => None,
        };

        Ok(AstNode::IfStmt {
            condition,
            then_stmt,
            else_stmt,
        })
    }

    fn for_statement(&mut self) -> Result<AstNode> {
        let for_token = self.advance()?;
        let _ = expect!(
            self,
            LeftParen,
            Error::ExpectedOpeningParen(for_token.clone())
        )?;
        let next = self.peek().ok_or(Error::RanOutOfTokens)?;
        let initializer = match next.token_type {
            Semicolon => {
                self.advance()?;
                None
            }
            Var => Some(self.var_decleration()?),
            _ => Some(self.expr_statement()?),
        };

        let next = self.peek().ok_or(Error::RanOutOfTokens)?;
        let cond_expr = match next.token_type {
            Semicolon => None,
            _ => Some(self.expression()?),
        };
        let _ = expect!(
            self,
            Semicolon,
            Error::ExpectedSemicolon {
                line: for_token.line,
                preceding: for_token.token_type.to_string()
            }
        );

        let next = self.peek().ok_or(Error::RanOutOfTokens)?;
        let increment = match next.token_type {
            RightParen => None,
            _ => Some(self.expression()?),
        };
        let _ = expect!(self, RightParen, Error::ExpectedClosingParen(for_token))?;

        let mut body = self.statement()?;

        body = match increment {
            Some(stmt) => AstNode::Block(Box::new([body, stmt])),
            _ => body,
        };

        let condition = match cond_expr {
            Some(expr) => expr,
            None => AstNode::Literal(LiteralExpr::True),
        };

        let while_stmt = AstNode::WhileStmt {
            condition: Box::new(condition),
            body: Box::new(body),
        };

        match initializer {
            Some(stmt) => Ok(AstNode::Block(Box::new([stmt, while_stmt]))),
            None => Ok(while_stmt),
        }
    }

    fn expr_statement(&mut self) -> Result<AstNode> {
        let expr = self.expression()?;
        let next = self.peek().cloned();
        match next {
            None => Err(Error::RanOutOfTokens),
            Some(token) if matches!(token.token_type, EOF) => Err(Error::UnexpectedEOF),
            Some(token) => {
                let _ = expect!(
                    self,
                    Semicolon,
                    Error::ExpectedSemicolon {
                        line: token.line,
                        preceding: "Expression".into()
                    }
                )?;
                Ok(AstNode::ExprStmt(Box::new(expr)))
            }
        }
    }

    fn expression(&mut self) -> Result<AstNode> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<AstNode> {
        let expr = self.logic_or()?;

        match self.peek() {
            Some(token) if matches!(token.token_type, Equal) => {
                let equal = self.advance()?;
                let value = self.assignment()?;

                match &expr {
                    AstNode::Variable(_) => Ok(AstNode::Assign {
                        target: Box::new(expr),
                        value: Box::new(value),
                    }),
                    _ => Err(Error::InvalidAssignmentTarget(equal)),
                }
            }
            _ => Ok(expr),
        }
    }

    fn logic_or(&mut self) -> Result<AstNode> {
        let mut expr = self.logic_and()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                Or => {
                    let or = self.advance()?;
                    let right = Box::new(self.logic_and()?);

                    expr = AstNode::LogicalExpr {
                        left: Box::new(expr),
                        operator: or,
                        right,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<AstNode> {
        let mut expr = self.equality()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                And => {
                    let and = self.advance()?;
                    let right = Box::new(self.equality()?);

                    expr = AstNode::LogicalExpr {
                        left: Box::new(expr),
                        operator: and,
                        right,
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<AstNode> {
        let mut expr = self.comparison()?;
        while let Some(token) = self.peek() {
            match token.token_type {
                BangEqual | EqualEqual => {
                    let operator = self.advance()?;
                    let right = self.comparison()?;
                    expr = AstNode::BinaryExpr {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<AstNode> {
        let mut expr = self.term()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                Greater | GreaterEqual | Less | LessEqual => {
                    let operator = self.advance()?;
                    let right = self.term()?;
                    expr = AstNode::BinaryExpr {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<AstNode> {
        let mut expr = self.factor()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                Minus | Plus => {
                    let operator = self.advance()?;
                    let right = self.factor()?;
                    expr = AstNode::BinaryExpr {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<AstNode> {
        let mut expr = self.unary()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                Star | Slash => {
                    let operator = self.advance()?;
                    let right = self.unary()?;
                    expr = AstNode::BinaryExpr {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<AstNode> {
        let operator = self.advance()?;
        match operator.token_type {
            Bang | Minus => {
                let right = self.unary()?;
                Ok(AstNode::UnaryExpr {
                    operator,
                    right: Box::new(right),
                })
            }
            _ => self.call(),
        }
    }

    fn call(&mut self) -> Result<AstNode> {
        let expr = self.primary()?;

        let next = self.peek().ok_or(Error::RanOutOfTokens)?;
        match next.token_type {
            LeftParen => Ok(self.finish_call(expr)?),
            _ => Ok(expr),
        }
    }

    fn finish_call(&mut self, callee: AstNode) -> Result<AstNode> {
        let paren = self.advance()?;
        let mut arguments = Vec::new();
        while let Some(token) = self.peek() {
            match token.token_type {
                RightParen => break,
                Semicolon => break,
                _ => {
                    if arguments.len() > 255 {
                        return Err(Error::TooManyFunctionArgs(token.clone()));
                    }
                    arguments.push(self.expression()?);
                    let next = self.peek().ok_or(Error::RanOutOfTokens)?;
                    match next.token_type {
                        Comma => {
                            self.advance()?;
                            continue;
                        }
                        _ => break,
                    }
                }
            }
        }
        expect!(self, RightParen, Error::ExpectedClosingParen(paren.clone()))?;

        Ok(AstNode::CallExpr {
            calle: Box::new(callee),
            paren,
            arguments: Some(arguments.into()),
        })
    }

    fn primary(&mut self) -> Result<AstNode> {
        let token = self.previous();
        match token.token_type {
            False => Ok(AstNode::Literal(LiteralExpr::False)),
            True => Ok(AstNode::Literal(LiteralExpr::True)),
            Nil => Ok(AstNode::Literal(LiteralExpr::Nil)),
            Number(n) => Ok(AstNode::Literal(LiteralExpr::Number(n.clone()))),
            String(s) => Ok(AstNode::Literal(LiteralExpr::StringLit(s.into()))),
            LeftParen => Ok(self.grouping()?),
            Identifier(_) => Ok(AstNode::Variable(token.clone())),
            EOF => Err(Error::UnexpectedEOF),
            _ => Err(Error::UnrecognizedExpression),
        }
    }

    fn grouping(&mut self) -> Result<AstNode> {
        let open = self.previous();
        let expr = self.expression()?;
        let next = self.advance()?;
        match next.token_type {
            RightParen => Ok(AstNode::Grouping(Box::new(expr))),
            _ => Err(Error::ExpectedClosingParen(open)),
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_expr() {
        let tokens = vec![Token {
            token_type: False,
            line: 0,
        }];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::RanOutOfTokens])
            },
            expr
        );

        let tokens = vec![
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::UnexpectedEOF])
            },
            expr
        );

        let tokens = vec![Token {
            token_type: EOF,
            line: 0,
        }];
        let expr = Parser::new(tokens).parse();
        assert_eq!(AstNode::Prog(Box::new([])), expr);

        let tokens = vec![
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: EqualEqual,
                line: 0,
            },
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: BangEqual,
                line: 0,
            },
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(
                AstNode::BinaryExpr {
                    right: Box::new(AstNode::Literal(LiteralExpr::False)),
                    operator: Token {
                        token_type: BangEqual,
                        line: 0
                    },
                    left: Box::new(AstNode::BinaryExpr {
                        left: Box::new(AstNode::Literal(LiteralExpr::False)),
                        operator: Token {
                            token_type: EqualEqual,
                            line: 0
                        },
                        right: Box::new(AstNode::Literal(LiteralExpr::False))
                    }),
                }
            ))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: EqualEqual,
                line: 0,
            },
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: GreaterEqual,
                line: 0,
            },
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(
                AstNode::BinaryExpr {
                    left: Box::new(AstNode::Literal(LiteralExpr::False)),
                    operator: Token {
                        token_type: EqualEqual,
                        line: 0
                    },
                    right: Box::new(AstNode::BinaryExpr {
                        left: Box::new(AstNode::Literal(LiteralExpr::False)),
                        operator: Token {
                            token_type: GreaterEqual,
                            line: 0
                        },
                        right: Box::new(AstNode::Literal(LiteralExpr::False))
                    }),
                }
            ))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: Number(2.0),
                line: 0,
            },
            Token {
                token_type: Minus,
                line: 0,
            },
            Token {
                token_type: Number(4.0),
                line: 0,
            },
            Token {
                token_type: Star,
                line: 0,
            },
            Token {
                token_type: Number(6.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(
                AstNode::BinaryExpr {
                    left: Box::new(AstNode::Literal(LiteralExpr::Number(2.0))),
                    operator: Token {
                        token_type: Minus,
                        line: 0
                    },
                    right: Box::new(AstNode::BinaryExpr {
                        left: Box::new(AstNode::Literal(LiteralExpr::Number(4.0))),
                        operator: Token {
                            token_type: Star,
                            line: 0
                        },
                        right: Box::new(AstNode::Literal(LiteralExpr::Number(6.0)))
                    }),
                }
            ))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: Bang,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(
                AstNode::UnaryExpr {
                    operator: Token {
                        token_type: Bang,
                        line: 0
                    },
                    right: Box::new(AstNode::Literal(LiteralExpr::True))
                }
            ))])),
            expr
        );
        let tokens = vec![
            Token {
                token_type: Bang,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: Number(4.0),
                line: 1,
            },
            Token {
                token_type: Star,
                line: 1,
            },
            Token {
                token_type: Number(6.0),
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 1,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([
                AstNode::ExprStmt(Box::new(AstNode::UnaryExpr {
                    operator: Token {
                        token_type: Bang,
                        line: 0
                    },
                    right: Box::new(AstNode::Literal(LiteralExpr::True))
                })),
                AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
                    left: Box::new(AstNode::Literal(LiteralExpr::Number(4.0))),
                    operator: Token {
                        token_type: Star,
                        line: 1
                    },
                    right: Box::new(AstNode::Literal(LiteralExpr::Number(6.0)))
                })),
            ])),
            expr
        );
    }

    #[test]
    fn test_grouping() {
        let tokens = vec![
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(AstNode::Grouping(
                Box::new(AstNode::Literal(LiteralExpr::False))
            ))),])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: False,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::ExpectedClosingParen(Token {
                    token_type: LeftParen,
                    line: 0
                })])
            },
            expr
        );
    }

    #[test]
    fn test_print_stmt() {
        let tokens = vec![
            Token {
                token_type: Print,
                line: 0,
            },
            Token {
                token_type: String("this is a string".into()),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::PrintStmt(Box::new(AstNode::Literal(
                LiteralExpr::StringLit("this is a string".into())
            )))])),
            expr
        );
    }

    #[test]
    fn test_var_decl_stmt() {
        let tokens = vec![
            Token {
                token_type: Var,
                line: 0,
            },
            Token {
                token_type: Identifier("this".into()),
                line: 0,
            },
            Token {
                token_type: Equal,
                line: 0,
            },
            Token {
                token_type: Number(1.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::DeclStmt {
                identifier: Token {
                    line: 0,
                    token_type: Identifier("this".into()),
                },
                expr: Some(Box::new(AstNode::Literal(LiteralExpr::Number(1.0))))
            }])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: Var,
                line: 0,
            },
            Token {
                token_type: Identifier("this".into()),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::DeclStmt {
                identifier: Token {
                    token_type: Identifier("this".into()),
                    line: 0
                },
                expr: None,
            }])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: Var,
                line: 0,
            },
            Token {
                token_type: Equal,
                line: 0,
            },
            Token {
                token_type: Number(1.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::VarExpectedIdentifer(Token {
                    line: 0,
                    token_type: Var
                })])
            },
            expr
        );

        let tokens = vec![
            Token {
                token_type: Var,
                line: 0,
            },
            Token {
                token_type: Identifier("name".into()),
                line: 0,
            },
            Token {
                token_type: Number(1.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::ExpectedSemicolon {
                    line: 0,
                    preceding: Var.to_string()
                }])
            },
            expr
        );
    }

    #[test]
    fn test_assignment() {
        let tokens = vec![
            Token {
                token_type: Identifier("test".into()),
                line: 0,
            },
            Token {
                token_type: Equal,
                line: 0,
            },
            Token {
                token_type: Number(1.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(AstNode::Assign {
                target: Box::new(AstNode::Variable(Token {
                    token_type: Identifier("test".into()),
                    line: 0,
                })),
                value: Box::new(AstNode::Literal(LiteralExpr::Number(1.0)))
            }))])),
            expr
        );
    }

    #[test]
    fn test_block() {
        let tokens = vec![
            Token {
                token_type: LeftBrace,
                line: 0,
            },
            Token {
                token_type: RightBrace,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::Block(Box::new([]))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: LeftBrace,
                line: 0,
            },
            Token {
                token_type: Print,
                line: 1,
            },
            Token {
                token_type: Number(1.0),
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: RightBrace,
                line: 2,
            },
            Token {
                token_type: EOF,
                line: 2,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::Block(Box::new([AstNode::PrintStmt(
                Box::new(AstNode::Literal(LiteralExpr::Number(1.0)))
            )]))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: LeftBrace,
                line: 0,
            },
            Token {
                token_type: Print,
                line: 1,
            },
            Token {
                token_type: Number(1.0),
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 2,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::ExpectedClosingBrace(Token {
                    token_type: LeftBrace,
                    line: 0,
                })])
            },
            expr
        );
        let tokens = vec![
            Token {
                token_type: LeftBrace,
                line: 0,
            },
            Token {
                token_type: LeftBrace,
                line: 1,
            },
            Token {
                token_type: RightBrace,
                line: 1,
            },
            Token {
                token_type: RightBrace,
                line: 2,
            },
            Token {
                token_type: EOF,
                line: 2,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::Block(Box::new([AstNode::Block(
                Box::new([])
            )]))])),
            expr
        );
    }

    #[test]
    fn test_if_statement() {
        let tokens = vec![
            Token {
                token_type: If,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Print,
                line: 1,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 2,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::IfStmt {
                condition: Box::new(AstNode::Literal(LiteralExpr::True)),
                then_stmt: Box::new(AstNode::PrintStmt(Box::new(AstNode::Literal(
                    LiteralExpr::True
                )))),
                else_stmt: None,
            }])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: If,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Print,
                line: 1,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: Else,
                line: 2,
            },
            Token {
                token_type: Print,
                line: 2,
            },
            Token {
                token_type: False,
                line: 2,
            },
            Token {
                token_type: Semicolon,
                line: 2,
            },
            Token {
                token_type: EOF,
                line: 2,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::IfStmt {
                condition: Box::new(AstNode::Literal(LiteralExpr::True)),
                then_stmt: Box::new(AstNode::PrintStmt(Box::new(AstNode::Literal(
                    LiteralExpr::True
                )))),
                else_stmt: Some(Box::new(AstNode::PrintStmt(Box::new(AstNode::Literal(
                    LiteralExpr::False
                ))))),
            }])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: If,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Print,
                line: 1,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: Print,
                line: 2,
            },
            Token {
                token_type: False,
                line: 2,
            },
            Token {
                token_type: Semicolon,
                line: 2,
            },
            Token {
                token_type: EOF,
                line: 2,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([
                AstNode::IfStmt {
                    condition: Box::new(AstNode::Literal(LiteralExpr::True)),
                    then_stmt: Box::new(AstNode::PrintStmt(Box::new(AstNode::Literal(
                        LiteralExpr::True
                    )))),
                    else_stmt: None,
                },
                AstNode::PrintStmt(Box::new(AstNode::Literal(LiteralExpr::False))),
            ])),
            expr
        );
    }

    #[test]
    fn test_logical_expr() {
        let tokens = vec![
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Or,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(
                AstNode::LogicalExpr {
                    left: Box::new(AstNode::Literal(LiteralExpr::True)),
                    operator: Token {
                        line: 0,
                        token_type: Or
                    },
                    right: Box::new(AstNode::Literal(LiteralExpr::True)),
                }
            ))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: And,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(
                AstNode::LogicalExpr {
                    left: Box::new(AstNode::Literal(LiteralExpr::True)),
                    operator: Token {
                        line: 0,
                        token_type: And
                    },
                    right: Box::new(AstNode::Literal(LiteralExpr::True)),
                }
            ))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: And,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Or,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(
                AstNode::LogicalExpr {
                    left: Box::new(AstNode::LogicalExpr {
                        left: Box::new(AstNode::Literal(LiteralExpr::True)),
                        operator: Token {
                            line: 0,
                            token_type: And
                        },
                        right: Box::new(AstNode::Literal(LiteralExpr::True)),
                    }),
                    operator: Token {
                        line: 0,
                        token_type: Or
                    },
                    right: Box::new(AstNode::Literal(LiteralExpr::True)),
                }
            ))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Or,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: And,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(
                AstNode::LogicalExpr {
                    left: Box::new(AstNode::Literal(LiteralExpr::True)),
                    operator: Token {
                        line: 0,
                        token_type: Or
                    },
                    right: Box::new(AstNode::LogicalExpr {
                        left: Box::new(AstNode::Literal(LiteralExpr::True)),
                        operator: Token {
                            line: 0,
                            token_type: And
                        },
                        right: Box::new(AstNode::Literal(LiteralExpr::True)),
                    }),
                }
            ))])),
            expr
        );
    }

    #[test]
    fn test_while_statement() {
        let tokens = vec![
            Token {
                token_type: While,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Print,
                line: 1,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 1,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::WhileStmt {
                condition: Box::new(AstNode::Literal(LiteralExpr::True)),
                body: Box::new(AstNode::PrintStmt(Box::new(AstNode::Literal(
                    LiteralExpr::True
                ))))
            }])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: While,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Print,
                line: 1,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 1,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([AstNode::PrintStmt(Box::new(AstNode::Literal(
                    LiteralExpr::True
                )))]),
                errors: Box::new([Error::ExpectedOpeningParen(Token {
                    token_type: While,
                    line: 0
                })]),
            },
            expr
        );

        let tokens = vec![
            Token {
                token_type: While,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 0,
            },
            Token {
                token_type: Print,
                line: 1,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 1,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([AstNode::PrintStmt(Box::new(AstNode::Literal(
                    LiteralExpr::True
                )))]),
                errors: Box::new([Error::ExpectedClosingParen(Token {
                    token_type: While,
                    line: 0
                })]),
            },
            expr
        );
    }

    #[test]
    fn test_for_statement() {
        let tokens = vec![
            Token {
                token_type: For,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 1,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::WhileStmt {
                condition: Box::new(AstNode::Literal(LiteralExpr::True)),
                body: Box::new(AstNode::ExprStmt(Box::new(AstNode::Literal(
                    LiteralExpr::True
                ))))
            }])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: For,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Var,
                line: 0,
            },
            Token {
                token_type: Identifier("i".into()),
                line: 0,
            },
            Token {
                token_type: Equal,
                line: 0,
            },
            Token {
                token_type: Number(1.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 1,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::Block(Box::new([
                AstNode::DeclStmt {
                    identifier: Token {
                        token_type: Identifier("i".into()),
                        line: 0,
                    },
                    expr: Some(Box::new(AstNode::Literal(LiteralExpr::Number(1.0))))
                },
                AstNode::WhileStmt {
                    condition: Box::new(AstNode::Literal(LiteralExpr::True)),
                    body: Box::new(AstNode::ExprStmt(Box::new(AstNode::Literal(
                        LiteralExpr::True
                    ))))
                }
            ]))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: For,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Var,
                line: 0,
            },
            Token {
                token_type: Identifier("i".into()),
                line: 0,
            },
            Token {
                token_type: Equal,
                line: 0,
            },
            Token {
                token_type: Number(1.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: Identifier("i".into()),
                line: 0,
            },
            Token {
                token_type: LessEqual,
                line: 0,
            },
            Token {
                token_type: Number(3.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 1,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::Block(Box::new([
                AstNode::DeclStmt {
                    identifier: Token {
                        token_type: Identifier("i".into()),
                        line: 0,
                    },
                    expr: Some(Box::new(AstNode::Literal(LiteralExpr::Number(1.0))))
                },
                AstNode::WhileStmt {
                    condition: Box::new(AstNode::BinaryExpr {
                        left: Box::new(AstNode::Variable(Token {
                            token_type: Identifier("i".into()),
                            line: 0,
                        },)),
                        operator: Token {
                            token_type: LessEqual,
                            line: 0,
                        },
                        right: Box::new(AstNode::Literal(LiteralExpr::Number(3.0)))
                    }),
                    body: Box::new(AstNode::ExprStmt(Box::new(AstNode::Literal(
                        LiteralExpr::True
                    ))))
                }
            ]))])),
            expr
        );

        let tokens = vec![
            Token {
                token_type: For,
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Var,
                line: 0,
            },
            Token {
                token_type: Identifier("i".into()),
                line: 0,
            },
            Token {
                token_type: Equal,
                line: 0,
            },
            Token {
                token_type: Number(1.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: Identifier("i".into()),
                line: 0,
            },
            Token {
                token_type: LessEqual,
                line: 0,
            },
            Token {
                token_type: Number(3.0),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: Identifier("i".into()),
                line: 0,
            },
            Token {
                token_type: Equal,
                line: 0,
            },
            Token {
                token_type: Identifier("i".into()),
                line: 0,
            },
            Token {
                token_type: Plus,
                line: 0,
            },
            Token {
                token_type: Number(1.0),
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: True,
                line: 1,
            },
            Token {
                token_type: Semicolon,
                line: 1,
            },
            Token {
                token_type: EOF,
                line: 1,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::Block(Box::new([
                AstNode::DeclStmt {
                    identifier: Token {
                        token_type: Identifier("i".into()),
                        line: 0,
                    },
                    expr: Some(Box::new(AstNode::Literal(LiteralExpr::Number(1.0))))
                },
                AstNode::WhileStmt {
                    condition: Box::new(AstNode::BinaryExpr {
                        left: Box::new(AstNode::Variable(Token {
                            token_type: Identifier("i".into()),
                            line: 0,
                        },)),
                        operator: Token {
                            token_type: LessEqual,
                            line: 0,
                        },
                        right: Box::new(AstNode::Literal(LiteralExpr::Number(3.0)))
                    }),
                    body: Box::new(AstNode::Block(Box::new([
                        AstNode::ExprStmt(Box::new(AstNode::Literal(LiteralExpr::True))),
                        AstNode::Assign {
                            target: Box::new(AstNode::Variable(Token {
                                token_type: Identifier("i".into()),
                                line: 0,
                            })),
                            value: Box::new(AstNode::BinaryExpr {
                                left: Box::new(AstNode::Variable(Token {
                                    token_type: Identifier("i".into()),
                                    line: 0,
                                })),
                                operator: Token {
                                    token_type: Plus,
                                    line: 0,
                                },
                                right: Box::new(AstNode::Literal(LiteralExpr::Number(1.0)))
                            })
                        }
                    ])))
                }
            ]))])),
            expr
        );
    }

    #[test]
    fn test_call_expression() {
        let tokens = vec![
            Token {
                token_type: Identifier("f".into()),
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::ExpectedClosingParen(Token {
                    token_type: LeftParen,
                    line: 0,
                })]),
            },
            expr
        );

        let tokens = vec![
            Token {
                token_type: Identifier("f".into()),
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(AstNode::CallExpr {
                calle: Box::new(AstNode::Variable(Token {
                    token_type: Identifier("f".into()),
                    line: 0
                })),
                paren: Token {
                    token_type: LeftParen,
                    line: 0
                },
                arguments: Some(Box::new([])),
            }))]),),
            expr
        );

        let tokens = vec![
            Token {
                token_type: Identifier("f".into()),
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Identifier("x".into()),
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(AstNode::CallExpr {
                calle: Box::new(AstNode::Variable(Token {
                    token_type: Identifier("f".into()),
                    line: 0
                })),
                paren: Token {
                    token_type: LeftParen,
                    line: 0
                },
                arguments: Some(Box::new([AstNode::Variable(Token {
                    token_type: Identifier("x".into()),
                    line: 0
                })])),
            }))]),),
            expr
        );

        let tokens = vec![
            Token {
                token_type: Identifier("f".into()),
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Identifier("x".into()),
                line: 0,
            },
            Token {
                token_type: Comma,
                line: 0,
            },
            Token {
                token_type: Identifier("y".into()),
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::Prog(Box::new([AstNode::ExprStmt(Box::new(AstNode::CallExpr {
                calle: Box::new(AstNode::Variable(Token {
                    token_type: Identifier("f".into()),
                    line: 0
                })),
                paren: Token {
                    token_type: LeftParen,
                    line: 0
                },
                arguments: Some(Box::new([
                    AstNode::Variable(Token {
                        token_type: Identifier("x".into()),
                        line: 0
                    }),
                    AstNode::Variable(Token {
                        token_type: Identifier("y".into()),
                        line: 0
                    })
                ])),
            }))]),),
            expr
        );

        let tokens = vec![
            Token {
                token_type: Identifier("f".into()),
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Identifier("x".into()),
                line: 0,
            },
            Token {
                token_type: Identifier("y".into()),
                line: 0,
            },
            Token {
                token_type: RightParen,
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::ExpectedClosingParen(Token {
                    token_type: LeftParen,
                    line: 0
                })]),
            },
            expr
        );

        let tokens = vec![
            Token {
                token_type: Identifier("f".into()),
                line: 0,
            },
            Token {
                token_type: LeftParen,
                line: 0,
            },
            Token {
                token_type: Identifier("x".into()),
                line: 0,
            },
            Token {
                token_type: Semicolon,
                line: 0,
            },
            Token {
                token_type: EOF,
                line: 0,
            },
        ];
        let expr = Parser::new(tokens).parse();
        assert_eq!(
            AstNode::ProgInvalid {
                stmts: Box::new([]),
                errors: Box::new([Error::ExpectedClosingParen(Token {
                    token_type: LeftParen,
                    line: 0
                })]),
            },
            expr
        );
    }
}
