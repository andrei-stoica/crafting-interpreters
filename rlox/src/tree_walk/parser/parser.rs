use super::{Error, Result};
use crate::tree_walk::token::{Token, TokenType::*};

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Prog(Vec<AstNode>),
    ProgInvalid {
        stmts: Vec<AstNode>,
        errors: Vec<Error>,
    },
    ExprStmt(Box<AstNode>),
    PrintStmt(Box<AstNode>),
    DeclExpr {
        identifier: Token,
        expr: Option<Box<AstNode>>,
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
    Literal(LiteralExpr),
    Grouping(Box<AstNode>),
    Variable(Token),
}

#[derive(Debug, PartialEq)]
pub enum LiteralExpr {
    False,
    True,
    Nil,
    Number(f64),
    StringLit(std::string::String),
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

    pub fn parse(&mut self) -> AstNode {
        self.prog()
    }

    fn report_error(error: &Error) {
        eprintln!("{}", error);
    }

    fn expect_semicolon(
        &mut self,
        line: u32,
        preceding: std::string::String,
        ret_val: Result<AstNode>,
    ) -> Result<AstNode> {
        let next = self.advance()?;
        match next.token_type {
            Semicolon => ret_val,
            _ => Err(Error::ExpectedSemicolon { line, preceding }),
        }
    }

    fn syncronize(&mut self) {
        while let Some(token) = self.peek() {
            match token.token_type {
                Class | For | Fun | If | Print | Return | Var | While | EOF => break,
                Semicolon => {
                    self.advance();
                    break;
                }
                _ => {
                    let _ = self.advance();
                }
            }
        }
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
            false => AstNode::Prog(stmts),
            true => AstNode::ProgInvalid { stmts, errors },
        }
    }

    fn decleration(&mut self) -> Result<AstNode> {
        if let Some(token) = self.peek() {
            match token.token_type {
                Var => self.var_decleration(),
                _ => self.statement(),
            }
        } else {
            Err(Error::RanOutOfTokens)
        }
    }

    fn var_decleration(&mut self) -> Result<AstNode> {
        let var_token = self.advance()?;
        let next = self.advance()?;
        let identifier = if matches!(next.token_type, Identifier(_)) {
            next
        } else {
            return Err(Error::VarExpectedIdentifer(var_token));
        };
        let next = self.peek();
        let expr = match next {
            Some(token) if token.token_type == Equal => {
                self.advance();
                Some(Box::new(self.expression()?))
            }
            _ => None,
        };

        self.expect_semicolon(
            var_token.line,
            var_token.token_type.to_string(),
            Ok(AstNode::DeclExpr { identifier, expr }),
        )
    }

    fn statement(&mut self) -> Result<AstNode> {
        if let Some(token) = self.peek() {
            match token.token_type {
                Print => self.print_statement(),
                _ => self.expr_statement(),
            }
        } else {
            Err(Error::RanOutOfTokens)
        }
    }

    fn print_statement(&mut self) -> Result<AstNode> {
        let print_token = self.advance()?;
        let expr = self.expression()?;

        self.expect_semicolon(
            print_token.line,
            print_token.token_type.to_string(),
            Ok(AstNode::PrintStmt(Box::new(expr))),
        )
    }

    fn expr_statement(&mut self) -> Result<AstNode> {
        let expr = self.expression()?;
        let next = self.peek().cloned();
        match next {
            None => Err(Error::RanOutOfTokens),
            Some(token) if matches!(token.token_type, EOF) => Err(Error::UnexpectedEOF),
            Some(token) => self.expect_semicolon(
                token.line,
                "Expresion".into(),
                Ok(AstNode::ExprStmt(Box::new(expr))),
            ),
        }
    }

    fn expression(&mut self) -> Result<AstNode> {
        self.equality()
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
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<AstNode> {
        let token = self.previous();
        match token.token_type {
            False => Ok(AstNode::Literal(LiteralExpr::False)),
            True => Ok(AstNode::Literal(LiteralExpr::True)),
            Nil => Ok(AstNode::Literal(LiteralExpr::Nil)),
            Number(n) => Ok(AstNode::Literal(LiteralExpr::Number(n.clone()))),
            String(s) => Ok(AstNode::Literal(LiteralExpr::StringLit(s.clone()))),
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
                stmts: vec![],
                errors: vec![Error::RanOutOfTokens]
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
                stmts: vec![],
                errors: vec![Error::UnexpectedEOF]
            },
            expr
        );

        let tokens = vec![Token {
            token_type: EOF,
            line: 0,
        }];
        let expr = Parser::new(tokens).parse();
        assert_eq!(AstNode::Prog(vec![]), expr);

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
            AstNode::Prog(vec![AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
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
            }))]),
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
            AstNode::Prog(vec![AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
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
            }))]),
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
            AstNode::Prog(vec![AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
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
            }))]),
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
            AstNode::Prog(vec![AstNode::ExprStmt(Box::new(AstNode::UnaryExpr {
                operator: Token {
                    token_type: Bang,
                    line: 0
                },
                right: Box::new(AstNode::Literal(LiteralExpr::True))
            }))]),
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
            AstNode::Prog(vec![
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
            ]),
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
            AstNode::Prog(vec![AstNode::ExprStmt(Box::new(AstNode::Grouping(
                Box::new(AstNode::Literal(LiteralExpr::False))
            ))),]),
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
                stmts: vec![],
                errors: vec![Error::ExpectedClosingParen(Token {
                    token_type: LeftParen,
                    line: 0
                })]
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
            AstNode::Prog(vec![AstNode::PrintStmt(Box::new(AstNode::Literal(
                LiteralExpr::StringLit("this is a string".into())
            )))]),
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
            AstNode::Prog(vec![AstNode::DeclExpr {
                identifier: Token {
                    line: 0,
                    token_type: Identifier("this".into()),
                },
                expr: Some(Box::new(AstNode::Literal(LiteralExpr::Number(1.0))))
            }]),
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
            AstNode::Prog(vec![AstNode::DeclExpr {
                identifier: Token {
                    token_type: Identifier("this".into()),
                    line: 0
                },
                expr: None,
            }]),
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
                stmts: vec![],
                errors: vec![Error::VarExpectedIdentifer(Token {
                    line: 0,
                    token_type: Var
                })]
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
                stmts: vec![],
                errors: vec![Error::ExpectedSemicolon {
                    line: 0,
                    preceding: Var.to_string()
                }]
            },
            expr
        );
    }
}
