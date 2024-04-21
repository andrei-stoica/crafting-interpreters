#![allow(dead_code)]

use crate::tree_walk::TokenType;
use crate::tree_walk::{Token, TokenType::*};

type ParseResult = Result<AstNode, ParseError>;

#[derive(Debug, PartialEq)]
enum ParseError {
    RanOutOfTokens,
    UnexpectedEOF,
    UnexpectedToken, // HACK: This should not be used in the long run but
    // included for quick prototypeing
    UnrecognizedExpression,
    ExpectedSemicolon(Token),
}

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Prog {
        stmts: Vec<AstNode>,
    },
    ExprStmt(Box<AstNode>),
    PrintStmt {
        expr: Box<AstNode>,
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
}

#[derive(Debug, PartialEq)]
pub enum LiteralExpr {
    False,
    True,
    Nil,
    Number(f64),
    StringLit(std::string::String),
}
struct Parser {
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

    fn peek_type(&mut self) -> Option<&TokenType> {
        Some(&self.tokens.get(self.current)?.token_type)
    }

    fn advance(&mut self) -> Token {
        self.current += 1;
        self.tokens[self.current - 1].clone()
    }

    fn previous(&mut self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    pub fn parse(&mut self) -> ParseResult {
        self.prog()
    }

    fn report_error(token: Token, msg: &str) {
        println!("[line {}] {}", token.line, msg);
    }

    fn syncronize(&mut self) {
        while let Some(token) = self.peek() {
            match token.token_type {
                Semicolon | Class | For | Fun | If | Print | Return | Var | While => break,
                _ => self.advance(),
            };
        }
    }

    fn prog(&mut self) -> ParseResult {
        let mut stmts = Vec::new();

        while let Some(token) = self.peek() {
            match token.token_type {
                EOF => break,
                _ => stmts.push(self.statement()?),
            }
        }

        Ok(AstNode::Prog { stmts })
    }

    fn statement(&mut self) -> ParseResult {
        if let Some(token) = self.peek() {
            match token.token_type {
                Print => self.print_statement(),
                _ => self.expr_statement(),
            }
        } else {
            Err(ParseError::RanOutOfTokens)
        }
    }

    fn print_statement(&mut self) -> ParseResult {
        unimplemented!();
    }

    fn expr_statement(&mut self) -> ParseResult {
        let expr = self.expression()?;
        if let Some(token) = self.peek() {
            match token.token_type {
                Semicolon => {
                    self.advance();
                    Ok(AstNode::ExprStmt(Box::new(expr)))
                }
                _ => Err(ParseError::ExpectedSemicolon(token.clone())),
            }
        } else {
            Err(ParseError::RanOutOfTokens)
        }
    }

    fn expression(&mut self) -> ParseResult {
        self.equality()
    }

    fn equality(&mut self) -> ParseResult {
        let mut expr = self.comparison()?;
        while let Some(token) = self.peek() {
            match token.token_type {
                BangEqual | EqualEqual => {
                    let operator = self.advance();
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

    fn comparison(&mut self) -> ParseResult {
        let mut expr = self.term()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                Greater | GreaterEqual | Less | LessEqual => {
                    let operator = self.advance();
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

    fn term(&mut self) -> ParseResult {
        let mut expr = self.factor()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                Minus | Plus => {
                    let operator = self.advance();
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

    fn factor(&mut self) -> ParseResult {
        let mut expr = self.unary()?;

        while let Some(token) = self.peek() {
            match token.token_type {
                Star | Slash => {
                    let operator = self.advance();
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

    fn unary(&mut self) -> ParseResult {
        if let Some(token) = self.peek() {
            match token.token_type {
                Bang | Minus => {
                    let operator = self.advance();
                    let right = self.unary()?;
                    Ok(AstNode::UnaryExpr {
                        operator,
                        right: Box::new(right),
                    })
                }
                _ => Ok(self.primary()?),
            }
        } else {
            Ok(self.primary()?)
        }
    }

    fn primary(&mut self) -> ParseResult {
        let token = self.advance();
        match token.token_type {
            False => Ok(AstNode::Literal(LiteralExpr::False)),
            True => Ok(AstNode::Literal(LiteralExpr::True)),
            Nil => Ok(AstNode::Literal(LiteralExpr::Nil)),
            Number(n) => Ok(AstNode::Literal(LiteralExpr::Number(n.clone()))),
            String(s) => Ok(AstNode::Literal(LiteralExpr::StringLit(s.clone()))),
            EOF => Err(ParseError::UnexpectedEOF),
            _ => Err(ParseError::UnrecognizedExpression),
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
        assert_eq!(Err(ParseError::RanOutOfTokens), expr);

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
            Err(ParseError::ExpectedSemicolon(Token {
                token_type: EOF,
                line: 0
            })),
            expr
        );

        let tokens = vec![Token {
            token_type: EOF,
            line: 0,
        }];
        let expr = Parser::new(tokens).parse();
        assert_eq!(Ok(AstNode::Prog { stmts: vec![] }), expr);

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
            Ok(AstNode::Prog {
                stmts: vec![AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
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
                }))]
            }),
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
            Ok(AstNode::Prog {
                stmts: vec![AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
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
                }))]
            }),
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
            Ok(AstNode::Prog {
                stmts: vec![AstNode::ExprStmt(Box::new(AstNode::BinaryExpr {
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
                }))]
            }),
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
            Ok(AstNode::Prog {
                stmts: vec![AstNode::ExprStmt(Box::new(AstNode::UnaryExpr {
                    operator: Token {
                        token_type: Bang,
                        line: 0
                    },
                    right: Box::new(AstNode::Literal(LiteralExpr::True))
                }))]
            }),
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
            Ok(AstNode::Prog {
                stmts: vec![
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
                ]
            }),
            expr
        );
    }
}
