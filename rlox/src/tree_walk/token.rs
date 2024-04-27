use std::fmt::Display;

use super::tokenizer::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // single char
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // one or more char
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals
    Identifier(String),
    String(String),
    Number(f64),

    // keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,

    Comment(String),
    Unrecognized(Error),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var => write!(f, "variable declearation"),
            Self::Print => write!(f, "print statement"),
            Self::If => write!(f, "if"),
            Self::LeftParen => write!(f, "opeing paren"),
            Self::RightParen => write!(f, "closing paren"),
            Self::While => write!(f, "while statement"),
            Self::EOF => write!(f, "end of file"),
            _ => unimplemented!("Display not implemented for {:?}", self),
        }
    }
}
