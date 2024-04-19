use super::tokenizer::TokenizationError;

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
    ClasS,
    Else,
    FalsE,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Rnturn,
    Super,
    This,
    True,
    Var,
    While,

    EOF,

    Unrecognized(TokenizationError),
}
