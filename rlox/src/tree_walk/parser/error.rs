use crate::tree_walk::token::Token;
use std::fmt::Display;

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    RanOutOfTokens,
    UnexpectedEOF,
    UnrecognizedExpression,
    ExpectedSemicolon(Token),
    ExpectedClosingParen(Token),
    VarExpectedIdentifer(Token),
    VarExpectedEqual(Token),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedSemicolon(token) => {
                writeln!(f, "[line {}] Expected Semicolon.", token.line,)
            }
            Self::ExpectedClosingParen(token) => {
                writeln!(f, "[line {}] Expected closing ')'.", token.line)
            }
            Self::VarExpectedIdentifer(token) => {
                writeln!(
                    f,
                    "[line {}] Expected Identifer after {:?}.",
                    token.line, token.token_type
                )
            }
            Self::VarExpectedEqual(token) => {
                writeln!(f, "[line {}] Expected '=' after Identifer.", token.line)
            }
            _ => writeln!(f, "{:?}", self),
        }
    }
}

impl std::error::Error for Error {}
