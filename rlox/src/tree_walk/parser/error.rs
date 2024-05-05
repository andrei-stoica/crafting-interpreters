use crate::tree_walk::token::{self, Token};
use std::fmt::Display;

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    RanOutOfTokens,
    UnexpectedEOF,
    UnrecognizedExpression,
    ExpectedComma,
    ExpectedSemicolon { line: u32, preceding: String },
    ExpectedOpeningParen(Token),
    ExpectedClosingParen(Token),
    ExpectedOpeningBrace(Token),
    ExpectedClosingBrace(Token),
    ExpectedIdentifer(Token),
    VarExpectedEqual(Token),
    InvalidAssignmentTarget(Token),
    TooManyFunctionArgs(Token),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedSemicolon { line, preceding } => {
                write!(f, "[line {}] Expected ';' after {}.", line, preceding)
            }
            Self::ExpectedClosingParen(token) => {
                write!(
                    f,
                    "[line {}] Expected closing ')' after {}.",
                    token.line, token.token_type
                )
            }
            Self::ExpectedOpeningBrace(token) => {
                write!(f, "[line {}] Expected opening '{{'.", token.line)
            }
            Self::ExpectedClosingBrace(token) => {
                write!(f, "[line {}] Expected closing '}}'.", token.line)
            }
            Self::ExpectedIdentifer(token) => {
                write!(
                    f,
                    "[line {}] Expected Identifer after {:?}.",
                    token.line, token.token_type
                )
            }
            Self::VarExpectedEqual(token) => {
                write!(f, "[line {}] Expected '=' after Identifer.", token.line)
            }
            Self::InvalidAssignmentTarget(token) => {
                write!(f, "[line {}] Invalid assignment target.", token.line)
            }
            Self::TooManyFunctionArgs(token) => {
                write!(
                    f,
                    "[line {}] Can't have more than 255 arguments.",
                    token.line
                )
            }
            Self::UnexpectedEOF => write!(f, "Reached End of file unexpectedly."),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl std::error::Error for Error {}
