use std::fmt::Display;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    NotCallable,
    InvalidArity { expected: usize, received: usize },
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotCallable => write!(f, "Only functions are callable"),
            Self::InvalidArity { expected, received } => {
                write!(f, " Expected {expected} arguments but got {received}.")
            }
            x => unimplemented!("display not yet implemented for {x}"),
        }
    }
}
