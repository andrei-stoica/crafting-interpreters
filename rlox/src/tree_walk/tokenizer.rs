use crate::tree_walk::token::{Token, TokenType};
use std::{fmt::Display, iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Tokenizer {
    tokens: Vec<Token>,
    line: u32,
}

// TODO: Decide whether or not to keep unrecognized and comments tokens as part of token stream
#[derive(Debug, Clone, PartialEq)]
pub enum TokenizationError {
    UntermiatedString(u32),
}

impl Display for TokenizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UntermiatedString(line) => {
                write!(f, "[Line {}] Unterminated string", line)
            }
            _ => unimplemented!(),
        }
    }
}
impl Tokenizer {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line: 0,
        }
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            token_type,
            line: self.line,
        });
    }

    fn add_token_at_line(&mut self, token_type: TokenType, line: u32) {
        self.tokens.push(Token { token_type, line });
    }
    fn consume_if(expected: char, src: &mut Peekable<Chars>) -> bool {
        if let Some(c) = src.peek() {
            if c == &expected {
                src.next();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn consume_comment(&mut self, src: &mut Peekable<Chars>) -> TokenType {
        let mut content: String = String::new();
        while let Some(c) = src.peek() {
            match c {
                '\n' => break,
                _ => content.push(src.next().expect("Character expected")),
            };
        }
        TokenType::Comment(content)
    }

    fn consume_string_lit(
        &mut self,
        src: &mut Peekable<Chars>,
    ) -> Result<TokenType, TokenizationError> {
        let mut content: String = String::new();
        while let Some(c) = src.peek() {
            match c {
                '"' => {
                    src.next();
                    return Ok(TokenType::String(content));
                }
                '\n' => {
                    content.push(src.next().expect("terminated string"));
                    self.line += 1;
                }

                _ => content.push(src.next().expect("terminated string")),
            };
        }
        Err(TokenizationError::UntermiatedString(self.line))
        // error
    }
    fn consume_number_lit(&mut self, src: &mut Peekable<Chars>, c: char) -> TokenType {
        let mut content: String = String::from(c);
        while let Some(c) = src.peek() {
            match c {
                '0'..='9' | '.' => content.push(src.next().expect("Next char should exist")),
                _ => break,
            }
        }

        TokenType::Number(content.parse().expect("Number should be parsable."))
    }

    pub fn parse(&mut self, text: &str) -> Vec<Token> {
        let mut src = text.chars().peekable();
        self.tokens = Vec::new();
        self.line = 0;

        while let Some(c) = src.next() {
            match c {
                // new line
                '\n' => self.line += 1,
                // single char
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
                '{' => self.add_token(TokenType::LeftBrace),
                '}' => self.add_token(TokenType::RightBrace),
                ',' => self.add_token(TokenType::Comma),
                '.' => self.add_token(TokenType::Dot),
                '-' => self.add_token(TokenType::Minus),
                '+' => self.add_token(TokenType::Plus),
                ';' => self.add_token(TokenType::Semicolon),
                '*' => self.add_token(TokenType::Star),

                // operators
                '!' if Self::consume_if('=', &mut src) => self.add_token(TokenType::BangEqual),
                '!' => self.add_token(TokenType::Bang),

                '=' if Self::consume_if('=', &mut src) => self.add_token(TokenType::EqualEqual),
                '=' => self.add_token(TokenType::Equal),

                '<' if Self::consume_if('=', &mut src) => self.add_token(TokenType::LessEqual),
                '<' => self.add_token(TokenType::Less),

                '>' if Self::consume_if('=', &mut src) => self.add_token(TokenType::GreaterEqual),
                '>' => self.add_token(TokenType::Greater),

                // comments
                '/' if Self::consume_if('/', &mut src) => {
                    let token_type = self.consume_comment(&mut src);
                    self.add_token(token_type);
                }
                '/' => self.add_token(TokenType::Slash),

                // strings
                '"' => {
                    let start_line = self.line;
                    match self.consume_string_lit(&mut src) {
                        Ok(token) => self.add_token_at_line(token, start_line),
                        Err(err) => {
                            println!("{}", err);
                            self.add_token(TokenType::Unrecognized(err));
                        }
                    }
                }
                '0'..='9' => {
                    let number = self.consume_number_lit(&mut src, c);
                    self.add_token(number);
                }
                _ => (),
            }
        }
        self.add_token(TokenType::EOF);
        return self.tokens.clone();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_single_char_tokens() {
        let mut tokenizer = Tokenizer::new();

        let eof = "";
        let tokens = tokenizer.parse(eof);
        assert_eq!(
            vec![Token {
                token_type: TokenType::EOF,
                line: 0
            }],
            tokens
        );

        let parens = "()";
        let tokens = tokenizer.parse(parens);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftParen,
                    line: 0
                },
                Token {
                    token_type: TokenType::RightParen,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let braces = "{}";
        let tokens = tokenizer.parse(braces);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 0
                },
                Token {
                    token_type: TokenType::RightBrace,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let math = "-+*/";
        let tokens = tokenizer.parse(math);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Minus,
                    line: 0
                },
                Token {
                    token_type: TokenType::Plus,
                    line: 0
                },
                Token {
                    token_type: TokenType::Star,
                    line: 0
                },
                Token {
                    token_type: TokenType::Slash,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let punctuation = ",.;";
        let tokens = tokenizer.parse(punctuation);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Comma,
                    line: 0
                },
                Token {
                    token_type: TokenType::Dot,
                    line: 0
                },
                Token {
                    token_type: TokenType::Semicolon,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );
    }

    #[test]
    fn test_new_line() {
        let mut tokenizer = Tokenizer::new();

        let src = "{";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "
{";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 1
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 1
                }
            ],
            tokens
        );

        let src = "
{
";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 1
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 2
                }
            ],
            tokens
        );

        let src = "
{

";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 1
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 3
                }
            ],
            tokens
        );

        let src = "{
{
{
}
}
}";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 0
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 1
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 2
                },
                Token {
                    token_type: TokenType::RightBrace,
                    line: 3
                },
                Token {
                    token_type: TokenType::RightBrace,
                    line: 4
                },
                Token {
                    token_type: TokenType::RightBrace,
                    line: 5
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 5
                }
            ],
            tokens
        );

        let src = "
{
{
{
}
}
}
";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 1
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 2
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    line: 3
                },
                Token {
                    token_type: TokenType::RightBrace,
                    line: 4
                },
                Token {
                    token_type: TokenType::RightBrace,
                    line: 5
                },
                Token {
                    token_type: TokenType::RightBrace,
                    line: 6
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 7
                }
            ],
            tokens
        );
    }

    #[test]
    fn test_operators() {
        let mut tokenizer = Tokenizer::new();

        let src = "!";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Bang,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );
        let src = "!=";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::BangEqual,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "=";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Equal,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );
        let src = "==";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::EqualEqual,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "<";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Less,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );
        let src = "<=";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LessEqual,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = ">";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Greater,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );
        let src = ">=";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::GreaterEqual,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "= == ! = != > = >= < = <=";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Equal,
                    line: 0
                },
                Token {
                    token_type: TokenType::EqualEqual,
                    line: 0
                },
                Token {
                    token_type: TokenType::Bang,
                    line: 0
                },
                Token {
                    token_type: TokenType::Equal,
                    line: 0
                },
                Token {
                    token_type: TokenType::BangEqual,
                    line: 0
                },
                Token {
                    token_type: TokenType::Greater,
                    line: 0
                },
                Token {
                    token_type: TokenType::Equal,
                    line: 0
                },
                Token {
                    token_type: TokenType::GreaterEqual,
                    line: 0
                },
                Token {
                    token_type: TokenType::Less,
                    line: 0
                },
                Token {
                    token_type: TokenType::Equal,
                    line: 0
                },
                Token {
                    token_type: TokenType::LessEqual,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );
    }

    #[test]
    fn test_comment() {
        let mut tokenizer = Tokenizer::new();

        let src = "// this is a comment";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Comment(" this is a comment".into()),
                    line: 0,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "// this is a comment
=";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Comment(" this is a comment".into()),
                    line: 0,
                },
                Token {
                    token_type: TokenType::Equal,
                    line: 1
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 1
                }
            ],
            tokens
        );
        let src = "=
// this is a comment
=";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Equal,
                    line: 0
                },
                Token {
                    token_type: TokenType::Comment(" this is a comment".into()),
                    line: 1,
                },
                Token {
                    token_type: TokenType::Equal,
                    line: 2
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 2
                }
            ],
            tokens
        );
    }

    #[test]
    fn test_string_lit() {
        let mut tokenizer = Tokenizer::new();

        let src = "\"this is a string\"";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::String("this is a string".into()),
                    line: 0,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "\"this is a multiline string
\"";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::String("this is a multiline string\n".into()),
                    line: 0,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 1
                }
            ],
            tokens
        );

        let src = "\"this is a unterminated string";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Unrecognized(TokenizationError::UntermiatedString(0)),
                    line: 0,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "\"this is a unterminated string

            ";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Unrecognized(TokenizationError::UntermiatedString(2)),
                    line: 2,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 2
                }
            ],
            tokens
        );
    }

    #[test]
    fn test_numbers() {
        let mut tokenizer = Tokenizer::new();

        let src = "1234567890";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Number(1234567890.0),
                    line: 0,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "-1";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Minus,
                    line: 0,
                },
                Token {
                    token_type: TokenType::Number(1.0),
                    line: 0,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "-3.72360";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Minus,
                    line: 0,
                },
                Token {
                    token_type: TokenType::Number(3.7236),
                    line: 0,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );

        let src = "3.7+";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Number(3.7),
                    line: 0,
                },
                Token {
                    token_type: TokenType::Plus,
                    line: 0,
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                }
            ],
            tokens
        );
    }
}
