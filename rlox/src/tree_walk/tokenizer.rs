use crate::tree_walk::token::{Token, TokenType};
use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Tokenizer {
    tokens: Vec<Token>,
    line: u32,
}

impl Tokenizer {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line: 0,
        }
    }
    fn add_token(&mut self, token_type: TokenType, lexeme: &str) {
        self.tokens.push(Token {
            token_type,
            lexeme: Some(lexeme.into()),
            line: self.line,
        });
    }

    fn add_token_bare(&mut self, token_type: TokenType) {
        self.tokens.push(Token {
            token_type,
            lexeme: None,
            line: self.line,
        });
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

    fn consume_comment(src: &mut Peekable<Chars>) {
        while let Some(c) = src.peek() {
            match c {
                '\n' => break,
                _ => src.next(),
            };
        }
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
                '(' => self.add_token_bare(TokenType::LeftParen),
                ')' => self.add_token_bare(TokenType::RightParen),
                '{' => self.add_token_bare(TokenType::LeftBrace),
                '}' => self.add_token_bare(TokenType::RightBrace),
                ',' => self.add_token_bare(TokenType::Comma),
                '.' => self.add_token_bare(TokenType::Dot),
                '-' => self.add_token_bare(TokenType::Minus),
                '+' => self.add_token_bare(TokenType::Plus),
                ';' => self.add_token_bare(TokenType::Semicolon),
                '*' => self.add_token_bare(TokenType::Star),

                // operators
                '!' if Self::consume_if('=', &mut src) => self.add_token_bare(TokenType::BangEqual),
                '!' => self.add_token_bare(TokenType::Bang),

                '=' if Self::consume_if('=', &mut src) => {
                    self.add_token_bare(TokenType::EqualEqual)
                }
                '=' => self.add_token_bare(TokenType::Equal),

                '<' if Self::consume_if('=', &mut src) => self.add_token_bare(TokenType::LessEqual),
                '<' => self.add_token_bare(TokenType::Less),

                '>' if Self::consume_if('=', &mut src) => {
                    self.add_token_bare(TokenType::GreaterEqual)
                }
                '>' => self.add_token_bare(TokenType::Greater),

                // comments
                '/' if Self::consume_if('/', &mut src) => Self::consume_comment(&mut src),
                '/' => self.add_token_bare(TokenType::Slash),
                _ => (),
            }
        }
        self.add_token_bare(TokenType::EOF);
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
                lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::RightParen,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::RightBrace,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Plus,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Star,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Slash,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Dot,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Semicolon,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 1
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 1
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 1
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    lexeme: None,
                    line: 1
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    lexeme: None,
                    line: 2
                },
                Token {
                    token_type: TokenType::RightBrace,
                    lexeme: None,
                    line: 3
                },
                Token {
                    token_type: TokenType::RightBrace,
                    lexeme: None,
                    line: 4
                },
                Token {
                    token_type: TokenType::RightBrace,
                    lexeme: None,
                    line: 5
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 1
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    lexeme: None,
                    line: 2
                },
                Token {
                    token_type: TokenType::LeftBrace,
                    lexeme: None,
                    line: 3
                },
                Token {
                    token_type: TokenType::RightBrace,
                    lexeme: None,
                    line: 4
                },
                Token {
                    token_type: TokenType::RightBrace,
                    lexeme: None,
                    line: 5
                },
                Token {
                    token_type: TokenType::RightBrace,
                    lexeme: None,
                    line: 6
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EqualEqual,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Bang,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Equal,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::BangEqual,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Greater,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Equal,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::GreaterEqual,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Less,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Equal,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::LessEqual,
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
            vec![Token {
                token_type: TokenType::EOF,
                lexeme: None,
                line: 0
            }],
            tokens
        );

        let src = "// this is a comment
=";
        let tokens = tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Equal,
                    lexeme: None,
                    line: 1
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
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
                    lexeme: None,
                    line: 0
                },
                Token {
                    token_type: TokenType::Equal,
                    lexeme: None,
                    line: 2
                },
                Token {
                    token_type: TokenType::EOF,
                    lexeme: None,
                    line: 2
                }
            ],
            tokens
        );
    }
}
