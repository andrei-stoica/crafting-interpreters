use crate::tree_walk::token::{Token, TokenType};

#[derive(Debug)]
pub struct Tokenizer {
    tokens: Vec<Token>,
    line: u32,
    current: u32,
    start: u32,
}

impl Tokenizer {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line: 0,
            current: 0,
            start: 0,
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

    pub fn parse(&mut self, source: &str) -> Vec<Token> {
        let mut src = source.chars();
        self.tokens = Vec::new();
        self.line = 0;
        self.current = 0;
        self.start = 0;

        while let Some(c) = src.nth(self.current.try_into().unwrap()) {
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
                _ => (),
            }
        }
        self.add_token_bare(TokenType::EOF);
        self.current += 1;

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

        let left_paren = "(";
        let tokens = tokenizer.parse(left_paren);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::LeftParen,
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

        let math = "-+*";
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
}
