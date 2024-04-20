use crate::tree_walk::token::{Token, TokenType};
use std::collections::HashMap;
use std::{fmt::Display, iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Tokenizer {
    pub tokens: Vec<Token>,
    keywords: HashMap<String, TokenType>,
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
        // keywords should be in a lazy cell when that makes it to stable:
        // https://doc.rust-lang.org/std/cell/struct.LazyCell.html
        let mut keywords = HashMap::new();
        keywords.insert("and".into(), TokenType::And);
        keywords.insert("class".into(), TokenType::Class);
        keywords.insert("else".into(), TokenType::Else);
        keywords.insert("false".into(), TokenType::False);
        keywords.insert("for".into(), TokenType::For);
        keywords.insert("fun".into(), TokenType::Fun);
        keywords.insert("if".into(), TokenType::If);
        keywords.insert("nil".into(), TokenType::Nil);
        keywords.insert("or".into(), TokenType::Or);
        keywords.insert("print".into(), TokenType::Print);
        keywords.insert("return".into(), TokenType::Return);
        keywords.insert("super".into(), TokenType::Super);
        keywords.insert("this".into(), TokenType::This);
        keywords.insert("true".into(), TokenType::True);
        keywords.insert("var".into(), TokenType::Var);
        keywords.insert("while".into(), TokenType::While);

        Self {
            tokens: Vec::new(),
            keywords,
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
    fn consume_identifier(&mut self, src: &mut Peekable<Chars>, c: char) -> TokenType {
        let mut content: String = String::from(c);
        while let Some(c) = src.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' => {
                    content.push(src.next().expect("Identifier char"));
                }
                _ => break,
            }
        }
        match self.keywords.get(&content) {
            None => TokenType::Identifier(content),
            Some(token_type) => token_type.clone(),
        }
    }

    pub fn parse(&mut self, text: &str) {
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
                'a'..='z' | 'A'..='Z' => {
                    let identifier = self.consume_identifier(&mut src, c);
                    self.add_token(identifier);
                }
                _ => (),
            }
        }
        self.add_token(TokenType::EOF);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_single_char_tokens() {
        let mut tokenizer = Tokenizer::new();

        let eof = "";
        tokenizer.parse(eof);
        assert_eq!(
            vec![Token {
                token_type: TokenType::EOF,
                line: 0
            }],
            tokenizer.tokens
        );

        let parens = "()";
        tokenizer.parse(parens);
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
            tokenizer.tokens
        );

        let braces = "{}";
        tokenizer.parse(braces);
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
            tokenizer.tokens
        );

        let math = "-+*/";
        tokenizer.parse(math);
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
            tokenizer.tokens
        );

        let punctuation = ",.;";
        tokenizer.parse(punctuation);
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
            tokenizer.tokens
        );
    }

    #[test]
    fn test_new_line() {
        let mut tokenizer = Tokenizer::new();

        let src = "{";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "
{";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "
{
";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "
{

";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "{
{
{
}
}
}";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "
{
{
{
}
}
}
";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
    }

    #[test]
    fn test_operators() {
        let mut tokenizer = Tokenizer::new();

        let src = "!";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
        let src = "!=";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "=";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
        let src = "==";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "<";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
        let src = "<=";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = ">";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
        let src = ">=";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "= == ! = != > = >= < = <=";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
    }

    #[test]
    fn test_comment() {
        let mut tokenizer = Tokenizer::new();

        let src = "// this is a comment";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "// this is a comment
=";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
        let src = "=
// this is a comment
=";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
    }

    #[test]
    fn test_string_lit() {
        let mut tokenizer = Tokenizer::new();

        let src = "\"this is a string\"";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "\"this is a multiline string
\"";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "\"this is a unterminated string";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "\"this is a unterminated string

            ";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
    }

    #[test]
    fn test_numbers() {
        let mut tokenizer = Tokenizer::new();

        let src = "1234567890";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "-1";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "-3.72360";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );

        let src = "3.7+";
        tokenizer.parse(src);
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
            tokenizer.tokens
        );
    }

    #[test]
    fn test_identifiers() {
        let mut tokenizer = Tokenizer::new();

        let src = "and class else false for fun if nil or print return super this true var while";
        tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::And,
                    line: 0
                },
                Token {
                    token_type: TokenType::Class,
                    line: 0
                },
                Token {
                    token_type: TokenType::Else,
                    line: 0
                },
                Token {
                    token_type: TokenType::False,
                    line: 0
                },
                Token {
                    token_type: TokenType::For,
                    line: 0
                },
                Token {
                    token_type: TokenType::Fun,
                    line: 0
                },
                Token {
                    token_type: TokenType::If,
                    line: 0
                },
                Token {
                    token_type: TokenType::Nil,
                    line: 0
                },
                Token {
                    token_type: TokenType::Or,
                    line: 0
                },
                Token {
                    token_type: TokenType::Print,
                    line: 0
                },
                Token {
                    token_type: TokenType::Return,
                    line: 0
                },
                Token {
                    token_type: TokenType::Super,
                    line: 0
                },
                Token {
                    token_type: TokenType::This,
                    line: 0
                },
                Token {
                    token_type: TokenType::True,
                    line: 0
                },
                Token {
                    token_type: TokenType::Var,
                    line: 0
                },
                Token {
                    token_type: TokenType::While,
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                },
            ],
            tokenizer.tokens
        );

        let src = "these are identifiers and keywords";
        tokenizer.parse(src);
        assert_eq!(
            vec![
                Token {
                    token_type: TokenType::Identifier("these".into()),
                    line: 0
                },
                Token {
                    token_type: TokenType::Identifier("are".into()),
                    line: 0
                },
                Token {
                    token_type: TokenType::Identifier("identifiers".into()),
                    line: 0
                },
                Token {
                    token_type: TokenType::And,
                    line: 0
                },
                Token {
                    token_type: TokenType::Identifier("keywords".into()),
                    line: 0
                },
                Token {
                    token_type: TokenType::EOF,
                    line: 0
                },
            ],
            tokenizer.tokens
        );
    }
}
