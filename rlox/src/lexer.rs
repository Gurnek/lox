use crate::error;
use crate::token::{Token, TokenType};
use fxhash::FxHashMap;
use std::str::from_utf8;

pub struct Lexer<'a> {
    source: &'a [u8],
    start: usize,
    current: usize,
    line: i32,
    keywords: FxHashMap<&'a str, TokenType>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        let mut keywords: FxHashMap<&str, TokenType> = FxHashMap::default();
        let pairs = [
            ("and", TokenType::And),
            ("class", TokenType::Class),
            ("else", TokenType::Else),
            ("false", TokenType::False),
            ("for", TokenType::For),
            ("fun", TokenType::Fun),
            ("if", TokenType::If),
            ("nil", TokenType::Nil),
            ("or", TokenType::Or),
            ("print", TokenType::Print),
            ("return", TokenType::Return),
            ("super", TokenType::Super),
            ("this", TokenType::This),
            ("true", TokenType::True),
            ("var", TokenType::Var),
            ("while", TokenType::While),
        ];
        for (s, t) in pairs {
            keywords.insert(s, t);
        }
        Lexer {
            source: source.as_bytes(),
            start: 0,
            current: 0,
            line: 1,
            keywords,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<&u8> {
        let c = self.source.get(self.current);
        self.current += 1;
        c
    }

    fn match_longer(&mut self, second: char) -> bool {
        if self.is_at_end() || self.source[self.current] as char != second {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn compound_operators(
        &mut self,
        second: char,
        t_token: TokenType,
        f_token: TokenType,
        t_lexeme: &str,
        f_lexeme: &str,
    ) -> Option<Token> {
        if self.match_longer(second) {
            Some(Token::new(t_token, t_lexeme.to_string(), self.line))
        } else {
            Some(Token::new(f_token, f_lexeme.to_string(), self.line))
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source[self.current] as char
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source[self.current + 1] as char
    }

    fn parse_string(&mut self) -> Option<Token> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            error(self.line, "Unterminated String.");
            None
        } else {
            self.advance();
            let s = &self.source[self.start..self.current];
            Some(Token::new(
                TokenType::String(from_utf8(&s[1..s.len() - 1]).unwrap().into()),
                from_utf8(&s[1..s.len() - 1]).unwrap().into(),
                self.line,
            ))
        }
    }

    fn parse_number(&mut self) -> Option<Token> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        let num: &str = from_utf8(&self.source[self.start..self.current]).unwrap();
        Some(Token::new(
            TokenType::Number(num.parse::<f64>().unwrap()),
            num.into(),
            self.line,
        ))
    }

    fn parse_identifier(&mut self) -> Option<Token> {
        while self.peek().is_ascii_alphanumeric() {
            self.advance();
        }
        let identifier = from_utf8(&self.source[self.start..self.current]).unwrap();
        let token_type = self.keywords.get(identifier);
        match token_type {
            Some(t) => Some(Token::new((*t).clone(), identifier.into(), self.line)),
            None => Some(Token::new(
                TokenType::Identifier(identifier.into()),
                identifier.into(),
                self.line,
            )),
        }
    }

    fn get_token(&mut self) -> Option<Token> {
        match self.advance() {
            Some(&c) => match c as char {
                '(' => Some(Token::new(
                    TokenType::LeftParen,
                    (c as char).to_string(),
                    self.line,
                )),
                ')' => Some(Token::new(
                    TokenType::RightParen,
                    (c as char).to_string(),
                    self.line,
                )),
                '{' => Some(Token::new(
                    TokenType::LeftBrace,
                    (c as char).to_string(),
                    self.line,
                )),
                '}' => Some(Token::new(
                    TokenType::RightBrace,
                    (c as char).to_string(),
                    self.line,
                )),
                ',' => Some(Token::new(
                    TokenType::Comma,
                    (c as char).to_string(),
                    self.line,
                )),
                '.' => Some(Token::new(
                    TokenType::Dot,
                    (c as char).to_string(),
                    self.line,
                )),
                '-' => Some(Token::new(
                    TokenType::Minus,
                    (c as char).to_string(),
                    self.line,
                )),
                '+' => Some(Token::new(
                    TokenType::Plus,
                    (c as char).to_string(),
                    self.line,
                )),
                ';' => Some(Token::new(
                    TokenType::Semicolon,
                    (c as char).to_string(),
                    self.line,
                )),
                '*' => Some(Token::new(
                    TokenType::Star,
                    (c as char).to_string(),
                    self.line,
                )),
                '!' => {
                    self.compound_operators('=', TokenType::BangEqual, TokenType::Bang, "!=", "!")
                }
                '=' => {
                    self.compound_operators('=', TokenType::EqualEqual, TokenType::Equal, "==", "=")
                }
                '<' => {
                    self.compound_operators('=', TokenType::LessEqual, TokenType::Less, "<=", "<")
                }
                '>' => self.compound_operators(
                    '=',
                    TokenType::GreaterEqual,
                    TokenType::Greater,
                    ">=",
                    ">",
                ),
                '/' => {
                    if self.match_longer('/') {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                        Some(Token::new(TokenType::Comment, "".to_string(), self.line))
                    } else {
                        Some(Token::new(
                            TokenType::Slash,
                            (c as char).to_string(),
                            self.line,
                        ))
                    }
                }
                ' ' | '\r' | '\t' => {
                    Some(Token::new(TokenType::Whitespace, "".to_string(), self.line))
                }
                '\n' => {
                    self.line += 1;
                    Some(Token::new(TokenType::Whitespace, "".to_string(), self.line))
                }
                '"' => self.parse_string(),
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => self.parse_number(),
                _ => {
                    if (c as char).is_ascii_alphanumeric() || (c as char) == '_' {
                        self.parse_identifier()
                    } else {
                        error(self.line, "Unexpected character");
                        None
                    }
                }
            },
            None => None,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.start = self.current;
        while let Some(t) = self.get_token() {
            if *t.token_type() == TokenType::Comment || *t.token_type() == TokenType::Whitespace {
                self.start = self.current;
                continue;
            } else {
                return Some(t);
            }
        }
        None
    }
}
