use core::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
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
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(String),
    String(String),
    Number(f64),
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
    Comment,
    Whitespace,
}

impl From<String> for TokenType {
    fn from(other: String) -> TokenType {
        TokenType::Identifier(other)
    }
}

impl<'a> From<&'a str> for TokenType {
    fn from(other: &'a str) -> TokenType {
        TokenType::Identifier(other.to_string())
    }
}

impl From<f64> for TokenType {
    fn from(other: f64) -> TokenType {
        TokenType::Number(other)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: i32,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: i32) -> Token {
        Token {
            token_type,
            lexeme,
            line,
        }
    }

    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.token_type, self.lexeme)
    }
}
