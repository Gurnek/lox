use logos::{Lexer, Logos};

fn parse_num<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<f64> {
    let slice = lex.slice();
    slice.parse::<f64>().ok()
}

fn parse_string<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(&slice[1..slice.len() - 1])
}

fn parse_identifier<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let slice = lex.slice();
    Some(slice)
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token<'source> {
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("-")]
    Minus,

    #[token("+")]
    Plus,

    #[token(";")]
    Semicolon,

    #[token("/")]
    Slash,

    #[token("*")]
    Star,

    #[token("!")]
    Bang,

    #[token("!=")]
    BangEqual,

    #[token("=")]
    Equal,

    #[token("==")]
    EqualEqual,

    #[token(">")]
    Greater,

    #[token(">=")]
    GreaterEqual,

    #[token("<")]
    Less,

    #[token("<=")]
    LessEqual,

    #[regex("[a-zA-Z][_a-zA-Z0-9]*", parse_identifier)]
    Identifier(&'source str),

    #[regex(r#""[^"]*""#, parse_string)]
    String(&'source str),

    #[regex("([0-9]*[.])?[0-9]+", parse_num)]
    Number(f64),

    #[token("&&")]
    And,

    #[token("class")]
    Class,

    #[token("else")]
    Else,

    #[token("false")]
    False,

    #[token("for")]
    For,

    #[token("fun")]
    Fun,

    #[token("if")]
    If,

    #[token("nil")]
    Nil,

    #[token("||")]
    Or,

    #[token("print")]
    Print,

    #[token("return")]
    Return,

    #[token("super")]
    Super,

    #[token("this")]
    This,

    #[token("true")]
    True,

    #[token("var")]
    Var,

    #[token("while")]
    While,

    #[error]
    #[regex(r"[ \t\n]+", logos::skip)]
    Error,
}
