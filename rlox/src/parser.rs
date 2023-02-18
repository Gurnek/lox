use crate::error;
use crate::token::{Token, TokenType};
use core::fmt;
use core::mem::discriminant;

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(usize, Token, Box<Expr>),
    Binary(usize, Box<Expr>, Token, Box<Expr>),
    Call(usize, Box<Expr>, Token, Vec<Expr>),
    Get(usize, Box<Expr>, Token),
    Set(usize, Box<Expr>, Token, Box<Expr>),
    Grouping(usize, Box<Expr>),
    This(usize, Token),
    Unary(usize, Token, Box<Expr>),
    Var(usize, Token),
    Number(usize, f64),
    String(usize, String),
    Boolean(usize, bool),
    Logical(usize, Box<Expr>, Token, Box<Expr>),
    Nil(usize),
}

impl Expr {
    pub fn id(&self) -> usize {
        match self {
            Expr::Assign(id, _, _) => *id,
            Expr::Binary(id, _, _, _) => *id,
            Expr::Call(id, _, _, _) => *id,
            Expr::Get(id, _, _) => *id,
            Expr::Set(id, _, _, _) => *id,
            Expr::Grouping(id, _) => *id,
            Expr::This(id, _) => *id,
            Expr::Unary(id, _, _) => *id,
            Expr::Var(id, _) => *id,
            Expr::Number(id, _) => *id,
            Expr::String(id, _) => *id,
            Expr::Boolean(id, _) => *id,
            Expr::Logical(id, _, _, _) => *id,
            Expr::Nil(id) => *id,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Binary(_, left, op, right) => write!(f, "({} {} {})", op.lexeme, *left, *right),
            Expr::Call(_, callee, _, args) => {
                write!(f, "{}(", callee);
                for a in args {
                    write!(f, "{}, ", a);
                }
                write!(f, ")")
            }
            Expr::Get(_, expr, name) => write!(f, "{}.{}", expr, name.lexeme),
            Expr::Set(_, obj, name, expr) => write!(f, "{}{} = {}", obj, name.lexeme, expr),
            Expr::Grouping(_, exp) => write!(f, "(group {})", *exp),
            Expr::This(_, _) => write!(f, "this."),
            Expr::Unary(_, op, right) => write!(f, "({} {})", op.lexeme, *right),
            Expr::Var(_, t) => write!(f, "{}", t.lexeme),
            Expr::Number(_, t) => write!(f, "{}", t),
            Expr::String(_, s) => write!(f, "{}", s),
            Expr::Boolean(_, b) => write!(f, "{}", b),
            Expr::Nil(_) => write!(f, "Nil"),
            Expr::Assign(_, t, e) => write!(f, "{} = {}", t.lexeme, e),
            Expr::Logical(_, lhs, op, rhs) => write!(f, "({} {} {})", op.lexeme, *lhs, *rhs),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Class(Token, Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Function(Token, Vec<Token>, Vec<Stmt>),
    Expression(Expr),
    While(Expr, Box<Stmt>),
    Print(Expr),
    Return(Token, Expr),
    Var(Token, Expr),
}

enum FunctionType {
    Function,
    Method,
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionType::Function => write!(f, "function"),
            FunctionType::Method => write!(f, "method"),
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    id: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
            id: 0,
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration());
        }
        statements
    }

    fn is_at_end(&self) -> bool {
        self.current == self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, t: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        match self.peek() {
            Some(token) => discriminant(&token.token_type) == discriminant(t),
            None => false,
        }
    }

    fn consume(&mut self, token_type: &TokenType, msg: &str) -> &Token {
        if self.check(token_type) {
            self.advance()
        } else {
            panic!("{}{}", self.peek().unwrap(), msg)
        }
    }

    fn match_types(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                break;
            } else {
                match self.peek() {
                    Some(t) => match t.token_type {
                        TokenType::Class
                        | TokenType::Fun
                        | TokenType::Var
                        | TokenType::For
                        | TokenType::If
                        | TokenType::While
                        | TokenType::Print
                        | TokenType::Return => break,
                        _ => self.advance(),
                    },
                    None => break,
                };
            }
        }
    }

    fn expression(&mut self) -> Expr {
        self.assignment()
    }

    fn declaration(&mut self) -> Stmt {
        if self.match_types(&[TokenType::Class]) {
            self.class_declaration()
        } else if self.match_types(&[TokenType::Fun]) {
            self.function(FunctionType::Function)
        } else if self.match_types(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn class_declaration(&mut self) -> Stmt {
        let name = self
            .consume(&TokenType::Identifier("".into()), "Expected class name")
            .clone();
        self.consume(&TokenType::LeftBrace, "Expect '{' after class name");

        let mut methods = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function(FunctionType::Method));
        }
        self.consume(&TokenType::RightBrace, "Expect '}' after class body");

        Stmt::Class(name, methods)
    }

    fn var_declaration(&mut self) -> Stmt {
        let name = self
            .consume(&TokenType::Identifier("".into()), "Expected variable name")
            .clone();
        let initializer = if self.match_types(&[TokenType::Equal]) {
            self.expression()
        } else {
            panic!("Must initialize variable");
        };
        self.consume(&TokenType::Semicolon, "Expect ; after variable declaration");
        Stmt::Var(name, initializer)
    }

    fn while_statement(&mut self) -> Stmt {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'");
        let condition = self.expression();
        self.consume(&TokenType::RightParen, "Expect ')' after condition");
        let body = self.statement();
        Stmt::While(condition, Box::new(body))
    }

    fn statement(&mut self) -> Stmt {
        if self.match_types(&[TokenType::For]) {
            self.for_statement()
        } else if self.match_types(&[TokenType::If]) {
            self.if_statement()
        } else if self.match_types(&[TokenType::Print]) {
            self.print_statement()
        } else if self.match_types(&[TokenType::Return]) {
            self.return_statement()
        } else if self.match_types(&[TokenType::While]) {
            self.while_statement()
        } else if self.match_types(&[TokenType::LeftBrace]) {
            Stmt::Block(self.block())
        } else {
            self.expression_statement()
        }
    }

    fn for_statement(&mut self) -> Stmt {
        self.consume(&TokenType::LeftParen, "Expected '(' after 'for'");

        let initializer = if self.match_types(&[TokenType::Semicolon]) {
            None
        } else if self.match_types(&[TokenType::Var]) {
            Some(self.var_declaration())
        } else {
            Some(self.expression_statement())
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression())
        } else {
            None
        };
        self.consume(&TokenType::Semicolon, "Expect ';' after loop condition");

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression())
        } else {
            None
        };
        self.consume(&TokenType::RightParen, "Expect ')' after for clauses.");

        let mut body = self.statement();

        if let Some(i) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(i)]);
        }

        let condition = match condition {
            Some(c) => c,
            None => {
                self.id += 1;
                Expr::Boolean(self.id, true)
            }
        };
        body = Stmt::While(condition, Box::new(body));

        if let Some(s) = initializer {
            body = Stmt::Block(vec![s, body]);
        }
        body
    }

    fn if_statement(&mut self) -> Stmt {
        self.consume(&TokenType::LeftParen, "Expected '(' after 'if'");
        let condition = self.expression();
        self.consume(&TokenType::RightParen, "Expected ')' after if condition");

        let true_branch = self.statement();
        let false_branch = if self.match_types(&[TokenType::Else]) {
            Some(Box::new(self.statement()))
        } else {
            None
        };
        Stmt::If(condition, Box::new(true_branch), false_branch)
    }

    fn print_statement(&mut self) -> Stmt {
        let value = self.expression();
        self.consume(&TokenType::Semicolon, "Expected ; after value");
        Stmt::Print(value)
    }

    fn return_statement(&mut self) -> Stmt {
        let keyword = self.previous().clone();
        let value = if self.check(&TokenType::Semicolon) {
            self.id += 1;
            Expr::Nil(self.id)
        } else {
            self.expression()
        };
        self.consume(&TokenType::Semicolon, "Expected ';' after return value");
        Stmt::Return(keyword, value)
    }

    fn expression_statement(&mut self) -> Stmt {
        let expr = self.expression();
        self.consume(&TokenType::Semicolon, "Expected ; after value");
        Stmt::Expression(expr)
    }

    fn function(&mut self, kind: FunctionType) -> Stmt {
        let name = self
            .consume(
                &TokenType::Identifier("".into()),
                &format!("Expected {kind} name"),
            )
            .clone();
        self.consume(
            &TokenType::LeftParen,
            &format!("Expect '(' after {kind} name"),
        );

        let mut params = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() > 255 {
                    panic!("Can't have more than 255 params");
                }

                params.push(
                    self.consume(&TokenType::Identifier("".into()), "Expected param name")
                        .clone(),
                );
                if !self.match_types(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        self.consume(&TokenType::RightParen, "Expect ')' after params");

        self.consume(
            &TokenType::LeftBrace,
            &format!("Expected '{{' before {kind} body"),
        );
        let body = self.block();
        Stmt::Function(name, params, body)
    }

    fn block(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration());
        }

        self.consume(&TokenType::RightBrace, "Expected '}' after block");
        statements
    }

    fn assignment(&mut self) -> Expr {
        let expr = self.or();

        if self.match_types(&[TokenType::Equal]) {
            let v = self.assignment();

            match expr {
                Expr::Var(_, t) => {
                    self.id += 1;
                    Expr::Assign(self.id, t, Box::new(v))
                }
                Expr::Get(_, obj, name) => {
                    self.id += 1;
                    Expr::Set(self.id, obj, name, Box::new(v))
                }
                _ => panic!("Invalid Assignment target"),
            }
        } else {
            expr
        }
    }

    fn or(&mut self) -> Expr {
        let mut expr = self.and();

        while self.match_types(&[TokenType::Or]) {
            let op = self.previous().clone();
            let right = self.and();
            self.id += 1;
            expr = Expr::Logical(self.id, Box::new(expr), op, Box::new(right))
        }

        expr
    }

    fn and(&mut self) -> Expr {
        let mut expr = self.equality();

        while self.match_types(&[TokenType::And]) {
            let op = self.previous().clone();
            let right = self.equality();
            self.id += 1;
            expr = Expr::Logical(self.id, Box::new(expr), op, Box::new(right))
        }

        expr
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison();
            self.id += 1;
            expr = Expr::Binary(self.id, Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self.match_types(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term();
            self.id += 1;
            expr = Expr::Binary(self.id, Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while self.match_types(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor();
            self.id += 1;
            expr = Expr::Binary(self.id, Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while self.match_types(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary();
            self.id += 1;
            expr = Expr::Binary(self.id, Box::new(expr), operator, Box::new(right));
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        if self.match_types(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary();
            self.id += 1;
            Expr::Unary(self.id, operator, Box::new(right))
        } else {
            self.call()
        }
    }

    fn finish_call(&mut self, callee: Expr) -> Expr {
        let mut args = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if args.len() >= 255 {
                    panic!("Can't have more than 255 args.");
                }
                args.push(self.expression());
                if !self.match_types(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self
            .consume(&TokenType::RightParen, "Expected ')' after args.")
            .clone();
        self.id += 1;
        Expr::Call(self.id, Box::new(callee), paren, args)
    }

    fn call(&mut self) -> Expr {
        let mut expr = self.primary();

        loop {
            if self.match_types(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr);
            } else if self.match_types(&[TokenType::Dot]) {
                let name = self
                    .consume(
                        &TokenType::Identifier("".into()),
                        "Expect property name after '.'",
                    )
                    .clone();
                self.id += 1;
                expr = Expr::Get(self.id, Box::new(expr), name);
            } else {
                break;
            }
        }

        expr
    }

    fn primary(&mut self) -> Expr {
        self.id += 1;
        if self.match_types(&[TokenType::False]) {
            Expr::Boolean(self.id, false)
        } else if self.match_types(&[TokenType::True]) {
            Expr::Boolean(self.id, true)
        } else if self.match_types(&[TokenType::Nil]) {
            Expr::Nil(self.id)
        } else if self.match_types(&[TokenType::Number(0.)]) {
            match &self.previous().token_type {
                TokenType::Number(n) => Expr::Number(self.id, *n),
                e => {
                    error(
                        self.previous().line,
                        &format!("Parse Error: Expected a number but got {:?}", e),
                    );
                    Expr::Number(self.id, 0.)
                }
            }
        } else if self.match_types(&[TokenType::String("".into())]) {
            match &self.previous().token_type {
                TokenType::String(s) => Expr::String(self.id, s.to_string()),
                e => {
                    error(
                        self.previous().line,
                        &format!("Parse Error: Expected a string but got {:?}", e),
                    );
                    Expr::String(self.id, "".into())
                }
            }
        } else if self.match_types(&[TokenType::LeftParen]) {
            let expr = self.expression();
            self.consume(&TokenType::RightParen, "Expect ')' after expression");
            Expr::Grouping(self.id, Box::new(expr))
        } else if self.match_types(&[TokenType::This]) {
            Expr::This(self.id, self.previous().clone())
        } else if self.match_types(&[TokenType::Identifier("".into())]) {
            Expr::Var(self.id, self.previous().clone())
        } else {
            let t = self.peek();
            match t {
                Some(token) => panic!(
                    "{}",
                    &format!(
                        "{} at {}, {}",
                        token.line, token.lexeme, "Expected expression"
                    )
                ),
                None => panic!("Error at EOF, Expected expression"),
            }
        }
    }
}
