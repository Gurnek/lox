use logos::Lexer;

use crate::{
    chunk::{Chunk, Op},
    obj::{Function, Object},
    scanner::Token,
    value::Value,
};

use std::{mem::discriminant, ops::Add};

const UINT8_COUNT: usize = u8::MAX as usize + 1;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Precedence {
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
}

impl Add<u8> for Precedence {
    type Output = Precedence;

    fn add(self, rhs: u8) -> Self::Output {
        use Precedence::*;

        let mut n = (self as u8) + rhs;
        if n > 10 {
            n = 10
        }
        match n {
            0 => None,
            1 => Assignment,
            2 => Or,
            3 => And,
            4 => Equality,
            5 => Comparison,
            6 => Term,
            7 => Factor,
            8 => Unary,
            9 => Call,
            _ => Primary,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FunctionType {
    Script,
    Function,
}

#[derive(PartialEq, Eq)]
enum ParseFunction {
    None,
    Grouping,
    Unary,
    Binary,
    Number,
    Literal,
    String,
    Variable,
    And,
    Or,
    Call,
}
struct ParseRule {
    prefix: ParseFunction,
    infix: ParseFunction,
    precedence: Precedence,
}

#[derive(Clone)]
struct Local<'a>(Token<'a>, usize);
struct Scope<'a> {
    function: Function,
    function_type: FunctionType,
    locals: [Option<Local<'a>>; UINT8_COUNT],
    local_count: usize,
    scope_depth: usize,
}

impl<'a> Scope<'a> {
    fn new(ftype: FunctionType) -> Scope<'a> {
        const LOCAL: Option<Local> = None;
        Scope {
            locals: [LOCAL; UINT8_COUNT],
            local_count: 0,
            scope_depth: 0,
            function_type: ftype,
            function: Function::new(0, None),
        }
    }
}

pub struct Compiler<'a, 'b> {
    lex: Option<&'b mut Lexer<'a, Token<'a>>>,
    prev: Option<Token<'a>>,
    current: Option<Token<'a>>,
    had_error: bool,
    panic_mode: bool,
    current_scope: Scope<'a>,
}

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn new(ftype: FunctionType) -> Compiler<'a, 'b> {
        let mut c = Compiler {
            lex: None,
            prev: None,
            current: None,
            had_error: false,
            panic_mode: false,
            current_scope: Scope::new(ftype),
        };
        c.current_scope.locals[0] = Local(Token::Identifier(""), 0).into();
        c.current_scope.local_count += 1;
        c
    }

    pub fn compile(mut self, lex: &'b mut Lexer<'a, Token<'a>>) -> Option<Function> {
        self.lex = Some(lex);
        self.advance();
        while !self.match_token(None) {
            self.declaration();
        }
        self.end_compilation()
    }

    fn declaration(&mut self) {
        if self.match_token(Token::Fun.into()) {
            self.fun_declaration();
        } else if self.match_token(Token::Var.into()) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.match_token(Token::Print.into()) {
            self.print_statement();
        } else if self.match_token(Token::If.into()) {
            self.if_statement();
        } else if self.match_token(Token::Return.into()) {
            self.return_statement();
        } else if self.match_token(Token::While.into()) {
            self.while_statement();
        } else if self.match_token(Token::For.into()) {
            self.for_statement();
        } else if self.match_token(Token::LeftBrace.into()) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expr_statement();
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name.");

        if self.match_token(Token::Equal.into()) {
            self.expr();
        } else {
            self.emit_op(Op::Nil);
        }
        self.consume(&Token::Semicolon, "Expect ';' after variable declaration.");
        self.define_variable(global);
    }

    fn print_statement(&mut self) {
        self.expr();
        self.consume(&Token::Semicolon, "Expect ';' after value");
        self.emit_op(Op::Print);
    }

    fn return_statement(&mut self) {
        if self.current_scope.function_type == FunctionType::Script {
            self.error("Can't return from top level code.");
        }
        if self.match_token(Token::Semicolon.into()) {
            self.emit_return();
        } else {
            self.expr();
            self.consume(&Token::Semicolon, "Expect ';' after return value.");
            self.emit_op(Op::Return);
        }
    }

    fn expr_statement(&mut self) {
        self.expr();
        self.consume(&Token::Semicolon, "Expect ';' after expression");
        self.emit_op(Op::Pop);
    }

    fn if_statement(&mut self) {
        self.consume(&Token::LeftParen, "Expect '(' after 'if'.");
        self.expr();
        self.consume(&Token::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(Op::JumpIfFalse(0));
        self.emit_op(Op::Pop.into());
        self.statement();

        let else_jump = self.emit_jump(Op::Jump(0));

        self.patch_jump(then_jump);

        self.emit_op(Op::Pop.into());
        if self.match_token(Token::Else.into()) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();
        self.consume(&Token::LeftParen, "Expect '(' after 'while'.");
        self.expr();
        self.consume(&Token::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(Op::JumpIfFalse(0));
        self.emit_op(Op::Pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_op(Op::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(&Token::LeftParen, "Expect '(' after 'for'.");
        if self.match_token(Token::Semicolon.into()) {
        } else if self.match_token(Token::Var.into()) {
            self.var_declaration();
        } else {
            self.expr_statement();
        }

        let mut loop_start = self.current_chunk().code.len();
        let mut exit_jump = usize::MAX;
        if !self.match_token(Token::Semicolon.into()) {
            self.expr();
            self.consume(&Token::Semicolon, "Expect ';' after loop condition.");

            exit_jump = self.emit_jump(Op::JumpIfFalse(0));
            self.emit_op(Op::Pop);
        }

        if !self.match_token(Token::RightParen.into()) {
            let body_jump = self.emit_jump(Op::Jump(0));
            let increment_start = self.current_chunk().code.len();
            self.expr();
            self.emit_op(Op::Pop);
            self.consume(&Token::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if exit_jump != usize::MAX {
            self.patch_jump(exit_jump);
            self.emit_op(Op::Pop);
        }
        self.end_scope();
    }

    fn block(&mut self) {
        while !self.check(Token::RightBrace.into()) && !self.check(None) {
            self.declaration();
        }
        self.consume(&Token::RightBrace, "Expect '}' after block.");
    }

    fn function(&mut self, ftype: FunctionType) {
        let mut compiler = Compiler::new(ftype);
        compiler.lex = self.lex.as_deref_mut();

        compiler.current = self.current.take();
        match ftype {
            FunctionType::Function => {
                if let Some(Token::Identifier(name)) = self.prev {
                    compiler.current_scope.function.name = name.to_string().into();
                    compiler.current_scope.scope_depth = self.current_scope.scope_depth + 1;
                }
            }
            _ => {}
        }
        compiler.consume(&Token::LeftParen, "Expect '(' after function name.");
        if !compiler.check(Token::RightParen.into()) {
            loop {
                compiler.current_scope.function.arity += 1;
                if compiler.current_scope.function.arity > 255 {
                    compiler.error_at_current("Can't have more than 255 parameters.");
                }
                let constant = compiler.parse_variable("Expect parameter name.");
                compiler.define_variable(constant);
                if !compiler.match_token(Token::Comma.into()) {
                    break;
                }
            }
        }
        compiler.consume(&Token::RightParen, "Expect ')' after parameters.");
        compiler.consume(&Token::LeftBrace, "Expect '{' before function body.");
        compiler.block();

        self.current = compiler.current.take();

        let f = compiler.end_compilation();
        //self.emit_constant(Box::new(Object::Function(f.unwrap())).into());
        let index = self.current_chunk().add_constant(Box::new(Object::Function(f.unwrap())).into());
        self.emit_op(Op::Closure(index));
    }

    fn expr(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn grouping(&mut self) {
        self.expr();
        self.consume(&Token::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_type = self.prev.take();

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            Some(Token::Minus) => self.emit_op(Op::Negate),
            Some(Token::Bang) => self.emit_op(Op::Not),
            _ => unreachable!(),
        }
    }

    fn binary(&mut self) {
        let operator_type = self.prev.take();
        let rule = self.get_rule(&operator_type);

        self.parse_precedence(rule.precedence + 1u8);

        match operator_type {
            Some(Token::Plus) => self.emit_op(Op::Add),
            Some(Token::Minus) => self.emit_op(Op::Subtract),
            Some(Token::Star) => self.emit_op(Op::Multiply),
            Some(Token::Slash) => self.emit_op(Op::Divide),
            Some(Token::BangEqual) => self.emit_ops(Op::Equal, Op::Not),
            Some(Token::EqualEqual) => self.emit_op(Op::Equal),
            Some(Token::Greater) => self.emit_op(Op::Greater),
            Some(Token::GreaterEqual) => self.emit_ops(Op::Less, Op::Not),
            Some(Token::Less) => self.emit_op(Op::Less),
            Some(Token::LessEqual) => self.emit_ops(Op::Greater, Op::Not),
            _ => unreachable!(),
        }
    }

    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_op(Op::Call(arg_count));
    }

    fn number(&mut self) {
        if let Token::Number(n) = self.prev.as_ref().unwrap() {
            self.emit_constant((*n).into());
        }
    }

    fn literal(&mut self) {
        match self.prev.as_ref().unwrap() {
            Token::False => self.emit_op(Op::False),
            Token::Nil => self.emit_op(Op::Nil),
            Token::True => self.emit_op(Op::True),
            _ => unreachable!(),
        }
    }

    fn string(&mut self) {
        if let Token::String(s) = self.prev.as_ref().unwrap() {
            self.emit_constant(Box::new(Object::String(String::from(*s))).into())
        }
    }

    fn variable(&mut self, can_assign: bool) {
        let prev = self.prev.take();
        self.named_variable(prev, can_assign);
    }

    fn and(&mut self) {
        let end_jump = self.emit_jump(Op::JumpIfFalse(0));
        self.emit_op(Op::Pop);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self) {
        let else_jump = self.emit_jump(Op::JumpIfFalse(0));
        let end_jump = self.emit_jump(Op::Jump(0));

        self.patch_jump(else_jump);
        self.emit_op(Op::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn resolve_local(&mut self, scope: Option<&Scope>, name: Option<&Token>) -> usize {
        for i in (0..self.current_scope.local_count).rev() {
            let local = match scope {
                Some(s) => &s.locals[i],
                None => &self.current_scope.locals[i],
            };
            if name.unwrap() == &local.as_ref().unwrap().0 {
                if local.as_ref().unwrap().1 == usize::MAX {
                    self.error("Can't read local variable in its own initializer.");
                }
                return i;
            }
        }
        usize::MAX
    }

    fn named_variable(&mut self, name: Option<Token>, can_assign: bool) {
        let (get_op, set_op): (Op, Op);
        let mut arg = self.resolve_local(None, name.as_ref());
        if arg != usize::MAX {
            get_op = Op::GetLocal(arg);
            set_op = Op::SetLocal(arg);
        } else {
            arg = self.identifier_constant(&name);
            get_op = Op::GetGlobal(arg);
            set_op = Op::SetGlobal(arg);
        }

        if can_assign && self.match_token(Token::Equal.into()) {
            self.expr();
            self.emit_op(set_op);
        } else {
            self.emit_op(get_op);
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = self.get_rule(&self.prev).prefix;
        if prefix_rule == ParseFunction::None {
            self.error("Expect Expression");
            return;
        }
        let can_assign = precedence <= Precedence::Assignment;
        match prefix_rule {
            ParseFunction::Unary => self.unary(),
            ParseFunction::Grouping => self.grouping(),
            ParseFunction::Number => self.number(),
            ParseFunction::Literal => self.literal(),
            ParseFunction::String => self.string(),
            ParseFunction::Variable => self.variable(can_assign),
            _ => unreachable!(),
        }

        while precedence <= self.get_rule(&self.current).precedence {
            self.advance();
            let infix_rule = self.get_rule(&self.prev).infix;
            match infix_rule {
                ParseFunction::Binary => self.binary(),
                ParseFunction::And => self.and(),
                ParseFunction::Or => self.or(),
                ParseFunction::Call => self.call(),
                _ => unreachable!(),
            }
            if self.current.is_none() {
                break;
            }
        }

        if can_assign && self.match_token(Token::Equal.into()) {
            self.error("Invalid assignment target");
        }
    }

    const fn get_rule(&self, token: &Option<Token>) -> ParseRule {
        macro_rules! parse_token {
            (Identifier) => {
                Token::Identifier(_)
            };
            (Number) => {
                Token::Number(_)
            };
            (String) => {
                Token::String(_)
            };
            ($t:ident) => {
                Token::$t
            };
        }
        macro_rules! parse_rules {
            ($token:expr, {
                $($t:ident,ParseFunction::$prefix:tt,ParseFunction::$infix:tt,Precedence::$prec:tt),*
            }) => {
                match $token {
                    $(parse_token!($t) => ParseRule {
                        prefix: ParseFunction::$prefix,
                        infix: ParseFunction::$infix,
                        precedence: Precedence::$prec
                    },)*
                }
            };
        }
        let t = match token.as_ref() {
            Some(t) => t,
            None => {
                return ParseRule {
                    prefix: ParseFunction::None,
                    infix: ParseFunction::None,
                    precedence: Precedence::None,
                }
            }
        };
        parse_rules!(t, {
            LeftParen,ParseFunction::Grouping,ParseFunction::Call,Precedence::Call,
            RightParen,ParseFunction::None,ParseFunction::None,Precedence::None,
            LeftBrace,ParseFunction::None,ParseFunction::None,Precedence::None,
            RightBrace,ParseFunction::None,ParseFunction::None,Precedence::None,
            Comma,ParseFunction::None,ParseFunction::None,Precedence::None,
            Dot,ParseFunction::None,ParseFunction::None,Precedence::None,
            Minus,ParseFunction::Unary,ParseFunction::Binary,Precedence::Term,
            Plus,ParseFunction::None,ParseFunction::Binary,Precedence::Term,
            Semicolon,ParseFunction::None,ParseFunction::None,Precedence::None,
            Slash,ParseFunction::None,ParseFunction::Binary,Precedence::Factor,
            Star,ParseFunction::None,ParseFunction::Binary,Precedence::Factor,
            Bang,ParseFunction::Unary,ParseFunction::None,Precedence::None,
            BangEqual,ParseFunction::None,ParseFunction::Binary,Precedence::Equality,
            Equal,ParseFunction::None,ParseFunction::None,Precedence::None,
            EqualEqual,ParseFunction::None,ParseFunction::Binary,Precedence::Equality,
            Less,ParseFunction::None,ParseFunction::Binary,Precedence::Comparison,
            LessEqual,ParseFunction::None,ParseFunction::Binary,Precedence::Comparison,
            Greater,ParseFunction::None,ParseFunction::Binary,Precedence::Comparison,
            GreaterEqual,ParseFunction::None,ParseFunction::Binary,Precedence::Comparison,
            Identifier,ParseFunction::Variable,ParseFunction::None,Precedence::None,
            String,ParseFunction::String,ParseFunction::None,Precedence::None,
            Number,ParseFunction::Number,ParseFunction::None,Precedence::None,
            And,ParseFunction::None,ParseFunction::And,Precedence::And,
            Class,ParseFunction::None,ParseFunction::None,Precedence::None,
            Else,ParseFunction::None,ParseFunction::None,Precedence::None,
            False,ParseFunction::Literal,ParseFunction::None,Precedence::None,
            For,ParseFunction::None,ParseFunction::None,Precedence::None,
            Fun,ParseFunction::None,ParseFunction::None,Precedence::None,
            If,ParseFunction::None,ParseFunction::None,Precedence::None,
            Nil,ParseFunction::Literal,ParseFunction::None,Precedence::None,
            Or,ParseFunction::None,ParseFunction::Or,Precedence::Or,
            Print,ParseFunction::None,ParseFunction::None,Precedence::None,
            Return,ParseFunction::None,ParseFunction::None,Precedence::None,
            Super,ParseFunction::None,ParseFunction::None,Precedence::None,
            This,ParseFunction::None,ParseFunction::None,Precedence::None,
            True,ParseFunction::Literal,ParseFunction::None,Precedence::None,
            Var,ParseFunction::None,ParseFunction::None,Precedence::None,
            While,ParseFunction::None,ParseFunction::None,Precedence::None,
            Error,ParseFunction::None,ParseFunction::None,Precedence::None
        })
    }

    fn parse_variable(&mut self, error_msg: &str) -> usize {
        self.consume(&Token::Identifier(""), error_msg);

        self.declare_variable();
        if self.current_scope.scope_depth > 0 {
            return 0;
        }

        let prev = self.prev.clone();
        self.identifier_constant(&prev)
    }

    fn identifier_constant(&mut self, name: &Option<Token>) -> usize {
        if let Token::Identifier(n) = name.as_ref().unwrap() {
            self.current_chunk()
                .add_constant(Box::new(Object::String((*n).into())).into())
        } else {
            self.error("Expected a name after var keyword.");
            0
        }
    }

    fn add_local(&mut self, name: Token<'a>) {
        if self.current_scope.local_count == UINT8_COUNT {
            self.error("Too many local variables in function.");
            return;
        }
        self.current_scope.locals[self.current_scope.local_count] = Local(name, usize::MAX).into();
        self.current_scope.local_count += 1;
    }

    fn declare_variable(&mut self) {
        if self.current_scope.scope_depth == 0 {
            return;
        }

        let name = self.prev.clone().unwrap();

        for i in (0..self.current_scope.local_count).rev() {
            let local = self.current_scope.locals[i].as_ref();
            if local.unwrap().1 != usize::MAX && local.unwrap().1 <= self.current_scope.scope_depth
            {
                break;
            }

            if local.unwrap().0 == name {
                self.error("Already a variable with a name in this scope.");
            }
        }
        self.add_local(name);
    }

    fn mark_initialized(&mut self) {
        if self.current_scope.scope_depth == 0 {
            return;
        }
        self.current_scope.locals[self.current_scope.local_count - 1]
            .as_mut()
            .unwrap()
            .1 = self.current_scope.scope_depth;
    }

    fn define_variable(&mut self, global: usize) {
        if self.current_scope.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_op(Op::DefineGlobal(global));
    }

    fn argument_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check(Token::RightParen.into()) {
            loop {
                self.expr();
                if arg_count == 255 {
                    println!("Can't have more than 255 arguments.");
                }
                arg_count += 1;
                if !self.match_token(Token::Comma.into()) {
                    break;
                }
            }
        }
        self.consume(&Token::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    #[inline(always)]
    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.current_scope.function.chunk
    }

    #[inline(always)]
    fn emit_op(&mut self, op: Op) {
        self.current_chunk().write_chunk(op)
    }

    #[inline(always)]
    fn emit_ops(&mut self, o1: Op, o2: Op) {
        self.emit_op(o1);
        self.emit_op(o2);
    }

    #[inline(always)]
    fn emit_constant(&mut self, val: Value) {
        let index = self.current_chunk().add_constant(val);
        self.emit_op(Op::Constant(index));
    }

    #[inline(always)]
    fn emit_return(&mut self) {
        self.emit_op(Op::Nil);
        self.emit_op(Op::Return);
    }

    #[inline(always)]
    fn emit_jump(&mut self, instruction: Op) -> usize {
        self.emit_op(instruction);
        self.current_chunk().code.len() - 1
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let offset = self.current_chunk().code.len() - loop_start + 1;
        if offset > usize::MAX {
            self.error("Loop body too large");
        }
        self.emit_op(Op::Loop(offset));
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk().code.len() - offset - 1;

        if jump > usize::MAX {
            self.error("Too much code to jump over.");
        }
        match self.current_chunk().code[offset] {
            Op::JumpIfFalse(ref mut j) | Op::Jump(ref mut j) => {
                *j = jump;
            }
            _ => unreachable!(),
        }
    }

    #[inline(always)]
    fn end_compilation(mut self) -> Option<Function> {
        self.emit_return();
        if self.had_error {
            None
        } else {
            self.current_scope.function.into()
        }
    }

    #[inline(always)]
    fn begin_scope(&mut self) {
        self.current_scope.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.current_scope.scope_depth -= 1;

        while self.current_scope.local_count > 0
            && self.current_scope.locals[self.current_scope.local_count - 1]
                .as_ref()
                .unwrap()
                .1
                > self.current_scope.scope_depth
        {
            self.emit_op(Op::Pop);
            self.current_scope.local_count -= 1;
        }
    }

    #[inline(always)]
    fn error_at_current(&mut self, message: &str) {
        self.error_at(&self.current, message);
        self.had_error = true;
        self.panic_mode = true;
    }

    #[inline(always)]
    fn error(&mut self, message: &str) {
        self.error_at(&self.prev, message);
        self.had_error = true;
        self.panic_mode = true;
    }

    fn error_at(&self, token: &Option<Token<'a>>, message: &str) {
        eprint!("Error ");

        match token {
            None => eprint!("at end"),
            Some(Token::Error) => {}
            Some(t) => eprint!("{t:?}"),
        }

        eprintln!(": {message}");
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.current.is_none() {
            if self.prev == Token::Semicolon.into() {
                return;
            }
            match self.current {
                Some(Token::Class) | Some(Token::Fun) | Some(Token::Var) | Some(Token::For)
                | Some(Token::If) | Some(Token::While) | Some(Token::Print)
                | Some(Token::Return) => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn advance(&mut self) {
        self.prev = self.current.take();

        loop {
            self.current = self.lex.as_mut().unwrap().next();
            match self.current {
                Some(Token::Error) => {
                    let message = self.lex.as_ref().unwrap().slice();
                    self.error_at_current(message);
                }
                _ => break,
            }
        }
    }

    fn consume(&mut self, token: &Token, message: &str) {
        if discriminant(self.current.as_ref().unwrap()) == discriminant(token) {
            self.advance();
        } else {
            self.error_at_current(message);
        }
    }

    fn match_token(&mut self, token: Option<Token>) -> bool {
        if !self.check(token) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn check(&mut self, token: Option<Token>) -> bool {
        self.current == token
    }
}
