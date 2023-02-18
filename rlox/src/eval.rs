use crate::environment::Environment;
use crate::parser::{Expr, Stmt};
use crate::token::{Token, TokenType};
use core::fmt;
use fxhash::FxHashMap;
use std::cell::RefCell;
use std::hash::Hash;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone)]
pub enum LoxCallable {
    Clock,
    LoxFunction(Stmt, Rc<RefCell<Environment>>, bool),
    LoxClass(String, FxHashMap<String, LoxCallable>),
}

impl LoxCallable {
    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Val>) -> Val {
        use LoxCallable::*;
        match self {
            Clock => Val::Number(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards")
                    .as_millis() as f64,
            ),
            LoxFunction(declaration, closure, is_init) => {
                let environment = Rc::new(RefCell::new(Environment::new(closure)));
                if let Stmt::Function(_, params, body) = declaration {
                    params
                        .iter()
                        .zip(args)
                        .map(|(p, a)| environment.borrow_mut().define(p.lexeme.clone(), a))
                        .count();
                    if let Some(v) = interpreter.execute_block(body.to_vec(), environment) {
                        if *is_init {
                            return closure.borrow().get_at(0, "this");
                        } else {
                            return v;
                        }
                    }
                }
                if *is_init {
                    return closure.borrow().get_at(0, "this");
                }
                Val::Nil
            }
            l @ LoxClass(_, methods) => {
                let mut instance = Val::Instance(l.clone(), FxHashMap::default());
                if let Some(f) = methods.get("init") {
                    instance = bind(f, instance).call(interpreter, args);
                }
                instance
            }
        }
    }

    pub fn arity(&self) -> usize {
        use LoxCallable::*;
        match self {
            Clock => 0,
            LoxFunction(declaration, _, _) => match declaration {
                Stmt::Function(_, params, _) => params.len(),
                _ => panic!("Not a valid function"),
            },
            LoxClass(_, methods) => {
                if let Some(f) = methods.get("init") {
                    f.arity()
                } else {
                    0
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Val {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(LoxCallable),
    Class(LoxCallable),
    Instance(LoxCallable, FxHashMap<String, Val>),
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Number(n) => write!(f, "{n}"),
            Val::String(s) => write!(f, "{s}"),
            Val::Boolean(b) => write!(f, "{b}"),
            Val::Nil => write!(f, "null"),
            Val::Function(_) => write!(f, "<function-call>"),
            Val::Class(class) => {
                if let LoxCallable::LoxClass(name, _) = class {
                    write!(f, "{}", name)
                } else {
                    write!(f, "")
                }
            }
            Val::Instance(instance, _) => {
                if let LoxCallable::LoxClass(name, _) = instance {
                    write!(f, "{} instance", name)
                } else {
                    write!(f, "")
                }
            }
        }
    }
}

impl Add for Val {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Val::Number(f1), Val::Number(f2)) => Val::Number(f1 + f2),
            (Val::String(s1), Val::String(s2)) => Val::String(format!("{s1}{s2}")),
            args => panic!("Runtime Error: Bad arguments for op, {:?}", args),
        }
    }
}

impl Sub for Val {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Val::Number(f1), Val::Number(f2)) => Val::Number(f1 - f2),
            _ => panic!("Runtime Error: Bad arguments for op"),
        }
    }
}

impl Mul for Val {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Val::Number(f1), Val::Number(f2)) => Val::Number(f1 * f2),
            _ => panic!("Runtime Error: Bad arguments for op"),
        }
    }
}

impl Div for Val {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Val::Number(f1), Val::Number(f2)) => {
                if f2 == 0. {
                    panic!("Runtime Error: Can't divide by 0")
                } else {
                    Val::Number(f1 / f2)
                }
            }
            _ => panic!("Runtime Error: Bad arguments for op"),
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Val::Number(f1), Val::Number(f2)) => f1.eq(f2),
            (Val::String(s1), Val::String(s2)) => s1.eq(s2),
            (Val::Boolean(b1), Val::Boolean(b2)) => b1.eq(b2),
            _ => false,
        }
    }
}

impl PartialOrd for Val {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Val::Number(f1), Val::Number(f2)) => f1.partial_cmp(f2),
            (Val::String(s1), Val::String(s2)) => s1.partial_cmp(s2),
            (Val::Boolean(b1), Val::Boolean(b2)) => b1.partial_cmp(b2),
            _ => panic!("Runtime Error: Bad arguments for op"),
        }
    }
}

impl Neg for Val {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Val::Number(f1) => Val::Number(-f1),
            _ => panic!("Runtime Error: Bad arguments for op"),
        }
    }
}

impl Not for Val {
    type Output = bool;

    fn not(self) -> Self::Output {
        match self {
            Val::Number(f1) => f1 == 0.,
            Val::Boolean(b1) => !b1,
            _ => panic!("Runtime Error: Bad arguments for op"),
        }
    }
}

fn binary_ops(op: &TokenType) -> Box<fn(Val, Val) -> Val> {
    match op {
        TokenType::Plus => Box::new(|f1, f2| f1 + f2),
        TokenType::Minus => Box::new(|f1, f2| f1 - f2),
        TokenType::Star => Box::new(|f1, f2| f1 * f2),
        TokenType::Slash => Box::new(|f1, f2| f1 / f2),
        TokenType::Greater => Box::new(|f1, f2| Val::Boolean(f1 > f2)),
        TokenType::GreaterEqual => Box::new(|f1, f2| Val::Boolean(f1 >= f2)),
        TokenType::Less => Box::new(|f1, f2| Val::Boolean(f1 < f2)),
        TokenType::LessEqual => Box::new(|f1, f2| Val::Boolean(f1 <= f2)),
        TokenType::EqualEqual => Box::new(|f1, f2| Val::Boolean(f1 == f2)),
        TokenType::BangEqual => Box::new(|f1, f2| Val::Boolean(f1 != f2)),
        _ => panic!("Not a binary operator"),
    }
}

fn unary_ops(op: &TokenType) -> Box<fn(Val) -> Val> {
    match op {
        TokenType::Minus => Box::new(|v1| -v1),
        TokenType::Plus => Box::new(|v1| v1),
        TokenType::Bang => Box::new(|v1| Val::Boolean(!v1)),
        _ => panic!("Not a unary operator"),
    }
}

impl std::cmp::PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}
impl std::cmp::Eq for Expr {}

impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

fn bind(method: &LoxCallable, instance: Val) -> LoxCallable {
    if let LoxCallable::LoxFunction(body, closure, is_init) = method {
        let environment = Rc::new(RefCell::new(Environment::new(&closure)));
        environment.borrow_mut().define("this".into(), instance);
        LoxCallable::LoxFunction(body.clone(), environment, *is_init)
    } else {
        panic!("Cannot bind an instance to a non-method")
    }
}

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
    locals: FxHashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut i = Interpreter {
            globals: Rc::new(RefCell::new(Environment::global())),
            environment: Rc::new(RefCell::new(Environment::global())),
            locals: FxHashMap::default(),
        };

        i.environment = i.globals.clone();

        i.globals
            .borrow_mut()
            .define("clock".into(), Val::Function(LoxCallable::Clock));
        i
    }

    fn execute_block(
        &mut self,
        statements: Vec<Stmt>,
        env: Rc<RefCell<Environment>>,
    ) -> Option<Val> {
        let previous = self.environment.clone();

        self.environment = env;
        for s in statements {
            match self.exec(s) {
                Some(v) => {
                    self.environment = previous;
                    return Some(v);
                }
                None => continue,
            }
        }

        self.environment = previous;
        None
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.clone(), depth);
    }

    fn lookup_var(&self, expr: &Expr) -> Val {
        match expr {
            Expr::Var(_, ref name) => {
                let distance = self.locals.get(expr);
                match distance {
                    Some(d) => self.environment.borrow().get_at(*d, &name.lexeme),
                    None => self.globals.borrow().get(name),
                }
            }
            Expr::This(_, keyword) => {
                let distance = self.locals.get(expr);
                match distance {
                    Some(d) => self.environment.borrow().get_at(*d, &keyword.lexeme),
                    None => self.globals.borrow().get(keyword),
                }
            }
            _ => Val::Nil,
        }
    }

    pub fn eval(&mut self, e: &Expr) -> Val {
        match e {
            Expr::Number(_, n) => Val::Number(*n),
            Expr::String(_, s) => Val::String(s.clone()),
            Expr::Boolean(_, b) => Val::Boolean(*b),
            Expr::Grouping(_, new_e) => self.eval(new_e),
            Expr::Unary(_, op, new_e) => {
                let eval_e = self.eval(new_e);
                unary_ops(&op.token_type)(eval_e)
            }
            Expr::Binary(_, lhs, op, rhs) => {
                let eval_lhs = self.eval(lhs);
                let eval_rhs = self.eval(rhs);

                binary_ops(&op.token_type)(eval_lhs, eval_rhs)
            }
            Expr::Nil(_) => Val::Nil,
            v @ Expr::Var(_, _) => self.lookup_var(v),
            v @ Expr::Assign(_, ref t, ref new_e) => {
                let value = self.eval(new_e);
                let distance = self.locals.get(v);
                match distance {
                    Some(d) => self.environment.borrow_mut().assign_at(*d, t, &value),
                    None => self.globals.borrow_mut().assign(t, &value),
                }
                value
            }
            Expr::Logical(_, lhs, op, rhs) => {
                let left = self.eval(lhs);

                match op.token_type {
                    TokenType::Or => {
                        if left == Val::Boolean(true) {
                            return left;
                        }
                    }
                    _ => {
                        if left != Val::Boolean(true) {
                            return left;
                        }
                    }
                }

                self.eval(rhs)
            }
            Expr::Call(_, callee, _, args) => {
                let callee = self.eval(callee);

                let args: Vec<Val> = args.iter().map(|e| self.eval(e)).collect();
                match callee {
                    Val::Function(c) => {
                        if c.arity() != args.len() {
                            panic!("Expected {} args but got {}", c.arity(), args.len());
                        } else {
                            c.call(self, args)
                        }
                    }
                    Val::Class(c) => c.call(self, args),
                    _ => panic!("Not a callable entity"),
                }
            }
            Expr::Get(_, expr, name) => {
                let obj = self.eval(expr);
                if let Val::Instance(ref class, ref fields) = obj {
                    if let Some(v) = fields.get(&name.lexeme) {
                        return v.clone();
                    } else if let LoxCallable::LoxClass(_, methods) = class {
                        if let Some(m) = methods.get(&name.lexeme) {
                            Val::Function(bind(m, obj.clone()))
                        } else {
                            panic!("Undefined property {}", name.lexeme)
                        }
                    } else {
                        panic!("Undefined property {}", name.lexeme)
                    }
                } else {
                    panic!("Only instances have properties, {}", name.lexeme)
                }
            }
            Expr::Set(_, obj, name, value) => {
                let eval_obj = self.eval(obj);

                if let Val::Instance(_, mut fields) = eval_obj {
                    let eval_value = self.eval(value);
                    fields.insert(name.lexeme.clone(), eval_value.clone());
                    eval_value
                } else {
                    panic!("Only instances have fields");
                }
            }
            t @ Expr::This(_, _) => self.lookup_var(t),
        }
    }

    pub fn exec(&mut self, s: Stmt) -> Option<Val> {
        match s {
            Stmt::Expression(e) => {
                self.eval(&e);
                None
            }
            Stmt::Print(e) => {
                let v = self.eval(&e);
                match v {
                    Val::String(s) => {
                        if s == "i am liv" {
                            println!("I love you <3");
                        } else {
                            println!("{}", s);
                        }
                    }
                    _ => println!("{}", v),
                }
                None
            }
            Stmt::Var(name, value) => {
                let v = self.eval(&value);
                self.environment.borrow_mut().define(name.lexeme, v);
                None
            }
            Stmt::Block(stmts) => self.execute_block(
                stmts,
                Rc::new(RefCell::new(Environment::new(&self.environment))),
            ),
            Stmt::If(condition, true_branch, false_branch) => match self.eval(&condition) {
                Val::Boolean(b) => {
                    if b {
                        if let Some(v) = self.exec(*true_branch) {
                            return Some(v);
                        }
                    } else {
                        match false_branch {
                            Some(body) => {
                                if let Some(v) = self.exec(*body) {
                                    return Some(v);
                                }
                            }
                            None => {
                                if let Some(v) = self.exec(Stmt::Block(Vec::new())) {
                                    return Some(v);
                                }
                            }
                        };
                    }
                    None
                }
                _ => panic!("Expected boolean value in 'if' conditional"),
            },
            Stmt::While(condition, body) => {
                while self.eval(&condition) == Val::Boolean(true) {
                    match self.exec(*body.clone()) {
                        Some(v) => return Some(v),
                        None => continue,
                    };
                }
                None
            }
            Stmt::Function(name, params, body) => {
                let stmt = Stmt::Function(name.clone(), params, body);
                let function = LoxCallable::LoxFunction(stmt, self.environment.clone(), false);
                self.environment
                    .borrow_mut()
                    .define(name.lexeme, Val::Function(function));
                None
            }
            Stmt::Return(_, expr) => Some(self.eval(&expr)),
            Stmt::Class(name, methods) => {
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), Val::Nil);
                let mut method_map = FxHashMap::<String, LoxCallable>::default();
                for method in methods {
                    if let Stmt::Function(name, params, body) = method {
                        let m = Stmt::Function(name.clone(), params, body);
                        let function = LoxCallable::LoxFunction(
                            m,
                            self.environment.clone(),
                            name.lexeme == "init",
                        );
                        method_map.insert(name.lexeme, function);
                    }
                }
                let class = LoxCallable::LoxClass(name.lexeme.clone(), method_map);
                self.environment
                    .borrow_mut()
                    .assign(&name, &Val::Class(class));
                None
            }
        }
    }
}
