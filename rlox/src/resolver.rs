use crate::eval::Interpreter;
use crate::parser::{Expr, Stmt};
use crate::token::Token;
use fxhash::FxHashMap;

pub trait Resolve {
    fn resolve(&self, resolver: &mut Resolver);
}

impl Resolve for Vec<Stmt> {
    fn resolve(&self, resolver: &mut Resolver) {
        for s in self {
            s.resolve(resolver)
        }
    }
}

impl Resolve for Stmt {
    fn resolve(&self, resolver: &mut Resolver) {
        resolver.resolve_stmt(self)
    }
}

impl Resolve for Expr {
    fn resolve(&self, resolver: &mut Resolver) {
        resolver.resolve_expr(self);
    }
}

#[derive(PartialEq, Clone, Copy)]
enum FunctionType {
    Function,
    Initializer,
    Method,
    None,
}

#[derive(PartialEq, Clone, Copy)]
enum ClassType {
    Class,
    None,
}

pub struct Resolver {
    pub interpreter: Interpreter,
    scopes: Vec<FxHashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Resolver {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if let Some(s) = self.scopes.last_mut() {
            if s.contains_key(&name.lexeme) {
                panic!("Already a variable with this name in scope.");
            } else {
                s.insert(name.lexeme.clone(), false);
            }
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(s) = self.scopes.last_mut() {
            s.insert(name.lexeme.clone(), true);
        }
    }

    pub fn resolve_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Block(statements) => {
                self.begin_scope();
                statements.resolve(self);
                self.end_scope();
            }
            Stmt::Var(name, initializer) => {
                self.declare(name);
                initializer.resolve(self);
                self.define(name);
            }
            Stmt::Function(name, params, body) => {
                self.declare(name);
                self.define(name);
                let stmt = Stmt::Function(name.clone(), params.to_vec(), body.to_vec());
                self.resolve_function(&stmt, FunctionType::Function);
            }
            Stmt::Expression(expr) => {
                expr.resolve(self);
            }
            Stmt::If(condition, true_branch, else_branch) => {
                condition.resolve(self);
                true_branch.resolve(self);
                match else_branch {
                    Some(b) => b.resolve(self),
                    None => {}
                }
            }
            Stmt::Print(expr) => {
                expr.resolve(self);
            }
            Stmt::Return(_, expr) => {
                if self.current_function == FunctionType::None {
                    panic!("Cannot return from top-level code.");
                }
                match expr {
                    Expr::Nil(_) => {}
                    _ => {
                        if self.current_function == FunctionType::Initializer {
                            panic!("Cannot return a value from init function");
                        }
                        expr.resolve(self)
                    }
                }
            }
            Stmt::While(condition, body) => {
                condition.resolve(self);
                body.resolve(self);
            }
            Stmt::Class(name, methods) => {
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;
                self.declare(name);
                self.define(name);

                self.begin_scope();
                self.scopes.last_mut().unwrap().insert("this".into(), true);

                for method in methods {
                    let mut declaration = FunctionType::Method;
                    if let Stmt::Function(name, _, _) = method {
                        if name.lexeme == "init" {
                            declaration = FunctionType::Initializer;
                        }
                    }
                    self.resolve_function(method, declaration);
                }

                self.end_scope();
                self.current_class = enclosing_class;
            }
        }
    }

    pub fn resolve_expr(&mut self, e: &Expr) {
        match e {
            Expr::Var(_, name) => {
                match self.scopes.last() {
                    Some(s) => {
                        if s.get(&name.lexeme).unwrap_or(&true) == &false {
                            panic!(
                                "Can't read local variable {}:{} in it's own initializer",
                                name.lexeme, name.line
                            );
                        }
                    }
                    None => panic!("Can't read local variable in it's own initializer"),
                }
                self.resolve_local(e, name);
            }
            Expr::Assign(_, name, value) => {
                value.resolve(self);
                self.resolve_local(e, name);
            }
            Expr::Binary(_, left, _, right) => {
                left.resolve(self);
                right.resolve(self);
            }
            Expr::Call(_, callee, _, args) => {
                callee.resolve(self);
                for arg in args {
                    arg.resolve(self);
                }
            }
            Expr::Grouping(_, inside) => {
                inside.resolve(self);
            }
            Expr::Logical(_, left, _, right) => {
                left.resolve(self);
                right.resolve(self);
            }
            Expr::Unary(_, _, right) => {
                right.resolve(self);
            }
            Expr::Get(_, expr, _) => expr.resolve(self),
            Expr::Set(_, obj, _, expr) => {
                expr.resolve(self);
                obj.resolve(self);
            }
            expr @ Expr::This(_, keyword) => {
                if self.current_class == ClassType::None {
                    panic!("Can't use 'this' outside of a class");
                }
                self.resolve_local(expr, keyword)
            }
            _ => {}
        }
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter.resolve(expr, i)
            }
        }
    }

    fn resolve_function(&mut self, stmt: &Stmt, function_type: FunctionType) {
        let enclosing_function = self.current_function;
        self.current_function = function_type;
        self.begin_scope();
        if let Stmt::Function(_, params, body) = stmt {
            for p in params {
                self.declare(p);
                self.define(p);
            }
            body.resolve(self);
        }
        self.end_scope();
        self.current_function = enclosing_function;
    }
}
