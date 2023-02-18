use std::cell::RefCell;
use std::rc::Rc;

use crate::compiler::FunctionType;
use crate::obj::{NativeFn, Object, Closure};

use crate::{
    chunk::Op,
    compiler::Compiler,
    scanner::Token,
    value::{As, Value},
};
use logos::Logos;
use rustc_hash::FxHashMap;

const FRAME_MAX: usize = 64;
const STACK_MAX: usize = FRAME_MAX * u8::MAX as usize;

#[derive(Clone)]
struct CallFrame {
    pub closure: Closure,
    pub ip: usize,
    pub offset: usize,
}

pub struct VM {
    frames: [Option<Rc<RefCell<CallFrame>>>; FRAME_MAX],
    frame_count: usize,

    stack: [Option<Value>; STACK_MAX],
    stack_top: usize,
    globals: FxHashMap<String, Value>,
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

impl VM {
    pub fn new() -> VM {
        const VAL: Option<Value> = None;
        const FRAME: Option<Rc<RefCell<CallFrame>>> = None;
        let mut vm = VM {
            frames: [FRAME; FRAME_MAX],
            frame_count: 0,
            stack: [VAL; STACK_MAX],
            stack_top: 0,
            globals: FxHashMap::default(),
        };

        vm.define_native("clock", NativeFn::Clock);
        vm.define_native("sqrt", NativeFn::Sqrt);
        vm.define_native("pow", NativeFn::Pow);
        vm
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut lex = Token::lexer(source);
        let compiler = Compiler::new(FunctionType::Script);
        let f = compiler.compile(&mut lex);
        if f.is_none() {
            return InterpretResult::CompileError;
        }
        //return InterpretResult::CompileError;
        self.stack_top += 1;
        let closure = Closure {
            function: f.unwrap(),
        };
        self.push(Box::new(Object::Closure(closure.clone())).into());

        self.call(&closure, 0);
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        macro_rules! binary_op {
            ($op:tt) => {
                {
                    match (self.peek(0), self.peek(1)) {
                        (Some(v1), Some(v2)) => {
                            if !v1.is_number() || !v2.is_number() {
                                self.runtime_error("Operands must be numbers");
                                return InterpretResult::RuntimeError;
                            }
                        }
                        _ => {
                            self.runtime_error("Operands must be numbers");
                            return InterpretResult::RuntimeError;
                        }
                    }
                    let b = self.pop().as_number();
                    let a = self.pop().as_number();
                    self.push((a $op b).into());
                }
            };
        }

        let mut frame = self.frames[self.frame_count - 1].clone();

        macro_rules! read_constant {
            ($ci:expr) => {
                frame.as_ref().unwrap().borrow().closure.function.chunk.constants.0[$ci].clone()
            }
        }

        while frame.as_ref().unwrap().borrow().ip
            != frame.as_ref().unwrap().borrow().closure.function.chunk.code.len()
        {
            let byte = frame.as_ref().unwrap().borrow().closure.function.chunk.code
                [frame.as_ref().unwrap().borrow().ip]
                .clone();
            frame.as_mut().unwrap().borrow_mut().ip += 1;
            match byte {
                Op::Return => {
                    let value = self.pop();
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return InterpretResult::Ok;
                    }
                    self.stack_top = frame.as_ref().unwrap().borrow().offset;
                    self.push(value);
                    frame = self.frames[self.frame_count - 1].clone();
                }
                Op::Constant(ci) => {
                    let constant = read_constant!(ci);
                    self.push(constant);
                    continue;
                }
                Op::Nil => self.push(Value::nil()),
                Op::True => self.push(true.into()),
                Op::False => self.push(false.into()),
                Op::Negate => {
                    match self.peek(0) {
                        Some(v) => {
                            if !v.is_number() {
                                self.runtime_error("Operand must be a number");
                                return InterpretResult::RuntimeError;
                            }
                        }
                        None => {
                            self.runtime_error("Operand must be a number");
                            return InterpretResult::RuntimeError;
                        }
                    }
                    if let Some(ref mut v) = self.stack[self.stack_top - 1] {
                        if let As::Number(ref mut n) = (*v).r#as() {
                            *n *= -1.0;
                        }
                    }
                    //let v = Value(-self.pop().0);
                    //self.push(v);
                }
                Op::Add => match (self.peek(0), self.peek(1)) {
                    (Some(v1), Some(v2)) => {
                        if v1.is_string() && v2.is_string() {
                            self.concatenate();
                        } else if v1.is_number() && v2.is_number() {
                            let b = self.pop().as_number();
                            let a = self.pop().as_number();
                            self.push((a + b).into());
                        }
                    }
                    _ => {
                        self.runtime_error("Incompatible errors for + operation");
                        return InterpretResult::RuntimeError;
                    }
                },
                Op::Subtract => binary_op!(-),
                Op::Multiply => binary_op!(*),
                Op::Divide => binary_op!(/),
                Op::Not => {
                    let v = self.pop().is_falsey();
                    self.push(v.into())
                }
                Op::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a == b).into());
                }
                Op::Greater => binary_op!(>),
                Op::Less => binary_op!(<),
                Op::Print => println!("{}", self.pop()),
                Op::Pop => {
                    self.pop();
                }
                Op::DefineGlobal(gi) => {
                    let mut name = read_constant!(gi);
                    if let As::Obj(s) = name.r#as() {
                        if let Object::String(n) = s.as_ref() {
                            self.globals
                                .insert(n.to_string(), self.peek(0).to_owned().unwrap());
                            self.pop();
                        }
                    }
                }
                Op::GetGlobal(gi) => {
                    let mut name = read_constant!(gi);
                    if let As::Obj(s) = name.r#as() {
                        if let Object::String(n) = s.as_ref() {
                            let value = match self.globals.get(n) {
                                Some(v) => v.clone(),
                                None => {
                                    self.runtime_error(&format!("Undefined variable {n}"));
                                    return InterpretResult::RuntimeError;
                                }
                            };
                            self.push(value);
                        }
                    }
                }
                Op::SetGlobal(gi) => {
                    let mut name = read_constant!(gi);
                    if let As::Obj(s) = name.r#as() {
                        if let Object::String(n) = s.as_ref() {
                            match self
                                .globals
                                .insert(n.to_string(), self.peek(0).to_owned().unwrap())
                            {
                                Some(_) => {}
                                None => {
                                    self.globals.remove(n);
                                    self.runtime_error(&format!("Undefined variable {n}"));
                                    return InterpretResult::RuntimeError;
                                }
                            }
                        }
                    }
                }
                Op::GetLocal(li) => {
                    let n = self.stack[frame.as_ref().unwrap().borrow().offset + li]
                        .as_ref()
                        .unwrap()
                        .clone();
                    self.push(n);
                }
                Op::SetLocal(li) => {
                    let offset = frame.as_ref().unwrap().borrow().offset + li;
                    self.stack[offset] = self.peek(0).clone();
                }
                Op::JumpIfFalse(offset) => {
                    if self.peek(0).as_ref().unwrap().is_falsey() {
                        frame.as_mut().unwrap().borrow_mut().ip += offset;
                    }
                }
                Op::Jump(offset) => {
                    frame.as_mut().unwrap().borrow_mut().ip += offset;
                }
                Op::Loop(offset) => {
                    frame.as_mut().unwrap().borrow_mut().ip -= offset;
                }
                Op::Call(arg_count) => {
                    let v = self.peek(arg_count).clone();
                    if !self.call_value(&v, arg_count) {
                        return InterpretResult::RuntimeError;
                    }
                    frame = self.frames[self.frame_count - 1].clone();
                }
                Op::Closure(ci) => {
                    let function = read_constant!(ci);
                    let closure = Closure {
                        function: function.as_func().clone(),
                    };
                    self.push(Box::new(Object::Closure(closure)).into())
                }
            }
        }

        InterpretResult::Ok
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    fn push(&mut self, v: Value) {
        self.stack[self.stack_top] = Some(v);
        self.stack_top += 1
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top].take().unwrap()
    }

    fn peek(&self, distance: usize) -> &Option<Value> {
        &self.stack[self.stack_top - 1 - distance]
    }

    fn concatenate(&mut self) {
        let b = self.pop().as_obj().to_owned();
        let a = self.pop().as_obj().to_owned();

        if let (Object::String(s1), Object::String(s2)) = (a, b) {
            let new_s = Box::new(Object::String(s1 + &s2));
            self.push(new_s.into());
        }
    }

    fn call_value(&mut self, callee: &Option<Value>, arg_count: usize) -> bool {
        if let Some(v) = callee {
            match v.as_obj() {
                Object::Closure(closure) => return self.call(closure, arg_count),
                Object::Native(nf) => {
                    let value = nf.call(
                        arg_count,
                        &self.stack[(self.stack_top - arg_count)..self.stack_top],
                    );
                    self.stack_top -= arg_count + 1;
                    self.push(value);
                    return true;
                }
                _ => {}
            }
        }
        self.runtime_error("Can only call functions and classes");
        false
    }

    fn call(&mut self, closure: &Closure, arg_count: usize) -> bool {
        if arg_count != closure.function.arity {
            self.runtime_error(&format!(
                "Expected {} arguments but got {}",
                closure.function.arity, arg_count
            ));
            return false;
        }

        if self.frame_count == FRAME_MAX {
            self.runtime_error("Stack overflow");
            return false;
        }
        let frame = CallFrame {
            closure: closure.clone(),
            ip: 0,
            offset: self.stack_top - arg_count - 1,
        };
        self.frames[self.frame_count] = Rc::new(RefCell::new(frame)).into();
        self.frame_count += 1;
        true
    }

    fn runtime_error(&mut self, message: &str) {
        println!("{message}");
        for i in (0..self.frame_count).rev() {
            let cf = &self.frames[i];
            let function = &cf.as_ref().unwrap().borrow().closure.function;
            match function.name.as_ref() {
                Some(n) => eprintln!("Error in function: {}", n),
                None => eprintln!("Error in script"),
            }
        }
        self.reset_stack();
    }

    fn define_native(&mut self, name: &str, function: NativeFn) {
        self.globals
            .insert(name.to_string(), Box::new(Object::Native(function)).into());
    }
}
