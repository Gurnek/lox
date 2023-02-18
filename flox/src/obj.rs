use std::time::{SystemTime, UNIX_EPOCH};

use crate::{chunk::Chunk, value::Value};

#[derive(Clone, PartialEq, Eq)]
pub enum ObjType {
    String,
    Function,
    NativeFn,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum NativeFn {
    Clock,
    Sqrt,
    Pow,
}

impl NativeFn {
    pub fn call(&self, arg_count: usize, args: &[Option<Value>]) -> Value {
        match self {
            NativeFn::Clock => (SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards")
                .as_millis() as f64)
                .into(),
            NativeFn::Sqrt => {
                args[0].as_ref().unwrap().as_number().sqrt().into()
            }
            NativeFn::Pow => {
                // x ^ y
                let x = args[0].as_ref().unwrap().as_number();
                let y = args[1].as_ref().unwrap().as_number();
                x.powf(y).into()
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<String>,
}

impl Function {
    pub fn new(arity: usize, name: Option<&str>) -> Function {
        Function {
            arity,
            name: name.map(str::to_string),
            chunk: Chunk::new(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Closure {
    pub function: Function,
}


#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Object {
    String(String),
    Function(Function),
    Native(NativeFn),
    Closure(Closure),
}

impl Object {
    pub const fn otype(&self) -> ObjType {
        match self {
            Object::String(_) => ObjType::String,
            Object::Function { .. } => ObjType::Function,
            Object::Native(_) => ObjType::NativeFn,
            Object::Closure(_) => ObjType::Function,
        }
    }

    pub fn new_func() -> Object {
        Object::Function(Function::new(0, None))
    }
}
