use crate::obj::{Function, ObjType, Object};
use std::cmp::PartialEq;
use std::convert::From;
use std::fmt;

#[derive(Clone, PartialEq, Eq, Debug)]
enum ValueType {
    Bool,
    Nil,
    Number,
    Obj,
}

#[derive(Clone, Debug)]
pub enum As {
    Boolean(bool),
    Number(f64),
    Obj(Box<Object>),
}

#[derive(Clone, Debug)]
pub struct Value {
    vtype: ValueType,
    r#as: As,
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value {
            vtype: ValueType::Number,
            r#as: As::Number(value),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value {
            vtype: ValueType::Bool,
            r#as: As::Boolean(value),
        }
    }
}

impl From<Box<Object>> for Value {
    fn from(value: Box<Object>) -> Self {
        Value {
            vtype: ValueType::Obj,
            r#as: As::Obj(value),
        }
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        if self.vtype != other.vtype {
            return false;
        }
        match self.vtype {
            ValueType::Bool => self.as_bool() == other.as_bool(),
            ValueType::Nil => true,
            ValueType::Number => self.as_number() == other.as_number(),
            ValueType::Obj => self.as_obj() == other.as_obj(),
        }
    }
}

impl Eq for Value {}

impl Value {
    #[inline(always)]
    pub fn nil() -> Value {
        Value {
            vtype: ValueType::Nil,
            r#as: As::Number(0.0),
        }
    }

    #[inline(always)]
    pub fn as_bool(&self) -> bool {
        if let As::Boolean(b) = self.r#as {
            b
        } else {
            panic!("Cannot use value of type {:?} as boolean", self.vtype);
        }
    }

    #[inline(always)]
    pub fn as_number(&self) -> f64 {
        if let As::Number(n) = self.r#as {
            n
        } else {
            panic!("Cannot use value of type {:?} as number", self.vtype);
        }
    }

    #[inline(always)]
    pub fn as_obj(&self) -> &Object {
        if let As::Obj(o) = &self.r#as {
            o
        } else {
            panic!("Cannot use value of type {:?} as object", self.vtype);
        }
    }

    pub fn as_func(&self) -> &Function {
        if let Object::Function(f) = self.as_obj() {
            f
        } else {
            panic!("Cannot use value of type {:?} as function", self.vtype);
        }
    }

    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        self.vtype == ValueType::Bool
    }

    #[inline(always)]
    pub fn is_nil(&self) -> bool {
        self.vtype == ValueType::Nil
    }

    #[inline(always)]
    pub fn is_number(&self) -> bool {
        self.vtype == ValueType::Number
    }

    #[inline(always)]
    pub fn is_obj(&self) -> bool {
        self.vtype == ValueType::Obj
    }

    #[inline(always)]
    pub fn is_obj_type(&self, otype: ObjType) -> bool {
        self.is_obj() && self.as_obj().otype() == otype
    }

    #[inline(always)]
    pub fn is_string(&self) -> bool {
        self.is_obj_type(ObjType::String)
    }

    #[inline(always)]
    pub fn is_func(&self) -> bool {
        self.is_obj_type(ObjType::Function)
    }

    #[inline(always)]
    pub fn r#as(&mut self) -> &mut As {
        &mut self.r#as
    }

    #[inline(always)]
    pub fn is_falsey(&self) -> bool {
        self.is_nil() || (self.is_bool() && !self.as_bool())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueArray(pub Vec<Value>);

impl ValueArray {
    pub const fn new() -> ValueArray {
        ValueArray(Vec::new())
    }

    pub fn push(&mut self, v: Value) -> usize {
        self.0.push(v);
        self.0.len() - 1
    }

    pub fn get(&self, index: usize) -> Option<&Value> {
        self.0.get(index)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_bool() {
            write!(f, "{}", self.as_bool())
        } else if self.is_number() {
            write!(f, "{}", self.as_number())
        } else if self.is_obj() {
            match self.as_obj() {
                Object::String(s) => write!(f, "{s}"),
                Object::Function(Function { name, .. }) => match name {
                    Some(n) => write!(f, "<{n}>"),
                    None => write!(f, "<script>"),
                },
                Object::Native(_) => write!(f, "<native fn>"),
                Object::Closure(c) => {
                    match &c.function.name {
                        Some(n) => write!(f, "<{n}>"),
                        None => write!(f, "<script>")
                    }
                }
            }
        } else {
            write!(f, "nil")
        }
    }
}
