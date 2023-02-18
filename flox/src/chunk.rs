use crate::value::*;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Return,
    Constant(usize), // TODO! change size of index to see performace changes
    Nil,
    True,
    False,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),
    GetLocal(usize),
    SetLocal(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Loop(usize),
    Call(usize),
    Closure(usize),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: ValueArray,
}

impl Chunk {
    pub const fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: ValueArray::new(),
        }
    }

    pub fn write_chunk(&mut self, byte: Op) {
        self.code.push(byte);
    }

    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {name} ==");

        for (i, instruction) in self.code.iter().enumerate() {
            print!("{i:04} ");
            match instruction {
                Op::Constant(ci) => println!("OP_CONSTANT {ci:04}"),
                Op::DefineGlobal(gi) => println!("OP_DEFINE_GLOBAL {gi:04}"),
                Op::GetGlobal(gi) => println!("OP_GET_GLOBAL {gi:04}"),
                Op::SetGlobal(gi) => println!("OP_SET_GLOBAL {gi:04}"),
                Op::JumpIfFalse(offset) => println!("OP_JUMP_IF_FALSE {offset:04}"),
                Op::Jump(offset) => println!("OP_JUMP {offset:04}"),
                _ => println!("OP_{instruction:?}"),
            }
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value)
    }
}

impl fmt::Display for Chunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "==  ==")?;

        for (i, instruction) in self.code.iter().enumerate() {
            write!(f, "{i:04} ")?;
            match instruction {
                Op::Constant(ci) => {
                    let constant = self
                        .constants
                        .get(*ci)
                        .expect("Constant index out of bounds");
                    writeln!(f, "OP_CONSTANT {constant}")?
                }
                _ => writeln!(f, "OP_{instruction:?}")?,
            }
        }
        write!(f, "")
    }
}
