mod environment;
mod eval;
mod lexer;
mod parser;
mod resolver;
mod token;

extern crate fxhash;

use eval::Interpreter;
use lexer::Lexer;
use parser::{Parser, Stmt};
use resolver::{Resolve, Resolver};
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use token::Token;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

static mut HAD_ERROR: bool = false;

fn run_file(path: &String) {
    let mut reader: io::BufReader<File> = match File::open(path) {
        Ok(file) => io::BufReader::new(file),
        Err(err) => panic!("Error opening the file {:?}", err),
    };

    let mut source = String::new();
    match reader.read_to_string(&mut source) {
        Ok(_) => run(source),
        Err(error) => panic!("{}", error),
    }

    unsafe {
        if HAD_ERROR {
            panic!();
        }
    }
}

fn run_prompt() {
    let input = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush();
        let mut line = String::new();
        match input.read_line(&mut line) {
            Ok(_) => run(line),
            Err(error) => panic!("{}", error),
        }
        io::stdout().flush();
        unsafe {
            HAD_ERROR = false;
        }
    }
}

fn run(source: String) {
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer.collect::<Vec<Token>>());
    let statements = Stmt::Block(parser.parse());
    let interpreter = Interpreter::new();
    let mut resolver = Resolver::new(interpreter);
    statements.resolve(&mut resolver);
    resolver.interpreter.exec(statements);

    unsafe {
        if HAD_ERROR {
            return;
        }
    }
}

fn error(line: i32, msg: &str) {
    report(line, "".into(), msg);
}

fn report(line: i32, scope: String, msg: &str) {
    println!("[line {}] Error{}: {}", line, scope, msg);
    unsafe {
        HAD_ERROR = true;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        panic!("Usage: rlox [Script]");
    }

    if args.len() == 2 {
        run_file(&args[1])
    } else {
        run_prompt();
    }
}
