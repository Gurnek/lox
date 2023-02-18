mod chunk;
mod compiler;
mod obj;
mod scanner;
mod value;
mod vm;

use std::env::args;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use vm::VM;

fn repl() {
    let input = io::stdin();
    let mut vm = VM::new();
    loop {
        print!("> ");
        io::stdout().flush();
        let mut line = String::new();
        match input.read_line(&mut line) {
            Ok(_) => {
                vm.interpret(&line);
            }
            Err(error) => panic!("{}", error),
        }
        io::stdout().flush();
    }
}

fn run_file(path: &str) {
    let mut reader: io::BufReader<File> = match File::open(path) {
        Ok(file) => io::BufReader::new(file),
        Err(err) => panic!("Error opening the file {err:?}"),
    };

    let mut vm = VM::new();
    let mut source = String::new();
    match reader.read_to_string(&mut source) {
        Ok(_) => vm.interpret(&source),
        Err(error) => panic!("{error}"),
    };
}

fn main() {
    let a: Vec<String> = args().collect();

    if a.len() == 1 {
        repl();
    } else if a.len() == 2 {
        run_file(&a[1]);
    } else {
        eprintln!("Usage: flox [path]")
    }
}
