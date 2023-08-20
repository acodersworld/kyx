mod chunk;
mod compiler;
mod disassembler;
mod float;
mod opcode;
mod read_input_type;
mod scanner;
mod value;
mod var_len_int;
mod vm;

use rustyline::DefaultEditor;
use std::env;
use std::io::Read;

pub struct DefaultPrinter {}

impl vm::Printer for DefaultPrinter {
    fn print(&mut self, s: &str) {
        println!("{}", s);
    }
}

fn repl() {
    let mut rl = {
        match DefaultEditor::new() {
            Ok(x) => x,
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    };

    let mut printer = DefaultPrinter {};
    let mut machine = vm::VM::new(&mut printer);
    loop {
        let line = rl.readline(">> ");
        match line {
            Ok(l) => {
                if let Err(e) = machine.interpret(&l) {
                    println!("Error: {}", e);
                }
            }
            Err(err) => {
                println!("{}", err);
                break;
            }
        }
    }
}

fn run_file(filename: &str) {
    let mut file = {
        match std::fs::File::open(filename) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("Unable to open file {}. {}", filename, e);
                std::process::exit(1);
            }
        }
    };

    let mut contents = String::new();

    if let Err(e) = file.read_to_string(&mut contents) {
        eprintln!("Failed to read file {}. {}", filename, e);
        std::process::exit(1);
    }

    println!("{}", contents);
    let mut printer = DefaultPrinter {};
    let mut machine = vm::VM::new(&mut printer);
    if let Err(e) = machine.interpret(&contents) {
        println!("Error: {}", e);
    }
}

fn main() {
    let args = env::args();

    if args.len() == 2 {
        run_file(&args.last().unwrap());
    } else {
        repl();
    }
}
