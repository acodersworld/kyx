mod builtin_functions;
mod chunk;
mod compiler;
mod disassembler;
mod float;
mod opcode;
mod read_input_type;
mod rust_function_ctx;
mod scanner;
mod test;
mod value;
mod var_len_int;
mod vm;

use rust_function_ctx::{RustFunctionCtx, RustValue};
use rustyline::DefaultEditor;
use std::env;
use std::io::Read;

pub struct DefaultPrinter {}

impl vm::Printer for DefaultPrinter {
    fn print(&mut self, s: &str) {
        eprintln!("{}", s);
    }
}

fn readlines(ctx: &mut dyn RustFunctionCtx) {
    let filename = ctx.get_parameter_string(0).unwrap();

    let mut file = match std::fs::File::open(&filename) {
        Result::Ok(f) => f,
        Result::Err(x) => {
            eprintln!("Failed to open file {}: {}", filename, x);
            ctx.set_result(rust_function_ctx::RustValue::StringVector(vec![]));
            return;
        }
    };

    let mut buf = String::new();
    if let Result::Err(e) = file.read_to_string(&mut buf) {
        eprintln!("Failed to read file {}: {}", filename, e);
        ctx.set_result(rust_function_ctx::RustValue::StringVector(vec![]));
        return;
    }

    let mut lines = vec![];
    for line in buf.lines() {
        lines.push(line.to_string());
    }

    ctx.set_result(RustValue::StringVector(lines));
}

fn register_builtin_functions(machine: &mut vm::VM) {
    machine
        .create_function("fn readlines(string) -> [string]", &readlines)
        .expect("Failed to register readlines");
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
    register_builtin_functions(&mut machine);
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
    register_builtin_functions(&mut machine);

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
