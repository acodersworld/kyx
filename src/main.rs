mod scanner;
mod opcode;
mod compiler;
mod chunk;
mod var_len_int;
mod float;
mod disassembler;
mod value;
mod vm;

use rustyline::DefaultEditor;

pub struct DefaultPrinter {}

impl vm::Printer for DefaultPrinter {
    fn print(&mut self, s: &str) {
        println!("{}", s);
    }
}

fn main() -> rustyline::Result<()> {
    let mut rl = DefaultEditor::new()?;

    loop {
        let line = rl.readline(">> ");
        match line {
            Ok(l) => {
                let mut printer = DefaultPrinter{};
                let mut machine = vm::VM::new(&mut printer);
                if let Err(e) = machine.interpret(&l) {
                    println!("Error: {}", e);
                }
            },
            Err(err) => {
                println!("{}", err);
                break;
            }
        }
    }

    Ok(())
}
