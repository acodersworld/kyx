mod scanner;
mod opcode;
mod compiler;
mod chunk;
mod var_len_int;
mod float;
mod disassembler;
mod value;
mod vm;

use compiler::Compiler;
use disassembler::Disassembler;
use value::FromValue;

fn main() {
    let src = "3.1 + 4.8";
    println!("SRC: {}", src);
    let mut compiler = Compiler::new(src);
    if let Err(e) = compiler.compile() {
        println!("Error: {}", e);
        return;
    }

    let chunk = compiler.take_chunk();

    println!("{:?}", chunk.code);
    let mut disassembler = Disassembler::new(&chunk.code);
    disassembler.disassemble();

    let mut machine = vm::VM::new();
    if let Err(e) = machine.interpret(src) {
        println!("Error: {}", e);
    }

    println!("TOP: {:?}", machine.top());
 //   compile("1+2");
 //
    let v: value::Value = value::Value::Float(3.142);

    let f = f32::from_value(&v);
}
