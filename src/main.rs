mod scanner;
mod opcode;
mod compiler;
mod chunk;
mod var_len_int;
mod float;
mod disassembler;

use compiler::Compiler;
use disassembler::Disassembler;

fn main() {
    let mut compiler = Compiler::new("3735928559");
    compiler.compile();
    let chunk = compiler.take_chunk();

    println!("{:?}", chunk.code);
    let mut disassembler = Disassembler::new(&chunk.code);
    disassembler.disassemble();

 //   compile("1+2");
}
