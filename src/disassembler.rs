use crate::opcode;
use crate::var_len_int;
use crate::float;

pub struct Disassembler<'a> {
    code: &'a [u8],
    offset: usize
}

impl<'a> Disassembler<'a> {
    pub fn new(code: &'a [u8]) -> Disassembler {
        Disassembler {
            code,
            offset: 0
        }
    }

    pub fn disassemble(self: &mut Self) {
        while self.offset < self.code.len() {
            let offset = self.offset;
            self.offset += 1;
            
            match self.code[offset] {
                opcode::CONSTANT_INTEGER => self.constant_integer_instruction(),
                opcode::CONSTANT_FLOAT => self.constant_float_instruction(),
                opcode::CONSTANT_STRING => self.constant_string_instruction(),
                opcode::PUSH_GLOBAL => self.push_global(),
                opcode::DEFINE_GLOBAL_CONSTANT => self.define_global_constant(),
                opcode::ADDI => self.simple_instruction("addi"),
                opcode::SUBI => self.simple_instruction("subi"),
                opcode::MULI => self.simple_instruction("muli"),
                opcode::DIVI => self.simple_instruction("divi"),
                opcode::ADDF => self.simple_instruction("addf"),
                opcode::SUBF => self.simple_instruction("subf"),
                opcode::MULF => self.simple_instruction("mulf"),
                opcode::DIVF => self.simple_instruction("divf"),
                opcode::PRINT => self.simple_instruction("print"),
                _ => {
                    println!("Unknown instruction")
                }
            }
        }
    }

    fn simple_instruction(self: &mut Self, name: &str) {
        println!("{}", name);
    }

    fn constant_integer_instruction(self: &mut Self) {
        let mut decoder = var_len_int::Decoder::new(); 
        while !decoder.step_decode(self.code[self.offset]) {
            self.offset += 1;
        }
        self.offset += 1;

        println!("CONSTANT INT: {}", decoder.val());
    }

    fn constant_float_instruction(self: &mut Self) {
        let value = float::decode(&self.code[self.offset..self.offset+4].try_into().unwrap());
        self.offset += 4;

        println!("CONSTANT FLOAT: {}", value);
    }

    fn constant_string_instruction(self: &mut Self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("CONSTANT STRING: idx({})", idx);
    }

    fn push_global(self: &mut Self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("PUSH GLOBAL: idx({})", idx);
    }

    fn define_global_constant(self: &mut Self) {
        let idx = self.code[self.offset];
        self.offset += 1;

        println!("DEFINE GLOBAL CONSTANT: idx({})", idx);
    }
}

