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
                _ => {
                    println!("Unknown instruction")
                }
            }
        }
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
}

