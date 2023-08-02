use crate::scanner::{Scanner, Token};
use crate::chunk::Chunk;
use crate::opcode;
use crate::var_len_int;
use crate::float;

use std::vec::Vec;

/*
    expression -> term
    term -> primary ( "-" | "+" primary )*
    primary -> NUMBER | FLOAT
 */

#[derive(PartialEq, Clone, Copy)]
enum ValueType {
    Integer,
    Float
}

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    chunk: Chunk,
    type_stack: Vec<ValueType>
}

impl<'a> Compiler<'a> {
    pub fn new(src: &'a str) -> Compiler {
        Compiler{
            scanner: Scanner::new(&src),
            chunk: Chunk::new(),
            type_stack: Vec::new()
        }
    }

    pub fn compile(self: &mut Self) -> Result<(), String> {
        self.expression()?;

        Ok(())
    }

    pub fn take_chunk(self: Self) -> Chunk {
        self.chunk
    }

    fn expression(self: &mut Self) -> Result<(), String> {
        self.term()?;

        Ok(())
    }

    fn term_right(self: &mut Self, opi: u8, opf: u8) -> Result<(), String> {
        self.primary()?;

        let len = self.type_stack.len();
        let left_type = self.type_stack[len - 2];
        let right_type = self.type_stack[len - 1];

        if left_type != right_type {
            return Err("Type error".to_owned())
        }
        else if left_type == ValueType::Integer {
            self.chunk.write_byte(opi)
        }
        else {
            self.chunk.write_byte(opf)
        }

        self.type_stack.pop();
        Ok(())
    }

    fn term(self: &mut Self) -> Result<(), String> {
        self.primary()?;

        let token = self.scanner.scan_token();
        match token {
            Ok(Token::Plus) => {
                self.term_right(opcode::ADDI, opcode::ADDF)?;
            },
            Ok(Token::Minus) => {
                self.term_right(opcode::SUBI, opcode::SUBF)?;
            },
            _ => {
                println!("SKIP {:?}", token);
            }
        }

        Ok(())
    }

    fn primary(self: &mut Self) -> Result<(), String> {
        match self.scanner.scan_token() {
            Ok(Token::Integer(i)) => {
                self.chunk.write_byte(opcode::CONSTANT_INTEGER);
                let mut encoder = var_len_int::Encoder::new(i);
                loop {
                    let (byte, complete) = encoder.step_encode();
                    self.chunk.write_byte(byte);

                    if complete {
                        break;
                    }
                }
                self.type_stack.push(ValueType::Integer);
            },
            Ok(Token::Float(f)) => {
                self.chunk.write_byte(opcode::CONSTANT_FLOAT);
                for byte in float::encode(f) {
                    self.chunk.write_byte(byte);
                }
                self.type_stack.push(ValueType::Float);
            },
            Err(msg) => {
                return Err(msg)
            },
            _ => {
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_float() {
        let mut compiler = Compiler::new("3.142");
        assert_eq!(compiler.compile(), Ok(()));
        let chunk = compiler.take_chunk();
        assert_eq!(chunk.code, [1, 135, 22, 73, 64]);
    }

    #[test]
    fn test_integer() {
        {
            let mut compiler = Compiler::new("735928559");
            assert_eq!(compiler.compile(), Ok(()));
            let chunk = compiler.take_chunk();
            assert_eq!(chunk.code, [0, 130, 222, 245, 193, 111]);
        }

        {
            let mut compiler = Compiler::new("-735928559");
            assert_eq!(compiler.compile(), Ok(()));
            let chunk = compiler.take_chunk();
            assert_eq!(chunk.code, [0, 253, 161, 138, 190, 17]);
        }
    }
}
