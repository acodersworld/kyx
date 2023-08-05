use crate::scanner::{Scanner, Token};
use crate::chunk::Chunk;
use crate::opcode;
use crate::var_len_int;
use crate::float;
use crate::vm::VM;

use std::vec::Vec;

/*
    expression -> term
    term -> factor ( "-" | "+" factor )*
    factor -> primary ( "*" | "/" primary )*
    primary -> NUMBER | FLOAT | STRING | "(" expression ")"
 */

#[derive(PartialEq, Clone, Copy)]
enum ValueType {
    Integer,
    Float,
    Str
}

pub struct Compiler<'a, 'vm> {
    vm: &'vm mut VM,
    scanner: Scanner<'a>,
    chunk: Chunk,
    type_stack: Vec<ValueType>
}

impl<'a, 'vm> Compiler<'a, 'vm> {
    pub fn new(vm: &'vm mut VM, src: &'a str) -> Compiler<'a, 'vm> {
        Compiler{
            vm,
            scanner: Scanner::new(&src),
            chunk: Chunk::new(),
            type_stack: Vec::new()
        }
    }

    pub fn compile(self: &mut Self) -> Result<(), String> {
        self.statement()?;

        Ok(())
    }

    pub fn take_chunk(self: Self) -> Chunk {
        self.chunk
    }

    fn consume(self: &mut Self, token: Token) -> Result<(), String> {
        if self.scanner.match_token(token)? {
            return Ok(())
        }

        Err("Expected ')'".to_owned())
    }

    fn statement(self: &mut Self) -> Result<(), String> {
        if self.scanner.match_token(Token::Print)? {
            self.print()?;
        }
        else {
            self.expression()?;
        }

        Ok(())
    }

    fn print(self: &mut Self) -> Result<(), String> {
        self.expression()?;
        self.chunk.write_byte(opcode::PRINT);

        Ok(())
    }

    fn expression(self: &mut Self) -> Result<(), String> {
        self.term()?;

        Ok(())
    }

    fn term_factor(self: &mut Self, opi: u8, opf: u8) -> Result<(), String> {
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

    fn factor(self: &mut Self) -> Result<(), String> {
        self.primary()?;

        loop {
            if self.scanner.match_token(Token::Star)? {
                self.term_factor(opcode::MULI, opcode::MULF)?;
            }
            else if self.scanner.match_token(Token::Slash)? {
                self.term_factor(opcode::DIVI, opcode::DIVF)?;
            }
            else {
                break;
            }
        }


        Ok(())
    }

    fn term_right(self: &mut Self, opi: u8, opf: u8) -> Result<(), String> {
        self.factor()?;

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
        self.factor()?;

        loop {
            if self.scanner.match_token(Token::Plus)? {
                self.term_right(opcode::ADDI, opcode::ADDF)?;
            }
            else if self.scanner.match_token(Token::Minus)? {
                self.term_right(opcode::SUBI, opcode::SUBF)?;
            }
            else {
                break;
            }
        }


        Ok(())
    }

    fn primary(self: &mut Self) -> Result<(), String> {
        let t = self.scanner.scan_token();
        match t {
            Ok(Token::LeftParen) => {
                self.expression()?;
                self.consume(Token::RightParen)?;
                return Ok(())
            },
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
            Ok(Token::Str(s)) => {
                self.chunk.write_byte(opcode::CONSTANT_STRING);
                self.chunk.write_byte(self.vm.create_constant_str(s));
                self.type_stack.push(ValueType::Str);
            },
            Err(msg) => {
                return Err(msg)
            },
            _ => {
                println!("primary Unknown token: {:?}", t);
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
        let mut vm = VM::new();
        let mut compiler = Compiler::new(&mut vm, "3.142");
        assert_eq!(compiler.compile(), Ok(()));
        let chunk = compiler.take_chunk();
        assert_eq!(chunk.code, [1, 135, 22, 73, 64]);
    }

    #[test]
    fn test_integer() {
        {
            let mut vm = VM::new();
            let mut compiler = Compiler::new(&mut vm, "735928559");
            assert_eq!(compiler.compile(), Ok(()));
            let chunk = compiler.take_chunk();
            assert_eq!(chunk.code, [0, 130, 222, 245, 193, 111]);
        }

        {
            let mut vm = VM::new();
            let mut compiler = Compiler::new(&mut vm, "-735928559");
            assert_eq!(compiler.compile(), Ok(()));
            let chunk = compiler.take_chunk();
            assert_eq!(chunk.code, [0, 253, 161, 138, 190, 17]);
        }
    }
}
