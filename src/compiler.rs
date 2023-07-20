use crate::scanner::{Scanner, Token};
use crate::chunk::Chunk;
use crate::opcode;
use crate::var_len_int;
use crate::float;

/*

   primary -> NUMBER | FLOAT
 */

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    chunk: Chunk
}

impl<'a> Compiler<'a> {
    pub fn new(src: &'a str) -> Compiler {
        Compiler{
            scanner: Scanner::new(&src),
            chunk: Chunk::new()
        }
    }

    pub fn compile(self: &mut Self) -> Result<(), String> {
        self.primary()?;

        Ok(())
    }

    pub fn take_chunk(self: Self) -> Chunk {
        self.chunk
    }

    fn primary(self: &mut Self) -> Result<(), String> {
        match self.scanner.scan_token() {
            Ok(Token::Integer(i)) => {
                self.chunk.write(opcode::CONSTANT_INTEGER);
                let mut encoder = var_len_int::Encoder::new(i);
                loop {
                    let (byte, complete) = encoder.step_encode();
                    self.chunk.write(byte);

                    if complete {
                        break;
                    }
                }

            },
            Ok(Token::Float(f)) => {
                self.chunk.write(opcode::CONSTANT_FLOAT);
                for byte in float::encode(f) {
                    self.chunk.write(byte);
                }
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
        let mut compiler = Compiler::new("3735928559");
        assert_eq!(compiler.compile(), Ok(()));
        let chunk = compiler.take_chunk();
        assert_eq!(chunk.code, [0, 141, 245, 182, 253, 111]);
    }
}
