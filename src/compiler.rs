use crate::chunk::Chunk;
use crate::float;
use crate::opcode;
use crate::scanner::{Scanner, Token};
use crate::var_len_int;

use std::collections::HashMap;
use std::fmt;
use std::vec::Vec;

/*
   PROGRAM:
       program -> declaration* EOF

   DECLARATIONS:
       declaration -> letDecl | statement ;
           letDecl -> let (mut)? identifier type : = expression
           statement -> print expression | exprStmt ;

   STATEMENTS:
       exprStmt -> expression ;

   EXPRESSIONS:
       expression -> assignment | block_expression
           assignment -> identifier = expression | term
           term -> factor ( "-" | "+" factor )*
           factor -> primary ( "*" | "/" primary )*
           primary -> NUMBER | FLOAT | STRING | identifier | "(" expression ")"
*/

#[derive(PartialEq, Clone, Copy)]
enum ValueType {
    Integer,
    Float,
    Str,
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Integer => "int",
            Self::Float => "float",
            Self::Str => "string",
        };

        write!(f, "{}", s)
    }
}

pub trait DataSection {
    fn create_constant_str(self: &mut Self, s: &str) -> u8;
}

#[derive(Clone)]
struct Global {
    read_only: bool,
    value_type: ValueType,
}

#[derive(PartialEq)]
struct Type {
    value_type: ValueType,
    read_only: bool,
}

pub struct Compiler {
    globals: HashMap<String, Global>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            globals: HashMap::new(),
        }
    }

    pub fn compile<'a, T: DataSection>(
        self: &mut Self,
        data_section: &'a mut T,
        src: &'a str,
    ) -> Result<Chunk, String> {
        let mut compiler = SrcCompiler::<'_, T> {
            globals: &mut self.globals,
            data_section,
            scanner: Scanner::new(&src),
            chunk: Chunk::new(),
            type_stack: Vec::new(),
        };

        compiler.compile()?;
        Ok(compiler.chunk)
    }
}

pub struct SrcCompiler<'a, T> {
    globals: &'a mut HashMap<String, Global>,
    scanner: Scanner<'a>,
    chunk: Chunk,
    type_stack: Vec<Type>,
    data_section: &'a mut T,
}

impl<'a, T: DataSection> SrcCompiler<'a, T> {
    pub fn compile(self: &mut Self) -> Result<(), String> {
        while self.scanner.peek_token()? != Token::Eof {
            self.declaration()?;
        }

        Ok(())
    }

    fn clear_stack(self: &mut Self) {
        let pop_count = self.type_stack.len();
        self.type_stack.clear();

        for _ in 0..pop_count {
            self.chunk.code.push(opcode::POP)
        }
    }

    fn consume(self: &mut Self, token: Token) -> Result<(), String> {
        if self.scanner.match_token(token)? {
            return Ok(());
        }

        Err(format!("Expected {}", token))
    }

    fn declaration(self: &mut Self) -> Result<(), String> {
        self.statement()?;

        Ok(())
    }

    fn statement(self: &mut Self) -> Result<(), String> {
        if self.scanner.match_token(Token::Let)? {
            self.let_statement()?;
        } else if self.scanner.match_token(Token::Print)? {
            self.print()?
        } else {
            self.statement_expression()?
        }

        self.clear_stack();
        self.consume(Token::SemiColon)?;
        Ok(())
    }

    fn statement_expression(self: &mut Self) -> Result<(), String> {
        self.expression()?;

        Ok(())
    }

    fn let_statement(self: &mut Self) -> Result<(), String> {
        let mutable = self.scanner.match_token(Token::Mut)?;
        let identifier_name = match self.scanner.scan_token()? {
            Token::Identifier(ident) => ident,
            _ => return Err("Expected identifier after 'let'".to_owned()),
        };

        self.consume(Token::Colon)?;

        let var_type = match self.scanner.scan_token()? {
            Token::TypeInt => ValueType::Integer,
            Token::TypeFloat => ValueType::Float,
            Token::TypeString => ValueType::Str,
            token => return Err(format!("Expected type but got {}", token)),
        };

        self.consume(Token::Equal)?;
        self.expression()?;

        if self.type_stack.len() == 0 {
            return Err("Expected value on right hand side of '=', got None".to_owned());
        }

        let expr_type = self.type_stack.pop().unwrap().value_type;
        if var_type != expr_type {
            return Err(format!("Expected type {}, got {}", var_type, expr_type));
        }

        if self
            .globals
            .insert(
                identifier_name.to_string(),
                Global {
                    read_only: !mutable,
                    value_type: var_type,
                },
            )
            .is_some()
        {
            return Err(format!("Global {} is already defined", identifier_name));
        }

        self.chunk.write_byte(opcode::DEFINE_GLOBAL);
        self.chunk
            .write_byte(self.data_section.create_constant_str(identifier_name));

        Ok(())
    }

    fn identifier(self: &mut Self, name: &str) -> Result<(), String> {
        let variable_type = match self.globals.get(name) {
            None => return Err(format!("Global {} not defined", name)),
            Some(x) => x,
        };

        if self.scanner.match_token(Token::Equal)? {
            self.expression()?;

            if self.type_stack.len() == 0 {
                return Err("Expected right hand side value, got None".to_owned());
            }

            if let Some(g) = self.globals.get(name) {
                let expr_type = self.type_stack.last().unwrap().value_type;
                if g.value_type != expr_type {
                    return Err(format!("Expected type {}, got {}", g.value_type, expr_type));
                }

                if g.read_only {
                    return Err(format!("Global {} is ready only", name));
                }
            } else {
                return Err(format!("Global {} not declared", name));
            }

            self.chunk.write_byte(opcode::SET_GLOBAL);
            self.chunk
                .write_byte(self.data_section.create_constant_str(name));
        } else {
            self.chunk.write_byte(opcode::PUSH_GLOBAL);
            self.chunk
                .write_byte(self.data_section.create_constant_str(name));
            self.type_stack.push(Type {
                value_type: variable_type.value_type,
                read_only: variable_type.read_only,
            });
        }

        Ok(())
    }

    fn print(self: &mut Self) -> Result<(), String> {
        self.expression()?;
        self.chunk.write_byte(opcode::PRINT);
        self.type_stack.pop();
        Ok(())
    }

    fn expression(self: &mut Self) -> Result<(), String> {
        if self.scanner.match_token(Token::LeftBrace)? {
            self.block_expression()?;
        } else {
            self.term()?;

            if self.scanner.match_token(Token::Equal)? {
                assert!(self.type_stack.len() > 0);
                if self.type_stack.last().unwrap().read_only {
                    return Err("left hand side is read only".to_string());
                }

                self.expression()?;
            }
        }

        Ok(())
    }

    fn block_expression(self: &mut Self) -> Result<(), String> {
        while !self.scanner.match_token(Token::RightBrace)? {
            if !self.type_stack.is_empty() {
                return Err("Expected ';'".to_owned());
            }

            self.expression()?;

            if self.scanner.match_token(Token::SemiColon)? {
                self.clear_stack();
            }
        }

        Ok(())
    }

    fn term_factor(self: &mut Self, opi: u8, opf: u8) -> Result<(), String> {
        self.primary()?;

        let len = self.type_stack.len();
        let left_type = self.type_stack[len - 2].value_type;
        let right_type = self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else if left_type == ValueType::Integer {
            self.chunk.write_byte(opi)
        } else {
            self.chunk.write_byte(opf)
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn factor(self: &mut Self) -> Result<(), String> {
        self.primary()?;

        loop {
            if self.scanner.match_token(Token::Star)? {
                self.term_factor(opcode::MULI, opcode::MULF)?;
            } else if self.scanner.match_token(Token::Slash)? {
                self.term_factor(opcode::DIVI, opcode::DIVF)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn term_right(self: &mut Self, opi: u8, opf: u8) -> Result<(), String> {
        self.factor()?;

        let len = self.type_stack.len();
        let left_type = self.type_stack[len - 2].value_type;
        let right_type = self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else if left_type == ValueType::Integer {
            self.chunk.write_byte(opi)
        } else {
            self.chunk.write_byte(opf)
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn term(self: &mut Self) -> Result<(), String> {
        self.factor()?;

        loop {
            if self.scanner.match_token(Token::Plus)? {
                self.term_right(opcode::ADDI, opcode::ADDF)?;
            } else if self.scanner.match_token(Token::Minus)? {
                self.term_right(opcode::SUBI, opcode::SUBF)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn integer(self: &mut Self, i: i32) {
        self.chunk.write_byte(opcode::CONSTANT_INTEGER);
        let mut encoder = var_len_int::Encoder::new(i);
        loop {
            let (byte, complete) = encoder.step_encode();
            self.chunk.write_byte(byte);

            if complete {
                break;
            }
        }
        self.type_stack.push(Type {
            value_type: ValueType::Integer,
            read_only: true,
        });
    }

    fn float(self: &mut Self, f: f32) {
        self.chunk.write_byte(opcode::CONSTANT_FLOAT);
        for byte in float::encode(f) {
            self.chunk.write_byte(byte);
        }
        self.type_stack.push(Type {
            value_type: ValueType::Float,
            read_only: true,
        });
    }

    fn primary(self: &mut Self) -> Result<(), String> {
        let t = self.scanner.scan_token();
        match t {
            Ok(Token::Identifier(name)) => {
                self.identifier(name)?;
            }
            Ok(Token::LeftParen) => {
                self.expression()?;
                self.consume(Token::RightParen)?;
            }
            Ok(Token::Minus) => {
                let next = self.scanner.scan_token();
                match next {
                    Ok(Token::Integer(i)) => {
                        self.integer(-i);
                    }
                    Ok(Token::Float(f)) => {
                        self.float(-f);
                    }
                    _ => return Err("Expected number after '-'".to_owned()),
                }
            }
            Ok(Token::Integer(i)) => {
                self.integer(i);
            }
            Ok(Token::Float(f)) => {
                self.float(f);
            }
            Ok(Token::Str(s)) => {
                self.chunk.write_byte(opcode::CONSTANT_STRING);
                self.chunk
                    .write_byte(self.data_section.create_constant_str(s));
                self.type_stack.push(Type {
                    value_type: ValueType::Str,
                    read_only: true,
                });
            }
            Err(msg) => return Err(msg),
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

    struct TestStringTable {}

    impl TestStringTable {
        fn new() -> TestStringTable {
            TestStringTable {}
        }
    }

    impl StringTable for TestStringTable {
        fn create_constant_str(self: &mut Self, _s: &str) -> u8 {
            return 0;
        }
    }

    #[test]
    fn test_error_missing_semicolon() {
        let mut table = TestStringTable::new();
        let mut compiler = Compiler::new();
        assert!(compiler.compile(&mut table, "print 1").is_err());
    }

    #[test]
    fn test_error_negative_string() {
        let mut table = TestStringTable::new();
        let mut compiler = Compiler::new();
        assert!(compiler.compile(&mut table, "print -\"hello\";").is_err());
    }

    #[test]
    fn test_let_statement() {
        {
            let mut table = TestStringTable::new();
            let mut compiler = Compiler::new();
            assert_eq!(
                compiler
                    .compile(&mut table, "let identifier: int = 0;")
                    .err(),
                None
            );
        }

        {
            let mut table = TestStringTable::new();
            let mut compiler = Compiler::new();
            assert_eq!(
                compiler
                    .compile(&mut table, "let identifier: float = 0.0;")
                    .err(),
                None
            );
        }

        {
            let mut table = TestStringTable::new();
            let mut compiler = Compiler::new();
            assert_eq!(
                compiler
                    .compile(&mut table, "let identifier: string = \"hello\";")
                    .err(),
                None
            );
        }

        {
            let mut table = TestStringTable::new();
            let mut compiler = Compiler::new();
            assert!(compiler
                .compile(&mut table, "let identifier: int = \"hello\";")
                .is_err());
        }
    }
}
