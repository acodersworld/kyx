use crate::scanner::{Scanner, Token};
use crate::chunk::Chunk;
use crate::opcode;
use crate::var_len_int;
use crate::float;

use std::vec::Vec;
use std::fmt;
use std::collections::HashMap;

/*
    expression -> term
    term -> factor ( "-" | "+" factor )*
    factor -> primary ( "*" | "/" primary )*
    primary -> NUMBER | FLOAT | STRING | identifier | "(" expression ")"
 */

#[derive(PartialEq, Clone, Copy)]
enum ValueType {
    Integer,
    Float,
    Str
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

pub trait StringTable {
    fn create_constant_str(self: &mut Self, s: &str) -> u8;
}

#[derive(Clone)]
pub struct CompilerState {
    globals: HashMap<String, ValueType>,
}

impl CompilerState {
    pub fn new() -> CompilerState {
        CompilerState {
            globals: HashMap::new()
        }
    }
}

pub struct Compiler<'a, 'table, T> {
    string_table: &'table mut T,
    pub state: CompilerState,
    scanner: Scanner<'a>,
    chunk: Chunk,
    type_stack: Vec<ValueType>
}

impl<'a, 'state, 'table, T: StringTable> Compiler<'a, 'table, T> {
    pub fn new(string_table: &'table mut T, state: CompilerState, src: &'a str) -> Compiler<'a, 'table, T> {
        Compiler{
            string_table,
            state,
            scanner: Scanner::new(&src),
            chunk: Chunk::new(),
            type_stack: Vec::new()
        }
    }

    pub fn compile(self: &mut Self) -> Result<(), String> {
        while self.scanner.peek_token()? != Token::Eof {
            self.statement()?;
        }

        Ok(())
    }

    pub fn take_chunk(self: Self) -> Chunk {
        self.chunk
    }

    fn consume(self: &mut Self, token: Token) -> Result<(), String> {
        if self.scanner.match_token(token)? {
            return Ok(())
        }

        Err(format!("Expected {}", token))
    }

    fn statement(self: &mut Self) -> Result<(), String> {
        if self.scanner.match_token(Token::Print)? {
            self.print()?;
        }
        else if self.scanner.match_token(Token::Let)? {
            self.let_statement()?;
        }
        else {
            self.expression()?;
        }

        self.consume(Token::SemiColon)?;
        Ok(())
    }

    fn let_statement(self: &mut Self) -> Result<(), String> {
        let mutable = self.scanner.match_token(Token::Mut)?;
        let identifier_name = match self.scanner.scan_token()? {
            Token::Identifier(ident) => ident,
            _ => return Err("Expected identifier after 'let'".to_owned())
        };

        self.consume(Token::Colon)?;

        let var_type = match self.scanner.scan_token()? {
            Token::TypeInt => ValueType::Integer,
            Token::TypeFloat => ValueType::Float,
            Token::TypeString => ValueType::Str,
            token => return Err(format!("Expected type but got {}", token))
        };

        if self.state.globals.insert(identifier_name.to_string(), var_type).is_some() {
            return Err(format!("Global {} is already defined", identifier_name))
        }

        self.consume(Token::Equal)?;
        self.expression()?;

        let expr_type = *self.type_stack.last().unwrap();
        if var_type != expr_type {
            return Err(format!("Expected type {}, got {}", var_type, expr_type))
        }

        let define_op = if mutable { opcode::DEFINE_GLOBAL_VARIABLE } else { opcode::DEFINE_GLOBAL_CONSTANT };
        self.chunk.write_byte(define_op);
        self.chunk.write_byte(self.string_table.create_constant_str(identifier_name));

        Ok(())
    }

    fn identifier(self: &mut Self, name: &str) -> Result<(), String> {
        let variable_type = match self.state.globals.get(name) {
            None => return Err(format!("Global {} not defined", name)),
            Some(&x) => x
        };

        self.chunk.write_byte(opcode::PUSH_GLOBAL);
        self.chunk.write_byte(self.string_table.create_constant_str(name));
        self.type_stack.push(variable_type);
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
        self.type_stack.push(ValueType::Integer);
    }

    fn float(self: &mut Self, f: f32) {
        self.chunk.write_byte(opcode::CONSTANT_FLOAT);
        for byte in float::encode(f) {
            self.chunk.write_byte(byte);
        }
        self.type_stack.push(ValueType::Float);
    }

    fn primary(self: &mut Self) -> Result<(), String> {
        let t = self.scanner.scan_token();
        match t {
            Ok(Token::Let) => {
                self.let_statement()?;
            },
            Ok(Token::Identifier(name)) => {
                self.identifier(name)?;
            },
            Ok(Token::LeftParen) => {
                self.expression()?;
                self.consume(Token::RightParen)?;
            },
            Ok(Token::Minus) => {
                let next = self.scanner.scan_token();
                match next {
                    Ok(Token::Integer(i)) => {
                        self.integer(-i);
                    },
                    Ok(Token::Float(f)) => {
                        self.float(-f);
                    },
                    _ => {
                        return Err("Expected number after '-'".to_owned())
                    }
                }
            },
            Ok(Token::Integer(i)) => {
                self.integer(i);
            },
            Ok(Token::Float(f)) => {
                self.float(f);
            },
            Ok(Token::Str(s)) => {
                self.chunk.write_byte(opcode::CONSTANT_STRING);
                self.chunk.write_byte(self.string_table.create_constant_str(s));
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

    struct TestStringTable {
    }

    impl TestStringTable {
        fn new() -> TestStringTable {
            TestStringTable{}
        }
    }

    impl StringTable for TestStringTable {
        fn create_constant_str(self: &mut Self, _s: &str) -> u8 {
            return 0;
        }
    }

    #[test]
    fn test_error_missing_semicolon() {
        let state = CompilerState::new();
        let mut table = TestStringTable::new();
        let mut compiler = Compiler::new(&mut table, state, "print 1");
        assert!(compiler.compile().is_err());
    }

    #[test]
    fn test_error_negative_string() {
        let state = CompilerState::new();
        let mut table = TestStringTable::new();
        let mut compiler = Compiler::new(&mut table, state, "print -\"hello\";");
        assert!(compiler.compile().is_err());
    }

    #[test]
    fn test_let_statement() {
        {
            let state = CompilerState::new();
            let mut table = TestStringTable::new();
            let mut compiler = Compiler::new(&mut table, state, "let identifier: int = 0;");
            assert_eq!(compiler.compile(), Ok(()));
        }

        {
            let state = CompilerState::new();
            let mut table = TestStringTable::new();
            let mut compiler = Compiler::new(&mut table, state, "let identifier: float = 0.0;");
            assert_eq!(compiler.compile(), Ok(()));
        }

        {
            let state = CompilerState::new();
            let mut table = TestStringTable::new();
            let mut compiler = Compiler::new(&mut table, state, "let identifier: string = \"hello\";");
            assert_eq!(compiler.compile(), Ok(()));
        }

        {
            let state = CompilerState::new();
            let mut table = TestStringTable::new();
            let mut compiler = Compiler::new(&mut table, state, "let identifier: int = \"hello\";");
            assert!(compiler.compile().is_err());
        }
    }

}
