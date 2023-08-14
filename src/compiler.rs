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
       declaration -> let_decl | print_stmt | expr_stmt
           let_decl -> let (mut)? identifier type : = expression ";"
           print_stmt -> print expression ";"

   STATEMENTS:
       expr_stmt -> block_expression | expression ";"

   EXPRESSIONS:
       expression -> assignment | block_expression
           block_expression -> { declaration* expression? }
           assignment -> identifier = expression | term
           term -> factor ( "-" | "+" factor )*
           factor -> primary ( "*" | "/" primary )*
           primary -> NUMBER | FLOAT | STRING | identifier | "(" expression ")"
*/

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
struct Variable {
    value_type: ValueType,
    read_only: bool,
}

#[derive(PartialEq)]
struct LocalVariable {
    name: String,
    v: Variable,
    scope: u32,
}

struct StackFrame {
    current_scope: u32,
    locals: Vec<LocalVariable>,
}

pub struct Compiler {
    globals: HashMap<String, Variable>,
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
            stack_frames: Vec::new(),
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
    globals: &'a mut HashMap<String, Variable>,
    stack_frames: Vec<StackFrame>,
    scanner: Scanner<'a>,
    chunk: Chunk,
    type_stack: Vec<Variable>,
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
        if self.scanner.match_token(Token::Let)? {
            self.let_statement()?;
        } else if self.scanner.match_token(Token::Print)? {
            self.print()?;
        } else {
            self.expression_statement()?;
        }

        Ok(())
    }

    fn expression_statement(self: &mut Self) -> Result<(), String> {
        if self.scanner.match_token(Token::LeftBrace)? {
            self.block_expression()?;
            if self.scanner.peek_token()? != Token::RightBrace {
                self.clear_stack();
            }
        } else {
            self.expression()?;
            self.clear_stack();
            self.consume(Token::SemiColon)?;
        }

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

        if self.stack_frames.len() == 0 {
            if self
                .globals
                .insert(
                    identifier_name.to_string(),
                    Variable {
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
        } else {
            let frame = self.stack_frames.last_mut().unwrap();
            let found = frame
                .locals
                .iter()
                .find(|l| l.scope == frame.current_scope && l.name == identifier_name);

            if found.is_none() {
                frame.locals.push(LocalVariable {
                    name: identifier_name.to_string(),
                    v: Variable {
                        read_only: !mutable,
                        value_type: var_type,
                    },
                    scope: frame.current_scope,
                });
                self.chunk.write_byte(opcode::DEFINE_LOCAL);
            } else {
                return Err(format!(
                    "Local {} is already defined in current scope",
                    identifier_name
                ));
            }
        }

        self.consume(Token::SemiColon)?;
        Ok(())
    }

    fn find_local(self: &mut Self, name: &str) -> Option<(Variable, u32)> {
        if let Some(frame) = self.stack_frames.last_mut() {
            let found = frame
                .locals
                .iter()
                .enumerate()
                .rev()
                .find(|l| l.1.name == name);

            if let Some(l) = found {
                return Some((l.1.v, l.0 as u32));
            }
        }

        None
    }

    fn find_global(self: &Self, name: &str) -> Result<Variable, String> {
        match self.globals.get(name) {
            None => return Err(format!("Global {} not defined", name)),
            Some(x) => Ok(*x),
        }
    }

    fn identifier(self: &mut Self, name: &str) -> Result<(), String> {
        enum Identifier {
            Global,
            Local(u8),
        }

        let (variable, symbol, type_str) = {
            if let Some((local, idx)) = self.find_local(name) {
                (local, Identifier::Local(idx as u8), "Local")
            } else {
                (self.find_global(name)?, Identifier::Global, "Global")
            }
        };

        if self.scanner.match_token(Token::Equal)? {
            self.expression()?;

            if self.type_stack.len() == 0 {
                return Err("Expected right hand side value, got None".to_owned());
            }

            let expr_type = self.type_stack.last().unwrap().value_type;
            if variable.value_type != expr_type {
                return Err(format!(
                    "Expected type {}, got {}",
                    variable.value_type, expr_type
                ));
            }

            if variable.read_only {
                return Err(format!("{} {} is ready only", type_str, name));
            }

            match symbol {
                Identifier::Global => {
                    self.chunk.write_byte(opcode::SET_GLOBAL);
                    self.chunk
                        .write_byte(self.data_section.create_constant_str(name));
                }
                Identifier::Local(idx) => {
                    self.chunk.write_byte(opcode::SET_LOCAL);
                    self.chunk.write_byte(idx);
                }
            }

            assert!(self.type_stack.len() > 0);
            self.type_stack.pop();
        } else {
            match symbol {
                Identifier::Global => {
                    self.chunk.write_byte(opcode::PUSH_GLOBAL);
                    self.chunk
                        .write_byte(self.data_section.create_constant_str(name));
                }
                Identifier::Local(idx) => {
                    self.chunk.write_byte(opcode::PUSH_LOCAL);
                    self.chunk.write_byte(idx);
                }
            }

            self.type_stack.push(Variable {
                value_type: variable.value_type,
                read_only: variable.read_only,
            });
        }

        Ok(())
    }

    fn print(self: &mut Self) -> Result<(), String> {
        self.expression()?;
        self.chunk.write_byte(opcode::PRINT);
        self.type_stack.pop();
        self.consume(Token::SemiColon)?;
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

    fn block(self: &mut Self) -> Result<(), String> {
        while !(self.scanner.match_token(Token::RightBrace)?
            || self.scanner.match_token(Token::Eof)?)
        {
            if self.scanner.match_token(Token::Let)? {
                self.let_statement()?;
            } else if self.scanner.match_token(Token::Print)? {
                self.print()?;
            } else if self.scanner.match_token(Token::LeftBrace)? {
                self.block_expression()?;
                if (self.scanner.peek_token()? != Token::RightBrace)
                    && self.scanner.match_token(Token::SemiColon)?
                {
                    self.clear_stack();
                }
            } else {
                self.expression()?;
                if self.scanner.match_token(Token::SemiColon)? {
                    self.clear_stack();
                } else if self.scanner.peek_token()? != Token::RightBrace {
                    return Err("Expected ';'".to_string());
                }
            }
        }

        Ok(())
    }

    fn block_expression(self: &mut Self) -> Result<(), String> {
        let push_local = {
            if let Some(top) = self.stack_frames.last_mut() {
                top.current_scope += 1;
                false
            } else {
                self.stack_frames.push(StackFrame {
                    current_scope: 0,
                    locals: Vec::new(),
                });
                self.chunk.code.push(opcode::PUSH_FRAME);
                true
            }
        };

        let locals_top = self.stack_frames.last().unwrap().locals.len();
        self.block()?;

        assert!(self.stack_frames.len() > 0);
        if push_local {
            self.stack_frames.pop();
            self.chunk.code.push(opcode::POP_FRAME);
        } else {
            let frame = self.stack_frames.last_mut().unwrap();
            while frame.locals.len() > locals_top {
                self.chunk.code.push(opcode::LOCAL_POP);
                frame.locals.pop();
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
        self.type_stack.push(Variable {
            value_type: ValueType::Integer,
            read_only: true,
        });
    }

    fn float(self: &mut Self, f: f32) {
        self.chunk.write_byte(opcode::CONSTANT_FLOAT);
        for byte in float::encode(f) {
            self.chunk.write_byte(byte);
        }
        self.type_stack.push(Variable {
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
                self.type_stack.push(Variable {
                    value_type: ValueType::Str,
                    read_only: true,
                });
            }
            Err(msg) => return Err(msg),
            _ => {
                panic!("primary Unknown token: {:?}", t);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct TestDataSection {}

    impl TestDataSection {
        fn new() -> TestDataSection {
            TestDataSection {}
        }
    }

    impl DataSection for TestDataSection {
        fn create_constant_str(self: &mut Self, _s: &str) -> u8 {
            return 0;
        }
    }

    #[test]
    fn test_error_missing_semicolon() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler.compile(&mut table, "print 1").is_err());
    }

    #[test]
    fn test_error_negative_string() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler.compile(&mut table, "print -\"hello\";").is_err());
    }

    #[test]
    fn test_let_statement() {
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert_eq!(
                compiler
                    .compile(&mut table, "let identifier: int = 0;")
                    .err(),
                None
            );
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert_eq!(
                compiler
                    .compile(&mut table, "let identifier: float = 0.0;")
                    .err(),
                None
            );
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert_eq!(
                compiler
                    .compile(&mut table, "let identifier: string = \"hello\";")
                    .err(),
                None
            );
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert!(compiler
                .compile(&mut table, "let identifier: int = \"hello\";")
                .is_err());
        }
    }
}
