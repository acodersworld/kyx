use crate::chunk::Chunk;
use crate::float;
use crate::opcode;
use crate::scanner::{Scanner, Token};
use crate::var_len_int;

use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::vec::Vec;

/*
   PROGRAM:
       program -> rule* EOF

       rule -> declaration | expression ";"?

       type -> "int" | "float" | "string" | "[" type "]" | "[" type ":" type "]"

   DECLARATIONS:
       declaration -> let_decl |
                      while_stmt |
                      for_stmt |
                      break |
                      print_stmt |

           let_decl -> "let" ("mut")? ":" identifier type = expression ";"
           print_stmt -> print expression ";"
           while_stmt -> "while" expression block_expression
           break_stmt -> "break" ";"
           for_stmt "for" identifier ":" NUMBER (".." | "..=") NUMBER block_expression

   EXPRESSIONS:
       expression -> assignment |
                     block_expression |
                     if_expr |
                     vector_constructor |
                     hash_map_constructor
                     read_expr

            block_expression -> "{" declaration* expression? "}"
            if_expr -> "if" expression block_expression ("else" block_expression)?
            vector_constructor -> vec<type>{ expression? ("," expression)* }
            hash_map_constructor -> hash_map<type, type>{ hash_map_argument? ("," hash_map_argument)* }
                 hash_map_argument -> expression ":" expression
            read_expr -> "read" type

            assignment -> identifier = expression | equality
            equality -> comparison (("!=" | "==") comparison)?
            comparison -> term (("<" | "<=" | ">" | ">=") term)?
            term -> factor ( "-" | "+" factor )*
            factor -> index ( "*" | "/" index )*
            index -> primary ("[" expression "]")* |
            primary -> NUMBER | FLOAT | STRING | identifier | "(" expression ")"
*/

#[derive(Debug, PartialEq, Clone)]
enum ValueType {
    Integer,
    Float,
    Str,
    Bool,
    Vector(Rc<ValueType>),
    HashMap(Rc<(ValueType, ValueType)>),
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Integer => "int".to_owned(),
            Self::Float => "float".to_owned(),
            Self::Str => "string".to_owned(),
            Self::Bool => "bool".to_owned(),
            Self::Vector(tp) => format!("[{}]", tp),
            Self::HashMap(tp) => format!("[{}: {}]", tp.0, tp.1),
        };

        write!(f, "{}", s)
    }
}

pub trait DataSection {
    fn create_constant_str(&mut self, s: &str) -> u8;
}

#[derive(Debug, PartialEq, Clone)]
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

pub struct SrcCompiler<'a, T> {
    globals: &'a mut HashMap<String, Variable>,
    stack_frames: Vec<StackFrame>,
    scanner: Scanner<'a>,
    type_stack: Vec<Variable>,
    data_section: &'a mut T,
    unpatched_break_offsets: Vec<usize>,
    is_in_loop: bool,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            globals: HashMap::new(),
        }
    }

    pub fn compile<'a, T: DataSection>(
        &mut self,
        data_section: &'a mut T,
        src: &'a str,
    ) -> Result<Chunk, String> {
        let mut compiler = SrcCompiler::<'_, T> {
            globals: &mut self.globals,
            stack_frames: Vec::new(),
            data_section,
            scanner: Scanner::new(src),
            type_stack: Vec::new(),
            unpatched_break_offsets: Vec::new(),
            is_in_loop: false,
        };

        let mut chunk = Chunk::new();
        compiler.compile(&mut chunk)?;
        Ok(chunk)
    }
}

impl<'a, T: DataSection> SrcCompiler<'a, T> {
    fn clear_stack(&mut self, chunk: &mut Chunk) {
        let pop_count = self.type_stack.len();
        self.type_stack.clear();

        for _ in 0..pop_count {
            chunk.code.push(opcode::POP)
        }
    }

    fn consume(&mut self, token: Token) -> Result<(), String> {
        if self.scanner.match_token(token)? {
            return Ok(());
        }

        Err(format!(
            "Expected {}, got {}",
            token,
            self.scanner.peek_token()?
        ))
    }

    pub fn compile(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        while self.scanner.peek_token()? != Token::Eof {
            self.rule(chunk)?;
        }

        Ok(())
    }

    fn rule(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let is_declaration = self.try_declaration(chunk)?;

        if !is_declaration {
            let is_block = self.expression(chunk)?;

            // only times an expression doesn't have to be followed by a ';'
            // is when it is the last expression of a block or if it is a block itself
            let peeked_token = self.scanner.peek_token()?;
            if peeked_token != Token::RightBrace && peeked_token != Token::Eof {
                if is_block {
                    if self.scanner.match_token(Token::SemiColon)? {
                        self.clear_stack(chunk);
                    }
                } else {
                    self.consume(Token::SemiColon)?;
                    self.clear_stack(chunk);
                }
            }
        }

        Ok(())
    }

    fn try_declaration(&mut self, chunk: &mut Chunk) -> Result<bool, String> {
        if self.scanner.match_token(Token::Let)? {
            self.let_statement(chunk)?;
        } else if self.scanner.match_token(Token::Print)? {
            self.print(chunk)?;
        } else if self.scanner.match_token(Token::While)? {
            self.while_statement(chunk)?;
            return Ok(true);
        } else if self.scanner.match_token(Token::For)? {
            self.for_statement(chunk)?;
            return Ok(true);
        } else if self.scanner.match_token(Token::Break)? {
            self.break_statement(chunk)?;
            return Ok(true);
        } else if self.scanner.match_token(Token::Continue)? {
            self.continue_statement(chunk)?;
            return Ok(true);
        } else {
            return Ok(false);
        }

        Ok(true)
    }

    fn expression(&mut self, chunk: &mut Chunk) -> Result<bool, String> {
        let mut is_block = false;
        if self.scanner.match_token(Token::LeftBrace)? {
            self.block_expression(chunk)?;
            is_block = true;
        } else if self.scanner.match_token(Token::If)? {
            self.if_expression(chunk)?;
            is_block = true;
        } else if self.scanner.match_token(Token::ReadInput)? {
            self.read_expression(chunk)?;
        } else if self.scanner.match_token(Token::Vector)? {
            self.vector_constructor(chunk)?;
        } else if self.scanner.match_token(Token::HashMap)? {
            self.hash_map_constructor(chunk)?;
        } else {
            self.equality(chunk)?;
        }

        Ok(is_block)
    }

    fn vector_constructor(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.consume(Token::Less)?;
        let elem_type = self.parse_type()?;
        self.consume(Token::Greater)?;

        self.consume(Token::LeftBrace)?;
        if self.scanner.match_token(Token::RightBrace)? {
            chunk.write_byte(opcode::CREATE_VEC);
            chunk.write_byte(0);
            self.type_stack.push(Variable {
                value_type: ValueType::Vector(Rc::new(elem_type)),
                read_only: false,
            });
            return Ok(());
        }

        self.expression(chunk)?;
        {
            let tp = &self.type_stack.pop().unwrap().value_type;
            if elem_type != *tp {
                return Err(format!(
                    "Vector argument type mismatch. Expected {}, got {}",
                    elem_type, tp
                ));
            }
        }

        let mut arg_count = 1;
        while !self.scanner.match_token(Token::RightBrace)? {
            self.consume(Token::Comma)?;
            self.expression(chunk)?;
            let tp = &self.type_stack.pop().unwrap().value_type;
            if elem_type != *tp {
                return Err(format!(
                    "Vector argument type mismatch. Expected {}, got {}",
                    elem_type, tp
                ));
            }

            arg_count += 1;
        }

        chunk.write_byte(opcode::CREATE_VEC);
        chunk.write_byte(arg_count);
        self.type_stack.push(Variable {
            value_type: ValueType::Vector(Rc::new(elem_type)),
            read_only: false,
        });
        Ok(())
    }

    fn hash_map_constructor_arg(
        &mut self,
        chunk: &mut Chunk,
        key_type: &ValueType,
        value_type: &ValueType,
    ) -> Result<(), String> {
        self.expression(chunk)?;
        let expr_key_type = &self.type_stack.pop().unwrap().value_type;
        if key_type != expr_key_type {
            return Err(format!(
                "Hash map key argument type mismatch. Expected {}, got {}",
                key_type, expr_key_type
            ));
        }

        self.consume(Token::Colon)?;
        self.expression(chunk)?;
        let expr_value_type = &self.type_stack.pop().unwrap().value_type;
        if value_type != expr_value_type {
            return Err(format!(
                "Hash map key argument type mismatch. Expected {}, got {}",
                key_type, expr_value_type
            ));
        }

        Ok(())
    }

    fn hash_map_constructor(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.consume(Token::Less)?;
        let key_type = self.parse_type()?;
        self.consume(Token::Comma)?;
        let value_type = self.parse_type()?;
        self.consume(Token::Greater)?;

        self.consume(Token::LeftBrace)?;
        if self.scanner.match_token(Token::RightBrace)? {
            chunk.write_byte(opcode::CREATE_HASH_MAP);
            chunk.write_byte(0);
            self.type_stack.push(Variable {
                value_type: ValueType::HashMap(Rc::new((key_type, value_type))),
                read_only: false,
            });
            return Ok(());
        }

        self.hash_map_constructor_arg(chunk, &key_type, &value_type)?;

        let mut arg_count = 1;
        while !self.scanner.match_token(Token::RightBrace)? {
            self.consume(Token::Comma)?;
            self.hash_map_constructor_arg(chunk, &key_type, &value_type)?;

            arg_count += 1;
        }

        chunk.write_byte(opcode::CREATE_HASH_MAP);
        chunk.write_byte(arg_count);
        self.type_stack.push(Variable {
            value_type: ValueType::HashMap(Rc::new((key_type, value_type))),
            read_only: false,
        });
        Ok(())
    }

    fn parse_type_vec_or_map(&mut self) -> Result<ValueType, String> {
        let t0 = self.parse_type()?;

        let result = {
            if self.scanner.match_token(Token::Colon)? {
                let t1 = self.parse_type()?;
                Ok(ValueType::HashMap(Rc::new((t0, t1))))
            } else {
                Ok(ValueType::Vector(Rc::new(t0)))
            }
        };

        self.consume(Token::RightBracket)?;
        result
    }

    fn parse_type(&mut self) -> Result<ValueType, String> {
        let var_type = match self.scanner.scan_token()? {
            Token::TypeInt => ValueType::Integer,
            Token::TypeFloat => ValueType::Float,
            Token::TypeString => ValueType::Str,
            Token::LeftBracket => self.parse_type_vec_or_map()?,
            token => return Err(format!("Expected type but got {}", token)),
        };

        Ok(var_type)
    }

    fn let_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let mutable = self.scanner.match_token(Token::Mut)?;
        let identifier_name = match self.scanner.scan_token()? {
            Token::Identifier(ident) => ident,
            _ => return Err("Expected identifier after 'let'".to_owned()),
        };

        self.consume(Token::Colon)?;

        let var_type = self.parse_type()?;

        self.consume(Token::Equal)?;
        self.expression(chunk)?;

        if self.type_stack.is_empty() {
            return Err("Expected value on right hand side of '=', got None".to_owned());
        }

        let expr_type = self.type_stack.pop().unwrap().value_type;
        if var_type != expr_type {
            return Err(format!("Expected type {}, got {}", var_type, expr_type));
        }

        if self.stack_frames.is_empty() {
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

            chunk.write_byte(opcode::DEFINE_GLOBAL);
            chunk.write_byte(self.data_section.create_constant_str(identifier_name));
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
                chunk.write_byte(opcode::DEFINE_LOCAL);
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

    fn find_local(&mut self, name: &str) -> Option<(Variable, u32)> {
        if let Some(frame) = self.stack_frames.last_mut() {
            let found = frame
                .locals
                .iter()
                .enumerate()
                .rev()
                .find(|l| l.1.name == name);

            if let Some(l) = found {
                return Some((l.1.v.clone(), l.0 as u32));
            }
        }

        None
    }

    fn find_global(&self, name: &str) -> Result<Variable, String> {
        match self.globals.get(name) {
            None => Err(format!("Global {} not defined", name)),
            Some(x) => Ok(x.clone()),
        }
    }

    fn identifier(&mut self, chunk: &mut Chunk, name: &str) -> Result<(), String> {
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
            self.expression(chunk)?;

            if self.type_stack.is_empty() {
                return Err("Expected right hand side value, got None".to_owned());
            }

            let expr_type = &self.type_stack.last().unwrap().value_type;
            if variable.value_type != *expr_type {
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
                    chunk.write_byte(opcode::SET_GLOBAL);
                    chunk
                        .write_byte(self.data_section.create_constant_str(name));
                }
                Identifier::Local(idx) => {
                    chunk.write_byte(opcode::SET_LOCAL);
                    chunk.write_byte(idx);
                }
            }

            assert!(!self.type_stack.is_empty());
            self.type_stack.pop();
        } else {
            match symbol {
                Identifier::Global => {
                    chunk.write_byte(opcode::PUSH_GLOBAL);
                    chunk.write_byte(self.data_section.create_constant_str(name));
                }
                Identifier::Local(idx) => {
                    chunk.write_byte(opcode::PUSH_LOCAL);
                    chunk.write_byte(idx);
                }
            }

            self.type_stack.push(Variable {
                value_type: variable.value_type,
                read_only: variable.read_only,
            });
        }

        Ok(())
    }

    fn print(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.expression(chunk)?;
        chunk.write_byte(opcode::PRINT);
        self.type_stack.pop();
        self.consume(Token::SemiColon)?;
        Ok(())
    }

    fn if_expression(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.expression(chunk)?;
        self.consume(Token::LeftBrace)?;
        chunk.write_byte(opcode::JMP_IF_FALSE);
        self.type_stack.pop();

        let if_jmp_idx = chunk.write_byte(0);

        let stack_top = self.type_stack.len();
        self.block_expression(chunk)?;

        if self.scanner.match_token(Token::Else)? {
            let (if_type, if_is_read_only) = {
                if self.type_stack.len() == stack_top {
                    (None, true)
                } else {
                    let t = self.type_stack.pop().unwrap();
                    (Some(t.value_type), t.read_only)
                }
            };

            chunk.write_byte(opcode::JMP);
            let jmp_skip_else_idx = chunk.write_byte(0);

            chunk.code[if_jmp_idx] = (chunk.code.len() - if_jmp_idx) as u8;

            self.consume(Token::LeftBrace)?;
            self.block_expression(chunk)?;

            let (else_type, else_is_read_only) = {
                if self.type_stack.len() == stack_top {
                    (None, true)
                } else {
                    let t = self.type_stack.last().unwrap();
                    (Some(t.value_type.clone()), t.read_only)
                }
            };

            if else_type != if_type {
                return Err(format!(
                    "if & else arms return different types. if: {:?}, else: {:?}",
                    if_type, else_type
                ));
            }

            if else_is_read_only != if_is_read_only {
                // one is read only, make the result read only
                if let Some(t) = self.type_stack.last_mut() {
                    t.read_only = true;
                }
            }

            chunk.code[jmp_skip_else_idx] = (chunk.code.len() - jmp_skip_else_idx) as u8;
        } else {
            // If there is no else, this cannot always return a value.
            assert!(self.type_stack.len() >= stack_top);
            unsafe {
                self.type_stack.set_len(stack_top);
            }
            chunk.code[if_jmp_idx] = (chunk.code.len() - if_jmp_idx) as u8;
        }

        Ok(())
    }

    fn patch_break(&mut self, chunk: &mut Chunk, start_idx: usize, jmp_idx: usize) {
        self.unpatched_break_offsets.retain(|&idx| {
            if idx > start_idx {
                chunk.code[idx] = (jmp_idx - idx) as u8;
                false
            } else {
                true
            }
        });
    }

    fn while_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let prev_in_loop = self.is_in_loop;
        self.is_in_loop = true;

        let loop_begin_idx = chunk.code.len() + 1;
        self.equality(chunk)?;
        self.consume(Token::LeftBrace)?;
        chunk.write_byte(opcode::JMP_IF_FALSE);
        self.type_stack.pop();
        let cond_break_idx = chunk.write_byte(0);

        self.scoped_block(chunk, |cm, ch| {
            while !cm.scanner.match_token(Token::RightBrace)? {
                cm.rule(ch)?;
            }

            cm.patch_break(ch, loop_begin_idx, ch.code.len());
            Ok(())
        })?;

        chunk.write_byte(opcode::LOOP);
        chunk.write_byte((chunk.code.len() - loop_begin_idx + 1) as u8);
        chunk.code[cond_break_idx] = (chunk.code.len() - cond_break_idx) as u8;

        self.is_in_loop = prev_in_loop;
        Ok(())
    }

    fn scoped_block<F>(&mut self, chunk: &mut Chunk, block: F) -> Result<(), String>
    where
        F: FnOnce(&mut Self, &mut Chunk) -> Result<(), String>,
    {
        let push_local = {
            if let Some(top) = self.stack_frames.last_mut() {
                top.current_scope += 1;
                false
            } else {
                self.stack_frames.push(StackFrame {
                    current_scope: 0,
                    locals: Vec::new(),
                });
                chunk.code.push(opcode::PUSH_FRAME);
                true
            }
        };

        let locals_top = self.stack_frames.last().unwrap().locals.len();
        block(self, chunk)?;

        assert!(!self.stack_frames.is_empty());
        if push_local {
            self.stack_frames.pop();
            chunk.code.push(opcode::POP_FRAME);
        } else {
            let frame = self.stack_frames.last_mut().unwrap();
            while frame.locals.len() > locals_top {
                chunk.code.push(opcode::LOCAL_POP);
                frame.locals.pop();
            }
        }

        Ok(())
    }

    fn for_block(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.scoped_block(chunk, |cm, ch| {
            while !cm.scanner.match_token(Token::RightBrace)? {
                cm.rule(ch)?;
            }
            Ok(())
        })
    }

    fn for_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let prev_in_loop = self.is_in_loop;
        self.is_in_loop = false; // disable break (maybe enable later)

        self.scoped_block(chunk, |cm, ch| {
            let identifier_name = {
                match cm.scanner.scan_token()? {
                    Token::Identifier(i) => i,
                    _ => return Err("Expected identifier".to_owned()),
                }
            };

            cm.consume(Token::Colon)?;
            let start = {
                match cm.scanner.scan_token()? {
                    Token::Integer(i) => i,
                    _ => return Err("Expected number".to_owned()),
                }
            };

            let is_inclusive = if cm.scanner.match_token(Token::DotDot)? {
                false
            } else if cm.scanner.match_token(Token::DotDotEqual)? {
                true
            } else {
                return Err("Expected range delimiter '..' or '..='".to_owned());
            };

            let end = {
                match cm.scanner.scan_token()? {
                    Token::Integer(i) => i,
                    _ => return Err("Expected number".to_owned()),
                }
            };

            let frame = cm.stack_frames.last_mut().unwrap();
            let var_idx = frame.locals.len() as u8;
            frame.locals.push(LocalVariable {
                name: identifier_name.to_string(),
                v: Variable {
                    read_only: true,
                    value_type: ValueType::Integer,
                },
                scope: frame.current_scope,
            });
            cm.integer(ch, start);
            ch.write_byte(opcode::DEFINE_LOCAL);
            cm.type_stack.pop();

            cm.consume(Token::LeftBrace)?;
            cm.scoped_block(ch, |cm, ch| {
                let loop_begin_idx = ch.write_byte(opcode::PUSH_LOCAL);
                ch.write_byte(var_idx);
                cm.integer(ch, end);
                if is_inclusive {
                    ch.write_byte(opcode::LESS_EQUAL);
                } else {
                    ch.write_byte(opcode::LESS);
                }

                ch.write_byte(opcode::JMP_IF_FALSE);
                let cond_break_idx = ch.write_byte(0);

                cm.for_block(ch)?;

                // This will need to be shifted to the top of the loop if breaks are to be enabled.
                ch.write_byte(opcode::PUSH_LOCAL);
                ch.write_byte(var_idx);
                cm.integer(ch, 1);
                ch.write_byte(opcode::ADD);
                ch.write_byte(opcode::SET_LOCAL);
                ch.write_byte(var_idx);

                ch.write_byte(opcode::LOOP);
                ch.write_byte((ch.code.len() - loop_begin_idx) as u8);
                ch.code[cond_break_idx] = (ch.code.len() - cond_break_idx) as u8;

                Ok(())
            })?;
            Ok(())
        })?;

        self.is_in_loop = prev_in_loop;
        Ok(())
    }

    fn break_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        if !self.is_in_loop {
            return Err("break can only be used in a loop".to_owned());
        }

        self.consume(Token::SemiColon)?;

        chunk.write_byte(opcode::BREAK);
        let idx = chunk.write_byte(0);
        self.unpatched_break_offsets.push(idx);

        Ok(())
    }

    fn continue_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        if !self.is_in_loop {
            return Err("continue can only be used in a loop".to_owned());
        }

        self.consume(Token::SemiColon)?;

        chunk.write_byte(opcode::JMP);
        let idx = chunk.write_byte(0);
        self.unpatched_break_offsets.push(idx);

        Ok(())
    }

    fn read_expression(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        chunk.write_byte(opcode::READ_INPUT);

        let value_type = self.parse_type()?;
        let type_code = match value_type {
            ValueType::Integer => 0,
            ValueType::Float => 1,
            ValueType::Str => 2,
            t => return Err(format!("Cannot read type {:?}", t)),
        };

        self.type_stack.push(Variable {
            value_type,
            read_only: true,
        });
        chunk.write_byte(type_code);

        Ok(())
    }

    fn block_expression(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.scoped_block(chunk, |cm, ch| {
            while !cm.scanner.match_token(Token::RightBrace)? {
                cm.rule(ch)?;
            }

            Ok(())
        })?;

        Ok(())
    }

    fn comparison_right(&mut self, chunk: &mut Chunk, op: u8) -> Result<(), String> {
        self.term(chunk)?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            match left_type {
                ValueType::Integer | ValueType::Float => chunk.write_byte(op),
                ValueType::Str => return Err("Cannot use comparison with string".to_owned()),
                ValueType::Bool => return Err("Cannot use comparison with bool".to_owned()),
                ValueType::Vector(_) => return Err("Cannot use comparison with vector".to_owned()),
                ValueType::HashMap(_) => {
                    return Err("Cannot use comparison with hash map".to_owned())
                }
            };
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn comparison(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.term(chunk)?;

        if self.scanner.match_token(Token::Less)? {
            self.comparison_right(chunk, opcode::LESS)?;
        } else if self.scanner.match_token(Token::LessEqual)? {
            self.comparison_right(chunk, opcode::LESS_EQUAL)?;
        } else if self.scanner.match_token(Token::Greater)? {
            self.comparison_right(chunk, opcode::GREATER)?;
        } else if self.scanner.match_token(Token::GreaterEqual)? {
            self.comparison_right(chunk, opcode::GREATER_EQUAL)?;
        }

        Ok(())
    }

    fn equality_right(&mut self, chunk: &mut Chunk, op: u8) -> Result<(), String> {
        self.comparison(chunk)?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            match left_type {
                ValueType::Integer
                | ValueType::Float
                | ValueType::Str
                | ValueType::Bool
                | ValueType::Vector(_)
                | ValueType::HashMap(_) => chunk.write_byte(op),
            };
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn equality(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.comparison(chunk)?;

        if self.scanner.match_token(Token::EqualEqual)? {
            self.equality_right(chunk, opcode::EQ)?;
        } else if self.scanner.match_token(Token::BangEqual)? {
            self.equality_right(chunk, opcode::NEQ)?;
        }

        Ok(())
    }

    fn factor_right(&mut self, chunk: &mut Chunk, op: u8) -> Result<(), String> {
        self.index(chunk)?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            match left_type {
                ValueType::Integer | ValueType::Float => chunk.write_byte(op),
                _ => return Err("Type error".to_owned()),
            };
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn factor(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.index(chunk)?;

        loop {
            if self.scanner.match_token(Token::Star)? {
                self.factor_right(chunk, opcode::MUL)?;
            } else if self.scanner.match_token(Token::Slash)? {
                self.factor_right(chunk, opcode::DIV)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn index_vec(&mut self, chunk: &mut Chunk, read_only: bool, elem_type: &ValueType) -> Result<(), String> {
        self.expression(chunk)?;
        let index_type = self.type_stack.pop().unwrap();
        if index_type.value_type != ValueType::Integer {
            return Err(format!(
                "Can only index using Integer. Got {}",
                index_type.value_type
            ));
        }

        self.consume(Token::RightBracket)?;

        if self.scanner.match_token(Token::Equal)? {
            self.expression(chunk)?;
            let new_value_type = self.type_stack.pop().unwrap().value_type;
            if new_value_type != *elem_type {
                return Err(format!(
                    "Type mismatch for setting vector. Expected {}, got {}",
                    *elem_type, new_value_type
                ));
            }

            chunk.write_byte(opcode::SET_VEC);
        } else {
            chunk.write_byte(opcode::INDEX_VEC);
        }

        self.type_stack.push(Variable {
            value_type: (*elem_type).clone(),
            read_only,
        });

        Ok(())
    }

    fn index_hash_map(
        &mut self,
        chunk: &mut Chunk,
        read_only: bool,
        key_type: &ValueType,
        value_type: &ValueType,
    ) -> Result<(), String> {
        self.expression(chunk)?;
        let index_type = self.type_stack.pop().unwrap();
        if index_type.value_type != *key_type {
            return Err(format!(
                "Can only index using {}. Got {}",
                key_type, index_type.value_type
            ));
        }

        self.consume(Token::RightBracket)?;

        if self.scanner.match_token(Token::Equal)? {
            self.expression(chunk)?;
            let new_value_type = self.type_stack.pop().unwrap().value_type;
            if new_value_type != *value_type {
                return Err(format!(
                    "Type mismatch for setting hash map. Expected {}, got {}",
                    *value_type, new_value_type
                ));
            }

            chunk.write_byte(opcode::SET_HASH_MAP);
        } else {
            chunk.write_byte(opcode::INDEX_HASH_MAP);
        }

        self.type_stack.push(Variable {
            value_type: (*value_type).clone(),
            read_only,
        });

        Ok(())
    }

    fn index(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.primary(chunk)?;

        while self.scanner.match_token(Token::LeftBracket)? {
            let indexable = self.type_stack.pop().unwrap();
            match &indexable.value_type {
                ValueType::Vector(e) => self.index_vec(chunk, indexable.read_only, e)?,
                ValueType::HashMap(kv) => self.index_hash_map(chunk, indexable.read_only, &kv.0, &kv.1)?,
                t => return Err(format!("Cannot index type {}", t)),
            }
        }

        Ok(())
    }

    fn term_right(&mut self, chunk: &mut Chunk, op: u8) -> Result<(), String> {
        self.factor(chunk)?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            match left_type {
                ValueType::Integer | ValueType::Float => chunk.write_byte(op),
                _ => return Err("Type error".to_owned()),
            };
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn term(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.factor(chunk)?;

        loop {
            if self.scanner.match_token(Token::Plus)? {
                self.term_right(chunk, opcode::ADD)?;
            } else if self.scanner.match_token(Token::Minus)? {
                self.term_right(chunk, opcode::SUB)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn integer(&mut self, chunk: &mut Chunk, i: i32) {
        chunk.write_byte(opcode::CONSTANT_INTEGER);
        let mut encoder = var_len_int::Encoder::new(i);
        loop {
            let (byte, complete) = encoder.step_encode();
            chunk.write_byte(byte);

            if complete {
                break;
            }
        }
        self.type_stack.push(Variable {
            value_type: ValueType::Integer,
            read_only: true,
        });
    }

    fn float(&mut self, chunk: &mut Chunk, f: f32) {
        chunk.write_byte(opcode::CONSTANT_FLOAT);
        for byte in float::encode(f) {
            chunk.write_byte(byte);
        }
        self.type_stack.push(Variable {
            value_type: ValueType::Float,
            read_only: true,
        });
    }

    fn string(&mut self, chunk: &mut Chunk, s: &str) {
        chunk.write_byte(opcode::CONSTANT_STRING);
        chunk
            .write_byte(self.data_section.create_constant_str(s));
        self.type_stack.push(Variable {
            value_type: ValueType::Str,
            read_only: true,
        });
    }

    fn boolean(&mut self, chunk: &mut Chunk, b: bool) {
        let value = if b { 1 } else { 0 };

        chunk.write_byte(opcode::CONSTANT_BOOL);
        chunk.write_byte(value);

        self.type_stack.push(Variable {
            value_type: ValueType::Bool,
            read_only: true,
        });
    }

    fn primary(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let t = self.scanner.scan_token();
        match t {
            Ok(Token::Identifier(name)) => {
                self.identifier(chunk, name)?;
            }
            Ok(Token::LeftParen) => {
                self.expression(chunk)?;
                self.consume(Token::RightParen)?;
            }
            Ok(Token::Minus) => {
                let next = self.scanner.scan_token();
                match next {
                    Ok(Token::Integer(i)) => {
                        self.integer(chunk, -i);
                    }
                    Ok(Token::Float(f)) => {
                        self.float(chunk, -f);
                    }
                    _ => return Err("Expected number after '-'".to_owned()),
                }
            }
            Ok(Token::Integer(i)) => {
                self.integer(chunk, i);
            }
            Ok(Token::Float(f)) => {
                self.float(chunk, f);
            }
            Ok(Token::Str(s)) => {
                self.string(chunk, s);
            }
            Ok(Token::True) => {
                self.boolean(chunk, true);
            }
            Ok(Token::False) => {
                self.boolean(chunk, false);
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
        fn create_constant_str(&mut self, _s: &str) -> u8 {
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
    fn test_if_no_else_expression() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        let src = "
        let i: int = if true {
            10
        };
        ";

        assert_ne!(compiler.compile(&mut table, src).err(), None);
    }

    #[test]
    fn test_if_with_else_expression() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        let src = "
        let i: int = if true {
            if true { 1 } // not the last expression so should not cause an error
            if false { 5 } else { 10 }
        } else {
            20
        };
        ";

        assert_eq!(compiler.compile(&mut table, src).err(), None);
    }

    #[test]
    fn test_vector_decl() {
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let v: [int] = vec<int>{};
            ";

            assert_eq!(compiler.compile(&mut table, src).err(), None);
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let v: [int] = vec<int>{1};
            ";

            assert_eq!(compiler.compile(&mut table, src).err(), None);
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let v: [int] = vec<int>{1, 2, 3};
            ";

            assert_eq!(compiler.compile(&mut table, src).err(), None);
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let v: [string] = vec<string>{\"hello\", \"world\"};
            ";

            assert_eq!(compiler.compile(&mut table, src).err(), None);
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let v: [string] = vec<string>{0, \"world\"};
            ";

            assert_ne!(compiler.compile(&mut table, src).err(), None);
        }
    }

    #[test]
    fn test_vector_index() {
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let v: [int] = vec<int>{1,2,3};
                print v[0];
            ";

            assert_eq!(compiler.compile(&mut table, src).err(), None);
        }
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

    #[test]
    fn test_break_error() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler.compile(&mut table, "break;").is_err());
    }

    #[test]
    fn test_continue_error() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler.compile(&mut table, "continue;").is_err());
    }
}
