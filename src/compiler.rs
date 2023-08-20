use crate::chunk::Chunk;
use crate::float;
use crate::opcode;
use crate::scanner::{Scanner, Token};
use crate::var_len_int;

use std::collections::HashMap;
use std::fmt;
use std::vec::Vec;
use std::rc::Rc;

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
            Self::HashMap(tp) => format!("[{}: {}]", tp.0, tp.1)
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
    chunk: Chunk,
    type_stack: Vec<Variable>,
    data_section: &'a mut T,
    unpatched_break_offsets: Vec<usize>,
    is_in_loop: bool
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
            chunk: Chunk::new(),
            type_stack: Vec::new(),
            unpatched_break_offsets: Vec::new(),
            is_in_loop: false
        };

        compiler.compile()?;
        Ok(compiler.chunk)
    }
}

impl<'a, T: DataSection> SrcCompiler<'a, T> {
    fn clear_stack(&mut self) {
        let pop_count = self.type_stack.len();
        self.type_stack.clear();

        for _ in 0..pop_count {
            self.chunk.code.push(opcode::POP)
        }
    }

    fn consume(&mut self, token: Token) -> Result<(), String> {
        if self.scanner.match_token(token)? {
            return Ok(());
        }

        Err(format!("Expected {}, got {}", token, self.scanner.peek_token()?))
    }

    pub fn compile(&mut self) -> Result<(), String> {
        while self.scanner.peek_token()? != Token::Eof {
            self.rule()?;
        }

        Ok(())
    }

    fn rule(&mut self) -> Result<(), String> {
        let is_declaration = self.try_declaration()?;

        if !is_declaration {
            let is_block = self.expression()?;

            // only times an expression doesn't have to be followed by a ';'
            // is when it is the last expression of a block or if it is a block itself
            let peeked_token = self.scanner.peek_token()?;
            if peeked_token != Token::RightBrace && peeked_token != Token::Eof {
                if is_block {
                    if self.scanner.match_token(Token::SemiColon)? {
                        self.clear_stack();
                    }
                } else {
                    self.consume(Token::SemiColon)?;
                    self.clear_stack();
                }
            }
        }

        Ok(())
    }

    fn try_declaration(&mut self) -> Result<bool, String> {
        if self.scanner.match_token(Token::Let)? {
            self.let_statement()?;
        } else if self.scanner.match_token(Token::Print)? {
            self.print()?;
        } else if self.scanner.match_token(Token::While)? {
            self.while_statement()?;
            return Ok(true);
        } else if self.scanner.match_token(Token::For)? {
            self.for_statement()?;
            return Ok(true);
        } else if self.scanner.match_token(Token::Break)? {
            self.break_statement()?;
            return Ok(true);
        } else if self.scanner.match_token(Token::Continue)? {
            self.continue_statement()?;
            return Ok(true);
        } else {
            return Ok(false);
        }

        Ok(true)
    }

    fn expression(&mut self) -> Result<bool, String> {
        let mut is_block = false;
        if self.scanner.match_token(Token::LeftBrace)? {
            self.block_expression()?;
            is_block = true;
        } else if self.scanner.match_token(Token::If)? {
            self.if_expression()?;
            is_block = true;
        } else if self.scanner.match_token(Token::ReadInput)? {
            self.read_expression()?;
        } else if self.scanner.match_token(Token::Vector)? {
            self.vector_constructor()?;
        } else if self.scanner.match_token(Token::HashMap)? {
            self.hash_map_constructor()?;
        } else {
            self.equality()?;
        }

        Ok(is_block)
    }

    fn vector_constructor(&mut self) -> Result<(), String> {
        
        self.consume(Token::Less)?;
        let elem_type = self.parse_type()?;
        self.consume(Token::Greater)?;

        self.consume(Token::LeftBrace)?;
        if self.scanner.match_token(Token::RightBrace)? {

            self.chunk.write_byte(opcode::CREATE_VEC);
            self.chunk.write_byte(0);
            self.type_stack.push(Variable {
                value_type: ValueType::Vector(Rc::new(elem_type)),
                read_only: false,
            });
            return Ok(())
        }

        self.expression()?;
        {
            let tp = &self.type_stack.pop().unwrap().value_type;
            if elem_type != *tp {
                return Err(format!("Vector argument type mismatch. Expected {}, got {}", elem_type, tp));
            }
        }

        let mut arg_count = 1;
        while !self.scanner.match_token(Token::RightBrace)? {
            self.consume(Token::Comma)?;
            self.expression()?;
            let tp = &self.type_stack.pop().unwrap().value_type;
            if elem_type != *tp {
                return Err(format!("Vector argument type mismatch. Expected {}, got {}", elem_type, tp));
            }

            arg_count += 1;
        }

        self.chunk.write_byte(opcode::CREATE_VEC);
        self.chunk.write_byte(arg_count);
        self.type_stack.push(Variable {
            value_type: ValueType::Vector(Rc::new(elem_type)),
            read_only: false,
        });
        Ok(())
    }

    fn hash_map_constructor_arg(&mut self, key_type: &ValueType, value_type: &ValueType) -> Result<(), String> {
        self.expression()?;
        let expr_key_type = &self.type_stack.pop().unwrap().value_type;
        if key_type != expr_key_type {
            return Err(format!("Hash map key argument type mismatch. Expected {}, got {}", key_type, expr_key_type));
        }

        self.consume(Token::Colon)?;
        self.expression()?;
        let expr_value_type = &self.type_stack.pop().unwrap().value_type;
        if value_type != expr_value_type {
            return Err(format!("Hash map key argument type mismatch. Expected {}, got {}", key_type, expr_value_type));
        }

        Ok(())
    }

    fn hash_map_constructor(&mut self) -> Result<(), String> {
        self.consume(Token::Less)?;
        let key_type = self.parse_type()?;
        self.consume(Token::Comma)?;
        let value_type = self.parse_type()?;
        self.consume(Token::Greater)?;

        self.consume(Token::LeftBrace)?;
        if self.scanner.match_token(Token::RightBrace)? {

            self.chunk.write_byte(opcode::CREATE_HASH_MAP);
            self.chunk.write_byte(0);
            self.type_stack.push(Variable {
                value_type: ValueType::HashMap(Rc::new((key_type, value_type))),
                read_only: false,
            });
            return Ok(())
        }

        self.hash_map_constructor_arg(&key_type, &value_type)?;

        let mut arg_count = 1;
        while !self.scanner.match_token(Token::RightBrace)? {
            self.consume(Token::Comma)?;
            self.hash_map_constructor_arg(&key_type, &value_type)?;

            arg_count += 1;
        }

        self.chunk.write_byte(opcode::CREATE_HASH_MAP);
        self.chunk.write_byte(arg_count);
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
            }
            else {
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

    fn let_statement(&mut self) -> Result<(), String> {
        let mutable = self.scanner.match_token(Token::Mut)?;
        let identifier_name = match self.scanner.scan_token()? {
            Token::Identifier(ident) => ident,
            _ => return Err("Expected identifier after 'let'".to_owned()),
        };

        self.consume(Token::Colon)?;

        let var_type = self.parse_type()?;

        self.consume(Token::Equal)?;
        self.expression()?;

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

    fn identifier(&mut self, name: &str) -> Result<(), String> {
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
                    self.chunk.write_byte(opcode::SET_GLOBAL);
                    self.chunk
                        .write_byte(self.data_section.create_constant_str(name));
                }
                Identifier::Local(idx) => {
                    self.chunk.write_byte(opcode::SET_LOCAL);
                    self.chunk.write_byte(idx);
                }
            }

            assert!(!self.type_stack.is_empty());
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

    fn print(&mut self) -> Result<(), String> {
        self.expression()?;
        self.chunk.write_byte(opcode::PRINT);
        self.type_stack.pop();
        self.consume(Token::SemiColon)?;
        Ok(())
    }

    fn if_expression(&mut self) -> Result<(), String> {
        self.expression()?;
        self.consume(Token::LeftBrace)?;
        self.chunk.write_byte(opcode::JMP_IF_FALSE);
        self.type_stack.pop();

        let if_jmp_idx = self.chunk.write_byte(0);

        let stack_top = self.type_stack.len();
        self.block_expression()?;

        if self.scanner.match_token(Token::Else)? {
            let (if_type, if_is_read_only) = {
                if self.type_stack.len() == stack_top {
                    (None, true)
                } else {
                    let t = self.type_stack.pop().unwrap();
                    (Some(t.value_type), t.read_only)
                }
            };

            self.chunk.write_byte(opcode::JMP);
            let jmp_skip_else_idx = self.chunk.write_byte(0);

            self.chunk.code[if_jmp_idx] = (self.chunk.code.len() - if_jmp_idx) as u8;

            self.consume(Token::LeftBrace)?;
            self.block_expression()?;

            let (else_type, else_is_read_only) = {
                if self.type_stack.len() == stack_top {
                    (None, true)
                } else {
                    let t = self.type_stack.last().unwrap();
                    (Some(t.value_type.clone()), t.read_only)
                }
            };

            if else_type != if_type {
                return Err(format!("if & else arms return different types. if: {:?}, else: {:?}", if_type, else_type));
            }

            if else_is_read_only != if_is_read_only {
                // one is read only, make the result read only
                if let Some(t) = self.type_stack.last_mut() {
                    t.read_only = true;
                }
            }

            self.chunk.code[jmp_skip_else_idx] = (self.chunk.code.len() - jmp_skip_else_idx) as u8;
        }
        else {
            // If there is no else, this cannot always return a value.
            assert!(self.type_stack.len() >= stack_top);
            unsafe { self.type_stack.set_len(stack_top); }
            self.chunk.code[if_jmp_idx] = (self.chunk.code.len() - if_jmp_idx) as u8;
        }

        Ok(())
    }

    fn patch_break(&mut self, start_idx: usize, jmp_idx: usize) {
        self.unpatched_break_offsets.retain(|&idx| {
            if idx > start_idx {
                self.chunk.code[idx] = (jmp_idx - idx) as u8;
                false
            }
            else {
                true
            }
        });
    }

    fn while_statement(&mut self) -> Result<(), String> {
        let prev_in_loop = self.is_in_loop;
        self.is_in_loop = true;

        let loop_begin_idx = self.chunk.code.len() + 1;
        self.equality()?;
        self.consume(Token::LeftBrace)?;
        self.chunk.write_byte(opcode::JMP_IF_FALSE);
        self.type_stack.pop();
        let cond_break_idx = self.chunk.write_byte(0);

        self.scoped_block(|c| {
            while !c.scanner.match_token(Token::RightBrace)? {
                c.rule()?;
            }

            c.patch_break(loop_begin_idx, c.chunk.code.len());
            Ok(())
        })?;

        self.chunk.write_byte(opcode::LOOP);
        self.chunk
            .write_byte((self.chunk.code.len() - loop_begin_idx + 1) as u8);
        self.chunk.code[cond_break_idx] = (self.chunk.code.len() - cond_break_idx) as u8;

        self.is_in_loop = prev_in_loop;
        Ok(())
    }

    fn scoped_block<F>(&mut self, block: F) -> Result<(), String> 
        where F: FnOnce(&mut Self) -> Result<(), String>
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
                self.chunk.code.push(opcode::PUSH_FRAME);
                true
            }
        };

        let locals_top = self.stack_frames.last().unwrap().locals.len();
        block(self)?;

        assert!(!self.stack_frames.is_empty());
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

    fn for_block(&mut self) -> Result<(), String> {
        self.scoped_block(|c| {
            while !c.scanner.match_token(Token::RightBrace)? {
                c.rule()?;
            }
            Ok(())
        })
    }

    fn for_statement(&mut self) ->Result<(), String> {
        let prev_in_loop = self.is_in_loop;
        self.is_in_loop = false; // disable break (maybe enable later)

        self.scoped_block(|c| {
            let identifier_name = {
                match c.scanner.scan_token()? {
                    Token::Identifier(i) => i,
                    _ => return Err("Expected identifier".to_owned())
                }
            };

            c.consume(Token::Colon)?;
            let start = {
                match c.scanner.scan_token()? {
                    Token::Integer(i) => i,
                    _ => return Err("Expected number".to_owned())
                }
            };

            let is_inclusive = 
                if c.scanner.match_token(Token::DotDot)? {
                    false
                }
                else if c.scanner.match_token(Token::DotDotEqual)? {
                    true
                }
                else {
                    return Err("Expected range delimiter '..' or '..='".to_owned());
                };

            let end = {
                match c.scanner.scan_token()? {
                    Token::Integer(i) => i,
                    _ => return Err("Expected number".to_owned())
                }
            };

            let frame = c.stack_frames.last_mut().unwrap();
            let var_idx = frame.locals.len() as u8;
            frame.locals.push(LocalVariable {
                name: identifier_name.to_string(),
                v: Variable {
                    read_only: true,
                    value_type: ValueType::Integer,
                },
                scope: frame.current_scope,
            });
            c.integer(start);
            c.chunk.write_byte(opcode::DEFINE_LOCAL);
            c.type_stack.pop();

            c.consume(Token::LeftBrace)?;
            c.scoped_block(|c| {
                let loop_begin_idx = c.chunk.write_byte(opcode::PUSH_LOCAL);
                c.chunk.write_byte(var_idx);
                c.integer(end);
                if is_inclusive {
                    c.chunk.write_byte(opcode::LESS_EQUAL);
                } else {
                    c.chunk.write_byte(opcode::LESS);
                }

                c.chunk.write_byte(opcode::JMP_IF_FALSE);
                let cond_break_idx = c.chunk.write_byte(0);

                c.for_block()?;

                // This will need to be shifted to the top of the loop if breaks are to be enabled.
                c.chunk.write_byte(opcode::PUSH_LOCAL);
                c.chunk.write_byte(var_idx);
                c.integer(1);
                c.chunk.write_byte(opcode::ADD);
                c.chunk.write_byte(opcode::SET_LOCAL);
                c.chunk.write_byte(var_idx);

                c.chunk.write_byte(opcode::LOOP);
                c.chunk.write_byte((c.chunk.code.len() - loop_begin_idx) as u8);
                c.chunk.code[cond_break_idx] = (c.chunk.code.len() - cond_break_idx) as u8;

                Ok(())
            })?;
            Ok(())
        })?;

        self.is_in_loop = prev_in_loop;
        Ok(())
    }

    fn break_statement(&mut self) -> Result<(), String> {
        if !self.is_in_loop {
            return Err("break can only be used in a loop".to_owned());
        }

        self.consume(Token::SemiColon)?;

        self.chunk.write_byte(opcode::BREAK);
        let idx = self.chunk.write_byte(0);
        self.unpatched_break_offsets.push(idx);

        Ok(())
    }

    fn continue_statement(&mut self) -> Result<(), String> {
        if !self.is_in_loop {
            return Err("continue can only be used in a loop".to_owned());
        }

        self.consume(Token::SemiColon)?;

        self.chunk.write_byte(opcode::JMP);
        let idx = self.chunk.write_byte(0);
        self.unpatched_break_offsets.push(idx);

        Ok(())
    }

    fn read_expression(&mut self) -> Result<(), String> {
        self.chunk.write_byte(opcode::READ_INPUT);

        let value_type = self.parse_type()?;
        let type_code = match value_type {
            ValueType::Integer => 0,
            ValueType::Float => 1,
            ValueType::Str => 2,
            t => return Err(format!("Cannot read type {:?}", t))
        };

        self.type_stack.push(Variable {
            value_type,
            read_only: true,
        });
        self.chunk.write_byte(type_code);

        Ok(())
    }

    fn block_expression(&mut self) -> Result<(), String> {
        self.scoped_block(|c| {
            while !c.scanner.match_token(Token::RightBrace)? {
                c.rule()?;
            }

            Ok(())
        })?;

        Ok(())
    }

    fn comparison_right(&mut self, op: u8) -> Result<(), String> {
        self.term()?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            match left_type {
                ValueType::Integer | ValueType::Float => self.chunk.write_byte(op),
                ValueType::Str => return Err("Cannot use comparison with string".to_owned()),
                ValueType::Bool => return Err("Cannot use comparison with bool".to_owned()),
                ValueType::Vector(_) => return Err("Cannot use comparison with vector".to_owned()),
                ValueType::HashMap(_) => return Err("Cannot use comparison with hash map".to_owned()),
            };
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn comparison(&mut self) -> Result<(), String> {
        self.term()?;

        if self.scanner.match_token(Token::Less)? {
            self.comparison_right(opcode::LESS)?;
        } else if self.scanner.match_token(Token::LessEqual)? {
            self.comparison_right(opcode::LESS_EQUAL)?;
        } else if self.scanner.match_token(Token::Greater)? {
            self.comparison_right(opcode::GREATER)?;
        } else if self.scanner.match_token(Token::GreaterEqual)? {
            self.comparison_right(opcode::GREATER_EQUAL)?;
        }

        Ok(())
    }

    fn equality_right(&mut self, op: u8) -> Result<(), String> {
        self.comparison()?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            match left_type {
                ValueType::Integer | ValueType::Float | ValueType::Str | ValueType::Bool | ValueType::Vector(_) | ValueType::HashMap(_) => self.chunk.write_byte(op),
            };
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn equality(&mut self) -> Result<(), String> {
        self.comparison()?;

        if self.scanner.match_token(Token::EqualEqual)? {
            self.equality_right(opcode::EQ)?;
        } else if self.scanner.match_token(Token::BangEqual)? {
            self.equality_right(opcode::NEQ)?;
        }

        Ok(())
    }

    fn factor_right(&mut self, op: u8) -> Result<(), String> {
        self.index()?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            match left_type {
                ValueType::Integer | ValueType::Float => self.chunk.write_byte(op),
                _ => return Err("Type error".to_owned()),
            };
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn factor(&mut self) -> Result<(), String> {
        self.index()?;

        loop {
            if self.scanner.match_token(Token::Star)? {
                self.factor_right(opcode::MUL)?;
            } else if self.scanner.match_token(Token::Slash)? {
                self.factor_right(opcode::DIV)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn index(&mut self) -> Result<(), String> {
        self.primary()?;

        while self.scanner.match_token(Token::LeftBracket)? {
            let vector = self.type_stack.pop().unwrap();
            let elem_type = match &vector.value_type {
                ValueType::Vector(e) => {
                    e.clone()
                },
                t => return Err(format!("Cannot index type {}", t))
            };

            self.expression()?;
            let index_type = self.type_stack.pop().unwrap();
            if index_type.value_type != ValueType::Integer {
                return Err(format!("Can only index using Integer. Got {}", index_type.value_type));
            }

            self.consume(Token::RightBracket)?;

            if self.scanner.match_token(Token::Equal)? {
                self.expression()?;
                let new_value_type = self.type_stack.pop().unwrap().value_type;
                if new_value_type != *elem_type {
                    return Err(format!("Type mismatch for setting vector. Expected {}, got {}", *elem_type, new_value_type));
                }

                self.chunk.write_byte(opcode::SET_VEC);
            }
            else {
                self.chunk.write_byte(opcode::INDEX_VEC);
            }

            self.type_stack.push(Variable {
                value_type: (*elem_type).clone(),
                read_only: vector.read_only
            });
        }

        Ok(())
    }

    fn term_right(&mut self, op: u8) -> Result<(), String> {
        self.factor()?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            match left_type {
                ValueType::Integer | ValueType::Float => self.chunk.write_byte(op),
                _ => return Err("Type error".to_owned()),
            };
        }

        self.type_stack.pop();
        self.type_stack.last_mut().unwrap().read_only = true;
        Ok(())
    }

    fn term(&mut self) -> Result<(), String> {
        self.factor()?;

        loop {
            if self.scanner.match_token(Token::Plus)? {
                self.term_right(opcode::ADD)?;
            } else if self.scanner.match_token(Token::Minus)? {
                self.term_right(opcode::SUB)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn integer(&mut self, i: i32) {
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

    fn float(&mut self, f: f32) {
        self.chunk.write_byte(opcode::CONSTANT_FLOAT);
        for byte in float::encode(f) {
            self.chunk.write_byte(byte);
        }
        self.type_stack.push(Variable {
            value_type: ValueType::Float,
            read_only: true,
        });
    }

    fn string(&mut self, s: &str) {
        self.chunk.write_byte(opcode::CONSTANT_STRING);
        self.chunk
            .write_byte(self.data_section.create_constant_str(s));
        self.type_stack.push(Variable {
            value_type: ValueType::Str,
            read_only: true,
        });
    }

    fn boolean(&mut self, b: bool) {
        let value = if b { 1 } else { 0 };

        self.chunk.write_byte(opcode::CONSTANT_BOOL);
        self.chunk.write_byte(value);

        self.type_stack.push(Variable {
            value_type: ValueType::Bool,
            read_only: true,
        });
    }

    fn primary(&mut self) -> Result<(), String> {
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
                self.string(s);
            }
            Ok(Token::True) => {
                self.boolean(true);
            }
            Ok(Token::False) => {
                self.boolean(false);
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
        assert!(compiler
            .compile(&mut table, "break;")
            .is_err());
    }

    #[test]
    fn test_continue_error() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler
            .compile(&mut table, "continue;")
            .is_err());
    }
}
