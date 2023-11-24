use crate::builtin_functions;
use crate::chunk::Chunk;
use crate::float;
use crate::opcode;
use crate::scanner::{Scanner, Token, TokenLocation};
use crate::var_len_int;

use itertools::Itertools;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::vec::Vec;

/*
   PROGRAM:
       program -> rule* EOF

       rule -> declaration | expression ";"?

       type -> "int" | "float" | "string" | "[" type "]" | "[" type ":" type "]" | "fn" "(" (type ",")* ")" ("->" type)?
       literal -> NUMBER | FLOAT | STRING

   DECLARATIONS:
       declaration -> let_decl |
                      while_stmt |
                      for_stmt |
                      break |
                      function_definition |
                      print_stmt |
                      return_stmt |
                      enum_definition |
                      union_definition |
                      type_alias

           let_decl -> "let" ("mut")? ":" identifier type = expression ";"
           print_stmt -> print expression ";"
           while_stmt -> "while" expression block_expression
           break_stmt -> "break" ";"
           for_stmt "for" identifier ":" (NUMBER | expression) (".." | "..=") (NUMBER | expression) block_expression
           function_definition -> "fn" identifier "(" parameter_list ")" -> type block
           function_prototype -> "fn" identifier "(" parameter_list ")" -> type
                parameter_list -> parameter? ("," parameter)*
           return_stmt -> "return" expression?
           enum_definition -> "enum" identifier ":" type "{" enum_value* "}"
                enum_value -> identifier ("=" literal)? ","?
           struct_definition -> "struct" identifier "{" member* "}" ("impl" "{" function_definition* "}")?
                member -> identifier ":" type ","
           tuple_definition -> "(" member+ ")"
                member -> type ","
           union_definition -> "union" identifier ("<" template_type* ">")? "{" member* "}"
                template_type -> identifier ","
                member -> identifier ("(" type ")") ","
           interface_definition "interface" identifier "{" function_prototype* "}"
           type_alias -> "type_alias" identifier = type

   EXPRESSIONS:
       expression -> assignment |
                     block_expression |
                     if_expr |
                     if_let_expr |
                     vector_constructor |
                     hash_map_constructor |
                     struct_constructor |
                     union_constructor |
                     index |
                     field_index |
                     function_call |
                     read_expr

            block_expression -> "{" declaration* expression? "}"
            if_expr -> "if" expression block_expression ("else" block_expression)?
            if_let_expr -> "if" "let" identifier ("(" identifier+ ")")? = expression block_expression ("else" block_expression)?
            vector_constructor -> vec<type>{ expression? ("," expression)* }
            hash_map_constructor -> hash_map<type, type>{ hash_map_argument? ("," hash_map_argument)* }
                hash_map_argument -> expression ":" expression
            struct_constructor -> identifier "{" struct_member_init* "}"
                struct_member_init -> identifier "=" expression ","
            union_constructor -> identifier "." identifier "(" union_member_init* ")"
                union_member_init -> expresion ","
            function_call -> identifier "(" argument_list ")"
                argument_list -> identifier? ("," identifier)*
            read_expr -> "read" type
            enum_value -> identifier "." identifier
            index -> expression "[" expression "]"
            field_index -> expression "." identifier

            assignment -> identifier = expression | equality
            logical_or -> logical_and "||" logical_and
            logical_and -> equality "&&" equality
            equality -> comparison (("!=" | "==") comparison)?
            comparison -> term (("<" | "<=" | ">" | ">=") term)?
            term -> factor ( "-" | "+" factor )*
            factor -> index ( "*" | "/" index )*
            index -> primary ("[" expression "]")* |
            primary -> literal | identifier | "(" expression ")" | enum_value
*/

#[derive(Debug, PartialEq)]
struct FunctionType {
    return_type: ValueType,
    parameters: Vec<ValueType>,
}

#[derive(Debug, PartialEq)]
enum EnumValue {
    Str(String),
    Integer(i64),
    Float(f64),
}

#[derive(Debug, PartialEq)]
struct EnumType {
    base_type: ValueType,
    members: HashMap<String, EnumValue>,
}

#[derive(Debug, PartialEq)]
struct Method {
    name: String,
    function_type: FunctionType,
    method_idx: usize,
}

#[derive(Debug, PartialEq)]
struct StructType {
    members: Vec<(String, ValueType)>,
    methods: Vec<Method>,
}

impl StructType {
    fn get_member_idx(&self, name: &str) -> Option<usize> {
        for (idx, member) in self.members.iter().enumerate() {
            if member.0 == name {
                return Some(idx);
            }
        }

        None
    }

    fn get_method_call_idx(&self, name: &str) -> Option<usize> {
        for method in &self.methods {
            if method.name == name {
                return Some(method.method_idx);
            }
        }

        None
    }

    fn has_method(&self, method_name: &str, function_type: &FunctionType) -> bool {
        for m in &self.methods {
            if m.name == method_name && m.function_type == *function_type {
                return true;
            }
        }

        false
    }
}

#[derive(Debug, PartialEq)]
struct InterfaceType {
    methods: Vec<(String, FunctionType)>,
}

impl InterfaceType {
    fn does_struct_satisfy_interface(&self, struct_type: &StructType) -> bool {
        for m in &self.methods {
            if !struct_type.has_method(&m.0, &m.1) {
                return false;
            }
        }

        true
    }

    fn get_method_idx(&self, method_name: &str) -> Option<usize> {
        for (idx, m) in self.methods.iter().enumerate() {
            if m.0 == method_name {
                return Some(idx);
            }
        }

        None
    }
}

#[derive(Debug, PartialEq)]
struct UnionType {
    members: Vec<(String, Vec<ValueType>)>,
}

#[derive(Debug, PartialEq)]
enum UnionMemberType {
    Templated(usize),
    Fixed(ValueType),
}

#[derive(Debug, PartialEq)]
struct UnionTemplatedType {
    template_parameter_count: usize,
    members: Vec<(String, Vec<UnionMemberType>)>,
    instanced_unions: RefCell<Vec<(Vec<ValueType>, Rc<UnionType>)>>,
}

impl UnionTemplatedType {
    fn instance_union(
        &self,
        template_parameters: &Vec<ValueType>,
    ) -> Result<Rc<UnionType>, String> {
        if self.template_parameter_count != template_parameters.len() {
            return Err(format!(
                "Expected {} template parameters, got {}",
                self.template_parameter_count,
                template_parameters.len()
            ));
        }

        for instance in self.instanced_unions.borrow().iter() {
            if instance.0 == *template_parameters {
                return Ok(instance.1.clone());
            }
        }

        Ok(self.create_new_instance(template_parameters))
    }

    fn create_new_instance(&self, template_parameters: &Vec<ValueType>) -> Rc<UnionType> {
        let new_members = self
            .members
            .iter()
            .map(|(name, member_types)| {
                let new_types = member_types
                    .iter()
                    .map(|t| match t {
                        UnionMemberType::Templated(idx) => template_parameters[*idx].clone(),
                        UnionMemberType::Fixed(v) => v.clone(),
                    })
                    .collect();

                (name.clone(), new_types)
            })
            .collect::<Vec<(String, Vec<ValueType>)>>();

        let new_instance = Rc::new(UnionType {
            members: new_members,
        });
        self.instanced_unions
            .borrow_mut()
            .push((template_parameters.clone(), new_instance.clone()));

        new_instance
    }
}

#[derive(Debug, PartialEq)]
struct TupleType {
    element_types: Vec<ValueType>,
}

#[derive(Debug, PartialEq, Clone)]
enum UserType {
    Enum(Rc<EnumType>),
    Struct(Rc<StructType>),
    TemplatedUnion(Rc<UnionTemplatedType>),
    Union(Rc<UnionType>),
    Tuple(Rc<TupleType>),
    Interface(Rc<InterfaceType>),
}

#[derive(Debug, PartialEq, Clone)]
enum ValueType {
    Unit,
    Integer,
    Float,
    Str,
    Bool,
    Char,
    Vector(Rc<ValueType>),
    HashMap(Rc<(ValueType, ValueType)>),
    Function(Rc<FunctionType>),
    Enum(Rc<EnumType>),
    Struct(Rc<StructType>),
    Union(Rc<UnionType>),
    Tuple(Rc<TupleType>),
    Interface(Rc<InterfaceType>),
}

impl ValueType {
    fn can_assign(&self, src: &ValueType) -> bool {
        if self == src {
            return true;
        }

        return src.can_coerce_to(self);
    }

    fn can_coerce_to(&self, dest: &ValueType) -> bool {
        if let ValueType::Enum(e) = self {
            *dest == e.base_type
        } else {
            false
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Unit => "()".to_owned(),
            Self::Integer => "int".to_owned(),
            Self::Float => "float".to_owned(),
            Self::Str => "string".to_owned(),
            Self::Bool => "bool".to_owned(),
            Self::Char => "char".to_owned(),
            Self::Vector(tp) => format!("[{}]", tp),
            Self::HashMap(tp) => format!("[{}: {}]", tp.0, tp.1),
            Self::Function(f) => {
                let parameters = f.parameters.iter().join(", ");
                format!("fn({}) -> {}", parameters, f.return_type)
            }
            Self::Enum(e) => {
                format!("enum {:?}", e.members)
            }
            Self::Struct(s) => {
                format!("struct {:?}", s.members)
            }
            Self::Union(u) => {
                format!("union {:?}", u.members)
            }
            Self::Tuple(t) => {
                format!("tuple {:?}", t)
            }
            Self::Interface(i) => {
                format!("interface {:?}", i)
            }
        };

        write!(f, "{}", s)
    }
}

pub trait DataSection {
    fn create_constant_str(&mut self, s: &str) -> u16;
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
    method_counter: usize,
    user_types: HashMap<String, UserType>,
    tuple_types: Vec<Rc<TupleType>>,
}

pub struct SrcCompiler<'a, T> {
    src: &'a str,
    globals: &'a mut HashMap<String, Variable>,
    method_counter: &'a mut usize,
    stack_frames: Vec<StackFrame>,
    scanner: Scanner<'a>,
    type_stack: Vec<Variable>,
    data_section: &'a mut T,
    unpatched_break_offsets: Vec<usize>,
    is_in_loop: bool,
    function_chunks: HashMap<String, Chunk>,
    method_chunks: HashMap<usize, Chunk>,
    user_types: &'a mut HashMap<String, UserType>,
    tuple_types: &'a mut Vec<Rc<TupleType>>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            globals: HashMap::new(),
            method_counter: 0,
            user_types: HashMap::new(),
            tuple_types: vec![],
        }
    }

    fn define_rust_function(
        &mut self,
        name: &str,
        parameters: Vec<ValueType>,
        return_type: ValueType,
    ) {
        self.globals.insert(
            name.to_string(),
            Variable {
                value_type: ValueType::Function(Rc::new(FunctionType {
                    return_type,
                    parameters,
                })),
                read_only: true,
            },
        );
    }

    fn new_src_compiler<'a, T: DataSection>(
        &'a mut self,
        data_section: &'a mut T,
        src: &'a str,
    ) -> SrcCompiler<'a, T> {
        SrcCompiler::<'a, T> {
            src,
            globals: &mut self.globals,
            method_counter: &mut self.method_counter,
            stack_frames: Vec::new(),
            data_section,
            scanner: Scanner::new(src),
            type_stack: Vec::new(),
            unpatched_break_offsets: Vec::new(),
            is_in_loop: false,
            function_chunks: HashMap::new(),
            method_chunks: HashMap::new(),
            user_types: &mut self.user_types,
            tuple_types: &mut self.tuple_types,
        }
    }

    pub fn compile<'a, T: DataSection>(
        &mut self,
        data_section: &'a mut T,
        src: &'a str,
    ) -> Result<(Chunk, HashMap<String, Chunk>, HashMap<usize, Chunk>), String> {
        let mut compiler = self.new_src_compiler(data_section, src);

        let mut chunk = Chunk::new();
        //compiler.compile(&mut chunk)?;
        if let Err(e) = compiler.compile(&mut chunk) {
            return Err(e);
        }

        Ok((chunk, compiler.function_chunks, compiler.method_chunks))
    }

    pub fn create_function<'a, T: DataSection>(
        &mut self,
        data_section: &'a mut T,
        signature: &'a str,
    ) -> Result<String, String> {
        let mut return_type = ValueType::Unit;
        let name: String;
        let mut parameter_types: Vec<ValueType> = vec![];

        {
            let mut compiler = self.new_src_compiler(data_section, signature);
            if !compiler.scanner.match_token(Token::Fn)? {
                return Err("Expected 'Fn'".to_owned());
            }

            name = match compiler.scanner.scan_token()?.token {
                Token::Identifier(i) => i.to_string(),
                _ => return Err("Expected identifier".to_owned()),
            };

            if !compiler.scanner.match_token(Token::LeftParen)? {
                return Err("Expected '('".to_owned());
            }

            compiler.parse_commas_separate_list(Token::RightParen, |cm, _| {
                parameter_types.push(cm.parse_type()?);
                Ok(())
            })?;

            if compiler.scanner.match_token(Token::ThinArrow)? {
                return_type = compiler.parse_type()?;
            }
        }

        self.define_rust_function(&name, parameter_types, return_type);
        Ok(name.to_string())
    }
}

impl<'a, T: DataSection> SrcCompiler<'a, T> {
    fn clear_stack(&mut self, chunk: &mut Chunk, pop_count: usize) {
        if pop_count == 0 {
            return;
        }

        if pop_count == 1 && self.type_stack.last().unwrap().value_type == ValueType::Unit {
            // This will happen when a function returns no values. Since it does not return any values
            // no pop instruction should be emitted.
            //
            // Unit is on the type stack so types can still be validated, eg.
            // let a = func();
            // where func does not return a value.
            self.type_stack.pop();
        } else {
            for _ in 0..pop_count {
                self.type_stack.pop();
                chunk.code.push(opcode::POP)
            }
        }
    }

    fn consume(&mut self, expected_token: Token) -> Result<(), String> {
        if self.scanner.match_token(expected_token)? {
            return Ok(());
        }

        let t = self.scanner.peek_token()?;
        Err(self.make_error_msg(
            &format!("Expected {}, got {}", expected_token, t.token),
            &t.location,
        ))
    }

    fn alloc_method_idx(&mut self) -> usize {
        let idx = *self.method_counter;
        *self.method_counter += 1;
        idx
    }

    pub fn compile(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        while self.scanner.peek_token()?.token != Token::Eof {
            if self.scanner.match_token(Token::Fn)? {
                self.function_definition()?;
            } else if self.scanner.match_token(Token::Enum)? {
                self.enum_definition()?;
            } else if self.scanner.match_token(Token::Struct)? {
                self.struct_definition()?;
            } else if self.scanner.match_token(Token::Union)? {
                self.union_definition()?;
            } else if self.scanner.match_token(Token::Interface)? {
                self.interface_definition()?;
            } else if self.scanner.match_token(Token::TypeAlias)? {
                self.type_alias_definition()?;
                self.consume(Token::SemiColon)?;
            } else {
                self.rule(chunk)?;
            }
        }

        Ok(())
    }

    fn rule(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        if !self.try_statement(chunk)? {
            if self.scanner.match_token(Token::Return)? {
                self.return_statement(chunk)?;
            } else {
                let stack_top = self.type_stack.len();
                let is_block = self.expression(chunk)?;

                // only times an expression doesn't have to be followed by a ';'
                // is when it is the last expression of a block or if it is a block itself
                let peeked_token = self.scanner.peek_token()?.token;
                if peeked_token != Token::RightBrace && peeked_token != Token::Eof {
                    if is_block {
                        if self.scanner.match_token(Token::SemiColon)? {
                            self.clear_stack(chunk, self.type_stack.len() - stack_top);
                        }
                    } else {
                        self.consume(Token::SemiColon)?;
                        self.clear_stack(chunk, self.type_stack.len() - stack_top);
                    }
                }
            }
        }

        Ok(())
    }

    fn location_info_str(&self, location: &TokenLocation) -> String {
        let line = format!("{}: ", location.line);
        let spaces: String = std::iter::repeat(' ')
            .take(line.len() + location.column)
            .collect();

        spaces.to_string()
            + "Here\n"
            + &spaces
            + "|\n"
            + &spaces
            + "V\n"
            + &line
            + location.get_text_line(&self.src)
    }

    fn make_error_msg(&self, msg: &str, location: &TokenLocation) -> String {
        let location: String = self.location_info_str(location);
        return format!("{}\n{}", msg, location);
    }

    fn skip_to_matching_brace(&mut self) -> Result<(), String> {
        let mut scope = 1;

        loop {
            let t = self.scanner.scan_token()?;
            match t.token {
                Token::LeftBrace => scope += 1,
                Token::RightBrace => {
                    scope -= 1;
                    if scope == 0 {
                        return Ok(());
                    }
                }
                Token::Eof => break,
                _ => {}
            }
        }

        return Err(format!("Reached EOF, unmatched right brace"));
    }

    fn enum_definition(&mut self) -> Result<ValueType, String> {
        let (enum_name, enum_name_location) = self.match_identifier()?;

        self.consume(Token::Colon)?;
        let enum_base_type = self.parse_type()?;
        self.consume(Token::LeftBrace)?;

        let mut enum_type = EnumType {
            base_type: enum_base_type.clone(),
            members: HashMap::new(),
        };

        while !self.scanner.match_token(Token::RightBrace)? {
            let (member_name, member_name_location) = self.match_identifier()?;
            self.consume(Token::Equal)?;

            let enum_value = {
                let t = self.scanner.scan_token()?;
                match t.token {
                    Token::Str(s) => {
                        if enum_base_type != ValueType::Str {
                            return Err(self.make_error_msg(
                                &format!(
                                    "Enum type is string but enum value is {}",
                                    enum_base_type
                                ),
                                &t.location,
                            ));
                        }
                        EnumValue::Str(s.to_string())
                    }
                    Token::Integer(i) => {
                        if enum_base_type != ValueType::Integer {
                            return Err(self.make_error_msg(
                                &format!("Enum type is int but enum value is {}", enum_base_type),
                                &t.location,
                            ));
                        }
                        EnumValue::Integer(i)
                    }
                    Token::Float(f) => {
                        if enum_base_type != ValueType::Float {
                            return Err(self.make_error_msg(
                                &format!("Enum type is float but enum value is {}", enum_base_type),
                                &t.location,
                            ));
                        }
                        EnumValue::Float(f)
                    }
                    _ => return Err(self.make_error_msg("Expected name for enum", &t.location)),
                }
            };

            let member_already_exists = enum_type
                .members
                .insert(member_name.to_string(), enum_value)
                .is_some();

            if member_already_exists {
                return Err(self.make_error_msg(
                    &format!("{} already defined in enum {}", member_name, enum_name),
                    &member_name_location,
                ));
            }

            self.consume(Token::Comma)?;
        }

        let enum_type = Rc::new(enum_type);
        let type_already_exists = self
            .user_types
            .insert(enum_name.to_string(), UserType::Enum(enum_type.clone()))
            .is_some();

        if type_already_exists {
            return Err(self.make_error_msg(
                &format!("{} already defined", enum_name),
                &enum_name_location,
            ));
        }

        Ok(ValueType::Enum(enum_type))
    }

    fn struct_definition(&mut self) -> Result<(), String> {
        let (struct_name, struct_name_location) = self.match_identifier()?;

        if self.user_types.contains_key(&struct_name) {
            return Err(format!("{} already defined", struct_name));
        }

        let mut members_map = HashMap::new();

        self.consume(Token::LeftBrace)?;
        while !self.scanner.match_token(Token::RightBrace)? {
            let (member_name, member_name_location) = self.match_identifier()?;

            self.consume(Token::Colon)?;
            let member_type = self.parse_type()?;
            self.consume(Token::Comma)?;

            if members_map
                .insert(member_name.to_string(), member_type)
                .is_some()
            {
                return Err(self.make_error_msg(
                    &format!(
                        "Member {} already defined in struct {}",
                        member_name, struct_name
                    ),
                    &member_name_location,
                ));
            }
        }

        if self.user_types.contains_key(&struct_name) {
            return Err(self.make_error_msg(
                &format!("Symbol {} is already defined", struct_name),
                &struct_name_location,
            ));
        }

        let mut method_map = HashMap::new();
        let mut method_metadata_map = HashMap::new();
        if self.scanner.match_token(Token::Impl)? {
            self.consume(Token::LeftBrace)?;

            while !self.scanner.match_token(Token::RightBrace)? {
                self.consume(Token::Fn)?;

                self.consume(Token::LeftParen)?;
                self.consume(Token::SmallSelf)?;
                self.consume(Token::RightParen)?;

                let (function_name, function_name_location) = self.match_identifier()?;

                if members_map.contains_key(&function_name) {
                    return Err(self.make_error_msg(
                        &format!(
                            "Function defined as {}, but it is already defined as a data member",
                            function_name
                        ),
                        &function_name_location,
                    ));
                }

                self.consume(Token::LeftParen)?;

                let mut parameter_names = vec![];
                let mut parameter_types = vec![];

                self.parse_commas_separate_list(Token::RightParen, |cm, _parameter_idx| {
                    let (name, name_location) = cm.match_identifier()?;

                    if parameter_names.contains(&name) {
                        return Err(cm.make_error_msg(
                            &format!("Parameter {} redefined", name),
                            &name_location,
                        ));
                    }

                    parameter_names.push(name);

                    cm.consume(Token::Colon)?;
                    parameter_types.push(cm.parse_type()?);

                    Ok(())
                })?;

                let mut return_type = ValueType::Unit;
                if self.scanner.match_token(Token::ThinArrow)? {
                    return_type = self.parse_type()?;
                }

                self.consume(Token::LeftBrace)?;
                let scanner_method_head = self.scanner.clone();
                self.skip_to_matching_brace()?;

                let parameter_names_types = {
                    let mut v = vec![];
                    for (pname, ptype) in
                        std::iter::zip(parameter_names.into_iter(), parameter_types.iter())
                    {
                        v.push((pname, ptype.clone()));
                    }
                    v
                };

                let function_type = FunctionType {
                    return_type: return_type.clone(),
                    parameters: parameter_types,
                };

                let method_already_defined = method_map
                    .insert(function_name.clone(), function_type)
                    .is_some();
                if method_already_defined {
                    return Err(self.make_error_msg(
                        &format!("Method '{}' already defined", function_name),
                        &function_name_location,
                    ));
                }

                method_metadata_map.insert(
                    function_name,
                    (scanner_method_head, parameter_names_types, return_type),
                );
            }
        }

        let members = members_map
            .into_iter()
            .map(|(name, value_type)| (name, value_type))
            .collect::<Vec<(String, ValueType)>>();

        let methods = method_map
            .into_iter()
            .map(|(name, function_type)| Method {
                name,
                function_type,
                method_idx: self.alloc_method_idx(),
            })
            .collect::<Vec<Method>>();

        let has_methods = !methods.is_empty();
        let struct_type = Rc::new(StructType { members, methods });

        self.user_types.insert(
            struct_name.to_string(),
            UserType::Struct(struct_type.clone()),
        );

        // A bit of a hack tbh. We need to first parse just the function signatures
        // and then only generate code for them. This is because member functions are allows to
        // call other member functions.
        let saved_scanner = self.scanner.clone();

        if has_methods {
            for (method_name, (scanner, parameters, return_type)) in method_metadata_map {
                let mut locals = vec![];

                locals.push(LocalVariable {
                    name: "self".to_owned(),
                    v: Variable {
                        value_type: ValueType::Struct(struct_type.clone()),
                        read_only: true,
                    },
                    scope: 0,
                });

                for (pname, ptype) in &parameters {
                    locals.push(LocalVariable {
                        name: pname.clone(),
                        v: Variable {
                            value_type: (*ptype).clone(),
                            read_only: true,
                        },
                        scope: 0,
                    });
                }

                self.stack_frames.push(StackFrame {
                    current_scope: 0,
                    locals,
                });

                let mut chunk = Chunk::new();
                self.scanner = scanner;
                while !self.scanner.match_token(Token::RightBrace)? {
                    if self.scanner.match_token(Token::Return)? {
                        if !self.scanner.match_token(Token::SemiColon)? {
                            self.expression(&mut chunk)?;

                            let expr_type = self.type_stack.pop().unwrap().value_type;
                            if expr_type != return_type {
                                return Err(format!(
                                    "return type does not match return expression. Expected {}, got {}",
                                    return_type, expr_type
                                ));
                            }

                            self.consume(Token::SemiColon)?;
                        }

                        chunk.write_byte(opcode::RETURN);
                    } else {
                        self.rule(&mut chunk)?;
                    }
                }

                if chunk.code.is_empty() || *chunk.code.last().unwrap() != opcode::RETURN {
                    chunk.write_byte(opcode::RETURN);
                }

                self.stack_frames.pop();

                let method_idx = struct_type
                    .methods
                    .iter()
                    .find(|x| x.name == method_name)
                    .map(|x| x.method_idx)
                    .expect(&format!("Method {} not found!", method_name));
                self.method_chunks.insert(method_idx, chunk);
            }
        }

        self.scanner = saved_scanner;
        Ok(())
    }

    fn union_member_type(
        &mut self,
        template_parameter_types: &Vec<String>,
    ) -> Result<UnionMemberType, String> {
        if let Ok((type_name, _)) = self.match_identifier() {
            if let Some(idx) = template_parameter_types
                .iter()
                .position(|x| *x == type_name)
            {
                return Ok(UnionMemberType::Templated(idx));
            }

            if let Some(user_type) = self.user_types.get(&type_name) {
                match user_type {
                    UserType::Enum(e) => {
                        return Ok(UnionMemberType::Fixed(ValueType::Enum(e.clone())))
                    }
                    UserType::Struct(s) => {
                        return Ok(UnionMemberType::Fixed(ValueType::Struct(s.clone())))
                    }
                    UserType::TemplatedUnion(_u) => todo!(), //return Ok(UnionMemberType::Fixed(ValueType::Union(u.clone()))),
                    UserType::Union(_u) => todo!(), //return Ok(UnionMemberType::Fixed(ValueType::Union(u.clone()))),
                    UserType::Tuple(t) => {
                        return Ok(UnionMemberType::Fixed(ValueType::Tuple(t.clone())))
                    }
                    UserType::Interface(i) => {
                        return Ok(UnionMemberType::Fixed(ValueType::Interface(i.clone())))
                    }
                }
            }
        }

        return Ok(UnionMemberType::Fixed(self.parse_type()?));
    }

    fn union_definition(&mut self) -> Result<(), String> {
        let (union_name, union_name_location) = self.match_identifier()?;

        if self.user_types.contains_key(&union_name) {
            return Err(self.make_error_msg(
                &format!("{} already defined", union_name),
                &union_name_location,
            ));
        }

        let mut template_parameter_types = vec![];
        if self.scanner.match_token(Token::Less)? {
            template_parameter_types.push(self.match_identifier()?.0);
            while self.scanner.match_token(Token::Comma)? {
                let template_parameter = self.match_identifier()?.0;
                if template_parameter_types.contains(&template_parameter) {
                    return Err(format!(
                        "{} previously defined in template type list",
                        template_parameter
                    ));
                }
                template_parameter_types.push(template_parameter);
            }

            self.consume(Token::Greater)?;
        }

        self.consume(Token::LeftBrace)?;

        let mut members: Vec<(String, Vec<UnionMemberType>)> = vec![];

        while !self.scanner.match_token(Token::RightBrace)? {
            let (member_name, member_name_location) = self.match_identifier()?;

            let mut member_types = vec![];
            if self.scanner.match_token(Token::LeftParen)? {
                member_types.push(self.union_member_type(&mut template_parameter_types)?);

                while self.scanner.match_token(Token::Comma)? {
                    member_types.push(self.union_member_type(&mut template_parameter_types)?);
                }

                self.consume(Token::RightParen)?;
            }

            if members
                .iter()
                .find(|(name, _)| *name == member_name)
                .is_some()
            {
                return Err(self.make_error_msg(
                    &format!(
                        "Union {} already has a member {} defined",
                        union_name, member_name
                    ),
                    &member_name_location,
                ));
            }

            self.consume(Token::Comma)?;
            members.push((member_name, member_types));
        }

        let template_parameter_count = template_parameter_types.len();
        if template_parameter_count > 0 {
            self.user_types.insert(
                union_name.to_string(),
                UserType::TemplatedUnion(Rc::new(UnionTemplatedType {
                    template_parameter_count,
                    members,
                    instanced_unions: RefCell::new(vec![]),
                })),
            );
        } else {
            let mut fixed_members = vec![];

            for (name, member_types) in members.into_iter() {
                let mut fixed_types = vec![];
                for t in member_types {
                    if let UnionMemberType::Fixed(f) = t {
                        fixed_types.push(f);
                    } else {
                        panic!("Should not be fixed!");
                    }
                }

                fixed_members.push((name, fixed_types));
            }

            self.user_types.insert(
                union_name.to_string(),
                UserType::Union(Rc::new(UnionType {
                    members: fixed_members,
                })),
            );
        }

        Ok(())
    }

    fn interface_definition(&mut self) -> Result<(), String> {
        let (interface_name, interface_name_location) = self.match_identifier()?;

        if self.user_types.contains_key(&interface_name) {
            return Err(self.make_error_msg(
                &format!("{} already defined", interface_name),
                &interface_name_location,
            ));
        }

        self.consume(Token::LeftBrace)?;

        let mut method_map = HashMap::new();
        while !self.scanner.match_token(Token::RightBrace)? {
            self.consume(Token::Fn)?;
            let (function_name, function_name_location) = self.match_identifier()?;
            self.consume(Token::LeftParen)?;

            let mut parameter_names = vec![];
            let mut parameter_types = vec![];
            self.parse_commas_separate_list(Token::RightParen, |cm, _| {
                let (parameter_name, parameter_name_location) = cm.match_identifier()?;

                if parameter_names.contains(&parameter_name) {
                    return Err(cm.make_error_msg(
                        &format!("Parameter {} redefined", parameter_name),
                        &parameter_name_location,
                    ));
                }

                cm.consume(Token::Colon)?;

                parameter_names.push(parameter_name);
                parameter_types.push(cm.parse_type()?);

                Ok(())
            })?;

            let mut return_type = ValueType::Unit;
            if self.scanner.match_token(Token::ThinArrow)? {
                return_type = self.parse_type()?;
            }

            let function_type = FunctionType {
                return_type,
                parameters: parameter_types,
            };

            if method_map
                .insert(function_name.clone(), function_type)
                .is_some()
            {
                return Err(self.make_error_msg(
                    &format!("Method {} redefined", function_name),
                    &function_name_location,
                ));
            }
        }

        let interface_type = InterfaceType {
            methods: method_map
                .into_iter()
                .map(|(name, function_type)| (name.clone(), function_type))
                .collect(),
        };

        self.user_types
            .insert(interface_name, UserType::Interface(Rc::new(interface_type)));
        Ok(())
    }

    fn type_alias_definition(&mut self) -> Result<(), String> {
        let (alias_name, alias_name_location) = self.match_identifier()?;

        if self.user_types.contains_key(&alias_name) {
            return Err(self.make_error_msg(
                &format!("{} is already defined", alias_name),
                &alias_name_location,
            ));
        }

        self.consume(Token::Equal)?;

        let alias_type = self.parse_type()?;

        let user_type = match alias_type {
            ValueType::Unit => todo!(),
            ValueType::Integer => todo!(),
            ValueType::Float => todo!(),
            ValueType::Str => todo!(),
            ValueType::Bool => todo!(),
            ValueType::Char => todo!(),
            ValueType::Vector(_) => todo!(),
            ValueType::HashMap(_) => todo!(),
            ValueType::Function(_) => todo!(),
            ValueType::Enum(e) => UserType::Enum(e),
            ValueType::Struct(s) => UserType::Struct(s),
            ValueType::Union(u) => UserType::Union(u),
            ValueType::Tuple(t) => UserType::Tuple(t),
            ValueType::Interface(i) => UserType::Interface(i),
        };

        self.user_types.insert(alias_name, user_type);
        Ok(())
    }

    fn try_statement(&mut self, chunk: &mut Chunk) -> Result<bool, String> {
        if self.scanner.match_token(Token::Let)? {
            self.let_statement(chunk)?;
        } else if self.scanner.match_token(Token::Print)? {
            self.print(chunk)?;
        } else if self.scanner.match_token(Token::While)? {
            self.while_statement(chunk)?;
        } else if self.scanner.match_token(Token::For)? {
            self.for_statement(chunk)?;
        } else if self.scanner.match_token(Token::Break)? {
            self.break_statement(chunk)?;
        } else if self.scanner.match_token(Token::Continue)? {
            self.continue_statement(chunk)?;
        } else {
            return Ok(false);
        }

        Ok(true)
    }

    fn match_struct_type(&mut self) -> Result<Option<Rc<StructType>>, String> {
        let t = self.scanner.peek_token()?;
        if let Token::Identifier(i) = t.token {
            let user_type = match self.user_types.get(i) {
                Some(ut) => ut,
                None => return Ok(None),
            };

            if let UserType::Struct(s) = user_type {
                self.scanner.scan_token()?; // eat struct name
                return Ok(Some(s.clone()));
            }
        }

        Ok(None)
    }

    fn match_union_type(&mut self) -> Result<Option<Rc<UnionType>>, String> {
        if let Token::Identifier(i) = self.scanner.peek_token()?.token {
            let user_type = match self.user_types.get(i) {
                Some(ut) => ut,
                None => return Ok(None),
            }
            .clone();

            if let UserType::TemplatedUnion(u) = user_type {
                self.scanner.scan_token()?; // eat union name
                self.consume(Token::Less)?;
                let list = self.parse_template_type_arg_list()?;

                return Ok(Some(u.instance_union(&list)?));
            }

            if let UserType::Union(s) = user_type {
                self.scanner.scan_token()?; // eat struct name
                return Ok(Some(s.clone()));
            }
        }

        Ok(None)
    }

    pub fn match_identifier(&mut self) -> Result<(String, TokenLocation), String> {
        let t = self.scanner.peek_token()?;
        if let Token::Identifier(i) = t.token {
            self.scanner.scan_token()?; // eat identifier name
            return Ok((i.to_string(), t.location));
        }

        Err(format!(
            "Expected identifier, got {:?}\n{}",
            t.token,
            self.location_info_str(&t.location)
        ))
    }

    pub fn match_integer(&mut self) -> Result<(i64, TokenLocation), String> {
        let t = self.scanner.peek_token()?;
        if let Token::Integer(i) = t.token {
            self.scanner.scan_token()?; // eat identifier name
            return Ok((i, t.location));
        }

        Err(format!(
            "Expected integer, got {:?}\n{}",
            t.token,
            self.location_info_str(&t.location)
        ))
    }

    fn struct_method(
        &mut self,
        struct_type: &StructType,
        method_name: &str,
        method_name_location: &TokenLocation,
        chunk: &mut Chunk,
    ) -> Result<(), String> {
        // struct is already on the stack, first local 'self'

        let (function_type, method_idx) = match struct_type
            .methods
            .iter()
            .find(|x| x.name == method_name)
            .map(|x| (&x.function_type, x.method_idx))
        {
            Some(idx) => idx,
            None => {
                return Err(self.make_error_msg(
                    &format!("No method in struct named {}", method_name),
                    method_name_location,
                ))
            }
        };

        let argument_count_minus_self = self.parse_commas_separate_list(Token::RightParen, |cm, _| {
            cm.expression(chunk)?;
            Ok(())
        })?;

        if argument_count_minus_self != function_type.parameters.len() {
            return Err(self.make_error_msg(
                &format!("Incorrect argument count for method. Expected {}, got {}",
                         function_type.parameters.len(), argument_count_minus_self),
                method_name_location,
            ))
        }

        let stack_len = self.type_stack.len();
        let argument_count = argument_count_minus_self + 1;
        assert!(stack_len >= argument_count);
        chunk.write_byte(opcode::PUSH_METHOD);
        chunk.write_short(method_idx.try_into().unwrap());
        chunk.write_byte(opcode::CALL);
        chunk.write_byte(argument_count.try_into().unwrap());

        let args: Vec<_> = self.type_stack.drain(stack_len - argument_count_minus_self..stack_len).collect();
        for (i, a) in args.iter().enumerate() {
            let arg_type = &function_type.parameters[i];
            if a.value_type != *arg_type {
                return Err(self.make_error_msg(
                    &format!("Argument {} type is incorrect. Expected {}, got {}",
                             i + 1, arg_type, a.value_type),
                    method_name_location,
                ))
            }
        }

        self.type_stack.pop(); // pop self
        self.type_stack.push(Variable {
            value_type: function_type.return_type.clone(),
            read_only: true,
        });
        Ok(())
    }

    fn interface_method(
        &mut self,
        interface_type: &InterfaceType,
        method_name: &str,
        method_name_location: &TokenLocation,
        chunk: &mut Chunk,
    ) -> Result<(), String> {
        // interface is already on the stack, first local 'self'

        let method_slot_idx = match interface_type.get_method_idx(method_name) {
            Some(idx) => idx,
            None => {
                return Err(self.make_error_msg(
                    &format!("No method in interface named {}", method_name),
                    method_name_location,
                ))
            }
        };

        let function_type = &interface_type.methods[method_slot_idx].1;

        let argument_count = 1 /* self */+ self.parse_commas_separate_list(Token::RightParen, |cm, parameter_idx| {
            let location = cm.scanner.peek_token()?.location;
            if function_type.parameters.len() <= parameter_idx {
                return Err(cm.make_error_msg("Too many function arguments", &location));
            }

            cm.expression(chunk)?;

            if !function_type.parameters[parameter_idx].can_assign(&cm.type_stack.last().unwrap().value_type) {
                return Err(cm.make_error_msg("Incorrect argument type", &location));
            }

            Ok(())
        })?;

        let stack_len = self.type_stack.len();
        assert!(stack_len >= argument_count);
        chunk.write_byte(opcode::CALL_INTERFACE);
        chunk.write_byte(argument_count.try_into().unwrap());
        chunk.write_byte((method_slot_idx + 1).try_into().unwrap()); // object 'self' is at index 0

        self.type_stack.drain(stack_len - argument_count..stack_len);
        self.type_stack.push(Variable {
            value_type: function_type.return_type.clone(),
            read_only: true,
        });
        Ok(())
    }

    fn field(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let location = self.scanner.peek_token()?.location;

        let variable = self.type_stack.last().unwrap().clone();
        let (member_idx, member_name, member_type) = match &variable.value_type {
            ValueType::Struct(s) => {
                let (member_name, member_name_location) = self.match_identifier()?;
                if self.scanner.match_token(Token::LeftParen)? {
                    self.struct_method(s, &member_name, &member_name_location, chunk)?;
                    return Ok(());
                }

                let member_idx = match s.get_member_idx(&member_name) {
                    Some(i) => i,
                    None => {
                        return Err(self.make_error_msg(
                            &format!("Struct does not have member named '{}'", member_name),
                            &member_name_location,
                        ))
                    }
                };

                (member_idx, member_name, s.members[member_idx].1.clone())
            }
            ValueType::Tuple(t) => {
                let (member_idx, member_idx_location) = self.match_integer()?;

                if member_idx < 0 {
                    return Err(self.make_error_msg(
                        &format!("Negative tuple fields are not allowed, fot {}", member_idx),
                        &member_idx_location,
                    ));
                }

                let member_idx = member_idx as usize;
                let elem_count = t.element_types.len();

                if member_idx >= elem_count {
                    return Err(format!(
                        "Tuple index is too high. Must be less than {}, got {}",
                        elem_count, member_idx
                    ));
                }

                (
                    member_idx,
                    member_idx.to_string(),
                    t.element_types[member_idx].clone(),
                )
            }
            ValueType::Interface(i) => {
                let (member_name, member_name_location) = self.match_identifier()?;

                self.consume(Token::LeftParen)?;
                self.interface_method(i, &member_name, &member_name_location, chunk)?;
                return Ok(());
            }
            ValueType::Vector(elem_type) => {
                let (member_name, member_name_location) = self.match_identifier()?;
                match member_name.as_str() {
                    "len" => {
                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::vector::LEN);
                        chunk.write_byte(0);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Integer,
                            read_only: true,
                        });
                    }
                    "push" => {
                        if variable.read_only {
                            return Err(self.make_error_msg("Cannot push to immutable vector", &location));
                        }

                        self.consume(Token::LeftParen)?;
                        let arg_loc = self.scanner.peek_token()?.location;
                        self.expression(chunk)?;
                        self.consume(Token::RightParen)?;

                        let arg_type = self.type_stack.pop().unwrap();

                        if arg_type.value_type != *elem_type.as_ref() {
                            return Err(self.make_error_msg(
                                &format!(
                                    "Vector push expects type {}, got {}",
                                    arg_type.value_type, elem_type
                                ),
                                &arg_loc,
                            ));
                        }

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::vector::PUSH);
                        chunk.write_byte(1);
                    }
                    "pop" => {
                        if variable.read_only {
                            return Err(self.make_error_msg("Cannot push to immutable vector", &location));
                        }

                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::vector::POP);
                        chunk.write_byte(0);

                        self.type_stack.push(Variable {
                            value_type: elem_type.as_ref().clone(),
                            read_only: true,
                        });
                    }
                    "sort" => {
                        if variable.read_only {
                            return Err(self.make_error_msg("Cannot sort an immutable vector", &location));
                        }

                        self.consume(Token::LeftParen)?;

                        let arg_count = match elem_type.as_ref() {
                            ValueType::Integer | ValueType::Float | ValueType::Str | ValueType::Bool => 0,
                            _ => {
                                let location = self.scanner.peek_token()?.location;
                                self.expression(chunk)?;

                                if let ValueType::Function(f) = &self.type_stack.last().unwrap().value_type {
                                    if f.return_type != ValueType::Bool {
                                        return Err(self.make_error_msg("Sort predicate must return a bool", &location));
                                    }

                                    if f.parameters.len() != 2 {
                                        return Err(self.make_error_msg("Sort predicate must accept 2 parameters", &location));
                                    }

                                    if f.parameters[0] != *elem_type.as_ref() || f.parameters[1] != *elem_type.as_ref() {
                                        return Err(self.make_error_msg(
                                                &format!("Sort parameters must be type {}, got {} / {}", elem_type, f.parameters[0], f.parameters[1]), &location));
                                    }
                                }
                                else {
                                    return Err(self.make_error_msg("Sort takes a function as it's first parameter", &location));
                                }

                                self.type_stack.pop();
                                1
                            }
                        };

                        self.consume(Token::RightParen)?;

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::vector::SORT);
                        chunk.write_byte(arg_count);

                    }
                    "clear" => {
                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;
                        
                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::vector::CLEAR);
                        chunk.write_byte(0);
                    }
                    "clone" => {
                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;
                        
                        // no need to pop, expecting same vector type
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::vector::CLONE);
                        chunk.write_byte(0);
                    }
                    "remove" => {
                        self.consume(Token::LeftParen)?;
                        let arg_loc = self.scanner.peek_token()?.location;
                        self.expression(chunk)?;
                        self.consume(Token::RightParen)?;

                        let arg_type = self.type_stack.pop().unwrap();

                        if arg_type.value_type != *elem_type.as_ref() {
                            return Err(self.make_error_msg(
                                &format!(
                                    "Vector remove expects type {}, got {}",
                                    arg_type.value_type, elem_type
                                ),
                                &arg_loc,
                            ));
                        }
                        
                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::vector::REMOVE);
                        chunk.write_byte(1);
                    }
                    _ => {
                        return Err(self.make_error_msg(
                            &format!("Vector does not have method '{}'", member_name),
                            &member_name_location,
                        ));
                    }
                };

                return Ok(());
            }
            ValueType::Str => {
                let (member_name, member_name_location) = self.match_identifier()?;
                match member_name.as_str() {
                    "len" => {
                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::string::LEN);
                        chunk.write_byte(0);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Integer,
                            read_only: true,
                        });
                    }
                    "to_integer" => {
                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::string::TO_INTEGER);
                        chunk.write_byte(0);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Integer,
                            read_only: true,
                        });
                    }
                    "split" => {
                        self.consume(Token::LeftParen)?;
                        let char_loc = self.scanner.peek_token()?.location;
                        self.expression(chunk)?;
                        self.consume(Token::RightParen)?;

                        let delimiter_type = self.type_stack.pop().unwrap();

                        if delimiter_type.value_type != ValueType::Char {
                            return Err(self.make_error_msg(
                                &format!(
                                    "String delimiter is type char, but got {}",
                                    delimiter_type.value_type
                                ),
                                &char_loc,
                            ));
                        }

                        self.type_stack.pop(); // pop string
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::string::SPLIT);
                        chunk.write_byte(1);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Vector(Rc::new(ValueType::Str)),
                            read_only: false,
                        });
                    }
                    "trim" | "trim_start" | "trim_end" => {
                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(match member_name.as_str() {
                            "trim" => builtin_functions::string::TRIM,
                            "trim_start" => builtin_functions::string::TRIM_START,
                            "trim_end" => builtin_functions::string::TRIM_END,
                            _ => unreachable!(),
                        });
                        chunk.write_byte(0);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Str,
                            read_only: false,
                        });
                    }
                    "starts_with" | "ends_with" => {
                        self.consume(Token::LeftParen)?;
                        let substr_loc = self.scanner.peek_token()?.location;
                        self.expression(chunk)?;
                        self.consume(Token::RightParen)?;

                        let substr_type = self.type_stack.pop().unwrap();
                        if substr_type.value_type != ValueType::Str {
                            return Err(self.make_error_msg(
                                &format!(
                                    "substr type string, but got {}",
                                    substr_type.value_type
                                ),
                                &substr_loc,
                            ));
                        }

                        self.type_stack.pop(); // pop substr
                        self.type_stack.pop(); // pop string
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(match member_name.as_str() {
                            "starts_with" => builtin_functions::string::STARTS_WITH,
                            "ends_with" => builtin_functions::string::ENDS_WITH,
                            _ => unreachable!(),
                        });
                        chunk.write_byte(1);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Bool,
                            read_only: true,
                        });
                    }
                    "contains" => {
                        self.consume(Token::LeftParen)?;
                        let substr_loc = self.scanner.peek_token()?.location;
                        self.expression(chunk)?;
                        self.consume(Token::RightParen)?;

                        let substr_type = self.type_stack.pop().unwrap();
                        if substr_type.value_type != ValueType::Str {
                            return Err(self.make_error_msg(
                                &format!(
                                    "substr type string, but got {}",
                                    substr_type.value_type
                                ),
                                &substr_loc,
                            ));
                        }

                        self.type_stack.pop(); // pop substr
                        self.type_stack.pop(); // pop string
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::string::CONTAINS);
                        chunk.write_byte(1);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Bool,
                            read_only: true,
                        });
                    }
                    _ => {
                        return Err(self.make_error_msg(
                            &format!("String does not have method '{}'", member_name),
                            &member_name_location,
                        ));
                    }
                };

                return Ok(());
            }
            ValueType::HashMap(h) => {
                let (member_name, member_name_location) = self.match_identifier()?;
                match member_name.as_str() {
                    "contains_key" => {
                        self.consume(Token::LeftParen)?;
                        self.expression(chunk)?;
                        self.consume(Token::RightParen)?;

                        let index_type = self.type_stack.pop().unwrap();
                        self.type_stack.pop();

                        if index_type.value_type != h.0 {
                            return Err(self.make_error_msg(
                                &format!(
                                    "HashMap index is type {}, but got {}",
                                    index_type.value_type, h.0
                                ),
                                &member_name_location,
                            ));
                        }

                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::hashmap::CONTAINS_KEY);
                        chunk.write_byte(1);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Bool,
                            read_only: true,
                        });
                    }
                    "keys" => {
                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        chunk.write_byte(builtin_functions::hashmap::KEYS);
                        chunk.write_byte(0);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Vector(Rc::new(h.0.clone())),
                            read_only: true,
                        });
                    }
                    _ => {
                        return Err(self.make_error_msg(
                            &format!("HashMap does not have method '{}'", member_name),
                            &member_name_location,
                        ));
                    }
                };

                return Ok(());
            }
            ValueType::Char => {
                let (member_name, member_name_location) = self.match_identifier()?;
                match member_name.as_str() {
                    "to_lowercase" | "to_uppercase" => {
                        self.consume(Token::LeftParen)?;
                        self.consume(Token::RightParen)?;

                        self.type_stack.pop();
                        chunk.write_byte(opcode::CALL_BUILTIN);
                        match member_name.as_str() {
                            "to_lowercase" => { chunk.write_byte(builtin_functions::ch::TO_LOWERCASE); }
                            "to_uppercase" => { chunk.write_byte(builtin_functions::ch::TO_UPPERCASE); }
                            _ => unimplemented!()
                        }

                        chunk.write_byte(0);

                        self.type_stack.push(Variable {
                            value_type: ValueType::Char,
                            read_only: true,
                        });
                    }
                    _ => {
                        return Err(self.make_error_msg(
                            &format!("Char does not have method '{}'", member_name),
                            &member_name_location,
                        ));
                    }
                }
                return Ok(());
            }
            x => return Err(format!("Only structs have members, got {}", x)),
        };

        if self.scanner.match_token(Token::Equal)? {
            if variable.read_only {
                return Err(format!("Cannot set read only field {}", member_name));
            }

            self.expression(chunk)?;

            let expr_type = &self.type_stack.last().unwrap().value_type;
            if !member_type.can_assign(&expr_type) {
                return Err(format!("Expected type {}, got {}", member_type, expr_type));
            }

            chunk.write_byte(opcode::SET_FIELD);
            chunk.write_byte(member_idx.try_into().unwrap());
            self.type_stack.drain(self.type_stack.len() - 2..);
        } else {
            self.type_stack.pop();
            chunk.write_byte(opcode::GET_FIELD);
            chunk.write_byte(member_idx.try_into().unwrap());
            self.type_stack.push(Variable {
                value_type: member_type,
                read_only: variable.read_only,
            });
        }

        Ok(())
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
        } else if self.scanner.match_token(Token::Tuple)? {
            self.tuple_constructor(chunk)?;
        } else if let Ok(Some(struct_type)) = self.match_struct_type() {
            self.struct_constructor(struct_type, chunk)?;
        } else if let Ok(Some(union_type)) = self.match_union_type() {
            self.union_constructor(union_type, chunk)?;
        } else {
            self.logical_or(chunk)?;
        }

        Ok(is_block)
    }

    fn vector_constructor(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.consume(Token::Less)?;
        let elem_type = self.parse_type()?;
        self.consume(Token::Greater)?;

        if self.scanner.match_token(Token::LeftBrace)? {
            let arg_count = self.parse_commas_separate_list(Token::RightBrace, |cm, _| {
                cm.expression(chunk)?;
                let tp = cm.type_stack.pop().unwrap().value_type;
                if elem_type != tp {
                    let interface_satisfied = match (&elem_type, &tp) {
                        (ValueType::Interface(i), ValueType::Struct(s)) => {
                            if i.does_struct_satisfy_interface(s) {
                                cm.create_interface_object_for_struct(i, s, chunk);
                                true
                            } else {
                                false
                            }
                        }
                        _ => false,
                    };

                    if !interface_satisfied {
                        return Err(format!(
                            "Vector argument type mismatch. Expected {}, got {}",
                            elem_type, tp
                        ));
                    }
                }
                Ok(())
            })?;

            chunk.write_byte(opcode::CREATE_VEC);
            chunk.write_byte(0);
            chunk.write_byte(arg_count.try_into().unwrap());
            self.type_stack.push(Variable {
                value_type: ValueType::Vector(Rc::new(elem_type)),
                read_only: false,
            });
        }
        else if self.scanner.match_token(Token::LeftBracket)? {
            let size_location = self.scanner.peek_token()?.location;
            match elem_type {
                ValueType::Integer | ValueType::Float | ValueType::Bool | ValueType::Str => {},
                _ => return Err(self.make_error_msg(
                                    &format!("Vector repeat intialiser can only be used with primitive types, got {}", elem_type),
                                    &size_location
                ))
            }

            self.expression(chunk)?;
            let size_type = self.type_stack.pop().unwrap().value_type;
            if size_type != ValueType::Integer {
                return Err(self.make_error_msg(
                    &format!("Vector size intialiser must be an integer, got {}", size_type),
                    &size_location
                ));
            }
            self.consume(Token::RightBracket)?;
            self.consume(Token::LeftBrace)?;

            let init_val_location = self.scanner.peek_token()?.location;
            self.expression(chunk)?;

            let init_val_type = self.type_stack.pop().unwrap().value_type;
            if init_val_type != elem_type {
                return Err(self.make_error_msg(
                    &format!("Vector argument type mismatch. Expected {}, got {}",
                    elem_type, init_val_type), &init_val_location));
            }

            self.consume(Token::RightBrace)?;

            chunk.write_byte(opcode::CREATE_VEC);
            chunk.write_byte(1);

            self.type_stack.push(Variable {
                value_type: ValueType::Vector(Rc::new(elem_type)),
                read_only: false,
            });
        }
        else {
            let location = self.scanner.peek_token()?.location;
            return Err(self.make_error_msg(
                &format!("Expected '{{' or '['"), &location));
        }

        Ok(())
    }

    fn struct_constructor(
        &mut self,
        struct_type: Rc<StructType>,
        chunk: &mut Chunk,
    ) -> Result<(), String> {
        //    hash_map_constructor -> identifier { struct_member_init }
        //        struct_member_init -> identifier "=" expression ","
        self.consume(Token::LeftBrace)?;

        let mut unset_members: HashSet<String> = struct_type
            .members
            .iter()
            .map(|(member_name, _)| member_name.to_string())
            .collect();
        let mut member_indices = Vec::new();

        while !self.scanner.match_token(Token::RightBrace)? {
            let (member_name, member_name_location) = self.match_identifier()?;

            let member_index = match struct_type.get_member_idx(&member_name) {
                Some(i) => i,
                None => {
                    return Err(self.make_error_msg(
                        &format!("{} is not a member of struct", member_name),
                        &member_name_location,
                    ))
                }
            };

            self.consume(Token::Equal)?;
            self.expression(chunk)?;
            self.consume(Token::Comma)?;
            member_indices.push(member_index.try_into().unwrap());

            unset_members.remove(&member_name);
        }

        if !unset_members.is_empty() {
            return Err(format!("Members {:?} not initialised", unset_members));
        }

        chunk.write_byte(opcode::CREATE_STRUCT);
        chunk.write_byte(struct_type.members.len().try_into().unwrap());
        for idx in member_indices.iter().rev() {
            chunk.write_byte(*idx);
        }

        self.type_stack
            .drain(self.type_stack.len() - member_indices.len()..);
        self.type_stack.push(Variable {
            value_type: ValueType::Struct(struct_type),
            read_only: false,
        });
        Ok(())
    }

    fn union_constructor(
        &mut self,
        union_type: Rc<UnionType>,
        chunk: &mut Chunk,
    ) -> Result<(), String> {
        //   union_constructor -> identifier "." identifier "(" union_member_init* ")"
        //       union_member_init -> expresion ","

        /*
        MyUnion.Member( expr );
         */
        self.consume(Token::Dot)?;

        let member_name = self.match_identifier()?.0;

        let (determinant, member) = match union_type
            .members
            .iter()
            .enumerate()
            .find(|(_, x)| x.0 == member_name)
        {
            Some(x) => x,
            None => return Err(format!("{} is not a member of union", member_name)),
        };

        let member_count = member.1.len();
        if member_count > 0 {
            self.consume(Token::LeftParen)?;

            for member_type in &member.1 {
                self.expression(chunk)?;

                let expr_type = &self.type_stack.pop().unwrap().value_type;
                if !member_type.can_assign(expr_type) {
                    return Err(format!(
                        "Cannot assign type {} to {}",
                        expr_type, member_type
                    ));
                }

                self.consume(Token::Comma)?;
            }

            self.consume(Token::RightParen)?;
        }

        chunk.write_byte(opcode::CREATE_UNION);
        chunk.write_byte(member.1.len().try_into().unwrap());
        chunk.write_byte(determinant.try_into().unwrap());

        self.type_stack.push(Variable {
            value_type: ValueType::Union(union_type),
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

    fn tuple_constructor(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.consume(Token::LeftParen)?;

        let mut type_list = vec![];
        while !self.scanner.match_token(Token::RightParen)? {
            self.expression(chunk)?;
            self.consume(Token::Comma)?;

            type_list.push(self.type_stack.pop().unwrap().value_type);
        }

        let tuple_type = self.instance_tuple(&type_list);
        self.type_stack.push(Variable {
            value_type: tuple_type,
            read_only: false,
        });

        chunk.write_byte(opcode::CREATE_TUPLE);
        chunk.write_byte(type_list.len().try_into().unwrap());

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

    fn instance_tuple(&mut self, type_list: &Vec<ValueType>) -> ValueType {
        let found = self
            .tuple_types
            .iter()
            .find(|x| x.element_types == *type_list);
        if let Some(x) = found {
            return ValueType::Tuple(x.clone());
        }

        let tuple_type = Rc::new(TupleType {
            element_types: type_list.clone(),
        });
        self.tuple_types.push(tuple_type.clone());
        ValueType::Tuple(tuple_type)
    }

    fn parse_tuple(&mut self) -> Result<ValueType, String> {
        let mut type_list = vec![];

        while !self.scanner.match_token(Token::RightParen)? {
            type_list.push(self.parse_type()?);
            self.consume(Token::Comma)?;
        }

        return Ok(self.instance_tuple(&type_list));
    }

    fn parse_template_type_arg_list(&mut self) -> Result<Vec<ValueType>, String> {
        let mut list = vec![];

        while !self.scanner.match_token(Token::Greater)? {
            list.push(self.parse_type()?);
            self.consume(Token::Comma)?;
        }

        Ok(list)
    }

    fn parse_commas_separate_list<F>(
        &mut self,
        terminal_token: Token,
        mut f: F,
    ) -> Result<usize, String>
    where
        F: FnMut(&mut Self, usize) -> Result<(), String>,
    {
        if self.scanner.match_token(terminal_token)? {
            return Ok(0);
        }

        f(self, 0)?;

        let mut idx = 1;
        while !self.scanner.match_token(terminal_token)? {
            self.consume(Token::Comma)?;
            f(self, idx)?;
            idx += 1;
        }

        Ok(idx)
    }

    fn parse_type_function(&mut self) -> Result<ValueType, String> {
        // "fn" "(" (type ",")* ")" ("->" type)?
        self.consume(Token::LeftParen)?;

        let mut parameters = vec![];
        self.parse_commas_separate_list(Token::RightParen, |cm, _| {
            parameters.push(cm.parse_type()?);
            Ok(())
        })?;

        let mut return_type = ValueType::Unit;
        if self.scanner.match_token(Token::ThinArrow)? {
            return_type = self.parse_type()?;
        }

        Ok(ValueType::Function(Rc::new(FunctionType {
            return_type,
            parameters,
        })))
    }

    fn parse_type(&mut self) -> Result<ValueType, String> {
        let t = self.scanner.scan_token()?;
        let var_type = match t.token {
            Token::TypeInt => ValueType::Integer,
            Token::TypeFloat => ValueType::Float,
            Token::TypeString => ValueType::Str,
            Token::TypeChar => ValueType::Char,
            Token::TypeBool => ValueType::Bool,
            Token::LeftBracket => self.parse_type_vec_or_map()?,
            Token::LeftParen => self.parse_tuple()?,
            Token::Fn => self.parse_type_function()?,
            Token::Identifier(i) => {
                if let Some(ut) = self.user_types.get(i) {
                    let ut = ut.clone();
                    match ut {
                        UserType::Enum(e) => return Ok(ValueType::Enum(e.clone())),
                        UserType::Struct(s) => return Ok(ValueType::Struct(s.clone())),
                        UserType::TemplatedUnion(u) => {
                            self.consume(Token::Less)?;
                            let list = self.parse_template_type_arg_list()?;
                            return Ok(ValueType::Union(u.instance_union(&list)?));
                        }
                        UserType::Union(u) => return Ok(ValueType::Union(u.clone())),
                        UserType::Tuple(t) => return Ok(ValueType::Tuple(t.clone())),
                        UserType::Interface(i) => return Ok(ValueType::Interface(i.clone())),
                    }
                }

                return Err(
                    self.make_error_msg(&format!("Expected type but got {}", i), &t.location)
                );
            }
            token => {
                return Err(
                    self.make_error_msg(&format!("Expected type but got {}", token), &t.location)
                )
            }
        };

        Ok(var_type)
    }

    fn return_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        if !self.scanner.match_token(Token::SemiColon)? {
            self.expression(chunk)?;

            //let expr_type = self.type_stack.pop().unwrap().value_type;
            //if expr_type != return_type {
            //    return Err(format!("return type does not match return expression. Expected {}, got {}", return_type, expr_type));
            //}

            self.consume(Token::SemiColon)?;
        }

        chunk.write_byte(opcode::RETURN);
        return Ok(());
    }

    fn function(&mut self, function_name: &str) -> Result<Chunk, String> {
        let mut chunk = Chunk::new();
        self.consume(Token::LeftParen)?;

        self.stack_frames.push(StackFrame {
            current_scope: 0,
            locals: Vec::new(),
        });

        let mut parameters: Vec<ValueType> = Vec::new();
        if !self.scanner.match_token(Token::RightParen)? {
            let mut parameter_names = vec![];
            {
                let (name, _) = self.match_identifier()?;

                self.consume(Token::Colon)?;

                let mutable = self.scanner.match_token(Token::Mut)?;
                let param_type = self.parse_type()?;

                parameter_names.push(name.clone());
                parameters.push(param_type.clone());

                self.stack_frames
                    .last_mut()
                    .unwrap()
                    .locals
                    .push(LocalVariable {
                        name,
                        v: Variable {
                            value_type: param_type,
                            read_only: !mutable,
                        },
                        scope: 0,
                    });
            }

            while !self.scanner.match_token(Token::RightParen)? {
                self.consume(Token::Comma)?;

                let (name, name_location) = self.match_identifier()?;

                self.consume(Token::Colon)?;

                let mutable = self.scanner.match_token(Token::Mut)?;
                let param_type = self.parse_type()?;

                if parameter_names.iter().contains(&name) {
                    return Err(self
                        .make_error_msg(&format!("Parameter {} redefined", name), &name_location));
                }

                parameter_names.push(name.clone());
                parameters.push(param_type.clone());

                self.stack_frames
                    .last_mut()
                    .unwrap()
                    .locals
                    .push(LocalVariable {
                        name,
                        v: Variable {
                            value_type: param_type,
                            read_only: !mutable,
                        },
                        scope: 0,
                    });
            }
        }

        let return_type = {
            if self.scanner.match_token(Token::ThinArrow)? {
                self.parse_type()?
            } else {
                ValueType::Unit
            }
        };

        let function_type = FunctionType {
            return_type: return_type.clone(),
            parameters,
        };

        self.globals.insert(
            function_name.to_string(),
            Variable {
                value_type: ValueType::Function(Rc::new(function_type)),
                read_only: true,
            },
        );
        self.consume(Token::LeftBrace)?;

        self.stack_frames.last_mut().unwrap().current_scope = 1;
        while !self.scanner.match_token(Token::RightBrace)? {
            if self.scanner.match_token(Token::Return)? {
                if !self.scanner.match_token(Token::SemiColon)? {
                    self.expression(&mut chunk)?;

                    let expr_type = self.type_stack.pop().unwrap().value_type;
                    if expr_type != return_type {
                        return Err(format!(
                            "return type does not match return expression. Expected {}, got {}",
                            return_type, expr_type
                        ));
                    }

                    self.consume(Token::SemiColon)?;
                }

                chunk.write_byte(opcode::RETURN);
            } else {
                self.rule(&mut chunk)?;
            }
        }

        if chunk.code.is_empty() || *chunk.code.last().unwrap() != opcode::RETURN {
            chunk.write_byte(opcode::RETURN);
        }

        self.stack_frames.pop();
        Ok(chunk)
    }

    fn function_definition(&mut self) -> Result<(), String> {
        let (identifier, identifier_location) = self.match_identifier()?;

        if self.globals.contains_key(&identifier) {
            return Err(self.make_error_msg(
                &format!("Global {} is already defined", identifier),
                &identifier_location,
            ));
        }

        let chunk = self.function(&identifier)?;

        self.function_chunks.insert(identifier, chunk);

        Ok(())
    }

    fn create_interface_object_for_struct(
        &mut self,
        interface_type: &InterfaceType,
        struct_type: &StructType,
        chunk: &mut Chunk,
    ) {
        for m in &interface_type.methods {
            chunk.write_byte(opcode::PUSH_METHOD);

            let idx = struct_type.get_method_call_idx(&m.0).unwrap();
            chunk.write_short(idx.try_into().unwrap());
        }

        chunk.write_byte(opcode::CREATE_TUPLE);
        chunk.write_byte((interface_type.methods.len() + 1).try_into().unwrap());
    }

    fn let_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let mutable = self.scanner.match_token(Token::Mut)?;
        let (identifier_name, identifier_name_location) = self.match_identifier()?;

        self.consume(Token::Colon)?;

        let var_type = self.parse_type()?;

        self.consume(Token::Equal)?;

        let rvalue_loc = self.scanner.peek_token()?.location;
        self.expression(chunk)?;

        if self.type_stack.is_empty() {
            return Err(self.make_error_msg("Expected value on right hand side of '=', got None", &rvalue_loc));
        }

        let expr_type = self.type_stack.pop().unwrap().value_type;
        if !var_type.can_assign(&expr_type) {
            let interface_satisfied = match (&var_type, &expr_type) {
                (ValueType::Interface(i), ValueType::Struct(s)) => {
                    if i.does_struct_satisfy_interface(s) {
                        self.create_interface_object_for_struct(i, s, chunk);
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            };

            if !interface_satisfied {
                return Err(self.make_error_msg(&format!("Expected type {}, got {}", var_type, expr_type), &rvalue_loc));
            }
        }

        if self.stack_frames.is_empty() {
            if self.globals.contains_key(&identifier_name) {
                return Err(self.make_error_msg(
                    &format!("Global {} is already defined", identifier_name),
                    &identifier_name_location,
                ));
            }

            self.globals.insert(
                identifier_name.to_string(),
                Variable {
                    read_only: !mutable,
                    value_type: var_type,
                },
            );

            chunk.write_byte(opcode::DEFINE_GLOBAL);
            let v = self
                .data_section
                .create_constant_str(&identifier_name)
                .try_into()
                .unwrap();
            self.write_var_len_int(chunk, v);
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
                return Err(self.make_error_msg(
                    &format!(
                        "Local {} is already defined in current scope",
                        identifier_name
                    ),
                    &identifier_name_location,
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
                (local, Identifier::Local(idx.try_into().unwrap()), "Local")
            } else {
                (self.find_global(name)?, Identifier::Global, "Global")
            }
        };

        if self.scanner.match_token(Token::Equal)? {
            self.expression(chunk)?;

            if self.type_stack.is_empty() {
                return Err("Expected right hand side value, got None".to_owned());
            }

            let expr_type = self.type_stack.last().unwrap().value_type.clone();
            if variable.value_type != expr_type {
                let interface_satisfied = match (&variable.value_type, &expr_type) {
                    (ValueType::Interface(i), ValueType::Struct(s)) => {
                        if i.does_struct_satisfy_interface(s) {
                            self.create_interface_object_for_struct(i, s, chunk);
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                };

                if !interface_satisfied {
                    return Err(format!(
                        "Expected type {}, got {}",
                        variable.value_type, expr_type
                    ));
                }
            }

            if variable.read_only {
                return Err(format!("{} {} is ready only", type_str, name));
            }

            match symbol {
                Identifier::Global => {
                    chunk.write_byte(opcode::SET_GLOBAL);
                    let v = self
                        .data_section
                        .create_constant_str(name)
                        .try_into()
                        .unwrap();
                    self.write_var_len_int(chunk, v);
                }
                Identifier::Local(idx) => {
                    chunk.write_byte(opcode::SET_LOCAL);
                    chunk.write_byte(idx);
                }
            }

            assert!(!self.type_stack.is_empty());
            self.type_stack.pop();
        } else if self.scanner.match_token(Token::LeftParen)? {
            self.call(chunk, name)?;
        } else {
            match symbol {
                Identifier::Global => {
                    chunk.write_byte(opcode::PUSH_GLOBAL);
                    let v = self
                        .data_section
                        .create_constant_str(name)
                        .try_into()
                        .unwrap();
                    self.write_var_len_int(chunk, v);
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
        self.scoped_block(chunk, |cm, ch| cm.if_expression_impl(ch))
    }

    fn parse_if_let_expression(
        &mut self,
        chunk: &mut Chunk,
    ) -> Result<(usize, Vec<String>, Rc<UnionType>), String> {
        let union_name = self.match_identifier()?.0;

        let mut template_parameters = vec![];

        let union_type = if self.scanner.match_token(Token::Less)? {
            while !self.scanner.match_token(Token::Greater)? {
                template_parameters.push(self.parse_type()?);
                self.consume(Token::Comma)?;
            }

            match self.user_types.get(&union_name) {
                Some(user_type) => {
                    let templated_union_type = match user_type {
                        UserType::TemplatedUnion(u) => u,
                        _ => return Err(format!("Not a templated union")),
                    };

                    templated_union_type.instance_union(&template_parameters)?
                }
                None => return Err(format!("No union named {}", union_name)),
            }
        } else {
            match self.user_types.get(&union_name) {
                Some(user_type) => {
                    let union_type = match user_type {
                        UserType::Union(u) => u,
                        _ => return Err(format!("Not a union")),
                    };

                    union_type.clone()
                }
                None => return Err(format!("No union named {}", union_name)),
            }
        };

        self.consume(Token::Dot)?;

        let member_name = self.match_identifier()?.0;
        let (determinant, member) = match union_type
            .members
            .iter()
            .enumerate()
            .find(|(_, (name, _))| *name == member_name)
        {
            Some(x) => (x.0, &x.1 .1),
            None => {
                return Err(format!(
                    "{} not a member of union {}",
                    member_name, union_name
                ))
            }
        };

        self.consume(Token::LeftParen)?;

        let mut variable_names = vec![];
        for _ in 0..member.len() {
            variable_names.push(self.match_identifier()?.0);
            self.consume(Token::Comma)?;
        }

        self.consume(Token::RightParen)?;
        self.consume(Token::Equal)?;

        self.expression(chunk)?;

        if self.type_stack.last().unwrap().value_type != ValueType::Union(union_type.clone()) {
            return Err(format!("Union type does not match"));
        }

        Ok((determinant, variable_names, union_type))
    }

    fn if_expression_impl(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let mut is_if_let = false;
        let if_jmp_idx;
        if self.scanner.match_token(Token::Let)? {
            let (determinant, variable_names, union_type) = self.parse_if_let_expression(chunk)?;

            chunk.write_byte(opcode::JMP_IF_DETERMINANT_MISMATCH);
            chunk.write_byte(determinant.try_into().unwrap());
            if_jmp_idx = chunk.write_byte(0);
            chunk.write_byte(0);

            let frame = self.stack_frames.last_mut().unwrap();

            for var_name in &variable_names {
                let found = frame
                    .locals
                    .iter()
                    .find(|l| l.scope == frame.current_scope && l.name == *var_name);

                if found.is_some() {
                    return Err(format!(
                        "Local {} is already defined in current scope",
                        var_name
                    ));
                }
            }

            for (idx, var_name) in variable_names.iter().enumerate().rev() {
                frame.locals.push(LocalVariable {
                    name: var_name.clone(),
                    v: Variable {
                        read_only: false,
                        value_type: union_type.members[determinant].1[idx].clone(),
                    },
                    scope: frame.current_scope,
                });
                chunk.write_byte(opcode::DEFINE_LOCAL);
            }

            is_if_let = true;
            self.type_stack.pop();
            self.consume(Token::LeftBrace)?;
        } else {
            self.expression(chunk)?;
            self.consume(Token::LeftBrace)?;
            chunk.write_byte(opcode::JMP_IF_FALSE);
            self.type_stack.pop();
            if_jmp_idx = chunk.write_short(0);
        }

        let stack_top = self.type_stack.len();
        self.block_expression(chunk)?;

        if is_if_let {
            let frame = self.stack_frames.last_mut().unwrap();
            frame.locals.pop();
        }

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
            let jmp_skip_else_idx = chunk.write_short(0);

            chunk.write_short_at(
                if_jmp_idx,
                (chunk.code.len() - if_jmp_idx).try_into().unwrap(),
            );

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

            chunk.write_short_at(
                jmp_skip_else_idx,
                (chunk.code.len() - jmp_skip_else_idx).try_into().unwrap(),
            );
        } else {
            // If there is no else, this cannot always return a value.
            assert!(self.type_stack.len() >= stack_top);
            unsafe {
                self.type_stack.set_len(stack_top);
            }
            chunk.write_short_at(
                if_jmp_idx,
                (chunk.code.len() - if_jmp_idx).try_into().unwrap(),
            );
        }

        Ok(())
    }

    fn patch_break(&mut self, chunk: &mut Chunk, start_idx: usize, jmp_idx: usize) {
        self.unpatched_break_offsets.retain(|&idx| {
            if idx > start_idx {
                chunk.write_short_at(idx, (jmp_idx - idx).try_into().unwrap());
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
        self.logical_or(chunk)?;
        self.consume(Token::LeftBrace)?;
        chunk.write_byte(opcode::JMP_IF_FALSE);
        self.type_stack.pop();
        let cond_break_idx = chunk.write_short(0);

        self.scoped_block(chunk, |cm, ch| {
            while !cm.scanner.match_token(Token::RightBrace)? {
                cm.rule(ch)?;
            }

            cm.patch_break(ch, loop_begin_idx, ch.code.len());
            Ok(())
        })?;

        chunk.write_byte(opcode::LOOP);
        chunk.write_short((chunk.code.len() - loop_begin_idx + 1).try_into().unwrap());
        chunk.write_short_at(
            cond_break_idx,
            (chunk.code.len() - cond_break_idx).try_into().unwrap(),
        );

        self.is_in_loop = prev_in_loop;
        Ok(())
    }

    fn push_stack_frame(&mut self, chunk: &mut Chunk) {
        self.stack_frames.push(StackFrame {
            current_scope: 0,
            locals: Vec::new(),
        });
        chunk.code.push(opcode::PUSH_FRAME);
    }

    fn pop_stack_frame(&mut self, chunk: &mut Chunk) {
        self.stack_frames.pop();
        chunk.code.push(opcode::POP_FRAME);
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
                self.push_stack_frame(chunk);
                true
            }
        };

        let locals_top = self.stack_frames.last().unwrap().locals.len();
        block(self, chunk)?;

        assert!(!self.stack_frames.is_empty());
        if push_local {
            self.pop_stack_frame(chunk);
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
            let (identifier_name, _) = cm.match_identifier()?;

            cm.consume(Token::Colon)?;

            let start_loc = cm.scanner.peek_token()?.location;
            cm.expression(ch)?;
            if cm.type_stack.last().unwrap().value_type != ValueType::Integer {
                return Err(cm.make_error_msg("Expected integer value", &start_loc));
            }

            let var_idx;
            {
                let frame = cm.stack_frames.last_mut().unwrap();
                var_idx = frame.locals.len().try_into().unwrap();
                ch.write_byte(opcode::DEFINE_LOCAL);
                cm.type_stack.pop();

                frame.locals.push(LocalVariable {
                    name: identifier_name.to_string(),
                    v: Variable {
                        read_only: true,
                        value_type: ValueType::Integer,
                    },
                    scope: frame.current_scope,
                });
            }

            let is_inclusive = {
                let t = cm.scanner.scan_token()?;
                match t.token {
                    Token::DotDot => false,
                    Token::DotDotEqual => true,
                    _ => {
                        return Err(cm
                            .make_error_msg("Expected range delimiter '..' or '..='", &t.location))
                    }
                }
            };

            let end_idx;
            {
                let end_loc = cm.scanner.peek_token()?.location;
                cm.expression(ch)?;
                if cm.type_stack.last().unwrap().value_type != ValueType::Integer {
                    return Err(cm.make_error_msg("Expected integer value", &end_loc));
                }

                let frame = cm.stack_frames.last_mut().unwrap();
                end_idx = frame.locals.len().try_into().unwrap();
                frame.locals.push(LocalVariable {
                    name: "#".to_string(),
                    v: Variable {
                        read_only: true,
                        value_type: ValueType::Integer,
                    },
                    scope: frame.current_scope,
                });
            }

            ch.write_byte(opcode::DEFINE_LOCAL);
            cm.type_stack.pop();

            cm.consume(Token::LeftBrace)?;
            cm.scoped_block(ch, |cm, ch| {
                let loop_begin_idx = ch.write_byte(opcode::PUSH_LOCAL);
                ch.write_byte(var_idx);
                ch.write_byte(opcode::PUSH_LOCAL);
                ch.write_byte(end_idx);
                if is_inclusive {
                    ch.write_byte(opcode::LESS_EQUAL);
                } else {
                    ch.write_byte(opcode::LESS);
                }

                ch.write_byte(opcode::JMP_IF_FALSE);
                let cond_break_idx = ch.write_short(0);

                cm.for_block(ch)?;

                // This will need to be shifted to the top of the loop if breaks are to be enabled.
                ch.write_byte(opcode::PUSH_LOCAL);
                ch.write_byte(var_idx);
                cm.integer(ch, 1);
                ch.write_byte(opcode::ADD);
                ch.write_byte(opcode::SET_LOCAL);
                ch.write_byte(var_idx);

                ch.write_byte(opcode::LOOP);
                ch.write_short((ch.code.len() - loop_begin_idx).try_into().unwrap());
                ch.write_short_at(
                    cond_break_idx,
                    (ch.code.len() - cond_break_idx).try_into().unwrap(),
                );

                Ok(())
            })?;
            Ok(())
        })?;

        self.is_in_loop = prev_in_loop;
        self.type_stack.clear(); // FIX ME
        Ok(())
    }

    fn break_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        if !self.is_in_loop {
            return Err("break can only be used in a loop".to_owned());
        }

        self.consume(Token::SemiColon)?;

        chunk.write_byte(opcode::BREAK);
        let idx = chunk.write_short(0);
        self.unpatched_break_offsets.push(idx);

        Ok(())
    }

    fn continue_statement(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        if !self.is_in_loop {
            return Err("continue can only be used in a loop".to_owned());
        }

        self.consume(Token::SemiColon)?;

        chunk.write_byte(opcode::JMP);
        let idx = chunk.write_short(0);
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
                ValueType::Unit => todo!(),
                ValueType::Integer | ValueType::Float | ValueType::Char => chunk.write_byte(op),
                ValueType::Str => return Err("Cannot use comparison with string".to_owned()),
                ValueType::Bool => return Err("Cannot use comparison with bool".to_owned()),
                ValueType::Vector(_) => return Err("Cannot use comparison with vector".to_owned()),
                ValueType::HashMap(_) => {
                    return Err("Cannot use comparison with hash map".to_owned())
                }
                ValueType::Function(_) => {
                    return Err("Cannot use comparison with function".to_owned())
                }
                ValueType::Enum(_) => return Err("Cannot use comparison with enum".to_owned()), // FIXME
                ValueType::Struct(_) => return Err("Cannot use comparison with struct".to_owned()), // FIXME
                ValueType::Union(_) => return Err("Cannot use comparison with union".to_owned()), // FIXME
                ValueType::Tuple(_) => return Err("Cannot use comparison with tuple".to_owned()), // FIXME
                ValueType::Interface(_) => {
                    return Err("Cannot use comparison with interface".to_owned())
                } // FIXME
            };
        }

        self.type_stack.pop();
        let top = self.type_stack.last_mut().unwrap();
        top.read_only = true;
        top.value_type = ValueType::Bool;

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

    fn logical_or(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let location = self.scanner.peek_token()?.location;
        self.logical_and(chunk)?;

        while self.scanner.match_token(Token::LogicalOr)? {
            let expr_type = self.type_stack.pop().unwrap().value_type;
            if expr_type != ValueType::Bool {
                return Err(self.make_error_msg(
                        &format!("Expected boolean type for logical or, got {}", expr_type), &location));
            }

            let location = self.scanner.peek_token()?.location;
            self.logical_and(chunk)?;
            let expr_type = &self.type_stack.last().unwrap().value_type;
            if *expr_type != ValueType::Bool {
                return Err(self.make_error_msg(
                        &format!("Expected boolean type for logical or, got {}", expr_type), &location));
            }

            chunk.write_byte(opcode::LOGICAL_OR);
            self.type_stack.last_mut().unwrap().read_only = true;
        }

        Ok(())
    }

    fn logical_and(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let location = self.scanner.peek_token()?.location;
        self.equality(chunk)?;

        while self.scanner.match_token(Token::LogicalAnd)? {
            let expr_type = self.type_stack.pop().unwrap().value_type;
            if expr_type != ValueType::Bool {
                return Err(self.make_error_msg(
                        &format!("Expected boolean type for logical or, got {}", expr_type), &location));
            }

            let location = self.scanner.peek_token()?.location;
            self.equality(chunk)?;
            let expr_type = &self.type_stack.last().unwrap().value_type;
            if *expr_type != ValueType::Bool {
                return Err(self.make_error_msg(
                        &format!("Expected boolean type for logical or, got {}", expr_type), &location));
            }

            chunk.write_byte(opcode::LOGICAL_AND);
            self.type_stack.last_mut().unwrap().read_only = true;
        }

        Ok(())
    }

    fn equality_right(&mut self, chunk: &mut Chunk, op: u8) -> Result<(), String> {
        let location = self.scanner.peek_token()?.location;

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
                | ValueType::Char
                | ValueType::Vector(_)
                | ValueType::HashMap(_)
                | ValueType::Enum(_) => chunk.write_byte(op),
                _ => return Err(self.make_error_msg("Cannot compare functions", &location)),
            };
        }

        self.type_stack.pop();

        let last = self.type_stack.last_mut().unwrap();
        last.value_type = ValueType::Bool;
        last.read_only = true;
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
        self.unary(chunk)?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err("Type error".to_owned());
        } else {
            if op == opcode::MOD && *left_type != ValueType::Integer {
                return Err("Can only mod integers".to_owned());
            }

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
        self.unary(chunk)?;

        loop {
            if self.scanner.match_token(Token::Star)? {
                self.factor_right(chunk, opcode::MUL)?;
            } else if self.scanner.match_token(Token::Slash)? {
                self.factor_right(chunk, opcode::DIV)?;
            } else if self.scanner.match_token(Token::Percent)? {
                self.factor_right(chunk, opcode::MOD)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn unary(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let do_not = self.scanner.match_token(Token::Bang)?;
        let loc = self.scanner.peek_token()?.location;

        self.index(chunk)?;

        if do_not {
            match &self.type_stack.last().unwrap().value_type {
                ValueType::Bool => {},
                x => return Err(self.make_error_msg(&format!("Must be a boolean value, got {}", x), &loc))
            }

            chunk.write_byte(opcode::NOT);
        }
        Ok(())
    }

    fn index_vec(
        &mut self,
        chunk: &mut Chunk,
        read_only: bool,
        elem_type: &ValueType,
    ) -> Result<(), String> {
        let index_loc = self.scanner.peek_token()?.location;
        self.expression(chunk)?;
        let index_type = self.type_stack.pop().unwrap();
        if index_type.value_type != ValueType::Integer {
            return Err(self.make_error_msg(&format!(
                "Can only index using Integer. Got {}",
                index_type.value_type), &index_loc));
        }

        self.consume(Token::RightBracket)?;

        if self.scanner.match_token(Token::Equal)? {
            let expr_loc = self.scanner.peek_token()?.location;
            self.expression(chunk)?;
            let new_value_type = self.type_stack.pop().unwrap().value_type;
            if new_value_type != *elem_type {
                return Err(self.make_error_msg(&format!(
                    "Type mismatch for setting vector. Expected {}, got {}",
                    *elem_type, new_value_type), &expr_loc));
            }

            chunk.write_byte(opcode::SET_INDEX);
        } else {
            chunk.write_byte(opcode::GET_INDEX);
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

            chunk.write_byte(opcode::SET_INDEX);
        } else {
            chunk.write_byte(opcode::GET_INDEX);
        }

        self.type_stack.push(Variable {
            value_type: (*value_type).clone(),
            read_only,
        });

        Ok(())
    }

    fn index_str(&mut self, chunk: &mut Chunk, read_only: bool) -> Result<(), String> {
        let from_begin = self.scanner.match_token(Token::DotDot)?;

        self.expression(chunk)?;
        let index_type = self.type_stack.pop().unwrap();
        if index_type.value_type != ValueType::Integer {
            return Err(format!(
                "Can only index using Integer. Got {}",
                index_type.value_type
            ));
        }

        if from_begin {
            self.consume(Token::RightBracket)?;

            chunk.write_byte(opcode::CALL_BUILTIN);
            chunk.write_byte(builtin_functions::string::SUBSTR_FROM_START);
            chunk.write_byte(1);

            self.type_stack.push(Variable {
                value_type: ValueType::Str,
                read_only,
            });
        } else if self.scanner.match_token(Token::DotDot)? {
            if self.scanner.match_token(Token::RightBracket)? {
                chunk.write_byte(opcode::CALL_BUILTIN);
                chunk.write_byte(builtin_functions::string::SUBSTR_TO_END);
                chunk.write_byte(1);

                self.type_stack.push(Variable {
                    value_type: ValueType::Str,
                    read_only,
                });
            } else {
                self.expression(chunk)?;
                let end_index_type = self.type_stack.pop().unwrap();
                if end_index_type.value_type != ValueType::Integer {
                    return Err(format!(
                        "Can only index using Integer. Got {}",
                        index_type.value_type
                    ));
                }
                self.consume(Token::RightBracket)?;

                chunk.write_byte(opcode::CALL_BUILTIN);
                chunk.write_byte(builtin_functions::string::SUBSTR);
                chunk.write_byte(2);

                self.type_stack.push(Variable {
                    value_type: ValueType::Str,
                    read_only,
                });
            }
        } else {
            self.consume(Token::RightBracket)?;

            if self.scanner.match_token(Token::Equal)? {
                return Err("Cannot assign to string".to_owned());
            } else {
                chunk.write_byte(opcode::GET_INDEX);
            }

            self.type_stack.push(Variable {
                value_type: ValueType::Char,
                read_only,
            });
        }

        Ok(())
    }

    fn index(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        self.primary(chunk)?;

        loop {
            match self.scanner.peek_token()?.token {
                Token::LeftBracket => {
                    self.scanner.scan_token()?;
                    let indexable = self.type_stack.pop().unwrap();
                    match &indexable.value_type {
                        ValueType::Vector(e) => self.index_vec(chunk, indexable.read_only, e)?,
                        ValueType::HashMap(kv) => {
                            self.index_hash_map(chunk, indexable.read_only, &kv.0, &kv.1)?
                        }
                        ValueType::Str => self.index_str(chunk, indexable.read_only)?,
                        t => return Err(format!("Cannot index type {}", t)),
                    }
                },
                Token::Dot => {
                    self.scanner.scan_token()?;
                    self.field(chunk)?;
                }
                _ => break
            }
        }

        Ok(())
    }

    fn call(&mut self, chunk: &mut Chunk, name: &str) -> Result<(), String> {
        let stack_len = self.type_stack.len();

        let (function_type, func_opcode_1, func_opcode_2) =
            if let Some((local, idx)) = self.find_local(name) {
                (local.value_type, opcode::PUSH_LOCAL, idx as u16)
            } else {
                (
                    self.find_global(name)?.value_type,
                    opcode::PUSH_GLOBAL,
                    self.data_section.create_constant_str(name),
                )
            };

        let function_type = match function_type {
            ValueType::Function(f) => f,
            _ => return Err(format!("\"{}\" is not a function", name)),
        };

        let mut argument_types: Vec<ValueType> = Vec::new();
        self.parse_commas_separate_list(Token::RightParen, |cm, parameter_idx| {
            let location = cm.scanner.peek_token()?.location;

            cm.expression(chunk)?;
            let expr_type = cm.type_stack.last().unwrap().value_type.clone();

            if parameter_idx >= function_type.parameters.len() {
                return Err(cm.make_error_msg("Function call has too many arguments", &location));
            }

            let param_type = &function_type.parameters[parameter_idx];
            if !param_type.can_assign(&expr_type) {
                let interface_satisfied = match (param_type, &expr_type) {
                    (ValueType::Interface(i), ValueType::Struct(s)) => {
                        if i.does_struct_satisfy_interface(s) {
                            cm.create_interface_object_for_struct(i, s, chunk);
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                };

                if !interface_satisfied {
                    return Err(cm.make_error_msg(
                        &format!(
                            "Function call has incorrect argument type. Expected {}, got {}",
                            param_type, expr_type
                        ),
                        &location,
                    ));
                }
            }

            argument_types.push(expr_type);
            Ok(())
        })?;

        let argument_count = argument_types.len();
        let arity = function_type.parameters.len();
        if argument_count as usize != arity {
            return Err(format!(
                "Unexpected number of arguments to function call. Expected {}, got {}",
                arity, argument_count
            ));
        }

        // the arguments get moved to frame locals
        while self.type_stack.len() > stack_len {
            self.type_stack.pop();
        }

        self.type_stack.push(Variable {
            value_type: function_type.return_type.clone(),
            read_only: true,
        });

        chunk.write_byte(func_opcode_1);
        self.write_var_len_int(chunk, func_opcode_2.try_into().unwrap());

        chunk.write_byte(opcode::CALL);
        chunk.write_byte(argument_count.try_into().unwrap());

        Ok(())
    }

    fn term_right(&mut self, chunk: &mut Chunk, op: u8) -> Result<(), String> {
        let loc = self.scanner.peek_token()?.location;
        self.factor(chunk)?;

        let len = self.type_stack.len();
        let left_type = &self.type_stack[len - 2].value_type;
        let right_type = &self.type_stack[len - 1].value_type;

        if left_type != right_type {
            return Err(self.make_error_msg(&format!("Type mismatch. Left {}, Right {}", left_type, right_type), &loc));
        } else {
            match left_type {
                ValueType::Integer | ValueType::Float => chunk.write_byte(op),
                x => return Err(self.make_error_msg(&format!("Expected integer or float, got {}", x), &loc))
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

    fn write_var_len_int(&mut self, chunk: &mut Chunk, i: i64) {
        let mut encoder = var_len_int::Encoder::new(i);
        loop {
            let (byte, complete) = encoder.step_encode();
            chunk.write_byte(byte);

            if complete {
                break;
            }
        }
    }

    fn integer(&mut self, chunk: &mut Chunk, i: i64) {
        chunk.write_byte(opcode::CONSTANT_INTEGER);
        self.write_var_len_int(chunk, i);
        self.type_stack.push(Variable {
            value_type: ValueType::Integer,
            read_only: true,
        });
    }

    fn float(&mut self, chunk: &mut Chunk, f: f64) {
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
        let v = self.data_section.create_constant_str(s).try_into().unwrap();
        self.write_var_len_int(chunk, v);
        self.type_stack.push(Variable {
            value_type: ValueType::Str,
            read_only: true,
        });
    }

    fn character(&mut self, chunk: &mut Chunk, c: char) {
        chunk.write_byte(opcode::CONSTANT_CHAR);

        let mut buf: [u8; 4] = [0; 4];
        let bytes = c.encode_utf8(&mut buf).as_bytes();
        chunk.write_byte(bytes.len().try_into().unwrap());
        for b in bytes {
            chunk.write_byte(*b);
        }

        self.type_stack.push(Variable {
            value_type: ValueType::Char,
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

    fn enum_value(
        &mut self,
        chunk: &mut Chunk,
        enum_name: &str,
        enum_type: Rc<EnumType>,
    ) -> Result<(), String> {
        self.consume(Token::Dot)?;
        let (enum_value_name, enum_value_name_location) = self.match_identifier()?;

        let enum_value = match enum_type.members.get(&enum_value_name) {
            Some(v) => v,
            None => {
                return Err(self.make_error_msg(
                    &format!(
                        "Enum value {} does not exist in {}",
                        enum_value_name, enum_name
                    ),
                    &enum_value_name_location,
                ));
            }
        };

        match enum_value {
            EnumValue::Str(s) => self.string(chunk, s),
            EnumValue::Integer(i) => self.integer(chunk, *i),
            EnumValue::Float(f) => self.float(chunk, *f),
        }

        self.type_stack.last_mut().unwrap().value_type = ValueType::Enum(enum_type.clone());

        Ok(())
    }

    fn struct_construction(
        &mut self,
        chunk: &mut Chunk,
        struct_name: &str,
        struct_type: Rc<StructType>,
    ) -> Result<(), String> {
        self.consume(Token::LeftBrace)?;

        let mut members = HashSet::new();
        while !self.scanner.match_token(Token::RightBrace)? {
            let (member_name, member_name_location) = self.match_identifier()?;

            if !members.insert(member_name.clone()) {
                return Err(self.make_error_msg(
                    &format!(
                        "Struct {} member {} assigned more than once",
                        struct_name, member_name
                    ),
                    &member_name_location,
                ));
            }

            let _idx = match struct_type.get_member_idx(&member_name) {
                Some(i) => i,
                None => {
                    return Err(self.make_error_msg(
                        &format!("{} is not a member of struct {}", member_name, struct_name),
                        &member_name_location,
                    ));
                }
            };

            self.consume(Token::Equal)?;
            self.expression(chunk)?;
            self.consume(Token::Comma)?;
        }

        self.type_stack.push(Variable {
            value_type: ValueType::Struct(struct_type),
            read_only: false,
        });
        Ok(())
    }

    fn primary(&mut self, chunk: &mut Chunk) -> Result<(), String> {
        let t = self.scanner.scan_token()?;
        match t.token {
            Token::Identifier(name) => {
                if let Some(ut) = self.user_types.get(name) {
                    match ut {
                        UserType::Enum(e) => self.enum_value(chunk, name, e.clone())?,
                        UserType::Struct(s) => self.struct_construction(chunk, name, s.clone())?,
                        UserType::TemplatedUnion(_) => todo!(),
                        UserType::Union(_) => todo!(),
                        UserType::Tuple(_) => todo!(),
                        UserType::Interface(_) => todo!(),
                    }
                } else {
                    self.identifier(chunk, name)?;
                }
            }
            Token::SmallSelf => {
                if let Some((local, idx)) = self.find_local("self") {
                    chunk.write_byte(opcode::PUSH_LOCAL);
                    chunk.write_byte(idx.try_into().unwrap());
                    self.type_stack.push(local);
                } else {
                    panic!("self not found in locals!");
                }
            }
            Token::LeftParen => {
                self.expression(chunk)?;
                self.consume(Token::RightParen)?;
            }
            Token::Minus => {
                let next = self.scanner.scan_token()?;
                match next.token {
                    Token::Integer(i) => {
                        self.integer(chunk, -i);
                    }
                    Token::Float(f) => {
                        self.float(chunk, -f);
                    }
                    _ => {
                        return Err(self.make_error_msg("Expected number after '-'", &next.location))
                    }
                }
            }
            Token::Integer(i) => {
                self.integer(chunk, i);
            }
            Token::Float(f) => {
                self.float(chunk, f);
            }
            Token::Str(s) => {
                self.string(chunk, s);
            }
            Token::Char(c) => {
                self.character(chunk, c);
            }
            Token::True => {
                self.boolean(chunk, true);
            }
            Token::False => {
                self.boolean(chunk, false);
            }
            _ => {
                return Err(
                    self.make_error_msg(&format!("Unexpected token: {:?}", t.token), &t.location)
                );
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
        fn create_constant_str(&mut self, _s: &str) -> u16 {
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
    fn test_builtin_types() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        let src = "
        let i: int = 0;
        let f: float = 3.142;
        let s: string = \"hello\";
        let b: bool = false;
        ";

        assert!({
            compiler.compile(&mut table, src).unwrap();
            true
        });
    }

    #[test]
    fn test_error_negative_string() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler.compile(&mut table, "print -\"hello\";").is_err());
    }

    #[test]
    fn test_not() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        let src = "
        let cond: bool = false;
        let not: bool = !cond;
        ";

        assert!({
            compiler.compile(&mut table, src).unwrap();
            true
        });
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

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let v: [int] = vec<int>[5]{11};
            ";

            assert_eq!(
                compiler
                    .compile(&mut table, src)
                    .err(),
                None
            );
        }
    }

    #[test]
    fn test_vector_index() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        let src = "
            let v: [int] = vec<int>{1,2,3};
            print v[0];
        ";

        assert_eq!(compiler.compile(&mut table, src).err(), None);
    }

    #[test]
    fn test_string_index() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        let src = "
            let s: string = \"Hello\";
            print s[1];
        ";

        assert_eq!(compiler.compile(&mut table, src).err(), None);
    }

    #[test]
    fn test_string_index_range() {
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let s: string = \"Hello\";
                let r: string = s[1..3];
                print r;
            ";

            assert_eq!(compiler.compile(&mut table, src).err(), None);
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let s: string = \"Hello\";
                let r: string = s[..3];
                print r;
            ";

            assert_eq!(compiler.compile(&mut table, src).err(), None);
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            let src = "
                let s: string = \"Hello\";
                let r: string = s[1..];
                print r;
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

    #[test]
    fn empty_function() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler.compile(&mut table, "fn test() {}").unwrap();
            true
        });
    }

    #[test]
    fn function_with_single_parameter() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler.compile(&mut table, "fn test(a: int) {}").unwrap();
            true
        });
    }

    #[test]
    fn function_with_parameters() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler.compile(&mut table, "fn test(a: int, b: float, c: string, d: mut int, e: mut float, f: mut string) {}").unwrap();
            true
        });
    }

    #[test]
    fn function_call_with_parameters() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "fn test(a: int, b: float, c: string) {} test(1, 1.2, \"hello\");",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn function_wrong_argument_count() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler
            .compile(
                &mut table,
                "
                fn test() {}
                test(1);
                "
            )
            .is_err());
    }

    #[test]
    fn function_wrong_argument_type() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler
            .compile(
                &mut table,
                "
                fn test(a: string) {}
                test(1);
                "
            )
            .is_err());
    }

    #[test]
    fn function_recursion() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(&mut table, "fn test() { test(); }")
                .unwrap();
            true
        });
    }

    #[test]
    fn fib() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                fn fib(i: int) -> int {
                    if i <= 1 {
                        return i;
                    }
                    return fib(i - 1) + fib(i - 2);
                }",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_empty_enum() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                enum Enum : int {}",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_enum() {
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert!({
                compiler
                    .compile(
                        &mut table,
                        "
                    enum Enum : int {
                        a = 1,
                        b = 2,
                    }",
                    )
                    .unwrap();
                true
            });
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert!({
                compiler
                    .compile(
                        &mut table,
                        "
                    enum Enum : float {
                        a = 1.0,
                        b = 2.2,
                    }",
                    )
                    .unwrap();
                true
            });
        }
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert!({
                compiler
                    .compile(
                        &mut table,
                        "
                    enum Enum : string {
                        a = \"hello\",
                        b = \"world\",
                    }",
                    )
                    .unwrap();
                true
            });
        }
    }

    #[test]
    fn test_mismatch_type_enum() {
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert!(compiler
                .compile(
                    &mut table,
                    "
                    enum Enum : int {
                        a = \"hello\",
                    }"
                )
                .is_err());
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert!(compiler
                .compile(
                    &mut table,
                    "
                    enum Enum : float {
                        a = \"hello\",
                    }"
                )
                .is_err());
        }
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();
            assert!(compiler
                .compile(
                    &mut table,
                    "
                    enum Enum : string {
                        a = 1.23,
                    }"
                )
                .is_err());
        }
    }

    #[test]
    fn test_use_enum() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                enum Enum : int {
                    a = 1,
                    b = 2,
                }

                let mut e: Enum = Enum.a;
                e = Enum.b;
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_enum_type_mismatch() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!(compiler
            .compile(
                &mut table,
                "
                enum Enum : int {
                    a = 1,
                    b = 2,
                }

                let e: Enum = 1;
                "
            )
            .is_err());
    }

    #[test]
    fn test_coerce_to_base_type() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                enum Enum : int {
                    a = 1,
                    b = 2,
                }

                let e: int = Enum.a;

                fn test(i: int) {}
                test(Enum.a);
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_logical() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                    let and: bool = true && false && true && false;
                    let or: bool = true || false || true || false;
                    let mix: bool = true || false && true || false && false;
                ",
                )
                .unwrap();
            true
        });
    }
    #[test]
    fn test_empty_struct() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler.compile(&mut table, "struct Struct {}").unwrap();
            true
        });
    }

    #[test]
    fn test_struct_with_fields() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "struct Struct
                {
                    i: int,
                    f: float,
                    s: string,
                }
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_struct_with_methods() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                struct Struct {}
                impl {
                    fn (self) test() {
                        print \"Test method\";
                    }
                }

                let s: Struct = Struct {};
                s.test();
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_struct_construct() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "struct Struct
                {
                    i: int,
                    f: float,
                    s: string,
                }

                let s: Struct = Struct {
                    i = 0,
                    f = 2.3,
                    s = \"hello world\",
                };

                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_struct_read_only_field() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "struct Struct
                {
                    i: int,
                    f: float,
                    s: string,
                }

                let s: Struct = Struct {
                    i = 0,
                    f = 2.3,
                    s = \"hello world\",
                };

                ",
                )
                .unwrap();
            true
        });

        assert!(compiler
            .compile(
                &mut table,
                "
                    s.i = 10;
                "
            )
            .is_err());
    }

    #[test]
    fn test_struct_mutable_field() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "struct Struct
                {
                    i: int,
                    f: float,
                    s: string,
                }

                let mut s: Struct = Struct {
                    i = 0,
                    f = 2.3,
                    s = \"hello world\",
                };

                ",
                )
                .unwrap();
            true
        });

        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                    s.i = 10;
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_struct_set_field_wrong_type() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "struct Struct
                {
                    i: int,
                }

                let mut s: Struct = Struct {
                    i = 0,
                };
                ",
                )
                .unwrap();
            true
        });

        assert!(compiler
            .compile(
                &mut table,
                "
                    s.i = 1.5;
                "
            )
            .is_err());
    }

    #[test]
    fn test_define_union() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Union_empty {}
                ",
                )
                .unwrap();
            true
        });

        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Union_0
                {
                    A, B, C,
                }
                ",
                )
                .unwrap();
            true
        });

        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Union_1
                {
                    I(int),
                    F(float),
                    S(string),
                }
                ",
                )
                .unwrap();
            true
        });

        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Union_2<T>
                {
                    A(T),
                }
                ",
                )
                .unwrap();
            true
        });

        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Union_3<T, U, V>
                {
                    A(T),
                    B(U),
                    C(V),
                }
                ",
                )
                .unwrap();
            true
        });

        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Union_4<T, U, V>
                {
                    A(T), A1(T), A2(T),
                    B(U),
                    C(V), C1(V),

                    I(int), I1(int),
                    F(float), F1(float),
                    S(string), S1(string),
                }
                ",
                )
                .unwrap();
            true
        });

        assert!(compiler
            .compile(&mut table, "union Union_err<T, T> {} ")
            .is_err());
    }

    #[test]
    fn test_union_constructor() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Union
                {
                    I(int), F(float), S(string),
                }

                let mut u: Union = Union.I(9,);
                u = Union.S(\"Hello world\",);
                u = Union.F(3.142,);
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_templated_union_constructor() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Union<A, B, C>
                {
                    I(A), F(B), S(C),
                }

                let mut u: Union<int, float, string,> = Union<int, float, string,>.I(9,);
                u = Union<int, float, string,>.S(\"Hello world\",);
                u = Union<int, float, string,>.F(3.142,);
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_union_option() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Option<T>
                {
                    Some(T),
                    None,
                }
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_if_let() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Option
                {
                    Some(int),
                    None,
                }

                let o: Option = Option.Some(10,);
                if let Option.Some(x,) = o {
                }
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_if_let_multiple() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "union Composite
                {
                    Two(int, float),
                    Three(int, float, string),
                }

                let mut c: Composite = Composite.Two(10,1.23,);
                if let Composite.Two(x, y,) = c {
                }

                c = Composite.Three(300, 3.142, \"Hello World\",);
                if let Composite.Three(x, y, z,) = c {
                }
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_if_let_templated() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                union Composite<A, B, C>
                {
                    Two(A, B),
                    Three(A, C, B),
                }

                let mut c: Composite<int, float, string,> = Composite<int, float, string,>.Two(10,1.23,);
                if let Composite<int, float, string,>.Two(x, y,) = c {
                }

                c = Composite<int, float, string,>.Three(300, \"Hello World\", 3.142,);
                if let Composite<int, float, string,>.Three(x, y, z,) = c {
                }
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_type_alias() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();
        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                union Option<T> {
                    Some(T),
                    None,
                }

                type_alias MyOption = Option<int,>;

                let o: Option<int,> = MyOption.Some(10,);

                struct Struct {
                    s: string,
                }

                type_alias MyStruct = Struct;

                let s: Struct = MyStruct { s = \"hello world\", };
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_tuple() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                type_alias MyTuple = (int, float, string,);

                let t: MyTuple = tuple (10, 3.142, \"Hello world\",);
                let i: int = t.0;
                let f: float = t.1;
                let s: string = t.2;
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_function_pointer() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                fn function(i:int) {
                    print(\"function\");
                    print(i);
                }

                fn call(f: fn(int), i: int) {
                    f(i);
                }

                let mut f: fn(int) = function;
                f(10);
                call(f, 20);
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_interface() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                interface Interface {
                    fn call(i: int) -> int
                    fn call2(f: float) -> float
                }

                struct S {}
                impl {
                    fn (self) call(i: int) -> int { return i; }
                    fn (self) call2(f: float) -> float { return f; }
                }

                let it: Interface = S {};
                it.call(10);
                it.call2(3.142);
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_vector_builtin() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                let mut v: [int] = vec<int>{};
                let len: int = v.len();
                v.push(10);
                v.pop();
                v.clear();
                let v2: [int] = v.clone();
                v.remove(0);
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_vector_sort() {
        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            assert!({
                compiler
                    .compile(
                        &mut table,
                        "
                    let mut i: [int] = vec<int>{};
                    i.sort();

                    let mut f: [float] = vec<float>{};
                    f.sort();

                    let mut s: [string] = vec<string>{};
                    s.sort();

                    let mut b: [bool] = vec<bool>{};
                    b.sort();
                    ",
                    )
                    .unwrap();
                true
            });
        }

        {
            let mut table = TestDataSection::new();
            let mut compiler = Compiler::new();

            assert!({
                compiler
                    .compile(
                        &mut table,
                        "
                    struct S {
                        i: int,
                        s: string,
                    }

                    fn pred(a: S, b: S) -> bool {
                        return a.i < b.i;
                    }

                    let mut v: [S] = vec<S>{};
                    v.sort(pred);
                    ",
                    )
                    .unwrap();
                true
            });
        }
    }

    #[test]
    fn test_char_builtin() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                let mut c: char = 'a';
                let upper: char = c.to_uppercase();
                let lower: char = c.to_lowercase();
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_string_builtin() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                let s: string = \"hello\";
                let len: int = s.len();
                print(len);

                print(\"-10\".to_integer());

                let split: [string] = s.split('e');
                let trim: string = s.trim();
                let trim_start: string = s.trim_start();
                let trim_end: string = s.trim_end();
                let starts_with: bool = s.starts_with(\"hello\");
                let ends_with: bool = s.starts_with(\"world\");
                let contains: bool = s.contains(\"contains\");
                ",
                )
                .unwrap();
            true
        });
    }

    #[test]
    fn test_hashmap_builtin() {
        let mut table = TestDataSection::new();
        let mut compiler = Compiler::new();

        assert!({
            compiler
                .compile(
                    &mut table,
                    "
                let h: [int: int] = hash_map<int, int>{};

                print(h.contains_key(0));

                let k: [int] = h.keys();
                ",
                )
                .unwrap();
            true
        });
    }
}
