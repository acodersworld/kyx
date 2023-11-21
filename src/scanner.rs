use std::fmt;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Struct,
    Enum,
    Union,
    Interface,
    Impl,
    SmallSelf,
    If,
    Else,
    While,
    For,
    Return,
    Print,
    Let,
    Mut,
    Fn,
    True,
    False,

    TypeInt,
    TypeFloat,
    TypeString,
    TypeChar,
    TypeBool,
    TypeAlias,

    Vector,
    HashMap,
    Tuple,

    Identifier(&'a str),
    Integer(i64),
    Float(f64),
    Str(&'a str),
    Char(char),

    Plus,
    Minus,
    Slash,
    Star,
    Percent,

    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,

    Dot,
    SemiColon,
    Colon,
    Comma,
    DotDot,
    DotDotEqual,

    ThinArrow,

    ReadInput,

    Break,
    Continue,

    Eof,
}

impl Token<'_> {
    fn as_string(&self) -> String {
        let s = match self {
            Self::Struct => "struct",
            Self::Enum => "enum",
            Self::Union => "union",
            Self::Interface => "interface",
            Self::Impl => "impl",
            Self::SmallSelf => "self",
            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::For => "for",
            Self::Return => "return",
            Self::Print => "print",
            Self::Let => "let",
            Self::Mut => "mut",
            Self::Fn => "fn",
            Self::True => "true",
            Self::False => "false",

            Self::TypeInt => "int",
            Self::TypeFloat => "float",
            Self::TypeString => "string",
            Self::TypeChar => "char",
            Self::TypeBool => "bool",
            Self::TypeAlias => "type_alias",

            Self::Vector => "vec",
            Self::HashMap => "hash_map",
            Self::Tuple => "tuple",

            Self::Identifier(id) => id,
            Self::Integer(i) => return i.to_string(),
            Self::Float(f) => return f.to_string(),
            Self::Str(s) => s,
            Self::Char(c) => return c.to_string(),

            Self::Plus => "+",
            Self::Minus => "-",
            Self::Slash => "/",
            Self::Star => "*",
            Self::Percent => "%",

            Self::LeftBrace => "{",
            Self::RightBrace => "}",
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::LeftBracket => "[",
            Self::RightBracket => "]",

            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",

            Self::Bang => "!",
            Self::BangEqual => "!=",
            Self::Equal => "=",
            Self::EqualEqual => "==",

            Self::Dot => ".",
            Self::SemiColon => ";",
            Self::Colon => ":",
            Self::Comma => ",",
            Self::DotDot => "..",
            Self::DotDotEqual => "..=",
            Self::ThinArrow => "->",

            Self::ReadInput => "read",

            Self::Break => "break",
            Self::Continue => "continue",

            Self::Eof => "<EOF>",
        };

        s.to_string()
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[derive(Clone, Copy)]
pub struct TokenLocation {
    src_line_start: usize,
    src_line_len: usize,
    pub column: usize,
    pub line: usize,
}

impl<'a> TokenLocation {
    pub fn get_text_line(&self, src: &'a str) -> &'a str {
        &src[self.src_line_start..self.src_line_start + self.src_line_len]
    }
}

#[derive(Clone, Copy)]
pub struct TokenWithLocation<'a> {
    pub token: Token<'a>,
    pub location: TokenLocation,
}

#[derive(Clone)]
pub struct Scanner<'a> {
    src: &'a str,
    src_head: &'a str,
    src_line: &'a str,
    peeked_token: Option<TokenWithLocation<'a>>,
    current_idx: usize,
    current_token_column: usize,
    current_column: usize,
    current_line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Scanner {
        Scanner {
            src,
            src_head: src,
            src_line: src,
            peeked_token: None,
            current_idx: 0,
            current_token_column: 0,
            current_column: 0,
            current_line: 1,
        }
    }

    fn peek(&self) -> char {
        self.src_head[self.current_idx..].chars().next().unwrap()
    }

    fn peek_next(&self) -> Option<char> {
        let mut iter = self.src_head[self.current_idx..].chars();
        iter.next();

        if let Some(c) = iter.next() {
            return Some(c);
        }

        None
    }

    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current_idx += c.len_utf8();
        self.current_column += 1;
        c
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.peek() == c {
            self.advance();
            return true;
        }

        false
    }

    fn at_eof(&self) -> bool {
        self.current_idx >= self.src_head.len()
    }

    fn skip_to_newline(&mut self) {
        while !self.at_eof() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn skip_white_space(&mut self) {
        if self.at_eof() {
            return;
        }

        while !self.at_eof() {
            match self.peek() {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' => {
                    self.advance();
                    self.src_line = &self.src_head[self.current_idx..];
                    self.current_line += 1;
                    self.current_column = 0;
                }
                _ => return,
            }
        }
    }

    fn identifier(&mut self) -> TokenWithLocation<'a> {
        while !self.at_eof() && (self.peek().is_alphanumeric() || self.peek() == '_') {
            self.advance();
        }

        let ident = &self.src_head[..self.current_idx];
        let t = match ident {
            "int" => Token::TypeInt,
            "float" => Token::TypeFloat,
            "string" => Token::TypeString,
            "char" => Token::TypeChar,
            "bool" => Token::TypeBool,
            "type_alias" => Token::TypeAlias,
            "vec" => Token::Vector,
            "hash_map" => Token::HashMap,
            "tuple" => Token::Tuple,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "union" => Token::Union,
            "interface" => Token::Interface,
            "impl" => Token::Impl,
            "self" => Token::SmallSelf,
            "fn" => Token::Fn,
            "let" => Token::Let,
            "mut" => Token::Mut,
            "print" => Token::Print,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "return" => Token::Return,
            "read" => Token::ReadInput,
            "break" => Token::Break,
            "continue" => Token::Continue,
            _ => Token::Identifier(ident),
        };

        TokenWithLocation {
            token: t,
            location: self.get_location(),
        }
    }

    fn string(&mut self) -> Result<TokenWithLocation<'a>, String> {
        while !self.at_eof() && self.peek() != '"' {
            self.advance();
        }

        if self.at_eof() {
            return Err("Error".to_string());
        }

        self.advance();
        Ok(TokenWithLocation {
            token: Token::Str(&self.src_head[1..self.current_idx - 1]),
            location: self.get_location(),
        })
    }

    fn character(&mut self) -> Result<TokenWithLocation<'a>, String> {
        if self.at_eof() {
            return Err("Expected character but found EOF".to_string());
        }

        let c = self.advance();
        let tok = TokenWithLocation {
            token: Token::Char(c),
            location: self.get_location(),
        };

        if self.at_eof() {
            return Err("Expected ' but found EOF".to_string());
        }

        let c = self.advance();
        if c != '\'' {
            return Err(format!("Expected ' but found {}", c));
        }

        Ok(tok)
    }

    fn number(&mut self) -> Result<TokenWithLocation<'a>, String> {
        while !self.at_eof() && self.peek().is_numeric() {
            self.advance();
        }

        if !self.at_eof() && self.peek() == '.' && self.peek_next() != Some('.') {
            self.advance();
            if self.at_eof() || !self.peek().is_ascii_digit() {
                return Err("Expected digit after '.'".to_owned());
            }

            while !self.at_eof() && self.peek().is_numeric() {
                self.advance();
            }

            let number_str = &self.src_head[..self.current_idx];
            return Ok(TokenWithLocation {
                token: Token::Float(str::parse::<f64>(number_str).unwrap()),
                location: self.get_location(),
            });
        }

        let number_str = &self.src_head[..self.current_idx];
        return Ok(TokenWithLocation {
            token: Token::Integer(str::parse::<i64>(number_str).unwrap()),
            location: self.get_location(),
        });
    }

    fn get_location(&self) -> TokenLocation {
        let src_line_start = self.src_line.as_ptr() as usize - self.src.as_ptr() as usize;
        let src_line_len = {
            let pos = self.src_line.find('\n');
            if let Some(idx) = pos {
                idx
            } else {
                self.src_line.len()
            }
        };

        TokenLocation {
            src_line_start,
            src_line_len,
            column: self.current_token_column,
            line: self.current_line,
        }
    }

    pub fn peek_token(&mut self) -> Result<TokenWithLocation<'a>, String> {
        if let Some(token) = &self.peeked_token {
            return Ok(*token);
        }

        let token = self.scan_token()?;
        self.peeked_token = Some(token);
        Ok(token)
    }

    pub fn match_token(&mut self, token: Token) -> Result<bool, String> {
        let t = self.peek_token()?;
        if t.token == token {
            self.peeked_token = None;
            return Ok(true);
        }

        Ok(false)
    }

    pub fn scan_token(&mut self) -> Result<TokenWithLocation<'a>, String> {
        if let Some(token) = self.peeked_token.take() {
            return Ok(token);
        }

        loop {
            self.skip_white_space();
            if self.at_eof() {
                return Ok(TokenWithLocation {
                    token: Token::Eof,
                    location: self.get_location(),
                });
            }

            if self.peek() == '/' {
                self.advance();
                if !self.at_eof() && self.peek() == '/' {
                    // hit a line comment
                    self.advance();
                    self.skip_to_newline();
                } else {
                    return Ok(TokenWithLocation {
                        token: Token::Slash,
                        location: TokenLocation {
                            src_line_start: self.src_line.as_ptr() as usize
                                - self.src.as_ptr() as usize,
                            src_line_len: self.src_line.len(),
                            column: self.current_token_column,
                            line: self.current_line,
                        },
                    });
                }
            } else {
                break;
            }
        }

        self.src_head = &self.src_head[self.current_idx..];
        self.current_token_column = self.current_column;
        self.current_idx = 0;

        let c = self.advance();

        if c.is_alphabetic() {
            return Ok(self.identifier());
        } else if c.is_ascii_digit() {
            return self.number();
        }

        let t = match c {
            '+' => Token::Plus,
            '-' => {
                if self.match_char('>') {
                    Token::ThinArrow
                } else {
                    Token::Minus
                }
            }
            '*' => Token::Star,
            '%' => Token::Percent,

            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,

            '.' => {
                if self.match_char('.') {
                    if self.match_char('=') {
                        Token::DotDotEqual
                    } else {
                        Token::DotDot
                    }
                } else {
                    Token::Dot
                }
            }
            ';' => Token::SemiColon,
            ':' => Token::Colon,
            ',' => Token::Comma,

            '!' => {
                if self.match_char('=') {
                    Token::BangEqual
                } else {
                    Token::Bang
                }
            }
            '<' => {
                if self.match_char('=') {
                    Token::LessEqual
                } else {
                    Token::Less
                }
            }
            '>' => {
                if self.match_char('=') {
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            }
            '=' => {
                if self.match_char('=') {
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            }
            '"' => return self.string(),
            '\'' => return self.character(),
            _ => return Err(format!("Unexpected token '{}'", c)),
        };

        Ok(TokenWithLocation {
            token: t,
            location: self.get_location(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comment() {
        {
            let src = "//a comment\n//another comment".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }

        {
            let src = "//".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }
    }

    #[test]
    fn test_line_column() {
        let src = "//a comment\n".to_owned() + "+\n" + "abc def\n" + "struct";
        let mut scanner = Scanner::new(&src);
        let mut location = scanner.scan_token().unwrap().location;
        assert_eq!(location.line, 2);
        assert_eq!(location.column, 0);
        assert_eq!(location.get_text_line(&src), "+");

        location = scanner.scan_token().unwrap().location;
        assert_eq!(location.line, 3);
        assert_eq!(location.column, 0);
        assert_eq!(location.get_text_line(&src), "abc def");

        location = scanner.scan_token().unwrap().location;
        assert_eq!(location.line, 3);
        assert_eq!(location.column, 4);
        assert_eq!(location.get_text_line(&src), "abc def");

        location = scanner.scan_token().unwrap().location;
        assert_eq!(location.line, 4);
        assert_eq!(location.column, 0);
        assert_eq!(location.get_text_line(&src), "struct");

        assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_arithmetic() {
        {
            let src = "+-/*%".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Plus);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Minus);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Slash);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Star);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Percent);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }

        {
            let src = "/".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Slash);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }
    }

    #[test]
    fn test_keywords() {
        let src =
            "int float string type_alias vec hash_map tuple struct enum union interface impl self while for return fn let mut true false";
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().unwrap().token, Token::TypeInt);
        assert_eq!(scanner.scan_token().unwrap().token, Token::TypeFloat);
        assert_eq!(scanner.scan_token().unwrap().token, Token::TypeString);
        assert_eq!(scanner.scan_token().unwrap().token, Token::TypeAlias);

        assert_eq!(scanner.scan_token().unwrap().token, Token::Vector);
        assert_eq!(scanner.scan_token().unwrap().token, Token::HashMap);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Tuple);

        assert_eq!(scanner.scan_token().unwrap().token, Token::Struct);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Enum);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Union);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Interface);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Impl);
        assert_eq!(scanner.scan_token().unwrap().token, Token::SmallSelf);
        assert_eq!(scanner.scan_token().unwrap().token, Token::While);
        assert_eq!(scanner.scan_token().unwrap().token, Token::For);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Return);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Fn);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Let);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Mut);
        assert_eq!(scanner.scan_token().unwrap().token, Token::True);
        assert_eq!(scanner.scan_token().unwrap().token, Token::False);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_identifier() {
        let src = "hello".to_string();
        let mut scanner = Scanner::new(&src);

        assert_eq!(
            scanner.scan_token().unwrap().token,
            Token::Identifier(&"hello")
        );
        assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_delimiters() {
        let src = "()[]{}".to_string();
        let mut scanner = Scanner::new(&src);

        assert_eq!(scanner.scan_token().unwrap().token, Token::LeftParen);
        assert_eq!(scanner.scan_token().unwrap().token, Token::RightParen);

        assert_eq!(scanner.scan_token().unwrap().token, Token::LeftBracket);
        assert_eq!(scanner.scan_token().unwrap().token, Token::RightBracket);

        assert_eq!(scanner.scan_token().unwrap().token, Token::LeftBrace);
        assert_eq!(scanner.scan_token().unwrap().token, Token::RightBrace);

        assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_integer() {
        {
            let src = "123".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Integer(123));

            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }

        {
            let src = "-123".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Minus);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Integer(123));

            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }
    }

    #[test]
    fn test_float() {
        {
            let src = "3.142".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Float(3.142));

            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }

        {
            let src = "3.".to_owned();
            let mut scanner = Scanner::new(&src);
            assert!(scanner.scan_token().is_err());
        }
    }

    #[test]
    fn test_numbers() {
        let src = "467 1.45 20.1 50".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Integer(467));
        assert_eq!(scanner.scan_token().unwrap().token, Token::Float(1.45));
        assert_eq!(scanner.scan_token().unwrap().token, Token::Float(20.1));
        assert_eq!(scanner.scan_token().unwrap().token, Token::Integer(50));

        assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_comparison() {
        let src = "<>>====!!=<=".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Less);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Greater);
        assert_eq!(scanner.scan_token().unwrap().token, Token::GreaterEqual);
        assert_eq!(scanner.scan_token().unwrap().token, Token::EqualEqual);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Equal);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Bang);
        assert_eq!(scanner.scan_token().unwrap().token, Token::BangEqual);
        assert_eq!(scanner.scan_token().unwrap().token, Token::LessEqual);

        assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_punctuation() {
        let src = "..=...;:,".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().unwrap().token, Token::DotDotEqual);
        assert_eq!(scanner.scan_token().unwrap().token, Token::DotDot);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Dot);
        assert_eq!(scanner.scan_token().unwrap().token, Token::SemiColon);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Colon);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Comma);

        assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_arrow() {
        let src = "->".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token().unwrap().token, Token::ThinArrow);
    }

    #[test]
    fn test_string() {
        {
            let src = "\"\"".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Str(""));
            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }

        {
            let src = "\"this is a string\"".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(
                scanner.scan_token().unwrap().token,
                Token::Str("this is a string")
            );
            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }

        {
            let src = "\"".to_owned();
            let mut scanner = Scanner::new(&src);
            assert!(scanner.scan_token().is_err());
        }
    }

    #[test]
    fn test_character() {
        {
            let src = "'a'".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token().unwrap().token, Token::Char('a'));
            assert_eq!(scanner.scan_token().unwrap().token, Token::Eof);
        }

        {
            let src = "''".to_owned();
            let mut scanner = Scanner::new(&src);
            assert!(scanner.scan_token().is_err());
        }
    }

    #[test]
    fn test_peek() {
        let src = "123+456";
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.peek_token().unwrap().token, Token::Integer(123));
        assert_eq!(scanner.peek_token().unwrap().token, Token::Integer(123));
        assert_eq!(scanner.scan_token().unwrap().token, Token::Integer(123));

        assert_eq!(scanner.peek_token().unwrap().token, Token::Plus);
        assert_eq!(scanner.scan_token().unwrap().token, Token::Plus);

        assert_eq!(scanner.peek_token().unwrap().token, Token::Integer(456));
        assert_eq!(scanner.scan_token().unwrap().token, Token::Integer(456));
    }

    #[test]
    fn test_match() {
        let src = "123+456";
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.match_token(Token::Print), Ok(false));
        assert_eq!(scanner.match_token(Token::Integer(123)), Ok(true));
        assert_eq!(scanner.match_token(Token::Minus), Ok(false));
        assert_eq!(scanner.match_token(Token::Plus), Ok(true));
        assert_eq!(scanner.match_token(Token::Integer(456)), Ok(true));
    }
}
