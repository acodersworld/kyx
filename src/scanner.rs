use std::fmt;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Struct,
    Enum,
    Union,
    Interface,
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

    Vector,
    HashMap,

    Identifier(&'a str),
    Integer(i32),
    Float(f32),
    Str(&'a str),

    Plus,
    Minus,
    Slash,
    Star,

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

    MinusGreater,

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
            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::For => "for",
            Self::Return => "return",
            Self::Print => "print",
            Self::Let => "",
            Self::Mut => "",
            Self::Fn => "",
            Self::True => "",
            Self::False => "",

            Self::TypeInt => "int",
            Self::TypeFloat => "float",
            Self::TypeString => "string",

            Self::Vector => "vec",
            Self::HashMap => "hash_map",

            Self::Identifier(id) => id,
            Self::Integer(i) => return i.to_string(),
            Self::Float(f) => return f.to_string(),
            Self::Str(s) => s,

            Self::Plus => "+",
            Self::Minus => "-",
            Self::Slash => "/",
            Self::Star => "*",

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
            Self::MinusGreater => "->",

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

pub struct Scanner<'a> {
    src: &'a str,
    peeked_token: Option<Token<'a>>,
    current_idx: usize,
    current_line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Scanner {
        Scanner {
            src,
            peeked_token: None,
            current_idx: 0,
            current_line: 1,
        }
    }

    fn peek(&self) -> char {
        self.src[self.current_idx..].chars().next().unwrap()
    }

    fn peek_next(&self) -> Option<char> {
        let mut iter = self.src[self.current_idx..].chars();
        iter.next();

        if let Some(c) = iter.next() {
            return Some(c);
        }

        None
    }

    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current_idx += c.len_utf8();
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
        self.current_idx >= self.src.len()
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
                    self.current_line += 1;
                    self.advance();
                }
                _ => return,
            }
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while !self.at_eof() && (self.peek().is_alphanumeric() || self.peek() == '_') {
            self.advance();
        }

        let ident = &self.src[..self.current_idx];
        match ident {
            "int" => Token::TypeInt,
            "float" => Token::TypeFloat,
            "string" => Token::TypeString,
            "vec" => Token::Vector,
            "hash_map" => Token::HashMap,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "union" => Token::Union,
            "interface" => Token::Interface,
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
        }
    }

    fn string(&mut self) -> Result<Token<'a>, String> {
        while !self.at_eof() && self.peek() != '"' {
            self.advance();
        }

        if self.at_eof() {
            return Err("Error".to_string());
        }

        self.advance();
        Ok(Token::Str(&self.src[1..self.current_idx - 1]))
    }

    fn number(&mut self) -> Result<Token<'a>, String> {
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

            let number_str = &self.src[..self.current_idx];
            return Ok(Token::Float(str::parse::<f32>(number_str).unwrap()));
        }

        let number_str = &self.src[..self.current_idx];
        return Ok(Token::Integer(str::parse::<i32>(number_str).unwrap()));
    }

    pub fn peek_token(&mut self) -> Result<Token<'a>, String> {
        if let Some(token) = self.peeked_token {
            return Ok(token);
        }

        let token = self.scan_token()?;
        self.peeked_token = Some(token);
        Ok(token)
    }

    pub fn match_token(&mut self, token: Token) -> Result<bool, String> {
        let t = self.peek_token()?;
        if t == token {
            self.peeked_token = None;
            return Ok(true);
        }

        Ok(false)
    }

    pub fn scan_token(&mut self) -> Result<Token<'a>, String> {
        if let Some(token) = self.peeked_token.take() {
            return Ok(token);
        }

        loop {
            self.skip_white_space();
            if self.at_eof() {
                return Ok(Token::Eof);
            }

            if self.peek() == '/' {
                self.advance();
                if !self.at_eof() && self.peek() == '/' {
                    // hit a line comment
                    self.advance();
                    self.skip_to_newline();
                } else {
                    return Ok(Token::Slash);
                }
            } else {
                break;
            }
        }

        self.src = &self.src[self.current_idx..];
        self.current_idx = 0;

        let c = self.advance();

        if c.is_alphabetic() {
            return Ok(self.identifier());
        } else if c.is_ascii_digit() {
            return self.number();
        }

        match c {
            '+' => Ok(Token::Plus),
            '-' => {
                if self.match_char('>') {
                    return Ok(Token::MinusGreater)
                }
                Ok(Token::Minus)
            },
            '*' => Ok(Token::Star),

            '{' => Ok(Token::LeftBrace),
            '}' => Ok(Token::RightBrace),
            '(' => Ok(Token::LeftParen),
            ')' => Ok(Token::RightParen),
            '[' => Ok(Token::LeftBracket),
            ']' => Ok(Token::RightBracket),

            '.' => {
                if self.match_char('.') {
                    if self.match_char('=') {
                        return Ok(Token::DotDotEqual);
                    } else {
                        return Ok(Token::DotDot);
                    }
                }

                Ok(Token::Dot)
            }
            ';' => Ok(Token::SemiColon),
            ':' => Ok(Token::Colon),
            ',' => Ok(Token::Comma),

            '!' => {
                if self.match_char('=') {
                    return Ok(Token::BangEqual);
                }
                Ok(Token::Bang)
            }
            '<' => {
                if self.match_char('=') {
                    return Ok(Token::LessEqual);
                }
                Ok(Token::Less)
            }
            '>' => {
                if self.match_char('=') {
                    return Ok(Token::GreaterEqual);
                }
                Ok(Token::Greater)
            }
            '=' => {
                if self.match_char('=') {
                    return Ok(Token::EqualEqual);
                }
                Ok(Token::Equal)
            }
            '"' => self.string(),
            _ => Err(format!("Unexpected token '{}'", c)),
        }
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
            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }

        {
            let src = "//".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }
    }

    #[test]
    fn test_line() {
        let src = r"//a comment
                    +
                    abc def
                    struct
                    "
        .to_owned();
        let mut scanner = Scanner::new(&src);
        scanner.scan_token().unwrap();
        assert_eq!(scanner.current_line, 2);

        scanner.scan_token().unwrap();
        assert_eq!(scanner.current_line, 3);
        scanner.scan_token().unwrap();
        assert_eq!(scanner.current_line, 3);

        scanner.scan_token().unwrap();
        assert_eq!(scanner.current_line, 4);

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_arithmetic() {
        {
            let src = "+-/*".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Plus));
            assert_eq!(scanner.scan_token(), Ok(Token::Minus));
            assert_eq!(scanner.scan_token(), Ok(Token::Slash));
            assert_eq!(scanner.scan_token(), Ok(Token::Star));
            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }

        {
            let src = "/".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Slash));
            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }
    }

    #[test]
    fn test_keywords() {
        let src =
            "int float string vec hash_map struct enum union interface while for return fn let mut true false";
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::TypeInt));
        assert_eq!(scanner.scan_token(), Ok(Token::TypeFloat));
        assert_eq!(scanner.scan_token(), Ok(Token::TypeString));

        assert_eq!(scanner.scan_token(), Ok(Token::Vector));
        assert_eq!(scanner.scan_token(), Ok(Token::HashMap));

        assert_eq!(scanner.scan_token(), Ok(Token::Struct));
        assert_eq!(scanner.scan_token(), Ok(Token::Enum));
        assert_eq!(scanner.scan_token(), Ok(Token::Union));
        assert_eq!(scanner.scan_token(), Ok(Token::Interface));
        assert_eq!(scanner.scan_token(), Ok(Token::While));
        assert_eq!(scanner.scan_token(), Ok(Token::For));
        assert_eq!(scanner.scan_token(), Ok(Token::Return));
        assert_eq!(scanner.scan_token(), Ok(Token::Fn));
        assert_eq!(scanner.scan_token(), Ok(Token::Let));
        assert_eq!(scanner.scan_token(), Ok(Token::Mut));
        assert_eq!(scanner.scan_token(), Ok(Token::True));
        assert_eq!(scanner.scan_token(), Ok(Token::False));
        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_identifier() {
        let src = "hello".to_string();
        let mut scanner = Scanner::new(&src);

        assert_eq!(scanner.scan_token(), Ok(Token::Identifier(&"hello")));
        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_delimiters() {
        let src = "()[]{}".to_string();
        let mut scanner = Scanner::new(&src);

        assert_eq!(scanner.scan_token(), Ok(Token::LeftParen));
        assert_eq!(scanner.scan_token(), Ok(Token::RightParen));

        assert_eq!(scanner.scan_token(), Ok(Token::LeftBracket));
        assert_eq!(scanner.scan_token(), Ok(Token::RightBracket));

        assert_eq!(scanner.scan_token(), Ok(Token::LeftBrace));
        assert_eq!(scanner.scan_token(), Ok(Token::RightBrace));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_integer() {
        {
            let src = "123".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Integer(123)));

            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }

        {
            let src = "-123".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Minus));
            assert_eq!(scanner.scan_token(), Ok(Token::Integer(123)));

            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }
    }

    #[test]
    fn test_float() {
        {
            let src = "3.142".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Float(3.142)));

            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
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
        assert_eq!(scanner.scan_token(), Ok(Token::Integer(467)));
        assert_eq!(scanner.scan_token(), Ok(Token::Float(1.45)));
        assert_eq!(scanner.scan_token(), Ok(Token::Float(20.1)));
        assert_eq!(scanner.scan_token(), Ok(Token::Integer(50)));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_comparison() {
        let src = "<>>====!!=<=".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Less));
        assert_eq!(scanner.scan_token(), Ok(Token::Greater));
        assert_eq!(scanner.scan_token(), Ok(Token::GreaterEqual));
        assert_eq!(scanner.scan_token(), Ok(Token::EqualEqual));
        assert_eq!(scanner.scan_token(), Ok(Token::Equal));
        assert_eq!(scanner.scan_token(), Ok(Token::Bang));
        assert_eq!(scanner.scan_token(), Ok(Token::BangEqual));
        assert_eq!(scanner.scan_token(), Ok(Token::LessEqual));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_punctuation() {
        let src = "..=...;:,".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::DotDotEqual));
        assert_eq!(scanner.scan_token(), Ok(Token::DotDot));
        assert_eq!(scanner.scan_token(), Ok(Token::Dot));
        assert_eq!(scanner.scan_token(), Ok(Token::SemiColon));
        assert_eq!(scanner.scan_token(), Ok(Token::Colon));
        assert_eq!(scanner.scan_token(), Ok(Token::Comma));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_array() {
        let src = "->".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::MinusGreater));
    }

    #[test]
    fn test_string() {
        {
            let src = "\"\"".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Str("")));
            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }

        {
            let src = "\"this is a string\"".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Str("this is a string")));
            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }

        {
            let src = "\"".to_owned();
            let mut scanner = Scanner::new(&src);
            assert!(scanner.scan_token().is_err());
        }
    }

    #[test]
    fn test_peek() {
        let src = "123+456";
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.peek_token(), Ok(Token::Integer(123)));
        assert_eq!(scanner.peek_token(), Ok(Token::Integer(123)));
        assert_eq!(scanner.scan_token(), Ok(Token::Integer(123)));

        assert_eq!(scanner.peek_token(), Ok(Token::Plus));
        assert_eq!(scanner.scan_token(), Ok(Token::Plus));

        assert_eq!(scanner.peek_token(), Ok(Token::Integer(456)));
        assert_eq!(scanner.scan_token(), Ok(Token::Integer(456)));
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
