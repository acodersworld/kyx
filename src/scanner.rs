
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Struct,
    Interface,
    While,
    For,
    Return,
    Print,
    Let,
    Mut,
    Fn,
    True,
    False,


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
    Comma,

    Eof
}

pub struct Scanner<'a> {
    src: &'a str,
    current_idx: usize,
    current_line: u32
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Scanner {
        Scanner {
            src,
            current_idx: 0,
            current_line: 1
        }
    }

    pub fn line(self: &Self) -> u32 {
        self.current_line
    }

    fn peek(self: &Self) -> char {
        self.src[self.current_idx..].chars().next().unwrap()
    }

    fn advance(self: &mut Self) -> char {
        let c = self.peek();
        self.current_idx += c.len_utf8();
        c
    }

    fn match_char(self: &mut Self, c: char) -> bool {
        if self.peek() == c {
            self.advance();
            return true
        }

        return false
    }

    fn at_eof(self: &Self) -> bool {
        return (self.current_idx) >= self.src.len()
    }

    fn skip_to_newline(self: &mut Self) {
        while !self.at_eof() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn skip_white_space(self: &mut Self) {
        if self.at_eof() {
            return
        }

        while !self.at_eof() {
            match self.peek() {
                ' ' | '\t' | '\r' => { self.advance(); },
                '\n' => {
                    self.current_line += 1;
                    self.advance();
                },
                _ => return
            }
        }
    }

    fn identifier(self: &mut Self) -> Token<'a> {
        while !self.at_eof() && self.peek().is_alphanumeric() {
            self.advance();
        }

        let ident = &self.src[..self.current_idx];
        match ident {
            "struct" => Token::Struct,
            "interface" => Token::Interface,
            "fn" => Token::Fn,
            "let" => Token::Let,
            "mut" => Token::Mut,
            "print" => Token::Print,
            "true" => Token::True,
            "false" => Token::False,
            "while" => Token::While,
            "for" => Token::For,
            "return" => Token::Return,
            _ => Token::Identifier(ident)
        }
    }

    fn string(self: &mut Self) -> Result<Token<'a>, String> {
        while !self.at_eof() && self.peek() != '"' {
            self.advance();
        }

        if self.at_eof() {
            return Err("Error".to_string())
        }

        self.advance();
        Ok(Token::Str(&self.src[1..self.current_idx-1]))
    }

    fn number(self: &mut Self, sign: i32) -> Result<Token<'a>, String> {
        while !self.at_eof() && self.peek().is_numeric() {
            self.advance();
        }

        if !self.at_eof() && self.peek() == '.' {
            self.advance();
            if self.at_eof() || !self.peek().is_digit(10) {
                return Err("Expected digit after '.'".to_owned())
            }

            while !self.at_eof() && self.peek().is_numeric() {
                self.advance();
            }

            let number_str = &self.src[..self.current_idx];
            return Ok(Token::Float(str::parse::<f32>(number_str).unwrap()))
        }

        let number_str = &self.src[..self.current_idx];
        return Ok(Token::Integer(str::parse::<i32>(number_str).unwrap()))
    }

    pub fn scan_token(self: &mut Self) -> Result<Token<'a>, String> {
        loop {
            self.skip_white_space();
            if self.at_eof() {
                return Ok(Token::Eof)
            }

            if self.peek() == '/' {
                self.advance();
                if !self.at_eof() && self.peek() == '/' {
                    // hit a line comment
                    self.advance();
                    self.skip_to_newline();
                }
                else { 
                    return Ok(Token::Slash);
                }
            }
            else {
                break;
            }
        }

        self.src = &self.src[self.current_idx..];
        self.current_idx = 0;

        let c = self.advance();

        if c.is_alphabetic() {
            return Ok(self.identifier())
        }
        else if c.is_digit(10) {
            return self.number(1)
        }

        match c {
            '+' => Ok(Token::Plus),
            '-' => {
                if self.peek().is_digit(10) {
                    return self.number(-1)
                }

                Ok(Token::Minus)
            }
            '*' => Ok(Token::Star),

            '{' => Ok(Token::LeftBrace),
            '}' => Ok(Token::RightBrace),
            '(' => Ok(Token::LeftParen),
            ')' => Ok(Token::RightParen),
            '[' => Ok(Token::LeftBracket),
            ']' => Ok(Token::RightBracket),

            '.' => Ok(Token::Dot),
            ';' => Ok(Token::SemiColon),
            ',' => Ok(Token::Comma),

            '!' => {
                if self.match_char('=') {
                    return Ok(Token::BangEqual)
                }
                return Ok(Token::Bang)
            }
            '<' => {
                if self.match_char('=') {
                    return Ok(Token::LessEqual)
                }
                return Ok(Token::Less)
            },
            '>' => {
                if self.match_char('=') {
                    return Ok(Token::GreaterEqual)
                }
                return Ok(Token::Greater)
            },
            '=' => {
                if self.match_char('=') {
                    return Ok(Token::EqualEqual)
                }
                return Ok(Token::Equal)
            },
            '"' => return self.string(),
            _ => Err(format!("Unexpected token '{}'", c))
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
                    ".to_owned();
        let mut scanner = Scanner::new(&src);
        scanner.scan_token().unwrap();
        assert_eq!(scanner.line(), 2);

        scanner.scan_token().unwrap();
        assert_eq!(scanner.line(), 3);
        scanner.scan_token().unwrap();
        assert_eq!(scanner.line(), 3);

        scanner.scan_token().unwrap();
        assert_eq!(scanner.line(), 4);

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
        let src = "struct interface while for return fn let mut true false";
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Struct));
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
    fn test_number() {
        {
            let src = "123".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Integer(123)));

            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }

        {
            let src = "-123".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Integer(-123)));

            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }

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

        {
            let src = "467 1.45 20.1 50".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Integer(467)));
            assert_eq!(scanner.scan_token(), Ok(Token::Float(1.45)));
            assert_eq!(scanner.scan_token(), Ok(Token::Float(20.1)));
            assert_eq!(scanner.scan_token(), Ok(Token::Integer(50)));

            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
        }
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
        let src = ".;,".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Dot));
        assert_eq!(scanner.scan_token(), Ok(Token::SemiColon));
        assert_eq!(scanner.scan_token(), Ok(Token::Comma));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
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
}
