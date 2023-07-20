
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Struct,
    Interface,

    Fn,
    Identifier(&'a str),
    Integer(u32),
    Float(f32),

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

    Equal,
    EqualEqual,

    Let,
    Mut,

    Dot,
    SemiColon,
    Comma,

    Print,

    True,
    False,

    Eof
}

pub struct Scanner<'a> {
    src: &'a str,
    current: usize,
    line: u32
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Scanner {
        Scanner {
            src,
            current: 0,
            line: 1
        }
    }

    pub fn line(self: &Self) -> u32 {
        self.line
    }

    fn peek(self: &Self) -> char {
        self.src[self.current..].chars().next().unwrap()
    }

    fn peek_next(self: &Self) -> char {
        self.src[self.current..].chars().nth(1).unwrap()
    }

    fn advance(self: &mut Self) -> char {
        let c = self.peek();
        self.current += c.len_utf8();
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
        return (self.current) >= self.src.len()
    }

    fn skip_white_space(self: &mut Self) {
        if self.at_eof() {
            return
        }

        while !self.at_eof() {
            match self.peek() {
                ' ' | '\t' | '\r' => { self.advance(); },
                '\n' => {
                    self.line += 1;
                    self.advance();
                },
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' {
                            self.advance();
                            if self.at_eof() {
                                return
                            }
                        }
                    }
                    else {
                        return
                    }
                },
                _ => return
            }
        }
    }

    fn identifier(self: &mut Self) -> Token<'a> {
        while !self.at_eof() && self.peek().is_alphanumeric() {
            self.advance();
        }

        let ident = &self.src[..self.current];
        match ident {
            "struct" => Token::Struct,
            "interface" => Token::Interface,
            "fn" => Token::Fn,
            "let" => Token::Let,
            "mut" => Token::Mut,
            "print" => Token::Print,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Identifier(ident)
        }
    }

    fn number(self: &mut Self) -> Result<Token<'a>, String> {
        while !self.at_eof() && self.peek().is_numeric() {
            self.advance();
        }

        if !self.at_eof() && self.peek() == '.' {
            self.advance();
            if !self.peek().is_digit(10) {
                return Err("Expected digit after '.'".to_owned())
            }

            while !self.at_eof() && self.peek().is_numeric() {
                self.advance();
            }

            let number_str = &self.src[..self.current];
            return Ok(Token::Float(str::parse::<f32>(number_str).unwrap()))
        }

        let number_str = &self.src[..self.current];
        return Ok(Token::Integer(str::parse::<u32>(number_str).unwrap()))
    }

    pub fn scan_token(self: &mut Self) -> Result<Token<'a>, String> {
        self.skip_white_space();

        if self.at_eof() {
            return Ok(Token::Eof)
        }

        self.src = &self.src[self.current..];
        self.current = 0;

        let c = self.advance();
        println!("CHAR {}", c);

        if c.is_alphabetic() {
            return Ok(self.identifier())
        }
        else if c.is_digit(10) {
            return self.number()
        }

        match c {
            '+' => Ok(Token::Plus),
            '-' => Ok(Token::Minus),
            '/' => Ok(Token::Slash),
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

            _ => Err("Unexpected token".to_owned())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comment() {
        let src = "//a comment\n//another comment".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
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
        assert_eq!(scanner.line, 2);

        scanner.scan_token().unwrap();
        assert_eq!(scanner.line, 3);
        scanner.scan_token().unwrap();
        assert_eq!(scanner.line, 3);

        scanner.scan_token().unwrap();
        assert_eq!(scanner.line, 4);

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_arithmetic() {
        let src = "+-/*".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Plus));
        assert_eq!(scanner.scan_token(), Ok(Token::Minus));
        assert_eq!(scanner.scan_token(), Ok(Token::Slash));
        assert_eq!(scanner.scan_token(), Ok(Token::Star));
        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_keywords() {
        let src = "struct interface";
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Struct));
        assert_eq!(scanner.scan_token(), Ok(Token::Interface));
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
    fn test_function() {
        let src = "fn".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Fn));

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
            let src = "3.142".to_owned();
            let mut scanner = Scanner::new(&src);
            assert_eq!(scanner.scan_token(), Ok(Token::Float(3.142)));

            assert_eq!(scanner.scan_token(), Ok(Token::Eof));
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
        let src = "<>>====<=".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Less));
        assert_eq!(scanner.scan_token(), Ok(Token::Greater));
        assert_eq!(scanner.scan_token(), Ok(Token::GreaterEqual));
        assert_eq!(scanner.scan_token(), Ok(Token::EqualEqual));
        assert_eq!(scanner.scan_token(), Ok(Token::Equal));
        assert_eq!(scanner.scan_token(), Ok(Token::LessEqual));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_let_mut() {
        let src = "let mut".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Let));
        assert_eq!(scanner.scan_token(), Ok(Token::Mut));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_puncuation() {
        let src = ".;,".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::Dot));
        assert_eq!(scanner.scan_token(), Ok(Token::SemiColon));
        assert_eq!(scanner.scan_token(), Ok(Token::Comma));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }

    #[test]
    fn test_boolean() {
        let src = "true false".to_owned();
        let mut scanner = Scanner::new(&src);
        assert_eq!(scanner.scan_token(), Ok(Token::True));
        assert_eq!(scanner.scan_token(), Ok(Token::False));

        assert_eq!(scanner.scan_token(), Ok(Token::Eof));
    }
}
