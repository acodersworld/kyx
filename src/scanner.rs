
#[derive(Debug, PartialEq)]
enum Token<'a> {
    Struct,
    Interface,

    Identifier(&'a str),

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

    fn at_eof(self: &Self) -> bool {
        println!("EOF: {} {} {}", &self.src[self.current..], (self.current + 1), self.src.len());
        return (self.current) >= self.src.len()
    }

    fn skip_white_space(self: &mut Self) {
        if self.at_eof() {
            return
        }

        loop {
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
            _ => Token::Identifier(ident)
        }
    }

    fn scan_token(self: &mut Self) -> Result<Token<'a>, String> {
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
        let src = "//a comment\n+\nabc\nstruct".to_owned();
        let mut scanner = Scanner::new(&src);
        scanner.scan_token().unwrap();
        assert_eq!(scanner.line, 2);
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
}
