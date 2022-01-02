use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };

        lexer.read_char();

        return lexer;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.read_whitespace();

        let tok = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            b';' => Token::SEMICOLON,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b',' => Token::COMMA,
            b'+' => Token::PLUS,
            b'-' => Token::MINUS,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NE
                } else {
                    Token::BANG
                }
            }
            b'*' => Token::ASTERISK,
            b'/' => Token::SLASH,
            b'<' => Token::LT,
            b'>' => Token::GT,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            0 => Token::EOF,
            _ => {
                if self.is_letter() {
                    return self.read_identifier();
                } else if self.is_digit() {
                    return self.read_digit();
                } else {
                    Token::ILLEGAL
                }
            }
        };

        self.read_char();
        tok
    }

    fn is_digit(&self) -> bool {
        self.ch.is_ascii_digit()
    }

    fn is_letter(&self) -> bool {
        self.ch.is_ascii_alphabetic() || self.ch == b'_'
    }

    fn peek_char(&self) -> u8 {
        self.input.as_bytes()[self.read_position]
    }

    fn read_digit(&mut self) -> Token {
        let pos = self.position;
        while self.is_digit() {
            self.read_char();
        }

        let num_val = &self.input[pos..self.position];

        // TODO: add error here is not a int
        Token::INT(num_val.parse::<i64>().unwrap())
    }

    fn read_identifier(&mut self) -> Token {
        let pos = self.position;
        while self.is_letter() {
            self.read_char();
        }

        let ident_val = &self.input[pos..self.position];

        Token::lookup_ident(ident_val)
    }

    fn read_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn test_next_token() {
        let input = r#"
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        
        10 == 10;
        10 != 9;
        "#;

        struct TestCase {
            expected_type: Token,
        }

        impl TestCase {
            fn new(token_type: Token) -> Self {
                Self {
                    expected_type: token_type,
                }
            }
        }

        let mut tests = Vec::new();
        tests.push(TestCase::new(Token::LET));
        tests.push(TestCase::new(Token::IDENT(String::from("five"))));
        tests.push(TestCase::new(Token::ASSIGN));
        tests.push(TestCase::new(Token::INT(5)));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::LET));
        tests.push(TestCase::new(Token::IDENT(String::from("ten"))));
        tests.push(TestCase::new(Token::ASSIGN));
        tests.push(TestCase::new(Token::INT(10)));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::LET));
        tests.push(TestCase::new(Token::IDENT(String::from("add"))));
        tests.push(TestCase::new(Token::ASSIGN));
        tests.push(TestCase::new(Token::FUNCTION));
        tests.push(TestCase::new(Token::LPAREN));
        tests.push(TestCase::new(Token::IDENT(String::from("x"))));
        tests.push(TestCase::new(Token::COMMA));
        tests.push(TestCase::new(Token::IDENT(String::from("y"))));
        tests.push(TestCase::new(Token::RPAREN));
        tests.push(TestCase::new(Token::LBRACE));
        tests.push(TestCase::new(Token::IDENT(String::from("x"))));
        tests.push(TestCase::new(Token::PLUS));
        tests.push(TestCase::new(Token::IDENT(String::from("y"))));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::RBRACE));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::LET));
        tests.push(TestCase::new(Token::IDENT(String::from("result"))));
        tests.push(TestCase::new(Token::ASSIGN));
        tests.push(TestCase::new(Token::IDENT(String::from("add"))));
        tests.push(TestCase::new(Token::LPAREN));
        tests.push(TestCase::new(Token::IDENT(String::from("five"))));
        tests.push(TestCase::new(Token::COMMA));
        tests.push(TestCase::new(Token::IDENT(String::from("ten"))));
        tests.push(TestCase::new(Token::RPAREN));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::BANG));
        tests.push(TestCase::new(Token::MINUS));
        tests.push(TestCase::new(Token::SLASH));
        tests.push(TestCase::new(Token::ASTERISK));
        tests.push(TestCase::new(Token::INT(5)));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::INT(5)));
        tests.push(TestCase::new(Token::LT));
        tests.push(TestCase::new(Token::INT(10)));
        tests.push(TestCase::new(Token::GT));
        tests.push(TestCase::new(Token::INT(5)));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::IF));
        tests.push(TestCase::new(Token::LPAREN));
        tests.push(TestCase::new(Token::INT(5)));
        tests.push(TestCase::new(Token::LT));
        tests.push(TestCase::new(Token::INT(10)));
        tests.push(TestCase::new(Token::RPAREN));
        tests.push(TestCase::new(Token::LBRACE));
        tests.push(TestCase::new(Token::RETURN));
        tests.push(TestCase::new(Token::BOOLEAN(true)));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::RBRACE));
        tests.push(TestCase::new(Token::ELSE));
        tests.push(TestCase::new(Token::LBRACE));
        tests.push(TestCase::new(Token::RETURN));
        tests.push(TestCase::new(Token::BOOLEAN(false)));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::RBRACE));
        tests.push(TestCase::new(Token::INT(10)));
        tests.push(TestCase::new(Token::EQ));
        tests.push(TestCase::new(Token::INT(10)));
        tests.push(TestCase::new(Token::SEMICOLON));
        tests.push(TestCase::new(Token::INT(10)));
        tests.push(TestCase::new(Token::NE));
        tests.push(TestCase::new(Token::INT(9)));
        tests.push(TestCase::new(Token::SEMICOLON));

        tests.push(TestCase::new(Token::EOF));

        let mut lexer = Lexer::new(input);

        for (i, test) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(
                test.expected_type, tok,
                "Test: {}. Wrong Token. got={:?} expected={:?}",
                i, tok, test.expected_type
            );
        }
    }
}
