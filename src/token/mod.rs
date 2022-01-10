use std::fmt;

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Token {
    ILLEGAL,
    EOF,
    // Identifies + Literals
    IDENT(String),
    INT(i64),
    BOOLEAN(bool),
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NE,
    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    // Keywords
    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::IDENT(i) => write!(f, "IDENT(value: {})", i),
            Token::INT(i) => write!(f, "INT(value: {})", i),
            Token::BOOLEAN(b) => write!(f, "BOOLEAN(value: {})", b),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl Token {
    pub fn lookup_ident(ident: &str) -> Token {
        match ident {
            "fn" => Token::FUNCTION,
            "let" => Token::LET,
            "true" => Token::BOOLEAN(true),
            "false" => Token::BOOLEAN(false),
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            _ => Token::IDENT(String::from(ident)),
        }
    }
}
