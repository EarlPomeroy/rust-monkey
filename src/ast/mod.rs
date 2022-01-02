use std::fmt::{self};

#[derive(Debug, PartialEq)]
pub struct Ident(pub String);

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    BANG,
    MINUS,
    PLUS,
    ASTERISK,
    SLASH,
    GT,
    LT,
    EQ,
    NE,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::BANG => write!(f, "!"),
            Operator::MINUS => write!(f, "-"),
            Operator::PLUS => write!(f, "+"),
            Operator::ASTERISK => write!(f, "*"),
            Operator::SLASH => write!(f, "/"),
            Operator::GT => write!(f, ">"),
            Operator::LT => write!(f, "<"),
            Operator::EQ => write!(f, "=="),
            Operator::NE => write!(f, "!="),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(Ident, Box<Expr>),
    Return(Box<Expr>),
    ExprStmt(Box<Expr>),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Let(i, e) => write!(f, "let {} = {};", i.0, e),
            Stmt::Return(e) => write!(f, "return {};", e),
            Stmt::ExprStmt(e) => write!(f, "{};", e),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    IdentExp(Ident),
    LitExp(Literal),
    Prefix(Operator, Box<Expr>),
    Infix(Operator, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::IdentExp(i) => write!(f, "{}", i.0),
            Expr::LitExp(l) => write!(f, "{}", l),
            Expr::Prefix(o, e) => write!(f, "({}{})", o, e),
            Expr::Infix(o, l, r) => write!(f, "({} {} {})", l, o, r),
        }
    }
}

#[derive(PartialOrd, PartialEq, Clone, Copy)]
pub enum Precedence {
    LOWEST = 1,
    EQUALS,      // ==
    LESSGREATER, // < or >
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // !x or -x
    CALL,        // myFunc(x)
}

pub type Program = Vec<Stmt>;

#[cfg(test)]
mod testing {
    use super::{Expr, Ident, Literal, Program, Stmt};

    #[test]
    fn test_string() {
        let tests = vec!["let myVar = anotherVar;", "let foo = 42;"];

        let mut program = Program::new();

        program.push(Stmt::Let(
            Ident(String::from("myVar")),
            Box::new(Expr::IdentExp(Ident(String::from("anotherVar")))),
        ));
        program.push(Stmt::Let(
            Ident(String::from("foo")),
            Box::new(Expr::LitExp(Literal::Int(42))),
        ));

        for (i, stmt) in program.iter().enumerate() {
            assert_eq!(format!("{}", stmt), tests[i]);
        }
    }
}
