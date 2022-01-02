use std::ops::Add;

use crate::{
    ast::{Expr, Ident, Literal, Operator, Precedence, Program, Stmt},
    lexer::Lexer,
    token::Token,
};

struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_tok: Token,
    peek_tok: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Self {
        let curr_tok = lexer.next_token();
        let peek_tok = lexer.next_token();

        Self {
            lexer,
            curr_tok,
            peek_tok,
            errors: vec![],
        }
    }

    fn curr_token_is(&self, t: Token) -> bool {
        self.curr_tok == t
    }

    fn curr_precedence(&self) -> Precedence {
        Parser::precedence_order(self.curr_tok.clone())
    }

    fn errors(self) -> Vec<String> {
        self.errors.clone()
    }

    fn expect_peek(&mut self, t: &Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn peek_error(&mut self, t: &Token) {
        let err = format!(
            "expected next token to be {:?}, got {:?} instead.",
            t, self.peek_tok
        );

        self.errors.push(err);
    }

    fn peek_token_is(&self, t: &Token) -> bool {
        self.peek_tok == *t
    }

    fn peek_precedence(&self) -> Precedence {
        Parser::precedence_order(self.peek_tok.clone())
    }

    fn next_token(&mut self) {
        self.curr_tok = self.peek_tok.clone();
        self.peek_tok = self.lexer.next_token();
    }

    fn no_prefix_parser_err(&mut self) {
        let err = format!("no prefix parse function for {} found.", self.curr_tok);
        self.errors.push(err);
    }

    fn parse_identifier(&self, val: String) -> Expr {
        Expr::IdentExp(Ident(val))
    }

    fn parse_infix_expression(&mut self, left: Expr) -> Option<Expr> {
        let operator = match self.curr_tok {
            Token::MINUS => Operator::MINUS,
            Token::PLUS => Operator::PLUS,
            Token::ASTERISK => Operator::ASTERISK,
            Token::SLASH => Operator::SLASH,
            Token::LT => Operator::LT,
            Token::GT => Operator::GT,
            Token::EQ => Operator::EQ,
            Token::NE => Operator::NE,
            _ => return None,
        };

        let precedence = self.curr_precedence();
        self.next_token();

        match self.parse_expression(precedence) {
            Some(right) => Some(Expr::Infix(operator, Box::new(left), Box::new(right))),
            None => None,
        }
    }

    fn parse_integer_literal(&self, val: i64) -> Expr {
        Expr::LitExp(Literal::Int(val))
    }

    fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::new();

        while self.curr_tok != Token::EOF {
            match self.parse_statement() {
                Some(s) => program.push(s),
                None => return None,
            }
        }

        Some(program)
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        // println!("Current Token: ")
        match self.curr_tok {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut left_exp = match &self.curr_tok {
            Token::IDENT(val) => Some(self.parse_identifier(val.clone())),
            Token::INT(val) => Some(self.parse_integer_literal(val.clone())),
            Token::BANG | Token::MINUS => self.parse_prefix_expression(),

            _ => {
                self.no_prefix_parser_err();
                return None;
            }
        };

        while !self.peek_token_is(&Token::SEMICOLON) && precedence < self.peek_precedence() {
            self.next_token();
            left_exp = self.parse_infix_expression(left_exp.unwrap());
        }

        left_exp
    }

    fn parse_expression_statement(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(&Token::SEMICOLON) {
            self.next_token();
        }

        self.next_token();

        match expr {
            Some(e) => Some(Stmt::ExprStmt(Box::new(e))),
            None => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        self.next_token();
        let name = match self.curr_tok.clone() {
            Token::IDENT(s) => s,
            _ => return None,
        };

        if !self.expect_peek(&Token::ASSIGN) {
            return None;
        }

        // TODO: Fix getting expressions
        while !self.curr_token_is(Token::SEMICOLON) {
            self.next_token();
        }

        self.next_token();

        Some(Stmt::Let(
            Ident(name.to_string()),
            Box::new(Expr::LitExp(Literal::Int(5))),
        ))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expr> {
        let operator = match self.curr_tok {
            Token::MINUS => Operator::MINUS,
            Token::BANG => Operator::BANG,
            _ => return None,
        };

        self.next_token();

        match self.parse_expression(Precedence::PREFIX) {
            Some(expr) => Some(Expr::Prefix(operator, Box::new(expr))),
            None => None,
        }
    }

    fn parse_return_statement(&mut self) -> Option<Stmt> {
        // TODO: Fix getting expressions
        while !self.curr_token_is(Token::SEMICOLON) {
            self.next_token();
        }
        self.next_token();

        Some(Stmt::Return(Box::new(Expr::LitExp(Literal::Int(5)))))
    }

    fn precedence_order(token: Token) -> Precedence {
        match token {
            Token::PLUS => Precedence::SUM,
            Token::MINUS => Precedence::SUM,
            Token::ASTERISK => Precedence::PRODUCT,
            Token::SLASH => Precedence::PRODUCT,
            Token::LT => Precedence::LESSGREATER,
            Token::GT => Precedence::LESSGREATER,
            Token::EQ => Precedence::EQUALS,
            Token::NE => Precedence::EQUALS,
            _ => Precedence::LOWEST,
        }
    }
}

#[cfg(test)]
mod tests;
