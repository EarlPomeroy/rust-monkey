mod tests {
    use std::ops::Deref;

    use crate::{
        ast::{Expr, Ident, Literal, Operator, Stmt},
        lexer::Lexer,
        parser::Parser,
    };

    fn check_parse_errors(p: Parser) {
        let errors = p.errors();

        if errors.len() > 0 {
            println!("parser has {} error(s)", errors.len());

            for error in errors {
                assert!(false, "{}", error);
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let tests = vec![Stmt::ExprStmt(Box::new(Expr::IdentExp(Ident(
            "foobar".to_string(),
        ))))];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        match program {
            Some(p) => {
                assert_eq!(
                    p.len(),
                    1,
                    "program.statements does not contain 1 statements. got={}",
                    p.len()
                );

                for (i, stmt) in p.iter().enumerate() {
                    assert_eq!(
                        tests[i], *stmt,
                        "Identifier Expressions do not match. got={}, expected={}",
                        stmt, tests[i]
                    )
                }
            }
            None => assert!(false, "program.parse_program returned None"),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let tests = vec![Stmt::ExprStmt(Box::new(Expr::LitExp(Literal::Int(5))))];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        match program {
            Some(p) => {
                assert_eq!(
                    p.len(),
                    1,
                    "program.statements does not contain 1 statements. got={}",
                    p.len()
                );

                for (i, stmt) in p.iter().enumerate() {
                    assert_eq!(
                        tests[i], *stmt,
                        "Integer Literal Expressions do not match. got={}, expected={}",
                        stmt, tests[i]
                    )
                }
            }
            None => assert!(false, "program.parse_program returned None"),
        }
    }

    #[test]
    #[ignore]
    fn test_let_statements() {
        let input = r#"
          let x = 5;
          let y = 10;
          let foobar = 838383;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(parser);

        match program {
            Some(p) => {
                assert_eq!(
                    p.len(),
                    3,
                    "program.statements does not contain 3 statements. got={}",
                    p.len()
                );

                assert_eq!(
                    vec![
                        Stmt::Let(
                            Ident(String::from("x")),
                            Box::new(Expr::LitExp(Literal::Int(5)))
                        ),
                        Stmt::Let(
                            Ident(String::from("yz")),
                            Box::new(Expr::LitExp(Literal::Int(10)))
                        ),
                        Stmt::Let(
                            Ident(String::from("foobar")),
                            Box::new(Expr::LitExp(Literal::Int(838383)))
                        ),
                    ],
                    p,
                    "Let Statements do not match expected"
                )
            }
            None => assert!(false, "program.parse_program returned None"),
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let tests = vec![
            (
                "5 + 5",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::PLUS,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
            (
                "5 - 5",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::MINUS,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
            (
                "5 * 5",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::ASTERISK,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
            (
                "5 / 5",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::SLASH,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
            (
                "5 > 5",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::GT,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
            (
                "5 < 5",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::LT,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
            (
                "5 == 5",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::EQ,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
            (
                "5 != 5",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::NE,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
        ];

        for (test, expected) in tests {
            let lexer = Lexer::new(test);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parse_errors(parser);

            match program {
                Some(p) => {
                    assert_eq!(
                        p.len(),
                        1,
                        "program.statements does not contain 1 statement. got={}",
                        p.len()
                    );

                    assert_eq!(
                        vec![expected],
                        p,
                        "Prefix Literal Statement does not match expected"
                    )
                }
                None => assert!(false, "program.parse_program returned None"),
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let input = r#"
            !5;
            -15;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(parser);

        match program {
            Some(p) => {
                assert_eq!(
                    p.len(),
                    2,
                    "program.statements does not contain 2 statements. got={}",
                    p.len()
                );

                assert_eq!(
                    vec![
                        Stmt::ExprStmt(Box::new(Expr::Prefix(
                            Operator::BANG,
                            Box::new(Expr::LitExp(Literal::Int(5)))
                        ))),
                        Stmt::ExprStmt(Box::new(Expr::Prefix(
                            Operator::MINUS,
                            Box::new(Expr::LitExp(Literal::Int(15)))
                        ))),
                    ],
                    p,
                    "Prefix Literal Statements do not match expected"
                )
            }
            None => assert!(false, "program.parse_program returned None"),
        }
    }

    #[test]
    #[ignore]
    fn test_return_statement() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(parser);

        match program {
            Some(p) => {
                assert_eq!(
                    p.len(),
                    3,
                    "program.statements does not contain 3 statements. got={}",
                    p.len()
                );

                assert_eq!(
                    vec![
                        Stmt::Return(Box::new(Expr::LitExp(Literal::Int(5)))),
                        Stmt::Return(Box::new(Expr::LitExp(Literal::Int(10)))),
                        Stmt::Return(Box::new(Expr::LitExp(Literal::Int(993322)))),
                    ],
                    p,
                    "Let Statements do not match expected"
                )
            }
            None => assert!(false, "program.parse_program returned None"),
        }
    }
}
