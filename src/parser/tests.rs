mod tests {
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
    fn test_boolean_literal_expression() {
        let tests = vec![
            (
                "true;",
                Stmt::ExprStmt(Box::new(Expr::LitExp(Literal::Boolean(true)))),
            ),
            (
                "false;",
                Stmt::ExprStmt(Box::new(Expr::LitExp(Literal::Boolean(false)))),
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
                        "Boolean Literal Statement does not match expected"
                    )
                }
                None => assert!(false, "program.parse_program returned None"),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let tests = vec![(
            "add(1, 2 * 3, 4 + 5)",
            Stmt::ExprStmt(Box::new(Expr::Call(
                Box::new(Expr::IdentExp(Ident(String::from("add")))),
                vec![
                    Expr::LitExp(Literal::Int(1)),
                    Expr::Infix(
                        Operator::ASTERISK,
                        Box::new(Expr::LitExp(Literal::Int(2))),
                        Box::new(Expr::LitExp(Literal::Int(3))),
                    ),
                    Expr::Infix(
                        Operator::PLUS,
                        Box::new(Expr::LitExp(Literal::Int(4))),
                        Box::new(Expr::LitExp(Literal::Int(5))),
                    ),
                ],
            ))),
        )];

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

                    assert_eq!(vec![expected], p, "Call parsing does not match expected")
                }
                None => assert!(false, "program.parse_program returned None"),
            }
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let tests = vec![
            (
                "fn() { }",
                Stmt::ExprStmt(Box::new(Expr::Fn(vec![], vec![]))),
            ),
            (
                "fn(x) { }",
                Stmt::ExprStmt(Box::new(Expr::Fn(vec![Ident(String::from("x"))], vec![]))),
            ),
            (
                "fn(x, y, z) { }",
                Stmt::ExprStmt(Box::new(Expr::Fn(
                    vec![
                        Ident(String::from("x")),
                        Ident(String::from("y")),
                        Ident(String::from("z")),
                    ],
                    vec![],
                ))),
            ),
            (
                "fn(x, y) { x + y; }",
                Stmt::ExprStmt(Box::new(Expr::Fn(
                    vec![Ident(String::from("x")), Ident(String::from("y"))],
                    vec![Stmt::ExprStmt(Box::new(Expr::Infix(
                        Operator::PLUS,
                        Box::new(Expr::IdentExp(Ident(String::from("x")))),
                        Box::new(Expr::IdentExp(Ident(String::from("y")))),
                    )))],
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
                        "Function Literal does not match expected"
                    )
                }
                None => assert!(false, "program.parse_program returned None"),
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
    fn test_if_expression() {
        let tests = vec![
            (
                "if (x < y) { x }",
                Stmt::ExprStmt(Box::new(Expr::If(
                    Box::new(Expr::Infix(
                        Operator::LT,
                        Box::new(Expr::IdentExp(Ident(String::from("x")))),
                        Box::new(Expr::IdentExp(Ident(String::from("y")))),
                    )),
                    vec![Stmt::ExprStmt(Box::new(Expr::IdentExp(Ident(
                        String::from("x"),
                    ))))],
                    None,
                ))),
            ),
            (
                "if (x < y) { x } else { y }",
                Stmt::ExprStmt(Box::new(Expr::If(
                    Box::new(Expr::Infix(
                        Operator::LT,
                        Box::new(Expr::IdentExp(Ident(String::from("x")))),
                        Box::new(Expr::IdentExp(Ident(String::from("y")))),
                    )),
                    vec![Stmt::ExprStmt(Box::new(Expr::IdentExp(Ident(
                        String::from("x"),
                    ))))],
                    Some(vec![Stmt::ExprStmt(Box::new(Expr::IdentExp(Ident(
                        String::from("y"),
                    ))))]),
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

                    assert_eq!(expected, p[0], "If Statements do not match expected")
                }
                None => assert!(false, "program.parse_program returned None"),
            }
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
    fn test_let_statements() {
        let tests = vec![
            (
                "let x = 5;",
                Stmt::Let(
                    Ident(String::from("x")),
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ),
            ),
            (
                "let y = 10;",
                Stmt::Let(
                    Ident(String::from("y")),
                    Box::new(Expr::LitExp(Literal::Int(10))),
                ),
            ),
            (
                "let foobar = 838383;",
                Stmt::Let(
                    Ident(String::from("foobar")),
                    Box::new(Expr::LitExp(Literal::Int(838383))),
                ),
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

                    assert_eq!(expected, p[0], "Let Statements do not match expected")
                }
                None => assert!(false, "program.parse_program returned None"),
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("true", "true;"),
            ("false", "false;"),
            ("3 > 5 == false", "((3 > 5) == false);"),
            ("3 < 5 == true", "((3 < 5) == true);"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2", "((5 + 5) * 2);"),
            ("2 / (5 + 5)", "(2 / (5 + 5));"),
            ("-(5 + 5)", "(-(5 + 5));"),
            ("!(true == true)", "(!(true == true));"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d);"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g));",
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
                        expected,
                        format!("{}", p[0]).as_str(),
                        "Order precedence statement does not match expected"
                    )
                }
                None => assert!(false, "program.parse_program returned None"),
            }
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
            (
                "true == true",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::EQ,
                    Box::new(Expr::LitExp(Literal::Boolean(true))),
                    Box::new(Expr::LitExp(Literal::Boolean(true))),
                ))),
            ),
            (
                "true != false",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::NE,
                    Box::new(Expr::LitExp(Literal::Boolean(true))),
                    Box::new(Expr::LitExp(Literal::Boolean(false))),
                ))),
            ),
            (
                "false  == false",
                Stmt::ExprStmt(Box::new(Expr::Infix(
                    Operator::EQ,
                    Box::new(Expr::LitExp(Literal::Boolean(false))),
                    Box::new(Expr::LitExp(Literal::Boolean(false))),
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
                        "Infix Literal Statement does not match expected"
                    )
                }
                None => assert!(false, "program.parse_program returned None"),
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expression() {
        let tests = vec![
            (
                "!5",
                Stmt::ExprStmt(Box::new(Expr::Prefix(
                    Operator::BANG,
                    Box::new(Expr::LitExp(Literal::Int(5))),
                ))),
            ),
            (
                "-15",
                Stmt::ExprStmt(Box::new(Expr::Prefix(
                    Operator::MINUS,
                    Box::new(Expr::LitExp(Literal::Int(15))),
                ))),
            ),
            (
                "!true",
                Stmt::ExprStmt(Box::new(Expr::Prefix(
                    Operator::BANG,
                    Box::new(Expr::LitExp(Literal::Boolean(true))),
                ))),
            ),
            (
                "!false",
                Stmt::ExprStmt(Box::new(Expr::Prefix(
                    Operator::BANG,
                    Box::new(Expr::LitExp(Literal::Boolean(false))),
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
    fn test_return_statement() {
        let tests = vec![
            (
                "return 5;",
                Stmt::Return(Box::new(Expr::LitExp(Literal::Int(5)))),
            ),
            (
                "return 10;",
                Stmt::Return(Box::new(Expr::LitExp(Literal::Int(10)))),
            ),
            (
                "return 993322;",
                Stmt::Return(Box::new(Expr::LitExp(Literal::Int(993322)))),
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

                    assert_eq!(expected, p[0], "Return Statements do not match expected")
                }
                None => assert!(false, "program.parse_program returned None"),
            }
        }
    }
}
