use super::ast::{Boolean, Ident, Integer};
use crate::lexer::tokenizer::{Token, TokenType};

/// Utilities for testing
impl Token {
    pub fn mock_ident(ident: &str) -> Token {
        Token {
            token_type: TokenType::Ident,
            literal: ident.to_string(),
            line_num: 0,
            column_num: 0,
        }
    }

    pub fn mock_int(num: usize) -> Token {
        Token {
            token_type: TokenType::Int,
            literal: num.to_string(),
            line_num: 0,
            column_num: 0,
        }
    }

    pub fn mock_bool(val: bool) -> Token {
        Token {
            token_type: if val { TokenType::True } else { TokenType::False },
            literal: val.to_string(),
            line_num: 0,
            column_num: 0,
        }
    }

    pub fn mock_plus() -> Token {
        Token {
            token_type: TokenType::Plus,
            literal: String::from("+"),
            line_num: 0,
            column_num: 0,
        }
    }

    pub fn mock_asterisk() -> Token {
        Token {
            token_type: TokenType::Asterisk,
            literal: String::from("*"),
            line_num: 0,
            column_num: 0,
        }
    }

    pub fn mock_minus() -> Token {
        Token {
            token_type: TokenType::Minus,
            literal: String::from("-"),
            line_num: 0,
            column_num: 0,
        }
    }

    pub fn mock(tok_type: TokenType, literal: &str) -> Token {
        Token {
            token_type: tok_type,
            literal: literal.to_string(),
            line_num: 0,
            column_num: 0,
        }
    }
}

impl Integer {
    pub fn mock(num: usize) -> Integer {
        Integer(Token::mock_int(num), num)
    }
}

impl Boolean {
    pub fn mock(value: bool) -> Boolean {
        Boolean(Token::mock_bool(value), value)
    }
}

impl Ident {
    pub fn mock(value: &str) -> Ident {
        Ident(Token::mock_ident(value), value.to_string())
    }
}
#[cfg(test)]
mod tests {

    use crate::{
        lexer::tokenizer::{Token, TokenType, Tokenizer},
        parser::{ast::*, parser::Parser},
    };

    fn test_statement(input: &str, expected_ast: Vec<Statement>) -> () {
        let mut parser = Parser::new(Tokenizer::new(input));
        let Program { statements: actual } = parser.parse_program();

        assert_eq!(actual, expected_ast)
    }
    #[test]
    fn test_let_statement() {
        test_statement(
            "let foo = bar;",
            vec![Statement::Let(
                Ident(Token::mock_ident("foo"), String::from("foo")),
                Expression::Identifier(Ident(Token::mock_ident("bar"), String::from("bar"))),
            )],
        );
        test_statement(
            "let foo = 5;",
            vec![Statement::Let(
                Ident(Token::mock_ident("foo"), String::from("foo")),
                Expression::Integer(Integer::mock(5)),
            )],
        );
        test_statement(
            "let foo = 5 + 3;",
            vec![Statement::Let(
                Ident(Token::mock_ident("foo"), String::from("foo")),
                Expression::InfixExpression(Box::new(InfixExpression::new(
                    Token::new(TokenType::Plus, "+", 0, 0),
                    Expression::Integer(Integer::mock(5)),
                    Expression::Integer(Integer::mock(3)),
                ))),
            )],
        );
        test_statement(
            "let foo = bar + baz;",
            vec![Statement::Let(
                Ident(Token::mock_ident("foo"), String::from("foo")),
                Expression::InfixExpression(Box::new(InfixExpression::new(
                    Token::new(TokenType::Plus, "+", 0, 0),
                    Expression::Identifier(Ident(Token::mock_ident("bar"), String::from("bar"))),
                    Expression::Identifier(Ident(Token::mock_ident("baz"), String::from("baz"))),
                ))),
            )],
        );
    }

    #[test]
    fn test_expression_statement() {
        test_statement(
            "ident;",
            vec![Statement::ExpresssionStatement(Expression::Identifier(Ident(
                Token::mock_ident("ident"),
                String::from("ident"),
            )))],
        );
        test_statement(
            "5 + 3;",
            vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
                InfixExpression::new(
                    Token::new(TokenType::Plus, "+", 0, 0),
                    Expression::Integer(Integer::mock(5)),
                    Expression::Integer(Integer::mock(3)),
                ),
            )))],
        );

        test_statement(
            "(5 + 3);",
            vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
                InfixExpression::new(
                    Token::new(TokenType::Plus, "+", 0, 0),
                    Expression::Integer(Integer::mock(5)),
                    Expression::Integer(Integer::mock(3)),
                ),
            )))],
        );
        test_statement(
            "242;",
            vec![Statement::ExpresssionStatement(Expression::Integer(Integer::mock(242)))],
        );
        test_statement(
            "5 + foo;",
            vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
                InfixExpression::new(
                    Token::new(TokenType::Plus, "+", 0, 0),
                    Expression::Integer(Integer::mock(5)),
                    Expression::Identifier(Ident(Token::mock_ident("foo"), String::from("foo"))),
                ),
            )))],
        );
        test_statement(
            "foo + bar;",
            vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
                InfixExpression::new(
                    Token::new(TokenType::Plus, "+", 0, 0),
                    Expression::Identifier(Ident(Token::mock_ident("foo"), String::from("foo"))),
                    Expression::Identifier(Ident(Token::mock_ident("bar"), String::from("bar"))),
                ),
            )))],
        );
    }

    #[test]
    fn test_ast_display() {
        let input1 = "let foo = x * y;";
        let mut parser = Parser::new(Tokenizer::new(input1));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "let foo = (x * y);");

        let input2 = "let foo = 3 * 2 * 4 + 1;";
        let mut parser = Parser::new(Tokenizer::new(input2));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "let foo = (((3 * 2) * 4) + 1);");

        let input3 = "let foo = !3 * 2 * (4 + 1) / 10;";
        let mut parser = Parser::new(Tokenizer::new(input3));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "let foo = (((! 3 * 2) * (4 + 1)) / 10);");
    }

    #[test]
    fn test_boolean() {
        let input1 = "true;";
        let mut parser = Parser::new(Tokenizer::new(input1));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "true;");

        test_statement(
            input1,
            vec![Statement::ExpresssionStatement(Expression::Boolean(Boolean::mock(
                true,
            )))],
        );

        let input2 = "(3 < 4) == true;";
        let mut parser = Parser::new(Tokenizer::new(input2));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "((3 < 4) == true);");

        test_statement(
            input2,
            vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
                InfixExpression::new(
                    Token::mock(TokenType::Eq, "=="),
                    Expression::InfixExpression(Box::new(InfixExpression::new(
                        Token::mock(TokenType::Lt, "<"),
                        Expression::Integer(Integer::mock(3)),
                        Expression::Integer(Integer::mock(4)),
                    ))),
                    Expression::Boolean(Boolean::mock(true)),
                ),
            )))],
        );
    }

    #[test]
    fn test_if_statement() {
        let input1 = "if (false) { let x = 3 + 4; }";
        let mut parser = Parser::new(Tokenizer::new(input1));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "if (false) {\n\tlet x = (3 + 4);\n}");

        test_statement(
            input1,
            vec![Statement::If(IfStatement::new(
                Token::mock(TokenType::If, "if"),
                Expression::Boolean(Boolean::mock(false)),
                BlockStatement::new(
                    Token::mock(TokenType::LBrace, "{"),
                    vec![Statement::Let(
                        Ident::mock("x"),
                        Expression::InfixExpression(Box::new(InfixExpression::new(
                            Token::new(TokenType::Plus, "+", 0, 0),
                            Expression::Integer(Integer::mock(3)),
                            Expression::Integer(Integer::mock(4)),
                        ))),
                    )],
                ),
            ))],
        );

        let input2 = "if (3 < 4) { let x = y + z; }";
        let mut parser = Parser::new(Tokenizer::new(input2));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "if ((3 < 4)) {\n\tlet x = (y + z);\n}");

        test_statement(
            input2,
            vec![Statement::If(IfStatement::new(
                Token::mock(TokenType::If, "if"),
                Expression::InfixExpression(Box::new(InfixExpression::new(
                    Token::mock(TokenType::Lt, "<"),
                    Expression::Integer(Integer::mock(3)),
                    Expression::Integer(Integer::mock(4)),
                ))),
                BlockStatement::new(
                    Token::mock(TokenType::LBrace, "{"),
                    vec![Statement::Let(
                        Ident::mock("x"),
                        Expression::InfixExpression(Box::new(InfixExpression::new(
                            Token::new(TokenType::Plus, "+", 0, 0),
                            Expression::Identifier(Ident::mock("y")),
                            Expression::Identifier(Ident::mock("z")),
                        ))),
                    )],
                ),
            ))],
        );
    }

    #[test]
    fn test_multistatement_body_if_statement() {
        let input1 = "if (x < 10) { let y = x + 4; return y + 3;}";
        let mut parser = Parser::new(Tokenizer::new(input1));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "if ((x < 10)) {\n\
            \tlet y = (x + 4);\n\
            \treturn (y + 3);\n\
            }"
        );

        let input2 = "let foo = bar + 2;\n\
                      if (x < 10) { let y = x + 4; return y + 3;}\n\
                      let foo = foo + 5;";
        let mut parser = Parser::new(Tokenizer::new(input2));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "let foo = (bar + 2);\n\
            if ((x < 10)) {\n\
            \tlet y = (x + 4);\n\
            \treturn (y + 3);\n\
            }\n\
            let foo = (foo + 5);"
        );
    }

    #[test]
    fn test_if_condition_expressions() {
        // Just a boolean
        let input1 = "if (true) { let y = x + 4;}";
        let mut parser = Parser::new(Tokenizer::new(input1));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "if (true) {\n\
            \tlet y = (x + 4);\n\
            }"
        );

        // Eq
        let input2 = "if (x == 2) { let y = x + 4;}";
        let mut parser = Parser::new(Tokenizer::new(input2));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "if ((x == 2)) {\n\
            \tlet y = (x + 4);\n\
            }"
        );

        // Gt
        let input2 = "if (x > 3) { let y = x + 4;}";
        let mut parser = Parser::new(Tokenizer::new(input2));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "if ((x > 3)) {\n\
            \tlet y = (x + 4);\n\
            }"
        );

        // Lt
        let input2 = "if (x < 3) { let y = x + 4;}";
        let mut parser = Parser::new(Tokenizer::new(input2));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "if ((x < 3)) {\n\
            \tlet y = (x + 4);\n\
            }"
        );
    }

    #[test]
    fn test_function_literal_no_params() {
        let input = "fn () { return 4; }";
        let mut parser = Parser::new(Tokenizer::new(input));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "fn () {\n\
            \treturn 4;\n\
            };"
        );
    }

    #[test]
    fn test_function_literal_single_param() {
        let input = "fn (x) { return x * x; }";
        let mut parser = Parser::new(Tokenizer::new(input));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "fn (x) {\n\
            \treturn (x * x);\n\
            };"
        );
    }

    #[test]
    fn test_function_literal_multiple_param() {
        let input = "fn (x, y, z) { return x * y * z; }";
        let mut parser = Parser::new(Tokenizer::new(input));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "fn (x, y, z) {\n\
            \treturn ((x * y) * z);\n\
            };"
        );
    }

    #[test]
    fn test_assign_function_literal() {
        let input = "let cube = fn (x) { return x * x * x; };";
        let mut parser = Parser::new(Tokenizer::new(input));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "let cube = fn (x) {\n\
            \treturn ((x * x) * x);\n\
            };"
        );
    }

    #[test]
    fn test_call_function_with_name() {
        let input = "add();";
        let mut parser = Parser::new(Tokenizer::new(input));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "add();");
    }

    #[test]
    fn test_call_function_with_one_arg() {
        let input = "add(2, 3);";
        let mut parser = Parser::new(Tokenizer::new(input));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "add(2, 3);");
    }

    #[test]
    fn test_call_function_with_multiple_args() {
        let input = "add(2, 3);";
        let mut parser = Parser::new(Tokenizer::new(input));
        let program = parser.parse_program();
        assert_eq!(format!("{}", program), "add(2, 3);");
    }

    #[test]
    fn test_call_function_literal_with_args() {
        let input = "fn (x, y) { return x + y } (2, 3);";
        let mut parser = Parser::new(Tokenizer::new(input));
        let program = parser.parse_program();
        assert_eq!(
            format!("{}", program),
            "fn (x, y) {\n\
            return (x + y)\n\
            }();"
        );
    }
}
