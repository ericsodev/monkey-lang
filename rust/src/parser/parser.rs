use core::panic;

use crate::lexer::tokenizer::{Token, TokenType, Tokenizer};

use super::ast::{
    BlockStatement, Boolean, Expression, Function, FunctionCall, Ident, IfStatement, InfixExpression, Integer,
    OperatorPrecendence, PrecedenceMap, PrefixExpression, Program, Statement,
};

#[derive(Debug)]
struct ParserError {
    reason: String,
    token: Token,
}

pub struct Parser {
    tokenizer: Tokenizer,
    statements: Vec<Statement>,
    current_token: Token,
    next_token: Token,
}

impl Parser {
    pub fn new(mut tokenizer: Tokenizer) -> Self {
        let current_token = tokenizer.next_token();
        let next_token = tokenizer.next_token();

        Parser {
            tokenizer,
            statements: vec![],
            current_token,
            next_token,
        }
    }

    pub fn parse_program(&mut self) -> Program {
        loop {
            match self.parse_statement() {
                Some(statement) => {
                    self.statements.push(statement);
                }
                None => break,
            }
        }

        Program {
            statements: self.statements.to_vec(),
        }
    }
}

impl Parser {
    fn advance_token(&mut self) -> () {
        self.current_token = self.next_token.clone();
        self.next_token = self.tokenizer.next_token()
    }

    fn current_token(&self) -> Token {
        self.current_token.clone()
    }

    fn peek_token(&self) -> Token {
        self.next_token.clone()
    }

    fn match_token(&mut self, expected_type: TokenType) -> Option<Token> {
        match self.current_token() {
            token if token.token_type == expected_type => Some(token),
            _ => None,
        }
    }

    fn match_next_token_err(&mut self, expected_type: TokenType) -> Result<Token, ParserError> {
        match self.peek_token() {
            token if token.token_type == expected_type => Ok(token),
            token => Err(ParserError {
                reason: format!("Expected {:?}", expected_type),
                token,
            }),
        }
    }

    fn match_token_err(&mut self, expected_type: TokenType) -> Result<Token, ParserError> {
        match self.current_token() {
            token if token.token_type == expected_type => Ok(token),
            token => Err(ParserError {
                reason: format!("Expected {:?}", expected_type),
                token,
            }),
        }
    }

    fn consume_token_err(&mut self, expected_type: TokenType) -> Result<Token, ParserError> {
        let result = match self.current_token() {
            token if token.token_type == expected_type => Ok(token),
            token => Err(ParserError {
                reason: format!("Expected {:?}", expected_type),
                token,
            }),
        };
        self.advance_token();
        result
    }

    /// Parses a prefix unary operator and advances token
    fn parse_unary_operator(&mut self) -> Result<PrefixExpression, ParserError> {
        let operator = self.current_token();

        match operator.token_type {
            TokenType::Bang | TokenType::Minus => {}
            _ => {
                return Err(ParserError {
                    reason: String::from("Unexpected token as prefix operator"),
                    token: operator,
                })
            }
        }

        self.advance_token();

        let expression = self.parse_expression(OperatorPrecendence::Prefix)?;

        Ok(PrefixExpression { operator, expression })
    }

    fn current_precedence(&self) -> OperatorPrecendence {
        self.current_token.token_type.token_precedence_map()
    }

    fn peek_precedence(&self) -> OperatorPrecendence {
        self.next_token.token_type.token_precedence_map()
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let token = self.current_token();
        let statement: Result<Statement, ParserError> = match token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::If => self.parse_if_statement().and_then(|v| Ok(Statement::If(v))),
            TokenType::LBrace => self.parse_block_statements().and_then(|v| Ok(Statement::Block(v))),
            TokenType::EOF => return None,
            _ => self.parse_expression_statement(),
        };

        match statement {
            Err(ParserError { reason, token }) => {
                panic!("Parser error: {} at {:?}", reason, token);
            }
            Ok(statement) => Some(statement),
        }
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement, ParserError> {
        let if_token = self.consume_token_err(TokenType::If)?;

        // Parse if condition
        self.consume_token_err(TokenType::LParen)?;
        let cond = self.parse_expression(OperatorPrecendence::Lowest)?;
        self.advance_token();
        self.consume_token_err(TokenType::RParen)?;

        // Parse if body
        self.match_token_err(TokenType::LBrace)?;
        let body = self.parse_block_statements()?;
        self.advance_token();

        Ok(IfStatement::new(if_token, cond, body))
    }

    fn parse_block_statements(&mut self) -> Result<BlockStatement, ParserError> {
        let brace_token = self.consume_token_err(TokenType::LBrace)?;
        let mut statements: Vec<Statement> = vec![];
        while self.match_token(TokenType::RBrace).is_none() && self.match_token(TokenType::EOF).is_none() {
            match self.parse_statement() {
                Some(statement) => statements.push(statement),
                None => break,
            }
        }
        self.match_token_err(TokenType::RBrace)?;

        Ok(BlockStatement::new(brace_token, statements))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.parse_expression(OperatorPrecendence::Lowest)?;
        self.advance_token();

        match self.match_token(TokenType::Semicolon) {
            Some(_) => self.advance_token(),
            None => {}
        }

        Ok(Statement::ExpresssionStatement(expr))
    }

    fn parse_expression(&mut self, precedence: OperatorPrecendence) -> Result<Expression, ParserError> {
        let mut left_expr = self.parse_prefix_position()?;

        while self.next_token.token_type != TokenType::Semicolon && precedence < self.peek_precedence() {
            self.advance_token();
            left_expr = match self.current_token.token_type {
                TokenType::LParen => Expression::FunctionCall(self.parse_call_function(left_expr)?),
                _ => self.parse_infix_position(left_expr)?,
            }
        }

        Ok(left_expr)
    }

    fn parse_prefix_position(&mut self) -> Result<Expression, ParserError> {
        let token = self.current_token();
        let result = match token.token_type {
            TokenType::Ident => Expression::Identifier(self.parse_ident()?),
            TokenType::Int => Expression::Integer(self.parse_integer()?),
            TokenType::True | TokenType::False => Expression::Boolean(self.parse_boolean()?),
            TokenType::Bang | TokenType::Minus => Expression::PrefixExpression(Box::new(self.parse_unary_operator()?)),
            TokenType::LParen => self.parse_grouped_expression()?,
            TokenType::Fn => Expression::Function(self.parse_function_literal()?),
            _ => {
                return Err(ParserError {
                    reason: String::from("Unexpected token in prefix position"),
                    token,
                })
            }
        };

        Ok(result)
    }

    fn parse_call_function(&mut self, fn_expression: Expression) -> Result<FunctionCall, ParserError> {
        let paren_token = self.consume_token_err(TokenType::LParen)?;
        let arguments = self.parse_call_function_arguments()?;

        Ok(FunctionCall::new(paren_token, fn_expression, arguments))
    }

    fn parse_call_function_arguments(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut arguments: Vec<Expression> = vec![];

        if let Some(_) = self.match_token(TokenType::RParen) {
            return Ok(arguments);
        }

        arguments.push(self.parse_expression(OperatorPrecendence::Lowest)?);

        loop {
            self.advance_token();
            match self.current_token.token_type {
                TokenType::RParen => break,
                TokenType::Comma => {
                    self.advance_token();
                    arguments.push(self.parse_expression(OperatorPrecendence::Lowest)?);
                }
                _ => {
                    return Err(ParserError {
                        token: self.current_token(),
                        reason: String::from("Unexpected token in function call arguments"),
                    })
                }
            };
        }

        self.match_token_err(TokenType::RParen)?;
        Ok(arguments)
    }

    fn parse_function_literal(&mut self) -> Result<Function, ParserError> {
        let fn_token = self.consume_token_err(TokenType::Fn)?;
        self.consume_token_err(TokenType::LParen)?;
        let params = self.parse_function_parameters()?;
        self.advance_token();
        let body = self.parse_block_statements()?;

        Ok(Function::new(fn_token, params, body))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Ident>, ParserError> {
        let mut parameters: Vec<Ident> = vec![];

        // Empty parameters case
        if let Some(_) = self.match_token(TokenType::RParen) {
            return Ok(parameters);
        }

        parameters.push(self.parse_ident()?);
        self.advance_token();

        loop {
            if let Some(_) = self.match_token(TokenType::RParen) {
                break;
            }

            self.consume_token_err(TokenType::Comma)?;
            parameters.push(self.parse_ident()?);
            self.advance_token();
        }

        self.match_token_err(TokenType::RParen)?;
        Ok(parameters)
    }

    fn parse_infix_position(&mut self, left_expression: Expression) -> Result<Expression, ParserError> {
        let infix_token = self.current_token();
        let infix_precedence = self.current_precedence();
        self.advance_token();

        let right_expression = self.parse_expression(infix_precedence)?;

        Ok(Expression::InfixExpression(Box::new(InfixExpression {
            operator: infix_token,
            left_expression,
            right_expression,
        })))
    }

    /// Parse a grouped statement ignoring previous precedence outside of parentheses
    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.consume_token_err(TokenType::LParen)?;
        let expr = self.parse_expression(OperatorPrecendence::Lowest)?;
        self.advance_token();
        self.match_token_err(TokenType::RParen)?;

        Ok(expr)
    }

    fn parse_ident(&mut self) -> Result<Ident, ParserError> {
        let token = self.match_token_err(TokenType::Ident)?;
        Ok(Ident(token.clone(), token.literal))
    }

    fn parse_integer(&mut self) -> Result<Integer, ParserError> {
        let token = self.match_token_err(TokenType::Int)?;
        let num = match token.literal.parse::<usize>() {
            Ok(num) => Integer(token, num),
            Err(e) => {
                return Err(ParserError {
                    token: token.clone(),
                    reason: format!("Cannot parse {:?} into an integer: {:?}", token, e),
                })
            }
        };

        Ok(num)
    }

    fn parse_boolean(&mut self) -> Result<Boolean, ParserError> {
        let token = self.current_token();

        match token.token_type {
            TokenType::True => Ok(Boolean(token, true)),
            TokenType::False => Ok(Boolean(token, false)),
            _ => Err(ParserError {
                token,
                reason: format!("Expected a boolean, got {:?}", self.current_token()),
            }),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.consume_token_err(TokenType::Return)?;

        let expr = self.parse_expression(OperatorPrecendence::Lowest)?;
        self.advance_token();

        match self.match_token(TokenType::Semicolon) {
            Some(_) => self.advance_token(),
            None => {}
        }

        Ok(Statement::Return(expr))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.consume_token_err(TokenType::Let)?;
        let ident = self.parse_ident()?;
        self.advance_token();

        self.consume_token_err(TokenType::Assign)?;

        let value = self.parse_expression(OperatorPrecendence::Lowest)?;
        self.advance_token();
        self.consume_token_err(TokenType::Semicolon)?;

        Ok(Statement::Let(ident, value))
    }
}

#[cfg(test)]
mod private_test {
    use crate::parser::ast::FunctionCall;

    use super::*;

    fn test_statements(input: &str, expected_ast: Vec<Statement>) -> () {
        let mut parser = Parser::new(Tokenizer::new(input));
        let Program { statements: actual } = parser.parse_program();

        assert_eq!(actual, expected_ast)
    }

    #[test]
    fn test_parse_let_statement() {
        let input = "let foo = bar; let foo = 5;";

        let expected: Vec<Statement> = vec![
            Statement::Let(
                Ident(Token::mock_ident("foo"), String::from("foo")),
                Expression::Identifier(Ident(Token::mock_ident("bar"), String::from("bar"))),
            ),
            Statement::Let(
                Ident(Token::mock_ident("foo"), String::from("foo")),
                Expression::Integer(Integer::mock(5)),
            ),
        ];

        test_statements(input, expected);
    }

    #[test]
    fn test_return_statement() {
        test_statements(
            "return foo;",
            vec![Statement::Return(Expression::Identifier(Ident(
                Token::mock_ident("foo"),
                String::from("foo"),
            )))],
        );

        test_statements(
            "return 5;",
            vec![Statement::Return(Expression::Integer(Integer::mock(5)))],
        );
    }

    #[test]
    fn test_parse_grouped_expression() {
        let input = "(5 * 3)";
        let expected: Vec<Statement> = vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
            InfixExpression::new(
                Token::new(TokenType::Asterisk, "*", 0, 0),
                Expression::Integer(Integer::mock(5)),
                Expression::Integer(Integer::mock(3)),
            ),
        )))];

        test_statements(input, expected);

        let input = "(5 * 3) * 2";
        let expected: Vec<Statement> = vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
            InfixExpression::new(
                Token::new(TokenType::Asterisk, "*", 0, 0),
                Expression::InfixExpression(Box::new(InfixExpression::new(
                    Token::new(TokenType::Asterisk, "*", 0, 0),
                    Expression::Integer(Integer::mock(5)),
                    Expression::Integer(Integer::mock(3)),
                ))),
                Expression::Integer(Integer::mock(2)),
            ),
        )))];

        test_statements(input, expected);
    }

    #[test]
    fn test_parse_let_statement_basic() {
        let input = "let foo = bar; let foo = 5;";

        let expected: Vec<Statement> = vec![
            Statement::Let(
                Ident(Token::mock_ident("foo"), String::from("foo")),
                Expression::Identifier(Ident(Token::mock_ident("bar"), String::from("bar"))),
            ),
            Statement::Let(
                Ident(Token::mock_ident("foo"), String::from("foo")),
                Expression::Integer(Integer::mock(5)),
            ),
        ];

        test_statements(input, expected);
    }

    #[test]
    fn test_parse_bang() {
        let input = "!5; !foo;";

        let expected: Vec<Statement> = vec![
            Statement::ExpresssionStatement(Expression::PrefixExpression(Box::new(PrefixExpression {
                operator: Token::new(TokenType::Bang, "!", 0, 0),
                expression: Expression::Integer(Integer::mock(5)),
            }))),
            Statement::ExpresssionStatement(Expression::PrefixExpression(Box::new(PrefixExpression {
                operator: Token::new(TokenType::Bang, "!", 0, 0),
                expression: Expression::Identifier(Ident(Token::mock_ident("foo"), String::from("foo"))),
            }))),
        ];

        test_statements(input, expected);
    }

    #[test]
    fn test_parse_minus() {
        let input = "-5; -foo; -bar;";

        let expected: Vec<Statement> = vec![
            Statement::ExpresssionStatement(Expression::PrefixExpression(Box::new(PrefixExpression {
                operator: Token::new(TokenType::Minus, "-", 0, 0),
                expression: Expression::Integer(Integer::mock(5)),
            }))),
            Statement::ExpresssionStatement(Expression::PrefixExpression(Box::new(PrefixExpression {
                operator: Token::new(TokenType::Minus, "-", 0, 0),
                expression: Expression::Identifier(Ident(Token::mock_ident("foo"), String::from("foo"))),
            }))),
            Statement::ExpresssionStatement(Expression::PrefixExpression(Box::new(PrefixExpression {
                operator: Token::new(TokenType::Minus, "-", 0, 0),
                expression: Expression::Identifier(Ident(Token::mock_ident("bar"), String::from("bar"))),
            }))),
        ];

        test_statements(input, expected);
    }

    #[test]
    fn test_multiple_sum() {
        let input = "5 + 3 - 2 + 8 + 1";

        let expected: Vec<Statement> = vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
            InfixExpression::new(
                Token::new(TokenType::Plus, "+", 0, 0),
                Expression::InfixExpression(Box::new(InfixExpression::new(
                    Token::new(TokenType::Plus, "+", 0, 0),
                    Expression::InfixExpression(Box::new(InfixExpression::new(
                        Token::new(TokenType::Minus, "-", 0, 0),
                        Expression::InfixExpression(Box::new(InfixExpression::new(
                            Token::new(TokenType::Plus, "+", 0, 0),
                            Expression::Integer(Integer::mock(5)),
                            Expression::Integer(Integer::mock(3)),
                        ))),
                        Expression::Integer(Integer::mock(2)),
                    ))),
                    Expression::Integer(Integer::mock(8)),
                ))),
                Expression::Integer(Integer::mock(1)),
            ),
        )))];

        test_statements(input, expected);
    }

    #[test]
    fn test_mixed_precedence() {
        let input1 = "5 + 3 * 2";
        let expected: Vec<Statement> = vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
            InfixExpression::new(
                Token::new(TokenType::Plus, "+", 0, 0),
                Expression::Integer(Integer::mock(5)),
                Expression::InfixExpression(Box::new(InfixExpression::new(
                    Token::new(TokenType::Asterisk, "*", 0, 0),
                    Expression::Integer(Integer::mock(3)),
                    Expression::Integer(Integer::mock(2)),
                ))),
            ),
        )))];

        test_statements(input1, expected);

        let input2 = "2 * -(5 + 3);";
        let expected: Vec<Statement> = vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
            InfixExpression::new(
                Token::new(TokenType::Asterisk, "*", 0, 0),
                Expression::Integer(Integer::mock(2)),
                Expression::PrefixExpression(Box::new(PrefixExpression::new(
                    Token::new(TokenType::Minus, "-", 0, 0),
                    Expression::InfixExpression(Box::new(InfixExpression::new(
                        Token::new(TokenType::Plus, "+", 0, 0),
                        Expression::Integer(Integer::mock(5)),
                        Expression::Integer(Integer::mock(3)),
                    ))),
                ))),
            ),
        )))];

        test_statements(input2, expected);
    }

    #[test]
    fn test_infix_basic() {
        let input = "5 + 3; 4 - 2; foo + bar; foo - bar; foo + 3; foo - 3;";

        let expected: Vec<Statement> = vec![
            Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(InfixExpression {
                operator: Token::new(TokenType::Plus, "+", 0, 0),
                left_expression: Expression::Integer(Integer::mock(5)),
                right_expression: Expression::Integer(Integer::mock(3)),
            }))),
            Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(InfixExpression {
                operator: Token::new(TokenType::Minus, "-", 0, 0),
                left_expression: Expression::Integer(Integer::mock(4)),
                right_expression: Expression::Integer(Integer::mock(2)),
            }))),
            Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(InfixExpression {
                operator: Token::new(TokenType::Plus, "+", 0, 0),
                left_expression: Expression::Identifier(Ident(Token::mock_ident("foo"), String::from("foo"))),
                right_expression: Expression::Identifier(Ident(Token::mock_ident("bar"), String::from("bar"))),
            }))),
            Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(InfixExpression {
                operator: Token::new(TokenType::Minus, "-", 0, 0),
                left_expression: Expression::Identifier(Ident(Token::mock_ident("foo"), String::from("foo"))),
                right_expression: Expression::Identifier(Ident(Token::mock_ident("bar"), String::from("bar"))),
            }))),
            Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(InfixExpression {
                operator: Token::new(TokenType::Plus, "+", 0, 0),
                left_expression: Expression::Identifier(Ident(Token::mock_ident("foo"), String::from("foo"))),
                right_expression: Expression::Integer(Integer::mock(3)),
            }))),
            Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(InfixExpression {
                operator: Token::new(TokenType::Minus, "-", 0, 0),
                left_expression: Expression::Identifier(Ident(Token::mock_ident("foo"), String::from("foo"))),
                right_expression: Expression::Integer(Integer::mock(3)),
            }))),
        ];

        test_statements(input, expected);
    }

    #[test]
    fn test_function_call_ast() {
        let input = "add_numbers(1 + 2, foo);";

        let expected: Vec<Statement> = vec![Statement::ExpresssionStatement(Expression::FunctionCall(
            FunctionCall::new(
                Token::mock_left_paren(),
                Expression::Identifier(Ident::mock("add_numbers")),
                vec![
                    Expression::InfixExpression(Box::new(InfixExpression {
                        operator: Token::new(TokenType::Plus, "+", 0, 0),
                        left_expression: Expression::Integer(Integer::mock(1)),
                        right_expression: Expression::Integer(Integer::mock(2)),
                    })),
                    Expression::Identifier(Ident::mock("foo")),
                ],
            ),
        ))];

        test_statements(input, expected);
    }

    #[test]
    fn test_function_call_in_infix_expression() {
        let input = "4 + add_numbers(1, 2) * 3;";

        let expected: Vec<Statement> = vec![Statement::ExpresssionStatement(Expression::InfixExpression(Box::new(
            InfixExpression::new(
                Token::mock_plus(),
                Expression::Integer(Integer::mock(4)),
                Expression::InfixExpression(Box::new(InfixExpression::new(
                    Token::mock_asterisk(),
                    Expression::FunctionCall(FunctionCall::new(
                        Token::mock_left_paren(),
                        Expression::Identifier(Ident::mock("add_numbers")),
                        vec![
                            Expression::Integer(Integer::mock(1)),
                            Expression::Integer(Integer::mock(2)),
                        ],
                    )),
                    Expression::Integer(Integer::mock(3)),
                ))),
            ),
        )))];

        let mut parser = Parser::new(Tokenizer::new(input));
        let Program { statements: actual } = parser.parse_program();

        assert_eq!(actual[0], expected[0]);
    }
}
