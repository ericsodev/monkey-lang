use crate::{
    lexer::tokenizer::TokenType,
    parser::ast::{Boolean, Expression, InfixExpression, Integer, PrefixExpression, Statement},
};

use super::object::Object;

fn evaluate_bang_prefix_expression(value: Object) -> Object {
    match value {
        Object::Boolean(val) => Object::Boolean(!val),
        _ => Object::Null,
    }
}

fn evaluate_minus_prefix_expression(value: Object) -> Object {
    match value {
        Object::Integer(val) => Object::Integer(-val),
        _ => Object::Null,
    }
}

impl Expression {
    fn evaluate(&self) -> Object {
        match self {
            Expression::Integer(Integer(_, value)) => Object::Integer(value.to_owned()),
            Expression::Boolean(Boolean(_, value)) => Object::Boolean(value.to_owned()),
            Expression::PrefixExpression(expr) => {
                let PrefixExpression { operator, expression } = expr.as_ref();
                let val = expression.evaluate();

                match operator.token_type {
                    TokenType::Bang => evaluate_bang_prefix_expression(val),
                    TokenType::Minus => evaluate_minus_prefix_expression(val),
                    _ => Object::Null,
                }
            }
            Expression::InfixExpression(expr) => {
                let InfixExpression {
                    left_expression: left_expr,
                    right_expression: right_expr,
                    operator,
                } = expr.as_ref();
                if let (Object::Integer(left_val), Object::Integer(right_val)) =
                    (left_expr.evaluate(), right_expr.evaluate())
                {
                    return match operator.token_type {
                        TokenType::Plus => Object::Integer(left_val + right_val),
                        TokenType::Minus => Object::Integer(left_val - right_val),
                        TokenType::Asterisk => Object::Integer(left_val * right_val),
                        TokenType::Slash => Object::Integer(left_val / right_val),
                        _ => Object::Null,
                    };
                }
                Object::Null
            }
            _ => todo!(),
        }
    }
}

impl Statement {
    fn evaluate(&self) -> Object {
        match self {
            Statement::ExpresssionStatement(expr) => expr.evaluate(),
            _ => todo!(),
        }
    }
}

pub fn eval_program(program: Vec<Statement>) -> Object {
    let mut result = Object::Null;
    for statement in program.into_iter() {
        result = statement.evaluate()
    }
    result
}
