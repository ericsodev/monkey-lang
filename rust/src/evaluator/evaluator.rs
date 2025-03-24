use crate::parser::ast::{Boolean, Expression, Integer, Statement};

use super::object::Object;

impl Expression {
    fn evaluate(self) -> Object {
        match self {
            Expression::Integer(Integer(_, value)) => Object::Integer(value),
            Expression::Boolean(Boolean(_, value)) => Object::Boolean(value),
            _ => todo!(),
        }
    }
}

impl Statement {
    fn evaluate(self) -> Object {
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
