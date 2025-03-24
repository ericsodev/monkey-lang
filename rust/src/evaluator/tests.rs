#[cfg(test)]
mod tests {
    use crate::evaluator::evaluator::eval_program;
    use crate::evaluator::object::Object;
    use crate::{lexer::tokenizer::Tokenizer, parser::parser::Parser};

    fn eval_input(input: &str) -> Object {
        let lexer = Tokenizer::new(input);
        let mut parser = Parser::new(lexer);

        eval_program(parser.parse_program().statements)
    }

    #[test]
    fn test_integer_object() {
        assert_eq!(eval_input("5"), Object::Integer(5));
        assert_eq!(eval_input("3"), Object::Integer(3));
    }

    #[test]
    fn test_boolean_object() {
        assert_eq!(eval_input("true"), Object::Boolean(true));
        assert_eq!(eval_input("false"), Object::Boolean(false));
    }
}
