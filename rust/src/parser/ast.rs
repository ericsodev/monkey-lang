use std::{
    fmt::{Debug, Display},
    iter,
};

use crate::lexer::tokenizer::{Token, TokenType};

#[derive(PartialEq, PartialOrd)]
pub enum OperatorPrecendence {
    Lowest = 0,
    Equals = 1,
    LesserGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

pub trait PrecedenceMap {
    fn token_precedence_map(&self) -> OperatorPrecendence;
}

impl PrecedenceMap for TokenType {
    fn token_precedence_map(&self) -> OperatorPrecendence {
        match self {
            TokenType::Eq | TokenType::Neq => OperatorPrecendence::Equals,
            TokenType::Lt | TokenType::Gt => OperatorPrecendence::LesserGreater,
            TokenType::Plus | TokenType::Minus => OperatorPrecendence::Sum,
            TokenType::Asterisk | TokenType::Slash => OperatorPrecendence::Product,
            _ => OperatorPrecendence::Lowest,
        }
    }
}

impl PrecedenceMap for Token {
    fn token_precedence_map(&self) -> OperatorPrecendence {
        match self.token_type {
            TokenType::Eq | TokenType::Neq => OperatorPrecendence::Equals,
            TokenType::Lt | TokenType::Gt => OperatorPrecendence::LesserGreater,
            TokenType::Plus | TokenType::Minus => OperatorPrecendence::Sum,
            TokenType::Asterisk | TokenType::Slash => OperatorPrecendence::Product,
            _ => OperatorPrecendence::Lowest,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Ident(pub Token, pub String);

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.1)
    }
}

#[derive(Clone, PartialEq)]
pub struct Integer(pub Token, pub usize);

impl Debug for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.1)
    }
}

#[derive(Clone, PartialEq)]
pub struct Boolean(pub Token, pub bool);

impl Boolean {
    pub fn new(token: Token, val: bool) -> Self {
        Boolean(token, val)
    }
}

impl Debug for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.1)
    }
}

#[derive(Clone, PartialEq)]
pub struct PrefixExpression {
    pub operator: Token,
    pub expression: Expression,
}

impl PrefixExpression {
    pub fn new(operator: Token, expression: Expression) -> PrefixExpression {
        return PrefixExpression { operator, expression };
    }
}

impl Debug for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let PrefixExpression { operator, expression } = self;

        let mut body = format!("{}\nExpression: {:?}", operator.literal, expression);
        body = format_tabbed(&body);

        writeln!(f, "Prefix Expression\n{}", body)
    }
}

#[derive(Clone, PartialEq)]
pub struct InfixExpression {
    pub operator: Token,
    pub left_expression: Expression,
    pub right_expression: Expression,
}

impl InfixExpression {
    pub fn new(operator: Token, lhs: Expression, rhs: Expression) -> InfixExpression {
        return InfixExpression {
            operator,
            left_expression: lhs,
            right_expression: rhs,
        };
    }
}

impl Debug for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let InfixExpression {
            operator,
            right_expression,
            left_expression,
        } = self;

        let lhs = format_tabbed(&format!("{:?}", left_expression));
        let rhs = format_tabbed(&format!("{:?}", right_expression));

        let mut body = format!("{}\nLHS:\n{}\nRHS:\n{}", operator.literal, lhs, rhs);
        body = format_tabbed(&body);

        writeln!(f, "Infix Expression\n{}", body)
    }
}

#[derive(Clone, PartialEq)]
pub struct Function {
    params: Vec<Ident>,
    body: BlockStatement,
}

impl Function {
    pub fn new(params: Vec<Ident>, body: BlockStatement) -> Function {
        Function { params, body }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: &str = &self
            .params
            .iter()
            .map(|v| -> &str { &v.1 })
            .collect::<Vec<&str>>()
            .join(", ");

        let body: String = format!("{:?}", self.body);
        let details = format!("params: {}\nbody:{}", format_tabbed(params), format_tabbed(&body));
        write!(f, "Function\n{}", format_tabbed(&details))
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: &str = &self
            .params
            .iter()
            .map(|v| -> &str { &v.1 })
            .collect::<Vec<&str>>()
            .join(", ");

        let body: String = format!("{}", self.body);
        write!(f, "fn ({}) {{\n{}\n}}", params, format_tabbed(&body))
    }
}

#[derive(Clone, PartialEq)]
pub enum Expression {
    Integer(Integer),
    Identifier(Ident),
    Boolean(Boolean),
    PrefixExpression(Box<PrefixExpression>),
    InfixExpression(Box<InfixExpression>),
    Function(Function),
}

fn format_tabbed(str: &str) -> String {
    let mut tabbed = String::new();
    for line in str.lines() {
        tabbed.push_str(&format!("  {}\n", line));
    }

    tabbed.pop(); // Remove last new line
    tabbed
}

fn format_tabbed_n(str: &str, n: usize) -> String {
    let mut tabbed = String::new();
    let prefix: String = iter::repeat(" ").take(n).collect();
    for line in str.lines() {
        tabbed.push_str(&format!("{}{}\n", prefix, line));
    }

    tabbed.pop(); // Remove last new line
    tabbed
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Expression::Identifier(ident) => format!("Ident: {:?}", ident),
            Expression::Integer(val) => format!("Integer: {:?}", val),
            Expression::Boolean(val) => format!("Boolean: {:?}", val),
            Expression::PrefixExpression(expr) => return expr.fmt(f),
            Expression::InfixExpression(expr) => return expr.fmt(f),
            Expression::Function(func) => return Debug::fmt(&func, f),
        };
        write!(f, "{}", &str)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str: String = match self {
            Expression::Identifier(Ident(_, val)) => val.to_string(),
            Expression::Integer(Integer(_, val)) => val.to_string(),
            Expression::Boolean(Boolean(_, val)) => val.to_string(),
            Expression::PrefixExpression(expr) => {
                format!("{} {}", expr.operator.literal, expr.expression)
            }
            Expression::InfixExpression(expr) => {
                format!(
                    "({} {} {})",
                    expr.left_expression, expr.operator.literal, expr.right_expression
                )
            }
            Expression::Function(func) => format!("{}", func),
        };
        write!(f, "{}", str)
    }
}

#[derive(Clone, PartialEq)]
pub enum Statement {
    Let(Ident, Expression),
    Return(Expression),
    ExpresssionStatement(Expression),
    If(IfStatement),
    Block(BlockStatement),
}

#[derive(Clone, PartialEq)]
pub struct BlockStatement(pub Token, Vec<Statement>);

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        BlockStatement(token, statements)
    }
}

impl Debug for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut lines = String::new();
        for statement in &self.1 {
            lines.push_str(&format!("{:?}", statement));
        }

        lines.pop();
        write!(f, "{}", lines)
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut lines = String::new();
        for statement in &self.1 {
            lines.push_str(&format!("\t{}\n", statement));
        }

        lines.pop();
        write!(f, "{{\n{}\n}}", lines)
    }
}

#[derive(Clone, PartialEq)]
pub struct IfStatement(pub Token, pub Expression, pub BlockStatement);

impl IfStatement {
    pub fn new(token: Token, condition: Expression, block: BlockStatement) -> Self {
        Self(token, condition, block)
    }
}

impl Debug for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let IfStatement(_, cond, block) = self;
        let cond_str = format_tabbed(&format!("{:?}", cond));
        let block_str = format_tabbed(&format!("{:?}", block));
        write!(f, "if\ncond:\n{}\nblock:\n{}", cond_str, block_str)
    }
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let IfStatement(_, cond, block) = self;
        write!(f, "if ({}) {}", cond, block)
    }
}

impl Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::ExpresssionStatement(expr) => write!(f, "{:?}", expr),
            Statement::If(v) => write!(f, "{:?}", v),
            Statement::Block(block) => write!(f, "{:?}", block),
            Statement::Let(ident, expr) => write!(f, "Let\n{:?}\n{:?}", ident.0, expr),
            Statement::Return(expr) => write!(f, "Return\n{:?}", expr),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::ExpresssionStatement(expr) => write!(f, "{};", expr),
            Statement::If(v) => write!(f, "{}", v),
            Statement::Block(block) => write!(f, "{}", block),
            Statement::Let(ident, expr) => write!(f, "let {} = {};", ident.1, expr),
            Statement::Return(expr) => write!(f, "return {};", expr),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();

        for statement in &self.statements {
            str.push_str(&format!("{}\n", statement));
        }

        str.pop();
        write!(f, "{}", str)
    }
}

impl Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut lines = String::new();
        for statement in &self.statements {
            lines.push_str(&format!("{:?}\n", statement));
        }

        lines.pop();
        write!(f, "{}", lines)
    }
}

#[derive(Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
