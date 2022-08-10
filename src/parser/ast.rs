use std::borrow::Borrow;
use crate::Token;

pub trait Node {
    fn literal(&self) -> String;
}

pub trait Statement: Node {
    fn statement_node(&self) {}
}

pub trait Expression: Node {
    fn expression_node(&self) {}
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            statements: vec![]
        }
    }
}

impl Node for Program {
    fn literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements.get(0).unwrap().literal();
        }
        "".to_string()
    }
}

pub struct LetStatement {
    pub(crate) token: Token,
    pub(crate) name: Identifier,
    pub(crate) value: Option<Box<dyn Expression>>,
}

impl Node for LetStatement {
    fn literal(&self) -> String {
        format!("{} {}", self.token.literal(), self.name.literal())
    }
}

impl Statement for LetStatement {}

pub struct Identifier {
    pub(crate) token: Token,
}

impl Node for Identifier {
    fn literal(&self) -> String {
        self.token.literal()
    }
}

impl Expression for Identifier {}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {}

impl Node for ReturnStatement {
    fn literal(&self) -> String {
        format!("{}", self.token.literal())
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: dyn Expression,
}

impl Statement for ExpressionStatement {}
impl Node for ExpressionStatement{
    fn literal(&self) -> String {
        self.token.literal()
    }
}