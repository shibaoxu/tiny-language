//
//                          |-- LetStatement
//     |------ Statement--- |-- ExpressionStatement
//     |                    |-- ReturnStatement
//     |                    |-- BlockStatement
//     |
// Node|----- Program
//     |                    |-- Identifier
//     |                    |-- BooleanLiteral
//     |                    |-- IntegerLiteral
//     |----- Expression -- |-- PrefixExpression
//                          |-- InfixExpression
//                          |-- IfExpression
//                          |-- FunctionLiteral
//                          |-- CallExpression
//
use std::any::Any;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::lexer::token::Token;

pub trait Node: Display + Any {
    fn token_literal(&self) -> String;
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

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{}", stmt.to_string())?;
        }
        Ok(())
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self.statements.get(0).unwrap().token_literal();
        }
        "".to_string()
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = {};", self.token.literal, self.name.to_string(), self.value.as_ref().to_string())
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for LetStatement {}

#[derive(Clone)]
pub struct Identifier {
    pub token: Token,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }
}

impl Expression for Identifier {}

pub struct ReturnStatement {
    pub token: Token,
    pub value: Box<dyn Expression>,
}

impl Statement for ReturnStatement {}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token.literal, self.value.as_ref().to_string())
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl Statement for ExpressionStatement {}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        // self.token.literal.clone()
        format!("{:?}", self.token)
    }
}
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }
}

impl Expression for IntegerLiteral {}

pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }
}

impl Expression for PrefixExpression {}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left.to_string(), self.operator.to_string(), self.right.to_string())
    }
}

impl Expression for InfixExpression {}

pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> String {
        format!("{:?}", self.token)
    }
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Expression for BooleanLiteral {}

pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        format!("{}", self.token.literal)
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition.to_string(), self.consequence.to_string())?;
        if self.alternative.is_some() {
            write!(f, " else {}", self.alternative.as_ref().unwrap().to_string())?;
        }
        Ok(())
    }
}

impl Expression for IfExpression {}

pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        format!("{}", self.token.literal)
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("{")?;
        for stmt in self.statements.iter() {
            write!(f, "{}", stmt.to_string())?;
        }
        f.write_str("}")?;
        Ok(())
    }
}

impl Statement for BlockStatement {}

pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Rc<BlockStatement>,
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let paras = self.parameters.iter().map(|e| e.token.literal.clone()).collect::<Vec<_>>().join(",");
        write!(f, "{} ({}) {}", self.token.literal, paras, self.body)
    }
}

impl Expression for FunctionLiteral {}

pub struct CallExpression {
    pub token: Token,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arguments = self.arguments.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(",");
        write!(f, "{}({})", self.function, arguments)
    }
}

impl Expression for CallExpression {}

#[cfg(test)]
mod tests {
    use crate::ast::{Identifier, LetStatement, Program, ReturnStatement};
    use crate::lexer::token::Token;

    #[test]
    fn test_to_string_work() {
        let program = Program {
            statements: vec![
                Box::new(LetStatement {
                    token: Token::new_let(),
                    name: Identifier { token: Token::new_identity("my_var") },
                    value: Box::new(Identifier { token: Token::new_identity("another_var") }),
                }),
                Box::new(ReturnStatement {
                    token: Token::new_return(),
                    value: Box::new(Identifier { token: Token::new_identity("result") }),
                }),
            ]
        };
        assert_eq!(program.to_string(),
                   "let my_var = another_var;return result;");
    }
}
