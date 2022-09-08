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

use std::fmt::{Display, Formatter};

use crate::lexer::token::Token;

pub trait Node: Display {
    fn token_literal(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    LetStmt(LetStatement),
    ReturnStmt(ReturnStatement),
    BlockStmt(BlockStatement),
    ExpStmt(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStmt(stmt) => f.write_str(&stmt.token_literal()),
            Statement::ReturnStmt(stmt) => f.write_str(&stmt.token_literal()),
            Statement::BlockStmt(stmt) => f.write_str(&stmt.token_literal()),
            Statement::ExpStmt(stmt) => f.write_str(&stmt.token_literal()),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    BoolLiteral(BooleanLiteral),
    IntLiteral(IntegerLiteral),
    StrLiteral(StringLiteral),
    PrefixExpr(PrefixExpression),
    InfixExpr(InfixExpression),
    IfExpr(IfExpression),
    FuncLiteral(FunctionLiteral),
    CallExpr(CallExpression),
    ArrLiteral(ArrayLiteral),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(expr) => f.write_str(&expr.token_literal()),
            Expression::BoolLiteral(expr) => f.write_str(&expr.token_literal()),
            Expression::IntLiteral(expr) => f.write_str(&expr.token_literal()),
            Expression::StrLiteral(expr) => f.write_str(&expr.token_literal()),
            Expression::PrefixExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::InfixExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::IfExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::FuncLiteral(expr) => f.write_str(&expr.token_literal()),
            Expression::CallExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::ArrLiteral(expr) => f.write_str(&expr.token_literal())
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
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

impl Node for Program {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = {};", self.token.literal, self.name.to_string(), self.value.to_string())
    }
}

impl Node for LetStatement {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

impl Node for ReturnStatement {}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token.literal, self.value.to_string())
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
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

impl Node for BlockStatement {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifier {
    pub token: Token,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl Node for Identifier {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl Node for IntegerLiteral {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.token.literal)
    }
}

impl Node for StringLiteral {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}


impl Node for PrefixExpression {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left.to_string(), self.operator.to_string(), self.right.to_string())
    }
}

impl Node for InfixExpression {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for BooleanLiteral {}

// impl Expr for BooleanLiteral {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<Box<BlockStatement>>,
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

impl Node for IfExpression {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let paras = self.parameters.iter().map(|e| e.token.literal.clone()).collect::<Vec<_>>().join(",");
        write!(f, "{} ({}) {}", self.token.literal, paras, self.body)
    }
}

impl Node for FunctionLiteral {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Box<Expression>>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arguments = self.arguments.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(",");
        write!(f, "{}({})", self.function, arguments)
    }
}

impl Node for CallExpression {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Box<Expression>>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let elements = self.elements.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(",");
        f.write_fmt(format_args!("[{}]", elements))
    }
}

impl Node for ArrayLiteral {}