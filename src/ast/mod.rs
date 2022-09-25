use std::fmt::{Display, Formatter};
use std::hash::Hash;
use crate::evaluator::environment::Environment;

use crate::lexer::token::Token;

pub trait Node: Display {
    fn token_literal(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Expression {
    IdentExpr(Identifier),
    BoolExpr(BooleanLiteral),
    IntExpr(IntegerLiteral),
    StrExpr(StringLiteral),
    PrefixExpr(PrefixExpression),
    InfixExpr(InfixExpression),
    IfExpr(IfExpression),
    FuncExpr(FunctionLiteral),
    MacroExpr(FunctionLiteral),
    CallExpr(CallExpression),
    ArrayExpr(ArrayLiteral),
    IndexExpr(IndexExpression),
    HashMapExpr(HashmapLiteral),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IdentExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::BoolExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::IntExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::StrExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::PrefixExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::InfixExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::IfExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::FuncExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::MacroExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::CallExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::ArrayExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::IndexExpr(expr) => f.write_str(&expr.token_literal()),
            Expression::HashMapExpr(expr) => f.write_str(&expr.token_literal()),
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::IdentExpr(v) => v.token_literal(),
            Expression::BoolExpr(v) => v.token_literal(),
            Expression::IntExpr(v) => v.token_literal(),
            Expression::StrExpr(v) => v.token_literal(),
            Expression::PrefixExpr(v) => v.token_literal(),
            Expression::InfixExpr(v) => v.token_literal(),
            Expression::IfExpr(v) => v.token_literal(),
            Expression::FuncExpr(v) => v.token_literal(),
            Expression::MacroExpr(v) => v.token_literal(),
            Expression::CallExpr(v) => v.token_literal(),
            Expression::ArrayExpr(v) => v.token_literal(),
            Expression::IndexExpr(v) => v.token_literal(),
            Expression::HashMapExpr(v) => v.token_literal(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} = {};", self.token, self.name, self.value)
    }
}

impl Node for LetStatement {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ReturnStatement {
    pub token: Token,
    pub value: Expression,
}

impl Node for ReturnStatement {}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token, self.value)
    }
}


#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("{")?;
        for stmt in self.statements.iter() {
            write!(f, "{}", stmt)?;
        }
        f.write_str("}")?;
        Ok(())
    }
}

impl Node for BlockStatement {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Identifier {
    pub token: Token,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

impl Node for Identifier {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token)
    }
}

impl Node for IntegerLiteral {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.token.to_string())
        // write!(f, "\"{}\"", self.token.to_string())
    }
}

impl Node for StringLiteral {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl Node for InfixExpression {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<Box<BlockStatement>>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        if self.alternative.is_some() {
            write!(f, " else {}", self.alternative.as_ref().unwrap())?;
        }
        Ok(())
    }
}

impl Node for IfExpression {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Expression>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let paras = self.parameters.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(",");
        write!(f, "{} ({}) {}", self.token, paras, self.body)
    }
}

impl Node for FunctionLiteral {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct IndexExpression {
    pub token: Token,
    pub name: Box<Expression>,
    pub index: Box<Expression>,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("({}[{}])", self.name, self.index))
    }
}

impl Node for IndexExpression {}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct HashmapLiteral {
    pub token: Token,
    pub pairs: Vec<(Expression, Expression)>,
}

impl Display for HashmapLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = self.pairs.iter().map(|e| format!("{} : {}", e.0, e.1)).collect::<Vec<String>>().join(",");
        write!(f, "{{{}}}", v)
    }
}

impl Node for HashmapLiteral {}

pub fn modify(node: &Expression, env: &Environment, modifier: fn(&Expression, &Environment) -> Expression) -> Expression {
    match node {
        Expression::InfixExpr(expr) => {
            Expression::InfixExpr(InfixExpression {
                token: expr.token.clone(),
                left: Box::new(modify(expr.left.as_ref(), env, modifier)),
                operator: expr.operator.clone(),
                right: Box::new(modify(expr.right.as_ref(), env, modifier)),
            })
        }
        Expression::IfExpr(expr) => {
            Expression::IfExpr(IfExpression {
                token: expr.token.clone(),
                condition: Box::new(modify(expr.condition.as_ref(), env, modifier)),
                consequence: modify_block_statement(&expr.consequence, env, modifier),
                alternative: expr.alternative.as_ref().map(|e| Box::new(modify_block_statement(e.as_ref(), env, modifier))),
            })
        }
        Expression::FuncExpr(expr) => {
            Expression::FuncExpr(FunctionLiteral {
                token: expr.token.clone(),
                parameters: expr.parameters.iter().map(|e| modify(&e, env, modifier)).collect(),
                body: modify_block_statement(&expr.body, env, modifier),
            })
        }
        Expression::ArrayExpr(expr) => {
            Expression::ArrayExpr(ArrayLiteral {
                token: expr.token.clone(),
                elements: expr.elements.iter().map(|e| Box::new(modify(e.as_ref(), env, modifier))).collect::<Vec<_>>(),
            })
        }

        Expression::HashMapExpr(expr) => {
            Expression::HashMapExpr(HashmapLiteral {
                token: expr.token.clone(),
                pairs: expr.pairs.iter().map(|e| (modify(&e.0, env, modifier), modify(&e.1, env, modifier))).collect(),
            })
        }
        Expression::PrefixExpr(expr) => {
            Expression::PrefixExpr(PrefixExpression {
                token: expr.token.clone(),
                operator: expr.operator.clone(),
                right: Box::new(modify(expr.right.as_ref(), env, modifier)),
            })
        }
        Expression::IndexExpr(expr) => {
            Expression::IndexExpr(IndexExpression {
                token: expr.token.clone(),
                name: Box::new(modify(expr.name.as_ref(), env, modifier)),
                index: Box::new(modify(expr.name.as_ref(), env, modifier)),
            })
        }
        // Expression::CallExpr(expr) => {
        //     println!("4.{:?}", expr);
        //     Expression::CallExpr(CallExpression {
        //         token: expr.token.clone(),
        //         function: Box::new(modify(expr.function.as_ref(), env, modifier)),
        //         arguments: expr.arguments.iter().map(|e| Box::new(modify(e.as_ref(), env, modifier))).collect(),
        //     })
        // }
        // Expression::IdentExpr(expr_)=>{}
        // Expression::BoolExpr(_) => {}
        // Expression::IntExpr(_) => {}
        // Expression::StrExpr(_) => {}
        _ => {
            modifier(node, env)
        }
    }
}

fn modify_statement(node: &Statement, env: &Environment, modifier: fn(&Expression, &Environment) -> Expression) -> Statement {
    match node {
        Statement::LetStmt(stmt) => {
            Statement::LetStmt(LetStatement {
                token: stmt.token.clone(),
                name: stmt.name.clone(),
                value: modify(&stmt.value, env, modifier),
            })
        }

        Statement::ReturnStmt(stmt) => {
            Statement::ReturnStmt(ReturnStatement {
                token: stmt.token.clone(),
                value: modify(&stmt.value, env, modifier),
            })
        }
        Statement::BlockStmt(stmt) => {
            Statement::BlockStmt(
                modify_block_statement(stmt, env, modifier)
            )
        }
        Statement::ExpStmt(stmt) => {
            Statement::ExpStmt(
                ExpressionStatement {
                    token: stmt.token.clone(),
                    expression: modify(&stmt.expression, env, modifier),
                }
            )
        }
    }
}

fn modify_block_statement(stmt: &BlockStatement, env: &Environment, modifier: fn(&Expression, &Environment) -> Expression) -> BlockStatement {
    let stmts = stmt.statements.iter().map(|e| modify_statement(e, env, modifier.clone())).collect::<Vec<_>>();
    BlockStatement {
        token: stmt.token.clone(),
        statements: stmts,
    }
}

pub fn modify_program(program: &Program, env: &Environment, modifier: fn(&Expression, &Environment) -> Expression) -> Program {
    Program {
        statements: program.statements.iter().map(|e| modify_statement(e, env, modifier)).collect()
    }
}