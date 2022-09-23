use std::collections::HashMap;
use std::io::{BufRead, BufReader, Cursor, Seek};
use std::str::FromStr;
use crate::Lexer;

use thiserror::Error;
use anyhow::Result;
use tracing::{error, info};

use crate::ast::*;
use crate::ast::Expression::BoolExpr;
use crate::lexer::token::Token;
use crate::parser::ParsingError::{NoInfixParseFun, NoPrefixParseFun, WrongHashmapSyntax};

#[derive(Error, Debug, Clone)]
pub enum ParsingError {
    #[error("invalid peek token: expected is {expected:?} actual is {actual:?}")]
    PeekError {
        expected: Token,
        actual: Token,
    },
    #[error("no prefix parse function for {0:?}")]
    NoPrefixParseFun(Token),
    #[error("no infix parse function for {0:?}")]
    NoInfixParseFun(Token),
    #[error("expect a right express for {0:?}")]
    ExpectRightExpress(Token),
    #[error("expected a valid expression")]
    ExpectValidExpression,
    #[error("{0}")]
    UnknownError(String),
    #[error("wrong hashmap syntax")]
    WrongHashmapSyntax,
}

#[derive(Debug, Copy, Clone)]
enum Precedence {
    Lowest = 0,
    Equal,
    //==
    LessOrGreater,
    //> or <
    Sum,
    //+
    Product,
    //*
    Prefix,
    //-,!
    Call,
    //fn
    Index, //arr[index]
}

pub struct Parser<T: BufRead + Seek> {
    lexer: Lexer<T>,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<ParsingError>,
    precedence: HashMap<Token, Precedence>,
    prefix_parse_fns: HashMap<Token, fn(&mut Self) -> Result<Expression>>,
    infix_parse_fns: HashMap<Token, fn(&mut Self, Expression) -> Result<Expression>>,
}

impl<T: BufRead + Seek> Parser<T> {
    pub fn from_buf_reader(reader: T) -> Self {
        let lexer = Lexer::from_buf_reader(reader);
        Self::from_lexer(lexer)
    }

    pub fn from_lexer(mut lexer: Lexer<T>) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let mut parser = Self {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
            precedence: HashMap::from([
                (Token::EQ, Precedence::Equal),
                (Token::NotEq, Precedence::Equal),
                (Token::LT, Precedence::LessOrGreater),
                (Token::GT, Precedence::LessOrGreater),
                (Token::Plus, Precedence::Sum),
                (Token::Minus, Precedence::Sum),
                (Token::Slash, Precedence::Product),
                (Token::Asterisk, Precedence::Product),
                (Token::LParen, Precedence::Call),
                (Token::LBracket, Precedence::Index),
            ]),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        parser.init();
        parser
    }

    fn init(&mut self) {
        self.register_prefix(Token::Identity(String::from("_")), Self::parse_identifier);
        self.register_prefix(Token::Int(0), Self::parse_integer_literal);
        self.register_prefix(Token::String(String::from("_")), Self::parse_string_literal);
        self.register_prefix(Token::Bang, Self::parse_prefix_expression);
        self.register_prefix(Token::Minus, Self::parse_prefix_expression);
        self.register_prefix(Token::True, Self::parse_boolean_literal);
        self.register_prefix(Token::False, Self::parse_boolean_literal);
        self.register_prefix(Token::LParen, Self::parse_grouped_expression);
        self.register_prefix(Token::LBracket, Self::parse_array_literal);
        self.register_prefix(Token::LBrace, Self::parse_hashmap_literal);
        self.register_prefix(Token::If, Self::parse_if_expression);
        self.register_prefix(Token::Function, Self::parse_function_literal);

        self.register_infix(Token::Plus, Self::parse_infix_expression);
        self.register_infix(Token::Minus, Self::parse_infix_expression);
        self.register_infix(Token::Slash, Self::parse_infix_expression);
        self.register_infix(Token::Asterisk, Self::parse_infix_expression);
        self.register_infix(Token::EQ, Self::parse_infix_expression);
        self.register_infix(Token::NotEq, Self::parse_infix_expression);
        self.register_infix(Token::LT, Self::parse_infix_expression);
        self.register_infix(Token::GT, Self::parse_infix_expression);
        self.register_infix(Token::LParen, Self::parse_call_expression);
        self.register_infix(Token::LBracket, Self::parse_array_index);
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut program = Program::new();
        loop {
            if self.cur_token == Token::EOF {
                break;
            }
            program.statements.push(self.parse_statement()?);
            self.next_token();
        }
        Ok(program)
    }

    pub fn parse_statement(&mut self) -> Result<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement().map(|let_stmt| Statement::LetStmt(let_stmt)),
            Token::Return => self.parse_return_statement().map(|return_stmt| Statement::ReturnStmt(return_stmt)),
            _ => self.parse_expression_statement().map(|expr_stmt| Statement::ExpStmt(expr_stmt)),
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement> {
        self.expected_peek(Token::identify_placeholder())?;
        let name = Identifier { token: self.cur_token.clone() };

        self.expected_peek(Token::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.expected_peek(Token::Semicolon)?;

        Ok(LetStatement {
            token: Token::Let,
            name,
            value,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement> {
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        self.expected_peek(Token::Semicolon)?;
        Ok(ReturnStatement { token: Token::Return, value })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }
        Ok(ExpressionStatement {
            token,
            expression,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let prefix = self.get_prefix_fn(&self.cur_token)?;

        let mut left_exp = prefix(self)?;

        while !self.peek_token_is(Token::Semicolon) && (precedence as u8) < self.peek_precedence() as u8 {
            let infix = self.get_infix_fn(&self.peek_token);
            if infix.is_err() {
                return Ok(left_exp);
            }
            let infix = infix.unwrap().clone();
            self.next_token();
            left_exp = infix(self, left_exp)?;
        }
        Ok(left_exp)
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        Ok(Expression::IdentExpr(
            Identifier { token: self.cur_token.clone() }
        ))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        if let Token::Int(v) = self.cur_token {
            return Ok(Expression::IntExpr(
                IntegerLiteral { token: self.cur_token.clone(), value: v }
            ));
        }
        panic!("found a invalid int token {:?}", self.cur_token);
    }

    fn parse_string_literal(&mut self) -> Result<Expression> {
        if let Token::String(value) = &self.cur_token{
            Ok(
                Expression::StrExpr(
                    StringLiteral { token: self.cur_token.clone(), value: value.clone()}
                )
            )
        }else{
            panic!("found a invalid int token {:?}", self.cur_token);
        }
    }

    fn parse_boolean_literal(&mut self) -> Result<Expression> {
        Ok(
            BoolExpr(BooleanLiteral {
                token: self.cur_token.clone(),
                value: bool::from_str(&self.cur_token.to_string()).unwrap(),
            })
        )
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        self.expected_peek(Token::LParen)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expected_peek(Token::RParen)?;
        self.expected_peek(Token::LBrace)?;

        let consequence = self.parse_block_statement()?;
        let mut if_expression = IfExpression {
            token: Token::If,
            condition: Box::new(condition),
            consequence,
            alternative: None,
        };

        if self.peek_token_is(Token::Else) {
            self.next_token();
            self.expected_peek(Token::LBrace)?;
            if_expression.alternative = self.parse_block_statement()
                .map_or_else(
                    |_| None,
                    |block_stmts| Some(Box::new(block_stmts)),
                );
        }
        Ok(Expression::IfExpr(if_expression))
    }

    fn parse_function_literal(&mut self) -> Result<Expression> {
        self.expected_peek(Token::LParen)?;
        let parameters = self.parse_expression_list(Token::RParen)?;

        self.expected_peek(Token::LBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::FuncExpr(FunctionLiteral {
            token: Token::Function,
            parameters,
            body,
        }))
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expression>> {
        let mut list = vec![];
        if self.peek_token_is(end.to_placeholder()) {
            self.next_token();
            return Ok(list);
        }
        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }
        self.expected_peek(end)?;
        Ok(list)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        let token = self.cur_token.clone();

        let arguments = self.parse_expression_list(Token::RParen)?;
        Ok(Expression::CallExpr(CallExpression {
            token,
            function: Box::new(function),
            arguments: arguments.into_iter().map(Box::new).collect(),
        }))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        self.next_token();
        let mut statements = vec![];
        while !self.current_token_is(Token::RBrace) && !self.current_token_is(Token::EOF) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.next_token();
        }
        Ok(BlockStatement { token: Token::LBrace, statements })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest)?;

        self.expected_peek(Token::RParen)?;
        Ok(exp)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let token = self.cur_token.clone();
        let operator = token.to_string();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::PrefixExpr(PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let token = self.cur_token.clone();
        let operator = token.to_string();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(Expression::InfixExpr(InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    fn parse_array_literal(&mut self) -> Result<Expression> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(Token::RBracket)?
            .into_iter()
            .map(|e| Box::new(e)).collect::<Vec<_>>();
        Ok(Expression::ArrayExpr(ArrayLiteral { token, elements }))
    }

    fn parse_array_index(&mut self, expr: Expression) -> Result<Expression> {
        let token = self.cur_token.clone();
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;
        self.expected_peek(Token::RBracket)?;
        Ok(
            Expression::ArrayIndex(
                ArrayIndex {
                    token,
                    name: Box::new(expr),
                    index: Box::new(index),
                }
            )
        )
    }

    fn parse_hashmap_literal(&mut self) -> Result<Expression> {
        let token = self.cur_token.clone();
        let mut pairs = vec![];
        while !self.peek_token_is(Token::RBrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;
            self.expected_peek(Token::Colon)?;
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((key, value));

            match self.peek_token {
                Token::Comma => self.next_token(),
                Token::RBrace => {}
                _ => return Err(WrongHashmapSyntax.into())
            }
        }
        self.expected_peek(Token::RBrace)?;
        Ok(Expression::HashMapExpr(HashmapLiteral {
            token,
            pairs,
        }))
    }

    fn expected_peek(&mut self, expected_token: Token) -> Result<()> {
        if std::mem::discriminant(&self.peek_token) == std::mem::discriminant(&expected_token) {
            self.next_token();
            Ok(())
        } else {
            error!("expect {:?}, found {:?}", expected_token, self.peek_token);
            Err(ParsingError::PeekError { expected: expected_token, actual: self.peek_token.clone() }.into())
        }
    }

    fn current_token_is(&self, expected_token: Token) -> bool {
        std::mem::discriminant(&self.cur_token) == std::mem::discriminant(&expected_token)
    }

    fn peek_token_is(&self, expected_token: Token) -> bool {
        std::mem::discriminant(&self.peek_token) == std::mem::discriminant(&expected_token)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn register_prefix(&mut self, token: Token, func: fn(&mut Parser<T>) -> Result<Expression>) {
        self.prefix_parse_fns.insert(token, func);
    }

    fn get_prefix_fn(&self, token: &Token) -> Result<&fn(&mut Parser<T>) -> Result<Expression>> {
        self.prefix_parse_fns.get(&token.to_placeholder()).ok_or_else(|| {
            info!("no prefix function has been found for `{:?}`", token);
            NoPrefixParseFun(token.clone()).into()
        })
    }

    fn register_infix(&mut self, token_type: Token, func: fn(&mut Self, left: Expression) -> Result<Expression>) {
        self.infix_parse_fns.insert(token_type, func);
    }

    fn get_infix_fn(&self, token: &Token) -> Result<&fn(&mut Self, Expression) -> Result<Expression>> {
        self.infix_parse_fns.get(&token.to_placeholder()).ok_or_else(|| {
            info!("no infix function has been found for `{:?}`", token);
            NoInfixParseFun(token.clone()).into()
        })
    }


    fn peek_precedence(&self) -> Precedence {
        match self.precedence.get(&self.peek_token) {
            Some(precedence) => *precedence,
            None => Precedence::Lowest,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match self.precedence.get(&self.cur_token) {
            Some(precedence) => *precedence,
            None => Precedence::Lowest,
        }
    }
}

impl Parser<BufReader<Cursor<String>>> {
    pub fn from_string(program: &str) -> Parser<BufReader<Cursor<String>>> {
        let lexer = Lexer::from_str(program);
        Self::from_lexer(lexer)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use tracing_subscriber;

    #[test]
    fn test_parsing_let_statement_work() {
        let cases = vec![
            ("let x = 5;", "let x = 5;"),
            ("let y=true;", "let y = true;"),
            ("let foobar = y;", "let foobar = y;"),
        ];
        run_cases(&cases);
    }


    #[test]
    fn test_parsing_return_statement() {
        let cases = vec![
            ("return 5;", "return 5;"),
            ("return true;", "return true;"),
            ("return foobar;", "return foobar;"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_parsing_identifier() {
        let cases = vec![("foobar", "foobar")];
        run_cases(&cases);
    }

    #[test]
    fn test_parsing_integer_literal() {
        let cases = vec![
            ("5;", "5")
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_parsing_string_literal() {
        let cases = vec![
            ("\"f\"", "\"f\""),
            ("\"foo 'bar\"", "\"foo 'bar\""),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let cases = vec![
            ("!15;", "!", "(!15)"),
            ("-15;", "-", "(-15)"),
        ];

        for &case in cases.iter() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse().unwrap();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements.get(0).unwrap().to_string(), case.2);
        }
    }

    #[test]
    fn test_parsing_infix_expression_work() {
        let cases = vec![
            ("5 + 5;", "(5 + 5)"),
            ("5 - 5;", "(5 - 5)"),
            ("5 * 5;", "(5 * 5)"),
            ("5 / 5;", "(5 / 5)"),
            ("5 > 5;", "(5 > 5)"),
            ("5 < 5;", "(5 < 5)"),
            ("5 == 5;", "(5 == 5)"),
            ("5 != 5;", "(5 != 5)"),
            ("-5 < 5;", "((-5) < 5)"),
            ("foobar + barfoo;", "(foobar + barfoo)"),
            ("foobar - barfoo;", "(foobar - barfoo)"),
            ("foobar * barfoo;", "(foobar * barfoo)"),
            ("foobar / barfoo;", "(foobar / barfoo)"),
            ("foobar > barfoo;", "(foobar > barfoo)"),
            ("foobar < barfoo;", "(foobar < barfoo)"),
            ("foobar == barfoo;", "(foobar == barfoo)"),
            ("foobar != barfoo;", "(foobar != barfoo)"),
            ("true == true", "(true == true)"),
            ("true != false", "(true != false)"),
            ("false == false", "(false == false)"),
        ];
        for &case in cases.iter() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse().unwrap();
            assert_eq!(program.statements.len(), 1);
            assert_eq!(program.statements.get(0).unwrap().to_string(), case.1);
        }
    }

    #[test]
    fn test_parsing_boolean_expression_work() {
        let cases = vec![
            ("true", "true"),
            ("false", "false"),
            ("3>5 == false", "((3 > 5) == false)"),
            ("3<5 == true", "((3 < 5) == true)"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_parsing_function_literal_work() {
        let cases = vec![
            ("fn(){y}", "fn () {y}"),
            ("fn(x){y}", "fn (x) {y}"),
            ("fn(x,y,z){a}", "fn (x,y,z) {a}"),
            ("fn(x,y) { x + y}", "fn (x,y) {(x + y)}"),
            ("fn(x,y) {}", "fn (x,y) {}"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_if_expression_work() {
        let cases = vec![
            ("if (x<y) { x }", "if (x < y) {x}"),
            ("if (x<y) { x } else { y }", "if (x < y) {x} else {y}"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_call_expression_work() {
        let cases = vec![
            ("add(1,2*3,4+5);", "add(1,(2 * 3),(4 + 5))"),
            ("add();", "add()"),
            ("add(1);", "add(1)"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_operator_precedence() {
        let cases = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))", ),
            ("a + b + c", "((a + b) + c)", ),
            ("a + b - c", "((a + b) - c)", ),
            ("a * b * c", "((a * b) * c)", ),
            ("a * b / c", "((a * b) / c)", ),
            ("a + b / c", "(a + (b / c))", ),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)", ),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)", ),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))", ),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))", ),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))", ),
            ("true", "true", ),
            ("false", "false", ),
            ("3 > 5 == false", "((3 > 5) == false)", ),
            ("3 < 5 == true", "((3 < 5) == true)", ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)", ),
            ("(5 + 5) * 2", "((5 + 5) * 2)", ),
            ("2 / (5 + 5)", "(2 / (5 + 5))", ),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))", ),
            ("-(5 + 5)", "(-(5 + 5))", ),
            ("!(true == true)", "(!(true == true))", ),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)", ),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a,b,1,(2 * 3),(4 + 5),add(6,(7 * 8)))", ),
            ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))", ),
            ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1,2,3,4][(b * c)])) * d)"),
            ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])),(b[1]),(2 * ([1,2][1])))"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_hashmap_literal(){
        let cases = vec![
            ("{1+1: 2, true: 1, \"foo\": \"bar\"}", "{(1 + 1) : 2,true : 1,\"foo\" : \"bar\"}"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_array_literal() {
        let cases = vec![
            ("[1, 2*2, 3 + 3]", "[1,(2 * 2),(3 + 3)]"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_array_index() {
        let cases = vec![
            ("myArray[1+1]", "(myArray[(1 + 1)])")
        ];
        run_cases(&cases);
    }

    fn run_cases(cases: &Vec<(&str, &str)>) {
        tracing_subscriber::fmt().try_init().unwrap_or(());
        for (no, case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse().unwrap();
            assert_eq!(parser.errors.len(), 0, "input is {}, error is {}", case.0, parser.errors.get(0).unwrap());
            assert_eq!(program.to_string(), case.1, "{}: input is {}", no, case.0);
        }
    }
}
