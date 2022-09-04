use std::collections::HashMap;
use std::io::{BufRead, BufReader, Cursor, Seek};
use std::rc::Rc;
use std::str::FromStr;
use thiserror::Error;
use crate::{Lexer, TokenType};
use crate::ast::{BlockStatement, BooleanLiteral, CallExpression, Expression, ExpressionStatement, FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement};
use crate::lexer::token::Token;
use crate::parser::ParsingError::{ExpectRightExpress, ExpectValidExpression, NoPrefixParseFun};

#[derive(Error, Debug, Clone)]
pub enum ParsingError {
    #[error("invalid peek token: expected is {expected:?} actual is {actual:?}")]
    PeekError {
        expected: TokenType,
        actual: TokenType,
    },
    #[error("no prefix parse function for {0:?}")]
    NoPrefixParseFun(TokenType),
    #[error("expect a right express for {0:?}")]
    ExpectRightExpress(TokenType),
    #[error("expected a valid expression")]
    ExpectValidExpression,
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
    Call,//fn
}

pub struct Parser<T: BufRead + Seek> {
    lexer: Lexer<T>,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<ParsingError>,
    precedence: HashMap<TokenType, Precedence>,
    prefix_parse_fns: HashMap<TokenType, fn(&mut Self) -> Option<Box<dyn Expression>>>,
    infix_parse_fns: HashMap<TokenType, fn(&mut Self, Box<dyn Expression>) -> Option<Box<dyn Expression>>>,
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
                (TokenType::EQ, Precedence::Equal),
                (TokenType::NotEq, Precedence::Equal),
                (TokenType::LT, Precedence::LessOrGreater),
                (TokenType::GT, Precedence::LessOrGreater),
                (TokenType::Plus, Precedence::Sum),
                (TokenType::Minus, Precedence::Sum),
                (TokenType::Slash, Precedence::Product),
                (TokenType::Asterisk, Precedence::Product),
                (TokenType::LParen, Precedence::Call),
            ]),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        parser.init();
        parser
    }

    fn init(&mut self) {
        self.register_prefix(TokenType::Identity, Self::parse_identifier);
        self.register_prefix(TokenType::Int, Self::parse_integer_literal);
        self.register_prefix(TokenType::Bang, Self::parse_prefix_expression);
        self.register_prefix(TokenType::Minus, Self::parse_prefix_expression);
        self.register_prefix(TokenType::True, Self::parse_boolean_literal);
        self.register_prefix(TokenType::False, Self::parse_boolean_literal);
        self.register_prefix(TokenType::LParen, Self::parse_grouped_expression);
        self.register_prefix(TokenType::If, Self::parse_if_expression);
        self.register_prefix(TokenType::Function, Self::parse_function_literal);

        self.register_infix(TokenType::Plus, Self::parse_infix_expression);
        self.register_infix(TokenType::Minus, Self::parse_infix_expression);
        self.register_infix(TokenType::Slash, Self::parse_infix_expression);
        self.register_infix(TokenType::Asterisk, Self::parse_infix_expression);
        self.register_infix(TokenType::EQ, Self::parse_infix_expression);
        self.register_infix(TokenType::NotEq, Self::parse_infix_expression);
        self.register_infix(TokenType::LT, Self::parse_infix_expression);
        self.register_infix(TokenType::GT, Self::parse_infix_expression);
        self.register_infix(TokenType::LParen, Self::parse_call_expression);
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();
        loop {
            if self.cur_token.token_type == TokenType::EOF {
                break;
            }
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt)
            }
            // self.expected_peek(TokenType::Semicolon);
            self.next_token();
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement().map(|stmt| Box::new(stmt) as Box<dyn Statement>),
            TokenType::Return => self.parse_return_statement().map(|stmt| Box::new(stmt) as Box<dyn Statement>),
            _ => self.parse_expression_statement().map(|stmt| Box::new(stmt) as Box<dyn Statement>),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<ExpressionStatement> {
        let cur = self.cur_token.clone();
        if let Some(expression) = self.parse_expression(Precedence::Lowest) {
            if self.peek_token_is(TokenType::Semicolon) {
                self.next_token();
            }

            return Some(ExpressionStatement {
                token: cur,
                expression,
            });
        }
        None
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type);
        if prefix.is_none() {
            self.errors.push(NoPrefixParseFun(self.cur_token.token_type));
            return None;
        }
        let mut left_exp = prefix.unwrap()(self);
        if left_exp.is_none() {
            return None;
        }

        while !self.peek_token_is(TokenType::Semicolon) && (precedence as u8) < self.peek_precedence() as u8 {
            let infix = self.infix_parse_fns.get(&self.peek_token.token_type);
            if infix.is_none() {
                return left_exp;
            }
            let infix = infix.unwrap().clone();
            self.next_token();
            left_exp = infix(self, left_exp.unwrap());
            if left_exp.is_none() {
                return None;
            }
        }
        left_exp
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if !self.expected_peek(TokenType::Identity) {
            return None;
        }
        let id = Identifier {
            token: self.cur_token.clone(),
        };

        if !self.expected_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();
        let value = self.parse_expression(Precedence::Lowest);
        if value.is_none() {
            return None;
        }

        if !self.expected_peek(TokenType::Semicolon) {
            return None;
        }

        Some(LetStatement {
            token: Token::new_let(),
            name: id,
            value: value.unwrap(),
        })
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement> {
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest);
        if value.is_none() {
            return None;
        }

        if !self.expected_peek(TokenType::Semicolon) {
            return None;
        }
        Some(ReturnStatement { token: Token::new_return(), value: value.unwrap() })
    }

    fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(Identifier { token: self.cur_token.clone() }))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(IntegerLiteral { token: self.cur_token.clone(), value: i64::from_str(&self.cur_token.literal).unwrap() }))
    }

    fn parse_boolean_literal(&mut self) -> Option<Box<dyn Expression>> {
        Some(Box::new(BooleanLiteral {
            token: self.cur_token.clone(),
            value: bool::from_str(&self.cur_token.literal).unwrap(),
        }))
    }

    fn parse_if_expression(&mut self) -> Option<Box<dyn Expression>> {
        if !self.expected_peek(TokenType::LParen) {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest);
        if condition.is_none() || !self.expected_peek(TokenType::RParen) {
            return None;
        }
        if !self.expected_peek(TokenType::LBrace) {
            return None;
        }
        let consequence = self.parse_block_statement();
        if consequence.is_none() {
            return None;
        }

        let mut if_expression = Box::new(IfExpression {
            token: Token::new_if(),
            condition: condition.unwrap(),
            consequence: consequence.unwrap(),
            alternative: None,
        });

        if self.peek_token_is(TokenType::Else) {
            self.next_token();
            if self.expected_peek(TokenType::LBrace) {
                if_expression.alternative = self.parse_block_statement();
            } else {
                return None;
            }
        }
        Some(if_expression)
    }

    fn parse_function_literal(&mut self) -> Option<Box<dyn Expression>> {
        if !self.expected_peek(TokenType::LParen) {
            return None;
        }
        let parameters = self.parse_function_parameters();
        if parameters.is_none() {
            return None;
        }

        if !self.expected_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();
        if body.is_none() {
            return None;
        }

        Some(Box::new(FunctionLiteral {
            token: Token::new_fun(),
            parameters: parameters.unwrap(),
            body: Rc::new(body.unwrap()),
        }))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut parameters = vec![];
        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Some(parameters);
        }

        if self.expected_peek(TokenType::Identity) {
            parameters.push(Identifier { token: self.cur_token.clone() });
        } else {
            return None;
        }

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            if self.expected_peek(TokenType::Identity) {
                parameters.push(Identifier { token: self.cur_token.clone() });
            } else {
                return None;
            }
        }

        if !self.expected_peek(TokenType::RParen) {
            return None;
        }
        Some(parameters)
    }

    fn parse_call_expression(&mut self, function: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let token = self.cur_token.clone();

        let arguments = self.parse_call_arguments();
        if arguments.is_none() {
            return None;
        }
        Some(Box::new(CallExpression {
            token,
            function,
            arguments: arguments.unwrap(),
        }))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<dyn Expression>>> {
        let mut arguments = vec![];
        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Some(arguments);
        }

        self.next_token();
        let argument = self.parse_expression(Precedence::Lowest);
        if argument.is_none() {
            self.errors.push(ExpectValidExpression);
            return None;
        }
        arguments.push(argument.unwrap());

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            let argument = self.parse_expression(Precedence::Lowest);
            if argument.is_none() {
                self.errors.push(ExpectValidExpression);
                return None;
            }
            arguments.push(argument.unwrap());
        }

        if !self.expected_peek(TokenType::RParen) {
            return None;
        }

        Some(arguments)
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        self.next_token();
        let mut statements = vec![];
        while !self.current_token_is(TokenType::RBrace) && !self.current_token_is(TokenType::EOF) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
                self.next_token();
            } else {
                return None;
            }
        }
        Some(BlockStatement { token: Token::new_lbrace(), statements })
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<dyn Expression>> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);

        if self.expected_peek(TokenType::RParen) {
            return exp;
        } else {
            return None;
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let cur_token = self.cur_token.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix);
        if right.is_none() {
            self.errors.push(ExpectValidExpression);
            return None;
        }
        Some(Box::new(PrefixExpression {
            token: cur_token.clone(),
            operator: cur_token.literal.clone(),
            right: right.unwrap(),
        }))
    }
    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>> {
        let cur_token = self.cur_token.clone();

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence);
        if right.is_none() {
            self.errors.push(ExpectRightExpress(cur_token.token_type));
            return None;
        }
        Some(Box::new(InfixExpression {
            token: cur_token.clone(),
            left,
            operator: cur_token.literal,
            right: right.unwrap(),
        }))
    }


    fn expected_peek(&mut self, expected_token: TokenType) -> bool {
        if self.peek_token.token_type == expected_token {
            self.next_token();
            true
        } else {
            self.errors.push(ParsingError::PeekError { expected: expected_token, actual: self.peek_token.token_type });
            false
        }
    }

    fn current_token_is(&self, expected_token: TokenType) -> bool {
        self.cur_token.token_type == expected_token
    }

    fn peek_token_is(&self, expected_token: TokenType) -> bool {
        self.peek_token.token_type == expected_token
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn register_prefix(&mut self, token: TokenType, func: fn(&mut Parser<T>) -> Option<Box<dyn Expression>>) {
        self.prefix_parse_fns.insert(token, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: fn(&mut Self, left: Box<dyn Expression>) -> Option<Box<dyn Expression>>) {
        self.infix_parse_fns.insert(token_type, func);
    }

    fn peek_precedence(&self) -> Precedence {
        match self.precedence.get(&self.peek_token.token_type) {
            Some(precedence) => *precedence,
            None => Precedence::Lowest,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match self.precedence.get(&self.cur_token.token_type) {
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
    fn test_parsing_return_statement_work() {
        let cases = vec![
            ("return 5;", "return 5;"),
            ("return true;", "return true;"),
            ("return foobar;", "return foobar;"),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_parsing_identifier_expression_work() {
        let cases = vec![("foobar", "foobar")];
        run_cases(&cases);
    }

    #[test]
    fn test_parsing_integer_literal_expression_work() {
        let cases = vec![
            ("5;", "5")
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_parsing_prefix_expressions_work() {
        let cases = vec![
            ("!15;", "!", "(!15)"),
            ("-15;", "-", "(-15)"),
        ];

        for &case in cases.iter() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse();
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
            let program = parser.parse();
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
        ];
        run_cases(&cases);
    }

    fn run_cases(cases: &Vec<(&str, &str)>) {
        for (no, case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse();
            assert_eq!(parser.errors.len(), 0, "input is {}, error is {}", case.0, parser.errors.get(0).unwrap());
            assert_eq!(program.to_string(), case.1, "{}: input is {}", no, case.0);
        }
    }
}

