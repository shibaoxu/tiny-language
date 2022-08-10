use std::any::Any;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Cursor, Seek};
use thiserror::Error;
// use anyhow::{Error, Result};
use crate::{Lexer, Token};
use crate::parser::ast::{Expression, Identifier, LetStatement, Program, ReturnStatement, Statement};
use crate::Token::Identity;

mod ast;

#[derive(Error, Debug, Clone)]
pub enum ParsingError {
    #[error("invalid peek token: expected is {expected:?} actual is {actual:?}")]
    PeekError {
        expected: Token,
        actual: Token,
    },
}

struct Parser<T: BufRead + Seek> {
    lexer: Lexer<T>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParsingError>,
    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    infix_parse_fns: HashMap<Token, InfixParseFn>,

}

impl<T: BufRead + Seek> Parser<T> {
    pub fn from_buf_reader(reader: T) -> Self {
        let mut lexer = Lexer::from_buf_reader(reader);
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();
        loop {
            if self.cur_token == Token::EOF {
                break;
            }
            if let Some(stmt) = self.parse_statement(){
                program.statements.push(stmt)
            }
            self.next_token();
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token {
            Token::Let => self.parse_let_statement().map(|stmt| Box::new(stmt) as Box<dyn Statement>),
            Token::Return => self.parse_return_statement().map(|stmt| Box::new(stmt) as Box<dyn Statement>),
            _ => None
        }
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        if !self.expected_peek(Token::Identity("".to_string())) {
            return None;
        }
        let id = Identifier {
            token: self.cur_token.clone(),
        };

        if !self.expected_peek(Token::Assign) {
            return None;
        }

        while self.cur_token != Token::Semicolon {
            self.next_token();
        }
        Some(LetStatement {
            token: Token::Let,
            name: id,
            value: None,
        })
    }

    fn parse_return_statement(&mut self) -> Option<ReturnStatement>{
        let stmt = ReturnStatement{
            token: Token::Return,
            value: None,
        };

        self.next_token();
        while !self.current_token_is(Token::Semicolon){
            self.next_token();
        }
        Some(stmt)
    }

    fn expected_peek(&mut self, expected_token: Token) -> bool {
        if std::mem::discriminant(&expected_token) == std::mem::discriminant(&self.peek_token) {
            self.next_token();
            true
        }else{
            self.errors.push(ParsingError::PeekError {expected: expected_token, actual: self.peek_token.clone()});
            false
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

    fn register_prefix(&mut self, token: Token, func: PrefixParseFn){
        self.prefix_parse_fns.insert(token, func);
    }
}

impl Parser<BufReader<Cursor<String>>> {
    pub fn from_string(program: &str) -> Parser<BufReader<Cursor<String>>> {
        let mut lexer = Lexer::from_str(program);
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        }
    }
}

type PrefixParseFn = fn() -> dyn Expression;
type InfixParseFn = fn(dyn Expression) -> dyn Expression;

#[cfg(test)]
mod tests {
    use std::any::Any;
    use crate::parser::Parser;
    use crate::Token;

    #[test]
    fn test_parse_let_work() {
        let input = "let x = 5;\
        let y = 10;\
        let foobar = 838383;";
        let mut parser = Parser::from_string(input);
        let program = parser.parse();
        assert_eq!(program.statements.len(), 3, "program.statements does not contain 3 statements, got = {}", program.statements.len());
    }

    #[test]
    fn test_parse_let_fail(){
        let input = "\
        let x 5;\
        let = 10;\
        let 838;";
        let mut parser = Parser::from_string(input);
        let program = parser.parse();
        assert_eq!(parser.errors.len(), 3);
    }

    #[test]
    fn test_parse_return_statement_work(){
        let input = "\
        return 5;\
        return 10;\
        return 993;";
        let mut parser = Parser::from_string(input);
        let program = parser.parse();
        assert_eq!(program.statements.len(), 3);
    }
}

