use std::convert::identity;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Illegal(char),
    EOF,

    Identity(String),
    Int(i64),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,
    EQ,
    NotEq,
    LE,
    GE,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Token {
    pub fn lookup_ident(ident: &str) -> Token {
        match ident {
            "fn" => Token::Function,
            "let" => Token::Let,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Identity(ident.to_string()),
        }
    }

    pub fn literal(&self) -> String{
        match self {
            Token::Illegal(c) => format!("illegal({})", c),
            Token::EOF => "EOF".to_string(),
            Token::Identity(id) => format!("identity({})", id),
            Token::Int(number) => format!("number({})", number),
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Bang => "!".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "\\".to_string(),
            Token::LT => "<".to_string(),
            Token::GT => ">".to_string(),
            Token::EQ => "==".to_string(),
            Token::NotEq => "!=".to_string(),
            Token::LE => "<=".to_string(),
            Token::GE => ">=".to_string(),
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::LBrace => "{".to_string(),
            Token::RBrace => "}".to_string(),
            Token::Function => "fn".to_string(),
            Token::Let => "let".to_string(),
            Token::True => "True".to_string(),
            Token::False => "False".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Return => "return".to_string(),
        }

    }
}

impl Into<String> for &Token{
    fn into(self) -> String {
        self.literal()
    }
}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        todo!()
    }
}