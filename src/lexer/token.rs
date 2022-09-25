use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, Clone, Hash, Ord, PartialOrd)]
pub enum Token {
    Illegal(String),
    EOF,

    Identity(String),
    Int(i64),
    String(String),

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
    LBracket,
    RBracket,
    Colon,

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Macro,
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
            "macro" => Token::Macro,
            _ => Token::Identity(String::from(ident)),
        }
    }

    pub fn to_placeholder(&self) -> Self {
        match self {
            Token::Illegal(_) => Token::illegal_placeholder(),
            Token::Identity(_) => Token::identify_placeholder(),
            Token::Int(_) => Token::int_placeholder(),
            Token::String(_) => Token::string_placeholder(),
            _ => self.clone()
        }
    }

    pub fn identify_placeholder() -> Self {
        Token::Identity("_".into())
    }

    pub fn int_placeholder() -> Self {
        Token::Int(0)
    }

    pub fn string_placeholder() -> Self {
        Token::String("_".into())
    }

    pub fn illegal_placeholder() -> Self {
        Token::Illegal("_".into())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&match self {
            Token::Illegal(v) => format!("illegal {}", v),
            Token::EOF => "eof".into(),
            Token::Identity(v) => format!("{}", v),
            Token::Int(v) => format!("{}", v),
            Token::String(v) => format!("\"{}\"", v),
            Token::Assign => "=".into(),
            Token::Plus => "+".into(),
            Token::Minus => "-".into(),
            Token::Bang => "!".into(),
            Token::Asterisk => "*".into(),
            Token::Slash => "/".into(),
            Token::LT => "<".into(),
            Token::GT => ">".into(),
            Token::EQ => "==".into(),
            Token::NotEq => "!=".into(),
            Token::LE => "<=".into(),
            Token::GE => ">=".into(),
            Token::Comma => ",".into(),
            Token::Semicolon => ";".into(),
            Token::LParen => "(".into(),
            Token::RParen => ")".into(),
            Token::LBrace => "{".into(),
            Token::RBrace => "}".into(),
            Token::Colon => ":".into(),
            Token::Function => "fn".into(),
            Token::Let => "let".into(),
            Token::True => "true".into(),
            Token::False => "false".into(),
            Token::If => "if".into(),
            Token::Else => "else".into(),
            Token::Return => "return".into(),
            Token::LBracket => "[".into(),
            Token::RBracket => "]".into(),
            Token::Macro => "macro".into(),
        })
    }
}