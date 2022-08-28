use std::fmt::{Display, Formatter};
use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenType {
    Illegal,
    EOF,

    Identity,
    Int,

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

impl TokenType {
    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            _ => TokenType::Identity,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: &str) -> Self {
        Self {
            token_type,
            literal: literal.into(),
        }
    }
    pub fn new_illegal(literal: &str) -> Self {
        Self::new(TokenType::Illegal, literal)
    }
    pub fn new_eof() -> Self {
        Self::new(TokenType::EOF, "eof")
    }
    pub fn new_identity(literal: &str) -> Self {
        Self::new(TokenType::Identity, literal)
    }
    pub fn new_int(literal: &str) -> Self {
        Self::new(TokenType::Int, literal)
    }
    pub fn new_assign() -> Self {
        Self::new(TokenType::Assign, "=")
    }
    pub fn new_plus() -> Self {
        Self::new(TokenType::Plus, "+")
    }
    pub fn new_minus() -> Self {
        Self::new(TokenType::Minus, "-")
    }

    pub fn new_bang() -> Self {
        Self::new(TokenType::Bang, "!")
    }

    pub fn new_asterisk() -> Self {
        Self::new(TokenType::Asterisk, "*")
    }

    pub fn new_slash() -> Self {
        Self::new(TokenType::Slash, "/")
    }

    pub fn new_lt() -> Self {
        Self::new(TokenType::LT, "<")
    }

    pub fn new_gt() -> Self {
        Self::new(TokenType::GT, ">")
    }

    pub fn new_eq() -> Self {
        Self::new(TokenType::EQ, "==")
    }

    pub fn new_not_eq() -> Self {
        Self::new(TokenType::NotEq, "!=")
    }

    pub fn new_le() -> Self {
        Self::new(TokenType::Let, "<=")
    }

    pub fn new_ge() -> Self {
        Self::new(TokenType::GE, ">=")
    }

    pub fn new_comma() -> Self {
        Self::new(TokenType::Comma, ",")
    }

    pub fn new_semicolon() -> Self {
        Self::new(TokenType::Semicolon, ";")
    }

    pub fn new_lparen() -> Self {
        Self::new(TokenType::LParen, "(")
    }

    pub fn new_rparen() -> Self {
        Self::new(TokenType::RParen, ")")
    }

    pub fn new_lbrace() -> Self {
        Self::new(TokenType::LBrace, "{")
    }

    pub fn new_rbrace() -> Self {
        Self::new(TokenType::RBrace, "}")
    }

    pub fn new_fun() -> Self {
        Self::new(TokenType::Function, "fn")
    }
    pub fn new_let() -> Self {
        Self::new(TokenType::Let, "let")
    }

    pub fn new_true() -> Self {
        Self::new(TokenType::True, "true")
    }

    pub fn new_false() -> Self {
        Self::new(TokenType::False, "false")
    }

    pub fn new_if() -> Self {
        Self::new(TokenType::If, "if")
    }

    pub fn new_else() -> Self {
        Self::new(TokenType::Else, "else")
    }

    pub fn new_return() -> Self {
        Self::new(TokenType::Return, "return")
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            TokenType::Illegal => "illegal",
            TokenType::EOF => "eof",
            TokenType::Identity => "identity",
            TokenType::Int => "INTEGER",
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::LT => "<",
            TokenType::GT => ">",
            TokenType::EQ => "==",
            TokenType::NotEq => "!=",
            TokenType::LE => "<=",
            TokenType::GE => ">=",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",
            TokenType::Function => "fn",
            TokenType::Let => "let",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
        })
    }
}