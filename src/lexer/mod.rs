use std::fs::File;
use std::io::{BufRead, BufReader, Cursor, Seek, SeekFrom};
use crate::lexer::token::Token;

pub mod token;

#[derive(PartialEq)]
enum State {
    Start,
    Done,
    GreatEqual,
    LessEqual,
    NotEqual,
    Equal,
    Int,
    String,
    Word,
}

pub struct Lexer<T: BufRead + Seek> {
    reader: T,
}

impl<T: BufRead + Seek> Lexer<T> {
    pub fn from_buf_reader(input: T) -> Self {
        Lexer {
            reader: input,
        }
    }

    pub fn next_token(&mut self) -> Token {
        let mut state = State::Start;
        let mut buf = String::new();
        let mut token: Token = Token::EOF;
        while state != State::Done {
            match self.read_char() {
                Some(c) => {
                    match state {
                        State::Start => {
                            match c {
                                _ if c.is_whitespace() => continue,
                                _ if c.is_ascii_digit() => {
                                    state = State::Int;
                                    buf.push_str(&c.to_string());
                                }
                                _ if Self::is_ident(c) => {
                                    state = State::Word;
                                    buf.push_str(&c.to_string())
                                }
                                '>' => state = State::GreatEqual,
                                '<' => state = State::LessEqual,
                                '=' => state = State::Equal,
                                '!' => state = State::NotEqual,
                                '"' => state = State::String,
                                _ => {
                                    state = State::Done;
                                    token = match c {
                                        '+' => Token::Plus,
                                        '-' => Token::Minus,
                                        '*' => Token::Asterisk,
                                        '/' => Token::Slash,
                                        ';' => Token::Semicolon,
                                        '(' => Token::LParen,
                                        ')' => Token::RParen,
                                        '{' => Token::LBrace,
                                        '}' => Token::RBrace,
                                        '[' => Token::LBracket,
                                        ']' => Token::RBracket,
                                        ',' => Token::Comma,
                                        ':' => Token::Colon,
                                        _ => Token::Illegal(c.to_string()),
                                    }
                                }
                            }
                        }
                        State::GreatEqual => {
                            state = State::Done;
                            if c == '=' {
                                token = Token::GE;
                            } else {
                                token = Token::GT;
                                self.back();
                            }
                        }
                        State::LessEqual => {
                            state = State::Done;
                            if c == '=' {
                                token = Token::LE;
                            } else {
                                token = Token::LT;
                                self.back();
                            }
                        }
                        State::NotEqual => {
                            state = State::Done;
                            if c == '=' {
                                token = Token::NotEq;
                            } else {
                                token = Token::Bang;
                                self.back();
                            }
                        }
                        State::Equal => {
                            state = State::Done;
                            if c == '=' {
                                token = Token::EQ;
                            } else {
                                token = Token::Assign;
                                self.back();
                            }
                        }
                        State::Word => {
                            if Self::is_letter(c) {
                                buf.push_str(&c.to_string());
                                continue;
                            } else {
                                state = State::Done;
                                token = Token::lookup_ident(&buf);
                                self.back();
                            }
                        }
                        State::Int => {
                            if c.is_ascii_digit() {
                                buf.push_str(&c.to_string());
                                continue;
                            } else {
                                state = State::Done;
                                token = Token::Int(buf.parse::<i64>().unwrap());
                                self.back();
                            }
                        }
                        State::String => {
                            if c == '"' {
                                state = State::Done;
                                token = Token::String(buf.clone());
                            } else {
                                buf.push_str(&String::from(c));
                                continue;
                            }
                        }
                        State::Done => {
                            panic!("should not arrive here")
                        }
                    }
                }
                None => {
                    token = match state {
                        State::Start => Token::EOF,
                        State::Done => panic!("should not arrive here"),
                        State::GreatEqual => Token::GT,
                        State::LessEqual => Token::LT,
                        State::NotEqual => Token::Bang,
                        State::Equal => Token::Assign,
                        State::Int => Token::Int(buf.parse().unwrap()),
                        State::String => Token::Illegal(buf.clone()),
                        State::Word => Token::lookup_ident(&buf),
                    };
                    state = State::Done;
                }
            }
        }
        token
    }

    fn read_char(&mut self) -> Option<char> {
        let mut buf = [0];
        match self.reader.read(&mut buf) {
            Ok(size) if size > 0 => Some(char::from(buf[0])),
            _ => None,
        }
    }
    fn back(&mut self) {
        self.reader.seek(SeekFrom::Current(-1)).expect("unknown error when reader seeking");
    }

    fn is_letter(c: char) -> bool {
        c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c >= '0' && c <= '9'
    }
    fn is_ident(c: char) -> bool {
        c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
    }
}

impl Lexer<BufReader<Cursor<String>>> {
    pub fn from_str(input: &str) -> Self {
        let buf_reader = BufReader::new(Cursor::new(input.to_string()));
        Lexer::from_buf_reader(buf_reader)
    }
}


impl Lexer<BufReader<File>> {
    pub fn from_file(file: File) -> Lexer<BufReader<File>> {
        Lexer::from_buf_reader(BufReader::new(file))
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::path::Path;
    use crate::lexer::Lexer;
    use crate::lexer::token::Token;

    #[test]
    fn test_next_token_work() {
        let input = "\
        let five = 5;\
        let ten = 10;\
        let add = fn(x, y) {\
            x + y;\
        };\
        let result = add(five, ten);\
        !-/*5;\
        5 < 10 > 5;\
        if (5 < 10) {\
            return true;\
        } else {\
            return false;\
        }\
        10 == 10;\
        10 != 9; \
        \"foobar\" \
        \"foo bar\" \
        [foo, 10] \
        {\"foo\": \"bar\"} \
        ";

        let outputs = vec![
            Token::Let, Token::Identity("five".to_string()), Token::Assign, Token::Int(5), Token::Semicolon,
            Token::Let, Token::Identity("ten".to_string()), Token::Assign, Token::Int(10), Token::Semicolon,
            Token::Let, Token::Identity("add".to_string()), Token::Assign, Token::Function, Token::LParen, Token::Identity("x".to_string()), Token::Comma, Token::Identity("y".to_string()), Token::RParen, Token::LBrace,
            Token::Identity("x".to_string()), Token::Plus, Token::Identity("y".to_string()), Token::Semicolon,
            Token::RBrace, Token::Semicolon,
            Token::Let, Token::Identity("result".to_string()), Token::Assign, Token::Identity("add".to_string()), Token::LParen, Token::Identity("five".to_string()), Token::Comma, Token::Identity("ten".to_string()), Token::RParen, Token::Semicolon,
            Token::Bang, Token::Minus, Token::Slash, Token::Asterisk, Token::Int(5), Token::Semicolon,
            Token::Int(5), Token::LT, Token::Int(10), Token::GT, Token::Int(5), Token::Semicolon,
            Token::If, Token::LParen, Token::Int(5), Token::LT, Token::Int(10), Token::RParen, Token::LBrace,
            Token::Return, Token::True, Token::Semicolon,
            Token::RBrace, Token::Else, Token::LBrace,
            Token::Return, Token::False, Token::Semicolon,
            Token::RBrace,
            Token::Int(10), Token::EQ, Token::Int(10), Token::Semicolon,
            Token::Int(10), Token::NotEq, Token::Int(9), Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::LBracket, Token::Identity("foo".to_string()), Token::Comma, Token::Int(10), Token::RBracket,
            Token::LBrace, Token::String("foo".to_string()),Token::Colon, Token::String("bar".to_string()), Token::RBrace,
            Token::EOF,
        ];

        let mut lexer_string = Lexer::from_str(input);
        for expected in outputs.iter() {
            let token = lexer_string.next_token();
            assert_eq!(&token, expected, "actual is {:?}, expected is {:?}", &token, expected);
        }

        let mut lexer_file = Lexer::from_file(File::open(Path::new("./monkey_code")).unwrap());
        for expected in outputs.iter() {
            let token = lexer_file.next_token();
            assert_eq!(&token, expected, "actual is {:?}, expected is {:?}", &token, expected);
        }
    }
}