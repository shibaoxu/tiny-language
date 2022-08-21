use std::fs::File;
use std::io::{BufRead, BufReader, Cursor, Seek, SeekFrom};
use crate::lexer::token::Token;
pub use crate::lexer::token::TokenType;

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
        let mut token: Token = Token::new(TokenType::EOF, "eof");
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
                                _ => {
                                    state = State::Done;
                                    token = match c {
                                        '+' => Token::new_plus(),
                                        '-' => Token::new_minus(),
                                        '*' => Token::new_asterisk(),
                                        '/' => Token::new_slash(),
                                        ';' => Token::new_semicolon(),
                                        '(' => Token::new_lparen(),
                                        ')' => Token::new_rparen(),
                                        '{' => Token::new_lbrace(),
                                        '}' => Token::new_rbrace(),
                                        ',' => Token::new_comma(),
                                        _ => Token::new_illegal(&c.to_string()),
                                    }
                                }
                            }
                        }
                        State::GreatEqual => {
                            state = State::Done;
                            if c == '=' {
                                token = Token::new(TokenType::GE, ">=");
                            } else {
                                token = Token::new(TokenType::GT, ">");
                                self.back();
                            }
                        }
                        State::LessEqual => {
                            state = State::Done;
                            if c == '=' {
                                token = Token::new(TokenType::LE, "<=");
                            } else {
                                token = Token::new(TokenType::LT, "<");
                                self.back();
                            }
                        }
                        State::NotEqual => {
                            state = State::Done;
                            if c == '=' {
                                token = Token::new(TokenType::NotEq, "!=");
                            } else {
                                token = Token::new(TokenType::Bang, "!");
                                self.back();
                            }
                        }
                        State::Equal => {
                            state = State::Done;
                            if c == '=' {
                                token = Token::new(TokenType::EQ, "==");
                            } else {
                                token = Token::new(TokenType::Assign, "=");
                                self.back();
                            }
                        }
                        State::Word => {
                            if Self::is_letter(c){
                                buf.push_str(&c.to_string());
                                continue;
                            }else{
                                state = State::Done;
                                token = Token::new(TokenType::lookup_ident(&buf), &buf);
                                self.back();
                            }
                        }
                        State::Int => {
                            if c.is_ascii_digit(){
                                buf.push_str(&c.to_string());
                                continue;
                            }else{
                                state = State::Done;
                                token = Token::new(TokenType::Int, &buf);
                                self.back();
                            }
                        }
                        State::Done => {
                            panic!("should not arrive here")
                        }
                    }
                }
                None => {
                    token = match state {
                        State::Start => Token::new(TokenType::EOF, "eof"),
                        State::Done => panic!("should not arrive here"),
                        State::GreatEqual => Token::new(TokenType::GT, ">"),
                        State::LessEqual => Token::new(TokenType::LT, "<"),
                        State::NotEqual => Token::new(TokenType::Illegal, "!"),
                        State::Equal => Token::new(TokenType::Assign,"="),
                        State::Int => Token::new(TokenType::Int, &buf),
                        State::Word => Token::new(TokenType::lookup_ident(&buf), &buf),
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
        let input = "let five = 5;\
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
        10 != 9;";

        let outputs = vec![
            Token::new_let(), Token::new_identity("five"), Token::new_assign(), Token::new_int( "5"), Token::new_semicolon(),
            Token::new_let(), Token::new_identity("ten"), Token::new_assign(), Token::new_int("10"), Token::new_semicolon(),
            Token::new_let(), Token::new_identity("add"), Token::new_assign(), Token::new_fun(), Token::new_lparen(), Token::new_identity("x"), Token::new_comma(), Token::new_identity( "y"), Token::new_rparen(), Token::new_lbrace(),
            Token::new_identity("x"), Token::new_plus(), Token::new_identity("y"), Token::new_semicolon(),
            Token::new_rbrace(), Token::new_semicolon(),
            Token::new_let(), Token::new_identity("result"), Token::new_assign(), Token::new_identity("add"), Token::new_lparen(), Token::new_identity("five"), Token::new_comma(), Token::new_identity("ten"), Token::new_rparen(), Token::new_semicolon(),
            // TokenType::Bang, TokenType::Minus, TokenType::Slash, TokenType::Asterisk, TokenType::Int(5), TokenType::Semicolon,
            Token::new_bang(), Token::new_minus(),Token::new_slash(),Token::new_asterisk(),Token::new_int("5"),Token::new_semicolon(),
            // TokenType::Int(5), TokenType::LT, TokenType::Int(10), TokenType::GT, TokenType::Int(5), TokenType::Semicolon,
            Token::new_int("5"), Token::new_lt(), Token::new_int("10"), Token::new_gt(), Token::new_int("5"), Token::new_semicolon(),
            // TokenType::If, TokenType::LParen, TokenType::Int(5), TokenType::LT, TokenType::Int(10), TokenType::RParen, TokenType::LBrace,
            Token::new_if(), Token::new_lparen(), Token::new_int("5"), Token::new_lt(), Token::new_int("10"), Token::new_rparen(), Token::new_lbrace(),
            // TokenType::Return, TokenType::True, TokenType::Semicolon,
            Token::new_return(), Token::new_true(), Token::new_semicolon(),
            // TokenType::RBrace, TokenType::Else, TokenType::LBrace,
            Token::new_rbrace(), Token::new_else(), Token::new_lbrace(),
            // TokenType::Return, TokenType::False, TokenType::Semicolon,
            Token::new_return(), Token::new_false(),Token::new_semicolon(),
            // TokenType::RBrace,
            Token::new_rbrace(),
            // TokenType::Int(10), TokenType::EQ, TokenType::Int(10), TokenType::Semicolon,
            Token::new_int("10"), Token::new_eq(), Token::new_int("10"), Token::new_semicolon(),
            // TokenType::Int(10), TokenType::NotEq, TokenType::Int(9), TokenType::Semicolon,
            Token::new_int("10"), Token::new_not_eq(), Token::new_int("9"), Token::new_semicolon(),
            // TokenType::EOF,
            Token::new_eof(),
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