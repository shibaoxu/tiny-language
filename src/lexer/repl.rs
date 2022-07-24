use std::io::{BufRead, Write};
use crate::lexer::Lexer;
use crate::Token;

static PROMPT: &str = ">> ";

pub fn start<I, O>(input: &mut I, output: &mut O)
    where I: BufRead,
          O: Write
{

    loop {
        let mut buf = String::new();

        output.write(PROMPT.as_bytes()).unwrap();
        output.flush().unwrap();
        input.read_line(&mut buf).unwrap();

        let mut lexer = Lexer::from_str(&buf);
        loop {
            let token = lexer.next_token();
            if token == Token::EOF{
                break;
            }else{
                output.write(format!("{:?}\n", token).as_bytes()).unwrap();
            }
        }
    }

}