use std::io::{BufRead, Write};
use crate::evaluator::environment::Environment;
use crate::evaluator::eval;
use crate::parser::Parser;



static PROMPT: &str = ">> ";

pub fn start<I, O>(input: &mut I, output: &mut O)
    where I: BufRead,
          O: Write
{
    let mut env = Environment::new();

    loop {
        let mut buf = String::new();

        output.write(PROMPT.as_bytes()).unwrap();
        output.flush().unwrap();
        input.read_line(&mut buf).unwrap();

        let mut parser = Parser::from_string(&buf);
        let program = parser.parse();
        let program = match program{
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}", e);
                continue;
            }
        };

        match eval(&program, &mut env){
            Ok(v) => {
                output.write(v.inspect().as_bytes()).unwrap();
                output.write(b"\n").unwrap();
            }
            Err(e) => {
                output.write(e.to_string().as_bytes()).unwrap();
                output.write(b"\n").unwrap();
            }
        }
        output.flush().unwrap();
    }
}