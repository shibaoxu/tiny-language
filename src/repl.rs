use std::io::{BufRead, Write};
use crate::evaluator::eval;
use crate::parser::{Parser, ParsingError};

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

        let mut parser = Parser::from_string(&buf);
        let program = parser.parse();
        if parser.errors.len() != 0{
            print_parser_errors(&parser.errors, output);
            continue;
        }
        let result = eval(&program);

        output.write(result.inspect().as_bytes()).unwrap();
        output.write(b"\n").unwrap();
        output.flush().unwrap();
    }
}

fn print_parser_errors(errors: &Vec<ParsingError>, output: &mut impl Write) {
    for e in errors.iter(){
        output.write_fmt(format_args!("{:?}\n", e.to_string())).unwrap()
    }
}