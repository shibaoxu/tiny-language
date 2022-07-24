use std::io;
use std::io::BufReader;
use monkey::repl;

fn main() {
    println!("Hello! This is the Monkey program language!");
    println!("Fell free to type in commands");
    let mut input =BufReader::new(io::stdin());
    let mut output = io::stdout();
    repl::start(&mut input, &mut output);
}