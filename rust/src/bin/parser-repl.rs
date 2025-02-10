extern crate monkey_lang;
use std::io::{stdin, stdout, Write};

use monkey_lang::{lexer::tokenizer, parser::parser};

pub fn main() {
    println!("Welcome to Monkey Lang Parser Repl");
    let mut input = String::new();
    loop {
        print!("\n>> ");
        if let Err(_) = stdout().flush() {
            break;
        }

        match stdin().read_line(&mut input) {
            Ok(0) => break,
            Err(_) => break,
            _ => {}
        }

        let tokenizer = tokenizer::Tokenizer::new(&input);
        let mut parser = parser::Parser::new(tokenizer);

        println!("{:?}", parser.parse_program());
    }
}
