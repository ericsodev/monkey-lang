extern crate monkey_lang;
use std::io::{stdin, stdout, Write};

use monkey_lang::lexer::tokenizer;

pub fn main() {
    println!("Welcome to Monkey Lang Lexer Repl");
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

        let mut tokenizer = tokenizer::build_tokenizer(input.clone());
        loop {
            let token = tokenizer.next_token();
            println!("{:?}", token);

            if tokenizer.eof() {
                break;
            }
        }
    }
}
