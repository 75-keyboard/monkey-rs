pub mod lexer;
pub mod ast;
pub mod token;
pub mod parser;

use lexer::Lexer;
use std::io::{self};

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!(">> ");
    let mut buffer = String::new();
    while let Ok(_) = io::stdin().read_line(&mut buffer) {
        let mut l = Lexer::new(&buffer);

        loop {
            match l.next_token() {
                token::Token::Eof => break,
                tok => println!("{:?}", tok)
            };
        }
        
        println!(">> ");
    }
}
