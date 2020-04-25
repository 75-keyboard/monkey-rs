pub mod lexer;
pub mod ast;
pub mod token;
pub mod parser;

use lexer::Lexer;
use std::io::{self};
use parser::Parser;

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!(">> ");
    let mut buffer = String::new();
    while let Ok(_) = io::stdin().read_line(&mut buffer) {
        let l = Lexer::new(&buffer);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        if p.errors().len() != 0 { print_parser_errors(p.errors()); continue; }
        println!("{}", program);
        println!(">> ");
        buffer = "".to_string();
    }
}

fn print_parser_errors(errors: Vec<String>) {
    
    let MonkeyFace = r#"            __,__
       .--.  .-"     "-.  .--.
      / .. \/  .-. .-.  \/ .. \
     | |  '|  /   Y   \  |'  | |
     | \   \  \ 0 | 0 /  /   / |
      \ '- ,\.-"""""""-./, -' /
       ''-' /_   ^ ^   _\ '-''
           |  \._   _./  |
           \   \ '~' /   /
            '._ '-=-' _.'
               '-----'
    "#;
    println!("{}\nWoops! We ran into some monkey business here!\n parser errors:", MonkeyFace);
    for msg in errors {
        println!("{}", msg);
    }
}
