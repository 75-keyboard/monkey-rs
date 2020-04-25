pub mod lexer;
pub mod ast;
pub mod token;
pub mod parser;
pub mod object;
pub mod evaluator;

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

        if let Some(evaluated) = evaluator::eval(evaluator::Node::Program(program)) {
            println!("{}", evaluated);
        }

        println!(">> ");
        buffer = "".to_string();
    }
}

fn print_parser_errors(errors: Vec<String>) {
    
    let monkey_face = r#"            __,__
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
    println!("{}\nWoops! We ran into some monkey business here!\n parser errors:", monkey_face);
    for msg in errors {
        println!("{}", msg);
    }
}
