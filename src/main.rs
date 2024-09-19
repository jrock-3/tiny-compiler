use std::io::stdin;

use parser::Parser;

mod input;
mod parser;
mod parser_data;
mod tokenizer;
mod tokenizer_data;

fn main() {
    let mut parser = Parser::new(stdin().lock());

    if let Some(()) = parser.computation() {
        // println!("{:?}", parser);
        parser.generate_graph("./tests/main.dot");
        parser.generate_instructions("./tests/main.ssa");
    } else {
        println!("Syntax Error");
        dbg!(&parser);
    }
}
