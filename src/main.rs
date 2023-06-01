pub mod interpreter;
pub mod parser;
pub mod stdlibrary;
pub mod tokeniser;

use interpreter::interpret;
use parser::parse_tokens;
use std::env;
use std::fs;
use std::process::exit;
use tokeniser::lex_string;

fn main() {
    let input = String::from(
        "
            let s = str(123);
            if s == \"123\" {
                println(\"Equal\");
            }
    ",
    );

    let file_name: String = match env::args().nth(1) {
        Some(a) => a,
        None => {
            println!("Please provide a filename");
            exit(1);
        }
    };

    let file_content = fs::read_to_string(file_name).expect("Unable to read file");

    let tokens = lex_string(file_content).unwrap();
    // println!("tokens: {:?}",tokens);
    let parsed = parse_tokens(tokens).unwrap();

    interpret(parsed);
}
