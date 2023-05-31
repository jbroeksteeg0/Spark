pub mod interpreter;
pub mod parser;
pub mod stdlibrary;
pub mod tokeniser;

use interpreter::interpret;
use parser::parse_tokens;
use tokeniser::lex_string;

fn main() {
    let input = String::from(
        "
            let s = str(1233);
            if s == \"123\" {
                println(\"Equal\");
            }
    ",
    );

    let tokens = parse_tokens(lex_string(input).unwrap()).unwrap();
    println!("{:?}", tokens);
    interpret(tokens);
}
