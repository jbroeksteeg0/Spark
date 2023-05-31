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
            let x = 5+(5*3); 
            if (x > 5) {
                println(\"Big\");
            }
            if (x < 5) {
                println(\"Small\");
            }
    ",
    );

    let tokens = parse_tokens(lex_string(input).unwrap()).unwrap();
    interpret(tokens);
}
