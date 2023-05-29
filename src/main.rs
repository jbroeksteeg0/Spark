pub mod parser;
pub mod tokeniser;

use parser::parse_tokens;
use tokeniser::lex_string;

fn main() {
    let input = String::from(
        "
        if 1+2 {
            return 3;
        }
    ",
    );
    println!("{:?}", parse_tokens(lex_string(input).unwrap()));
}
