pub mod parser;
pub mod tokeniser;

use parser::parse_tokens;
use tokeniser::lex_string;

fn main() {
    let input = String::from("1+2*3-4");
    parse_tokens(lex_string(input).unwrap());
}
