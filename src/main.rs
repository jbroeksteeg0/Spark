pub mod parser;
pub mod tokeniser;

use parser::parse_tokens;
use tokeniser::lex_string;

fn main() {
    let input = String::from("let x = 5+10*2; let y = \"hi\";");
    println!("{:?}", parse_tokens(lex_string(input).unwrap()));
}
