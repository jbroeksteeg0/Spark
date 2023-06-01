#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperation {
    PLUS,
    MINUS,
    TIMES,
    DIV,
    LE,
    GE,
    GREATER,
    LESS,
    EQUALS,
}

#[derive(Debug, Clone)]
pub enum Token {
    TkLet,
    TkEquals,
    TkVariable(String),
    TkNumber(f64),
    TkString(String),
    TkIf,
    TkElse,
    TkOpenCurly,
    TkCloseCurly,
    TkOpenRound,
    TkCloseRound,
    TkOpenSquare,
    TkCloseSquare,
    TkReturn,
    TkSemicolon,
    TkBinaryOperation(BinaryOperation),
    TkComma,
    TkFn,
}

type TokeniseFunction = fn(&str) -> Option<(Vec<Token>, &str)>;

fn lex_whitespace(input: &str) -> Option<(Vec<Token>, &str)> {
    assert!(!input.is_empty());

    return match input.chars().nth(0).unwrap() {
        ' ' | '\t' | '\n' => Some((vec![], &input[1..])),
        _ => None,
    };
}

fn lex_keyword(input: &str) -> Option<(Vec<Token>, &str)> {
    // All the keywords and their corresponding tokens
    let keywords = vec![
        ("let", Token::TkLet),
        ("if", Token::TkIf),
        ("else", Token::TkElse),
        ("fn", Token::TkFn),
        ("return", Token::TkReturn),
        ("==", Token::TkBinaryOperation(BinaryOperation::EQUALS)),
        ("=", Token::TkEquals),
        (";", Token::TkSemicolon),
        ("(", Token::TkOpenRound),
        (")", Token::TkCloseRound),
        ("[", Token::TkOpenSquare),
        ("]", Token::TkCloseSquare),
        ("{", Token::TkOpenCurly),
        ("}", Token::TkCloseCurly),
        (",", Token::TkComma),
        ("+", Token::TkBinaryOperation(BinaryOperation::PLUS)),
        ("-", Token::TkBinaryOperation(BinaryOperation::MINUS)),
        ("*", Token::TkBinaryOperation(BinaryOperation::TIMES)),
        ("/", Token::TkBinaryOperation(BinaryOperation::DIV)),
        (">=", Token::TkBinaryOperation(BinaryOperation::GE)),
        ("<=", Token::TkBinaryOperation(BinaryOperation::LE)),
        (">", Token::TkBinaryOperation(BinaryOperation::GREATER)),
        ("<", Token::TkBinaryOperation(BinaryOperation::LESS)),
    ];

    // Find the first keyword that is a prefix of the input
    let matching_keyword = keywords
        .iter()
        .find(|(pattern, _)| input.starts_with(pattern));

    return match matching_keyword {
        // If it exists, return it and the substring without the prefix
        Some((pattern, token)) => return Some((vec![token.clone()], &input[pattern.len()..])),
        None => None,
    };
}

fn lex_variable(input: &str) -> Option<(Vec<Token>, &str)> {
    // If the first char is a letter
    if input.chars().nth(0).unwrap().is_alphabetic() {
        let prefix: String = input
            .chars()
            .take_while(|ch| ch.is_alphanumeric())
            .collect();
        let len = prefix.len();
        return Some((vec![Token::TkVariable(prefix.into())], &input[len..]));
    }

    // If its not, return None
    return None;
}

fn lex_number(input: &str) -> Option<(Vec<Token>, &str)> {
    // If the first char is a number
    if input.chars().nth(0).unwrap().is_numeric() {
        // The integer part is all the digits
        let integer_part: String = input.chars().take_while(|ch| ch.is_numeric()).collect();

        // Check whether it has a decimal point after these digits
        let has_decimal = input.chars().nth(integer_part.len()) == Some('.');

        if has_decimal {
            // If it does, get all the digits after the decimal point
            let decimal_part: String = input
                .chars()
                .skip(integer_part.len() + 1)
                .take_while(|ch| ch.is_numeric())
                .collect();

            // Construct the full string
            let full_string = format!("{}.{}", integer_part, decimal_part);
            let full_len = full_string.len();

            // Return it
            return Some((
                vec![Token::TkNumber(full_string.parse::<f64>().unwrap())],
                &input[full_len..],
            ));
        }

        // Otherwise, just return the integer
        let integer_len = integer_part.len();
        return Some((
            vec![Token::TkNumber(integer_part.parse::<f64>().unwrap())],
            &input[integer_len..],
        ));
    }

    // If its not, return None
    return None;
}

fn lex_string_literal(input: &str) -> Option<(Vec<Token>, &str)> {
    // If the string opens with a "
    if input.chars().nth(0) == Some('"') {
        // Find the index of the closing quote
        let close_ind = input.chars().skip(1).position(|ch| ch == '"');

        return match close_ind {
            // If it exists, return the substring
            Some(ind) => {
                return Some((
                    vec![Token::TkString(input[1..ind + 1].into())],
                    &input[ind + 2..],
                ));
            }
            // Otherwise, exit
            None => None,
        };
    }
    return None;
}

pub fn lex_string(input: String) -> Result<Vec<Token>, String> {
    // The functions that can be used to lex a token, in order of priority
    let functions: Vec<TokeniseFunction> = vec![
        lex_keyword,
        lex_variable,
        lex_string_literal,
        lex_number,
        lex_whitespace,
    ];

    // The current string being proecessed, and the vector of tokens to return
    let mut curr_string: &str = input.as_str();
    let mut ret: Vec<Token> = vec![];

    // Loop until the string is empty
    while !curr_string.is_empty() {
        // Find the firtt function that successfully lexes the start of the string
        let first_working = functions.iter().find(|f| return f(curr_string).is_some());

        match first_working {
            // If it exists, use it to lex some prefix of the list
            Some(f) => {
                // f(curr_string) is guaranteed to be Some() here
                let (tokens, tail) = f(curr_string).unwrap();

                // Push the lexxed tokens into the return vector
                for token in tokens {
                    ret.push(token);
                }

                // Set the current string to the rest of the original
                curr_string = tail;
            }
            // Otherwise, return an error
            None => {
                let first_chars: String = curr_string.chars().take(20).collect::<String>();
                return Err(format!(
                    "Could not tokenise string starting with '{}'",
                    first_chars
                ));
            }
        }
    }

    // Return the tokens
    return Ok(ret);
}
