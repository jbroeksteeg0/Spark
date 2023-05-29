use std::fmt::Binary;

use crate::tokeniser::{BinaryOperation, Token};

#[derive(Debug, Clone)]
pub enum ASTNode {
    Block(Vec<ASTNode>),
    FunctionCall(String, Vec<ASTNode>),
    FunctionDefinition(Vec<String>, Vec<ASTNode>),
    BinaryOperation(Box<ASTNode>, Box<ASTNode>, BinaryOperation),
    Variable(String),
    NumberLiteral(f64),
    StringLiteral(String),
    LetStatement(String, Box<ASTNode>),
    ReturnStatement(Box<ASTNode>),
}

type ParseFunction = fn(&[Token]) -> Option<(ASTNode, &[Token])>;

fn parse_expression_base(input: &[Token]) -> Option<(ASTNode, &[Token])> {
    return match input {
        // If there is a number, return it
        [Token::TkNumber(x), ..] => Some((ASTNode::NumberLiteral(x.clone()), &input[1..])),
        // If there is a string literal, return it
        [Token::TkString(s), ..] => Some((ASTNode::StringLiteral(s.clone()), &input[1..])),
        // If there is a variable name, return it
        [Token::TkVariable(s), ..] => Some((ASTNode::Variable(s.clone()), &input[..])),
        // Otherwise, fail
        _ => None,
    };
}

fn parse_expression_md(input: &[Token]) -> Option<(ASTNode, &[Token])> {
    let mut ahead: &[Token] = input;
    let mut behind: Option<(&[Token], BinaryOperation)> = None;

    // Parse some prefix of [EXPRESSION_BASE, ["+" | "-"]]*
    loop {
        match parse_expression_base(ahead) {
            Some((_, tail1)) => match tail1.iter().nth(0) {
                Some(Token::TkBinaryOperation(binop))
                    if binop.clone() == BinaryOperation::TIMES
                        || binop.clone() == BinaryOperation::DIV =>
                {
                    let chars_behind = input.len() - (tail1.len() - 1);

                    behind = Some((&input[..chars_behind - 1], binop.clone()));
                    ahead = &input[chars_behind..];
                }
                _ => {
                    break;
                }
            },
            None => {
                break;
            }
        }
    }

    return match (behind, parse_expression_base(ahead)) {
        (Some((lhs, op)), Some((rhs_node, tail))) => Some((
            (ASTNode::BinaryOperation(
                Box::new(parse_expression_as(lhs).unwrap().0),
                Box::new(rhs_node),
                op,
            )),
            tail,
        )),
        _ => parse_expression_base(input),
    };
}

fn parse_expression_as(input: &[Token]) -> Option<(ASTNode, &[Token])> {
    let mut ahead: &[Token] = input;
    let mut behind: Option<(&[Token], BinaryOperation)> = None;

    // Parse some prefix of [EXPRESSION_BASE, ["+" | "-"]]*
    loop {
        match parse_expression_md(ahead) {
            Some((_, tail1)) => match tail1.iter().nth(0) {
                Some(Token::TkBinaryOperation(binop))
                    if binop.clone() == BinaryOperation::PLUS
                        || binop.clone() == BinaryOperation::MINUS =>
                {
                    let chars_behind = input.len() - (tail1.len() - 1);

                    behind = Some((&input[..chars_behind - 1], binop.clone()));
                    ahead = &input[chars_behind..];
                }
                _ => {
                    break;
                }
            },
            None => {
                break;
            }
        }
    }

    return match (behind, parse_expression_md(ahead)) {
        (Some((lhs, op)), Some((rhs_node, tail))) => Some((
            (ASTNode::BinaryOperation(
                Box::new(parse_expression_as(lhs).unwrap().0),
                Box::new(rhs_node),
                op,
            )),
            tail,
        )),
        _ => parse_expression_md(input),
    };
}

fn parse_expression(input: &[Token]) -> Result<(ASTNode, &[Token]), String> {
    return match parse_expression_as(input) {
        Some((node, xs)) => Ok((node, xs)),
        None => Err(format!("Could not parse expression starting with {:?}", input.iter().take(5)))
    }
}

fn parse_statement(input: &[Token]) -> Result<(ASTNode, &[Token]), String> {
    return match input {
        [Token::TkLet, xs  @ ..] => {
            match xs {
                [Token::TkVariable(name), Token::TkEquals, expr @ ..] => {
                    match parse_expression(expr) {
                        Ok((expr_node, tail)) => {
                            match tail {
                                [Token::TkSemicolon, after @ ..] => {
                                    Ok((ASTNode::LetStatement(name.clone(), Box::new(expr_node)), after))
                                },
                                _ => {
                                    Err("Expected semicolon at end of let statement".into())
                                }
                            }
                        },
                        Err(e) => Err(e)
                    }
                },
                _ => {
                    Err(format!("Could not parse assignment starting with {:?}", input.iter().take(10)))
                }
            }
        }
        [xs @ ..] => {
            parse_expression(xs)
        }
    }
}

fn parse_block(input: &[Token]) -> Result<ASTNode, String> {
    match input {
        [Token::TkOpenCurly, middle @ .., Token::TkCloseCurly] => {
            let mut middle_statments: Vec<ASTNode> = vec![];
            let mut remaining_tokens = middle;

            while !remaining_tokens.is_empty() {
                match parse_statement(remaining_tokens) {
                    Ok((node, xs)) => {
                        middle_statments.push(node);
                        remaining_tokens=xs;
                    },
                    Err(e) => {return Err(e);}
                };
            }
            return Ok(ASTNode::Block(middle_statments));
        },
        _ => Err("Block must start and end with {}".into())
    }
}

pub fn parse_tokens(input: Vec<Token>) -> Result<ASTNode, String> {
    let mut with_brackets = vec![Token::TkOpenCurly];
    with_brackets.append(&mut input.clone());
    with_brackets.append(&mut vec![Token::TkCloseCurly]);

    return parse_block(&with_brackets);
}
