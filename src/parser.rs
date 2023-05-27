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

fn parse_expression(input: &[Token]) -> Option<(ASTNode, &[Token])> {
    return parse_expression_as(input);
}
pub fn parse_tokens(input: Vec<Token>) -> ASTNode {
    println!("Parsed as: {:?}", parse_expression(input.as_slice()));

    return ASTNode::Block(vec![]);
}
