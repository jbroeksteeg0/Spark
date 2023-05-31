use std::fmt::Binary;

use crate::interpreter::{Scope, State, Value};
use crate::tokeniser::{BinaryOperation, Token};

#[derive(Clone)]
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
    IfStatement(Box<ASTNode>, Box<ASTNode>),
    BuiltInFunction(fn(&mut State) -> Value),
}

fn parse_expression_base(input: &[Token]) -> Option<(ASTNode, &[Token])> {
    return match input {
        // if there is a bracketed expression, return it
        [Token::TkOpenRound, after_bracket @ ..] => match parse_expression(after_bracket) {
            Ok((node, [Token::TkCloseRound, xs @ ..])) => Some((node, xs)),
            _ => None,
        },
        // if there is a function call, return it
        [Token::TkVariable(name), Token::TkOpenRound, xs @ ..] => {
            let mut curr_after = xs;
            let mut arg_expressions: Vec<ASTNode> = vec![];

            loop {
                match curr_after {
                    [Token::TkCloseRound, tail @ ..] => {
                        return Some((ASTNode::FunctionCall(name.clone(), arg_expressions), tail));
                    }
                    tail => match parse_expression(tail) {
                        Ok((expr_node, after)) => {
                            arg_expressions.push(expr_node);
                            curr_after = after;
                        }
                        Err(e) => {
                            break;
                        }
                    },
                }
            }
            None
        }
        // If there is a number, return it
        [Token::TkNumber(x), ..] => Some((ASTNode::NumberLiteral(x.clone()), &input[1..])),
        // If there is a string literal, return it
        [Token::TkString(s), ..] => Some((ASTNode::StringLiteral(s.clone()), &input[1..])),
        // If there is a variable name, return it
        [Token::TkVariable(s), ..] => Some((ASTNode::Variable(s.clone()), &input[1..])),
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

fn parse_expression_comp(input: &[Token]) -> Option<(ASTNode, &[Token])> {
    // parse EXPRESSION_AS [">=" | ">" | "<=" | "<"]
    match parse_expression_as(input) {
        Some((lhs, after_lhs)) => match after_lhs {
            [Token::TkBinaryOperation(binop), after_op @ ..]
                if !after_op.is_empty()
                    && (binop.clone() == BinaryOperation::GE
                        || binop.clone() == BinaryOperation::GREATER
                        || binop.clone() == BinaryOperation::LE
                        || binop.clone() == BinaryOperation::LESS) =>
            {
                match parse_expression_as(after_op) {
                    Some((rhs, after_rhs)) => {
                        return Some((
                            ASTNode::BinaryOperation(Box::new(lhs), Box::new(rhs), binop.clone()),
                            after_rhs,
                        ));
                    }
                    None => {}
                }
            }

            _ => {}
        },
        None => {}
    }

    return parse_expression_as(input);
}

fn parse_expression(input: &[Token]) -> Result<(ASTNode, &[Token]), String> {
    return match parse_expression_comp(input) {
        Some((node, xs)) => Ok((node, xs)),
        None => Err(format!(
            "Could not parse expression starting with {:?}",
            input.iter().take(5).collect::<Vec<_>>()
        )),
    };
}

fn parse_expression_and_semi(input: &[Token]) -> Result<(ASTNode, &[Token]), String> {
    return match parse_expression(input) {
        Ok((node, after_expr)) => match after_expr {
            [Token::TkSemicolon, after_semi @ ..] => Ok((node, after_semi)),
            _ => Err(format!("Expected semicolon after expression")),
        },
        Err(e) => Err(e),
    };
}

fn parse_statement(input: &[Token]) -> Result<(ASTNode, &[Token]), String> {
    return match input {
        // Parse Let Statement
        [Token::TkLet, xs @ ..] => match xs {
            [Token::TkVariable(name), Token::TkEquals, expr @ ..] => match parse_expression(expr) {
                Ok((expr_node, tail)) => match tail {
                    [Token::TkSemicolon, after @ ..] => Ok((
                        ASTNode::LetStatement(name.clone(), Box::new(expr_node)),
                        after,
                    )),
                    _ => Err("Expected semicolon at end of let statement".into()),
                },
                Err(e) => Err(e),
            },
            _ => Err(format!(
                "Could not parse assignment starting with {:?}",
                input.iter().take(10).collect::<Vec<_>>()
            )),
        },
        // Parse If Statemetn
        [Token::TkIf, after_if @ ..] => match parse_expression(after_if) {
            Ok((expr_node, after_expr)) => match parse_block(after_expr) {
                Ok((block_node, after_block)) => Ok((
                    ASTNode::IfStatement(Box::new(expr_node), Box::new(block_node)),
                    after_block,
                )),
                Err(e) => Err(e),
            },
            _ => Err(format!(
                "Could not parse if statement starting with {:?}",
                input.iter().take(10).collect::<Vec<_>>()
            )),
        },

        // Parse return statement
        [Token::TkReturn, after_return @ ..] => match parse_expression_and_semi(after_return) {
            Ok((node, after_semi)) => Ok((ASTNode::ReturnStatement(Box::new(node)), after_semi)),
            Err(e) => Err(e),
        },

        // Otherwise, assume it's an expression
        [xs @ ..] => match parse_expression_and_semi(xs) {
            Ok((node, after_semi)) => Ok((node, after_semi)),
            Err(e) => Err(e),
        },
    };
}

fn parse_block(input: &[Token]) -> Result<(ASTNode, &[Token]), String> {
    match input {
        [Token::TkOpenCurly, after_open @ ..] => {
            let mut middle_statements: Vec<ASTNode> = vec![];
            let mut remaining_tokens = after_open;

            loop {
                match remaining_tokens {
                    [Token::TkCloseCurly, xs @ ..] => {
                        return Ok((ASTNode::Block(middle_statements), xs));
                    }
                    [all @ ..] => {
                        if all.len() == 0 {
                            return Err(format!("Expected }} after block"));
                        }

                        match parse_statement(all) {
                            Ok((statement_node, after)) => {
                                middle_statements.push(statement_node);
                                remaining_tokens = after;
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                }
            }
        }
        _ => Err("Block must start and end with {}".into()),
    }
}

pub fn parse_tokens(input: Vec<Token>) -> Result<ASTNode, String> {
    let mut with_brackets = vec![Token::TkOpenCurly];
    with_brackets.append(&mut input.clone());
    with_brackets.append(&mut vec![Token::TkCloseCurly]);

    return match parse_block(&with_brackets) {
        Ok((node, _)) => Ok(node),
        Err(e) => Err(e),
    };
}
