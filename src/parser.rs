use crate::interpreter::{State, Value};
use crate::parser::ASTNode::*;
use crate::tokeniser::{BinaryOperation, Token};
use std::fmt::{self, Binary};

#[derive(Clone)]
pub enum ASTNode {
    Block(Vec<ASTNode>),
    FunctionCall(String, Vec<ASTNode>),
    FunctionDefinition(Vec<String>, Vec<ASTNode>),
    BinaryOperation(Box<ASTNode>, Box<ASTNode>, BinaryOperation),
    Variable(String),
    NumberLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    ListLiteral(Vec<ASTNode>),
    LetStatement(String, Box<ASTNode>),
    AssignStatement(String, Box<ASTNode>),
    ReturnStatement(Box<ASTNode>),
    IfStatement(Box<ASTNode>, Box<ASTNode>),
    WhileStatement(Box<ASTNode>, Box<ASTNode>),
    IfElseStatement(Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
    BuiltInFunction(fn(&mut State) -> Value),
}

impl fmt::Debug for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Block(lines) => write!(f, "Block({:?})", lines),
            IfStatement(cond, expr) => write!(f, "If({:?}, {:?})", cond, expr),
            IfElseStatement(cond, expr, else_expr) => {
                write!(f, "IfElse({:?}, {:?}, {:?})", cond, expr, else_expr)
            }
            LetStatement(name, expr) => write!(f, "Let({:?},{:?})", name, expr),
            AssignStatement(name, expr) => write!(f, "Assign({:?},{:?})", name, expr),
            NumberLiteral(fl) => write!(f, "{}", fl),
            BoolLiteral(b) => write!(f, "{}", b),
            BinaryOperation(l, r, binop) => write!(f, "BinOp{:?}({:?},{:?})", binop, l, r),
            Variable(x) => write!(f, "{:?}", x),
            FunctionCall(name, args) => write!(f, "Call({:?},{:?})", name, args),
            StringLiteral(s) => write!(f, "\"{}\"", s),
            BuiltInFunction(_) => write!(f, "BuiltInFn"),
            FunctionDefinition(args, lines) => write!(f, "DefineFunction({:?},{:?})", args, lines),
            ReturnStatement(expr) => write!(f, "Return({:?})", expr),
            ListLiteral(elems) => write!(f, "{:?}", elems),
            WhileStatement(cond, lines) => write!(f, "While({:?}, {:?})", cond, lines),
        }
    }
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
                        Ok((expr_node, after)) => match after {
                            [Token::TkComma, after_comma @ ..] => {
                                arg_expressions.push(expr_node);
                                curr_after = after_comma;
                            }
                            _ => {
                                arg_expressions.push(expr_node);
                                curr_after = after;
                            }
                        },
                        Err(_) => {
                            break;
                        }
                    },
                }
            }
            None
        }
        // if there is a function definition, parse that
        [Token::TkFn, Token::TkOpenRound, xs @ ..] => {
            let mut arg_names: Vec<String> = vec![];
            let mut after = xs;

            loop {
                match after {
                    [Token::TkCloseRound, after_close @ ..] => {
                        after = after_close;
                        break;
                    }
                    [Token::TkVariable(var_name), Token::TkComma, tail @ ..] => {
                        arg_names.push(var_name.clone());
                        after = tail;
                    }
                    [Token::TkVariable(var_name), Token::TkCloseRound, tail @ ..] => {
                        arg_names.push(var_name.clone());
                        after = tail;
                        break;
                    }
                    _ => {
                        return None;
                    }
                }
            }
            return match parse_block(after) {
                Ok((ASTNode::Block(lines), after_block)) => {
                    Some((ASTNode::FunctionDefinition(arg_names, lines), after_block))
                }
                _ => None,
            };
        }
        // If there is a list, return it
        [Token::TkOpenSquare, xs @ ..] => {
            let mut elems: Vec<ASTNode> = vec![];
            let mut remaining = xs;
            loop {
                match remaining {
                    [Token::TkCloseSquare, after_close @ ..] => {
                        return Some((ASTNode::ListLiteral(elems), after_close));
                    }
                    x => match parse_expression(x) {
                        Ok((node, after_expr)) => {
                            remaining = match after_expr {
                                [Token::TkComma, after_comma @ ..] => after_comma,
                                x => x,
                            };

                            elems.push(node);
                        }
                        Err(_) => return None,
                    },
                };
            }
        }
        // If there is a number, return it
        [Token::TkNumber(x), ..] => Some((ASTNode::NumberLiteral(x.clone()), &input[1..])),
        // If there is a string literal, return it
        [Token::TkString(s), ..] => Some((ASTNode::StringLiteral(s.clone()), &input[1..])),
        // If there is a bool literal, return it
        [Token::TkBool(b), ..] => Some((ASTNode::BoolLiteral(b.clone()), &input[1..])),
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
                        || binop.clone() == BinaryOperation::DIV
                        || binop.clone() == BinaryOperation::MOD  =>
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
                        || binop.clone() == BinaryOperation::EQUALS
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
        // Parse If Statement
        [Token::TkIf, after_if @ ..] => match parse_expression(after_if) {
            Ok((expr_node, after_expr)) => match parse_block(after_expr) {
                Ok((block_node, after_block)) => match after_block {
                    [Token::TkElse, after_else @ ..] => match parse_statement(after_else) {
                        Ok((else_node, after_else_block)) => Ok((
                            ASTNode::IfElseStatement(
                                Box::new(expr_node),
                                Box::new(block_node),
                                Box::new(ASTNode::Block(vec![else_node])),
                            ),
                            after_else_block,
                        )),
                        Err(e) => Err(e),
                    },
                    _ => Ok((
                        ASTNode::IfStatement(Box::new(expr_node), Box::new(block_node)),
                        after_block,
                    )),
                },
                Err(e) => Err(e),
            },
            _ => Err(format!(
                "Could not parse if statement starting with {:?}",
                input.iter().take(10).collect::<Vec<_>>()
            )),
        },
        // Parse While Statement
        [Token::TkWhile, after_while @ ..] => match parse_expression(after_while) {
            Ok((expr_node, after_expr)) => match parse_block(after_expr) {
                Ok((block_node, after_block)) => Ok((
                    ASTNode::WhileStatement(Box::new(expr_node), Box::new(block_node)),
                    after_block,
                )),
                Err(e) => Err(e),
            },
            _ => Err(format!(
                "Could not parse while statement starting with {:?}",
                input.iter().take(10).collect::<Vec<_>>()
            )),
        },
        // Parse return statement
        [Token::TkReturn, after_return @ ..] => match parse_expression_and_semi(after_return) {
            Ok((node, after_semi)) => Ok((ASTNode::ReturnStatement(Box::new(node)), after_semi)),
            Err(e) => Err(e),
        },

        [Token::TkOpenCurly, ..] => match parse_block(input) {
            Ok(x) => return Ok(x),
            Err(e) => Err(e),
        },

        [Token::TkVariable(var_name), Token::TkEquals, after @ ..] => {
            match parse_expression_and_semi(after) {
                Ok((expr_node, after_semi)) => Ok((
                    ASTNode::AssignStatement(var_name.clone(), Box::new(expr_node)),
                    after_semi,
                )),
                Err(e) => Err(e),
            }
        }
        // Otherwise, assume it's an expression or assignment
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
