use std::collections::hash_map::HashMap;
use std::fmt;

use crate::parser::ASTNode;
use crate::stdlibrary::create_default_scope;
use crate::tokeniser::BinaryOperation;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    List(Vec<Value>),
    Function(Vec<String>, Vec<ASTNode>),
    Boolean(bool),
    None,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            _ => panic!("Cannot compare types"),
        }
    }
}
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Number(fl) => write!(f, "{}", fl),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::None => write!(f, "None"),
            Value::Function(_, _) => write!(f, "Function"),
            Value::List(elems) => {
                let mut s = String::from("[");
                for elem in elems {
                    if s != "[" {
                        s += ",";
                    }
                    s += format!("{:?}", elem).as_str();
                }
                s += "]";
                write!(f, "{}", s)
            }
            _ => {
                unimplemented!();
            }
        }
    }
}

#[derive(Clone)]
pub struct Scope {
    pub values: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Self {
        return Scope {
            values: HashMap::new(),
        };
    }
}
pub struct State {
    scopes: Vec<Scope>,
    default_scope: Scope,
}

impl State {
    pub fn new() -> Self {
        return State {
            scopes: vec![],
            default_scope: create_default_scope(),
        };
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        assert!(!self.scopes.is_empty());

        self.scopes.pop();
    }

    pub fn push_value(&mut self, name: String, value: Value) {
        assert!(!self.scopes.is_empty());
        if self
            .scopes
            .last()
            .unwrap()
            .values
            .contains_key(&name.clone())
        {
            panic!("Variable is already defined");
        }

        self.scopes
            .last_mut()
            .unwrap()
            .values
            .insert(name.clone(), value.clone());
    }

    pub fn set_value(&mut self, name: String, value: Value) {
        if self.scopes.iter().any(|x| x.values.contains_key(&name)) {
            self.scopes
                .iter_mut()
                .find(|x| x.values.contains_key(&name))
                .unwrap()
                .values
                .insert(name, value.clone());
            return;
        }
        panic!("Variable is not defined");
    }

    pub fn get_value(&mut self, name: &String) -> Value {
        for scope in self.scopes.iter().rev() {
            if scope.values.contains_key(name) {
                return scope.values.get(name).unwrap().clone();
            }
        }

        if self.default_scope.values.contains_key(name) {
            return self.default_scope.values.get(name).unwrap().clone();
        }

        println!("Could not find variable {}", name);
        panic!();
    }

    fn print_scopes(&self) {
        println!("Current Scope:");
        for scope in self.scopes.iter() {
            println!("  {:?}", scope.values);
        }
    }
}

fn evaluate_binary_op(
    lhs: &ASTNode,
    rhs: &ASTNode,
    op: &BinaryOperation,
    state: &mut State,
) -> Value {
    let lhs_eval = evaluate_expr(lhs, state);
    let rhs_eval = evaluate_expr(rhs, state);
    // println!("{:?} {:?} {:?}",lhs_eval,op,rhs_eval) ;
    match (lhs_eval, op, rhs_eval) {
        // Algebra
        (Value::Number(l), BinaryOperation::PLUS, Value::Number(r)) => Value::Number(l + r),
        (Value::Number(l), BinaryOperation::MINUS, Value::Number(r)) => Value::Number(l - r),
        (Value::Number(l), BinaryOperation::DIV, Value::Number(r)) => Value::Number(l / r),
        (Value::Number(l), BinaryOperation::TIMES, Value::Number(r)) => Value::Number(l * r),
        (Value::Number(l), BinaryOperation::MOD, Value::Number(r)) => Value::Number(l % r),
        (Value::Number(l), BinaryOperation::LE, Value::Number(r)) => Value::Boolean(l <= r),
        (Value::Number(l), BinaryOperation::LESS, Value::Number(r)) => Value::Boolean(l < r),
        (Value::Number(l), BinaryOperation::GE, Value::Number(r)) => Value::Boolean(l >= r),
        (Value::Number(l), BinaryOperation::GREATER, Value::Number(r)) => Value::Boolean(l > r),
        (Value::Number(l), BinaryOperation::EQUALS, Value::Number(r)) => Value::Boolean(l == r),
        // String
        (Value::String(a), BinaryOperation::PLUS, Value::String(b)) => Value::String(a + &b),
        (Value::String(a), BinaryOperation::EQUALS, Value::String(b)) => Value::Boolean(a == b),
        // List
        (Value::List(a), BinaryOperation::EQUALS, Value::List(b)) => Value::Boolean(a == b),
        (Value::List(a), BinaryOperation::PLUS, Value::List(b)) => {
            let mut temp = a.clone();
            temp.append(&mut b.clone());
            Value::List(temp)
        },
        (Value::Boolean(a), BinaryOperation::EQUALS, Value::Boolean(b)) => Value::Boolean(a == b),
        _ => panic!("Could not evaluate binary operation"),
    }
}
fn evaluate_expr(expr: &ASTNode, state: &mut State) -> Value {
    match expr {
        ASTNode::NumberLiteral(f) => Value::Number(f.clone()),
        ASTNode::StringLiteral(s) => Value::String(s.clone()),
        ASTNode::BoolLiteral(b) => Value::Boolean(b.clone()),
        ASTNode::FunctionCall(name, args) => match state.get_value(name) {
            Value::Function(arg_names, lines) => {
                if arg_names.len() != args.len() {
                    panic!("Wrong number of arguments provided to function");
                }

                state.push_scope();

                for (arg_name, arg_expr) in arg_names.iter().zip(args.iter()) {
                    let evaluated = evaluate_expr(arg_expr, state);
                    state.push_value(arg_name.clone(), evaluated);
                }

                let ret_value = interpret_block(&lines, state);

                state.pop_scope();
                return ret_value.unwrap();
            }
            _ => panic!("Cannot use () on non-function variable"),
        },
        ASTNode::BuiltInFunction(f) => {
            return f(state);
        }
        ASTNode::Variable(s) => {
            return state.get_value(s);
        }
        ASTNode::BinaryOperation(l, r, op) => {
            return evaluate_binary_op(&*l, &*r, op, state);
        }
        ASTNode::FunctionDefinition(args, lines) => {
            return Value::Function(args.clone(), lines.clone());
        }
        ASTNode::ListLiteral(elems) => {
            let lst: Vec<Value> = elems
                .iter()
                .map(|elem| evaluate_expr(elem, state))
                .collect();
            return Value::List(lst);
        }
        e => {
            unimplemented!();
        }
    }
}

fn interpret_if(cond: &ASTNode, lines: &ASTNode, state: &mut State) -> Option<Value> {
    match (cond, lines) {
        (e, ASTNode::Block(lines)) => {
            if evaluate_expr(e, state) == Value::Boolean(true) {
                match interpret_block(lines, state) {
                    Some(v) => return Some(v),
                    None => {}
                };
            }
        }
        _ => panic!("If statement should be passed lines"),
    };

    None
}

fn interpret_while(cond: &ASTNode, lines: &ASTNode, state: &mut State) -> Option<Value> {
    loop {
        match (cond, lines) {
            (e, ASTNode::Block(lines)) => {
                if evaluate_expr(e, state) == Value::Boolean(true) {
                    match interpret_block(lines, state) {
                        Some(v) => return Some(v),
                        None => {}
                    };
                } else {
                    break;
                }
            }
            _ => panic!("If statement should be passed lines"),
        };
    }

    None
}

fn interpret_if_else(
    cond: &ASTNode,
    lines: &ASTNode,
    else_lines: &ASTNode,
    state: &mut State,
) -> Option<Value> {
    match (cond, lines, else_lines) {
        (e, ASTNode::Block(lines), ASTNode::Block(else_lines)) => {
            if evaluate_expr(e, state) == Value::Boolean(true) {
                match interpret_block(&lines, state) {
                    Some(v) => return Some(v),
                    None => {}
                }
            } else {
                match interpret_block(&else_lines, state) {
                    Some(v) => return Some(v),
                    None => {}
                }
            }
        }
        _ => panic!("If statement should be passed lines"),
    };

    None
}

fn interpret_block(lines: &Vec<ASTNode>, state: &mut State) -> Option<Value> {
    state.push_scope();
    let mut return_val = None;

    for line in lines {
        match line {
            ASTNode::LetStatement(name, expr) => {
                let evaluated = evaluate_expr(expr, state);
                state.push_value(name.clone(), evaluated);
            }
            ASTNode::AssignStatement(name, expr) => {
                let evaluated = evaluate_expr(expr, state);
                state.set_value(name.clone(), evaluated);
            }
            ASTNode::IfStatement(cond, lines) => match interpret_if(cond, lines, state) {
                Some(ret) => {
                    return_val = Some(ret);
                    break;
                }
                None => {}
            },
            ASTNode::WhileStatement(cond, lines) => match interpret_while(cond, lines, state) {
                Some(ret) => {
                    return_val = Some(ret);
                    break;
                }
                None => {}
            },
            ASTNode::IfElseStatement(cond, true_clause, false_clause) => {
                interpret_if_else(cond, true_clause, false_clause, state);
            }
            ASTNode::ReturnStatement(expr) => {
                return_val = Some(evaluate_expr(expr, state));
                break;
            }
            ASTNode::Block(lines) => match interpret_block(lines, state) {
                Some(ret) => {
                    return_val = Some(ret);
                    break;
                }
                None => {}
            },
            _ => {
                evaluate_expr(line, state);
            }
        };
    }

    state.pop_scope();

    return return_val;
}

pub fn interpret(root_block: ASTNode) {
    match &root_block {
        ASTNode::Block(lines) => {
            let mut state = State::new();
            interpret_block(lines, &mut state);
        }
        _ => panic!("Interpret should be called on a block"),
    };
}
