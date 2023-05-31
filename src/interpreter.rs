use std::{collections::hash_map::HashMap, rc::Rc};

use crate::parser::ASTNode;
use crate::stdlibrary::create_default_scope;
use crate::tokeniser::BinaryOperation;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    List(Vec<Box<Value>>),
    Function(Vec<String>, Vec<ASTNode>),
    Boolean(bool),
    None,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a==b,
            _ => panic!("Cannot compare types"),
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

    pub fn get_value(&mut self, name: &String) -> Value {
        for scope in self.scopes.iter().rev() {
            if scope.values.contains_key(name) {
                return scope.values.get(name).unwrap().clone();
            }
        }

        if self.default_scope.values.contains_key(name) {
            return self.default_scope.values.get(name).unwrap().clone();
        }

        panic!();
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

    match (lhs_eval, op, rhs_eval) {
        (Value::Number(l), BinaryOperation::PLUS, Value::Number(r)) => Value::Number(l + r),
        (Value::Number(l), BinaryOperation::MINUS, Value::Number(r)) => Value::Number(l - r),
        (Value::Number(l), BinaryOperation::DIV, Value::Number(r)) => Value::Number(l / r),
        (Value::Number(l), BinaryOperation::TIMES, Value::Number(r)) => Value::Number(l * r),
        (Value::Number(l), BinaryOperation::LE, Value::Number(r)) => Value::Boolean(l <= r),
        (Value::Number(l), BinaryOperation::LESS, Value::Number(r)) => Value::Boolean(l < r),
        (Value::Number(l), BinaryOperation::GE, Value::Number(r)) => Value::Boolean(l >= r),
        (Value::Number(l), BinaryOperation::GREATER, Value::Number(r)) => Value::Boolean(l > r),
        _ => panic!("Could not evaluate binary operation"),
    }
}
fn evaluate_expr(expr: &ASTNode, state: &mut State) -> Value {
    match expr {
        ASTNode::NumberLiteral(f) => Value::Number(f.clone()),
        ASTNode::StringLiteral(s) => Value::String(s.clone()),
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

                return ret_value;
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
        _ => unimplemented!(),
    }
}

fn interpret_if(cond: &ASTNode, lines: &ASTNode, state: &mut State) {
    match (cond, lines) {
        (e, ASTNode::Block(lines)) => {
            if evaluate_expr(e, state) == Value::Boolean(true) {
                interpret_block(lines, state);
            }
        }
        _ => panic!("If statement should be passed lines"),
    };
}

fn interpret_block(lines: &Vec<ASTNode>, state: &mut State) -> Value {
    state.push_scope();

    for line in lines {
        match line {
            ASTNode::LetStatement(name, expr) => {
                let evaluated = evaluate_expr(expr, state);
                state.push_value(name.clone(), evaluated);
            }
            ASTNode::IfStatement(cond, lines) => {
                interpret_if(cond, lines, state);
            }
            ASTNode::ReturnStatement(expr) => {
                return evaluate_expr(expr, state);
            }
            expr => {
                evaluate_expr(line, state);
            }
        };
    }

    state.pop_scope();

    return Value::None;
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
