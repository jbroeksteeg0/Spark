use std::{collections::hash_map::HashMap, rc::Rc};

use crate::parser::ASTNode;
use crate::stdlibrary::create_default_scope;

struct FunctionValue {}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    List(Vec<Box<Value>>),
    Function(Vec<String>, Vec<Box<ASTNode>>),
    Boolean(bool)
    None,
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
struct State {
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

    pub fn get_value(&mut self, name: String) -> Value {
        for scope in self.scopes.iter().rev() {
            if scope.values.contains_key(&name) {
                return scope.values.get(&name).unwrap().clone();
            }
        }

        panic!();
    }
}

fn evaluate_expr(expr: &ASTNode) -> Value {
    match expr {
        ASTNode::NumberLiteral(f) => Value::Number(f.clone()),
        ASTNode::StringLiteral(s) => Value::String(s.clone()),
        _ => unimplemented!(),
    }
}

fn interpret_block(lines: Vec<ASTNode>, state: &mut State) {
    state.push_scope();

    for line in &lines {
        match line {
            ASTNode::LetStatement(name, expr) => {
                state.push_value(name.clone(), evaluate_expr(expr));
            }
            x => {
                unimplemented!()
            }
        };
    }

    state.pop_scope();
}

pub fn interpret(root_block: ASTNode) {
    match root_block {
        ASTNode::Block(lines) => {
            let mut state = State::new();
            interpret_block(lines, &mut state);
        }
        _ => panic!("Interpret should be called on a block"),
    };

    println!("Finished interpreting");
}
