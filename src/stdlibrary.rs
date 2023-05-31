use crate::interpreter::{Scope, State, Value, Value::*};
use crate::parser::ASTNode;
use std::string::String;

pub fn create_default_scope() -> Scope {
    let mut scope = Scope::new();

    scope.values.insert(
        "println".into(),
        Value::Function(
            vec!["s".into()],
            vec![ASTNode::ReturnStatement(Box::new(ASTNode::BuiltInFunction(|state: &mut State| {
                let s = String::from("s");
                match state.get_value(&s) {
                    Number(f) => println!("{}", f),
                    String(s) => println!("{}", s),
                    None => println!("None"),
                    _ => unimplemented!(),
                }

                Value::None
            })))],
        ),
    );

    scope.values.insert(
        "str".into(),
        Value::Function(
            vec!["s".into()],
            vec![ASTNode::ReturnStatement(Box::new(ASTNode::BuiltInFunction(|state: &mut State| {
                let s = String::from("s");
                return match state.get_value(&s) {
                    Number(f) => Value::String(format!("{}",f)),
                    String(s) => Value::String(s),
                    Boolean(b) => Value::String(if b {"true".into()} else {"false".into()}),
                    None => Value::String("None".into()),
                    _ => unimplemented!(),
                };
            })))],
        ),
    );
    return scope;
}
