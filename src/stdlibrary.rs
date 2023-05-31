use crate::interpreter::{State, Scope, Value, Value::*};
use crate::parser::ASTNode;
use std::string::String;

pub fn create_default_scope() -> Scope {
    let mut scope = Scope::new();

    scope.values.insert(
        "println".into(),
        Value::Function(
            vec!["s".into()],
            vec![ASTNode::BuiltInFunction(|state: &mut State| {

                let s = String::from("s");
                match state.get_value(&s)  {
                    Number(f) => println!("{}", f),
                    String(s) => println!("{}", s),
                    None => println!("None"),
                    _ => unimplemented!(),
                }

                Value::None
            })],
        ),
    );
    return scope;
}
