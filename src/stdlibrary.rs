use crate::interpreter::{Scope, Value, Value::*};
use crate::parser::ASTNode;

pub fn create_default_scope() -> Scope {
    let mut scope = Scope::new();

    scope.values.insert(
        "println".into(),
        Value::Function(
            vec!["s".into()],
            vec![Box::new(ASTNode::BuiltInFunction(|args, scope| {
                match &args[0] {
                    Number(f) => println!("{}", f),
                    String(s) => println!("{}", s),
                    None => println!("None"),
                    _ => unimplemented!(),
                }

                Value::None
            }))],
        ),
    );
    return scope;
}
