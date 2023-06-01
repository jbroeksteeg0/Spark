use crate::interpreter::{Scope, State, Value, Value::*};
use crate::parser::ASTNode;
use std::string::String;

fn to_string(v: &Value) -> String {
    return match v {
        Number(f) => format!("{}", f),
        String(s) => s.clone(),
        Boolean(b) => {
            if b == &true {
                "true".into()
            } else {
                "false".into()
            }
        }
        Function(_, _) => "FunctionObject".into(),
        List(elems) => {
            let mut ret: String = String::from("[");

            for elem in elems {
                if ret != "[" {
                    ret += ","
                }
                ret += to_string(elem).as_str();
            }

            ret += "]";

            ret
        }
        None => "None".into(),
        _ => unimplemented!(),
    };
}
pub fn create_default_scope() -> Scope {
    let mut scope = Scope::new();

    scope.values.insert(
        "println".into(),
        Value::Function(
            vec!["s".into()],
            vec![
                // let string_name = str(s);
                ASTNode::LetStatement(
                    "string_name".into(),
                    Box::new(ASTNode::FunctionCall(
                        "str".into(),
                        vec![ASTNode::Variable("s".into())],
                    )),
                ),
                ASTNode::ReturnStatement(Box::new(ASTNode::BuiltInFunction(
                    |state: &mut State| {
                        let s = String::from("string_name");
                        match state.get_value(&s) {
                            Number(f) => println!("{}", f),
                            String(s) => println!("{}", s),
                            None => println!("None"),
                            _ => unimplemented!(),
                        }

                        Value::None
                    },
                ))),
            ],
        ),
    );

    scope.values.insert(
        "str".into(),
        Value::Function(
            vec!["s".into()],
            vec![ASTNode::ReturnStatement(Box::new(
                ASTNode::BuiltInFunction(|state: &mut State| {
                    return Value::String(to_string(&state.get_value(&String::from("s"))));
                }),
            ))],
        ),
    );

    scope.values.insert(
        "len".into(),
        Value::Function(
            vec!["lst".into()],
            vec![ASTNode::ReturnStatement(Box::new(
                ASTNode::BuiltInFunction(|state: &mut State| {
                    return match state.get_value(&String::from("lst")) {
                        Value::List(elems) => {
                            Value::Number(elems.len() as f64)
                        },
                        _ => {
                            panic!("len expected list");
                        }
                    }
                }),
            ))],
        ),
    );

    scope.values.insert(
        "sub".into(),
        Value::Function(
            vec!["lst".into(), "l".into(), "r".into()],
            vec![ASTNode::ReturnStatement(Box::new(
                ASTNode::BuiltInFunction(|state: &mut State| {
                    let a = state.get_value(&"l".into());
                    let b = state.get_value(&"r".into());
                    let l = state.get_value(&"lst".into());

                    return match (l, a, b) {
                        (Value::List(elems), Value::Number(l_float), Value::Number(r_float)) => {
                            let mut ret_list: Vec<Value> = vec![];
                            if l_float % 1.0f64 != 0.0 || r_float % 1.0f64 != 0.0 {
                                panic!("'sub' expects integral arguments");
                            }
                            let l_ind = l_float as i32;
                            let r_ind = r_float as i32;
                            
                            for idx in l_ind..(r_ind + 1) {
                                if idx < 0 || idx >= elems.len() as i32 {
                                    continue;
                                }
                                ret_list.push(elems.get(idx as usize).unwrap().clone());
                            }

                            Value::List(ret_list)
                        }
                        _ => {
                            panic!("'sub' expects a list and two numbers, other values found");
                        }
                    };
                }),
            ))],
        ),
    );

    scope.values.insert(
        "get".into(),
        Value::Function(
            vec!["lst".into(), "idx".into()],
            vec![ASTNode::ReturnStatement(Box::new(
                ASTNode::BuiltInFunction(|state: &mut State| {
                    let lst = state.get_value(&"lst".into());
                    let idx = state.get_value(&"idx".into());
                    
                    return match (lst, idx) {
                        (Value::List(elems), Value::Number(idx_float)) => {
                            if idx_float % 1.0f64 != 0.0 {
                                panic!("'get' expects integral arguments");
                            }
                            let idx_ind = idx_float as i32;
                            // println!("Given {} to {:?}",idx_float,elems);
                            if idx_ind < 0 || idx_ind >= (elems.len() as i32) {
                                panic!("index provided to 'len' out of bounds");
                            } 

                            let ret = elems.get(idx_ind as usize).unwrap().clone();
                            ret
                        }
                        _ => {
                            panic!("'get' expects a list and two numbers, other values found");
                        }
                    };

                }),
            ))],
        ),
    );
    return scope;
}
