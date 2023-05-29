use std::collections::hash_map::HashMap;

enum Value {
    Number(f32),
    String(String),
    List(Vec<Box<Value>>),
}

struct Scope {
    values: HashMap<String, Value>
}

struct State {
    scopes: Vec<Scope>
}