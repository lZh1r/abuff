use std::{collections::HashMap};


use crate::{ast::{Expr, Operation, Value}, interpreter::eval};

#[derive(Debug, Clone)]
pub struct Env {
    stack: Vec<HashMap<String, Value>>,
}

impl Env {
    pub fn get(&self, name: &String) -> Option<Value> {
        for scope in self.stack.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val.clone());
            }
        }
        None
    }
}

mod ast;
mod interpreter;

fn main() {
    let mut stack: Env = {
        Env { stack: Vec::new() }
    };
    let res = eval(
        &Expr::Let {
            name: "h".to_string(),
            expr: Box::new(Expr::Int(10)),
            body: Box::new(Expr::Binary {left: Box::new(Expr::Var("h".to_string())), operation: Operation::Add, right: Box::new(Expr::Int(5))})
        },
        &mut stack
    );
    match res {
        Err(e) => println!("{e}"),
        Ok(result) => {
            match result {
                Value::Int(i) => println!("{i}"),
                Value::Bool(b) => println!("{b}"),
                Value::Record(hash_map) => println!("record"),
                Value::Closure { param, body, env } => println!("closure"),
                Value::Float(f) => println!("{f}"),
            }
        }
    }
}
