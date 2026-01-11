use std::collections::HashMap;

use chumsky::extra::Err;

use crate::{Env, ast::{self, Expr, Operation, Value}};

pub fn eval(expr: &Expr, env: &mut Env) -> Result<Value, String> {
    match expr {
        Expr::Int(n) => Ok(Value::Int(*n)),
        Expr::Var(v) => {
            let result = env.get(v);
            match result {
                Some(value) => Ok(value),
                _ => Err(format!("Cannot resolve {v}").to_string())
            }
        },
        Expr::Binary { left, operation, right } => binary_operation(left, operation, right, env),
        Expr::Let{name, expr, body} => {
            let v = eval(expr, env);
            match v {
                Ok(value) => {
                    let mut new_scope = HashMap::new();
                    new_scope.insert(name.clone(), value);
                    env.stack.push(new_scope);
                    let res = eval(body, env);
                    env.stack.pop();
                    res
                },
                Err(e) => Err(e),
            }
            
        },
        _ => Err("Unknown token".to_string())
    }
}

fn binary_operation(left: &Box<Expr>, operation: &Operation, right: &Box<Expr>, env: &mut Env) -> Result<Value, String> {
    
    let left_result = match operation {
        ast::Operation::Add => {
            match &**left {
                Expr::Int(i) => Ok(Value::Int(*i)),
                Expr::Float(f) => Ok(Value::Float(*f)),
                Expr::Var(_) => eval(left, env),
                Expr::Bool(_) => Err("Type error".to_string()),
                Expr::Binary { left: l, operation: o, right: r } => 
                eval(&Expr::Binary {left: (l.clone()), operation: (o.clone()), right: (r.clone())}, env),
                Expr::Let { name, expr, body } => Err("Unexpected variable declaration".to_string()),
            }
        },
        _ => Err("Not implemented yet".to_string())
    };
    
    let left_value;
    
    match left_result {
        Result::Err(e) => return Err(e),
        Ok(v) => left_value = v
    };
    
    let right_result = match operation {
        ast::Operation::Add => {
            match &**right {
                Expr::Int(i) => Ok(Value::Int(*i)),
                Expr::Float(f) => Ok(Value::Float(*f)),
                Expr::Var(_) => eval(right, env),
                Expr::Bool(_) => Err("Type error".to_string()),
                Expr::Binary { left: l, operation: o, right: r } => 
                eval(&Expr::Binary {left: (l.clone()), operation: (o.clone()), right: (r.clone())}, env),
                Expr::Let { name, expr, body } => Err("Unexpected variable declaration".to_string()),
            }
        },
        _ => Err("Not implemented yet".to_string())
    };
    
    let right_value;
    
    match right_result {
        Result::Err(e) => return Err(e),
        Ok(v) => right_value = v
    };
    
    match (left_value, right_value) {
        (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
        (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
        _ => Err("Type mismatch".to_string())
    }
}