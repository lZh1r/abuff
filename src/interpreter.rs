use std::collections::HashMap;

use crate::{env::Env, ir::{Expr, Operation, Statement, Value}};

pub fn eval_expr(expr: &Expr, env: &mut Env) -> Result<Value, String> {
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
        Expr::Block(statements, final_expr) => eval_block(statements, final_expr, env),
        Expr::Fun { param, body } => Ok(Value::Closure { param: param.clone(), body: body.clone(), env: env.clone() }),
        Expr::Call { fun, arg } => eval_closure(eval_expr(fun, env), {
            let mut args = Vec::new();
            for a in arg.iter() {
                args.push(eval_expr(a, env)?);
            };
            args
        }),
        Expr::Record(fields) => {
            let mut result = HashMap::new();
            for (field_name, field_expr) in fields {
                let field_value = {
                    match eval_expr(field_expr, env) {
                        Ok(v) => v,
                        Err(e) => return Err(e),
                    }
                };
                result.insert(field_name.clone(), field_value);
            }
            Ok(Value::Record(result))
        },
        Expr::Get(record_expr, field_name) => {
            match eval_expr(record_expr, env) {
                Ok(v) => match v {
                    Value::Record(field_map) => {
                        match field_map.get(field_name) {
                            Some(value) => Ok(value.clone()),
                            None => Err(format!("Property {field_name} does not exist on this object").to_string()),
                        }
                    },
                    _ => Err("Cannot use an accessor on this".to_string())
                },
                Err(e) => Err(e),
            }
        },
        _ => Err("Unknown token".to_string())
    }
}

fn eval_block(stmts: &[Statement], final_expr: &Option<Box<Expr>>, env: &mut Env) -> Result<Value, String> {
    let mut new_scope = env.enter_scope();
    for statement in stmts {
        match statement {
            Statement::Let { name, expr } => {
                let val = eval_expr(expr, &mut new_scope);
                match val {
                    Ok(v) => {
                        println!("{name}");
                        println!("{:?}", v);
                        new_scope.add_variable(name.clone(), v)
                    },
                    Err(e) => return Err(e),
                }
            },
            Statement::Expr(expr) => {
                eval_expr(expr, &mut new_scope)?;
            },
        }
    };
    
    match final_expr {
        Some(expr) => eval_expr(expr, &mut new_scope),
        None => Ok(Value::Void),
    }
}

fn eval_closure(fun: Result<Value, String>, args: Vec<Value>) -> Result<Value, String> {
    let result;
    match fun {
        Ok(v) => {
            match v {
                Value::Closure { param, body, mut env } => {
                    let mut new_scope = env.enter_scope();
                    for (par, arg) in param.iter().zip(args.iter()) {
                        new_scope.add_variable(par.clone(), arg.clone());
                    }
                    
                    result = eval_expr(&*body, &mut new_scope);
                },
                _ => return Err("This expression is not callable".to_string())
            }
        },
        Err(e) => return Err(e),
    };
    
    result
}

fn binary_operation(left: &Box<Expr>, operation: &Operation, right: &Box<Expr>, env: &mut Env) -> Result<Value, String> {
    
    let left_result = eval_expr(left, env);
    
    let left_value;
    
    match left_result {
        Result::Err(e) => return Err(e),
        Ok(v) => left_value = v
    };
    
    let right_result = eval_expr(right, env);
    
    let right_value;
    
    match right_result {
        Result::Err(e) => return Err(e),
        Ok(v) => right_value = v
    };
    
    match (left_value, operation, right_value) {
        (Value::Int(l), Operation::Add, Value::Int(r)) => Ok(Value::Int(l + r)),
        (Value::Int(l), Operation::Subtract, Value::Int(r)) => Ok(Value::Int(l - r)),
        (Value::Int(l), Operation::Multiply, Value::Int(r)) => Ok(Value::Int(l * r)),
        (Value::Int(l), Operation::Divide, Value::Int(r)) => Ok(Value::Int(l / r)),
        (Value::Float(l), Operation::Add, Value::Float(r)) => Ok(Value::Float(l + r)),
        (Value::Float(l), Operation::Subtract, Value::Float(r)) => Ok(Value::Float(l - r)),
        (Value::Float(l), Operation::Multiply, Value::Float(r)) => Ok(Value::Float(l * r)),
        (Value::Float(l), Operation::Divide, Value::Float(r)) => Ok(Value::Float(l / r)),
        _ => Err("Type mismatch".to_string())
    }
}