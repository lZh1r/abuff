use crate::{Env, ast::{Expr, Operation, Statement, Value}};

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
        _ => Err("Unknown token".to_string())
    }
}

fn eval_block(stmts: &[Statement], final_expr: &Option<Box<Expr>>, env: &mut Env) -> Result<Value, String> {
    env.enter_scope();
    for statement in stmts {
        match statement {
            Statement::Let { name, expr } => {
                let val = eval_expr(expr, env);
                match val {
                    Ok(v) => env.add_variable(name.clone(), v),
                    Err(e) => return Err(e),
                }
            },
            Statement::Expr(expr) => {
                eval_expr(expr, env)?;
            },
        }
    };
    
    let result = match final_expr {
        Some(expr) => eval_expr(expr, env),
        None => Ok(Value::Void),
    };
    
    env.exit_scope();
    result
}

fn binary_operation(left: &Box<Expr>, operation: &Operation, right: &Box<Expr>, env: &mut Env) -> Result<Value, String> {
    
    let left_result = match &**left {
        Expr::Int(i) => Ok(Value::Int(*i)),
        Expr::Float(f) => Ok(Value::Float(*f)),
        Expr::Var(_) => eval_expr(left, env),
        Expr::Bool(_) => Err("Type error".to_string()),
        Expr::Binary { left: l, operation: o, right: r } => 
        eval_expr(&Expr::Binary {left: (l.clone()), operation: (o.clone()), right: (r.clone())}, env),
        Expr::Block(statements, final_expr) => eval_block(statements, final_expr, env),
    };
    
    let left_value;
    
    match left_result {
        Result::Err(e) => return Err(e),
        Ok(v) => left_value = v
    };
    
    let right_result = match &**right {
        Expr::Int(i) => Ok(Value::Int(*i)),
        Expr::Float(f) => Ok(Value::Float(*f)),
        Expr::Var(_) => eval_expr(right, env),
        Expr::Bool(_) => Err("Type error".to_string()),
        Expr::Binary { left: l, operation: o, right: r } => 
        eval_expr(&Expr::Binary {left: (l.clone()), operation: (o.clone()), right: (r.clone())}, env),
        Expr::Block(statements, final_expr) => eval_block(statements, final_expr, env),
    };
    
    let right_value;
    
    match right_result {
        Result::Err(e) => return Err(e),
        Ok(v) => right_value = v
    };
    
    match (left_value, operation, right_value) {
        (Value::Int(l), Operation::Add, Value::Int(r)) => Ok(Value::Int(l + r)),
        (Value::Int(l), Operation::Subtract, Value::Int(r)) => Ok(Value::Int(l - r)),
        (Value::Int(l), Operation::Multiply, Value::Int(r)) => Ok(Value::Int(l * r)),
        (Value::Int(l), Operation::Devide, Value::Int(r)) => Ok(Value::Int(l / r)),
        (Value::Float(l), Operation::Add, Value::Float(r)) => Ok(Value::Float(l + r)),
        (Value::Float(l), Operation::Subtract, Value::Float(r)) => Ok(Value::Float(l - r)),
        (Value::Float(l), Operation::Multiply, Value::Float(r)) => Ok(Value::Float(l * r)),
        (Value::Float(l), Operation::Devide, Value::Float(r)) => Ok(Value::Float(l / r)),
        _ => Err("Type mismatch".to_string())
    }
}