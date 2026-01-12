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
        Expr::Fun { param, body } => Ok(Value::Closure { param: param.clone(), body: body.clone(), env: env.clone() }),
        Expr::Call { fun, arg } => eval_closure(eval_expr(fun, env), eval_expr(arg, env)),
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
    
    result
}

fn eval_closure(fun: Result<Value, String>, arg: Result<Value, String>) -> Result<Value, String> {
    let arg_value;
    match arg {
        Ok(v) => {
            arg_value = v;
        },
        Err(e) => return Err(e),
    };
    
    let result;
    match fun {
        Ok(v) => {
            match v {
                Value::Closure { param, body, mut env } => {
                    env.enter_scope();
                    env.add_variable(param, arg_value);
                    result = eval_expr(&*body, &mut env);
                },
                _ => return Err("This expression is not callable".to_string())
            }
        },
        Err(e) => return Err(e),
    };
    
    result
}

fn binary_operation(left: &Box<Expr>, operation: &Operation, right: &Box<Expr>, env: &mut Env) -> Result<Value, String> {
    
    let left_result = match &**left {
        Expr::Int(i) => Ok(Value::Int(*i)),
        Expr::Float(f) => Ok(Value::Float(*f)),
        Expr::Var(_) => eval_expr(left, env),
        Expr::Binary { left: l, operation: o, right: r } => 
        eval_expr(&Expr::Binary {left: (l.clone()), operation: (o.clone()), right: (r.clone())}, env),
        Expr::Block(statements, final_expr) => eval_block(statements, final_expr, env),
        Expr::Call {fun, arg} => eval_closure(eval_expr(fun, env), eval_expr(arg, env)),
        _ => Err("Type error".to_string()),
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
        Expr::Call {fun, arg} => eval_closure(eval_expr(fun, env), eval_expr(arg, env)),
        _ => Err("Type error".to_string()),
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