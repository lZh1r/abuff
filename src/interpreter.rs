use std::collections::HashMap;

use crate::{env::Env, ir::{Expr, Operation, Statement, Value}};

pub fn eval_expr(expr: &Expr, env: &mut Env) -> Result<Value, String> {
    match expr {
        Expr::Int(n) => Ok(Value::Int(*n)),
        Expr::Float(f) => Ok(Value::Float(*f)),
        Expr::Var(v) => {
            let result = env.get(v);
            match result {
                Some(value) => Ok(value),
                _ => Err(format!("Cannot resolve {v}").to_string())
            }
        },
        Expr::Binary { left, operation, right } => binary_operation(left, operation, right, env),
        Expr::Block(statements, final_expr) => eval_block(statements, final_expr, env),
        Expr::Fun { params: param, body } => Ok(Value::Closure { params: param.clone(), body: body.clone(), env: env.clone() }),
        Expr::Call { fun, args: arg } => eval_closure(eval_expr(fun, env), {
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
        Expr::Assign { target, value } => {
            match &**target {
                Expr::Var(v) => {
                    let new_value = eval_expr(value, env)?;
                    env.set_variable(v.clone(), new_value);
                    Ok(Value::Void)
                },
                t => Err(format!("Cannot assign to {t:?}"))
            }
        },
        Expr::Unary(op, expr) => {
            match op {
                crate::ast::UnaryOp::Negate => {
                    match eval_expr(expr, env)? {
                        Value::Float(f) => Ok(Value::Float(-f)),
                        Value::Int(i) => Ok(Value::Int(-i)),
                        v => Err(format!("{v:?} cannot be negated"))
                    }
                },
                crate::ast::UnaryOp::Not => {
                    match eval_expr(expr, env)? {
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        v => Err(format!("{v:?} cannot be inverted"))
                    }
                },
            }
        },
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        Expr::If { condition, body, else_block } => {
            let condition_value = eval_expr(condition, env)?;
            match condition_value {
                Value::Bool(b) => {
                    if b {
                        eval_expr(body, env)
                    } else {
                        match else_block {
                            Some(e) => eval_expr(e, env),
                            None => Ok(Value::Void),
                        }
                    }
                },
                _ => Err(format!("{condition_value:?} is not a valid condition"))
            }
        },
        Expr::While { condition, body } => {
            fn check_condition(condition: &Box<Expr>, env: &mut Env) -> Result<bool, String> {
                match eval_expr(condition, env)? {
                    Value::Bool(b) => Ok(b),
                    v => Err(format!("{v:?} is not a vlid condition value"))
                }
            }
            while check_condition(condition, env)? {
                eval_expr(body, env);
            }
            Ok(Value::Void)
        },
        Expr::String(s) => Ok(Value::String(s.clone())),
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
                Value::Closure { params: param, body, mut env } => {
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

    match operation {
        Operation::Add => left_value.add(right_value),
        Operation::Subtract => left_value.subtract(right_value),
        Operation::Multiply => left_value.multiply(right_value),
        Operation::Divide => left_value.divide(right_value),
        Operation::Eq => Ok(Value::Bool(left_value == right_value)),
        Operation::LessThan => left_value.less_than(right_value),
        Operation::GreaterThan => left_value.greater_than(right_value),
        Operation::NotEq => Ok(Value::Bool(left_value != right_value)),
        Operation::LessThanEq => left_value.less_than_eq(right_value),
        Operation::GreaterThanEq => left_value.greater_than_eq(right_value),
        Operation::And => Ok(left_value.logic_and(right_value)?),
        Operation::Or => Ok(left_value.logic_or(right_value)?),
        Operation::Modulo => left_value.modulo(right_value),
    }
}
