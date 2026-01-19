use std::collections::HashMap;

use crate::{env::Env, ir::{Expr, Operation, Statement, Value, ControlFlow}};

pub fn eval_expr(expr: &Expr, env: &mut Env) -> Result<ControlFlow, String> {
    match expr {
        Expr::Int(n) => Ok(ControlFlow::Value(Value::Int(*n))),
        Expr::Float(f) => Ok(ControlFlow::Value(Value::Float(*f))),
        Expr::Var(v) => {
            let result = env.get(v);
            match result {
                Some(value) => Ok(ControlFlow::Value(value)),
                _ => Err(format!("Cannot resolve {v}").to_string())
            }
        },
        Expr::Binary { left, operation, right } => binary_operation(left, operation, right, env),
        Expr::Block(statements, final_expr) => eval_block(statements, final_expr, env),
        Expr::Fun { params: param, body } => Ok(ControlFlow::Value(Value::Closure { params: param.clone(), body: body.clone(), env: env.clone() })),
        Expr::Call { fun, args: arg } => {
            let fun_cf = eval_expr(fun, env)?;
            let mut args = Vec::new();
            for a in arg.iter() {
                match eval_expr(a, env)? {
                    ControlFlow::Value(v) => args.push(v),
                    cf => return Ok(cf),
                }
            };
            eval_closure(fun_cf, args)
        },
        Expr::Record(fields) => {
            let mut result = HashMap::new();
            for (field_name, field_expr) in fields {
                let field_value = {
                    match eval_expr(field_expr, env)? {
                        ControlFlow::Value(v) => v,
                        cf => return Ok(cf),
                    }
                };
                result.insert(field_name.clone(), field_value);
            }
            Ok(ControlFlow::Value(Value::Record(result)))
        },
        Expr::Get(record_expr, field_name) => {
            match eval_expr(record_expr, env)? {
                ControlFlow::Value(v) => match v {
                    Value::Record(field_map) => {
                        match field_map.get(field_name) {
                            Some(value) => Ok(ControlFlow::Value(value.clone())),
                            None => Err(format!("Property {field_name} does not exist on this object").to_string()),
                        }
                    },
                    _ => Err("Cannot use an accessor on this".to_string())
                },
                cf => Ok(cf),
            }
        },
        Expr::Assign { target, value } => {
            match &**target {
                Expr::Var(v) => {
                    let new_value = match eval_expr(value, env)? {
                        ControlFlow::Value(v) => v,
                        cf => return Ok(cf),
                    };
                    env.set_variable(v.clone(), new_value);
                    Ok(ControlFlow::Value(Value::Void))
                },
                t => Err(format!("Cannot assign to {t:?}"))
            }
        },
        Expr::Unary(op, expr) => {
            match eval_expr(expr, env)? {
                ControlFlow::Value(v) => {
                    match op {
                        crate::ast::UnaryOp::Negate => {
                            match v {
                                Value::Float(f) => Ok(ControlFlow::Value(Value::Float(-f))),
                                Value::Int(i) => Ok(ControlFlow::Value(Value::Int(-i))),
                                v => Err(format!("{v:?} cannot be negated"))
                            }
                        },
                        crate::ast::UnaryOp::Not => {
                            match v {
                                Value::Bool(b) => Ok(ControlFlow::Value(Value::Bool(!b))),
                                v => Err(format!("{v:?} cannot be inverted"))
                            }
                        },
                    }
                },
                cf => Ok(cf),
            }
        },
        Expr::Bool(b) => Ok(ControlFlow::Value(Value::Bool(*b))),
        Expr::If { condition, body, else_block } => {
            match eval_expr(condition, env)? {
                ControlFlow::Value(condition_value) => {
                    match condition_value {
                        Value::Bool(b) => {
                            if b {
                                eval_expr(body, env)
                            } else {
                                match else_block {
                                    Some(e) => eval_expr(e, env),
                                    None => Ok(ControlFlow::Value(Value::Void)),
                                }
                            }
                        },
                        _ => Err(format!("{condition_value:?} is not a valid condition"))
                    }
                },
                cf => Ok(cf),
            }
        },
        Expr::While { condition, body } => {
            loop {
                let cond = match eval_expr(condition, env)? {
                    ControlFlow::Value(Value::Bool(b)) => b,
                    ControlFlow::Value(v) => return Err(format!("{v:?} is not a valid condition value")),
                    cf => return Ok(cf),
                };
                
                if !cond { break }
        
                match eval_expr(body, env)? {
                    ControlFlow::Break => break,
                    ControlFlow::Continue => continue,
                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                    ControlFlow::Value(_) => (),
                }
            }
            Ok(ControlFlow::Value(Value::Void))
        },
        Expr::String(s) => Ok(ControlFlow::Value(Value::String(s.clone()))),
        Expr::Break => Ok(ControlFlow::Break),
        Expr::Continue => Ok(ControlFlow::Continue),
        Expr::Return(expr) => {
            match eval_expr(expr, env)? {
                ControlFlow::Value(value) => Ok(ControlFlow::Return(value)),
                ControlFlow::Return(value) => Ok(ControlFlow::Return(value)),
                ControlFlow::Break => Err("Expected a value, but found \"break\"".to_string()),
                ControlFlow::Continue => Err("Expected a value, but found \"continue\"".to_string()),
            }
        },
    }
}

fn eval_block(stmts: &[Statement], final_expr: &Option<Box<Expr>>, env: &mut Env) -> Result<ControlFlow, String> {
    let mut new_scope = env.enter_scope();
    for statement in stmts {
        match statement {
            Statement::Let { name, expr } => {
                match eval_expr(expr, &mut new_scope)? {
                    ControlFlow::Value(v) => new_scope.add_variable(name.clone(), v),
                    cf => return Ok(cf),
                }
            },
            Statement::Expr(expr) => {
                match eval_expr(expr, &mut new_scope)? {
                    ControlFlow::Value(_) => (),
                    cf => return Ok(cf),
                }
            },
        }
    };

    match final_expr {
        Some(expr) => eval_expr(expr, &mut new_scope),
        None => Ok(ControlFlow::Value(Value::Void)),
    }
}

pub fn eval_closure(fun: ControlFlow, args: Vec<Value>) -> Result<ControlFlow, String> {
    match fun {
        ControlFlow::Value(v) => {
            match v {
                Value::Closure { params: param, body, mut env } => {
                    let mut new_scope = env.enter_scope();
                    for (par, arg) in param.iter().zip(args.iter()) {
                        new_scope.add_variable(par.clone(), arg.clone());
                    }

                    match eval_expr(&*body, &mut new_scope)? {
                        ControlFlow::Return(v) => Ok(ControlFlow::Value(v)),
                        ControlFlow::Value(v) => Ok(ControlFlow::Value(v)),
                        cf => Ok(cf), // Break/Continue in a function body? Should probably be handled by checker, but here we propagate.
                    }
                },
                Value::NativeFun(native_fun) => {
                    if args.len() <= native_fun.max_args.unwrap_or(999999) {
                        match (native_fun.function)(args) {
                            Ok(v) => Ok(ControlFlow::Value(v)),
                            Err(e) => Err(e),
                        }
                    } else {
                        Err(format!("Expected {} arguments, but got {}", native_fun.max_args.unwrap_or(999999), args.len()))
                    }
                    
                }
                _ => Err("This expression is not callable".to_string())
            }
        },
        cf => Ok(cf),
    }
}

fn binary_operation(left: &Box<Expr>, operation: &Operation, right: &Box<Expr>, env: &mut Env) -> Result<ControlFlow, String> {
    let left_value = match eval_expr(left, env)? {
        ControlFlow::Value(v) => v,
        cf => return Ok(cf),
    };

    let right_value = match eval_expr(right, env)? {
        ControlFlow::Value(v) => v,
        cf => return Ok(cf),
    };

    match operation {
        Operation::Add => Ok(ControlFlow::Value(left_value.add(right_value)?)),
        Operation::Subtract => Ok(ControlFlow::Value(left_value.subtract(right_value)?)),
        Operation::Multiply => Ok(ControlFlow::Value(left_value.multiply(right_value)?)),
        Operation::Divide => Ok(ControlFlow::Value(left_value.divide(right_value)?)),
        Operation::Eq => Ok(ControlFlow::Value(Value::Bool(left_value == right_value))),
        Operation::LessThan => Ok(ControlFlow::Value(left_value.less_than(right_value)?)),
        Operation::GreaterThan => Ok(ControlFlow::Value(left_value.greater_than(right_value)?)),
        Operation::NotEq => Ok(ControlFlow::Value(Value::Bool(left_value != right_value))),
        Operation::LessThanEq => Ok(ControlFlow::Value(left_value.less_than_eq(right_value)?)),
        Operation::GreaterThanEq => Ok(ControlFlow::Value(left_value.greater_than_eq(right_value)?)),
        Operation::And => Ok(ControlFlow::Value(left_value.logic_and(right_value)?)),
        Operation::Or => Ok(ControlFlow::Value(left_value.logic_or(right_value)?)),
        Operation::Modulo => Ok(ControlFlow::Value(left_value.modulo(right_value)?)),
    }
}