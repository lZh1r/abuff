use std::collections::HashMap;

use crate::{ast::Spanned, env::Env, ir::{ControlFlow, Expr, Operation, Statement, Value}};

pub fn eval_expr(expr: &Spanned<Expr>, env: &mut Env) -> Result<ControlFlow, Spanned<String>> {
    match &expr.inner {
        Expr::Int(n) => Ok(ControlFlow::Value(Value::Int(*n))),
        Expr::Float(f) => Ok(ControlFlow::Value(Value::Float(*f))),
        Expr::Var(v) => {
            let result = env.get(v);
            match result {
                Some(value) => Ok(ControlFlow::Value(value)),
                _ => Err(Spanned {
                    inner: format!("Runtime error: Cannot resolve {v}"),
                    span: expr.span
                })
            }
        },
        Expr::Binary { left, operation, right } => binary_operation(left, operation, right, env, expr.span),
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
            eval_closure(fun_cf, args, expr.span)
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
                            None => Err(Spanned {
                                inner: format!("Runtime error: Property {field_name} does not exist on this object"),
                                span: expr.span
                            }),
                        }
                    },
                    _ => Err(Spanned {
                        inner: "Runtime error: Cannot use an accessor on this".to_string(),
                        span: expr.span
                    })
                },
                cf => Ok(cf),
            }
        },
        Expr::Assign { target, value } => {
            match &target.inner {
                Expr::Var(v) => {
                    let new_value = match eval_expr(value, env)? {
                        ControlFlow::Value(v) => v,
                        cf => return Ok(cf),
                    };
                    env.set_variable(v.clone(), new_value);
                    Ok(ControlFlow::Value(Value::Void))
                },
                t => Err(Spanned {
                    inner: format!("Runtime error: Cannot assign to {t:?}"),
                    span: expr.span
                })
            }
        },
        Expr::Unary(op, inner_expr) => {
            match eval_expr(inner_expr, env)? {
                ControlFlow::Value(v) => {
                    match op {
                        crate::ast::UnaryOp::Negate => {
                            match v {
                                Value::Float(f) => Ok(ControlFlow::Value(Value::Float(-f))),
                                Value::Int(i) => Ok(ControlFlow::Value(Value::Int(-i))),
                                v => Err(Spanned {
                                    inner: format!("Runtime error: {v:?} cannot be negated"),
                                    span: expr.span
                                })
                            }
                        },
                        crate::ast::UnaryOp::Not => {
                            match v {
                                Value::Bool(b) => Ok(ControlFlow::Value(Value::Bool(!b))),
                                v => Err(Spanned {
                                    inner: format!("Runtime error: {v:?} cannot be inverted"),
                                    span: expr.span
                                })
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
                        _ => Err(Spanned {
                            inner: format!("Runtime error: {condition_value:?} is not a valid condition"),
                            span: expr.span
                        })
                    }
                },
                cf => Ok(cf),
            }
        },
        Expr::While { condition, body } => {
            loop {
                let cond = match eval_expr(condition, env)? {
                    ControlFlow::Value(Value::Bool(b)) => b,
                    ControlFlow::Value(v) => return Err(Spanned {
                        inner: format!("Runtime error: {v:?} is not a valid condition value"),
                        span: expr.span
                    }),
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
        Expr::Return(inner_expr) => {
            match eval_expr(inner_expr, env)? {
                ControlFlow::Value(value) => Ok(ControlFlow::Return(value)),
                ControlFlow::Return(value) => Ok(ControlFlow::Return(value)),
                ControlFlow::Break => Err(Spanned {
                    inner: "Runtime error: Expected a value, but found \"break\"".to_string(),
                    span: expr.span
                }),
                ControlFlow::Continue => Err(Spanned {
                    inner: "Runtime error: Expected a value, but found \"continue\"".to_string(),
                    span: expr.span
                }),
            }
        },
    }
}

fn eval_block(stmts: &[Spanned<Statement>], final_expr: &Option<Box<Spanned<Expr>>>, env: &mut Env) -> Result<ControlFlow, Spanned<String>> {
    let mut new_scope = env.enter_scope();
    for statement in stmts {
        match &statement.inner {
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
            Statement::Import { symbols: _, path: _ } => return Err(Spanned {
                inner: "Non top level imports are prohibited".into(),
                span: statement.span
            }),
            Statement::Export(_) => return Err(Spanned {
                inner: "Non top level exports are prohibited".into(),
                span: statement.span
            }),
            Statement::NativeFun(_) => return Err(Spanned {
                inner: "Native functions can only be declared at the top level".to_string(),
                span: statement.span
            }),
        }
    };

    match final_expr {
        Some(expr) => eval_expr(expr, &mut new_scope),
        None => Ok(ControlFlow::Value(Value::Void)),
    }
}

pub fn eval_closure(fun: ControlFlow, args: Vec<Value>, span: crate::ast::Span) -> Result<ControlFlow, Spanned<String>> {
    match fun {
        ControlFlow::Value(v) => {
            match v {
                Value::Closure { params: param, body, mut env } => {
                    let mut new_scope = env.enter_scope();
                    for (par, arg) in param.iter().zip(args.iter()) {
                        new_scope.add_variable(par.clone(), arg.clone());
                    }

                    match eval_expr(&body, &mut new_scope)? {
                        ControlFlow::Return(v) => Ok(ControlFlow::Value(v)),
                        ControlFlow::Value(v) => Ok(ControlFlow::Value(v)),
                        cf => Ok(cf), // Break/Continue in a function body? Should probably be handled by checker, but here we propagate.
                    }
                },
                Value::NativeFun {path: _, name: _, pointer} => {
                    Ok(pointer(&args)?)
                }
                _ => Err(Spanned {
                    inner: "Runtime error: This expression is not callable".to_string(),
                    span
                })
            }
        },
        cf => Ok(cf),
    }
}

fn binary_operation(left: &Box<Spanned<Expr>>, operation: &Operation, right: &Box<Spanned<Expr>>, env: &mut Env, span: crate::ast::Span) -> Result<ControlFlow, Spanned<String>> {
    let left_value = match eval_expr(left, env)? {
        ControlFlow::Value(v) => v,
        cf => return Ok(cf),
    };

    let right_value = match eval_expr(right, env)? {
        ControlFlow::Value(v) => v,
        cf => return Ok(cf),
    };

    match operation {
        Operation::Add => Ok(ControlFlow::Value(left_value.add(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::Subtract => Ok(ControlFlow::Value(left_value.subtract(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::Multiply => Ok(ControlFlow::Value(left_value.multiply(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::Divide => Ok(ControlFlow::Value(left_value.divide(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::Eq => Ok(ControlFlow::Value(Value::Bool(left_value == right_value))),
        Operation::LessThan => Ok(ControlFlow::Value(left_value.less_than(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::GreaterThan => Ok(ControlFlow::Value(left_value.greater_than(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::NotEq => Ok(ControlFlow::Value(Value::Bool(left_value != right_value))),
        Operation::LessThanEq => Ok(ControlFlow::Value(left_value.less_than_eq(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::GreaterThanEq => Ok(ControlFlow::Value(left_value.greater_than_eq(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::And => Ok(ControlFlow::Value(left_value.logic_and(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::Or => Ok(ControlFlow::Value(left_value.logic_or(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::Modulo => Ok(ControlFlow::Value(left_value.modulo(right_value).map_err(|e| Spanned { inner: e, span })?)),
    }
}