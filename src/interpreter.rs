use std::collections::HashMap;

use smol_str::SmolStr;

use crate::{ast::Spanned, ast::Operation, env::Env, ir::{ControlFlow, Expr, MatchArm, Statement, Value}};

pub fn eval_expr(expr: &Spanned<Expr>, env: &mut Env) -> Result<ControlFlow, Spanned<SmolStr>> {
    match &expr.inner {
        Expr::Int(n) => Ok(ControlFlow::Value(Value::Int(*n))),
        Expr::Float(f) => Ok(ControlFlow::Value(Value::Float(*f))),
        Expr::Var(v) => {
            let result = env.get(v);
            match result {
                Some(value) => Ok(ControlFlow::Value(value)),
                _ => Err(Spanned {
                    inner: format!("Runtime error: Cannot resolve {v}").into(),
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
                ControlFlow::Value(v) => match &v {
                    Value::Record(field_map) => {
                        match field_map.get(field_name) {
                            Some(value) => Ok(ControlFlow::Value(value.clone())),
                            None => Err(Spanned {
                                inner: format!("Runtime error: Property {field_name} does not exist on {v}").into(),
                                span: expr.span
                            }),
                        }
                    },
                    _ => Err(Spanned {
                        inner: "Runtime error: Cannot use an accessor on this".into(),
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
                Expr::Index(target, index) => {
                    match &target.inner {
                        Expr::Var(var) => {
                            match env.get(var) {
                                Some(v) => {
                                    match v {
                                        Value::Array(elements) => {
                                            let index = match eval_expr(index, env)? {
                                                ControlFlow::Value(value) | ControlFlow::Return(value) => {
                                                    match value {
                                                        Value::Int(i) => i as usize,
                                                        _ => return Err(Spanned {
                                                            inner: format!("Runtime Error: Cannot index by {value}").into(),
                                                            span: index.span
                                                        })
                                                    }
                                                },
                                                _ => return Err(Spanned {
                                                    inner: format!("Runtime Error: Cannot use a control flow statement as an index").into(),
                                                    span: index.span
                                                })
                                            };
                                            if elements.get(index).is_none() {
                                                return Err(Spanned {
                                                    inner: format!("Runtime Error: Index {index} is out of bounds for length {}", elements.len()).into(),
                                                    span: target.span
                                                })
                                            }
                                            match eval_expr(value, env)? {
                                                //TODO: make some rc refcell things to avoid deep copy
                                                ControlFlow::Return(v) | ControlFlow::Value(v) => {
                                                    let mut new_array = elements.clone();
                                                    new_array[index] = v;
                                                    env.set_variable(var.clone(), Value::Array(new_array));
                                                    Ok(ControlFlow::Value(Value::Void))
                                                }
                                                _ => Err(Spanned {
                                                    inner: format!("Runtime Error: Cannot assign a control flow statement to a value").into(),
                                                    span: target.span
                                                })
                                            }
                                        },
                                        _ => Err(Spanned {
                                            inner: format!("Runtime Error: Cannot index {v}").into(),
                                            span: target.span
                                        })
                                    }
                                },
                                None => Err(Spanned {
                                    inner: format!("Runtime Error: Cannot resolve {var}").into(),
                                    span: target.span
                                })
                            }
                        },
                        Expr::Array(elements) => {
                            let index = match eval_expr(index, env)? {
                                ControlFlow::Value(value) | ControlFlow::Return(value) => {
                                    match value {
                                        Value::Int(i) => i as usize,
                                        _ => return Err(Spanned {
                                            inner: format!("Runtime Error: Cannot index by {value}").into(),
                                            span: index.span
                                        })
                                    }
                                },
                                _ => return Err(Spanned {
                                    inner: format!("Runtime Error: Cannot use a control flow statement as an index").into(),
                                    span: index.span
                                })
                            };
                            if elements.get(index).is_none() {
                                return Err(Spanned {
                                    inner: format!("Runtime Error: Index {index} is out of bounds for length {}", elements.len()).into(),
                                    span: target.span
                                })
                            }
                            Ok(ControlFlow::Value(Value::Void))
                        },
                        _ => Err(Spanned {
                            inner: format!("Runtime Error: Cannot index {:?}", target.inner).into(),
                            span: target.span
                        })
                    }
                },
                t => Err(Spanned {
                    inner: format!("Runtime error: Cannot assign to {t:?}").into(),
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
                                    inner: format!("Runtime error: {v:?} cannot be negated").into(),
                                    span: expr.span
                                })
                            }
                        },
                        crate::ast::UnaryOp::Not => {
                            match v {
                                Value::Bool(b) => Ok(ControlFlow::Value(Value::Bool(!b))),
                                v => Err(Spanned {
                                    inner: format!("Runtime error: {v:?} cannot be inverted").into(),
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
                            inner: format!("Runtime error: {condition_value:?} is not a valid condition").into(),
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
                        inner: format!("Runtime error: {v:?} is not a valid condition value").into(),
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
        Expr::Char(c) => Ok(ControlFlow::Value(Value::Char(c.clone()))),
        Expr::Break => Ok(ControlFlow::Break),
        Expr::Continue => Ok(ControlFlow::Continue),
        Expr::Return(inner_expr) => {
            match eval_expr(inner_expr, env)? {
                ControlFlow::Value(value) => Ok(ControlFlow::Return(value)),
                ControlFlow::Return(value) => Ok(ControlFlow::Return(value)),
                ControlFlow::Break => Err(Spanned {
                    inner: "Runtime error: Expected a value, but found \"break\"".into(),
                    span: expr.span
                }),
                ControlFlow::Continue => Err(Spanned {
                    inner: "Runtime error: Expected a value, but found \"continue\"".into(),
                    span: expr.span
                }),
            }
        },
        Expr::Array(elements) => {
            let mut values = Vec::new();
            for e in elements {
                match eval_expr(e, env)? {
                    ControlFlow::Value(value) => values.push(value),
                    ControlFlow::Return(value) => values.push(value),
                    ControlFlow::Break => return Err(Spanned {
                        inner: "Runtime Error: Expected a value, but got break".into(),
                        span: e.span
                    }),
                    ControlFlow::Continue => return Err(Spanned {
                        inner: "Runtime Error: Expected a value, but got continue".into(),
                        span: e.span
                    }),
                }
            }
            Ok(ControlFlow::Value(Value::Array(values)))
        },
        Expr::Index(target, index) => {
            match eval_expr(target, env)? {
                ControlFlow::Value(value) | ControlFlow::Return(value) => {
                    match value {
                        Value::Array(entries) => {
                            let index = match eval_expr(index, env)? {
                                ControlFlow::Value(v) | ControlFlow::Return(v) => {
                                    match v {
                                        Value::Int(i) => i,
                                        _ => return Err(Spanned {
                                            inner: format!("Runtime Error: Cannot index by {v}").into(),
                                            span: index.span
                                        })
                                    }
                                },
                                _ => return Err(Spanned {
                                    inner: format!("Runtime Error: Cannot index by control flow statements").into(),
                                    span: index.span
                                })
                            };
                            match entries.get(index.clone() as usize) {
                                Some(v) => Ok(ControlFlow::Value(v.clone())),
                                None => Err(Spanned {
                                    inner: format!("Runtime Error: Index {index} is out of bounds for length {}", entries.len()).into(),
                                    span: expr.span
                                }),
                            }
                        },
                        v => Err(Spanned {
                            inner: format!("Runtime Error: Cannot index {v}").into(),
                            span: expr.span
                        })
                    }
                },
                _ => Err(Spanned {
                    inner: "Runtime Error: cannot index that".into(),
                    span: expr.span
                })
            }
        },
        Expr::EnumConstructor { enum_name, variant, value } => {
            match env.get(enum_name) {
                Some(v) => match v {
                    Value::Record(hash_map) => {
                        match hash_map.get(variant) {
                            Some(_) => Ok(ControlFlow::Value(Value::EnumVariant { 
                                enum_name: enum_name.clone(),
                                variant: variant.clone(), 
                                value: Box::new(match eval_expr(value, env)? {
                                    ControlFlow::Value(v) | ControlFlow::Return(v) => v,
                                    _ => return Err(Spanned {
                                        inner: format!("Runtime Error: cannot use break/continue as a value").into(),
                                        span: expr.span
                                    }),
                                })
                            })),
                            None => Err(Spanned {
                                inner: format!("Runtime Error: Enum {enum_name} does not have a variant {variant}").into(),
                                span: expr.span
                            }),
                        }
                    },
                    _ => Err(Spanned {
                        inner: format!("Runtime Error: {enum_name} is not an enum").into(),
                        span: expr.span
                    }),
                },
                None => Err(Spanned {
                    inner: format!("Runtime Error: Cannot resolve enum {enum_name}").into(),
                    span: expr.span
                }),
            }
        },
        Expr::Void => Ok(ControlFlow::Value(Value::Void)),
        Expr::Null => Ok(ControlFlow::Value(Value::Null)),
        Expr::Match { target, branches } => {
            fn match_pattern(
                target: &Spanned<&Value>, 
                pattern: &Spanned<MatchArm>, 
                outcome: &Spanned<Expr>,
                env: &mut Env
            ) -> Result<Option<ControlFlow>, Spanned<SmolStr>> {
                match &pattern.inner {
                    MatchArm::Conditional { alias, condition } => {
                        let mut inner_env = env.clone();
                        inner_env.add_variable(alias.clone(), target.inner.clone());
                        
                        match eval_expr(condition, &mut inner_env)? {
                            ControlFlow::Value(Value::Bool(true)) => {
                                Ok(Some(eval_expr(outcome, &mut inner_env)?))
                            },
                            ControlFlow::Value(Value::Bool(false)) => Ok(None),
                            ControlFlow::Value(v) => Err(Spanned {
                                inner: format!("Runtime Error: match guard must be a Bool, found {:?}", v).into(),
                                span: condition.span
                            }),
                            cf => Err(Spanned {
                                inner: format!("Runtime Error: cannot use {:?} as match guard", cf).into(),
                                span: condition.span
                            })
                        }
                    },
                    MatchArm::Value(pattern_expr) => {
                        let pattern_value = match eval_expr(pattern_expr, env)? {
                            ControlFlow::Value(v) => v,
                            cf => return Err(Spanned {
                                inner: format!("Runtime Error: cannot use {:?} as pattern", cf).into(),
                                span: pattern_expr.span
                            })
                        };
                        
                        if *target.inner == pattern_value {
                            Ok(Some(eval_expr(outcome, env)?))
                        } else {
                            Ok(None)
                        }
                    },
                    MatchArm::Default(alias) => {
                        let mut inner_env = env.clone();
                        inner_env.add_variable(alias.clone(), target.inner.clone());
                        Ok(Some(eval_expr(outcome, &mut inner_env)?))
                    },
                    MatchArm::EnumConstructor { enum_name, variant, alias } => {
                        match target.inner {
                            Value::EnumVariant { enum_name: target_enum, variant: target_variant, value } => {
                                if enum_name == target_enum && variant == target_variant {
                                    let mut inner_env = env.clone();
                                    inner_env.add_variable(alias.clone(), *value.clone());
                                    Ok(Some(eval_expr(outcome, &mut inner_env)?))
                                } else {
                                    Ok(None)
                                }
                            },
                            _ => Ok(None)
                        }
                    }
                }
            }
            let val = eval_expr(target, env)?;
            match &val {
                ControlFlow::Value(value) => {
                    for (pattern, outcome) in branches {
                        if let Some(cf) = match_pattern(&Spanned {inner: value, span: expr.span}, pattern, outcome, env)? {
                            return Ok(cf)
                        }
                    }
                    Err(Spanned {
                        inner: "Runtime Error: non-exhaustive match expression".into(),
                        span: expr.span
                    })
                },
                ControlFlow::Break | ControlFlow::Continue | ControlFlow::Return(_) => Err(Spanned { 
                    inner: "Cannot match control flow statements".into(),
                    span: target.span
                }),
            }
        }
    }
}

fn eval_block(stmts: &[Spanned<Statement>], final_expr: &Option<Box<Spanned<Expr>>>, env: &mut Env) -> Result<ControlFlow, Spanned<SmolStr>> {
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
                inner: "Native functions can only be declared at the top level".into(),
                span: statement.span
            }),
        }
    };

    match final_expr {
        Some(expr) => eval_expr(expr, &mut new_scope),
        None => Ok(ControlFlow::Value(Value::Void)),
    }
}

pub fn eval_closure(fun: ControlFlow, args: Vec<Value>, span: crate::ast::Span) -> Result<ControlFlow, Spanned<SmolStr>> {
    match fun {
        ControlFlow::Value(v) => {
            match v {
                Value::Closure { params: param, body, mut env } => {
                    let mut new_scope = env.enter_scope();
                    if param.len() > 0 {
                        if param.last().unwrap().0 {
                            for ((is_spread, name), arg) in param.iter().zip(args.iter()) {
                                if *is_spread {break}
                                new_scope.add_variable(name.clone(), arg.clone());
                            }
                            let spread_args = &args[param.len() - 1..];
                            let mut zipped_args = Vec::new();
                            for a in spread_args {
                                zipped_args.push(a.clone());
                            }
                            new_scope.add_variable(param.last().unwrap().1.clone(), Value::Array(zipped_args));
                        } else {
                            for ((_, name), arg) in param.iter().zip(args.iter()) {
                                new_scope.add_variable(name.clone(), arg.clone());
                            }
                        }
                    }
                    
                    match eval_expr(&body, &mut new_scope)? {
                        ControlFlow::Return(v) => Ok(ControlFlow::Value(v)),
                        ControlFlow::Value(v) => Ok(ControlFlow::Value(v)),
                        _ => Err(Spanned {
                            inner: "Runtime Error: break/continue is not allowed here".into(),
                            span: body.span
                        }),
                    }
                },
                Value::NativeFun {path: _, name: _, pointer} => {
                    Ok(pointer(&args)?)
                }
                _ => Err(Spanned {
                    inner: "Runtime error: This expression is not callable".into(),
                    span
                })
            }
        },
        cf => Ok(cf),
    }
}

fn binary_operation(
    left: &Box<Spanned<Expr>>,
    operation: &Operation,
    right: &Box<Spanned<Expr>>,
    env: &mut Env,
    span: crate::ast::Span
) -> Result<ControlFlow, Spanned<SmolStr>> {
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
        Operation::NullCoal => Ok(ControlFlow::Value(left_value.nullish_coalescing(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::BitwiseAnd => Ok(ControlFlow::Value(left_value.bitwise_and(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::BitwiseOr => Ok(ControlFlow::Value(left_value.bitwise_or(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::BitwiseXor => Ok(ControlFlow::Value(left_value.bitwise_xor(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::BitwiseLeftShift => Ok(ControlFlow::Value(left_value.bitwise_left_shift(right_value).map_err(|e| Spanned { inner: e, span })?)),
        Operation::BitwiseRightShift => Ok(ControlFlow::Value(left_value.bitwise_right_shift(right_value).map_err(|e| Spanned { inner: e, span })?)),
    }
}