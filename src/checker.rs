use std::{collections::HashMap, env::current_dir};

use chumsky::span::SimpleSpan;

use crate::{ast::{self, Spanned, TypeInfo}, env::TypeEnv, ir, module::{GlobalRegistry, eval_import, insert_type_module}, native::get_native_fun};

pub fn hoist(statements: &[Spanned<ast::Statement>], env: &mut TypeEnv) -> Result<Vec<Spanned<ast::Statement>>, Spanned<String>> {
    let mut new_statements: Vec<Spanned<ast::Statement>> = Vec::new();
    let mut type_exports = HashMap::new();
    for st in statements.iter() {
        match &st.inner {
            ast::Statement::TypeDef { name, type_info } => {
                let resolved_type = unwrap_custom_type(type_info.clone(), env)?;
                env.add_custom_type(name.clone(), resolved_type);
            },
            ast::Statement::Import { symbols, path } => {
                if env.is_top_level() {
                    let global_reg = GlobalRegistry;
                    let module_types = eval_import(path, &global_reg)?;
                    for (name, alias, is_type) in symbols {
                        let actual_name = alias.as_ref().unwrap_or(&name.inner);
                        if !module_types.contains_key(&name.inner) {
                            return Err(Spanned {
                                inner: format!("Cannot resolve import {} from {}", name.inner, path.to_string()),
                                span: name.span
                            })
                        }
                        if *is_type {
                            env.add_custom_type(actual_name.clone(), module_types.get(&name.inner).unwrap().clone());
                        } else {
                            env.add_var_type(actual_name.clone(), module_types.get(&name.inner).unwrap().clone());
                        }
                        
                    }
                    new_statements.push(st.clone());
                } else {
                    return Err(Spanned {
                        inner: "Non top level imports are prohibited".to_string(),
                        span: st.span
                    });
                }
            }
            _ => new_statements.push(st.clone()),
        }
    }
    for st in statements.iter() {
        match &st.inner {
            ast::Statement::Let { name, expr, type_info } => {
                let inferred_type = unwrap_custom_type(get_type(&expr, env)?, env)?;
                match type_info {
                    Some(t) => {
                        let unwrapped_ti = unwrap_custom_type(t.clone(), env)?;
                        if unwrapped_ti.inner == inferred_type.inner {
                            env.add_var_type(name.clone(), unwrapped_ti);
                        } else {
                            return Err( Spanned {
                                inner: format!("Type mismatch when declaring {name}: expected {:?}, but got {:?}", unwrapped_ti.inner, inferred_type.inner),
                                span: st.span
                            })
                        }
                    },
                    None => env.add_var_type(name.clone(), inferred_type),
                }
            },
            ast::Statement::Fun { name, params, body, return_type } => {
                let r_type = return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: body.span});
                env.add_var_type(name.to_string(), Spanned {
                    inner: TypeInfo::Fun { 
                        args: params.clone(), 
                        return_type: Box::new(r_type.clone()) 
                    },
                    span: st.span
                });
    
                let mut new_scope = env.enter_scope();
                for (param_name, param_type) in params.clone() {
                    new_scope.add_var_type(param_name.to_string(), unwrap_custom_type(param_type.clone(), env)?);
                }
                new_scope.add_var_type("&return".to_string(), r_type.clone());
                let inferred_type = get_type(&body, &mut new_scope)?;
                if inferred_type.inner != r_type.inner {
                    return Err(Spanned {
                        inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                        span: st.span
                    })
                }
            },
            ast::Statement::TypeDef { name: _, type_info: _ } => (),
            ast::Statement::Expr(_) => (),
            ast::Statement::Import { symbols: _, path: _ } => (),
            ast::Statement::Export(spanned) => {
                match &spanned.inner {
                    ast::Statement::Let { name, expr, type_info } => {
                        let inferred_type = unwrap_custom_type(get_type(&expr, env)?, env)?;
                        match type_info { //copy paste but idc
                            Some(t) => {
                                let unwrapped_ti = unwrap_custom_type(t.clone(), env)?;
                                if unwrapped_ti.inner == inferred_type.inner {
                                    env.add_var_type(name.clone(), unwrapped_ti);
                                } else {
                                    return Err( Spanned {
                                        inner: format!(
                                            "Type mismatch when declaring {name}: expected {:?}, but got {:?}", 
                                            unwrapped_ti.inner, 
                                            inferred_type.inner
                                        ),
                                        span: st.span
                                    })
                                }
                            },
                            None => env.add_var_type(name.clone(), inferred_type.clone()),
                        }
                        type_exports.insert(name.clone(), inferred_type);
                    },
                    ast::Statement::Fun { name, params, body, return_type } => {
                        let r_type = unwrap_custom_type(
                            return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: spanned.span}),
                            env
                        )?;
            
                        let mut unwrapped_params = Vec::new();
            
                        for (pname, ti) in params {
                            unwrapped_params.push((pname.clone(), unwrap_custom_type(ti.clone(), env)?))
                        }
            
                        let function_type = Spanned {
                            inner: TypeInfo::Fun { 
                                args: unwrapped_params, 
                                return_type: Box::new(r_type.clone()) 
                            },
                            span: st.span
                        };
            
                        env.add_var_type(name.to_string(), function_type.clone());

                        let mut new_scope = env.enter_scope();
                        for (param_name, param_type) in params.clone() {
                            new_scope.add_var_type(param_name.to_string(), unwrap_custom_type(param_type.clone(), env)?);
                        }
                        new_scope.add_var_type("&return".to_string(), r_type.clone());
                        let inferred_type = get_type(&body, &mut new_scope)?;
                        if inferred_type.inner != r_type.inner {
                            return Err(Spanned {
                                inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                                span: st.span
                            })
                        }
            
                        type_exports.insert(name.clone(), function_type);
                    },
                    ast::Statement::TypeDef { name, type_info } => {
                        let resolved_type = unwrap_custom_type(type_info.clone(), env)?;
                        env.add_custom_type(name.clone(), resolved_type.clone());
                        type_exports.insert(name.clone(), resolved_type);
                    },
                    ast::Statement::Expr(_) => return Err(Spanned {
                        inner: "Cannot export expressions".to_string(),
                        span: spanned.span
                    }),
                    ast::Statement::Import { symbols: _, path: _ } => panic!(),
                    ast::Statement::Export(_) => panic!(),
                    ast::Statement::NativeFun { name, params, return_type } => {
                        if let None = get_native_fun(current_dir().unwrap().to_str().unwrap(), name) {
                            return Err(Spanned {
                                inner: format!("Cannot find native function definition for {}", name),
                                span: st.span
                            })
                        }
                        
                        let r_type = return_type.clone().unwrap_or(Spanned {
                            inner: TypeInfo::Void,
                            span: SimpleSpan::from(0..0)
                        });
                        
                        let mut unwrapped_params = Vec::new();
            
                        for (pname, ti) in params {
                            unwrapped_params.push((pname.clone(), unwrap_custom_type(ti.clone(), env)?))
                        }
                        
                        let function_type = Spanned {
                            inner: TypeInfo::Fun { 
                                args: unwrapped_params, 
                                return_type: Box::new(r_type.clone()) 
                            },
                            span: st.span
                        };
                        
                        env.add_var_type(name.to_string(), function_type.clone());
                        type_exports.insert(name.into(), function_type);
                    },
                }
            },
            ast::Statement::NativeFun { name, params, return_type } => todo!(), //impossible for now
        }
    }
    
    let registry = GlobalRegistry;
    
    insert_type_module(&registry, type_exports, env.clone());
    
    Ok(new_statements)
}

pub fn lower_statement(statement: &Spanned<ast::Statement>, env: &mut TypeEnv) -> Result<Option<Spanned<ir::Statement>>, Spanned<String>> {
    match &statement.inner {
        ast::Statement::TypeDef { name: _, type_info: _ } => Ok(None),
        ast::Statement::Expr(expr) => {
            Ok(Some(Spanned {
                inner: ir::Statement::Expr(lower_expr(&expr, env)?),
                span: statement.span
            }))
        },
        ast::Statement::Let { name, expr, type_info: _ } => {
            Ok(Some(Spanned {
                inner: ir::Statement::Let { name: name.clone(), expr: lower_expr(&expr, env)? },
                span: statement.span
            }))
        }
        ast::Statement::Fun { name, params, body, return_type } => {
            Ok(Some(Spanned {
                inner: ir::Statement::Let { 
                    name: name.clone(),
                    expr: lower_expr(&Spanned {
                        inner: ast::Expr::Fun { 
                            params: params.clone(), 
                            body: Box::new(body.clone()), 
                            return_type: return_type.clone() 
                        },
                        span: statement.span
                    }, env)?
                },
                span: statement.span
            }))
        },
        ast::Statement::Import { symbols, path } => {
            if env.is_top_level() {
                let mut lowered_imports = Vec::new();
                for (symbol, alias, is_type) in symbols {
                    if !is_type {
                        lowered_imports.push((symbol.clone(), alias.clone()));
                    } 
                }
                Ok(Some(Spanned {
                    inner: ir::Statement::Import { symbols: lowered_imports, path: path.clone() },
                    span: statement.span
                }))
            } else {
                Err(Spanned {
                    inner: "Non top level imports are prohibited".to_string(),
                    span: statement.span
                })
            }
        },
        ast::Statement::Export(spanned) => {
            if env.is_top_level() {
                match lower_statement(spanned, env)? {
                    Some(st) => Ok(Some(Spanned {
                        inner: ir::Statement::Export(Box::new(st)),
                        span: statement.span
                    })),
                    None => Ok(None),
                }
        
            } else {
                Err(Spanned {
                    inner: "Non top level exports are prohibited".to_string(),
                    span: statement.span
                })
            }
        },
        ast::Statement::NativeFun { name, params: _, return_type: _ } => {
            Ok(Some(Spanned {
                inner: ir::Statement::NativeFun(name.clone()),
                span: statement.span
            }))
        },
    }
}

pub fn lower_expr(expr: &Spanned<ast::Expr>, env: &mut TypeEnv) -> Result<Spanned<ir::Expr>, Spanned<String>> {
    match &expr.inner {
        ast::Expr::Bool(b) => Ok(Spanned { inner: ir::Expr::Bool(*b), span: expr.span }),
        ast::Expr::Float(f) => Ok(Spanned { inner: ir::Expr::Float(*f), span: expr.span }),
        ast::Expr::Int(i) => Ok(Spanned { inner: ir::Expr::Int(*i), span: expr.span }),
        ast::Expr::Var(v) => {
            let var_type = env.get_var_type(v);
            match var_type {
                Some(_) => {
                    Ok(Spanned { inner: ir::Expr::Var(v.clone()), span: expr.span })
                },
                _ => Err(Spanned {
                    inner: format!("Cannot resolve {v}"),
                    span: expr.span
                })
            }
        },
        ast::Expr::Binary { left, operation, right } => {
            let left_type = get_type(&left, env)?;
            let right_type = get_type(&right, env)?;
            if left_type.inner == right_type.inner {
                Ok(Spanned {
                    inner: ir::Expr::Binary { 
                        left: Box::new(lower_expr(&left, env)?),
                        operation: match operation {
                            ast::Operation::Add => ir::Operation::Add,
                            ast::Operation::Subtract => ir::Operation::Subtract,
                            ast::Operation::Multiply => ir::Operation::Multiply,
                            ast::Operation::Divide => ir::Operation::Divide,
                            ast::Operation::Eq => ir::Operation::Eq,
                            ast::Operation::LessThan => ir::Operation::LessThan,
                            ast::Operation::GreaterThan => ir::Operation::GreaterThan,
                            ast::Operation::NotEq => ir::Operation::NotEq,
                            ast::Operation::LessThanEq => ir::Operation::LessThanEq,
                            ast::Operation::GreaterThanEq => ir::Operation::GreaterThanEq,
                            ast::Operation::And => ir::Operation::And,
                            ast::Operation::Or => ir::Operation::Or,
                            ast::Operation::Modulo => ir::Operation::Modulo,
                        }, 
                        right: Box::new(lower_expr(&right, env)?)
                    },
                    span: expr.span
                })
            } else {
                Err(Spanned { 
                    inner: format!("Type {:?} is not assignable to type {:?}", left_type.inner, right_type.inner),
                    span: expr.span
                })
            }
        },
        ast::Expr::Block(statements, f_expr) => {
            let mut new_scope = env.enter_scope();
            let new_statements = hoist(statements, &mut new_scope)?;
            let mut lowered_statements = Vec::new();
            for s in new_statements {
                match lower_statement(&s, env) {
                    Ok(s) => match s {
                        Some(s) => lowered_statements.push(s),
                        None => (),
                    },
                    Err(e) => return Err(e),
                }
            }
            let final_expr = match f_expr {
                Some(e) => Some(Box::new(lower_expr(&e, &mut new_scope)?)),
                None => None,
            };
            Ok(Spanned { inner: ir::Expr::Block(lowered_statements, final_expr), span: expr.span })
        },
        ast::Expr::Fun { params, body, return_type } => {
            let mut new_scope = env.enter_scope();
            for (param_name, param_type) in params {
                new_scope.add_var_type(param_name.to_string(), param_type.clone());
            }
            let r_type = unwrap_custom_type(return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: expr.span}), env)?;
            new_scope.add_var_type("&return".to_string(), r_type.clone());
            let inferred_type = get_type(&body, &mut new_scope)?;
            
            if r_type.inner == inferred_type.inner {
                Ok(Spanned { 
                    inner: ir::Expr::Fun { params: params.iter().map(|(name, _)| name.to_string()).collect(), body: Box::new(lower_expr(&body, &mut new_scope)?) },
                    span: expr.span
                })
            } else {
                Err(Spanned { 
                    inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                    span: expr.span 
                })
            }
        },
        ast::Expr::Call { fun, args } => {
            match get_type(&fun, env)?.inner {
                TypeInfo::Fun { args: params, return_type: _ } => {
                    let first_param_type = params.get(0).map(|(_, t)| &t.inner).unwrap_or(&TypeInfo::Void);
                    if *first_param_type != TypeInfo::Any {
                        let mut position = 0;
                        for ((_, ti), expr) in params.iter().zip(args.iter()) {
                            let expected_type = unwrap_custom_type(ti.clone(), env)?;
                            let actual_type = get_type(&expr, env)?;
                            if expected_type.inner != actual_type.inner {
                                return Err(Spanned {
                                    inner: format!(
                                        "Argument type mismatch: expected argument #{position} to be {:?}, but got {:?}",
                                        expected_type.inner,
                                        actual_type.inner
                                    ),
                                    span: ti.span
                                })
                            }
                            position += 1;
                        }
                    }
            
                    Ok(Spanned {
                        inner: ir::Expr::Call { 
                            fun: Box::new(lower_expr(&fun, env)?), 
                            args: args.iter().map(|expr| lower_expr(&expr, env).unwrap()).collect() 
                        },
                        span: expr.span
                    })
                },
                t => Err(Spanned {
                    inner: format!("Type {t:?} is not callable"),
                    span: expr.span
                })
            }
        },
        ast::Expr::Record(items) => Ok(Spanned {
            inner: ir::Expr::Record(items.iter().map(|(name, expr)| (name.to_string(), lower_expr(&expr, env).unwrap())).collect()),
            span: expr.span 
        }),
        ast::Expr::Get(object, property_name) => {
            let object_type = get_type(object, env)?;
            match &object_type.inner {
                TypeInfo::Record(record) => {
                    if record.iter().find(|(name, _)| name == property_name).is_some() {
                        return Ok(Spanned { inner: ir::Expr::Get(Box::new(lower_expr(&object, env)?), property_name.to_string()), span: expr.span })
                    } else {
                        return Err(Spanned {
                            inner: format!("Type {:?} doesnt have property {property_name}", object_type.inner),
                            span: object.span
                        })
                    }
                }
                _ => return Err(Spanned {
                    inner: format!("Cannot use accessors on type {:?}", object_type.inner),
                    span: expr.span
                })
            };
        },
        ast::Expr::Assign { target, value } => {
            let target_type = get_type(&target, env)?;
            let value_type = get_type(&value, env)?;
            if target_type.inner == value_type.inner {
                Ok(Spanned { inner: ir::Expr::Assign { target: Box::new(lower_expr(&target, env)?), value: Box::new(lower_expr(&value, env)?) }, span: expr.span })
            } else {
                Err(Spanned { 
                    inner: format!("Type {:?} is not assignable to {:?}", value_type.inner, target_type.inner), 
                    span: expr.span 
                })
            }
        },
        ast::Expr::Unary(unary_op, expr) => {
            match unary_op {
                ast::UnaryOp::Negate => {
                    let expr_type = get_type(&expr, env)?;
                    match expr_type.inner {
                        TypeInfo::Int => Ok(Spanned { inner: ir::Expr::Unary(unary_op.clone(), Box::new(lower_expr(&expr, env)?)), span: expr.span }),
                        TypeInfo::Float => Ok(Spanned { inner: ir::Expr::Unary(unary_op.clone(), Box::new(lower_expr(&expr, env)?)), span: expr.span }),
                        _ => Err(Spanned {
                            inner: format!("Type {:?} cannot be negated", expr_type.inner),
                            span: expr.span
                        })
                    }
                },
                ast::UnaryOp::Not => {
                    let expr_type = get_type(&expr, env)?;
                    match expr_type.inner {
                        TypeInfo::Bool => Ok(Spanned { inner: ir::Expr::Unary(unary_op.clone(), Box::new(lower_expr(&expr, env)?)), span: expr.span }),
                        _ => Err(Spanned {
                            inner: format!("Type {:?} cannot be inverted", expr_type.inner),
                            span: expr.span
                        })
                    }
                },
            }
        },
        ast::Expr::If { condition, body, else_block } => {
            if get_type(&condition, env)?.inner == TypeInfo::Bool {
                match else_block {
                    Some(b) => {
                        let body_type = get_type(&body, env)?;
                        let else_type = get_type(&b, env)?;
                        if body_type.inner == else_type.inner {
                            Ok(Spanned {
                                inner: ir::Expr::If { 
                                    condition: Box::new(lower_expr(&condition, env)?),
                                    body: Box::new(lower_expr(&body, env)?),
                                    else_block: Some(Box::new(lower_expr(&b, env)?)) 
                                },
                                span: expr.span
                            })
                        } else {
                            Err(Spanned {
                                inner: format!("If branches have incompatible types: {:?} and {:?}", body_type.inner, else_type.inner),
                                span: expr.span
                            })
                        }
                    },
                    None => Ok(Spanned {
                        inner: ir::Expr::If { 
                            condition: Box::new(lower_expr(&condition, env)?),
                            body: Box::new(lower_expr(&body, env)?),
                            else_block: None 
                        },
                        span: expr.span
                    }),
                }
            } else {
                return Err(Spanned {
                    inner: "If condition should return Bool".to_string(),
                    span: expr.span
                });
            }
        },
        ast::Expr::While { condition, body } => {
            if get_type(&condition, env)?.inner == TypeInfo::Bool && get_type(&body, env)?.inner == TypeInfo::Void {
                Ok(Spanned { 
                    inner: ir::Expr::While { 
                        condition: Box::new(lower_expr(&condition, env)?),
                        body: Box::new(lower_expr(&body, env)?)
                    }, 
                    span: expr.span
                })
            } else {
                Err(Spanned {
                    inner: "While condition should return Bool and the body should return Void".to_string(),
                    span: expr.span
                })
            }
        },
        ast::Expr::String(s) => Ok(Spanned { inner: ir::Expr::String(s.clone()), span: expr.span }),
        ast::Expr::Break => Ok(Spanned { inner: ir::Expr::Break, span: expr.span }),
        ast::Expr::Continue => Ok(Spanned { inner: ir::Expr::Continue, span: expr.span }),
        ast::Expr::Return(expr) => Ok(Spanned { inner: ir::Expr::Return(Box::new(lower_expr(&expr, env)?)), span: expr.span }),
    }
}

pub fn get_type(expr: &Spanned<ast::Expr>, env: &mut TypeEnv) -> Result<Spanned<TypeInfo>, Spanned<String>> {
    match &expr.inner {
        ast::Expr::Bool(_) => Ok(Spanned{
            inner: TypeInfo::Bool,
            span: expr.span
        }),
        ast::Expr::Float(_) => Ok(Spanned{
            inner: TypeInfo::Float,
            span: expr.span
        }),
        ast::Expr::Int(_) => Ok(Spanned{
            inner: TypeInfo::Int,
            span: expr.span
        }),
        ast::Expr::Var(name) => {
            match env.get_var_type(&name) {
                Some(t) => Ok(unwrap_custom_type(t, env)?),
                None => Err(Spanned {
                    inner: format!("Type of \"{name}\" is unknown"),
                    span: expr.span
                }),
            }
        },
        ast::Expr::Binary { left, operation, right } => {
            let left_type = unwrap_custom_type(get_type(&left, env)?, env)?;
            let right_type = unwrap_custom_type(get_type(&right, env)?, env)?;
            if left_type.inner == right_type.inner {
                match operation {
                    ast::Operation::Eq => Ok(Spanned{
                        inner: TypeInfo::Bool,
                        span: expr.span
                    }),
                    ast::Operation::NotEq => Ok(Spanned{
                        inner: TypeInfo::Bool,
                        span: expr.span
                    }),
                    ast::Operation::LessThan => Ok(Spanned{
                        inner: TypeInfo::Bool,
                        span: expr.span
                    }),
                    ast::Operation::LessThanEq => Ok(Spanned{
                        inner: TypeInfo::Bool,
                        span: expr.span
                    }),
                    ast::Operation::GreaterThan => Ok(Spanned{
                        inner: TypeInfo::Bool,
                        span: expr.span
                    }),
                    ast::Operation::GreaterThanEq => Ok(Spanned{
                        inner: TypeInfo::Bool,
                        span: expr.span
                    }),
                    ast::Operation::And => Ok(Spanned{
                        inner: TypeInfo::Bool,
                        span: expr.span
                    }),
                    ast::Operation::Or => Ok(Spanned{
                        inner: TypeInfo::Bool,
                        span: expr.span
                    }),
                    _ => Ok(left_type)
                }
            } else {
                Err(Spanned {
                    inner: format!("Type {left_type:?} is not assignable to type {right_type:?} ({operation:?})"),
                    span: expr.span
                })
            }
        },
        ast::Expr::Block(statements, tail_expr) => {
            let mut inner_env = env.enter_scope(); 

            for stmt in statements {
                match &stmt.inner {
                    ast::Statement::Let { name, expr, type_info } => {
                        let expr_type = get_type(&expr, &mut inner_env)?;

                        let final_type = match type_info {
                            Some(ann) => {
                                ann.clone() 
                            }
                            None => expr_type
                        };
                        inner_env.add_var_type(name.clone(), final_type);
                    },
                    ast::Statement::TypeDef { name, type_info } => {
                        inner_env.add_custom_type(name.clone(), type_info.clone());
                    },
                    ast::Statement::Expr(expr) => {
                        get_type(&expr, &mut inner_env)?;
                    },
                    ast::Statement::Fun { name, params, body, return_type } => {
                        let mut new_scope = env.enter_scope();
                        for (param_name, param_type) in params {
                            new_scope.add_var_type(param_name.to_string(), param_type.clone());
                        }
                        let r_type = unwrap_custom_type(return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: stmt.span}), env)?;
                        new_scope.add_var_type("&return".to_string(), r_type.clone());
                        let inferred_type = unwrap_custom_type(get_type(&body, &mut new_scope)?, &mut new_scope)?;
                        if r_type.inner != inferred_type.inner {
                            return Err(Spanned {
                                inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                                span: stmt.span
                            })
                        } else {
                            inner_env.add_var_type(name.to_string(), r_type);
                        }
                    },
                    ast::Statement::Import { symbols: _, path: _ } => return Err(Spanned {
                        inner: "Non top level imports are prohibited".to_string(),
                        span: stmt.span
                    }),
                    ast::Statement::Export(_) => return Err(Spanned {
                        inner: "Non top level exports are prohibited".to_string(),
                        span: stmt.span
                    }),
                    ast::Statement::NativeFun { name: _, params: _, return_type: _ } => return Err(Spanned {
                        inner: "Native functions can only be declared at the top level".to_string(),
                        span: stmt.span
                    }),
                }
            }

            match tail_expr {
                Some(e) => get_type(&e, &mut inner_env),
                None => Ok(Spanned {inner: TypeInfo::Void, span: expr.span}),
            }
        },
        ast::Expr::Fun { params, body, return_type } => {
            let mut new_scope = env.enter_scope();
            for (param_name, param_type) in params {
                new_scope.add_var_type(param_name.to_string(), param_type.clone());
            }
            let r_type = unwrap_custom_type(return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: expr.span}), env)?;
            new_scope.add_var_type("&return".to_string(), r_type.clone());
            let inferred_type = unwrap_custom_type(get_type(&body, &mut new_scope)?, &mut new_scope)?;
            
            if r_type.inner != inferred_type.inner {
                return Err(Spanned {
                    inner: format!("Expected {:?} but got {:?}", r_type.inner, inferred_type.inner),
                    span: r_type.span
                })
            }
            Ok(Spanned {
                inner: TypeInfo::Fun { 
                    args: params.clone(), 
                    return_type: Box::new(r_type)
                },
                span: expr.span
            })
        },
        ast::Expr::Call { fun, args } => {
            let fun_type = unwrap_custom_type(get_type(&fun, env)?, env)?;
    
            match fun_type.inner {
                TypeInfo::Fun { args: params, return_type } => {
                    let first_param_type = params.get(0).map(|(_, t)| &t.inner).unwrap_or(&TypeInfo::Void);
                    if *first_param_type != TypeInfo::Any {
                        if params.len() != args.len() {
                            return Err(Spanned {
                                inner: format!("Expected {} args, got {}", params.len(), args.len()),
                                span: expr.span
                            });
                        }
                        for ((_, param_type), arg_expr) in params.iter().zip(args.iter()) {
                            let arg_type = get_type(&arg_expr, env)?;
                            if unwrap_custom_type(param_type.clone(), env)?.inner != unwrap_custom_type(arg_type, env)?.inner {
                                return Err(Spanned {
                                    inner: "Argument type mismatch".to_string(),
                                    span: arg_expr.span
                                });
                            }
                        }
                    }
                    Ok(*return_type.clone())
                },
                _ => Err(Spanned {
                    inner: format!("Attempted to call non-function type: {:?}", fun_type.inner),
                    span: expr.span
                }),
            }
        },
        ast::Expr::Record(items) => {
            let mut record_type = Vec::new();
            for (property_name, expr) in items {
                record_type.push((property_name.to_string(), get_type(&expr, env)?));
            }
            Ok(Spanned {
                inner: TypeInfo::Record(record_type),
                span: expr.span
            })
        },
        ast::Expr::Get(expr, property_name) => {
            println!("hello");
            let record_type = get_type(&expr, env)?;
            match unwrap_custom_type(record_type.clone(), env)?.inner {
                TypeInfo::Record(items) => {
                    let mut result = None;
                    for (prop_name, prop_type) in items {
                        if prop_name == property_name.to_string() {
                            result = Some(prop_type);
                            break;
                        }
                    }
                    match result {
                        Some(t) => Ok(t),
                        None => Err(Spanned {
                            inner: format!("Property {property_name} does not exist on type {:?}", record_type.inner),
                            span: expr.span
                        }),
                    }
                },
                t => Err(Spanned {
                    inner: format!("Type {t:?} does not have property {property_name}"),
                    span: expr.span
                })
            }
        },
        ast::Expr::Assign { target: _, value: _ } => Ok(Spanned {
            inner: TypeInfo::Void,
            span: expr.span
        }),
        ast::Expr::Unary(_, expr) => Ok(get_type(&expr, env)?),
        ast::Expr::If { condition, body, else_block } => {
            if get_type(&condition, env)?.inner == TypeInfo::Bool {
                let body_type = get_type(&body, env)?;
                match else_block {
                    Some(b) => {
                        let else_type = get_type(&b, env)?;
                        if body_type.inner == else_type.inner {
                            Ok(body_type)
                        } else {
                            Err(Spanned {
                                inner: format!("If branches have incompatible types: {:?} and {:?}", body_type.inner, else_type.inner),
                                span: b.span
                            })
                        }
                    },
                    None => Ok(body_type),
                }
            } else {
                return Err(Spanned {
                    inner: "If condition should return Bool".to_string(),
                    span: expr.span
                });
            }
        },
        ast::Expr::While { condition: _, body: _ } => Ok(Spanned {
            inner: TypeInfo::Void,
            span: expr.span
        }),
        ast::Expr::String(_) => Ok(Spanned {
            inner: TypeInfo::String,
            span: expr.span
        }),
        ast::Expr::Break => Ok(Spanned {
            inner: TypeInfo::Void,
            span: expr.span
        }),
        ast::Expr::Continue => Ok(Spanned {
            inner: TypeInfo::Void,
            span: expr.span
        }),
        ast::Expr::Return(expr) => {
            let return_type = get_type(&expr, env)?;
            let expected_type = env.get_var_type("&return");
            match expected_type {
                Some(ti) => {
                    if return_type.inner == ti.inner {
                        Ok(return_type)
                    } else {
                        Err(Spanned { 
                            inner: format!("Return type mismatch: expected {:?}, but got {:?}", ti.inner, return_type.inner),
                            span: ti.span
                        })
                    }
                },
                None => Err(Spanned { 
                    inner: "Unexpected return".to_string(),
                    span: expr.span
                }),
            }
        },
    }
}

pub fn unwrap_custom_type(type_info: Spanned<TypeInfo>, env: &mut TypeEnv) -> Result<Spanned<TypeInfo>, Spanned<String>> {
    match type_info.inner {
        TypeInfo::Custom(type_name) => {
            match env.resolve_type(type_name.as_str()) {
                Some(ti) => Ok(unwrap_custom_type(ti, env)?),
                None => Err(Spanned {
                    inner: format!("Cannot resolve type {type_name}"),
                    span: type_info.span
                }),
            }
        },
        TypeInfo::Record(fields) => {
            let mut new_fields = Vec::new();
            for (name, ti) in fields {
                match unwrap_custom_type(ti, env) {
                    Ok(t) => new_fields.push((name, t)),
                    Err(e) => return Err(e),
                }
            }
            Ok(Spanned {
                inner: TypeInfo::Record(new_fields),
                span: type_info.span
            })
        },
        TypeInfo::Fun { args, return_type } => {
            let mut new_args = Vec::new();
            for (name, ti) in args {
                match unwrap_custom_type(ti.clone(), env) {
                    Ok(t) => new_args.push((name, t)),
                    Err(e) => return Err(e),
                }
            }
            let new_return_type = unwrap_custom_type(*return_type.clone(), env)?;
            Ok(Spanned {
                inner: TypeInfo::Fun { args: new_args, return_type: Box::new(new_return_type) },
                span: type_info.span
            })
        },
        _ => Ok(type_info)
    }
}