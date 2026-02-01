use std::{cmp, collections::{HashMap, HashSet}, env::current_dir};

use crate::{ast::{self, Span, Spanned, TypeInfo}, env::TypeEnv, ir, module::{GlobalRegistry, eval_import, insert_type_module}, native::get_native_fun};

pub fn hoist(statements: &[Spanned<ast::Statement>], env: &mut TypeEnv) -> Result<Vec<Spanned<ast::Statement>>, Spanned<String>> {
    let mut new_statements: Vec<Spanned<ast::Statement>> = Vec::new();
    let mut var_exports = HashMap::new();
    let mut type_exports = HashMap::new();
    for st in statements.iter() {
        match &st.inner {
            ast::Statement::TypeDef { name, type_info, generic_params } => {
                if generic_params.len() == 0 {
                    let resolved_type = unwrap_custom_type(type_info.clone(), env, false)?;
                    env.add_custom_type(name.clone(), resolved_type);
                } else {
                    let mut generic_scope = env.enter_scope();
                    for generic in generic_params {
                        generic_scope.add_custom_type(generic.inner.clone(), Spanned {
                            inner: TypeInfo::GenericParam(generic.inner.clone()),
                            span: generic.span
                        });
                    }
                    let resolved_type = unwrap_custom_type(type_info.clone(), &mut generic_scope, false)?;
                    env.add_custom_type(name.clone(), Spanned { 
                        inner: TypeInfo::TypeClosure { 
                            params: generic_params.clone(),
                            env: generic_scope,
                            body: Box::new(resolved_type)
                        },
                        span: st.span
                    });
                }
            },
            ast::Statement::Import { symbols, path } => {
                if env.is_top_level() {
                    let global_reg = GlobalRegistry;
                    let module_types = eval_import(path.inner.as_str(), &global_reg)?;
                    for (name, alias, is_type) in symbols {
                        if *is_type {
                            let actual_name = alias.as_ref().unwrap_or(&name.inner);
                            if !module_types.1.contains_key(&name.inner) {
                                return Err(Spanned {
                                    inner: format!("Cannot resolve import {} from {}", name.inner, path.to_string()),
                                    span: name.span
                                })
                            }
                            env.add_custom_type(actual_name.clone(), module_types.1.get(&name.inner).unwrap().clone());
                        } else {
                            let actual_name = alias.as_ref().unwrap_or(&name.inner);
                            if !module_types.0.contains_key(&name.inner) {
                                return Err(Spanned {
                                    inner: format!("Cannot resolve import {} from {}", name.inner, path.to_string()),
                                    span: name.span
                                })
                            }
                            env.add_var_type(actual_name.clone(), module_types.0.get(&name.inner).unwrap().clone());
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
            ast::Statement::EnumDef { name, variants, generic_params } => {
                let mut generic_scope = env.enter_scope();
                for generic in generic_params {
                    generic_scope.add_custom_type(generic.inner.clone(), Spanned {
                        inner: TypeInfo::GenericParam(generic.inner.clone()),
                        span: generic.span
                    });
                }
                let mut variant_map = HashMap::new();
                let mut variant_vec = Vec::new();
                for (variant_name, variant_type) in variants {
                    let param_type = unwrap_custom_type(variant_type.clone().unwrap_or(Spanned {
                        inner: TypeInfo::Void,
                        span: Span::from(0..0)
                    }), &mut generic_scope, false)?;
                    variant_map.insert(variant_name.clone(), param_type.clone());
                    let g_params = get_generic_params(param_type.clone());
                    variant_vec.push((variant_name.clone(), Spanned {
                        inner: TypeInfo::Fun {
                            params: match variant_type {
                                Some(_) => vec![((false, "a".to_string()), param_type.clone())],
                                None => Vec::new(),
                            }, 
                            return_type: Box::new(Spanned {
                                inner: TypeInfo::EnumVariant { enum_name: name.clone(), variant: variant_name.clone() },
                                span: param_type.span
                            }),
                            generic_params: g_params, 
                        },
                        span: param_type.span
                    }));
                }
                env.add_custom_type(name.clone(), Spanned {
                    inner: TypeInfo::Enum { name: name.clone(), variants: variant_map, generic_params: generic_params.clone() },
                    span: st.span
                });
                env.add_var_type(name.clone(), Spanned {
                    inner: TypeInfo::Record(variant_vec),
                    span: st.span
                });
                new_statements.push(st.clone());
            },
            _ => new_statements.push(st.clone()),
        }
    }
    for st in statements.iter() {
        match &st.inner {
            ast::Statement::Let { name, expr, type_info } => {
                let inferred_type = unwrap_custom_type(get_type(&expr, env)?, env, false)?;
                match type_info {
                    Some(t) => {
                        let unwrapped_ti = unwrap_custom_type(t.clone(), env, false)?;
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
            ast::Statement::Fun { name, params, body, return_type, generic_params } => {
                let mut new_scope = env.enter_scope();
                for generic in generic_params {
                    new_scope.add_custom_type(generic.inner.clone(), Spanned {
                        inner: TypeInfo::GenericParam(generic.inner.clone()),
                        span: generic.span
                    });
                }
                
                let mut unwrapped_params = Vec::new();
                let mut spread_encountered = false;
                for ((spread, param_name), param_type) in params {
                    match (spread_encountered, spread) {
                        (true, true) => return Err(Spanned {
                            inner: "A function can only have 1 spread parameter".into(),
                            span: st.span
                        }),
                        (true, false) => return Err(Spanned {
                            inner: "Cannot have parameters after spread parameter".into(),
                            span: st.span
                        }),
                        (false, true) => {
                            spread_encountered = true;
                        },
                        (false, false) => (),
                    }
                    let unwrapped = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?;
                    unwrapped_params.push(((spread.clone(), param_name.clone()), unwrapped.clone()));
                    new_scope.add_var_type(param_name.to_string(), unwrapped);
                }
                
                let r_type = unwrap_custom_type(return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: body.span}), &mut new_scope, false)?;
                
                new_scope.add_var_type("&return".to_string(), r_type.clone());
                env.add_var_type(name.to_string(), Spanned {
                    inner: TypeInfo::Fun { 
                        params: unwrapped_params, 
                        return_type: Box::new(r_type.clone()),
                        generic_params: generic_params.clone(), 
                    },
                    span: st.span
                });
                
                let body_type = get_type(&body, &mut new_scope)?;
                let inferred_type = unwrap_custom_type(body_type, &mut new_scope, false)?;
                if inferred_type.inner != r_type.inner {
                    return Err(Spanned {
                        inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                        span: st.span
                    })
                }
            },
            ast::Statement::TypeDef { name: _, type_info: _, generic_params: _ } => (),
            ast::Statement::Expr(_) => (),
            ast::Statement::Import { symbols: _, path: _ } => (),
            ast::Statement::Export(spanned) => {
                match &spanned.inner {
                    ast::Statement::Let { name, expr, type_info } => {
                        let inferred_type = unwrap_custom_type(get_type(&expr, env)?, env, false)?;
                        match type_info { //copy paste but idc
                            Some(t) => {
                                let unwrapped_ti = unwrap_custom_type(t.clone(), env, false)?;
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
                        var_exports.insert(name.clone(), inferred_type);
                    },
                    ast::Statement::Fun { name, params, body, return_type, generic_params } => {
                        let mut new_scope = env.enter_scope();
                        for generic in generic_params {
                            new_scope.add_custom_type(generic.inner.clone(), Spanned {
                                inner: TypeInfo::GenericParam(generic.inner.clone()),
                                span: generic.span
                            });
                        }
                        
                        let mut unwrapped_params = Vec::new();
                        let mut spread_encountered = false;
                        for ((spread, param_name), param_type) in params.clone() {
                            match (spread_encountered, spread) {
                                (true, true) => return Err(Spanned {
                                    inner: "A function can only have 1 spread parameter".into(),
                                    span: st.span
                                }),
                                (true, false) => return Err(Spanned {
                                    inner: "Cannot have parameters after spread parameter".into(),
                                    span: st.span
                                }),
                                (false, true) => {
                                    spread_encountered = true;
                                },
                                (false, false) => (),
                            }
                
                            if spread {
                                match unwrap_custom_type(param_type.clone(), env, false)?.inner {
                                    TypeInfo::Array(_) => (),
                                    _ => return Err(Spanned {
                                        inner: "Spread param's type should be an array".into(),
                                        span: param_type.span
                                    })
                                };
                            } 
                            let unwrapped = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?;
                            unwrapped_params.push(((spread.clone(), param_name.clone()), unwrapped.clone()));
                            new_scope.add_var_type(param_name.to_string(), unwrapped);
                        }
                        
                        let r_type = unwrap_custom_type(
                            return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: body.span}),
                            &mut new_scope, 
                            false
                        )?;
                        
                        let function_type = Spanned {
                            inner: TypeInfo::Fun { 
                                params: unwrapped_params, 
                                return_type: Box::new(r_type.clone()),
                                generic_params: generic_params.clone(), 
                            },
                            span: st.span
                        };
                        
                        new_scope.add_var_type("&return".to_string(), r_type.clone());
                        env.add_var_type(name.to_string(), function_type.clone());
                        
                        let inferred_type = unwrap_custom_type(get_type(&body, &mut new_scope)?, &mut new_scope, false)?;
                        if inferred_type.inner != r_type.inner {
                            return Err(Spanned {
                                inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                                span: st.span
                            })
                        }
                        
                        var_exports.insert(name.clone(), function_type);
                    },
                    ast::Statement::TypeDef { name, type_info, generic_params } => {
                        let mut generic_scope = env.enter_scope();
                        for generic in generic_params {
                            generic_scope.add_custom_type(generic.inner.clone(), Spanned {
                                inner: TypeInfo::GenericParam(generic.inner.clone()),
                                span: generic.span
                            });
                        }
                        let resolved_type = unwrap_custom_type(type_info.clone(), &mut generic_scope, false)?;
                        env.add_custom_type(name.clone(), resolved_type.clone());
                        type_exports.insert(name.clone(), resolved_type);
                    },
                    ast::Statement::Expr(_) => return Err(Spanned {
                        inner: "Cannot export expressions".to_string(),
                        span: spanned.span
                    }),
                    ast::Statement::Import { symbols: _, path: _ } => panic!(),
                    ast::Statement::Export(_) => panic!(),
                    ast::Statement::NativeFun { name, params, return_type, generic_params } => {
                        if let None = get_native_fun(current_dir().unwrap().to_str().unwrap(), name) {
                            return Err(Spanned {
                                inner: format!("Cannot find native function definition for {}", name),
                                span: st.span
                            })
                        }
                        
                        let mut new_scope = env.enter_scope();
                        for generic in generic_params {
                            new_scope.add_custom_type(generic.inner.clone(), Spanned {
                                inner: TypeInfo::GenericParam(generic.inner.clone()),
                                span: generic.span
                            });
                        }
            
                        let r_type = return_type.clone().unwrap_or(Spanned {
                            inner: TypeInfo::Void,
                            span: Span::from(0..0)
                        });
            
                        let mut unwrapped_params = Vec::new();
                        let mut spread_encountered = false;
                        for ((spread, param_name), param_type) in params.clone() {
                            match (spread_encountered, spread) {
                                (true, true) => return Err(Spanned {
                                    inner: "A function can only have 1 spread parameter".into(),
                                    span: st.span
                                }),
                                (true, false) => return Err(Spanned {
                                    inner: "Cannot have parameters after spread parameter".into(),
                                    span: st.span
                                }),
                                (false, true) => {
                                    spread_encountered = true;
                                },
                                (false, false) => (),
                            }
                
                            if spread {
                                let unwrapped = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?;
                                match unwrapped.inner {
                                    TypeInfo::Array(ti) => unwrapped_params.push((
                                        (spread, param_name),
                                        Spanned {
                                            inner: TypeInfo::Array(ti),
                                            span: unwrapped.span
                                        }
                                    )),
                                    _ => return Err(Spanned {
                                        inner: "Spread param's type should be an array".into(),
                                        span: param_type.span
                                    })
                                };
                            } else {
                                unwrapped_params.push(((spread, param_name), param_type.clone()))
                            }
                            
                        }
            
                        let function_type = Spanned {
                            inner: TypeInfo::Fun { 
                                params: unwrapped_params, 
                                return_type: Box::new(r_type.clone()),
                                generic_params: generic_params.clone(), 
                            },
                            span: st.span
                        };
            
                        env.add_var_type(name.to_string(), function_type.clone());
                        var_exports.insert(name.into(), function_type);
                    },
                    ast::Statement::EnumDef { name, variants, generic_params } => {
                        let mut generic_scope = env.enter_scope();
                        for generic in generic_params {
                            generic_scope.add_custom_type(generic.inner.clone(), Spanned {
                                inner: TypeInfo::GenericParam(generic.inner.clone()),
                                span: generic.span
                            });
                        }
                        let mut variant_map = HashMap::new();
                        let mut variant_vec = Vec::new();
                        for (variant_name, variant_type) in variants {
                            let param_type = unwrap_custom_type(variant_type.clone().unwrap_or(Spanned {
                                inner: TypeInfo::Void,
                                span: Span::from(0..0)
                            }), &mut generic_scope, false)?;
                            variant_map.insert(variant_name.clone(), param_type.clone());
                            let g_params = get_generic_params(param_type.clone());
                            variant_vec.push((variant_name.clone(), Spanned {
                                inner: TypeInfo::Fun {
                                    params: match variant_type {
                                        Some(_) => vec![((false, "a".to_string()), param_type.clone())],
                                        None => Vec::new(),
                                    }, 
                                    return_type: Box::new(Spanned {
                                        inner: TypeInfo::EnumVariant { enum_name: name.clone(), variant: variant_name.clone() },
                                        span: param_type.span
                                    }),
                                    generic_params: g_params, },
                                span: param_type.span
                            }));
                        }
                        
                        let enum_type = Spanned {
                            inner: TypeInfo::Enum { name: name.clone(), variants: variant_map, generic_params: generic_params.clone() },
                            span: st.span
                        };
                        let enum_value_type = Spanned {
                            inner: TypeInfo::Record(variant_vec),
                            span: st.span
                        };
                        
                        var_exports.insert(name.clone(), enum_value_type.clone());
                        type_exports.insert(name.clone(), enum_type.clone());
                        env.add_custom_type(name.clone(), enum_type.clone());
                        env.add_var_type(name.clone(), enum_value_type.clone());
                    },
                }
            },
            ast::Statement::NativeFun { name, params, return_type, generic_params } => todo!(),
            ast::Statement::EnumDef { name: _, variants: _, generic_params: _ } => () //already handled
        }
    }
    
    let registry = GlobalRegistry;
    
    insert_type_module(&registry, var_exports, type_exports, env.clone());
    
    Ok(new_statements)
}

pub fn lower_statement(statement: &Spanned<ast::Statement>, env: &mut TypeEnv) -> Result<Option<Spanned<ir::Statement>>, Spanned<String>> {
    match &statement.inner {
        ast::Statement::TypeDef { name: _, type_info: _, generic_params: _ } => Ok(None),
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
        ast::Statement::Fun { name, params, body, return_type, generic_params } => {
            let mut new_scope = env.enter_scope();
            for generic in generic_params {
                new_scope.add_custom_type(generic.inner.clone(), Spanned {
                    inner: TypeInfo::GenericParam(generic.inner.clone()),
                    span: generic.span
                });
            }
            Ok(Some(Spanned {
                inner: ir::Statement::Let { 
                    name: name.clone(),
                    expr: lower_expr(&Spanned {
                        inner: ast::Expr::Fun { 
                            params: params.clone(), 
                            body: Box::new(body.clone()), 
                            return_type: return_type.clone(),
                            generic_params: generic_params.clone()
                        },
                        span: statement.span
                    }, &mut new_scope)?
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
        ast::Statement::NativeFun { name, params: _, return_type: _, generic_params: _ } => {
            Ok(Some(Spanned {
                inner: ir::Statement::NativeFun(name.clone()),
                span: statement.span
            }))
        },
        ast::Statement::EnumDef { name, variants, generic_params: _ } => {
            let mut record_exprs = Vec::new();
            for (v_name, v_type) in variants{
                match v_type {
                    Some(ti) => {
                        record_exprs.push(
                            (v_name.clone(), Spanned {
                                inner: ir::Expr::Fun { 
                                    params: vec![(false, "a".to_string())], 
                                    body: Box::new(Spanned {
                                        inner: ir::Expr::EnumConstructor { 
                                            enum_name: name.clone(),
                                            variant: v_name.clone(),
                                            value: Box::new(Spanned {
                                                inner: ir::Expr::Var("a".to_string()),
                                                span: ti.span
                                            })
                                        },
                                        span: ti.span
                                    })
                                },
                                span: ti.span
                            })
                        )
                    },
                    None => record_exprs.push(
                        (v_name.clone(), Spanned {
                            inner: ir::Expr::Fun { 
                                params: vec![], 
                                body: Box::new(Spanned {
                                    inner: ir::Expr::EnumConstructor { 
                                        enum_name: name.clone(),
                                        variant: v_name.clone(),
                                        value: Box::new(Spanned {
                                            inner: ir::Expr::Void,
                                            span: Span::from(0..0)
                                        })
                                    },
                                    span: Span::from(0..0)
                                })
                            },
                            span: Span::from(0..0)
                        })
                    )
                }
                
            }
            Ok(Some(Spanned {
                inner: ir::Statement::Let { 
                    name: name.clone(),
                    expr: Spanned {
                        inner: ir::Expr::Record(record_exprs),
                        span: statement.span
                    }
                },
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
        ast::Expr::Fun { params, body, return_type, generic_params } => {
            let mut new_scope = env.enter_scope();
            for generic in generic_params {
                new_scope.add_custom_type(generic.inner.clone(), Spanned {
                    inner: TypeInfo::GenericParam(generic.inner.clone()),
                    span: generic.span
                });
            }
            let mut spread_encountered = false;
            for ((spread, param_name), param_type) in params.clone() {
                match (spread_encountered, spread) {
                    (true, true) => return Err(Spanned {
                        inner: "A function can only have 1 spread parameter".into(),
                        span: expr.span
                    }),
                    (true, false) => return Err(Spanned {
                        inner: "Cannot have parameters after spread parameter".into(),
                        span: expr.span
                    }),
                    (false, true) => {
                        spread_encountered = true;
                    },
                    (false, false) => (),
                }
        
                if spread {
                    match unwrap_custom_type(param_type.clone(), env, false)?.inner {
                        TypeInfo::Array(_) => (),
                        _ => return Err(Spanned {
                            inner: "Spread param's type should be an array".into(),
                            span: param_type.span
                        })
                    };
                } 
                new_scope.add_var_type(param_name.to_string(), unwrap_custom_type(param_type.clone(), env, false)?);
            }
            let r_type = unwrap_custom_type(return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: expr.span}), env, false)?;
            new_scope.add_var_type("&return".to_string(), r_type.clone());
            let inferred_type = get_type(&body, &mut new_scope)?;
    
            if r_type.inner == inferred_type.inner {
                Ok(Spanned { 
                    inner: ir::Expr::Fun { 
                        params: params.iter().map(|(s, _)| s.clone()).collect(),
                        body: Box::new(lower_expr(&body, &mut new_scope)?)
                    },
                    span: expr.span
                })
            } else {
                Err(Spanned { 
                    inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                    span: expr.span 
                })
            }
        },
        ast::Expr::Call { fun, args, generic_args: _ } => {
            let _ = get_type(expr, env)?;
            Ok(Spanned {
                inner: ir::Expr::Call { 
                    fun: Box::new(lower_expr(&fun, env)?), 
                    args: args.iter().map(|expr| lower_expr(&expr, env).unwrap()).collect() 
                },
                span: expr.span
            })
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
            let _ = get_type(expr, env)?;
            match else_block {
                Some(b) => {
                    Ok(Spanned {
                        inner: ir::Expr::If { 
                            condition: Box::new(lower_expr(&condition, env)?),
                            body: Box::new(lower_expr(&body, env)?),
                            else_block: Some(Box::new(lower_expr(&b, env)?)) 
                        },
                        span: expr.span
                    })
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
        ast::Expr::Array(elements) => {
            let _ = get_type(expr, env)?; //checks for type consistency across the whole array
            let mut lowered_elements = Vec::new();
            for e in elements {
                lowered_elements.push(lower_expr(e, env)?);
            }
            Ok(Spanned {
                inner: ir::Expr::Array(lowered_elements),
                span: expr.span
            })
        },
        ast::Expr::Index(target, index) => {
            let _ = get_type(expr, env)?;
            Ok(Spanned {
                inner: ir::Expr::Index(Box::new(lower_expr(target, env)?), Box::new(lower_expr(index, env)?)),
                span: expr.span
            })
        },
        ast::Expr::Void => Ok(Spanned { inner: ir::Expr::Void, span: expr.span }),
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
                Some(t) => Ok(unwrap_custom_type(t, env, false)?),
                None => Err(Spanned {
                    inner: format!("Type of \"{name}\" is unknown"),
                    span: expr.span
                }),
            }
        },
        ast::Expr::Binary { left, operation, right } => {
            let left_type = unwrap_custom_type(get_type(&left, env)?, env, false)?;
            let right_type = unwrap_custom_type(get_type(&right, env)?, env, false)?;
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

            for st in statements {
                match &st.inner {
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
                    ast::Statement::TypeDef { name, type_info, generic_params } => {
                        let mut generic_scope = inner_env.enter_scope();
                        for generic in generic_params {
                            generic_scope.add_custom_type(generic.inner.clone(), Spanned {
                                inner: TypeInfo::GenericParam(generic.inner.clone()),
                                span: generic.span
                            });
                        }
                        let resolved_type = unwrap_custom_type(type_info.clone(), &mut generic_scope, false)?;
                        inner_env.add_custom_type(name.clone(), resolved_type);
                    },
                    ast::Statement::Expr(expr) => {
                        get_type(&expr, &mut inner_env)?;
                    },
                    ast::Statement::Fun { name, params, body, return_type, generic_params } => {
                        let mut new_scope = inner_env.enter_scope();
                        for generic in generic_params {
                            new_scope.add_custom_type(generic.inner.clone(), Spanned {
                                inner: TypeInfo::GenericParam(generic.inner.clone()),
                                span: generic.span
                            });
                        }
                
                        let mut unwrapped_params = Vec::new();
                        let mut spread_encountered = false;
                        for ((spread, param_name), param_type) in params {
                            match (spread_encountered, spread) {
                                (true, true) => return Err(Spanned {
                                    inner: "A function can only have 1 spread parameter".into(),
                                    span: st.span
                                }),
                                (true, false) => return Err(Spanned {
                                    inner: "Cannot have parameters after spread parameter".into(),
                                    span: st.span
                                }),
                                (false, true) => {
                                    spread_encountered = true;
                                },
                                (false, false) => (),
                            }
                            let unwrapped = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?;
                            unwrapped_params.push(((spread.clone(), param_name.clone()), unwrapped.clone()));
                            new_scope.add_var_type(param_name.to_string(), unwrapped);
                        }
                
                        let r_type = unwrap_custom_type(
                            return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: body.span}),
                            &mut new_scope, 
                            false
                        )?;
                
                        new_scope.add_var_type("&return".to_string(), r_type.clone());
                        inner_env.add_var_type(name.to_string(), Spanned {
                            inner: TypeInfo::Fun { 
                                params: unwrapped_params, 
                                return_type: Box::new(r_type.clone()),
                                generic_params: generic_params.clone(), 
                            },
                            span: st.span
                        });
                
                        let inferred_type = unwrap_custom_type(get_type(&body, &mut new_scope)?, &mut new_scope, false)?;
                        if inferred_type.inner != r_type.inner {
                            return Err(Spanned {
                                inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                                span: st.span
                            })
                        }
                    },
                    ast::Statement::Import { symbols: _, path: _ } => return Err(Spanned {
                        inner: "Non top level imports are prohibited".to_string(),
                        span: st.span
                    }),
                    ast::Statement::Export(_) => return Err(Spanned {
                        inner: "Non top level exports are prohibited".to_string(),
                        span: st.span
                    }),
                    ast::Statement::NativeFun { name: _, params: _, return_type: _, generic_params: _ } => return Err(Spanned {
                        inner: "Native functions can only be declared at the top level".to_string(),
                        span: st.span
                    }),
                    ast::Statement::EnumDef { name, variants, generic_params } => {
                        let mut generic_scope = inner_env.enter_scope();
                        for generic in generic_params {
                            generic_scope.add_custom_type(generic.inner.clone(), Spanned {
                                inner: TypeInfo::GenericParam(generic.inner.clone()),
                                span: generic.span
                            });
                        }
                        let mut variant_map = HashMap::new();
                        let mut variant_vec = Vec::new();
                        for (variant_name, variant_type) in variants {
                            let param_type = unwrap_custom_type(variant_type.clone().unwrap_or(Spanned {
                                inner: TypeInfo::Void,
                                span: Span::from(0..0)
                            }), &mut generic_scope, false)?;
                            variant_map.insert(variant_name.clone(), param_type.clone());
                            variant_vec.push((variant_name.clone(), Spanned {
                                inner: TypeInfo::Fun {
                                    params: vec![((false, "a".to_string()), param_type.clone())], return_type: Box::new(Spanned {
                                        inner: TypeInfo::EnumVariant { enum_name: name.clone(), variant: variant_name.clone() },
                                        span: param_type.span
                                    }),
                                    generic_params: vec![], },
                                span: param_type.span
                            }));
                        }
                        inner_env.add_custom_type(name.clone(), Spanned {
                            inner: TypeInfo::Enum { name: name.clone(), variants: variant_map, generic_params: generic_params.clone() },
                            span: st.span
                        });
                        inner_env.add_var_type(name.clone(), Spanned {
                            inner: TypeInfo::Record(variant_vec),
                            span: st.span
                        });
                    },
                }
            }

            match tail_expr {
                Some(e) => get_type(&e, &mut inner_env),
                None => Ok(Spanned {inner: TypeInfo::Void, span: expr.span}),
            }
        },
        ast::Expr::Fun { params, body, return_type, generic_params } => {
            let mut new_scope = env.enter_scope();
            for generic in generic_params {
                new_scope.add_custom_type(generic.inner.clone(), Spanned {
                    inner: TypeInfo::GenericParam(generic.inner.clone()),
                    span: generic.span
                });
            }
    
            let mut unwrapped_params = Vec::new();
            let mut spread_encountered = false;
            for ((spread, param_name), param_type) in params {
                match (spread_encountered, spread) {
                    (true, true) => return Err(Spanned {
                        inner: "A function can only have 1 spread parameter".into(),
                        span: expr.span
                    }),
                    (true, false) => return Err(Spanned {
                        inner: "Cannot have parameters after spread parameter".into(),
                        span: expr.span
                    }),
                    (false, true) => {
                        spread_encountered = true;
                    },
                    (false, false) => (),
                }
                let unwrapped = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?;
                unwrapped_params.push(((spread.clone(), param_name.clone()), unwrapped.clone()));
                new_scope.add_var_type(param_name.to_string(), unwrapped);
            }
    
            let r_type = unwrap_custom_type(
                return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: body.span}),
                &mut new_scope, 
                false
            )?;
    
    
            new_scope.add_var_type("&return".to_string(), r_type.clone());
    
            let inferred_type = unwrap_custom_type(get_type(&body, &mut new_scope)?, &mut new_scope, false)?;
            if inferred_type.inner != r_type.inner {
                return Err(Spanned {
                    inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                    span: expr.span
                })
            }
            Ok(Spanned {
                inner: TypeInfo::Fun { 
                    params: unwrapped_params, 
                    return_type: Box::new(r_type.clone()),
                    generic_params: generic_params.clone(), 
                },
                span: expr.span
            })
        },
        ast::Expr::Call { fun, args, generic_args } => {
            let fun_type = unwrap_custom_type(get_type(&fun, env)?, env, true)?;
            match fun_type.inner {
                TypeInfo::Fun { params, return_type, generic_params } => {
                    let mut new_scope = env.enter_scope();
                    let mut is_variadic = false;
                    //convert GenericParam into Custom
                    let mut new_params = Vec::new();
                    for ((spread, name), ti) in &params {
                        let ti = unwrap_generic(ti)?;
                        new_params.push(((spread.clone(), name.clone()), ti.clone()));
                        if spread.clone() {
                            is_variadic = true;
                            break;
                        }
                    }
                    let params = new_params;
                    if generic_args.len() > generic_params.len() {
                        return Err(Spanned {
                            inner: "Too many generic arguments provided".into(),
                            span: Span::from(generic_args.first().unwrap().span.start..generic_args.last().unwrap().span.end)
                        })
                    }
                    if generic_args.len() != 0 && generic_args.len() != generic_params.len() {
                        return Err(Spanned {
                            inner: "Partially supplied generic arguments are not allowed".into(),
                            span: Span::from(generic_args.first().unwrap().span.start..generic_args.last().unwrap().span.end)
                        })
                    }
                    match is_variadic {
                        true => {
                            if params.len() > args.len() {
                                return Err(Spanned {
                                    inner: format!("Expected at least {} args, got {}", params.len(), args.len()),
                                    span: expr.span
                                });
                            }
                            //a check for implicitly vs explicitly supplied generic args
                            match generic_args.len() == generic_params.len() {
                                true => {
                                    for (g_name, g_type) in generic_params.iter().zip(generic_args.iter()) {
                                        let unwrapped = unwrap_custom_type(g_type.clone(), &mut new_scope, false)?;
                                        new_scope.add_custom_type(g_name.inner.clone(), unwrapped);
                                    }
                                    for (((spread, _), param_type), arg_expr) in params.iter().zip(args.iter()) {
                                        if spread.clone() {break}
                                        let arg_type = get_type(&arg_expr, &mut new_scope)?;
                                        let param_type = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?.inner;
                                        let arg_type = unwrap_custom_type(arg_type, &mut new_scope, false)?.inner;
                                        if param_type != arg_type {
                                            return Err(Spanned {
                                                inner: format!("Argument type mismatch: expected {:?}, got {:?}", param_type, arg_type),
                                                span: arg_expr.span
                                            });
                                        }
                                    }
            
                                    let spread_param_type = params.last().unwrap().1.clone();
                                    let spread_args = &args[cmp::max(params.len() - 1, 0)..];
                                    let expected_spread_type = match unwrap_custom_type(spread_param_type.clone(), &mut new_scope, false)?.inner {
                                        TypeInfo::Array(a) => a.inner,
                                        _ => return Err(Spanned {
                                            inner: "Spread param's type should be an array".into(),
                                            span: spread_param_type.span
                                        })
                                    };
                                    for arg_expr in spread_args {
                                        let arg_type = get_type(arg_expr, &mut new_scope)?;
                                        if expected_spread_type != unwrap_custom_type(arg_type, &mut new_scope, false)?.inner {
                                            return Err(Spanned {
                                                inner: "Argument type mismatch".to_string(),
                                                span: arg_expr.span
                                            });
                                        }
                                    }
                                },
                                false => {
                                    let mut generic_set = HashSet::new();
                                    for g in generic_params {
                                        generic_set.insert(g.inner);
                                    }
                                    for (((spread, _), param_type), arg_expr) in params.iter().zip(args.iter()) {
                                        if spread.clone() {break}
                                        let arg_type = unwrap_custom_type(get_type(&arg_expr, &mut new_scope)?, &mut new_scope, false)?;
                                        let mut p_type = &param_type.inner;
                                        let mut a_type = &arg_type.inner;
                                        loop {
                                            match (p_type, a_type) {
                                                (TypeInfo::Array(ti), TypeInfo::Array(a_ti)) => {
                                                    p_type = &ti.inner;
                                                    a_type = &a_ti.inner;
                                                },
                                                (TypeInfo::Custom { name, generic_args: _ }, ti) => {
                                                    if generic_set.get(name).is_some() {
                                                        new_scope.add_custom_type(name.clone(), Spanned {inner: ti.clone(), span: arg_expr.span});
                                                    }
                                                    break
                                                },
                                                (TypeInfo::Array(_), ti) => {
                                                    return Err(Spanned { inner: format!("{ti:?} is not an array"), span: arg_expr.span })
                                                },
                                                _ => break
                                            };
                                        }
                                        let param_type = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?.inner;
                                        if param_type != arg_type.inner {
                                            return Err(Spanned {
                                                inner: format!("Argument type mismatch: expected {:?}, got {:?}", param_type, arg_type),
                                                span: arg_expr.span
                                            });
                                        }
                                    }
                                    
                                    let spread_param_type = params.last().unwrap().1.clone();
                                    println!("{spread_param_type:?}");
                                    let spread_args = &args[cmp::max(params.len() - 1, 0)..];
                                    
                                    let unwrapped_spread_type = unwrap_custom_type(spread_param_type.clone(), &mut new_scope, true)?;
                                    match &unwrapped_spread_type.inner {
                                        TypeInfo::Array(ti) => {
                                            match &ti.inner {
                                                TypeInfo::Custom { name, generic_args: _ } => {
                                                    if generic_set.get(name).is_some() {
                                                        let unwrapped = unwrap_custom_type(
                                                            get_type(spread_args.first().unwrap(), &mut new_scope)?,
                                                            &mut new_scope,
                                                            false
                                                        )?;
                                                        new_scope.add_custom_type(name.clone(), unwrapped);
                                                    }
                                                }
                                                _ => ()
                                            };
                                        },
                                        TypeInfo::Custom { name, generic_args: _ } => {
                                            if generic_set.get(name).is_some() {
                                                let unwrapped = unwrap_custom_type(
                                                    get_type(spread_args.first().unwrap(), &mut new_scope)?,
                                                    &mut new_scope,
                                                    false
                                                )?;
                                                new_scope.add_custom_type(name.clone(), unwrapped);
                                            }
                                        }
                                        _ => ()
                                    };
                                    
                                    let expected_spread_type = match unwrap_custom_type(spread_param_type.clone(), &mut new_scope, false)?.inner {
                                        TypeInfo::Array(a) => a.inner,
                                        _ => return Err(Spanned {
                                            inner: "Spread param's type should be an array".into(),
                                            span: spread_param_type.span
                                        })
                                    };
                                    for arg_expr in spread_args {
                                        let arg_type = get_type(arg_expr, &mut new_scope)?;
                                        if expected_spread_type != unwrap_custom_type(arg_type, &mut new_scope, false)?.inner {
                                            return Err(Spanned {
                                                inner: "Argument type mismatch".to_string(),
                                                span: arg_expr.span
                                            });
                                        }
                                    }
                                },
                            }
                        },
                        false => {
                            if params.len() != args.len() {
                                return Err(Spanned {
                                    inner: format!("Expected {} args, got {}", params.len(), args.len()),
                                    span: expr.span
                                });
                            }
                            //a check for implicitly vs explicitly supplied generic args
                            match generic_args.len() == generic_params.len() {
                                true => {
                                    for (g_name, g_type) in generic_params.iter().zip(generic_args.iter()) {
                                        let unwrapped = unwrap_custom_type(g_type.clone(), &mut new_scope, false)?;
                                        new_scope.add_custom_type(g_name.inner.clone(), unwrapped);
                                    }
                                    for ((_, param_type), arg_expr) in params.iter().zip(args.iter()) {
                                        let arg_type = get_type(&arg_expr, &mut new_scope)?;
                                        let param_type = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?.inner;
                                        let arg_type = unwrap_custom_type(arg_type, &mut new_scope, false)?.inner;
                                        if param_type != arg_type {
                                            return Err(Spanned {
                                                inner: format!("Argument type mismatch: expected {:?}, got {:?}", param_type, arg_type),
                                                span: arg_expr.span
                                            });
                                        }
                                    }
                                },
                                false => {
                                    let mut generic_set = HashSet::new();
                                    for g in generic_params {
                                        generic_set.insert(g.inner);
                                    }
                                    for ((_, param_type), arg_expr) in params.iter().zip(args.iter()) {
                                        let arg_type = unwrap_custom_type(get_type(&arg_expr, &mut new_scope)?, &mut new_scope, false)?;
                                        let mut p_type = &param_type.inner;
                                        let mut a_type = &arg_type.inner;
                                        loop {
                                            match (p_type, a_type) {
                                                (TypeInfo::Array(ti), TypeInfo::Array(a_ti)) => {
                                                    p_type = &ti.inner;
                                                    a_type = &a_ti.inner;
                                                },
                                                (TypeInfo::Custom { name, generic_args: _ }, ti) => {
                                                    if generic_set.get(name).is_some() {
                                                        new_scope.add_custom_type(name.clone(), Spanned {inner: ti.clone(), span: arg_expr.span});
                                                    }
                                                    break
                                                },
                                                (TypeInfo::Array(_), ti) => {
                                                    return Err(Spanned { inner: format!("{ti:?} is not an array"), span: arg_expr.span })
                                                },
                                                _ => break
                                            };
                                        }
                                        let param_type = unwrap_custom_type(param_type.clone(), &mut new_scope, false)?.inner;
                                        if param_type != arg_type.inner {
                                            return Err(Spanned {
                                                inner: format!("Argument type mismatch: expected {:?}, got {:?}", param_type, arg_type),
                                                span: arg_expr.span
                                            });
                                        }
                                    }
                                },
                            }
                    
                        },
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
            let record_type = get_type(&expr, env)?;
            match unwrap_custom_type(record_type.clone(), env, false)?.inner {
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
        ast::Expr::Array(elements) => {
            let mut first_type = TypeInfo::Any;
            for (i, e) in elements.iter().enumerate() {
                if i == 0 {
                    first_type = get_type(e, env)?.inner
                } else if first_type != get_type(e, env)?.inner {
                    return Err(Spanned {
                        inner: "Array elements should have the same type".to_string(),
                        span: expr.span
                    })
                }
            }
            Ok(Spanned {
                inner: TypeInfo::Array(Box::new(Spanned {
                    inner: first_type,
                    span: expr.span
                })),
                span: expr.span
            })
        },
        ast::Expr::Index(e, index) => {
            let index_type = get_type(index, env)?;
            match index_type.inner {
                TypeInfo::Int => (),
                _ => return Err(Spanned {
                    inner: format!("Type {:?} can not be used as an index", index_type.inner),
                    span: index.span
                })
            }
            let target_type = get_type(e, env)?;
            match target_type.inner {
                TypeInfo::Array(ti) => Ok(Spanned {
                    inner: ti.inner,
                    span: e.span
                }),
                _ => Err(Spanned {
                    inner: format!("Type {:?} is not indexable", target_type.inner),
                    span: e.span
                })
            }
        },
        ast::Expr::Void => Ok(Spanned { inner: TypeInfo::Void, span: expr.span }),
    }
}

pub fn unwrap_custom_type(type_info: Spanned<TypeInfo>, env: &mut TypeEnv, shallow: bool) -> Result<Spanned<TypeInfo>, Spanned<String>> {
    match type_info.inner.clone() {
        //TODO: make a separate node for generic types that acts like a closure on types
        TypeInfo::Custom {name, generic_args } => {
            match shallow {
                false => {
                    match generic_args.len() {
                        0 => match env.resolve_type(name.as_str()) {
                            Some(ti) => Ok(unwrap_custom_type(ti, env, shallow)?),
                            None => Err(Spanned {
                                inner: format!("Cannot resolve type {name}"),
                                span: type_info.span
                            }),
                        },
                        _ => {
                            let mut unwrapped_args = Vec::new();
                            for arg in &generic_args {
                                unwrapped_args.push(unwrap_custom_type(arg.clone(), env, shallow)?);
                            }
                            let (generic_params, mut scope, body) = match env.resolve_type(&name) {
                                Some(ti) => match unwrap_custom_type(ti.clone(), env, false)?.inner.clone() {
                                    TypeInfo::TypeClosure { params, env: scope, body } => {
                                        (params, scope, body)
                                    },
                                    _ => return Err(Spanned { 
                                        inner: format!("Type {ti:?} does not accept generic args"), 
                                        span: type_info.span
                                    }),
                                },
                                None => return Err(Spanned { 
                                    inner: format!("Cannot resolve type {name}"), 
                                    span: type_info.span
                                }),
                            };
                            
                            if generic_args.len() == generic_params.len() {
                                for (p_name, arg) in generic_params.iter().zip(generic_args.iter()) {
                                    scope.add_custom_type(p_name.to_string(), arg.clone());
                                }
                                Ok(unwrap_custom_type(unwrap_generic(&*body)?, &mut scope, false)?)
                            } else {
                                return Err(Spanned { 
                                    inner: format!("Expected {} arguments, but got {}", generic_params.len(), generic_args.len()), 
                                    span: type_info.span
                                })
                            }
                        }
                    }
                },
                true => Ok(type_info)
            }
        },
        TypeInfo::Record(fields) => {
            let mut new_fields = Vec::new();
            for (name, ti) in fields {
                match unwrap_custom_type(ti, env, shallow) {
                    Ok(t) => new_fields.push((name, t)),
                    Err(e) => return Err(e),
                }
            }
            Ok(Spanned {
                inner: TypeInfo::Record(new_fields),
                span: type_info.span
            })
        },
        TypeInfo::Fun { params, return_type, generic_params } => {
            let mut new_scope = env.enter_scope();
            for generic in &generic_params {
                new_scope.add_custom_type(generic.inner.clone(), Spanned {
                    inner: TypeInfo::GenericParam(generic.inner.clone()),
                    span: generic.span
                });
            }
            let mut new_params = Vec::new();
            for (name, ti) in params {
                match unwrap_custom_type(ti.clone(), &mut new_scope, shallow) {
                    Ok(t) => new_params.push((name, t)),
                    Err(e) => return Err(e),
                }
            }
            let new_return_type = unwrap_custom_type(*return_type, &mut new_scope, shallow)?;
            Ok(Spanned {
                inner: TypeInfo::Fun { params: new_params, return_type: Box::new(new_return_type), generic_params },
                span: type_info.span
            })
        },
        TypeInfo::Array(ti) => Ok(Spanned {
            inner: TypeInfo::Array(Box::new(unwrap_custom_type(*ti, env, shallow)?)),
            span: type_info.span
        }),
        _ => Ok(type_info)
    }
}

//helper that converts GenericParam and its variations into Custom
fn unwrap_generic(type_info: &Spanned<TypeInfo>) -> Result<Spanned<TypeInfo>, Spanned<String>> {
    match type_info.inner.clone() {
        TypeInfo::GenericParam(name) => Ok(Spanned {
            inner: TypeInfo::Custom { name, generic_args: Vec::new() },
            span: type_info.span
        }),
        TypeInfo::Array(ti) => Ok(Spanned {inner: TypeInfo::Array(Box::new(unwrap_generic(&*ti)?)), span: ti.span}),
        TypeInfo::Record(entries) => {
            let mut new_entries = Vec::new();
            for (name, ti) in entries {
                new_entries.push((name, unwrap_generic(&ti)?));
            }
            Ok(Spanned { 
                inner: TypeInfo::Record(new_entries),
                span: type_info.span
            })
        },
        _ => Ok(type_info.clone())
    }
}

// helper to get a list of all generic params used in an enum variant
pub fn get_generic_params(type_info: Spanned<TypeInfo>) -> Vec<Spanned<String>> {
    let mut params = Vec::new();
    match type_info.inner {
        TypeInfo::Fun { params: fun_params, return_type, generic_params } => {
            for (_, ti) in fun_params {
                for name in get_generic_params(ti) {
                    params.push(name);
                }
            }
            for name in get_generic_params(*return_type) {
                params.push(name);
            }
            for name in generic_params {
                params.push(name);
            }
        },
        TypeInfo::Record(items) => {
            for (_, ti) in items {
                for name in get_generic_params(ti) {
                    params.push(name);
                }
            }
        },
        TypeInfo::Custom { name: _, generic_args } => {
            for ti in generic_args {
                for name in get_generic_params(ti) {
                    params.push(name);
                }
            }
        },
        TypeInfo::GenericParam(name) => {params.push(Spanned{inner: name, span: type_info.span});},
        TypeInfo::Array(spanned) => {
            for name in get_generic_params(*spanned) {
                params.push(name);
            }
        },
        TypeInfo::Enum { name: _, variants: _, generic_params } => {
            for name in generic_params {
                params.push(name);
            }
        },
        _ => ()
    };
    let mut unique_params = Vec::new();
    let mut encountered = HashSet::new();
    for p in params {
        if encountered.get(&p.inner).is_none() {
            unique_params.push(p.clone());
            encountered.insert(p.inner);
        }
    }
    unique_params.into_iter().collect()
}