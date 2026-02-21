use std::{borrow::Cow, collections::{HashMap, HashSet}};

use smol_str::{SmolStr, format_smolstr};

use crate::{ast::{Expr, MatchArm, Operation, Span, Spanned, Statement, TypeInfo, TypeKind, UnaryOp}, env::TypeEnv, ir, module::{GlobalRegistry, eval_import, insert_type_module}, native::get_native_fun};

struct LoweringResult {
    name: Option<SmolStr>,
    var_type: Option<Spanned<TypeInfo>>,
    custom_type: Option<Spanned<TypeInfo>>,
    export: bool,
    lowered_statement: Option<Spanned<ir::Statement>>
}

fn spanned<T>(inner: T, span: Span) -> Spanned<T> {
    Spanned { inner, span }
}

pub fn hoist(statements: &Vec<Spanned<Statement>>, env: &mut TypeEnv, path: &str) -> Result<
    (
        Vec<Spanned<ir::Statement>>, //ordered statements
        HashMap<SmolStr, Spanned<TypeInfo>>, // var_exports
        HashMap<SmolStr, Spanned<TypeInfo>>, // type_exports
    ), Spanned<SmolStr>
> {
    let mut statements = statements.clone();
    
    let mut lowered_statements = Vec::new();
    let is_top_level = env.is_top_level();
    let mut var_exports = HashMap::new();
    let mut type_exports = HashMap::new();
    
    let mut types = Vec::new();
    let mut functions = Vec::new();
    let mut rest = Vec::new();
    
    //processing imports and sorting other statements into groups
    loop {
        if statements.is_empty() {break}
        let st = statements.pop().unwrap();
        match &st.inner {
            Statement::Import { symbols: _, path: _ } => {
                let result = process_statement(&st, env, path)?;
                if let Some(statement) = result.lowered_statement {
                    lowered_statements.push(statement);
                }
            },
            Statement::TypeDef { name: _, type_info: _, generic_params: _, implementation: _ }
            | Statement::EnumDef { name: _, variants: _, generic_params: _ } => types.push(st),
            Statement::Fun { name: _, params: _, body: _, return_type: _, generic_params: _ } 
            | Statement::NativeFun { name: _, params: _, return_type: _, generic_params: _ } => functions.push(st),
            Statement::Export(statement) => {
                if !is_top_level {
                    return Err(spanned(
                        "Exports should be made at the top level".into(),
                        st.span
                    ))
                }
                match &statement.inner {
                    Statement::TypeDef { name: _, type_info: _, generic_params: _, implementation: _ } 
                    | Statement::EnumDef { name: _, variants: _, generic_params: _ } => types.push(st),
                    Statement::Fun { name: _, params: _, body: _, return_type: _, generic_params: _ }
                   | Statement::NativeFun { name: _, params: _, return_type: _, generic_params: _ } => functions.push(st),
                    _ => rest.push(st)
                }
            }
            _ => rest.push(st),
        }
    }
    
    //processing types
    loop {
        if types.is_empty() {break}
        let st = types.pop().unwrap();
        let result = process_statement(&st, env, path)?;
        if result.export && let Some(name) = result.name {
            if let Some(ti) = result.var_type {
                var_exports.insert(name.clone(), ti);
            }
            if let Some(ti) = result.custom_type {
                type_exports.insert(name, ti);
            }
        }
        if let Some(statement) = result.lowered_statement {
            lowered_statements.push(statement);
        }
    }
    
    //processing functions
    loop {
        if functions.is_empty() {break}
        let st = functions.pop().unwrap();
        let result = process_statement(&st, env, path)?;
        if result.export && let Some(name) = result.name {
            if let Some(ti) = result.var_type {
                var_exports.insert(name.clone(), ti);
            }
            if let Some(ti) = result.custom_type {
                type_exports.insert(name, ti);
            }
        }
        lowered_statements.push(result.lowered_statement.unwrap());
    }
    
    //processing the rest
    loop {
        if rest.is_empty() {break}
        let st = rest.pop().unwrap();
        let result = process_statement(&st, env, path)?;
        if result.export && let Some(name) = result.name {
            if let Some(ti) = result.var_type {
                var_exports.insert(name.clone(), ti);
            }
            if let Some(ti) = result.custom_type {
                type_exports.insert(name, ti);
            }
        }
        lowered_statements.push(result.lowered_statement.unwrap());
    }
    
    let reg = GlobalRegistry;
    insert_type_module(&reg, var_exports.clone(), type_exports.clone(), env.clone(), path);
    
    Ok((lowered_statements, var_exports, type_exports))
}

fn process_statement(statement: &Spanned<Statement>, env: &mut TypeEnv, path: &str) -> Result<
    LoweringResult,
    Spanned<SmolStr>
> {
    match &statement.inner {
        Statement::Let { name, expr, type_info } => {
            let expr_result = lower_expr(expr, env)?;
            let expected_type = if let Some(ti) = type_info {
                let expected_type = flatten_type(ti, env)?;
                if expected_type.inner != expr_result.1.inner {
                    return Err(spanned(
                        format_smolstr!(
                            "Type mismatch in let declaration: expected {:?}, got {:?}",
                            expected_type.inner,
                            expr_result.1.inner
                        ),
                        statement.span
                    ))
                }
                expected_type.into_owned()
            } else {
                expr_result.1.clone()
            };
            env.add_var_type(name.clone(), expected_type.clone());
            Ok(LoweringResult { 
                name: Some(name.clone()),
                var_type: Some(expected_type), 
                custom_type: None,
                export: false,
                lowered_statement: Some(spanned(
                    ir::Statement::Let { 
                        name: name.clone(),
                        expr: expr_result.0
                    },
                    statement.span
                ))
            })
        },
        Statement::TypeDef { name, type_info, generic_params, implementation } => {
            let mut generic_scope = env.enter_scope();
            let flat_type = if generic_params.len() != 0 {
                for param in generic_params {
                    generic_scope.add_custom_type(
                        param.inner.clone(),
                        spanned(
                            TypeInfo::new(TypeKind::GenericParam(param.inner.clone())),
                            param.span.clone()
                        )
                    );
                }
                spanned(
                    TypeInfo::new(TypeKind::TypeClosure { 
                        params: generic_params.clone(),
                        body: Box::new(type_info.clone())
                    }),
                    statement.span
                )
            } else {
                flatten_type(type_info, env)?.into_owned()
            };
            
            env.add_custom_type(name.clone(), flat_type.clone());
            let mut implemented_methods = HashSet::new();
            for (interface_name, methods) in implementation {
                env.add_interface_impl(interface_name.clone(), type_info.clone());
                let mut interface_env = generic_scope.enter_scope();
                for m in methods {
                    if implemented_methods.contains(&m.inner.name) {
                        return Err(spanned(
                            format_smolstr!("Method {} is defined multiple times", m.inner.name),
                            m.span
                        ))
                    }
                    let mut inner_scope = interface_env.enter_scope();
                    inner_scope.add_var_type("self".into(), flat_type.clone());
                    let expected_type = m.inner.return_type.clone()
                        .unwrap_or(spanned(TypeInfo::void(), Span::from(0..0)));
                    if m.inner.generic_params.len() == 0 {
                        let mut flat_params = Vec::new();
                        for (n, p_type) in &m.inner.params {
                            let flattened = flatten_type(p_type, &mut inner_scope,)?.into_owned();
                            inner_scope.add_var_type(n.1.clone(), flattened.clone());
                            flat_params.push((n.clone(), flattened))
                        }
                        let expected_type = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                        inner_scope.add_var_type(SmolStr::new("+return"), expected_type.clone());
                        inner_scope.add_var_type(
                            name.clone(),
                            spanned(
                                TypeInfo::new(TypeKind::Fun {
                                    params: flat_params.clone(), 
                                    return_type: Box::new(expected_type.clone()),
                                    generic_params: generic_params.clone()
                                }),
                                statement.span
                            )
                        );
                        let method_result = lower_expr(&m.inner.body, &mut inner_scope)?;
                        if method_result.1.inner != expected_type.inner {
                            return Err(spanned(
                                format_smolstr!(
                                    "Function return type mismatch: expected {:?}, got {:?}",
                                    expected_type.inner,
                                    method_result.1.inner
                                ),
                                expected_type.span
                            ))
                        }
                        let fun_type = spanned(
                            TypeInfo::new(
                                TypeKind::Fun {
                                    params: flat_params, 
                                    return_type: Box::new(method_result.1.clone()),
                                    generic_params: Vec::new()
                                }
                            ),
                            statement.span
                        );
                        
                        env.insert_method(
                            flat_type.inner.id(),
                            (
                                m.inner.name.clone(),
                                (
                                    fun_type.clone(),
                                    spanned(
                                        ir::Expr::Fun { 
                                            params: m.inner.params.iter().map(|(e, _)| e.clone()).collect(), 
                                            body: Box::new(method_result.0)
                                        },
                                        m.span
                                    )
                                )
                            )
                        );
                        implemented_methods.insert(m.inner.name.clone());
                    } else {
                        // Register generic params in the inner scope so they can be referenced
                        for generic in generic_params {
                            inner_scope.add_custom_type(
                                generic.inner.clone(),
                                spanned(
                                    TypeInfo::new(TypeKind::GenericParam(generic.inner.clone())),
                                    generic.span
                                )
                            );
                        }
        
                        // Flatten parameter types with generics in scope and add them to the inner scope
                        let mut flat_params = Vec::new();
                        for (n, p_type) in &m.inner.params {
                            let flattened = flatten_type(p_type, &mut inner_scope)?.into_owned();
                            inner_scope.add_var_type(n.1.clone(), flattened.clone());
                            flat_params.push((n.clone(), flattened));
                        }
        
                        let expected_flat = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                        inner_scope.add_var_type(SmolStr::new("+return"), expected_flat.clone());
        
                        inner_scope.add_var_type(
                            name.clone(),
                            spanned(
                                TypeInfo::new(TypeKind::Fun {
                                    params: flat_params.clone(),
                                    return_type: Box::new(expected_flat.clone()),
                                    generic_params: generic_params.clone()
                                }),
                                statement.span
                            )
                        );
        
                        let method_result = lower_expr(&m.inner.body, &mut inner_scope)?;
                        if method_result.1.inner != expected_flat.inner {
                            return Err(spanned(
                                format_smolstr!(
                                    "Function return type mismatch: expected {:?}, got {:?}", 
                                    expected_flat.inner, 
                                    method_result.1.inner
                                ),
                                expected_flat.span
                            ))
                        }
                        let fun_type = spanned(
                            TypeInfo::new(TypeKind::Fun {
                                params: flat_params.clone(),
                                return_type: Box::new(method_result.1.clone()),
                                generic_params: generic_params.clone()
                            }),
                            statement.span
                        );
                        env.insert_method(
                            flat_type.inner.id(),
                            (
                                m.inner.name.clone(),
                                (
                                    fun_type.clone(),
                                    spanned(
                                        ir::Expr::Fun { 
                                            params: m.inner.params.iter().map(|(e, _)| e.clone()).collect(), 
                                            body: Box::new(method_result.0)
                                        },
                                        m.span
                                    )
                                )
                            )
                        );
                        implemented_methods.insert(m.inner.name.clone());
                    }
                }
            }
            Ok(LoweringResult { 
                name: Some(name.clone()),
                var_type: None, 
                custom_type: Some(flat_type),
                export: false,
                lowered_statement: None
            })
        },
        Statement::Expr(expr) => {
            let expr_result = lower_expr(expr, env)?;
            Ok(LoweringResult { 
                name: None,
                var_type: Some(expr_result.1), 
                custom_type: None,
                export: false,
                lowered_statement: Some(spanned(
                    ir::Statement::Expr(expr_result.0),
                    statement.span
                ))
            })
        },
        Statement::Fun { name, params, body, return_type, generic_params } => {
            let mut inner_scope = env.enter_scope();
            let expected_type = return_type.clone().unwrap_or(spanned(TypeInfo::void(), Span::from(0..0)));
            
            if generic_params.len() == 0 {
                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope,)?.into_owned();
                    inner_scope.add_var_type(n.1.clone(), flattened.clone());
                    flat_params.push((n.clone(), flattened))
                }
                let expected_type = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                inner_scope.add_var_type(SmolStr::new("+return"), expected_type.clone());
                inner_scope.add_var_type(
                    name.clone(),
                    spanned(
                        TypeInfo::new(TypeKind::Fun {
                            params: flat_params.clone(), 
                            return_type: Box::new(expected_type.clone()),
                            generic_params: generic_params.clone()
                        }),
                        statement.span
                    )
                );
                let body_result = lower_expr(body, &mut inner_scope)?;
                if body_result.1.inner != expected_type.inner {
                    return Err(spanned(
                        format_smolstr!(
                            "Function return type mismatch: expected {:?}, got {:?}",
                            expected_type.inner,
                            body_result.1.inner
                        ),
                        expected_type.span
                    ))
                }
                let fun_type = spanned(
                    TypeInfo::new(
                        TypeKind::Fun {
                            params: flat_params, 
                            return_type: Box::new(body_result.1),
                            generic_params: Vec::new()
                        }
                    ),
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok(LoweringResult { 
                    name: Some(name.clone()),
                    var_type: Some(fun_type), 
                    custom_type: None,
                    export: false,
                    lowered_statement: Some(spanned(
                        ir::Statement::Let { 
                            name: name.clone(),
                            expr: spanned(
                                ir::Expr::Fun { 
                                    params: params.iter().map(|(need, _)| need.clone()).collect(),
                                    body: Box::new(body_result.0)
                                },
                                body.span
                            )
                        }, 
                        statement.span
                    ))
                })
            } else {
                // Register generic params in the inner scope so they can be referenced
                for generic in generic_params {
                    inner_scope.add_custom_type(
                        generic.inner.clone(),
                        spanned(
                            TypeInfo::new(TypeKind::GenericParam(generic.inner.clone())),
                            generic.span
                        )
                    );
                }

                // Flatten parameter types with generics in scope and add them to the inner scope
                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope)?.into_owned();
                    inner_scope.add_var_type(n.1.clone(), flattened.clone());
                    flat_params.push((n.clone(), flattened));
                }

                let expected_flat = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                inner_scope.add_var_type(SmolStr::new("+return"), expected_flat.clone());

                let fun_type = spanned(
                    TypeInfo::new(TypeKind::Fun {
                        params: flat_params.clone(),
                        return_type: Box::new(expected_flat.clone()),
                        generic_params: generic_params.clone()
                    }),
                    statement.span
                );
                
                inner_scope.add_var_type(
                    name.clone(),
                    fun_type.clone()
                );

                let body_result = lower_expr(body, &mut inner_scope)?;
                if body_result.1.inner != expected_flat.inner {
                    return Err(spanned(
                        format_smolstr!(
                            "Function return type mismatch: expected {:?}, got {:?}",
                            expected_flat.inner,
                            body_result.1.inner
                        ),
                        expected_flat.span
                    ))
                }
                env.add_var_type(name.clone(), fun_type.clone());
                Ok(LoweringResult { 
                    name: Some(name.clone()),
                    var_type: Some(fun_type), 
                    custom_type: None,
                    export: false,
                    lowered_statement: Some(spanned(
                        ir::Statement::Let { 
                            name: name.clone(),
                            expr: spanned(
                                ir::Expr::Fun { 
                                    params: params.iter().map(|(need, _)| need.clone()).collect(),
                                    body: Box::new(body_result.0)
                                },
                                body.span
                            )
                        }, 
                        statement.span
                    ))
                })
            }
        },
        Statement::NativeFun { name, params, return_type, generic_params } => {
            if let None = get_native_fun(path, name) {
                return Err(Spanned {
                    inner: format!("Cannot find native function definition for {}", name).into(),
                    span: statement.span
                })
            }
            let mut inner_scope = env.enter_scope();
            let r_type = return_type.clone().unwrap_or(spanned(
                TypeInfo::void(),
                Span::from(0..0)
            ));
            if generic_params.len() == 0 {
                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    flat_params.push((n.clone(), flatten_type(p_type, &mut inner_scope)?.into_owned()))
                }
                let r_type = flatten_type(&r_type, &mut inner_scope)?.into_owned();
                let fun_type = spanned(
                    TypeInfo::new(TypeKind::Fun {
                        params: flat_params, 
                        return_type: Box::new(r_type),
                        generic_params: Vec::new()
                    }),
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok(LoweringResult { 
                    name: Some(name.clone()),
                    var_type: Some(fun_type), 
                    custom_type: None,
                    export: false,
                    lowered_statement: Some(spanned(
                        ir::Statement::NativeFun(name.clone()),
                        statement.span
                    ))
                })
            } else {
                // Register generic params in the inner scope so they can be referenced
                for generic in generic_params {
                    inner_scope.add_custom_type(
                        generic.inner.clone(),
                        spanned(
                            TypeInfo::new(TypeKind::GenericParam(generic.inner.clone())),
                            generic.span
                        )
                    );
                }

                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope)?.into_owned();
                    flat_params.push((n.clone(), flattened));
                }

                // Flatten return type as well
                let r_flat = flatten_type(&r_type, &mut inner_scope)?.into_owned();

                // Create the type-closure representing the generic native function
                let fun_type = spanned(
                    TypeInfo::new(TypeKind::Fun {
                        params: flat_params.clone(),
                        return_type: Box::new(r_flat.clone()),
                        generic_params: generic_params.clone()
                    }),
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok(LoweringResult { 
                    name: Some(name.clone()),
                    var_type: Some(fun_type), 
                    custom_type: None,
                    export: false,
                    lowered_statement: Some(spanned(
                        ir::Statement::NativeFun(name.clone()),
                        statement.span
                    ))
                })
            }
        },
        Statement::EnumDef { name, variants, generic_params } => {
            let mut inner_scope = env.enter_scope();
            for p in generic_params {
                inner_scope.add_custom_type(p.inner.clone(), spanned(
                    TypeInfo::new(TypeKind::GenericParam(p.inner.clone())),
                    p.span
                ));
            }
            
            let mut variant_funs = HashMap::new();
            let mut variant_hashmap = HashMap::new();
            // since enums are just fancy records, we need to store their lowered fields somewhere
            let mut record_exprs = HashMap::new();
            
            for (v_name, maybe_ti) in variants {
                if let Some(ti) = maybe_ti {
                    let ti = flatten_type(ti, &mut inner_scope)?.into_owned();
                    record_exprs.insert(
                        v_name.clone(),
                        Spanned {
                            inner: ir::Expr::Fun { 
                                params: vec![(false, SmolStr::new("+arg"))], 
                                body: Box::new(Spanned {
                                    inner: ir::Expr::EnumConstructor { 
                                        enum_name: name.clone(),
                                        variant: v_name.clone(),
                                        value: Box::new(Spanned {
                                            inner: ir::Expr::Var(SmolStr::new("+arg")),
                                            span: ti.span
                                        })
                                    },
                                    span: ti.span
                                })
                            },
                            span: ti.span
                        }
                    );
                    variant_funs.insert(v_name.clone(), spanned(
                        TypeInfo::new(TypeKind::Fun { 
                            params: vec![((false, "+arg".into()), ti.clone())],
                            return_type: Box::new(spanned(
                                TypeInfo::new(TypeKind::EnumVariant { 
                                    enum_name: name.clone(),
                                    variant: v_name.clone(),
                                    generic_args: generic_params
                                        .clone()
                                        .into_iter()
                                        .map(|name| TypeInfo::new(TypeKind::GenericParam(name.inner)))
                                        .collect()
                                }),
                                statement.span
                            )),
                            generic_params: generic_params.clone()
                        }),
                        statement.span
                    ));
                    variant_hashmap.insert(v_name.clone(), ti.clone());
                } else {
                    variant_funs.insert(v_name.clone(), spanned(
                        TypeInfo::new(TypeKind::Fun { 
                            params: Vec::new(),
                            return_type: Box::new(spanned(
                                TypeInfo::new(TypeKind::EnumVariant { 
                                    enum_name: name.clone(),
                                    variant: v_name.clone(),
                                    generic_args: vec![TypeInfo::any(); generic_params.len()]
                                }),
                                statement.span
                            )),
                            generic_params: Vec::new()
                        }),
                        statement.span
                    ));
                    record_exprs.insert(
                        v_name.clone(),
                        Spanned {
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
                        }
                    );
                    variant_hashmap.insert(v_name.clone(), spanned(TypeInfo::void(), statement.span));
                }
            }
            
            let enum_var = spanned(
                TypeInfo::new(TypeKind::Record(variant_funs)),
                statement.span
            );
            let enum_type = spanned(
                TypeInfo::new(TypeKind::Enum { name: name.clone(), variants: variant_hashmap, generic_params: generic_params.clone() }),
                statement.span
            );
            
            env.add_custom_type(name.clone(), enum_type.clone());
            env.add_var_type(name.clone(), enum_var.clone());
            
            Ok(LoweringResult { 
                name: Some(name.clone()),
                var_type: Some(enum_var), 
                custom_type: Some(enum_type),
                export: false,
                lowered_statement: Some(spanned(
                    ir::Statement::Let { 
                        name: name.clone(),
                        expr: Spanned {
                            inner: ir::Expr::Record(record_exprs),
                            span: statement.span
                        }
                    },
                    statement.span
                ))
            })
        },
        Statement::Import { symbols, path } => {
            if !env.is_top_level() {
                return Err(spanned(
                    "Imports should be made at the top level".into(),
                    statement.span
                ))
            }
            let (var_exports, type_exports) = eval_import(path.inner.as_str(), &GlobalRegistry)?;
            for (symbol, alias, is_type) in symbols {
                match is_type {
                    false => {
                        let var_type = var_exports.get(&symbol.inner);
                        if var_type.is_none() {
                            return Err(spanned(
                                format_smolstr!("Cannot resolve import {symbol} from {path}"), 
                                symbol.span
                            ))
                        }
                        env.add_var_type(
                            alias.clone().unwrap_or(symbol.inner.clone()),
                            var_type.unwrap().clone()
                        );
                    },
                    true => {
                        let import_type = type_exports.get(&symbol.inner);
                        if import_type.is_none() {
                            return Err(spanned(
                                format_smolstr!("Cannot resolve import type {symbol} from {path}"), 
                                symbol.span
                            ))
                        }
                        env.add_custom_type(
                            alias.clone().unwrap_or(symbol.inner.clone()),
                            import_type.unwrap().clone()
                        );
                    },
                }
            }
            Ok(LoweringResult { 
                name: None,
                var_type: None, 
                custom_type: None,
                export: false,
                lowered_statement: {
                    let mut new_symbols = Vec::new();
                    for (name, alias, is_type) in symbols {
                        if !is_type.clone() {
                            new_symbols.push((name.clone(), alias.clone()))
                        }
                    }
                    
                    if new_symbols.len() != 0 {
                        Some(spanned(
                            ir::Statement::Import { 
                                symbols: new_symbols, 
                                path: path.clone()
                            },
                            statement.span
                        ))
                    } else { None }
                }
            })
        },
        Statement::Export(st) => {
            if !env.is_top_level() {
                return Err(spanned(
                    "Exports should be made at the top level".into(),
                    statement.span
                ))
            }
            let result = process_statement(st, env, path)?;
            if result.export {
                return Err(spanned(
                    "Cannot export an export statement".into(),
                    st.span
                ))
            }
            if result.name.is_none() {
                return Err(spanned(
                    "Cannot export this".into(),
                    st.span
                ))
            }
            Ok(LoweringResult { 
                name: Some(result.name.unwrap().clone()),
                var_type: result.var_type.clone(), 
                custom_type: result.custom_type,
                export: true,
                lowered_statement: {
                    if result.var_type.is_some() {
                        Some(spanned(
                            ir::Statement::Export(Box::new(result.lowered_statement.unwrap())),
                            statement.span
                        ))
                    } else { None }
                }
            })
        },
    }
}

fn substitute_generic_params(type_info: &Spanned<TypeInfo>, generic_arg_map: &HashMap<SmolStr, Spanned<TypeInfo>>) -> Spanned<TypeInfo> {
    match type_info.inner.kind() {
        TypeKind::GenericParam(name) => {
            if let Some(concrete_type) = generic_arg_map.get(name) {
                concrete_type.clone()
            } else {
                spanned(TypeInfo::new(TypeKind::GenericParam(name.clone())), type_info.span)
            }
        },
        TypeKind::Fun { params, return_type, generic_params } => {
            let new_params = params.iter().map(|(n, ti)| {
                (n.clone(), substitute_generic_params(ti, generic_arg_map))
            }).collect();
            let new_return_type = Box::new(substitute_generic_params(return_type, generic_arg_map));
            spanned(
                TypeInfo::new(TypeKind::Fun {
                    params: new_params,
                    return_type: new_return_type,
                    generic_params: generic_params.clone()
                }),
                type_info.span
            )
        },
        TypeKind::Record(entries) => {
            let new_entries = entries.iter().map(|(name, ti)| {
                (name.clone(), substitute_generic_params(ti, generic_arg_map))
            }).collect();
            spanned(TypeInfo::new(TypeKind::Record(new_entries)), type_info.span)
        },
        TypeKind::Array(ti) => {
            spanned(TypeInfo::new(TypeKind::Array(Box::new(substitute_generic_params(ti, generic_arg_map)))), type_info.span)
        },
        TypeKind::EnumInstance { enum_name, variants, generic_args } => {
            let new_args = generic_args.iter()
                .map(|arg| substitute_generic_params(arg, generic_arg_map))
                .collect();
            spanned(
                TypeInfo::new(TypeKind::EnumInstance {
                    enum_name: enum_name.clone(),
                    variants: variants.clone(),
                    generic_args: new_args,
                }),
                type_info.span
            )
        },
        TypeKind::EnumVariant { enum_name, variant, generic_args } => {
            let new_args = generic_args.iter()
                .map(|arg| substitute_generic_params(&spanned(arg.clone(), type_info.span), generic_arg_map).inner)
                .collect();
            spanned(
                TypeInfo::new(TypeKind::EnumVariant {
                    enum_name: enum_name.clone(),
                    variant: variant.clone(),
                    generic_args: new_args,
                }),
                type_info.span
            )
        },
        _ => type_info.clone()
    }
}

fn collect_generic_params(
    generic_type: (&Spanned<TypeInfo>, bool), //bool is for handling variadics
    concrete_type: &Spanned<TypeInfo>,
) -> Vec<(SmolStr, Spanned<TypeInfo>)> {
    fn walk(
        gener: (&Spanned<TypeInfo>, bool),
        con: &Spanned<TypeInfo>,
        out: &mut Vec<(SmolStr, Spanned<TypeInfo>)>,
    ) {
        match (gener.0.inner.kind(), con.inner.kind()) {
            (TypeKind::GenericParam(name), _) => {
                out.push((name.clone(), con.clone()));
            },
            (
                TypeKind::Fun { params: gen_params, return_type: gen_ret, ..},
                TypeKind::Fun { params: con_params, return_type: con_ret, ..},
            ) => {
                for ((extra, gen_p), (_, con_p)) in gen_params.iter().zip(con_params.iter()) {
                    walk((gen_p, extra.0), con_p, out);
                }
                walk((gen_ret, false), con_ret, out);
            },
            (TypeKind::Record(gen_entries), TypeKind::Record(con_entries)) => {
                for ((_, gen_e), (_, con_e)) in gen_entries.iter().zip(con_entries.iter()) {
                    walk((gen_e, false), con_e, out);
                }
            },
            (TypeKind::Array(gen_inner), TypeKind::Array(con_inner)) => {
                walk((gen_inner, false), con_inner, out);
            },
            //for variadic
            (TypeKind::Array(gen_inner), _) => {
                if gener.1 {
                    walk((gen_inner, true), con, out);
                }
            },
            (
                TypeKind::Custom { generic_args: gen_args, .. },
                TypeKind::Custom { generic_args: con_args, .. },
            ) => {
                for (gen_arg, con_arg) in gen_args.iter().zip(con_args.iter()) {
                    walk((gen_arg, false), con_arg, out);
                }
            },
            _ => {}
        }
    }

    let mut result = Vec::new();
    walk(generic_type, concrete_type, &mut result);
    result
}

//replaces type aliases with their underlying type, processes type closures
fn flatten_type<'a>(type_info: &'a Spanned<TypeInfo>, env: &mut TypeEnv) -> Result<Cow<'a, Spanned<TypeInfo>>, Spanned<SmolStr>> {
    match type_info.inner.kind() {
        TypeKind::Custom { name, generic_args } => {
            let resolved_type = if let Some(ti) = env.resolve_type(name.as_str()) {
                spanned(
                    TypeInfo::new_with_id(ti.inner.kind().clone(), ti.inner.id()),
                    ti.span
                )
            } else {
                return Err(spanned(
                    format!("Cannot resolve type {name}").into(),
                    type_info.span
                ));
            };

            let generic_args = {
                let mut new_args = Vec::new();
                for ti in generic_args {
                    new_args.push(flatten_type(ti, env)?.into_owned())
                }
                new_args
            };

            match resolved_type.inner.kind() {
                //useless but idc
                TypeKind::Fun { params, return_type, generic_params } => {
                    if generic_params.len() != generic_args.len() {
                        return Err(spanned(
                            format!("A wrong number of generic args provided: expected: {}, got {}", generic_params.len(), generic_args.len()).into(),
                            resolved_type.span
                        ))
                    }
                    let mut new_scope = env.enter_scope();
                    for (param, arg) in generic_params.iter().zip(generic_args.iter()) {
                        new_scope.add_custom_type(param.inner.clone(), arg.clone());
                    }
                    let mut new_params = Vec::new();
                    for (n, ti) in params {
                        new_params.push((n.clone(), flatten_type(ti, &mut new_scope)?.into_owned()));
                    }
                    Ok(Cow::Owned(spanned(
                        TypeInfo::new_with_id(TypeKind::Fun { 
                            params: new_params, 
                            return_type: Box::new(flatten_type(return_type, &mut new_scope)?.into_owned()),
                            generic_params: Vec::new()
                        }, resolved_type.inner.id()),
                        type_info.span
                    )))
                },
                TypeKind::Enum { name, variants, generic_params } => {
                    if generic_params.len() != generic_args.len() {
                        return Err(spanned(
                            format!("A wrong number of generic args provided: expected: {}, got {}", generic_params.len(), generic_args.len()).into(),
                            resolved_type.span
                        ))
                    }
                    Ok(Cow::Owned(spanned(
                        TypeInfo::new_with_id(TypeKind::EnumInstance { enum_name: name.clone(), variants: variants.clone(), generic_args: generic_args.clone() }, resolved_type.inner.id()),
                        type_info.span
                    )))
                },
                TypeKind::TypeClosure { params, body } => {
                    let mut inner_scope = env.enter_scope();
                    if generic_args.len() == 0 && params.len() > 0 {
                        for p in params {
                            inner_scope.add_custom_type(
                                p.inner.clone(),
                                spanned(
                                    TypeInfo::new(TypeKind::GenericParam(p.inner.clone())),
                                    p.span.clone()
                                )
                            );
                        }
                        
                    } else if params.len() != generic_args.len() {
                        return Err(spanned(
                            format_smolstr!(
                                "A wrong number of generic args provided: expected: {}, got {}",
                                params.len(),
                                generic_args.len()
                            ),
                            resolved_type.span
                        ))
                    } else {
                        for (param, arg) in params.iter().zip(generic_args.iter()) {
                            let flat_arg = flatten_type(arg, env)?.into_owned();
                            inner_scope.add_custom_type(param.inner.clone(), flat_arg);
                        }
                        
                    }
                    
                    let flattened = flatten_type(body, &mut inner_scope)?.into_owned();
                    Ok(Cow::Owned(spanned(
                        TypeInfo::new_with_id(flattened.inner.kind().clone(), resolved_type.inner.id()),
                        flattened.span
                    )))
                },
                _ => {
                    if generic_args.len() != 0 {
                        return Err(spanned(
                            format!("Too many generic arguments provided: expected: 0, got {}", generic_args.len()).into(),
                            type_info.span
                        ))
                    }
                    Ok(Cow::Owned(flatten_type(&resolved_type, env)?.into_owned()))
                }
            }
        },
        TypeKind::Fun { params, return_type, generic_params } => {
            if generic_params.len() > 0 {
                // Build a set of generic parameter names
                let generic_names: std::collections::HashSet<SmolStr> = generic_params
                    .iter()
                    .map(|gp| gp.inner.clone())
                    .collect();

                // Helper to determine if a type contains any of the generic parameters
                fn contains_generic(
                    ti: &Spanned<TypeInfo>,
                    generic_names: &std::collections::HashSet<SmolStr>,
                ) -> bool {
                    match ti.inner.kind() {
                        TypeKind::GenericParam(name) => generic_names.contains(name),
                        TypeKind::Fun {
                            params,
                            return_type,
                            generic_params: _,
                        } => {
                            params.iter().any(|(_, p)| contains_generic(p, generic_names))
                                || contains_generic(return_type, generic_names)
                        }
                        TypeKind::Record(entries) => {
                            entries.iter().any(|(_, e)| contains_generic(e, generic_names))
                        }
                        TypeKind::Array(inner) => contains_generic(inner, generic_names),
                        TypeKind::Custom { generic_args, name } => {
                            generic_args.iter().any(|arg| contains_generic(arg, generic_names))
                                || generic_names.contains(name)
                        }
                        _ => false,
                    }
                }

                // Helper to replace a Custom type with a GenericParam when appropriate
                fn replace_custom_with_generic(
                    ti: Spanned<TypeInfo>,
                    generic_names: &std::collections::HashSet<SmolStr>,
                ) -> Spanned<TypeInfo> {
                    match ti.inner.kind() {
                        TypeKind::Custom { generic_args, name }
                            if generic_args.is_empty() && generic_names.contains(name) =>
                        {
                            spanned(TypeInfo::new(TypeKind::GenericParam(name.clone())), ti.span)
                        }
                        _ => ti,
                    }
                }

                // Flatten parameter types where possible, applying the replacement rule
                let mut new_params = Vec::new();
                for (a, ti) in params {
                    let flattened = if contains_generic(ti, &generic_names) {
                        ti.clone()
                    } else {
                        flatten_type(ti, env)?.into_owned()
                    };
                    let final_ti = replace_custom_with_generic(flattened, &generic_names);
                    new_params.push((a.clone(), final_ti));
                }

                // Flatten return type where possible, applying the replacement rule
                let new_return = if contains_generic(return_type.as_ref(), &generic_names) {
                    (**return_type).clone()
                } else {
                    flatten_type(return_type.as_ref(), env)?.into_owned()
                };
                let final_return = replace_custom_with_generic(new_return, &generic_names);

                return Ok(Cow::Owned(spanned(
                    TypeInfo::new_with_id(TypeKind::Fun {
                        params: new_params,
                        return_type: Box::new(final_return),
                        generic_params: generic_params.clone(),
                    }, type_info.inner.id()),
                    type_info.span,
                )));
            }

            // No generic parameters: just flatten everything normally
            let mut new_params = Vec::new();
            for (a, ti) in params {
                new_params.push((a.clone(), flatten_type(ti, env)?.into_owned()));
            }
            Ok(Cow::Owned(spanned(
                TypeInfo::new_with_id(TypeKind::Fun {
                    params: new_params,
                    return_type: Box::new(flatten_type(return_type, env)?.into_owned()),
                    generic_params: Vec::new(),
                }, type_info.inner.id()),
                type_info.span,
            )))
        },
        TypeKind::Record(entries) => {
            let mut new_entries = HashMap::new();
            for (name, ti) in entries {
                new_entries.insert(name.clone(), flatten_type(ti, env)?.into_owned());
            }
            Ok(Cow::Owned(spanned(
                TypeInfo::new_with_id(TypeKind::Record(new_entries), type_info.inner.id()),
                type_info.span
            )))
        },
        TypeKind::Array(ti) => Ok(Cow::Owned(spanned(
            TypeInfo::new_with_id(TypeKind::Array(Box::new(flatten_type(ti, env)?.into_owned())), type_info.inner.id()),
            type_info.span
        ))),
        TypeKind::TypeClosure { params, body } => {
            let mut inner_scope = env.enter_scope();
            for p in params {
                inner_scope.add_custom_type(
                    p.inner.clone(),
                    spanned(
                        TypeInfo::new(TypeKind::GenericParam(p.inner.clone())),
                        p.span.clone()
                    )
                );
            }
            
            let flattened = flatten_type(body, &mut inner_scope)?.into_owned();
            Ok(Cow::Owned(spanned(
                TypeInfo::new_with_id(flattened.inner.kind().clone(), type_info.inner.id()),
                flattened.span
            )))
        },
        _ => Ok(Cow::Borrowed(type_info))
    }
}

fn lower_expr(expr: &Spanned<Expr>, env: &mut TypeEnv) -> Result<
    (Spanned<ir::Expr>, Spanned<TypeInfo>),
    Spanned<SmolStr>
> {
    match &expr.inner {
        Expr::Bool(b) => Ok((
            spanned(ir::Expr::Bool(*b), expr.span),
            spanned(TypeInfo::bool(), expr.span)
        )),
        Expr::Float(f) => Ok((
            spanned(ir::Expr::Float(*f), expr.span),
            spanned(TypeInfo::float(), expr.span)
        )),
        Expr::Int(i) => Ok((
            spanned(ir::Expr::Int(*i), expr.span),
            spanned(TypeInfo::int(), expr.span)
        )),
        Expr::String(s) => Ok((
            spanned(ir::Expr::String(s.clone()), expr.span),
            spanned(TypeInfo::string(), expr.span)
        )),
        Expr::Char(c) => Ok((
            spanned(ir::Expr::Char(c.clone()), expr.span),
            spanned(TypeInfo::char(), expr.span)
        )),
        Expr::Void => Ok((
            spanned(ir::Expr::Void, expr.span),
            spanned(TypeInfo::void(), expr.span)
        )),
        Expr::Null => Ok((
            spanned(ir::Expr::Null, expr.span),
            spanned(TypeInfo::null(), expr.span)
        )),
        Expr::Var(name) => {
            let var_type = if let Some(ti) = env.get_var_type(name) {
                ti
            } else {
                return Err(spanned(
                    format_smolstr!("Cannot resolve variable {name}"),
                    expr.span
                ))
            };
            Ok((
                spanned(ir::Expr::Var(name.clone()), expr.span),
                var_type
            ))
        },
        Expr::Array(spanneds) => {
            let mut lowered = Vec::new();
            let mut expected_type = spanned(TypeInfo::void(), Span::from(0..0));
            for e in spanneds {
                let result = lower_expr(e, env)?;
                if result.1.inner != expected_type.inner {
                    if expected_type.inner == TypeInfo::void() {
                        expected_type = result.1
                    } else {
                        return Err(spanned(
                            format_smolstr!(
                                "Array Element Type Mismatch: Expected {:?}, got {:?}",
                                expected_type.inner, 
                                result.1.inner
                            ),
                            e.span
                        ))
                    }
                }
                lowered.push(result.0);
            }
            Ok((
                spanned(ir::Expr::Array(lowered), expr.span.clone()),
                spanned(
                    TypeInfo::new(TypeKind::Array(Box::new(expected_type))),
                    expr.span
                )
            ))
        },
        Expr::Index(target, index) => {
            let target_result = lower_expr(target, env)?;
            let index_result = lower_expr(index, env)?;
            match target_result.1.inner.kind() {
                TypeKind::Array(element_type) => {
                    if index_result.1.inner != TypeInfo::int() {
                        return Err(spanned(
                            format_smolstr!("Cannot use {:?} to index arrays", index_result.1.inner),
                            index.span
                        ))
                    }
                    Ok((
                        spanned(
                            ir::Expr::Index(
                                Box::new(target_result.0),
                                Box::new(index_result.0)
                            ),
                            expr.span
                        ),
                        *element_type.clone()
                    ))
                },
                _ => Err(spanned(
                    format_smolstr!("Cannot index {:?}", target_result.1.inner),
                    target.span
                ))
            }
        },
        Expr::Binary { left, operation, right } => {
            let left_result = lower_expr(left, env)?;
            let right_result = lower_expr(right, env)?;
            match operation {
                Operation::Add
                | Operation::Subtract 
                | Operation::Multiply
                | Operation::Divide
                | Operation::Modulo  => {
                    match left_result.1.inner.kind() {
                        TypeKind::Any 
                        | TypeKind::Float 
                        | TypeKind::Int => {
                            if left_result.1.inner != right_result.1.inner {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Type Mismatch: Expeccted {:?}, got {:?}",
                                        left_result.1.inner, 
                                        right_result.1.inner
                                    ), 
                                    left.span
                                ))
                            }
                            Ok((
                                spanned(
                                    ir::Expr::Binary { 
                                        left: Box::new(left_result.0),
                                        operation: operation.clone(),
                                        right: Box::new(right_result.0)
                                    },
                                    expr.span
                                ),
                                spanned(
                                    left_result.1.inner,
                                    expr.span
                                )
                            ))
                        },
                        _ => return Err(spanned(
                            format!("Cannot apply {:?} to {:?}", operation, left_result.1.inner).into(), 
                            left.span
                        ))
                    }
                },
                Operation::LessThan 
                | Operation::LessThanEq 
                | Operation::GreaterThan 
                | Operation::GreaterThanEq => {
                    match left_result.1.inner.kind() {
                        TypeKind::Any 
                        | TypeKind::Float 
                        | TypeKind::Int => {
                            if left_result.1.inner != right_result.1.inner {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Type Mismatch: Expeccted {:?}, got {:?}",
                                        left_result.1.inner, 
                                        right_result.1.inner
                                    ), 
                                    right.span
                                ))
                            }
                            Ok((
                                spanned(
                                    ir::Expr::Binary { 
                                        left: Box::new(left_result.0),
                                        operation: operation.clone(),
                                        right: Box::new(right_result.0)
                                    }, 
                                    expr.span.clone()
                                ),
                                spanned(
                                    TypeInfo::bool(),
                                    expr.span
                                )
                            ))
                        },
                        _ => return Err(spanned(
                            format_smolstr!("Cannot apply {:?} to {:?}", operation, left_result.1.inner), 
                            left.span
                        ))
                    }
                },
                Operation::Eq | Operation::NotEq => {
                    if left_result.1.inner != right_result.1.inner {
                        return Err(spanned(
                            format_smolstr!(
                                "Cannot compare {:?} to {:?}", 
                                left_result.1.inner, 
                                right_result.1.inner
                            ),
                            expr.span
                        ))
                    }
                    Ok((
                        spanned(
                            ir::Expr::Binary { 
                                left: Box::new(left_result.0),
                                operation: operation.clone(),
                                right: Box::new(right_result.0)
                            },
                            expr.span.clone()
                        ),
                        spanned(
                            TypeInfo::bool(),
                            expr.span
                        )
                    ))
                },
                Operation::And | Operation::Or => {
                    match (left_result.1.inner.kind(), right_result.1.inner.kind()) {
                        (TypeKind::Bool, TypeKind::Bool) => Ok((
                            spanned(
                                ir::Expr::Binary { 
                                    left: Box::new(left_result.0),
                                    operation: operation.clone(),
                                    right: Box::new(right_result.0)
                                },
                                expr.span.clone()
                            ),
                            spanned(
                                TypeInfo::bool(),
                                expr.span
                            )
                        )),
                        (a, b) => Err(spanned(
                            format_smolstr!(
                                "{:?} requires both sides to be Bool, got: {:?}, {:?}", 
                                operation, 
                                a, 
                                b
                            ),
                            expr.span
                        ))
                    }
                },
                Operation::NullCoal => {
                    match (left_result.1.inner.kind(), right_result.1.inner.kind()) {
                        (TypeKind::Null, _) => {
                            Ok((
                                spanned(
                                    ir::Expr::Binary { 
                                        left: Box::new(left_result.0),
                                        operation: operation.clone(),
                                        right: Box::new(right_result.0)
                                    },
                                    expr.span
                                ),
                                right_result.1
                            ))
                        },
                        _ => Ok((
                            spanned(
                                ir::Expr::Binary { 
                                    left: Box::new(left_result.0),
                                    operation: operation.clone(),
                                    right: Box::new(right_result.0)
                                },
                                expr.span
                            ),
                            left_result.1
                        ))
                    }
                },
                Operation::BitwiseAnd 
                | Operation::BitwiseOr 
                | Operation::BitwiseXor 
                | Operation::BitwiseLeftShift 
                | Operation::BitwiseRightShift => {
                    match left_result.1.inner.kind() {
                        TypeKind::Any | TypeKind::Int => {
                            if left_result.1.inner != right_result.1.inner {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Type Mismatch: Expected {:?}, got {:?}",
                                        left_result.1.inner,
                                        right_result.1.inner
                                    ), 
                                    expr.span
                                ))
                            }
                            Ok((
                                spanned(
                                    ir::Expr::Binary { 
                                        left: Box::new(left_result.0),
                                        operation: operation.clone(),
                                        right: Box::new(right_result.0)
                                    },
                                    expr.span.clone()
                                ),
                                spanned(
                                    TypeInfo::int(),
                                    expr.span
                                )
                            ))
                        },
                        _ => return Err(spanned(
                            format_smolstr!(
                                "Cannot apply {:?} to {:?}", 
                                operation, 
                                left_result.1.inner
                            ), 
                            left.span
                        ))
                    }
                },
            }
        },
        Expr::Block(statements, final_expr) => {
            let mut inner_scope = env.enter_scope();
            let (lowered_statements, _, _) = hoist(statements, &mut inner_scope, "")?;
            if let Some(e) = final_expr {
                let final_result = lower_expr(e, &mut inner_scope)?;
                Ok((
                    spanned(
                        ir::Expr::Block(lowered_statements, Some(Box::new(final_result.0))),
                        expr.span.clone()
                    ),
                    final_result.1
                ))
            } else {
                Ok((
                    spanned(
                        ir::Expr::Block(lowered_statements, None),
                        expr.span.clone()
                    ),
                    spanned(
                        TypeInfo::void(),
                        expr.span
                    )
                ))
            }
            
        },
        Expr::Fun { params, body, return_type, generic_params } => {
            let mut inner_scope = env.enter_scope();
            let expected_type = return_type.clone().unwrap_or(spanned(TypeInfo::void(), Span::from(0..0)));
    
            if generic_params.len() == 0 {
                let mut flat_params = Vec::new();
                let mut lower_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope)?.into_owned();
                    inner_scope.add_var_type(n.1.clone(), flattened.clone());
                    flat_params.push((n.clone(), flattened));
                    lower_params.push(n.clone());
                }
                let expected_type = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                inner_scope.add_var_type(SmolStr::new("+return"), expected_type.clone());
                let body_result = lower_expr(body, &mut inner_scope)?;
                let actual_type = flatten_type(&body_result.1, env)?;
                if actual_type.inner != expected_type.inner {
                    return Err(spanned(
                        format_smolstr!("Function return type mismatch: expected {:?}, got {:?}", expected_type.inner, actual_type.inner),
                        expected_type.span
                    ))
                }
                Ok((
                    spanned(
                        ir::Expr::Fun {
                            params: lower_params,
                            body: Box::new(body_result.0)
                        },
                        expr.span.clone()
                    ),
                    spanned(
                        TypeInfo::new(TypeKind::Fun { params: flat_params, return_type: Box::new(actual_type.into_owned()), generic_params: Vec::new() }),
                        expr.span
                    )
                ))
            } else {
                for generic in generic_params {
                    inner_scope.add_custom_type(generic.inner.clone(), spanned(
                        TypeInfo::new(TypeKind::GenericParam(generic.inner.clone())),
                        generic.span
                    ));
                }

                let mut flat_params = Vec::new();
                let mut lower_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope)?.into_owned();
                    inner_scope.add_var_type(n.1.clone(), flattened.clone());
                    flat_params.push((n.clone(), flattened));
                    lower_params.push(n.clone());
                }

                let expected_flat = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                inner_scope.add_var_type(SmolStr::new("+return"), expected_flat.clone());
                let body_result = lower_expr(body, &mut inner_scope)?;
                if body_result.1.inner != expected_flat.inner {
                    return Err(spanned(
                        format_smolstr!("Function return type mismatch: expected {:?}, got {:?}", expected_flat.inner, body_result.1.inner),
                        expected_flat.span
                    ))
                }

                Ok((
                    spanned(
                        ir::Expr::Fun { 
                            params: lower_params,
                            body: Box::new(body_result.0)
                        },
                        expr.span.clone()
                    ),
                    spanned(
                        TypeInfo::new(TypeKind::Fun { 
                            params: flat_params, 
                            return_type: Box::new(body_result.1),
                            generic_params: generic_params.clone()
                        }),
                        expr.span
                    )
                ))
            }
        },
        Expr::Call { fun, args, generic_args } => {
            let fun_result = lower_expr(fun, env)?;
            match fun_result.1.inner.kind() {
                TypeKind::Fun { params, return_type: _, generic_params } => {
                    let mut lowered_args = Vec::new();
                    let mut arg_types = Vec::new();
                    for arg in args {
                        let arg_result = lower_expr(arg, env)?;
                        arg_types.push(arg_result.1.clone());
                        lowered_args.push(arg_result.0);
                    }
                    
                    let mut generic_arg_map = HashMap::new();
                    if generic_params.len() > 0 
                    && generic_args.len() == 0 
                    && args.len() >= generic_params.len() {
                        for (param, arg_type) in params.iter().zip(arg_types.iter()) {
                            for (p_name, ti) in collect_generic_params(
                                (&param.1, param.0.0),
                                arg_type
                            ) {
                                generic_arg_map.insert(p_name, ti);
                            }
                        }
                    } else {
                        if generic_params.len() != generic_args.len() {
                            return Err(spanned(
                                format_smolstr!(
                                    "Insufficient generic arguments provided: expected {}, got {}",
                                    generic_params.len(),
                                    generic_args.len()
                                ),
                                expr.span
                            ))
                        }
                        for (param, arg) in generic_params.iter().zip(generic_args.iter()) {
                            generic_arg_map.insert(param.inner.clone(), arg.clone());
                        }
                    }
                    
                    let substituted_fun = substitute_generic_params(&fun_result.1, &generic_arg_map);
                    let substituted_params = match substituted_fun.inner.kind() {
                        TypeKind::Fun { params, .. } => params.clone(),
                        _ => return Err(spanned(
                            format_smolstr!("Substitution failed for function type"),
                            expr.span
                        ))
                    };
            
                    let substituted_return_type = match substituted_fun.inner.kind() {
                        TypeKind::Fun { return_type, .. } => return_type,
                        _ => return Err(spanned(
                            format_smolstr!("Substitution failed for function type"),
                            expr.span
                        ))
                    };
            
                    let is_variadic = if let Some(last) = substituted_params.last() {
                        last.0.0
                    } else {false};
            
                    if is_variadic {
                        if substituted_params.len() > args.len() {
                            return Err(spanned(
                                format_smolstr!(
                                    "Insufficient amount of arguments: expected at least {}, got {}",
                                    substituted_params.len(),
                                    args.len()
                                ),
                                expr.span
                            ))
                        }
                        for (i, (((v, _), p_type), arg_type)) in substituted_params.iter().zip(arg_types.iter()).enumerate() {
                            match v {
                                true => {
                                    if i != substituted_params.len() - 1 {
                                        return Err(spanned(
                                            "Cannot have multiple spread params in a function".into(),
                                            p_type.span
                                        ))
                                    } else {break}
                                },
                                false => (),
                            }
                            if arg_type.inner != p_type.inner {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Argument type mismatch: expected {:?}, got {:?}",
                                        p_type.inner,
                                        arg_type.inner
                                    ),
                                    lowered_args[i].span
                                ))
                            }
                        }
                        let spread_args = &args[substituted_params.len()-1..];
                        let temp = substituted_params.last().unwrap().1.clone();
                        let spread_type = match temp.inner.kind() {
                            TypeKind::Array(inner_type) => inner_type.clone(),
                            o => return Err(spanned(
                                format_smolstr!("Spread parameter should be an array: found {:?} instead", o),
                                temp.span
                            ))
                        };
                        for (arg, arg_type) in spread_args.iter().zip(arg_types.iter().skip(substituted_params.len()-1)) {
                            if arg_type.inner != spread_type.inner {
                                return Err(spanned(
                                    format_smolstr!("Argument type mismatch: expected {:?}, got {:?}", spread_type.inner, arg_type.inner),
                                    arg.span
                                ))
                            }
                        }
                    } else {
                        if substituted_params.len() != args.len() {
                            return Err(spanned(
                                format_smolstr!("Argument type mismatch: expected {} args, got {}", substituted_params.len(), args.len()),
                                expr.span
                            ))
                        }
                        for ((_, p_type), arg_type) in substituted_params.iter().zip(arg_types.iter()) {
                            if arg_type.inner != p_type.inner {
                                return Err(spanned(
                                    format_smolstr!("Argument type mismatch: expected {:?}, got {:?}", p_type.inner, arg_type.inner),
                                    expr.span
                                ))
                            }
                        }
                    }
            
                    Ok((
                        spanned(
                            ir::Expr::Call {
                                fun: Box::new(fun_result.0),
                                args: lowered_args
                            },
                            expr.span.clone()
                        ),
                        substituted_return_type.as_ref().clone()
                    ))
                },
                ti => Err(spanned(
                    format_smolstr!("Type {:?} is not callable", ti),
                    expr.span
                ))
            }
        },
        Expr::Record(items) => {
            let mut types = HashMap::new();
            let mut lowers = HashMap::new();
            for (name, e) in items {
                let item_result = lower_expr(e, env)?;
                types.insert(name.clone(), item_result.1);
                lowers.insert(name.clone(), item_result.0);
            }
            Ok((
                spanned(ir::Expr::Record(lowers), expr.span.clone()),
                spanned(TypeInfo::new(TypeKind::Record(types)), expr.span)
            ))
        },
        Expr::Get(target, field) => {
            let mut target_result = lower_expr(target, env)?;
            target_result.1 = flatten_type(&target_result.1, env)?.into_owned();
            if let Some((method_type, method_expr)) = env.get_method(target_result.1.inner.id(), field) {
                Ok((
                    spanned(
                        ir::Expr::Method { 
                            this: Box::new(target_result.0),
                            fun: Box::new(method_expr)
                        },
                        expr.span.clone()
                    ),
                    spanned(
                        method_type.inner,
                        expr.span
                    )
                ))
            } else {
                match target_result.1.inner.kind() {
                    TypeKind::Record(fields) => {
                        if let Some(e) = fields.get(field) {
                            return Ok((
                                spanned(
                                    ir::Expr::Get(Box::new(target_result.0), field.clone()),
                                    expr.span.clone()
                                ),
                                spanned(
                                    e.inner.clone(),
                                    expr.span
                                )
                            ))
                        }
                        Err(spanned(
                            format_smolstr!(
                                "Type {:?} does not have property {field}",
                                target_result.1.inner
                            ),
                            target.span
                        ))
                    },
                    _ => Err(spanned(
                        format_smolstr!(
                            "Type {:?} does not have property {field}",
                            target_result.1.inner
                        ),
                        target.span
                    ))
                }
            }
        },
        Expr::Assign { target, value } => {
            let target_result = lower_expr(target, env)?;
            let value_result = lower_expr(value, env)?;
            if target_result.1.inner != value_result.1.inner {
                return Err(spanned(
                    format_smolstr!(
                        "Cannot assign {:?} to {:?}", 
                        value_result.1.inner, 
                        target_result.1.inner
                    ),
                    value.span
                ))
            }
            Ok((
                spanned(
                    ir::Expr::Assign {
                        target: Box::new(target_result.0),
                        value: Box::new(value_result.0)
                    },
                    expr.span.clone()
                ),
                spanned(
                    TypeInfo::void(),
                    expr.span
                )
            ))
        },
        Expr::Unary(unary_op, e) => {
            let expr_result = lower_expr(e, env)?;
            match (unary_op, expr_result.1.inner.kind()) {
                (UnaryOp::Negate, TypeKind::Any) 
                | (UnaryOp::Negate, TypeKind::Float)
                | (UnaryOp::Negate, TypeKind::Int) => Ok((
                    spanned(
                        ir::Expr::Unary(unary_op.clone(), Box::new(expr_result.0)),
                        expr.span
                    ),
                    expr_result.1
                )),
                (UnaryOp::Not, TypeKind::Bool) => Ok((
                    spanned(
                        ir::Expr::Unary(unary_op.clone(), Box::new(expr_result.0)),
                        expr.span
                    ),
                    expr_result.1
                )),
                _ => Err(spanned(
                    format_smolstr!("Cannot apply {:?} to {:?}", unary_op, expr_result.1.inner),
                    e.span
                ))
            }
        },
        Expr::If { condition, body, else_block } => {
            let condition_result = lower_expr(condition, env)?;
            let body_result = lower_expr(body, env)?;
            let else_block_result = if let Some(block) = else_block {
                Some(lower_expr(block, env)?)
            } else {
                None
            };
            if *condition_result.1.inner.kind() != TypeKind::Bool {
                return Err(spanned(
                    "If condition should return Bool".into(),
                    condition.span
                ))
            }
            if let Some(result) = else_block_result {
                if result.1.inner != body_result.1.inner {
                    return Err(spanned(
                        format!(
                            "If condition branches have incompatible types: expected {:?}, got {:?}",
                            body_result.1.inner,
                            result.1.inner
                        ).into(),
                        result.1.span
                    ))
                }
                Ok((
                    spanned(
                        ir::Expr::If {
                            condition: Box::new(condition_result.0),
                            body: Box::new(body_result.0),
                            else_block: Some(Box::new(result.0))
                        },
                        expr.span.clone()
                    ),
                    spanned(
                        body_result.1.inner, 
                        expr.span
                    )
                ))
            } else {
                Ok((
                    spanned(
                        ir::Expr::If {
                            condition: Box::new(condition_result.0),
                            body: Box::new(body_result.0),
                            else_block: None
                        },
                        expr.span.clone()
                    ),
                    spanned(
                        body_result.1.inner, 
                        expr.span
                    )
                ))
            }
            
        },
        Expr::While { condition, body } => {
            let condition_result = lower_expr(condition, env)?;
            let body_result = lower_expr(body, env)?;
            if condition_result.1.inner != TypeInfo::bool() {
                return Err(spanned(
                    SmolStr::new("While loop condition should be a bool"), 
                    condition.span
                ))
            }
            if body_result.1.inner != TypeInfo::void() {
                return Err(spanned(
                    SmolStr::new("While loop body should return void"), 
                    body.span
                ))
            }
            Ok((
                spanned(
                    ir::Expr::While {
                        condition: Box::new(condition_result.0),
                        body: Box::new(body_result.0)
                    },
                    expr.span.clone()
                ),
                spanned(
                    TypeInfo::void(),
                    expr.span
                )
            ))
        },
        Expr::Break => Ok((
            spanned(ir::Expr::Break, expr.span.clone()),
            spanned(TypeInfo::void(), expr.span)
        )),
        Expr::Continue => Ok((
            spanned(ir::Expr::Continue, expr.span.clone()),
            spanned(TypeInfo::void(), expr.span)
        )),
        Expr::Return(e) => {
            let result = lower_expr(e, env)?;
            Ok((
                spanned(
                    ir::Expr::Return(Box::new(result.0)),
                    expr.span
                ),
                result.1
            ))
        },
        Expr::Match { target, branches } => {
            let target_result = lower_expr(target, env)?;
            
            fn match_branch(
                target: &TypeInfo, 
                pattern: &Spanned<MatchArm>,
                branch: &Spanned<Expr>,
                env: &mut TypeEnv
            ) -> Result<((Spanned<ir::MatchArm>, Spanned<ir::Expr>), Spanned<TypeInfo>, bool), Spanned<SmolStr>> {
                match &pattern.inner {
                    MatchArm::Conditional { alias, condition } => {
                        let mut inner_env = env.enter_scope();
                        inner_env.add_var_type(alias.clone(), spanned(
                            target.clone(),
                            Span::from(0..0)
                        ));
                        let cond_result = lower_expr(condition, &mut inner_env)?;
                        match cond_result.1.inner.kind() {
                            TypeKind::Bool => (),
                            _ => return Err(spanned(
                                format_smolstr!("Match guard condition should return a Bool, found {:?} instead", cond_result.1.inner),
                                condition.span
                            ))
                        }
                        let branch_result = lower_expr(branch, &mut inner_env)?;
                        let lowered_pattern = spanned(
                            ir::MatchArm::Conditional {
                                alias: alias.clone(),
                                condition: cond_result.0
                            },
                            pattern.span
                        );
                        Ok(((lowered_pattern, branch_result.0), branch_result.1, false))
                    },
                    MatchArm::Value(e) => {
                        let pattern_result = lower_expr(e, env)?;
                        if &pattern_result.1.inner != target {
                            return Err(spanned(
                                format_smolstr!("Type mismatch in match branch: expected {:?}, got {:?}", target, pattern_result.1.inner),
                                e.span
                            ))
                        }
                        let branch_result = lower_expr(branch, env)?;
                        let lowered_pattern = spanned(
                            ir::MatchArm::Value(pattern_result.0),
                            pattern.span
                        );
                        Ok(((lowered_pattern, branch_result.0), branch_result.1, false))
                    },
                    MatchArm::Default(alias) => {
                        let mut inner_env = env.enter_scope();
                        inner_env.add_var_type(alias.clone(), spanned(
                            target.clone(),
                            Span::from(0..0)
                        ));
                        let branch_result = lower_expr(branch, &mut inner_env)?;
                        let lowered_pattern = spanned(
                            ir::MatchArm::Default(alias.clone()),
                            pattern.span
                        );
                        Ok(((lowered_pattern, branch_result.0), branch_result.1, true))
                    },
                    MatchArm::EnumConstructor {..} => Err(spanned(
                        "Cannot have enum variants as patterns for non enum values".into(),
                        branch.span
                    ))
                }
            }
            
            fn match_enum_variant_branch(
                enum_name: &SmolStr,
                variant: &SmolStr,
                generic_args: &Vec<TypeInfo>,
                pattern: &Spanned<MatchArm>,
                branch: &Spanned<Expr>,
                env: &mut TypeEnv
            ) -> Result<((Spanned<ir::MatchArm>, Spanned<ir::Expr>), Spanned<TypeInfo>, bool), Spanned<SmolStr>> {
                match &pattern.inner {
                    MatchArm::Default(alias) => {
                        let mut inner_env = env.enter_scope();
                        inner_env.add_var_type(alias.clone(), spanned(
                            TypeInfo::new(TypeKind::EnumVariant { 
                                enum_name: enum_name.clone(),
                                variant: variant.clone(), 
                                generic_args: generic_args.clone()
                            }),
                            Span::from(0..0)
                        ));
                        let branch_result = lower_expr(branch, &mut inner_env)?;
                        let lowered_pattern = spanned(
                            ir::MatchArm::Default(alias.clone()),
                            pattern.span
                        );
                        Ok(((lowered_pattern, branch_result.0), branch_result.1, true))
                    },
                    MatchArm::EnumConstructor { enum_name: c_name, variant, alias } => {
                        let c_name = c_name.as_ref().unwrap_or(enum_name);
                        if let Some(ti) = env.resolve_type(c_name) {
                            if c_name != enum_name {
                                return Err(spanned(
                                    format_smolstr!("Enum mismatch: expected {enum_name}, got {c_name}"),
                                    ti.span
                                ))
                            }
                            match ti.inner.kind() {
                                TypeKind::Enum { name: _, variants, generic_params } => {
                                    if generic_params.len() != generic_args.len() {
                                        return Err(spanned(
                                            format_smolstr!(
                                                "Incorrect amount of generic arguments provided: expected {}, got {}",
                                                generic_params.len(),
                                                generic_args.len()
                                            ),
                                            ti.span
                                        ))
                                    }
                                    let inner_type = if let Some(ti) = variants.get(variant) {
                                        ti
                                    } else {
                                        return Err(spanned(
                                            format_smolstr!("Enum {enum_name} does not have a variant {variant}"),
                                            ti.span
                                        ))
                                    };
                                    
                                    let mut inner_scope = env.enter_scope();
                                    let mut generic_map = HashMap::new();
                                    for (g_name, g_type) in generic_params.iter().zip(generic_args.iter()) {
                                        generic_map.insert(g_name.inner.clone(), spanned(g_type.clone(), Span::from(0..0)));
                                        inner_scope.add_custom_type(g_name.inner.clone(), spanned(g_type.clone(), Span::from(0..0)));
                                    }
                                    let resolved_type = flatten_type(inner_type, &mut inner_scope)?.into_owned();
                                    inner_scope.add_var_type(alias.clone(), substitute_generic_params(&resolved_type, &generic_map));
                                    let branch_result = lower_expr(branch, &mut inner_scope)?;
                                    let lowered_pattern = spanned(
                                        ir::MatchArm::EnumConstructor {
                                            enum_name: enum_name.clone(),
                                            variant: variant.clone(),
                                            alias: alias.clone()
                                        },
                                        pattern.span
                                    );
                                    Ok(((lowered_pattern, branch_result.0), branch_result.1, false))
                                },
                                _ => Err(spanned(
                                    format_smolstr!("Type {c_name} is not an enum"),
                                    ti.span
                                ))
                            }
                        } else {
                            Err(spanned(
                                format_smolstr!("Cannot resolve enum {c_name}"),
                                pattern.span
                            ))
                        }
                    },
                    _ => Err(spanned(
                        "This branch is not compatible with enums".into(),
                        pattern.span
                    ))
                }
            }
            
            fn match_enum_instance_branch(
                enum_name: &SmolStr,
                variants: &HashMap<SmolStr, Spanned<TypeInfo>>,
                generic_args: &Vec<Spanned<TypeInfo>>,
                pattern: &Spanned<MatchArm>,
                branch: &Spanned<Expr>,
                env: &mut TypeEnv
            ) -> Result<((Spanned<ir::MatchArm>, Spanned<ir::Expr>), Spanned<TypeInfo>, bool), Spanned<SmolStr>> {
                match &pattern.inner {
                    MatchArm::Default(alias) => {
                        let mut inner_env = env.enter_scope();
                        inner_env.add_var_type(alias.clone(), spanned(
                            TypeInfo::new(TypeKind::EnumInstance { 
                                enum_name: enum_name.clone(),
                                variants: variants.clone(), 
                                generic_args: generic_args.clone()
                            }),
                            Span::from(0..0)
                        ));
                        let branch_result = lower_expr(branch, &mut inner_env)?;
                        let lowered_pattern = spanned(
                            ir::MatchArm::Default(alias.clone()),
                            pattern.span
                        );
                        Ok(((lowered_pattern, branch_result.0), branch_result.1, true))
                    },
                    MatchArm::EnumConstructor { enum_name: c_name, variant, alias } => {
                        let c_name = c_name.as_ref().unwrap_or(enum_name);
                        if let Some(ti) = env.resolve_type(c_name) {
                            if c_name != enum_name {
                                return Err(spanned(
                                    format_smolstr!("Enum mismatch: expected {enum_name}, got {c_name}"),
                                    ti.span
                                ))
                            }
                            match ti.inner.kind() {
                                TypeKind::Enum { name: _, variants, generic_params } => {
                                    if generic_params.len() != generic_args.len() {
                                        return Err(spanned(
                                            format_smolstr!(
                                                "Incorrect amount of generic arguments provided: expected {}, got {}",
                                                generic_params.len(),
                                                generic_args.len()
                                            ),
                                            ti.span
                                        ))
                                    }
                                    let inner_type = if let Some(ti) = variants.get(variant) {
                                        ti
                                    } else {
                                        return Err(spanned(
                                            format_smolstr!("Enum {enum_name} does not have a variant {variant}"),
                                            ti.span
                                        ))
                                    };
                                    
                                    let mut inner_scope = env.enter_scope();
                                    let mut generic_map = HashMap::new();
                                    for (g_name, g_type) in generic_params.iter().zip(generic_args.iter()) {
                                        generic_map.insert(g_name.inner.clone(), g_type.clone());
                                        inner_scope.add_custom_type(g_name.inner.clone(), g_type.clone());
                                    }
                                    let resolved_type = flatten_type(inner_type, &mut inner_scope)?.into_owned();
                                    inner_scope.add_var_type(alias.clone(), substitute_generic_params(&resolved_type, &generic_map));
                                    let branch_result = lower_expr(branch, &mut inner_scope)?;
                                    let lowered_pattern = spanned(
                                        ir::MatchArm::EnumConstructor {
                                            enum_name: enum_name.clone(),
                                            variant: variant.clone(),
                                            alias: alias.clone()
                                        },
                                        pattern.span
                                    );
                                    Ok(((lowered_pattern, branch_result.0), branch_result.1, false))
                                },
                                _ => Err(spanned(
                                    format_smolstr!("Type {c_name} is not an enum"),
                                    ti.span
                                ))
                            }
                        } else {
                            Err(spanned(
                                format_smolstr!("Cannot resolve enum {c_name}"),
                                pattern.span
                            ))
                        }
                    },
                    _ => Err(spanned(
                        "This branch is not compatible with enums".into(),
                        pattern.span
                    ))
                }
            }
            
            let mut expected_type = TypeInfo::void();
            let mut has_default = false;
            let mut lower_branches = Vec::new();
            
            match target_result.1.inner.kind() {
                TypeKind::Fun { params: _, return_type: _, generic_params: _ } => return Err(spanned(
                    "Cannot apply match to a function".into(), 
                    target.span
                )),
                TypeKind::EnumInstance { enum_name, variants, generic_args } => {
                    for (pattern, branch) in branches {
                        let ((lowered_pattern, lowered_body), branch_type, is_default) = match_enum_instance_branch(
                            enum_name, 
                            variants, 
                            generic_args, 
                            pattern, 
                            branch, 
                            env
                        )?;
                        if expected_type.kind() != &TypeKind::Void && expected_type.kind() != branch_type.inner.kind(){
                            return Err(spanned(
                                "Match branches cannot have different return types".into(),
                                branch_type.span
                            ))
                        } else {
                            expected_type = branch_type.inner.clone()
                        }
                        if is_default {
                            if has_default {
                                return Err(spanned(
                                    "Cannot have multiple default branches in a match expression".into(),
                                    branch.span
                                ))
                            } else {
                                has_default = true
                            }
                        }
                        lower_branches.push((lowered_pattern, lowered_body));
                    }
                    let variant_count = match env.resolve_type(enum_name.as_str()).unwrap().inner.kind() {
                        TypeKind::Enum { name: _, variants, generic_params: _ } => {
                            variants.len()
                        },
                        _ => panic!()
                    };
                    if branches.len() == variant_count {
                        has_default = true;
                    }
                }
                TypeKind::EnumVariant { enum_name, variant, generic_args } => {
                    for (pattern, branch) in branches {
                        let ((lowered_pattern, lowered_body), branch_type, is_default) = match_enum_variant_branch(
                            enum_name, 
                            variant, 
                            generic_args, 
                            pattern, 
                            branch, 
                            env
                        )?;
                        if expected_type.kind() != &TypeKind::Void && expected_type.kind() != branch_type.inner.kind(){
                            return Err(spanned(
                                "Match branches cannot have different return types".into(),
                                branch_type.span
                            ))
                        } else {
                            expected_type = branch_type.inner.clone()
                        }
                        if is_default {
                            if has_default {
                                return Err(spanned(
                                    "Cannot have multiple default branches in a match expression".into(),
                                    branch.span
                                ))
                            } else {
                                has_default = true
                            }
                        }
                        lower_branches.push((lowered_pattern, lowered_body));
                    }
                    let variant_count = match env.resolve_type(enum_name.as_str()).unwrap().inner.kind() {
                        TypeKind::Enum { name: _, variants, generic_params: _ } => {
                            variants.len()
                        },
                        _ => panic!()
                    };
                    if branches.len() == variant_count {
                        has_default = true;
                    }
                },
                _ti => {
                    for (pattern, branch) in branches {
                        let ((lowered_pattern, lowered_body), branch_type, is_default) = match_branch(&target_result.1.inner, pattern, branch, env)?;
                        if expected_type.kind() != &TypeKind::Any && expected_type.kind() != branch_type.inner.kind(){
                            return Err(spanned(
                                "Match branches cannot have different return types".into(),
                                branch_type.span
                            ))
                        } else {
                            expected_type = branch_type.inner
                        }
                        if is_default {
                            if has_default {
                                return Err(spanned(
                                    "Cannot have multiple default branches in a match expression".into(),
                                    branch.span
                                ))
                            } else {
                                has_default = true
                            }
                        }
                        lower_branches.push((lowered_pattern, lowered_body));
                    }
                }
            }
            if has_default {
                Ok((
                    spanned(
                        ir::Expr::Match { 
                            target: Box::new(target_result.0),
                            branches: lower_branches,
                        },
                        expr.span.clone()
                    ),
                    spanned(
                        expected_type,
                        expr.span
                    )
                ))
            } else {
                Err(spanned(
                    "Expected a default case for matching arbitrary values".into(),
                    expr.span
                ))
            }
        },
    }
}
