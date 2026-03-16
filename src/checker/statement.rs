use crate::checker::expression::process_expression;
use std::collections::HashMap;

use smol_str::{SmolStr, format_smolstr};

use crate::{ast::{clean, typed::{Expr, LetPattern, Statement, TypeInfo, TypeKind}}, checker::{flatten::flatten_type, mutability::check_variable_mutability}, env::TypeEnv, module::{GlobalRegistry, eval_import}, native::{get_native_fun, get_native_type}, span::{Span, Spanned, spanned}};

pub struct LoweringResult {
    pub name: Option<SmolStr>,
    pub var_type: Option<Spanned<TypeInfo>>,
    pub custom_type: Option<Spanned<TypeInfo>>,
    pub export: bool,
    pub lowered_statement: Option<Spanned<clean::Statement>>,
}

pub fn process_statement(statement: &Spanned<Statement>, env: &mut TypeEnv, path: &str) -> Result<
    LoweringResult,
    Spanned<SmolStr>
> {
    match &statement.inner {
        Statement::Let { pattern, expr, type_info, mutable } => {
            process_let_statement(pattern, expr, type_info, mutable, statement, env)
        },
        Statement::TypeDef { name, type_info, generic_params, implementation: _, interfaces: _ } => {
            process_type_statement(name, generic_params, type_info, statement, env)
        },
        Statement::Expr(expr) => {
            let expr_result = process_expression(expr, env)?;
            Ok(LoweringResult { 
                name: None,
                var_type: Some(expr_result.1), 
                custom_type: None,
                export: false,
                lowered_statement: Some(spanned(
                    clean::Statement::Expr(expr_result.0),
                    statement.span
                ))
            })
        },
        Statement::Fun { name, params, body, return_type, generic_params } => {
            process_function_statement(name, generic_params, params, return_type, body, statement, env)
        },
        Statement::NativeFun { name, params, return_type, generic_params } => {
            process_native_function_statement(name, generic_params, params, return_type, path, statement, env)
        },
        Statement::EnumDef { name, variants, generic_params, implementation: _, interfaces: _ } => {
            process_enum_def(name, variants, generic_params, statement, env)
        },
        Statement::Import { symbols, path } => {
            process_import_statement(symbols, path, statement, env)
        },
        Statement::NativeType { name, generic_params: _, implementation: _, interfaces: _ } => {
            process_native_type_statement(name, path, statement, env)
        },
        Statement::Export(exportee) => {
            process_export_statement(exportee, path, statement, env)
        },
    }
}

fn process_let_statement(
    pattern: &Spanned<LetPattern>, 
    expr: &Spanned<Expr>, 
    type_info: &Option<Spanned<TypeInfo>>,
    mutable: &bool, 
    statement: &Spanned<Statement>,
    env: &mut TypeEnv
) -> Result<LoweringResult, Spanned<SmolStr>> {
    let expr_result = process_expression(expr, env)?;
    let expected_type = if let Some(ti) = type_info {
        let expected_type = flatten_type(ti, env)?;
        if expected_type.inner != expr_result.1.inner {
            return Err(spanned(
                format_smolstr!(
                    "Type mismatch in let declaration: expected {}, got {}",
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
    
    let mut_result = check_variable_mutability(&expr_result.0, env);
    match mut_result {
        Ok(is_mut) => {
            if is_mut != *mutable {
                match is_mut {
                    true => return Err(spanned(
                        "Cannot assign an immutable reference to a mutable variable".into(),
                        expr_result.0.span
                    )),
                    false => return Err(spanned(
                        "Cannot assign a mutable reference to an immutable variable".into(),
                        expr_result.0.span
                    )),
                }
            }
        },
        Err(_) => (),
    }
    
    fn lower_pattern(
        pattern: &Spanned<LetPattern>,
        expected_type: &Spanned<TypeInfo>,
        mutable: &bool,
        env: &mut TypeEnv
    ) -> Result<Spanned<clean::LetPattern>, Spanned<SmolStr>> {
        match (&pattern.inner, expected_type.inner.kind()) {
            (LetPattern::Tuple(patterns), TypeKind::Tuple(types)) => {
                let mut lower_patterns = Vec::new();
                for (p, ti) in patterns.iter().zip(types.iter()) {
                    lower_patterns.push(
                        lower_pattern(
                            &p,
                            &*ti,
                            mutable,
                            env
                        )?
                    );
                }
                Ok(spanned(
                    clean::LetPattern::Tuple(lower_patterns),
                    pattern.span
                ))
            },
            (LetPattern::Record(elements), TypeKind::Record(map)) => {
                let mut lowered_elements = Vec::new();
                for (name, maybe_alias) in elements {
                    let ti = if let Some(ti) = map.get(&name.inner) {
                        ti
                    } else {
                        return Err(spanned(
                            format_smolstr!(
                                "Type {expected_type} does not have a property {name}"
                            ), 
                            name.span
                        ))
                    };
                    
                    if let Some(alias) = maybe_alias {
                        lowered_elements.push((name.inner.clone(), Some(alias.inner.clone())));
                        env.add_var_type(alias.inner.clone(), ti.clone(), mutable.clone());
                    } else {
                        lowered_elements.push((name.inner.clone(), None));
                        env.add_var_type(name.inner.clone(), ti.clone(), mutable.clone());
                    }
                }
                
                Ok(spanned(
                    clean::LetPattern::Record(lowered_elements), 
                    pattern.span
                ))
            },
            (LetPattern::Tuple(_), ti) | (LetPattern::Record(_), ti) => {
                Err(spanned(
                    format_smolstr!("Type {ti} cannot be destructured"), 
                    pattern.span
                ))
            },
            (LetPattern::Name(name), _) => {
                env.add_var_type(name.clone(), expected_type.clone(), mutable.clone());
                Ok(spanned(
                    clean::LetPattern::Name(name.clone()),
                    pattern.span
                ))
            }
        }
    }
    
    let name = match &pattern.inner {
        LetPattern::Name(smol_str) => Some(smol_str.clone()),
        _ => None,
    };
    
    let pattern = lower_pattern(
        pattern,
        &expected_type,
        mutable,
        env
    )?;
        
    Ok(LoweringResult { 
        name,
        var_type: Some(expected_type), 
        custom_type: None,
        export: false,
        lowered_statement: Some(spanned(
            clean::Statement::Let { 
                pattern,
                expr: expr_result.0
            },
            statement.span
        ))
    })
}

fn process_type_statement(
    name: &SmolStr,
    generic_params: &Vec<Spanned<SmolStr>>, 
    type_info: &Spanned<TypeInfo>, 
    statement: &Spanned<Statement>,
    env: &mut TypeEnv
) -> Result<LoweringResult, Spanned<SmolStr>> {
    let mut generic_scope = env.enter_scope();
    let mut source_id = None;
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
        let temp = flatten_type(type_info, env)?.into_owned();
        source_id = Some(temp.inner.id());
        spanned(
            TypeInfo::new(temp.inner.kind().clone()),
            temp.span
        )
    };
    if let Some(id) = source_id {
        env.schedule_or_try_method_copy(id, flat_type.inner.id());
    }

    env.add_custom_type(name.clone(), flat_type.clone());
    Ok(LoweringResult { 
        name: Some(name.clone()),
        var_type: None, 
        custom_type: Some(flat_type),
        export: false,
        lowered_statement: None
    })
}

fn process_function_statement(
    name: &SmolStr,
    generic_params: &Vec<Spanned<SmolStr>>,
    params: &Vec<((bool, SmolStr), Spanned<TypeInfo>)>,
    return_type: &Option<Spanned<TypeInfo>>,
    body: &Spanned<Expr>,
    statement: &Spanned<Statement>,
    env: &mut TypeEnv
) -> Result<LoweringResult, Spanned<SmolStr>> {
    let mut inner_scope = env.enter_scope();
    let expected_type = return_type.clone().unwrap_or(spanned(TypeInfo::void(), Span::at(0)));

    if generic_params.len() == 0 {
        let mut flat_params = Vec::new();
        for (n, p_type) in params {
            let flattened = flatten_type(p_type, &mut inner_scope,)?.into_owned();
            inner_scope.add_var_type(n.1.clone(), flattened.clone(), false);
            flat_params.push((n.clone(), flattened))
        }
        let expected_type = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
        inner_scope.add_var_type(SmolStr::new("+return"), expected_type.clone(), false);
        inner_scope.add_var_type(
            name.clone(),
            spanned(
                TypeInfo::new(TypeKind::Fun {
                    params: flat_params.clone(), 
                    return_type: Box::new(expected_type.clone()),
                    generic_params: generic_params.clone()
                }),
                statement.span
            ),
            false
        );
        let body_result = process_expression(body, &mut inner_scope)?;
        if body_result.1.inner != expected_type.inner {
            return Err(spanned(
                format_smolstr!(
                    "Function return type mismatch: expected {}, got {}",
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
        env.add_var_type(name.clone(), fun_type.clone(), false);
        Ok(LoweringResult { 
            name: Some(name.clone()),
            var_type: Some(fun_type), 
            custom_type: None,
            export: false,
            lowered_statement: Some(spanned(
                clean::Statement::Let { 
                    pattern: spanned(
                        clean::LetPattern::Name(name.clone()),
                        statement.span
                    ),
                    expr: spanned(
                        clean::Expr::Fun { 
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
            inner_scope.add_var_type(n.1.clone(), flattened.clone(), false);
            flat_params.push((n.clone(), flattened));
        }

        let expected_flat = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
        inner_scope.add_var_type(SmolStr::new("+return"), expected_flat.clone(), false);

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
            fun_type.clone(),
            false
        );

        let body_result = process_expression(body, &mut inner_scope)?;
        if body_result.1.inner != expected_flat.inner {
            return Err(spanned(
                format_smolstr!(
                    "Function return type mismatch: expected {}, got {}",
                    expected_flat.inner,
                    body_result.1.inner
                ),
                expected_flat.span
            ))
        }
        env.add_var_type(name.clone(), fun_type.clone(), false);
        Ok(LoweringResult { 
            name: Some(name.clone()),
            var_type: Some(fun_type), 
            custom_type: None,
            export: false,
            lowered_statement: Some(spanned(
                clean::Statement::Let { 
                    pattern: spanned(
                        clean::LetPattern::Name(name.clone()),
                        statement.span
                    ),
                    expr: spanned(
                        clean::Expr::Fun { 
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
}

fn process_native_function_statement(
    name: &SmolStr,
    generic_params: &Vec<Spanned<SmolStr>>,
    params: &Vec<((bool, SmolStr), Spanned<TypeInfo>)>,
    return_type: &Option<Spanned<TypeInfo>>,
    path: &str,
    statement: &Spanned<Statement>,
    env: &mut TypeEnv
) -> Result<LoweringResult, Spanned<SmolStr>> {
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
        env.add_var_type(name.clone(), fun_type.clone(), false);
        Ok(LoweringResult { 
            name: Some(name.clone()),
            var_type: Some(fun_type), 
            custom_type: None,
            export: false,
            lowered_statement: Some(spanned(
                clean::Statement::NativeFun(name.clone()),
                statement.span
            ))
        })
    } else {
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

        let r_flat = flatten_type(&r_type, &mut inner_scope)?.into_owned();

        let fun_type = spanned(
            TypeInfo::new(TypeKind::Fun {
                params: flat_params.clone(),
                return_type: Box::new(r_flat.clone()),
                generic_params: generic_params.clone()
            }),
            statement.span
        );
        env.add_var_type(name.clone(), fun_type.clone(), false);
        Ok(LoweringResult { 
            name: Some(name.clone()),
            var_type: Some(fun_type), 
            custom_type: None,
            export: false,
            lowered_statement: Some(spanned(
                clean::Statement::NativeFun(name.clone()),
                statement.span
            ))
        })
    }
}

fn process_enum_def(
    name: &SmolStr,
    variants: &Vec<(SmolStr, Option<Spanned<TypeInfo>>)>,
    generic_params: &Vec<Spanned<SmolStr>>,
    statement: &Spanned<Statement>,
    env: &mut TypeEnv
) -> Result<LoweringResult, Spanned<SmolStr>>{
    let mut generic_scope = env.enter_scope();
    if generic_params.len() != 0 {
        for param in generic_params {
            generic_scope.add_custom_type(
                param.inner.clone(),
                spanned(
                    TypeInfo::new(TypeKind::GenericParam(param.inner.clone())),
                    param.span.clone()
                )
            );
        }
    }

    let mut inner_scope = env.enter_scope();
    for p in generic_params {
        inner_scope.add_custom_type(p.inner.clone(), spanned(
            TypeInfo::new(TypeKind::GenericParam(p.inner.clone())),
            p.span
        ));
    }

    // Create enum_type first to get its typeId for use in EnumVariant types
    let enum_type = spanned(
        TypeInfo::new(TypeKind::Enum {
            name: name.clone(),
            variants: HashMap::new(), 
            generic_params: generic_params.clone()
        }),
        statement.span
    );
    let enum_type_id = enum_type.inner.id();

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
                    inner: clean::Expr::Fun { 
                        params: vec![(false, SmolStr::new("+arg"))], 
                        body: Box::new(Spanned {
                            inner: clean::Expr::EnumConstructor { 
                                enum_name: name.clone(),
                                variant: v_name.clone(),
                                value: Box::new(Spanned {
                                    inner: clean::Expr::Var(SmolStr::new("+arg")),
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
                        TypeInfo::new_with_id(TypeKind::EnumVariant { 
                            enum_name: name.clone(),
                            variant: v_name.clone(),
                            generic_args: generic_params
                                .clone()
                                .into_iter()
                                .map(|name| spanned(
                                    TypeInfo::new(TypeKind::GenericParam(name.inner)),
                                    name.span
                                ))
                                .collect()
                        }, enum_type_id),
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
                        TypeInfo::new_with_id(TypeKind::EnumVariant { 
                            enum_name: name.clone(),
                            variant: v_name.clone(),
                            generic_args: vec![spanned(TypeInfo::any(), Span::from(0..0)); generic_params.len()]
                        }, enum_type_id),
                        statement.span
                    )),
                    generic_params: Vec::new()
                }),
                statement.span
            ));
            record_exprs.insert(
                v_name.clone(),
                Spanned {
                    inner: clean::Expr::Fun { 
                        params: vec![], 
                        body: Box::new(Spanned {
                            inner: clean::Expr::EnumConstructor { 
                                enum_name: name.clone(),
                                variant: v_name.clone(),
                                value: Box::new(Spanned {
                                    inner: clean::Expr::Void,
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

    // Update enum_type with the correct variants
    let enum_type = spanned(
        TypeInfo::new_with_id(TypeKind::Enum { 
            name: name.clone(), 
            variants: variant_hashmap, 
            generic_params: generic_params.clone() 
        }, enum_type_id),
        statement.span
    );

    env.add_custom_type(name.clone(), enum_type.clone());
    env.add_var_type(name.clone(), enum_var.clone(), false);

    Ok(LoweringResult { 
        name: Some(name.clone()),
        var_type: Some(enum_var), 
        custom_type: Some(enum_type),
        export: false,
        lowered_statement: Some(spanned(
            clean::Statement::Let { 
                pattern: spanned(
                    clean::LetPattern::Name(name.clone()),
                    statement.span
                ),
                expr: Spanned {
                    inner: clean::Expr::Record(record_exprs),
                    span: statement.span
                }
            },
            statement.span
        ))
    })
}

fn process_import_statement(
    symbols: &Vec<(Spanned<SmolStr>, Option<SmolStr>, bool)>,
    path: &Spanned<SmolStr>,
    statement: &Spanned<Statement>,
    env: &mut TypeEnv
) -> Result<LoweringResult, Spanned<SmolStr>> {
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
                    var_type.unwrap().clone(),
                    false
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
                    clean::Statement::Import { 
                        symbols: new_symbols, 
                        path: path.clone()
                    },
                    statement.span
                ))
            } else { None }
        }
    })
}

fn process_export_statement(
    exportee: &Spanned<Statement>,
    path: &str,
    statement: &Spanned<Statement>,
    env: &mut TypeEnv
) -> Result<LoweringResult, Spanned<SmolStr>>{
    if !env.is_top_level() {
        return Err(spanned(
            "Exports should be made at the top level".into(),
            statement.span
        ))
    }
    let result = process_statement(exportee, env, path)?;
    if result.export {
        return Err(spanned(
            "Cannot export an export statement".into(),
            exportee.span
        ))
    }
    if result.name.is_none() {
        return Err(spanned(
            "Cannot export this".into(),
            exportee.span
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
                    clean::Statement::Export(Box::new(result.lowered_statement.unwrap())),
                    statement.span
                ))
            } else { None }
        }
    })
}

fn process_native_type_statement(
    name: &SmolStr,
    path: &str,
    statement: &Spanned<Statement>,
    env: &mut TypeEnv
) -> Result<LoweringResult, Spanned<SmolStr>>{
    let ti = get_native_type(path, name);
    if ti.is_none() {
        return Err(spanned(
            format_smolstr!("Cannot find native type definition for type {name}"),
            statement.span
        ))
    }
    let ti = ti.unwrap();
    env.add_custom_type(name.clone(), spanned(ti.clone(), statement.span));
    Ok(LoweringResult { 
        name: Some(name.clone()),
        var_type: None,
        custom_type: Some(spanned(ti, statement.span)),
        export: false,
        lowered_statement: None
    })
}