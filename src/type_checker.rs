use std::{borrow::Cow, collections::HashMap};

use smol_str::SmolStr;

use crate::{ast::{Expr, MatchArm, Operation, Span, Spanned, Statement, TypeInfo, TypeKind, UnaryOp}, env::TypeEnv, ir, module::{GlobalRegistry, eval_import, insert_type_module}, native::get_native_fun};

fn spanned<T>(inner: T, span: Span) -> Spanned<T> {
    Spanned { inner, span }
}

pub fn hoist(statements: &Vec<Spanned<Statement>>, env: &mut TypeEnv, path: &str) -> Result<
    (
        Vec<Spanned<Statement>>, //ordered statements
        HashMap<SmolStr, Spanned<TypeInfo>>, // var_exports
        HashMap<SmolStr, Spanned<TypeInfo>> // type_exports
    ), Spanned<SmolStr>
> {
    let mut statements = statements.clone();
    
    let mut new_statements = Vec::new();
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
            Statement::Import { symbols, path } => {
                if !is_top_level {
                    return Err(spanned(
                        "Imports should be made at the top level".into(),
                        st.span
                    ))
                }
                let (var_exports, type_exports) = eval_import(path.inner.as_str(), &GlobalRegistry)?;
                for (symbol, alias, is_type) in symbols {
                    match is_type {
                        false => {
                            let var_type = var_exports.get(&symbol.inner);
                            if var_type.is_none() {
                                return Err(spanned(
                                    format!("Cannot resolve import {symbol} from {path}").into(), 
                                    symbol.span
                                ))
                            }
                            env.add_var_type(alias.clone().unwrap_or(symbol.inner.clone()), var_type.unwrap().clone());
                        },
                        true => {
                            let import_type = type_exports.get(&symbol.inner);
                            if import_type.is_none() {
                                return Err(spanned(
                                    format!("Cannot resolve import type {symbol} from {path}").into(), 
                                    symbol.span
                                ))
                            }
                            env.add_custom_type(alias.clone().unwrap_or(symbol.inner.clone()), import_type.unwrap().clone());
                        },
                    }
                }
                new_statements.push(st);
            },
            Statement::TypeDef { name: _, type_info: _, generic_params: _ }
            | Statement::EnumDef { name: _, variants: _, generic_params: _ } => types.push(st),
            Statement::Fun { name: _, params: _, body: _, return_type: _, generic_params: _ } => functions.push(st),
            Statement::Export(statement) => {
                if !is_top_level {
                    return Err(spanned(
                        "Exports should be made at the top level".into(),
                        st.span
                    ))
                }
                match &statement.inner {
                    Statement::TypeDef { name: _, type_info: _, generic_params: _ } 
                    | Statement::EnumDef { name: _, variants: _, generic_params: _ } => types.push(st),
                    Statement::Fun { name: _, params: _, body: _, return_type: _, generic_params: _ } => functions.push(st),
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
        let (name, var_export, type_export, is_exported) = process_statement(&st, env, path)?;
        if is_exported && let Some(name) = name {
            if let Some(ti) = var_export {
                var_exports.insert(name.clone(), ti);
            }
            if let Some(ti) = type_export {
                type_exports.insert(name, ti);
            }
        }
        new_statements.push(st);
    }
    
    //processing functions
    loop {
        if functions.is_empty() {break}
        let st = functions.pop().unwrap();
        let (name, var_export, type_export, is_exported) = process_statement(&st, env, path)?;
        if is_exported && let Some(name) = name {
            if let Some(ti) = var_export {
                var_exports.insert(name.clone(), ti);
            }
            if let Some(ti) = type_export {
                type_exports.insert(name, ti);
            }
        }
        new_statements.push(st);
    }
    
    //processing the rest
    loop {
        if rest.is_empty() {break}
        let st = rest.pop().unwrap();
        let (name, var_export, type_export, is_exported) = process_statement(&st, env, path)?;
        if is_exported && let Some(name) = name {
            if let Some(ti) = var_export {
                var_exports.insert(name.clone(), ti);
            }
            if let Some(ti) = type_export {
                type_exports.insert(name, ti);
            }
        }
        new_statements.push(st);
    }
    
    let reg = GlobalRegistry;
    insert_type_module(&reg, var_exports.clone(), type_exports.clone(), env.clone(), path);
    
    Ok((new_statements, var_exports, type_exports))
}

//adds statement's type to the type env and returns (name, var_type, type_type, is_exported) tuple where type_type is for type_exports in the module
fn process_statement(statement: &Spanned<Statement>, env: &mut TypeEnv, path: &str) -> Result<
    (Option<SmolStr>, Option<Spanned<TypeInfo>>, Option<Spanned<TypeInfo>>, bool),
    Spanned<SmolStr>
> {
    match &statement.inner {
        Statement::Let { name, expr, type_info } => {
            let expr_type = get_type(expr, env)?;
            if let Some(ti) = type_info {
                let expected_type = flatten_type(ti, env)?;
                if expected_type.inner != expr_type.inner {
                    return Err(spanned(
                        format!("Type mismatch in let declaration: expected {:?}, got {:?}", expected_type.inner, expr_type.inner).into(),
                        statement.span
                    ))
                }
            }
            env.add_var_type(name.clone(), expr_type.clone());
            Ok((Some(name.clone()), Some(expr_type), None, false))
        },
        Statement::TypeDef { name, type_info, generic_params } => {
            if generic_params.len() != 0 {
                // we don't check this closure's type here, as it will be evaluated when used
                let closure_type = spanned(
                    TypeInfo::new(TypeKind::TypeClosure { params: generic_params.clone(), env: env.clone(), body: Box::new(type_info.clone()) }),
                    statement.span
                );
                env.add_custom_type(name.clone(), closure_type.clone());
                Ok((Some(name.clone()), None, Some(closure_type), false))
            } else {
                let flat_type = flatten_type(type_info, env)?.into_owned();
                env.add_custom_type(name.clone(), flat_type.clone());
                Ok((Some(name.clone()), None, Some(flat_type), false))
            }
        },
        Statement::Expr(expr) => {
            Ok((None, Some(get_type(expr, env)?), None, false))
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
                let actual_type = get_type(body, &mut inner_scope)?;
                if actual_type.inner != expected_type.inner {
                    return Err(spanned(
                        format!("Function return type mismatch: expected {:?}, got {:?}", expected_type.inner, actual_type.inner).into(),
                        expected_type.span
                    ))
                }
                let fun_type = spanned(
                    TypeInfo::new(TypeKind::Fun { params: flat_params, return_type: Box::new(actual_type), generic_params: Vec::new() }),
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok((
                    Some(name.clone()),
                    Some(fun_type),
                    None,
                    false
                ))
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

                let actual_type = get_type(body, &mut inner_scope)?;
                if actual_type.inner != expected_flat.inner {
                    return Err(spanned(
                        format!("Function return type mismatch: expected {:?}, got {:?}", expected_flat.inner, actual_type.inner).into(),
                        expected_flat.span
                    ))
                }
                let fun_type = spanned(
                    TypeInfo::new(TypeKind::Fun {
                        params: flat_params.clone(),
                        return_type: Box::new(actual_type.clone()),
                        generic_params: generic_params.clone()
                    }),
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok((
                    Some(name.clone()),
                    Some(fun_type),
                    None,
                    false
                ))
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
                Ok((
                    Some(name.clone()),
                    Some(fun_type),
                    None,
                    false
                ))
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
                Ok((
                    Some(name.clone()),
                    Some(fun_type),
                    None,
                    false
                ))
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
            
            let mut variant_funs = Vec::new();
            let mut variant_hashmap = HashMap::new();
            
            for (n, maybe_ti) in variants {
                if let Some(ti) = maybe_ti {
                    let ti = flatten_type(ti, &mut inner_scope)?.into_owned();
                    variant_funs.push((n.clone(), spanned(
                        TypeInfo::new(TypeKind::Fun { 
                            params: vec![((false, "+arg".into()), ti.clone())],
                            return_type: Box::new(spanned(
                                TypeInfo::new(TypeKind::EnumVariant { 
                                    enum_name: name.clone(),
                                    variant: n.clone(),
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
                    )));
                    variant_hashmap.insert(n.clone(), ti.clone());
                } else {
                    variant_funs.push((n.clone(), spanned(
                        TypeInfo::new(TypeKind::Fun { 
                            params: Vec::new(),
                            return_type: Box::new(spanned(
                                TypeInfo::new(TypeKind::EnumVariant { 
                                    enum_name: name.clone(),
                                    variant: n.clone(),
                                    generic_args: vec![TypeInfo::any(); generic_params.len()]
                                }),
                                statement.span
                            )),
                            generic_params: Vec::new()
                        }),
                        statement.span
                    )));
                    variant_hashmap.insert(n.clone(), spanned(TypeInfo::void(), statement.span));
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
            
            Ok((
                Some(name.clone()),
                Some(enum_var),
                Some(enum_type),
                false
            ))
        },
        Statement::Import { symbols: _, path: _ } => Err(spanned(
            "Imports should be made at the top level".into(),
            statement.span
        )),
        Statement::Export(st) => {
            if !env.is_top_level() {
                return Err(spanned(
                    "Exports should be made at the top level".into(),
                    statement.span
                ))
            }
            let (name, var_export, type_export, is_exported) = process_statement(st, env, path)?;
            if is_exported {
                return Err(spanned(
                    "Cannot export an export statement".into(),
                    st.span
                ))
            }
            if name.is_none() {
                return Err(spanned(
                    "Cannot export this".into(),
                    st.span
                ))
            }
            Ok((name, var_export, type_export, true))
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

//returrs the type of an expression while also checking for type errors
fn get_type(expr: &Spanned<Expr>, env: &mut TypeEnv) -> Result<Spanned<TypeInfo>, Spanned<SmolStr>> {
    match &expr.inner {
        Expr::Bool(_) => Ok(spanned(TypeInfo::bool(), expr.span)),
        Expr::Float(_) => Ok(spanned(TypeInfo::float(), expr.span)),
        Expr::Int(_) => Ok(spanned(TypeInfo::int(), expr.span)),
        Expr::String(_) => Ok(spanned(TypeInfo::string(), expr.span)),
        Expr::Char(_) => Ok(spanned(TypeInfo::char(), expr.span)),
        Expr::Void => Ok(spanned(TypeInfo::void(), expr.span)),
        Expr::Null => Ok(spanned(TypeInfo::null(), expr.span)),
        Expr::Var(name) => {
            Ok(env.get_var_type(name).ok_or(spanned(format!("Cannot resolve variable {name}").into(), expr.span))?)
        },
        Expr::Array(spanneds) => {
            if spanneds.len() == 0 {
                return Ok(spanned(
                    TypeInfo::new(TypeKind::Array(Box::new(spanned(TypeInfo::any(), Span::from(0..0))))),
                    expr.span
                ))
            }
            let first_type = get_type(spanneds.first().unwrap(), env)?;
            for ti in spanneds {
                let element_type = get_type(ti, env)?.inner;
                if element_type != first_type.inner {
                    return Err(spanned(
                        format!("Array Element Type Mismatch: Expected {:?}, got {:?}", element_type, first_type.inner).into(),
                        ti.span
                    ))
                }
            }
            Ok(spanned(TypeInfo::new(TypeKind::Array(Box::new(first_type))), expr.span))
        },
        Expr::Index(target, index) => {
            let index_type = get_type(index, env)?;
            if *index_type.inner.kind() != TypeKind::Int {
                return Err(spanned(
                    format!("Cannot use {:?} to index arrays", index_type.inner).into(),
                    index.span
                ))
            }
            let target_type = get_type(target, env)?;
            match target_type.inner.kind() {
                TypeKind::Array(element_type) => Ok(*element_type.clone()),
                _ => Err(spanned(
                    format!("Cannot index {:?}", target_type.inner).into(),
                    target.span
                ))
            }
        },
        Expr::Binary { left, operation, right } => {
            let left_type = get_type(left, env)?;
            let right_type = get_type(right, env)?;
            match operation {
                Operation::Add
                | Operation::Subtract 
                | Operation::Multiply
                | Operation::Divide
                | Operation::Modulo  => {
                    match left_type.inner.kind() {
                        TypeKind::Any 
                        | TypeKind::Float 
                        | TypeKind::Int => {
                            if left_type.inner != right_type.inner {
                                return Err(spanned(
                                    format!("Type Mismatch: Expeccted {:?}, got {:?}", left_type.inner, right_type.inner).into(), 
                                    left.span
                                ))
                            }
                            Ok(spanned(
                                left_type.inner.clone(),
                                Span::from(left_type.span.start..right_type.span.end)
                            ))
                        },
                        _ => return Err(spanned(
                            format!("Cannot apply {:?} to {:?}", operation, left_type).into(), 
                            left.span
                        ))
                    }
                },
                Operation::LessThan 
                | Operation::LessThanEq 
                | Operation::GreaterThan 
                | Operation::GreaterThanEq => {
                    match left_type.inner.kind() {
                        TypeKind::Any 
                        | TypeKind::Float 
                        | TypeKind::Int => {
                            if left_type.inner != right_type.inner {
                                return Err(spanned(
                                    format!("Type Mismatch: Expeccted {:?}, got {:?}", left_type.inner, right_type.inner).into(), 
                                    left.span
                                ))
                            }
                            Ok(spanned(
                                TypeInfo::bool(), 
                                Span::from(left_type.span.start..right_type.span.end)
                            ))
                        },
                        _ => return Err(spanned(
                            format!("Cannot apply {:?} to {:?}", operation, left_type).into(), 
                            left.span
                        ))
                    }
                },
                Operation::Eq | Operation::NotEq => {
                    if left_type.inner != right_type.inner {
                        return Err(spanned(
                            format!("Cannot compare {:?} to {:?}", left_type.inner, right_type.inner).into(),
                            Span::from(left_type.span.start..right_type.span.end)
                        ))
                    }
                    Ok(spanned(
                        TypeInfo::bool(),
                        Span::from(left_type.span.start..right_type.span.end)
                    ))
                },
                Operation::And | Operation::Or => {
                    match (left_type.inner.kind(), right_type.inner.kind()) {
                        (TypeKind::Bool, TypeKind::Bool) => Ok(spanned(
                            TypeInfo::bool(),
                            Span::from(left_type.span.start..right_type.span.end)
                        )),
                        (a, b) => Err(spanned(
                            format!("{:?} requires both sides to be Bool, got: {:?}, {:?}", operation, a, b).into(),
                            Span::from(left_type.span.start..right_type.span.end)
                        ))
                    }
                },
                Operation::NullCoal => {
                    match (left_type.inner.kind(), right_type.inner.kind()) {
                        (TypeKind::Null, _) => {
                            Ok(right_type)
                        },
                        _ => Ok(left_type)
                    }
                },
                Operation::BitwiseAnd 
                | Operation::BitwiseOr 
                | Operation::BitwiseXor 
                | Operation::BitwiseLeftShift 
                | Operation::BitwiseRightShift => {
                    match left_type.inner.kind() {
                        TypeKind::Any | TypeKind::Int => {
                            if left_type.inner != right_type.inner {
                                return Err(spanned(
                                    format!("Type Mismatch: Expected {:?}, got {:?}", left_type.inner, right_type.inner).into(), 
                                    left.span
                                ))
                            }
                            Ok(spanned(
                                left_type.inner,
                                Span::from(left_type.span.start..right_type.span.end)
                            ))
                        },
                        _ => return Err(spanned(
                            format!("Cannot apply {:?} to {:?}", operation, left_type).into(), 
                            left.span
                        ))
                    }
                },
            }
        },
        Expr::Block(statements, final_expr) => {
            let mut inner_scope = env.enter_scope();
            let mut r_span = if let Some(st) = statements.last() {
                st.span
            } else {
                expr.span
            };
            let mut r_type = TypeInfo::void();
            let _ = hoist(statements, &mut inner_scope, "")?;
     
            if let Some(e) = final_expr {
                r_type = get_type(e, &mut inner_scope)?.inner;
                r_span = e.span;
            }
            Ok(spanned(
                r_type,
                r_span
            ))
        },
        Expr::Fun { params, body, return_type, generic_params } => {
            let mut inner_scope = env.enter_scope();
            let expected_type = return_type.clone().unwrap_or(spanned(TypeInfo::void(), Span::from(0..0)));
    
            if generic_params.len() == 0 {
                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope)?.into_owned();
                    inner_scope.add_var_type(n.1.clone(), flattened.clone());
                    flat_params.push((n.clone(), flattened))
                }
                let expected_type = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                inner_scope.add_var_type(SmolStr::new("+return"), expected_type.clone());
                let non_flat = get_type(body, &mut inner_scope)?;
                let actual_type = flatten_type(&non_flat, env)?;
                if actual_type.inner != expected_type.inner {
                    return Err(spanned(
                        format!("Function return type mismatch: expected {:?}, got {:?}", expected_type.inner, actual_type.inner).into(),
                        expected_type.span
                    ))
                }
                Ok(spanned(
                    TypeInfo::new(TypeKind::Fun { params: flat_params, return_type: Box::new(actual_type.into_owned()), generic_params: Vec::new() }),
                    expr.span
                ))
            } else {
                for generic in generic_params {
                    inner_scope.add_custom_type(generic.inner.clone(), spanned(
                        TypeInfo::new(TypeKind::GenericParam(generic.inner.clone())),
                        generic.span
                    ));
                }

                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope)?.into_owned();
                    inner_scope.add_var_type(n.1.clone(), flattened.clone());
                    flat_params.push((n.clone(), flattened));
                }

                let expected_flat = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                inner_scope.add_var_type(SmolStr::new("+return"), expected_flat.clone());
                let actual_type = get_type(body, &mut inner_scope)?;
                if actual_type.inner != expected_flat.inner {
                    return Err(spanned(
                        format!("Function return type mismatch: expected {:?}, got {:?}", expected_flat.inner, actual_type.inner).into(),
                        expected_flat.span
                    ))
                }

                Ok(spanned(
                    TypeInfo::new(TypeKind::Fun { 
                        params: flat_params, 
                        return_type: Box::new(actual_type),
                        generic_params: generic_params.clone()
                    }),
                    expr.span
                ))
            }
        },
        Expr::Call { fun, args, generic_args } => {
            let fun_expr_type = get_type(fun, env)?;
            match fun_expr_type.inner.kind() {
                TypeKind::Fun { params, return_type: _, generic_params } => {
                    let mut generic_arg_map = HashMap::new();
                    if generic_params.len() > 0 && generic_args.len() == 0 && args.len() >= generic_params.len() {
                        for (param, arg) in params.iter().zip(args.iter()) {
                            for (p_name, ti) in collect_generic_params(
                                (&param.1, param.0.0),
                                &get_type(arg, env)?
                            ) {
                                generic_arg_map.insert(p_name, ti);
                            }
                        }
                    } else {
                        if generic_params.len() != generic_args.len() {
                            return Err(spanned(
                                format!(
                                    "Insufficient generic arguments provided: expected {}, got {}",
                                    generic_params.len(),
                                    generic_args.len()
                                ).into(),
                                expr.span
                            ))
                        }
                        for (param, arg) in generic_params.iter().zip(generic_args.iter()) {
                            generic_arg_map.insert(param.inner.clone(), arg.clone());
                        }
                    }
            
                    let substituted_fun = substitute_generic_params(&fun_expr_type, &generic_arg_map);
                    let substituted_params = match substituted_fun.inner.kind() {
                        TypeKind::Fun { params, .. } => params.clone(),
                        _ => return Err(spanned(
                            format!("Substitution failed for function type").into(),
                            expr.span
                        ))
                    };
            
                    let substituted_return_type = match substituted_fun.inner.kind() {
                        TypeKind::Fun { return_type, .. } => return_type,
                        _ => return Err(spanned(
                            format!("Substitution failed for function type").into(),
                            expr.span
                        ))
                    };
            
                    let is_variadic = if let Some (last) = substituted_params.last() {
                        last.0.0
                    } else {false};
            
                    if is_variadic {
                        if substituted_params.len() > args.len() {
                            return Err(spanned(
                                format!("Insufficient amount of arguments: expected at least {}, got {}", substituted_params.len(), args.len()).into(),
                                expr.span
                            ))
                        }
                        for (i, (((v, _), p_type), arg_expr)) in substituted_params.iter().zip(args.iter()).enumerate() {
                            match v {
                                true => {
                                    if i != substituted_params.len() - 1 {
                                        return Err(spanned(
                                            format!("Cannot have multiple spread params in a function").into(),
                                            p_type.span
                                        ))
                                    } else {break}
                                },
                                false => (),
                            }
                            let arg_type = get_type(arg_expr, env)?;
                            if arg_type.inner != p_type.inner {
                                return Err(spanned(
                                    format!("Argument type mismatch: expected {:?}, got {:?}", p_type.inner, arg_type.inner).into(),
                                    arg_expr.span
                                ))
                            }
                        }
                        let spread_args = args[substituted_params.len()-1..].into_iter();
                        let temp = substituted_params.last().unwrap().1.clone();
                        let spread_type = match temp.inner.kind() {
                            TypeKind::Array(inner_type) => inner_type.clone(),
                            o => return Err(spanned(
                                format!("Spread parameter should be an array: found {:?} instead", o).into(),
                                temp.span
                            ))
                        };
                        for arg in spread_args {
                            let arg_type = get_type(arg, env)?;
                            if arg_type.inner != spread_type.inner {
                                return Err(spanned(
                                    format!("Argument type mismatch: expected {:?}, got {:?}", spread_type.inner, arg_type.inner).into(),
                                    arg.span
                                ))
                            }
                        }
                    } else {
                        if substituted_params.len() != args.len() {
                            return Err(spanned(
                                format!("Argument type mismatch: expected {} args, got {}", substituted_params.len(), args.len()).into(),
                                expr.span
                            ))
                        }
                        for ((_, p_type), arg_expr) in substituted_params.iter().zip(args.iter()) {
                            let arg_type = get_type(arg_expr, env)?;
                            if arg_type.inner != p_type.inner {
                                return Err(spanned(
                                    format!("Argument type mismatch: expected {:?}, got {:?}", p_type.inner, arg_type.inner).into(),
                                    arg_expr.span
                                ))
                            }
                        }
                    }
            
                    Ok(substituted_return_type.as_ref().clone())
                },
                ti => Err(spanned(
                    format!("Type {:?} is not callable", ti).into(),
                    expr.span
                ))
            }
        },
        Expr::Record(items) => {
            let mut record_entries = Vec::new();
            for (name, e) in items {
                record_entries.push((name.clone(), get_type(e, env)?));
            }
            Ok(spanned(
                TypeInfo::new(TypeKind::Record(record_entries)),
                expr.span
            ))
        },
        Expr::Get(target, field) => {
            let ti = get_type(target, env)?;
            let target_type = flatten_type(&ti, env)?;
            match target_type.inner.kind() {
                TypeKind::Record(fields) => {
                    for (name, e) in fields {
                        if name == field {
                            return Ok(e.clone())
                        }
                    }
                    Err(spanned(
                        format!("Type {:?} does not have property {field}", target_type.inner).into(),
                        target.span
                    ))
                },
                _ => Err(spanned(
                    format!("Type {:?} does not have property {field}", target_type.inner).into(),
                    target.span
                ))
            }
        },
        Expr::Assign { target, value } => {
            let target_type = get_type(target, env)?.inner;
            let value_type = get_type(value, env)?.inner;
            if target_type != value_type {
                return Err(spanned(
                    format!("Cannot assign {:?} to {:?}", value_type, target_type).into(),
                    value.span
                ))
            }
            Ok(spanned(
                TypeInfo::void(),
                expr.span
            ))
        },
        Expr::Unary(unary_op, e) => {
            let expr_type = get_type(e, env)?;
            match (unary_op, expr_type.inner.kind()) {
                (UnaryOp::Negate, TypeKind::Any) 
                | (UnaryOp::Negate, TypeKind::Float)
                | (UnaryOp::Negate, TypeKind::Int) => Ok(expr_type),
                (UnaryOp::Not, TypeKind::Bool) => Ok(expr_type),
                _ => Err(spanned(
                    format!("Cannot apply {:?} to {:?}", unary_op, expr_type.inner).into(),
                    e.span
                ))
            }
        },
        Expr::If { condition, body, else_block } => {
            if *get_type(condition, env)?.inner.kind() != TypeKind::Bool {
                return Err(spanned(
                    format!("Loop condition should return Bool").into(),
                    condition.span
                ))
            }
            let body_type = get_type(body, env)?;
            if let Some(else_expr) = else_block {
                let else_type = get_type(else_expr, env)?;
                if else_type.inner != body_type.inner {
                    return Err(spanned(
                        format!(
                            "If condition branches have incompatible types: expected {:?}, got {:?}",
                            body_type.inner,
                            else_type.inner
                        ).into(),
                        else_expr.span
                    ))
                }
            }
            Ok(spanned(
                body_type.inner,
                expr.span
            ))
        },
        Expr::While { condition, body } => {
            //we *have* to check the body even though we are not using it
            let _ = get_type(body, env)?;
            if *get_type(condition, env)?.inner.kind() != TypeKind::Bool {
                return Err(spanned(
                    format!("Loop condition should return Bool").into(),
                    condition.span
                ))
            }
            Ok(spanned(
                TypeInfo::void(),
                expr.span
            ))
        },
        Expr::Break | Expr::Continue => Ok(spanned(TypeInfo::void(), expr.span)),
        Expr::Return(e) => {
            let ti = get_type(e, env)?;
            let expected = env.get_var_type("+return");
            if let Some(r_type) = expected {
                if r_type.inner == ti.inner {
                    Ok(ti)
                } else {
                    Err(spanned(
                        format!("Return type mismatch: expected {:?}, got {:?}", r_type.inner, ti.inner).into(),
                        ti.span
                    ))
                }
            } else {
                Err(spanned(
                    SmolStr::new("Cannot return outside of the function"),
                    expr.span
                ))
            }
        },
        Expr::Match { target, branches } => {
            let target_type = get_type(target, env)?;
            fn match_branch(
                target: &TypeInfo, 
                pattern: &Spanned<MatchArm>,
                branch: &Spanned<Expr>,
                env: &mut TypeEnv
            ) -> Result<(Spanned<TypeInfo>, bool), Spanned<SmolStr>> {
                match &pattern.inner {
                    MatchArm::Conditional { alias, condition } => {
                        let mut inner_env = env.enter_scope();
                        inner_env.add_var_type(alias.clone(), spanned(
                            target.clone(),
                            Span::from(0..0)
                        ));
                        let cond_type = get_type(condition, &mut inner_env)?;
                        match cond_type.inner.kind() {
                            TypeKind::Bool => (),
                            _ => return Err(spanned(
                                format!("Match guard condition should return a Bool, found {:?} instead", cond_type.inner).into(),
                                condition.span
                            ))
                        }
                        Ok((get_type(branch, &mut inner_env)?, false))
                    },
                    MatchArm::Value(expr) => {
                        let pattern_type = get_type(expr, env)?;
                        if &pattern_type.inner != target {
                            return Err(spanned(
                                format!("Type mismatch in match branch: expected {:?}, got {:?}", target, pattern_type.inner).into(),
                                expr.span
                            ))
                        }
                        Ok((get_type(branch, env)?, false))
                    },
                    MatchArm::Default(alias) => {
                        let mut inner_env = env.enter_scope();
                        inner_env.add_var_type(alias.clone(), spanned(
                            target.clone(),
                            Span::from(0..0)
                        ));
                        Ok((get_type(branch, &mut inner_env)?, true))
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
            ) -> Result<(Spanned<TypeInfo>, bool), Spanned<SmolStr>> {
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
                        Ok((get_type(branch, &mut inner_env)?, true))
                    },
                    MatchArm::EnumConstructor { enum_name: c_name, variant, alias } => {
                        if let Some(ti) = env.resolve_type(c_name) {
                            if c_name != enum_name {
                                return Err(spanned(
                                    format!("Enum mismatch: expected {enum_name}, got {c_name}").into(),
                                    ti.span
                                ))
                            }
                            match ti.inner.kind() {
                                TypeKind::Enum { name: _, variants, generic_params } => {
                                    if generic_params.len() != generic_args.len() {
                                        return Err(spanned(
                                            format!(
                                                "Incorrect amount of generic arguments provided: expected {}, got {}",
                                                generic_params.len(),
                                                generic_args.len()
                                            ).into(),
                                            ti.span
                                        ))
                                    }
                                    let inner_type = if let Some(ti) = variants.get(variant) {
                                        ti
                                    } else {
                                        return Err(spanned(
                                            format!("Enum {enum_name} does not have a variant {variant}").into(),
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
                                    Ok((get_type(branch, &mut inner_scope)?, false))
                                },
                                _ => Err(spanned(
                                    format!("Type {c_name} is not an enum").into(),
                                    ti.span
                                ))
                            }
                        } else {
                            Err(spanned(
                                format!("Cannot resolve enum {c_name}").into(),
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
            ) -> Result<(Spanned<TypeInfo>, bool), Spanned<SmolStr>> {
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
                        Ok((get_type(branch, &mut inner_env)?, true))
                    },
                    MatchArm::EnumConstructor { enum_name: c_name, variant, alias } => {
                        if let Some(ti) = env.resolve_type(c_name) {
                            if c_name != enum_name {
                                return Err(spanned(
                                    format!("Enum mismatch: expected {enum_name}, got {c_name}").into(),
                                    ti.span
                                ))
                            }
                            match ti.inner.kind() {
                                TypeKind::Enum { name: _, variants, generic_params } => {
                                    if generic_params.len() != generic_args.len() {
                                        return Err(spanned(
                                            format!(
                                                "Incorrect amount of generic arguments provided: expected {}, got {}",
                                                generic_params.len(),
                                                generic_args.len()
                                            ).into(),
                                            ti.span
                                        ))
                                    }
                                    let inner_type = if let Some(ti) = variants.get(variant) {
                                        ti
                                    } else {
                                        return Err(spanned(
                                            format!("Enum {enum_name} does not have a variant {variant}").into(),
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
                                    Ok((get_type(branch, &mut inner_scope)?, false))
                                },
                                _ => Err(spanned(
                                    format!("Type {c_name} is not an enum").into(),
                                    ti.span
                                ))
                            }
                        } else {
                            Err(spanned(
                                format!("Cannot resolve enum {c_name}").into(),
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
            
            match target_type.inner.kind() {
                TypeKind::Fun { params: _, return_type: _, generic_params: _ } => return Err(spanned(
                    "Cannot apply match to a function".into(), 
                    target.span
                )),
                TypeKind::EnumInstance { enum_name, variants, generic_args } => {
                    for (pattern, branch) in branches {
                        let (branch_type, is_default) = match_enum_instance_branch(
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
                        let (branch_type, is_default) = match_enum_variant_branch(
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
                        let (branch_type, is_default) = match_branch(&target_type.inner, pattern, branch, env)?;
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
                    }
                }
            }
            if has_default {
                Ok(spanned(
                    expected_type,
                    expr.span
                ))
            } else {
                Err(spanned(
                    "Expected a default case for matching arbitrary values".into(),
                    expr.span
                ))
            }
        }
    }
}

//replaces type aliases with their underlying type, processes type closures
fn flatten_type<'a>(type_info: &'a Spanned<TypeInfo>, env: &mut TypeEnv) -> Result<Cow<'a, Spanned<TypeInfo>>, Spanned<SmolStr>> {
    match type_info.inner.kind() {
        TypeKind::Custom { name, generic_args } => {
            let resolved_type = if let Some(ti) = env.resolve_type(name.as_str()) {
                ti
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
                        TypeInfo::new(TypeKind::Fun { 
                            params: new_params, 
                            return_type: Box::new(flatten_type(return_type, &mut new_scope)?.into_owned()),
                            generic_params: Vec::new()
                        }),
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
                        TypeInfo::new(TypeKind::EnumInstance { enum_name: name.clone(), variants: variants.clone(), generic_args: generic_args.clone() }),
                        type_info.span
                    )))
                },
                TypeKind::TypeClosure { params, env: closure_scope, body } => {
                    if params.len() != generic_args.len() {
                        return Err(spanned(
                            format!("A wrong number of generic args provided: expected: {}, got {}", params.len(), generic_args.len()).into(),
                            resolved_type.span
                        ))
                    }
                    let mut inner_scope = closure_scope.clone();
                    for (param, arg) in params.iter().zip(generic_args.iter()) {
                        let flat_arg = flatten_type(arg, env)?.into_owned();
                        inner_scope.add_custom_type(param.inner.clone(), flat_arg);
                    }
                    Ok(Cow::Owned(flatten_type(body, &mut inner_scope)?.into_owned()))
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
                    TypeInfo::new(TypeKind::Fun {
                        params: new_params,
                        return_type: Box::new(final_return),
                        generic_params: generic_params.clone(),
                    }),
                    type_info.span,
                )));
            }

            // No generic parameters: just flatten everything normally
            let mut new_params = Vec::new();
            for (a, ti) in params {
                new_params.push((a.clone(), flatten_type(ti, env)?.into_owned()));
            }
            Ok(Cow::Owned(spanned(
                TypeInfo::new(TypeKind::Fun {
                    params: new_params,
                    return_type: Box::new(flatten_type(return_type, env)?.into_owned()),
                    generic_params: Vec::new(),
                }),
                type_info.span,
            )))
        },
        TypeKind::Record(entries) => {
            let mut new_entries = Vec::new();
            for (name, ti) in entries {
                new_entries.push((name.clone(), flatten_type(ti, env)?.into_owned()));
            }
            Ok(Cow::Owned(spanned(
                TypeInfo::new(TypeKind::Record(new_entries)),
                type_info.span
            )))
        },
        TypeKind::Array(ti) => Ok(Cow::Owned(spanned(
            TypeInfo::new(TypeKind::Array(Box::new(flatten_type(ti, env)?.into_owned()))),
            type_info.span
        ))),
        _ => Ok(Cow::Borrowed(type_info))
    }
}

// blindly converts statements to lower version without types. all the type checking should be done prior to that
pub fn lower_statement(statement: &Spanned<Statement>, env: &mut TypeEnv) -> Result<Option<Spanned<ir::Statement>>, Spanned<SmolStr>> {
    match &statement.inner {
        Statement::Let { name, expr, type_info: _ } => Ok(Some(spanned(
            ir::Statement::Let { name: name.clone(), expr: lower_expr(expr, env)? },
            statement.span
        ))),
        Statement::TypeDef { name: _, type_info: _, generic_params: _ } => Ok(None),
        Statement::Expr(expr) => Ok(Some(spanned(
            ir::Statement::Expr(lower_expr(expr, env)?),
            statement.span
        ))),
        Statement::Fun { name, params, body, return_type: _, generic_params: _ } => {
            let mut lower_params = Vec::new();
            for (a, _) in params {
                lower_params.push(a.clone());
            }
            Ok(Some(spanned(
                ir::Statement::Let { 
                    name: name.clone(),
                    expr: spanned(
                        ir::Expr::Fun {
                            params: lower_params, 
                            body: Box::new(lower_expr(body, env)?)
                        },
                        statement.span
                    )
                },
                statement.span
            )))
        },
        Statement::NativeFun { name, params: _, return_type: _, generic_params: _ } => {
            Ok(Some(spanned(
                ir::Statement::NativeFun(name.clone()),
                statement.span
            )))
        },
        Statement::EnumDef { name, variants, generic_params: _ } => {
            let mut record_exprs = Vec::new();
            for (v_name, v_type) in variants{
                match v_type {
                    Some(ti) => {
                        record_exprs.push(
                            (v_name.clone(), Spanned {
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
        Statement::Import { symbols, path } => {
            let mut new_symbols = Vec::new();
            for (name, alias, is_type) in symbols {
                match is_type {
                    true => continue,
                    false => {
                        new_symbols.push((name.clone(), alias.clone()));
                    },
                }
            }
            Ok(Some(spanned(
                ir::Statement::Import { symbols: new_symbols, path: path.clone() }, 
                statement.span
            )))
        },
        Statement::Export(st) => {
            if let Some(lower_st) = lower_statement(st, env)? {
                Ok(Some(spanned(
                    ir::Statement::Export(Box::new(lower_st)),
                    statement.span
                )))
            } else {
                Ok(None)
            }
        },
    }
}

fn lower_expr(expr: &Spanned<Expr>, env: &mut TypeEnv) -> Result<Spanned<ir::Expr>, Spanned<SmolStr>> {
    match &expr.inner {
        Expr::Bool(b) => Ok(spanned(ir::Expr::Bool(*b), expr.span)),
        Expr::Float(f) => Ok(spanned(ir::Expr::Float(*f), expr.span)),
        Expr::Int(i) => Ok(spanned(ir::Expr::Int(*i), expr.span)),
        Expr::String(s) => Ok(spanned(ir::Expr::String(s.clone()), expr.span)),
        Expr::Char(c) => Ok(spanned(ir::Expr::Char(c.clone()), expr.span)),
        Expr::Void => Ok(spanned(ir::Expr::Void, expr.span)),
        Expr::Null => Ok(spanned(ir::Expr::Null, expr.span)),
        Expr::Var(name) => Ok(spanned(ir::Expr::Var(name.clone()), expr.span)),
        Expr::Array(spanneds) => {
            let mut lowered = Vec::new();
            for e in spanneds {
                lowered.push(lower_expr(e, env)?);
            }
            Ok(spanned(ir::Expr::Array(lowered), expr.span))
        },
        Expr::Index(target, index) => {
            Ok(spanned(
                ir::Expr::Index(
                    Box::new(lower_expr(target, env)?),
                    Box::new(lower_expr(index, env)?)
                ),
                expr.span
            ))
        },
        Expr::Binary { left, operation, right } => {
            Ok(spanned(
                ir::Expr::Binary {
                    left: Box::new(lower_expr(left, env)?),
                    operation: operation.clone(),
                    right: Box::new(lower_expr(right, env)?)
                },
                expr.span
            ))
        },
        Expr::Block(statements, final_expr) => {
            let mut lowered_statements = Vec::new();
            for st in statements {
                if let Some(lower_st) = lower_statement(st, env)? {
                    lowered_statements.push(lower_st);
                }
            }
            let lowered_final = if let Some(e) = final_expr {
                Some(Box::new(lower_expr(e, env)?))
            } else {
                None
            };
            Ok(spanned(ir::Expr::Block(lowered_statements, lowered_final), expr.span))
        },
        Expr::Fun { params, body, return_type: _, generic_params: _ } => {
            let mut lower_params = Vec::new();
            for (n, _) in params {
                lower_params.push(n.clone());
            }
            Ok(spanned(
                ir::Expr::Fun {
                    params: lower_params,
                    body: Box::new(lower_expr(body, env)?)
                },
                expr.span
            ))
        },
        Expr::Call { fun, args, generic_args: _ } => {
            let mut lowered_args = Vec::new();
            for arg in args {
                lowered_args.push(lower_expr(arg, env)?);
            }
            Ok(spanned(
                ir::Expr::Call {
                    fun: Box::new(lower_expr(fun, env)?),
                    args: lowered_args
                },
                expr.span
            ))
        },
        Expr::Record(items) => {
            let mut lowered_items = Vec::new();
            for (name, e) in items {
                lowered_items.push((name.clone(), lower_expr(e, env)?));
            }
            Ok(spanned(ir::Expr::Record(lowered_items), expr.span))
        },
        Expr::Get(target, field) => {
            Ok(spanned(
                ir::Expr::Get(Box::new(lower_expr(target, env)?), field.clone()),
                expr.span
            ))
        },
        Expr::Assign { target, value } => {
            Ok(spanned(
                ir::Expr::Assign {
                    target: Box::new(lower_expr(target, env)?),
                    value: Box::new(lower_expr(value, env)?)
                },
                expr.span
            ))
        },
        Expr::Unary(unary_op, e) => {
            Ok(spanned(
                ir::Expr::Unary(unary_op.clone(), Box::new(lower_expr(e, env)?)),
                expr.span
            ))
        },
        Expr::If { condition, body, else_block } => {
            let lowered_else = if let Some(else_expr) = else_block {
                Some(Box::new(lower_expr(else_expr, env)?))
            } else {
                None
            };
            Ok(spanned(
                ir::Expr::If {
                    condition: Box::new(lower_expr(condition, env)?),
                    body: Box::new(lower_expr(body, env)?),
                    else_block: lowered_else
                },
                expr.span
            ))
        },
        Expr::While { condition, body } => {
            Ok(spanned(
                ir::Expr::While {
                    condition: Box::new(lower_expr(condition, env)?),
                    body: Box::new(lower_expr(body, env)?)
                },
                expr.span
            ))
        },
        Expr::Break => Ok(spanned(ir::Expr::Break, expr.span)),
        Expr::Continue => Ok(spanned(ir::Expr::Continue, expr.span)),
        Expr::Return(e) => {
            Ok(spanned(
                ir::Expr::Return(Box::new(lower_expr(e, env)?)),
                expr.span
            ))
        },
        Expr::Match { target, branches } => {
            Ok(spanned(
                ir::Expr::Match { 
                    target: Box::new(lower_expr(target, env)?),
                    branches: {
                        let mut lower_branches = Vec::new();
                        for (pattern, body) in branches {
                            let lowered_pattern = match &pattern.inner {
                                MatchArm::Conditional { alias, condition } => spanned(
                                    ir::MatchArm::Conditional {
                                        alias: alias.clone(),
                                        condition: lower_expr(condition, env)?
                                    },
                                    pattern.span
                                ),
                                MatchArm::Value(e) => spanned(
                                    ir::MatchArm::Value(lower_expr(e, env)?),
                                    pattern.span
                                ),
                                MatchArm::Default(alias) => spanned(
                                    ir::MatchArm::Default(alias.clone()),
                                    pattern.span
                                ),
                                MatchArm::EnumConstructor { enum_name, variant, alias } => spanned(
                                    ir::MatchArm::EnumConstructor {
                                        enum_name: enum_name.clone(),
                                        variant: variant.clone(),
                                        alias: alias.clone()
                                    },
                                    pattern.span
                                ),
                            };
                            lower_branches.push((lowered_pattern, lower_expr(body, env)?))
                        }
                        lower_branches
                    },
                },
                expr.span
            ))
        },
    }
}
