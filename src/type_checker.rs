use std::{borrow::Cow, collections::HashMap};

use crate::{ast::{Expr, Operation, Span, Spanned, Statement, TypeInfo, UnaryOp}, env::TypeEnv, ir, module::{GlobalRegistry, eval_import, insert_type_module}, native::get_native_fun};

fn spanned<T>(inner: T, span: Span) -> Spanned<T> {
    Spanned { inner, span }
}

pub fn hoist(statements: &Vec<Spanned<Statement>>, env: &mut TypeEnv, path: &str) -> Result<
    (
        Vec<Spanned<Statement>>, //ordered statements
        HashMap<String, Spanned<TypeInfo>>, // var_exports
        HashMap<String, Spanned<TypeInfo>> // type_exports
    ), Spanned<String>
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
                        true => {
                            let var_type = var_exports.get(&symbol.inner);
                            if var_type.is_none() {
                                return Err(spanned(
                                    format!("Cannot resolve import {symbol} from {path}"), 
                                    symbol.span
                                ))
                            }
                            env.add_var_type(alias.clone().unwrap_or(symbol.inner.clone()), var_type.unwrap().clone());
                        },
                        false => {
                            let import_type = type_exports.get(&symbol.inner);
                            if import_type.is_none() {
                                return Err(spanned(
                                    format!("Cannot resolve import type {symbol} from {path}"), 
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
    (Option<String>, Option<Spanned<TypeInfo>>, Option<Spanned<TypeInfo>>, bool),
    Spanned<String>
> {
    match &statement.inner {
        Statement::Let { name, expr, type_info } => {
            let expr_type = get_type(expr, env, path)?;
            if let Some(ti) = type_info {
                let expected_type = flatten_type(ti, env, path)?;
                if expected_type.inner != expr_type.inner {
                    return Err(spanned(
                        format!("Type mismatch in let declaration: expected {:?}, got {:?}", expected_type.inner, expr_type.inner),
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
                    TypeInfo::TypeClosure { params: generic_params.clone(), env: env.clone(), body: Box::new(type_info.clone()) },
                    statement.span
                );
                env.add_custom_type(name.to_string(), closure_type.clone());
                Ok((Some(name.clone()), None, Some(closure_type), false))
            } else {
                let flat_type = flatten_type(type_info, env, path)?.into_owned();
                env.add_custom_type(name.clone(), flat_type.clone());
                Ok((Some(name.clone()), None, Some(flat_type), false))
            }
        },
        Statement::Expr(expr) => {
            Ok((None, Some(get_type(expr, env, path)?), None, false))
        },
        Statement::Fun { name, params, body, return_type, generic_params } => {
            let mut inner_scope = env.enter_scope();
            let expected_type = return_type.clone().unwrap_or(spanned(TypeInfo::Void, Span::from(0..0)));
            
            if generic_params.len() == 0 {
                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope, path)?.into_owned();
                    inner_scope.add_var_type(n.1.clone(), flattened.clone());
                    flat_params.push((n.clone(), flattened))
                }
                inner_scope.add_var_type(
                    name.to_string(),
                    spanned(
                        TypeInfo::Fun {
                            params: flat_params.clone(), 
                            return_type: Box::new(expected_type.clone()),
                            generic_params: generic_params.clone()
                        },
                        statement.span
                    )
                );
                let actual_type = get_type(body, &mut inner_scope, path)?;
                if actual_type.inner != expected_type.inner {
                    return Err(spanned(
                        format!("Function return type mismatch: expected {:?}, got {:?}", expected_type.inner, actual_type.inner),
                        expected_type.span
                    ))
                }
                let fun_type = spanned(
                    TypeInfo::Fun { params: flat_params, return_type: Box::new(expected_type), generic_params: Vec::new() },
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok((
                    Some(name.to_string()),
                    Some(fun_type),
                    None,
                    false
                ))
            } else {
                inner_scope.add_var_type(
                    name.to_string(),
                    spanned(
                        TypeInfo::Fun {
                            params: params.clone(), 
                            return_type: Box::new(expected_type.clone()),
                            generic_params: generic_params.clone()
                        },
                        statement.span
                    )
                );
                for (n, p_type) in params {
                    inner_scope.add_var_type(n.1.clone(), p_type.clone());
                }
                let actual_type = get_type(body, &mut inner_scope, path)?;
                if actual_type.inner != expected_type.inner {
                    return Err(spanned(
                        format!("Function return type mismatch: expected {:?}, got {:?}", expected_type.inner, actual_type.inner),
                        expected_type.span
                    ))
                }
                let fun_type = spanned(
                    TypeInfo::TypeClosure { 
                        params: generic_params.clone(),
                        env: inner_scope.clone(),
                        body: Box::new(spanned(
                            TypeInfo::Fun { params: params.clone(), return_type: Box::new(expected_type), generic_params: Vec::new() },
                            statement.span
                        )) 
                    },
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok((
                    Some(name.to_string()),
                    Some(fun_type),
                    None,
                    false
                ))
            }
        },
        Statement::NativeFun { name, params, return_type, generic_params } => {
            if let None = get_native_fun(path, name) {
                return Err(Spanned {
                    inner: format!("Cannot find native function definition for {}", name),
                    span: statement.span
                })
            }
            let mut inner_scope = env.enter_scope();
            let r_type = return_type.clone().unwrap_or(spanned(
                TypeInfo::Void,
                Span::from(0..0)
            ));
            if generic_params.len() == 0 {
                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    flat_params.push((n.clone(), flatten_type(p_type, &mut inner_scope, path)?.into_owned()))
                }
                let fun_type = spanned(
                    TypeInfo::Fun {
                        params: flat_params, 
                        return_type: Box::new(r_type),
                        generic_params: Vec::new()
                    },
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok((
                    Some(name.to_string()),
                    Some(fun_type),
                    None,
                    false
                ))
            } else {
                let fun_type = spanned(
                    TypeInfo::TypeClosure { 
                        params: generic_params.clone(),
                        env: inner_scope.clone(),
                        body: Box::new(spanned(
                            TypeInfo::Fun { params: params.clone(), return_type: Box::new(r_type), generic_params: Vec::new() },
                            statement.span
                        )) 
                    },
                    statement.span
                );
                env.add_var_type(name.clone(), fun_type.clone());
                Ok((
                    Some(name.to_string()),
                    Some(fun_type),
                    None,
                    false
                ))
            }
        },
        Statement::EnumDef { name, variants, generic_params } => todo!(),
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

//returrs the type of an expression while also checking for type errors
fn get_type(expr: &Spanned<Expr>, env: &mut TypeEnv, path: &str) -> Result<Spanned<TypeInfo>, Spanned<String>> {
    match &expr.inner {
        Expr::Bool(_) => Ok(spanned(TypeInfo::Bool, expr.span)),
        Expr::Float(_) => Ok(spanned(TypeInfo::Float, expr.span)),
        Expr::Int(_) => Ok(spanned(TypeInfo::Int, expr.span)),
        Expr::String(_) => Ok(spanned(TypeInfo::String, expr.span)),
        Expr::Void => Ok(spanned(TypeInfo::Void, expr.span)),
        Expr::Var(name) => {
            Ok(env.get_var_type(name).ok_or(spanned(format!("Cannot resolve variable {name}"), expr.span))?)
        },
        Expr::Array(spanneds) => {
            if spanneds.len() == 0 {
                return Ok(spanned(
                    TypeInfo::Array(Box::new(spanned(TypeInfo::Any, Span::from(0..0)))),
                    expr.span
                ))
            }
            let first_type = get_type(spanneds.first().unwrap(), env, path)?;
            for ti in spanneds {
                let element_type = get_type(ti, env, path)?.inner;
                if element_type != first_type.inner {
                    return Err(spanned(
                        format!("Array Element Type Mismatch: Expected {:?}, got {:?}", element_type, first_type.inner),
                        ti.span
                    ))
                }
            }
            Ok(spanned(TypeInfo::Array(Box::new(first_type)), expr.span))
        },
        Expr::Index(target, index) => {
            let index_type = get_type(index, env, path)?;
            if index_type.inner != TypeInfo::Int {
                return Err(spanned(
                    format!("Cannot use {:?} to index arrays", index_type.inner),
                    index.span
                ))
            }
            let target_type = get_type(target, env, path)?;
            match target_type.inner {
                TypeInfo::Array(element_type) => Ok(*element_type),
                _ => Err(spanned(
                    format!("Cannot index {:?}", target_type.inner),
                    target.span
                ))
            }
        },
        Expr::Binary { left, operation, right } => {
            let left_type = get_type(left, env, path)?;
            let right_type = get_type(right, env, path)?;
            match operation {
                Operation::Add
                | Operation::Subtract 
                | Operation::Multiply
                | Operation::Divide
                | Operation::Modulo  => {
                    match &left_type.inner {
                        TypeInfo::Any 
                        | TypeInfo::Float 
                        | TypeInfo::Int => {
                            if left_type.inner != right_type.inner {
                                return Err(spanned(
                                    format!("Type Mismatch: Expeccted {:?}, got {:?}", left_type.inner, right_type.inner), 
                                    left.span
                                ))
                            }
                            Ok(spanned(
                                left_type.inner,
                                Span::from(left_type.span.start..right_type.span.end)
                            ))
                        },
                        _ => return Err(spanned(
                            format!("Cannot apply {:?} to {:?}", operation, left_type), 
                            left.span
                        ))
                    }
                },
                Operation::LessThan 
                | Operation::LessThanEq 
                | Operation::GreaterThan 
                | Operation::GreaterThanEq => {
                    match &left_type.inner {
                        TypeInfo::Any 
                        | TypeInfo::Float 
                        | TypeInfo::Int => {
                            if left_type.inner != right_type.inner {
                                return Err(spanned(
                                    format!("Type Mismatch: Expeccted {:?}, got {:?}", left_type.inner, right_type.inner), 
                                    left.span
                                ))
                            }
                            Ok(spanned(
                                TypeInfo::Bool,
                                Span::from(left_type.span.start..right_type.span.end)
                            ))
                        },
                        _ => return Err(spanned(
                            format!("Cannot apply {:?} to {:?}", operation, left_type), 
                            left.span
                        ))
                    }
                },
                Operation::Eq | Operation::NotEq => {
                    if left_type.inner != right_type.inner {
                        return Err(spanned(
                            format!("Cannot compare {:?} to {:?}", left_type.inner, right_type.inner),
                            Span::from(left_type.span.start..right_type.span.end)
                        ))
                    }
                    Ok(spanned(
                        TypeInfo::Bool, 
                        Span::from(left_type.span.start..right_type.span.end)
                    ))
                },
                Operation::And | Operation::Or => {
                    match (left_type.inner, right_type.inner) {
                        (TypeInfo::Bool, TypeInfo::Bool) => Ok(spanned(
                            TypeInfo::Bool,
                            Span::from(left_type.span.start..right_type.span.end)
                        )),
                        (a, b) => Err(spanned(
                            format!("{:?} requires both sides to be Bool, got: {:?}, {:?}", operation, a, b),
                            Span::from(left_type.span.start..right_type.span.end)
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
            let mut r_type = TypeInfo::Void;
            let _ = hoist(statements, &mut inner_scope, path)?;
             
            if let Some(e) = final_expr {
                r_type = get_type(e, &mut inner_scope, path)?.inner;
                r_span = e.span;
            }
            Ok(spanned(
                r_type,
                r_span
            ))
        },
        Expr::Fun { params, body, return_type, generic_params } => {
            let mut inner_scope = env.enter_scope();
            let expected_type = return_type.clone().unwrap_or(spanned(TypeInfo::Void, Span::from(0..0)));
            
            if generic_params.len() == 0 {
                let mut flat_params = Vec::new();
                for (n, p_type) in params {
                    let flattened = flatten_type(p_type, &mut inner_scope, path)?.into_owned();
                    inner_scope.add_var_type(n.1.clone(), flattened.clone());
                    flat_params.push((n.clone(), flattened))
                }
                let non_flat = get_type(body, &mut inner_scope, path)?;
                let actual_type = flatten_type(&non_flat, env, path)?;
                if actual_type.inner != expected_type.inner {
                    return Err(spanned(
                        format!("Function return type mismatch: expected {:?}, got {:?}", expected_type.inner, actual_type.inner),
                        expected_type.span
                    ))
                }
                Ok(spanned(
                    TypeInfo::Fun { params: flat_params, return_type: Box::new(expected_type), generic_params: Vec::new() },
                    expr.span
                ))
            } else {
                for (n, p_type) in params {
                    inner_scope.add_var_type(n.1.clone(), p_type.clone());
                }
                let actual_type = get_type(body, &mut inner_scope, path)?;
                if actual_type.inner != expected_type.inner {
                    return Err(spanned(
                        format!("Function return type mismatch: expected {:?}, got {:?}", expected_type.inner, actual_type.inner),
                        expected_type.span
                    ))
                }
                Ok(spanned(
                    TypeInfo::TypeClosure { 
                        params: generic_params.clone(),
                        env: inner_scope.clone(),
                        body: Box::new(spanned(
                            TypeInfo::Fun { params: params.clone(), return_type: Box::new(expected_type), generic_params: Vec::new() },
                            expr.span
                        )) 
                    },
                    expr.span
                ))
            }
        },
        Expr::Call { fun, args, generic_args } => {
            let mut inner_scope = env.enter_scope();
            // treat fun + generic_args as a custom type with generic args provided.
            // this way we can hand off all the generic manipulations to flatten_type fn
            let target_type = spanned(
                TypeInfo::Custom { name: "+temp".into(), generic_args: generic_args.clone() },
                expr.span
            );
            let fun_type = get_type(fun, &mut inner_scope, path)?;
            inner_scope.add_custom_type("+temp".to_string(), fun_type);
            
            match &flatten_type(&target_type, &mut inner_scope, path)?.inner {
                TypeInfo::Fun { params, return_type, generic_params } => {
                    if generic_params.len() != 0 {
                        return Err(spanned(
                            format!(
                                "Insufficient generic arguments provided: expected {}, got {}",
                                generic_params.len(),
                                generic_args.len()
                            ),
                            expr.span
                        ))
                    }
                    let is_variadic = if let Some (last) = params.last() {
                        last.0.0
                    } else {false};
                    if is_variadic {
                        if params.len() > args.len() {
                            return Err(spanned(
                                format!("Insufficient amount of arguments: expected at least {}, got {}", params.len(), args.len()),
                                expr.span
                            ))
                        }
                        for (i, (((v, _), p_type), arg_expr)) in params.iter().zip(args.iter()).enumerate() {
                            match v {
                                true => {
                                    if i != params.len() - 1 {
                                        return Err(spanned(
                                            format!("Cannot have multiple spread params in a function"),
                                            p_type.span
                                        ))
                                    } else {break}
                                },
                                false => (),
                            }
                            let arg_type = get_type(arg_expr, &mut inner_scope, path)?;
                            if arg_type.inner != p_type.inner {
                                return Err(spanned(
                                    format!("Argument type mismatch: expected {:?}, got {:?}", p_type.inner, arg_type.inner),
                                    arg_expr.span
                                ))
                            }
                        }
                        let spread_args = args[params.len()-1..].into_iter();
                        let temp = params.last().unwrap().1.clone();
                        let spread_type = match flatten_type(&temp, &mut inner_scope, path)?.into_owned().inner {
                            TypeInfo::Array(ti) => ti,
                            o => return Err(spanned(
                                format!("Spread parameter should be an array: found {:?} instead", o),
                                temp.span
                            ))
                        };
                        for arg in spread_args {
                            let arg_type = get_type(arg, &mut inner_scope, path)?;
                            if arg_type.inner != spread_type.inner {
                                return Err(spanned(
                                    format!("Argument type mismatch: expected {:?}, got {:?}", spread_type.inner, arg_type.inner),
                                    arg.span
                                ))
                            }
                        }
                    } else {
                        if params.len() != args.len() {
                            return Err(spanned(
                                format!("Argument type mismatch: expected {} args, got {}", params.len(), args.len()),
                                expr.span
                            ))
                        }
                        for ((_, p_type), arg_expr) in params.iter().zip(args.iter()) {
                            let arg_type = get_type(arg_expr, &mut inner_scope, path)?;
                            if arg_type.inner != p_type.inner {
                                return Err(spanned(
                                    format!("Argument type mismatch: expected {:?}, got {:?}", p_type.inner, arg_type.inner),
                                    arg_expr.span
                                ))
                            }
                        }
                    }
                    
                    Ok(spanned(return_type.inner.clone(), expr.span))
                },
                ti => Err(spanned(
                    format!("Type {:?} is not callable", ti),
                    expr.span
                ))
            }
        },
        Expr::Record(items) => {
            let mut record_entries = Vec::new();
            for (name, e) in items {
                record_entries.push((name.clone(), get_type(e, env, path)?));
            }
            Ok(spanned(
                TypeInfo::Record(record_entries),
                expr.span
            ))
        },
        Expr::Get(target, field) => {
            let ti = get_type(target, env, path)?;
            let target_type = flatten_type(&ti, env, path)?;
            match &target_type.inner {
                TypeInfo::Record(fields) => {
                    for (name, e) in fields {
                        if name == field {
                            return Ok(e.clone())
                        }
                    }
                    Err(spanned(
                        format!("Type {:?} does not have property {field}", target_type.inner),
                        target.span
                    ))
                },
                _ => Err(spanned(
                    format!("Type {:?} does not have property {field}", target_type.inner),
                    target.span
                ))
            }
        },
        Expr::Assign { target, value } => {
            let target_type = get_type(target, env, path)?.inner;
            let value_type = get_type(value, env, path)?.inner;
            if target_type != value_type {
                return Err(spanned(
                    format!("Cannot assign {:?} to {:?}", value_type, target_type),
                    value.span
                ))
            }
            Ok(spanned(
                TypeInfo::Void,
                expr.span
            ))
        },
        Expr::Unary(unary_op, e) => {
            let expr_type = get_type(e, env, path)?;
            match (unary_op, &expr_type.inner) {
                (UnaryOp::Negate, TypeInfo::Any 
                | TypeInfo::Float 
                | TypeInfo::Int) => Ok(expr_type),
                (UnaryOp::Not, TypeInfo::Bool) => Ok(expr_type),
                _ => Err(spanned(
                    format!("Cannot apply {:?} to {:?}", unary_op, expr_type.inner),
                    e.span
                ))
            }
        },
        Expr::If { condition, body, else_block } => {
            if get_type(condition, env, path)?.inner != TypeInfo::Bool {
                return Err(spanned(
                    format!("Loop condition should return Bool"),
                    condition.span
                ))
            }
            let body_type = get_type(body, env, path)?;
            if let Some(else_expr) = else_block {
                let else_type = get_type(else_expr, env, path)?;
                if else_type.inner != body_type.inner {
                    return Err(spanned(
                        format!(
                            "If condition branches have incompatible types: expected {:?}, got {:?}",
                            body_type.inner,
                            else_type.inner
                        ),
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
            let _ = get_type(body, env, path)?;
            if get_type(condition, env, path)?.inner != TypeInfo::Bool {
                return Err(spanned(
                    format!("Loop condition should return Bool"),
                    condition.span
                ))
            }
            Ok(spanned(
                TypeInfo::Void,
                expr.span
            ))
        },
        Expr::Break | Expr::Continue => Ok(spanned(TypeInfo::Void, expr.span)),
        Expr::Return(e) => Ok(get_type(e, env, path)?),
    }
}

//replaces type aliases with their underlying type, processes type closures
fn flatten_type<'a>(type_info: &'a Spanned<TypeInfo>, env: &mut TypeEnv, path: &str) -> Result<Cow<'a, Spanned<TypeInfo>>, Spanned<String>> {
    match &type_info.inner {
        TypeInfo::Custom { name, generic_args } => {
            let resolved_type = if let Some(ti) = env.resolve_type(name.as_str()) {
                ti
            } else {
                return Err(spanned(
                    format!("Cannot resolve type {name}"),
                    type_info.span
                ));
            };
            
            match &resolved_type.inner {
                //useless but idc
                TypeInfo::Fun { params, return_type, generic_params } => {
                    if generic_params.len() != generic_args.len() {
                        return Err(spanned(
                            format!("A wrong number of generic args provided: expected: {}, got {}", generic_params.len(), generic_args.len()),
                            resolved_type.span
                        ))
                    }
                    let mut new_scope = env.enter_scope();
                    for (param, arg) in generic_params.iter().zip(generic_args.iter()) {
                        new_scope.add_custom_type(param.inner.clone(), arg.clone());
                    }
                    let mut new_params = Vec::new();
                    for (n, ti) in params {
                        new_params.push((n.clone(), flatten_type(ti, &mut new_scope, path)?.into_owned()));
                    }
                    Ok(Cow::Owned(spanned(
                        TypeInfo::Fun { 
                            params: new_params, 
                            return_type: Box::new(flatten_type(return_type, &mut new_scope, path)?.into_owned()),
                            generic_params: Vec::new()
                        },
                        type_info.span
                    )))
                },
                TypeInfo::Enum { name, variants, generic_params } => {
                    if generic_params.len() != generic_args.len() {
                        return Err(spanned(
                            format!("A wrong number of generic args provided: expected: {}, got {}", generic_params.len(), generic_args.len()),
                            resolved_type.span
                        ))
                    }
                    Ok(Cow::Owned(spanned(
                        TypeInfo::EnumInstance { enum_name: name.clone(), variants: variants.clone(), generic_args: generic_args.clone() },
                        type_info.span
                    )))
                },
                TypeInfo::TypeClosure { params, env: closure_scope, body } => {
                    if params.len() != generic_args.len() {
                        return Err(spanned(
                            format!("A wrong number of generic args provided: expected: {}, got {}", params.len(), generic_args.len()),
                            resolved_type.span
                        ))
                    }
                    let mut inner_scope = closure_scope.clone();
                    for (param, arg) in params.iter().zip(generic_args.iter()) {
                        let flat_arg = flatten_type(arg, &mut inner_scope, path)?.into_owned();
                        inner_scope.add_custom_type(param.inner.clone(), flat_arg);
                    }
                    Ok(Cow::Owned(flatten_type(body, &mut inner_scope, path)?.into_owned()))
                },
                _ => {
                    if generic_args.len() != 0 {
                        return Err(spanned(
                            format!("Too many generic arguments provided: expected: 0, got {}", generic_args.len()),
                            type_info.span
                        ))
                    }
                    Ok(Cow::Owned(flatten_type(&resolved_type, env, path)?.into_owned()))
                }
            }
        },
        TypeInfo::Fun { params, return_type, generic_params } => {
            if generic_params.len() > 0 {panic!()}
            let mut new_params = Vec::new();
            for (a, ti) in params {
                new_params.push((a.clone(), flatten_type(ti, env, path)?.into_owned()))
            }
            Ok(Cow::Owned(spanned(
                TypeInfo::Fun { 
                    params: new_params, 
                    return_type: Box::new(flatten_type(return_type, env, path)?.into_owned()),
                    generic_params: Vec::new()
                },
                type_info.span
            )))
        },
        TypeInfo::Record(entries) => {
            let mut new_entries = Vec::new();
            for (name, ti) in entries {
                new_entries.push((name.clone(), flatten_type(ti, env, path)?.into_owned()));
            }
            Ok(Cow::Owned(spanned(
                TypeInfo::Record(new_entries),
                type_info.span
            )))
        },
        TypeInfo::Array(ti) => Ok(Cow::Owned(spanned(
            TypeInfo::Array(Box::new(flatten_type(ti, env, path)?.into_owned())),
            type_info.span
        ))),
        _ => Ok(Cow::Borrowed(type_info))
    }
}

// blindly converts statements to lower version without types. all the type checking should be done prior to that
pub fn lower_statement(statement: &Spanned<Statement>, env: &mut TypeEnv) -> Result<Option<Spanned<ir::Statement>>, Spanned<String>> {
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
                ir::Statement::NativeFun(name.to_string()),
                statement.span
            )))
        },
        Statement::EnumDef { name, variants, generic_params: _ } => todo!(),
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

fn lower_expr(expr: &Spanned<Expr>, env: &mut TypeEnv) -> Result<Spanned<ir::Expr>, Spanned<String>> {
    match &expr.inner {
        Expr::Bool(b) => Ok(spanned(ir::Expr::Bool(*b), expr.span)),
        Expr::Float(f) => Ok(spanned(ir::Expr::Float(*f), expr.span)),
        Expr::Int(i) => Ok(spanned(ir::Expr::Int(*i), expr.span)),
        Expr::String(s) => Ok(spanned(ir::Expr::String(s.clone()), expr.span)),
        Expr::Void => Ok(spanned(ir::Expr::Void, expr.span)),
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
    }
}