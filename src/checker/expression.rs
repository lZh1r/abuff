use std::collections::HashMap;

use smol_str::{SmolStr, format_smolstr};

use crate::{ast::{clean, shared::{Operation, UnaryOp}, typed::{Expr, TypeInfo, TypeKind}}, checker::{flatten::flatten_type, mutability::check_mutability}, env::TypeEnv, pattern_matching::match_expr, span::{Span, Spanned, spanned}, type_checker::hoist};

pub fn substitute_generic_params(
    type_info: &Spanned<TypeInfo>,
    generic_arg_map: &HashMap<SmolStr, Spanned<TypeInfo>>
) -> Spanned<TypeInfo> {
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
                TypeInfo::new_with_id(TypeKind::Fun {
                    params: new_params,
                    return_type: new_return_type,
                    generic_params: generic_params.clone()
                }, type_info.inner.id()),
                type_info.span
            )
        },
        TypeKind::Record(entries) => {
            let new_entries = entries.iter().map(|(name, ti)| {
                (name.clone(), substitute_generic_params(ti, generic_arg_map))
            }).collect();
            spanned(TypeInfo::new_with_id(TypeKind::Record(new_entries), type_info.inner.id()), type_info.span)
        },
        TypeKind::Array(ti) => {
            spanned(
                TypeInfo::array(
                    substitute_generic_params(ti, generic_arg_map)
                ),
                type_info.span
            )
        },
        TypeKind::EnumInstance { enum_name, variants, generic_args } => {
            let new_args = generic_args.iter()
                .map(|arg| substitute_generic_params(arg, generic_arg_map))
                .collect();
            spanned(
                TypeInfo::new_with_id(TypeKind::EnumInstance {
                    enum_name: enum_name.clone(),
                    variants: variants.clone(),
                    generic_args: new_args,
                }, type_info.inner.id()),
                type_info.span
            )
        },
        TypeKind::EnumVariant { enum_name, variant, generic_args } => {
            let new_args = generic_args.iter()
                .map(|arg| substitute_generic_params(&spanned(arg.inner.clone(), type_info.span), generic_arg_map))
                .collect();
            spanned(
                TypeInfo::new_with_id(TypeKind::EnumVariant {
                    enum_name: enum_name.clone(),
                    variant: variant.clone(),
                    generic_args: new_args,
                }, type_info.inner.id()),
                type_info.span
            )
        },
        _ => type_info.clone()
    }
}

fn collect_generic_params(
    generic_type: (&Spanned<TypeInfo>, bool), //bool is for handling variadics
    concrete_type: &Spanned<TypeInfo>,
) -> HashMap<SmolStr, Spanned<TypeInfo>> {
    fn walk(
        gener: (&Spanned<TypeInfo>, bool),
        con: &Spanned<TypeInfo>,
        out: &mut HashMap<SmolStr, Spanned<TypeInfo>>,
    ) {
        match (gener.0.inner.kind(), con.inner.kind()) {
            (TypeKind::GenericParam(name), _) => {
                out.insert(name.clone(), con.clone());
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

    let mut result = HashMap::new();
    walk(generic_type, concrete_type, &mut result);
    result
}

pub fn lower_expr(expr: &Spanned<Expr>, env: &mut TypeEnv) -> Result<
    (Spanned<clean::Expr>, Spanned<TypeInfo>),
    Spanned<SmolStr>
> {
    match &expr.inner {
        Expr::Bool(b) => Ok((
            spanned(clean::Expr::Bool(*b), expr.span),
            spanned(TypeInfo::bool(), expr.span)
        )),
        Expr::Float(f) => Ok((
            spanned(clean::Expr::Float(*f), expr.span),
            spanned(TypeInfo::float(), expr.span)
        )),
        Expr::Int(i) => Ok((
            spanned(clean::Expr::Int(*i), expr.span),
            spanned(TypeInfo::int(), expr.span)
        )),
        Expr::String(s) => Ok((
            spanned(clean::Expr::String(s.clone()), expr.span),
            spanned(TypeInfo::string(), expr.span)
        )),
        Expr::Char(c) => Ok((
            spanned(clean::Expr::Char(c.clone()), expr.span),
            spanned(TypeInfo::char(), expr.span)
        )),
        Expr::Void => Ok((
            spanned(clean::Expr::Void, expr.span),
            spanned(TypeInfo::void(), expr.span)
        )),
        Expr::Null => Ok((
            spanned(clean::Expr::Null, expr.span),
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
                spanned(clean::Expr::Var(name.clone()), expr.span),
                var_type.0
            ))
        },
        Expr::Array(spanneds) => {
            let mut lowered = Vec::new();
            let mut expected_type = spanned(TypeInfo::void(), Span::at(0));
            for e in spanneds {
                let result = lower_expr(e, env)?;
                if result.1.inner != expected_type.inner {
                    if expected_type.inner == TypeInfo::void() {
                        expected_type = result.1
                    } else {
                        return Err(spanned(
                            format_smolstr!(
                                "Array Element Type Mismatch: Expected {}, got {}",
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
                spanned(clean::Expr::Array(lowered), expr.span.clone()),
                spanned(
                    TypeInfo::array(expected_type),
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
                            format_smolstr!("Cannot use {} to index arrays", index_result.1.inner),
                            index.span
                        ))
                    }
                    Ok((
                        spanned(
                            clean::Expr::Index(
                                Box::new(target_result.0),
                                Box::new(index_result.0)
                            ),
                            expr.span
                        ),
                        *element_type.clone()
                    ))
                },
                TypeKind::Tuple(elements) => {
                    match index_result.0.inner {
                        clean::Expr::Int(i) => {
                            if elements.get(i as usize).is_none() {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Index {i} is out of bounds for a tuple of length {}",
                                        elements.len()
                                    ),
                                    index.span
                                ))
                            }
                            Ok((
                                spanned(
                                    clean::Expr::Index(
                                        Box::new(target_result.0),
                                        Box::new(index_result.0)
                                    ),
                                    expr.span
                                ),
                                *elements[i as usize].clone()
                            ))
                        },
                        _ => Err(spanned(
                            format_smolstr!("Cannot use {} to index tuples", index_result.1.inner),
                            index.span
                        ))
                    }
                }
                _ => Err(spanned(
                    format_smolstr!("Cannot index {}", target_result.1.inner),
                    target.span
                ))
            }
        },
        Expr::Binary { left, operation, right } => {
            let left_result = lower_expr(left, env)?;
            let right_result = lower_expr(right, env)?;
            match operation {
                | Operation::Subtract 
                | Operation::Multiply
                | Operation::Divide
                | Operation::Modulo  => {
                    match left_result.1.inner.kind() {
                        TypeKind::Any 
                        | TypeKind::Float 
                        | TypeKind::Int => {
                            if left_result.1.inner != right_result.1.inner 
                            && !match right_result.1.inner.kind() {
                                TypeKind::Array(_) => true,
                                _ => false
                            } {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Type Mismatch: Expected {}, got {}",
                                        left_result.1.inner, 
                                        right_result.1.inner
                                    ), 
                                    left.span
                                ))
                            }
                            Ok((
                                spanned(
                                    clean::Expr::Binary { 
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
                        TypeKind::Array(_) => {
                            if right_result.1.inner.kind() != &TypeKind::Int {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Cannot apply {:?} to {}", 
                                        operation, 
                                        left_result.1.inner
                                    ), 
                                    left.span
                                ))
                            }
                            Ok((
                                spanned(
                                    clean::Expr::Binary { 
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
                            format!("Cannot apply {:?} to {}", operation, left_result.1.inner).into(), 
                            left.span
                        ))
                    }
                },
                Operation::Add => {
                    match left_result.1.inner.kind() {
                        TypeKind::Any 
                        | TypeKind::Float 
                        | TypeKind::Int
                        | TypeKind::String => {
                            if left_result.1.inner != right_result.1.inner {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Type Mismatch: Expeccted {}, got {}",
                                        left_result.1.inner, 
                                        right_result.1.inner
                                    ), 
                                    left.span
                                ))
                            }
                            Ok((
                                spanned(
                                    clean::Expr::Binary { 
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
                            format!("Cannot apply {:?} to {}", operation, left_result.1.inner).into(), 
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
                                        "Type Mismatch: Expeccted {}, got {}",
                                        left_result.1.inner, 
                                        right_result.1.inner
                                    ), 
                                    right.span
                                ))
                            }
                            Ok((
                                spanned(
                                    clean::Expr::Binary { 
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
                            format_smolstr!("Cannot apply {:?} to {}", operation, left_result.1.inner), 
                            left.span
                        ))
                    }
                },
                Operation::Eq | Operation::NotEq => {
                    if left_result.1.inner != right_result.1.inner {
                        return Err(spanned(
                            format_smolstr!(
                                "Cannot compare {} to {}", 
                                left_result.1.inner, 
                                right_result.1.inner
                            ),
                            expr.span
                        ))
                    }
                    Ok((
                        spanned(
                            clean::Expr::Binary { 
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
                                clean::Expr::Binary { 
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
                                "{:?} requires both sides to be Bool, got: {}, {}", 
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
                                    clean::Expr::Binary { 
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
                                clean::Expr::Binary { 
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
                                        "Type Mismatch: Expected {}, got {}",
                                        left_result.1.inner,
                                        right_result.1.inner
                                    ), 
                                    expr.span
                                ))
                            }
                            Ok((
                                spanned(
                                    clean::Expr::Binary { 
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
                                "Cannot apply {:?} to {}", 
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
                        clean::Expr::Block(lowered_statements, Some(Box::new(final_result.0))),
                        expr.span.clone()
                    ),
                    final_result.1
                ))
            } else {
                Ok((
                    spanned(
                        clean::Expr::Block(lowered_statements, None),
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
                    inner_scope.add_var_type(n.1.clone(), flattened.clone(), false);
                    flat_params.push((n.clone(), flattened));
                    lower_params.push(n.clone());
                }
                let expected_type = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                inner_scope.add_var_type(SmolStr::new("+return"), expected_type.clone(), false);
                let body_result = lower_expr(body, &mut inner_scope)?;
                let actual_type = flatten_type(&body_result.1, env)?;
                if actual_type.inner != expected_type.inner {
                    return Err(spanned(
                        format_smolstr!(
                            "Function return type mismatch: expected {}, got {}", 
                            expected_type.inner, 
                            actual_type.inner
                        ),
                        expected_type.span
                    ))
                }
                Ok((
                    spanned(
                        clean::Expr::Fun {
                            params: lower_params,
                            body: Box::new(body_result.0)
                        },
                        expr.span.clone()
                    ),
                    spanned(
                        TypeInfo::new(
                            TypeKind::Fun {
                                params: flat_params, 
                                return_type: Box::new(actual_type.into_owned()), 
                                generic_params: Vec::new()
                            }
                        ),
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
                    inner_scope.add_var_type(n.1.clone(), flattened.clone(), false);
                    flat_params.push((n.clone(), flattened));
                    lower_params.push(n.clone());
                }

                let expected_flat = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                inner_scope.add_var_type(SmolStr::new("+return"), expected_flat.clone(), false);
                let body_result = lower_expr(body, &mut inner_scope)?;
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

                Ok((
                    spanned(
                        clean::Expr::Fun { 
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
                    } else if generic_params.len() == 0 {
                
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
                        for (i, (((v, _), p_type), arg_type))
                        in substituted_params.iter().zip(arg_types.iter()).enumerate() {
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
                                        "Argument type mismatch: expected {}, got {}",
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
                                format_smolstr!("Spread parameter should be an array: found {} instead", o),
                                temp.span
                            ))
                        };
                        for (arg, arg_type) 
                        in spread_args.iter().zip(arg_types.iter().skip(substituted_params.len()-1)) {
                            if arg_type.inner != spread_type.inner {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Argument type mismatch: expected {}, got {}",
                                        spread_type.inner, 
                                        arg_type.inner
                                    ),
                                    arg.span
                                ))
                            }
                        }
                    } else {
                        if substituted_params.len() != args.len() {
                            return Err(spanned(
                                format_smolstr!(
                                    "Argument type mismatch: expected {} args, got {}",
                                    substituted_params.len(), args.len()
                                ),
                                expr.span
                            ))
                        }
                        for ((_, p_type), arg_type) in substituted_params.iter().zip(arg_types.iter()) {
                            if arg_type.inner != p_type.inner {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Argument type mismatch: expected {}, got {}",
                                        p_type.inner, 
                                        arg_type.inner
                                    ),
                                    expr.span
                                ))
                            }
                        }
                    }
    
                    Ok((
                        spanned(
                            clean::Expr::Call {
                                fun: Box::new(fun_result.0),
                                args: lowered_args
                            },
                            expr.span.clone()
                        ),
                        substituted_return_type.as_ref().clone()
                    ))
                },
                ti => Err(spanned(
                    format_smolstr!("Type {} is not callable", ti),
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
                spanned(clean::Expr::Record(lowers), expr.span.clone()),
                spanned(TypeInfo::new(TypeKind::Record(types)), expr.span)
            ))
        },
        Expr::Get(target, field) => {
            let mut target_result = lower_expr(target, env)?;
            // HORRIBLE BANDAID
            // ABSOLUTELY DISGUSTING
            // but idk what to do
            match &target.inner {
                Expr::Var(v) => if v == "self" {
                    target_result.1 = flatten_type(&target_result.1, env)?.into_owned();
                },
                _ => ()
            }
            // target_result.1 = flatten_type(&target_result.1, env)?.into_owned();
            if let Some(method_info) = env.get_method(target_result.1.inner.id(), field) {
                let ti = if let Some(template) = method_info.type_template {
                    let generic_map = compare_types(&template, &target_result.1);
                    substitute_generic_params(&method_info.type_info, &generic_map)
                } else {
                    method_info.type_info
                };
                if method_info.is_mutating {
                    if check_mutability(&target_result.0, env)? != method_info.is_mutating {
                        return Err(spanned(
                            "Cannot use a mutating method on an immutable value".into(),
                            method_info.lowered.span
                        ))
                    }
                }
                match method_info.lowered.inner {
                    clean::Expr::NativeFun { name, path, native_fun } => {
                        Ok((
                            spanned(
                                clean::Expr::NativeMethod { 
                                    this: Box::new(target_result.0),
                                    name, 
                                    path,
                                    native_fun
                                },
                                expr.span.clone()
                            ),
                            spanned(
                                ti.inner,
                                expr.span
                            )
                        ))
                    },
                    _ => {
                        Ok((
                            spanned(
                                clean::Expr::Method { 
                                    this: Box::new(target_result.0),
                                    fun: Box::new(method_info.lowered)
                                },
                                expr.span.clone()
                            ),
                            spanned(
                                ti.inner,
                                expr.span
                            )
                        ))
                    }
                }
            } else {
                match target_result.1.inner.kind() {
                    TypeKind::Record(fields) => {
                        if let Some(e) = fields.get(field) {
                            return Ok((
                                spanned(
                                    clean::Expr::Get(Box::new(target_result.0), field.clone()),
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
                                "Type {} does not have property {field}",
                                target_result.1.inner
                            ),
                            target.span
                        ))
                    },
                    _ => Err(spanned(
                        format_smolstr!(
                            "Type {} does not have property {field}",
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
                        "Cannot assign {} to {}", 
                        value_result.1.inner, 
                        target_result.1.inner
                    ),
                    value.span
                ))
            }
    
            let is_mutable = check_mutability(&target_result.0, env)?;
    
            if !is_mutable {
                return Err(spanned(
                    "Cannot assign to an immutable variable".into(),
                    expr.span
                ))
            }
    
            Ok((
                spanned(
                    clean::Expr::Assign {
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
                        clean::Expr::Unary(unary_op.clone(), Box::new(expr_result.0)),
                        expr.span
                    ),
                    expr_result.1
                )),
                (UnaryOp::Not, TypeKind::Bool) => Ok((
                    spanned(
                        clean::Expr::Unary(unary_op.clone(), Box::new(expr_result.0)),
                        expr.span
                    ),
                    expr_result.1
                )),
                _ => Err(spanned(
                    format_smolstr!("Cannot apply {:?} to {}", unary_op, expr_result.1.inner),
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
                            "If condition branches have incompatible types: expected {}, got {}",
                            body_result.1.inner,
                            result.1.inner
                        ).into(),
                        result.1.span
                    ))
                }
                Ok((
                    spanned(
                        clean::Expr::If {
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
                        clean::Expr::If {
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
                    clean::Expr::While {
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
            spanned(clean::Expr::Break, expr.span.clone()),
            spanned(TypeInfo::void(), expr.span)
        )),
        Expr::Continue => Ok((
            spanned(clean::Expr::Continue, expr.span.clone()),
            spanned(TypeInfo::void(), expr.span)
        )),
        Expr::Return(e) => {
            let result = lower_expr(e, env)?;
            Ok((
                spanned(
                    clean::Expr::Return(Box::new(result.0)),
                    expr.span
                ),
                spanned(
                    TypeInfo::new_return(result.1.clone()),
                    result.1.span
                )
            ))
        },
        Expr::Match { target, branches } => {
            match_expr(expr, target, branches, env)
        },
        Expr::Panic(reason) => {
            match reason {
                Some(reason) => {
                    let reason_result = lower_expr(reason, env)?;
                    Ok((
                        spanned(
                            clean::Expr::Panic(Some(Box::new(reason_result.0))), 
                            expr.span
                        ),
                        spanned(
                            TypeInfo::new_return(
                                spanned(TypeInfo::void(), Span::from(0..0))
                            ),
                            expr.span
                        )
                    ))
                },
                None => {
                    Ok((
                        spanned(
                            clean::Expr::Panic(None), 
                            expr.span
                        ),
                        spanned(
                            TypeInfo::new_return(
                                spanned(TypeInfo::void(), Span::from(0..0))
                            ),
                            expr.span
                        )
                    ))
                },
            }
        },
        Expr::StaticMethod { target, method } => {
            let target_type = match env.resolve_type(&target.inner) {
                Some(ti) => ti,
                None => return Err(spanned(
                    format_smolstr!("Cannot resolve type {}", target.inner),
                    target.span
                )),
            };
            let (ti, lowered_expr) = match env.get_static_method(target_type.inner.id(), &method.inner) {
                Some(method_info) => {
                    if method_info.type_template.is_some() {
                        let type_template = method_info.type_template.unwrap();
                        let generic_map = compare_types(&type_template, &target_type);
                        (substitute_generic_params(&method_info.type_info, &generic_map), method_info.lowered)
                    } else {
                        (method_info.type_info, method_info.lowered)
                    }
                },
                None => return Err(spanned(
                    format_smolstr!(
                        "Type {} does not have a static method {}", 
                        target_type.inner, 
                        method.inner
                    ),
                    method.span
                )),
            };
            Ok((lowered_expr, ti))
        },
        Expr::Tuple(exprs) => {
            let mut types = Vec::new();
            let mut lowered = Vec::new();
            
            for e in exprs {
                let result = lower_expr(e, env)?;
                lowered.push(result.0);
                types.push(Box::new(result.1));
            }
            
            Ok((
                spanned(
                    clean::Expr::Tuple(lowered),
                    expr.span
                ),
                spanned(
                    TypeInfo::new(TypeKind::Tuple(types)),
                    expr.span
                )
            ))
        },
    }
}

///Compares a generic template with its instance to collect a hashmap of all generic param substitutions
fn compare_types(
    generic_type: &Spanned<TypeInfo>,
    instantiated: &Spanned<TypeInfo>,
) -> HashMap<SmolStr, Spanned<TypeInfo>> {
    let mut map = HashMap::new();

    // The first argument must be a TypeClosure; otherwise return an empty map.
    let closure_params = if let TypeKind::TypeClosure { params, .. } = generic_type.inner.kind() {
        params.clone()
    } else {
        return map;
    };

    fn walk(
        generic: &Spanned<TypeInfo>,
        instance: &Spanned<TypeInfo>,
        map: &mut HashMap<SmolStr, Spanned<TypeInfo>>,
        closure_params: &Vec<SmolStr>,
    ) {
        match (generic.inner.kind(), instance.inner.kind()) {
            (TypeKind::GenericParam(name), _) => {
                // Only insert a substitution if the generic param is listed in the closure's params.
                if closure_params.contains(name) {
                    map.insert(name.clone(), instance.clone());
                }
            }

            (TypeKind::Array(gen_inner), TypeKind::Array(inst_inner)) => {
                walk(gen_inner, inst_inner, map, closure_params);
            }

            (
                TypeKind::Fun {
                    params: gen_params,
                    return_type: gen_ret,
                    ..
                },
                TypeKind::Fun {
                    params: inst_params,
                    return_type: inst_ret,
                    ..
                },
            ) => {
                for ((_, gen_p_ty), (_, inst_p_ty)) in
                    gen_params.iter().zip(inst_params.iter())
                {
                    walk(gen_p_ty, inst_p_ty, map, closure_params);
                }
                walk(gen_ret, inst_ret, map, closure_params);
            }

            (TypeKind::Record(gen_fields), TypeKind::Record(inst_fields)) => {
                for (field_name, gen_field_ty) in gen_fields.iter() {
                    if let Some(inst_field_ty) = inst_fields.get(field_name) {
                        walk(gen_field_ty, inst_field_ty, map, closure_params);
                    }
                }
            }

            (
                TypeKind::EnumInstance {
                    generic_args: gen_args,
                    ..
                },
                TypeKind::EnumInstance {
                    generic_args: inst_args,
                    ..
                },
            ) => {
                for (gen_arg, inst_arg) in gen_args.iter().zip(inst_args.iter()) {
                    walk(gen_arg, inst_arg, map, closure_params);
                }
            }

            (
                TypeKind::EnumVariant {
                    generic_args: gen_args,
                    ..
                },
                TypeKind::EnumVariant {
                    generic_args: inst_args,
                    ..
                },
            ) => {
                for (gen_arg, inst_arg) in gen_args.iter().zip(inst_args.iter()) {
                    walk(gen_arg, inst_arg, map, closure_params);
                }
            }

            (TypeKind::TypeClosure { body, params: _ }, _) => {
                // Recurse into the closure body.
                walk(body, instance, map, closure_params);
            }

            _ => {}
        }
    }

    // Start walking from the closure body against the instantiated type.
    if let TypeKind::TypeClosure { body, .. } = generic_type.inner.kind() {
        walk(body, instantiated, &mut map, &closure_params.into_iter().map(|spanned| spanned.inner).collect());
    }

    map
}