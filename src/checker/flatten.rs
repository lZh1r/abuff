use std::{borrow::Cow, collections::{HashMap, HashSet}};

use smol_str::{SmolStr, format_smolstr};

use crate::{ast::typed::{FunctionParam, TypeInfo, TypeKind}, checker::function::get_flat_params, env::TypeEnv, span::{Spanned, spanned}};

//replaces type aliases with their underlying type, processes type closures
pub fn flatten_type<'a>(
    type_info: &'a Spanned<TypeInfo>,
    env: &mut TypeEnv
) -> Result<Cow<'a, Spanned<TypeInfo>>, Spanned<SmolStr>> {
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
                            format_smolstr!(
                                "A wrong number of generic args provided: expected: {}, got {}",
                                generic_params.len(),
                                generic_args.len()
                            ),
                            resolved_type.span
                        ))
                    }
                    let mut new_scope = env.enter_scope();
                    for (param, arg) in generic_params.iter().zip(generic_args.iter()) {
                        new_scope.add_custom_type(param.inner.clone(), arg.clone());
                    }
                    let new_params = get_flat_params(params, &mut new_scope, false)?;
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
                            format_smolstr!(
                                "A wrong number of generic args provided: expected: {}, got {}",
                                generic_params.len(),
                                generic_args.len()
                            ),
                            resolved_type.span
                        ))
                    }
                    Ok(Cow::Owned(spanned(
                        TypeInfo::new_with_id(
                            TypeKind::EnumInstance {
                                enum_name: name.clone(),
                                variants: variants.clone(),
                                generic_args: generic_args.clone()
                            },
                            resolved_type.inner.id()
                        ),
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
                            format_smolstr!(
                                "Too many generic arguments provided: expected: 0, got {}",
                                generic_args.len()
                            ),
                            type_info.span
                        ))
                    }
                    Ok(Cow::Owned(spanned(
                        TypeInfo::new_with_id(
                            flatten_type(&resolved_type, env)?.inner.kind().clone(),
                            resolved_type.inner.id()
                        ), 
                        type_info.span
                    )))
                    // Ok(Cow::Owned(resolved_type))
                    // Ok(Cow::Owned(flatten_type(&resolved_type, env)?.into_owned()))
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
                            params.iter().any(|p| contains_generic(&p.type_info, generic_names))
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
                    generic_names: &HashSet<SmolStr>,
                ) -> Spanned<TypeInfo> {
                    match ti.inner.kind() {
                        TypeKind::Custom { generic_args, name }
                            if generic_args.is_empty() && generic_names.contains(name) =>
                        {
                            spanned(
                                TypeInfo::new_with_id(
                                    TypeKind::GenericParam(name.clone()),
                                    ti.inner.id()
                                ),
                                ti.span
                            )
                        }
                        _ => ti,
                    }
                }

                // Flatten parameter types where possible, applying the replacement rule
                let mut new_params = Vec::new();
                for p in params {
                    let flattened = if contains_generic(&p.type_info, &generic_names) {
                        p.type_info.clone()
                    } else {
                        flatten_type(&p.type_info, env)?.into_owned()
                    };
                    let final_ti = replace_custom_with_generic(flattened, &generic_names);
                    new_params.push(
                        FunctionParam {
                            type_info: final_ti,
                            ..p.clone()
                        }
                    );
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
            let new_params = get_flat_params(params, env, false)?;
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
            TypeInfo::array(flatten_type(ti, env)?.into_owned()),
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