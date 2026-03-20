use crate::{ast::typed::LetPattern, checker::flatten::flatten_type};
use std::collections::HashMap;

use smol_str::{SmolStr, format_smolstr};

use crate::{ast::{clean::{self}, typed::{Expr, TypeInfo, TypeKind, MatchArm}}, env::TypeEnv, span::{Span, Spanned, spanned}, checker::expression::{process_expression, substitute_generic_params}};

pub fn match_expr(
    expr: &Spanned<Expr>,
    target: &Spanned<Expr>,
    branches: &Vec<(Spanned<MatchArm>, Spanned<Expr>)>,
    env: &mut TypeEnv
) -> Result<(Spanned<clean::Expr>, Spanned<TypeInfo>), Spanned<SmolStr>> {
    let target_result = process_expression(target, env)?;

    /// Recursively lower a match pattern while populating `env` with any bound
    /// variables.  The `top_level` flag controls whether a `Default` pattern
    /// should be considered the branch's catch‑all (only the outermost
    /// invocation sees `true`; nested calls always pass `false`).
    fn lower_pattern_inner(
        target: &TypeInfo,
        pattern: &Spanned<MatchArm>,
        env: &mut TypeEnv,
        top_level: bool,
    ) -> Result<(Spanned<clean::MatchArm>, bool), Spanned<SmolStr>> {
        match &pattern.inner {
            MatchArm::Conditional { alias, condition } => {
                // bind alias for the duration of evaluating the guard
                env.add_var_type(
                    alias.clone(),
                    spanned(target.clone(), Span::from(0..0)),
                    false,
                );
                let cond_result = process_expression(condition, env)?;
                match cond_result.1.inner.kind() {
                    TypeKind::Bool => (),
                    _ => {
                        return Err(spanned(
                            format_smolstr!(
                                "Match guard condition should return a Bool, found {} instead",
                                cond_result.1.inner
                            ),
                            condition.span,
                        ))
                    }
                }
                let lowered = spanned(
                    clean::MatchArm::Conditional {
                        alias: alias.clone(),
                        condition: cond_result.0,
                    },
                    pattern.span,
                );
                Ok((lowered, false))
            }
            MatchArm::Value(e) => {
                // simple value pattern – just lower the expression and check the
                // type matches the target.
                let pattern_result = process_expression(e, env)?;
                if &pattern_result.1.inner != target {
                    return Err(spanned(
                        format_smolstr!(
                            "Type mismatch in match branch: expected {}, got {}",
                            target,
                            pattern_result.1.inner
                        ),
                        e.span,
                    ));
                }
                let lowered = spanned(clean::MatchArm::Value(pattern_result.0), pattern.span);
                Ok((lowered, false))
            }
            MatchArm::Default(alias) => {
                // binding pattern; top_level decides whether this counts as the
                // branch default.
                env.add_var_type(alias.clone(), spanned(target.clone(), Span::from(0..0)), false);
                let lowered = spanned(clean::MatchArm::Default(alias.clone()), pattern.span);
                Ok((lowered, top_level))
            }
            MatchArm::Tuple(patterns) => {
                // tuple destructuring – target must be a tuple type
                match target.kind() {
                    TypeKind::Tuple(elements) => {
                        if elements.len() != patterns.len() {
                            return Err(spanned(
                                format_smolstr!(
                                    "Tuple pattern has {} elements but target has {}",
                                    patterns.len(),
                                    elements.len()
                                ),
                                pattern.span,
                            ));
                        }
                        let mut lowered_elems = Vec::new();
                        for (subpat, elem_ty_box) in patterns.iter().zip(elements.iter()) {
                            // `elements` holds boxed spanned TypeInfo; pull out a reference
                            let elem_ty: &TypeInfo = &elem_ty_box.inner;
                            let (lp, _def) = lower_pattern_inner(elem_ty, subpat, env, false)?;
                            lowered_elems.push(lp);
                        }
                        Ok((spanned(clean::MatchArm::Tuple(lowered_elems), pattern.span), false))
                    }
                    _ => Err(spanned(
                        format_smolstr!(
                            "Cannot match tuple pattern against non-tuple type {}",
                            target
                        ),
                        pattern.span,
                    )),
                }
            }
            MatchArm::EnumConstructor {
                enum_name,
                variant,
                alias,
            } => {
                // we expect the target to be an enum-related type; copy logic
                // from `match_enum_branch` but only lower the pattern and bind the
                // alias in `env`.
                // resolve the enum we are matching against
                let effective_name = if let Some(n) = enum_name {
                    n.clone()
                } else {
                    // infer from target type
                    match target.kind() {
                        TypeKind::EnumInstance { enum_name, .. }
                        | TypeKind::EnumVariant { enum_name, .. }
                        | TypeKind::Enum { name: enum_name, .. } => enum_name.clone(),
                        _ => {
                            return Err(spanned(
                                "Cannot have enum variants as patterns for non enum values"
                                    .into(),
                                pattern.span,
                            ))
                        }
                    }
                };

                let ti = env
                    .resolve_type(&effective_name)
                    .ok_or(spanned(
                        format_smolstr!("Cannot resolve enum {effective_name}"),
                        pattern.span,
                    ))?;

                // make sure target is compatible with the enum
                let (resolved_variants, generic_args) = match target.kind() {
                    TypeKind::EnumInstance {
                        enum_name: _,
                        variants,
                        generic_args,
                    } => (variants.clone(), generic_args.clone()),
                    TypeKind::EnumVariant {
                        enum_name: _,
                        variant: _,
                        generic_args,
                    } => {
                        // we only know the specific variant, so fall back to the
                        // enum's own definition for the full map
                        let map = match ti.inner.kind() {
                            TypeKind::Enum { variants, .. } => variants.clone(),
                            _ => HashMap::new(),
                        };
                        (map, generic_args.clone())
                    }
                    TypeKind::Enum { variants, .. } => (variants.clone(), Vec::new()),
                    _ => {
                        return Err(spanned(
                            "Cannot have enum variants as patterns for non enum values"
                                .into(),
                            pattern.span,
                        ))
                    }
                };

                // check the variant exists
                let inner_type = if let Some(ti) = resolved_variants.get(variant) {
                    ti
                } else {
                    return Err(spanned(
                        format_smolstr!(
                            "Enum {effective_name} does not have a variant {variant}"
                        ),
                        pattern.span,
                    ));
                };

                // obtain generic parameters from the enum definition
                let generic_params = if let TypeKind::Enum {
                    generic_params, ..
                } = ti.inner.kind()
                {
                    generic_params.clone()
                } else {
                    Vec::new()
                };

                if generic_params.len() != generic_args.len() && generic_args.len() != 0 {
                    return Err(spanned(
                        format_smolstr!(
                            "Incorrect amount of generic arguments provided: expected {}, got {}",
                            generic_params.len(),
                            generic_args.len()
                        ),
                        pattern.span,
                    ));
                }

                // prepare a temporary environment for resolving the inner type
                let mut temp_env = env.enter_scope();
                let mut generic_map = HashMap::new();
                for (g_name, g_type) in generic_params.iter().zip(generic_args.iter()) {
                    generic_map.insert(g_name.inner.clone(), g_type.clone());
                    temp_env.add_custom_type(g_name.inner.clone(), g_type.clone());
                }

                let resolved_type = flatten_type(inner_type, &mut temp_env)?.into_owned();
                let final_type = substitute_generic_params(&resolved_type, &generic_map);
                env.add_var_type(alias.clone(), final_type, false);

                let lowered = spanned(
                    clean::MatchArm::EnumConstructor {
                        enum_name: effective_name.clone(),
                        variant: variant.clone(),
                        alias: alias.clone(),
                    },
                    pattern.span,
                );
                Ok((lowered, false))
            }
        }
    }

    fn match_branch(
        target: &TypeInfo,
        pattern: &Spanned<MatchArm>,
        branch: &Spanned<Expr>,
        env: &mut TypeEnv
    ) -> Result<
        ((Spanned<clean::MatchArm>, Spanned<clean::Expr>), Spanned<TypeInfo>, bool),
        Spanned<SmolStr>
    > {
        let mut inner_env = env.enter_scope();
        let (lowered_pattern, is_default) = lower_pattern_inner(target, pattern, &mut inner_env, true)?;
        let branch_result = process_expression(branch, &mut inner_env)?;
        Ok(((lowered_pattern, branch_result.0), branch_result.1, is_default))
    }

    fn match_enum_branch(
        enum_name: &SmolStr,
        variants: &HashMap<SmolStr, Spanned<TypeInfo>>,
        generic_args: &Vec<Spanned<TypeInfo>>,
        pattern: &Spanned<MatchArm>,
        branch: &Spanned<Expr>,
        env: &mut TypeEnv,
    ) -> Result<
        ((Spanned<clean::MatchArm>, Spanned<clean::Expr>), Spanned<TypeInfo>, bool),
        Spanned<SmolStr>
    > {
        // If the supplied variants map is empty we are matching a single enum variant.
        // Resolve the enum definition to obtain its full variant map in that case.
        let resolved_variants = if variants.is_empty() {
            match env.resolve_type(enum_name) {
                Some(ti) => match ti.inner.kind() {
                    TypeKind::Enum { variants, .. } => variants.clone(),
                    _ => {
                        return Err(spanned(
                            format_smolstr!("Type {enum_name} is not an enum"),
                            ti.span,
                        ))
                    }
                },
                None => {
                    return Err(spanned(
                        format_smolstr!("Cannot resolve enum {enum_name}"),
                        pattern.span,
                    ))
                }
            }
        } else {
            variants.clone()
        };

        match &pattern.inner {
            MatchArm::Default(alias) => {
                let mut inner_env = env.enter_scope();
                let enum_type_id = env.resolve_type(enum_name).map(|t| t.inner.id()).unwrap_or(0);

                // Bind the alias to the whole enum (instance or single variant)
                let kind = if variants.is_empty() {
                    // Single variant – we don't know the exact variant name here, so use a placeholder.
                    TypeKind::EnumVariant {
                        enum_name: enum_name.clone(),
                        variant: SmolStr::new(""),
                        generic_args: generic_args.clone(),
                    }
                } else {
                    TypeKind::EnumInstance {
                        enum_name: enum_name.clone(),
                        variants: resolved_variants.clone(),
                        generic_args: generic_args.clone(),
                    }
                };

                inner_env.add_var_type(
                    alias.clone(),
                    spanned(TypeInfo::new_with_id(kind, enum_type_id), Span::from(0..0)),
                    false
                );

                let branch_result = process_expression(branch, &mut inner_env)?;
                let lowered_pattern = spanned(clean::MatchArm::Default(alias.clone()), pattern.span);
                Ok(((lowered_pattern, branch_result.0), branch_result.1, true))
            }
            MatchArm::EnumConstructor {
                enum_name: c_name,
                variant,
                alias,
            } => {
                let c_name = c_name.as_ref().unwrap_or(enum_name);
                if let Some(ti) = env.resolve_type(c_name) {
                    if c_name != enum_name {
                        return Err(spanned(
                            format_smolstr!("Enum mismatch: expected {enum_name}, got {c_name}"),
                            ti.span,
                        ));
                    }
                    match ti.inner.kind() {
                        TypeKind::Enum {
                            name: _,
                            variants: _,
                            generic_params,
                        } => {
                            if generic_params.len() != generic_args.len() && generic_args.len() != 0{
                                return Err(spanned(
                                    format_smolstr!(
                                        "Incorrect amount of generic arguments provided: expected {}, got {}",
                                        generic_params.len(),
                                        generic_args.len()
                                    ),
                                    pattern.span,
                                ));
                            }

                            // Use the resolved variant map (either the supplied one or the enum's own)
                            let inner_type = if let Some(ti) = resolved_variants.get(variant) {
                                ti
                            } else {
                                return Err(spanned(
                                    format_smolstr!("Enum {enum_name} does not have a variant {variant}"),
                                    pattern.span,
                                ));
                            };

                            let mut inner_scope = env.enter_scope();
                            let mut generic_map = HashMap::new();
                            for (g_name, g_type) in generic_params.iter().zip(generic_args.iter()) {
                                generic_map.insert(g_name.inner.clone(), g_type.clone());
                                inner_scope.add_custom_type(g_name.inner.clone(), g_type.clone());
                            }

                            let resolved_type = flatten_type(inner_type, &mut inner_scope)?.into_owned();
                            inner_scope.add_var_type(
                                alias.clone(),
                                substitute_generic_params(&resolved_type, &generic_map),
                                false
                            );

                            let branch_result = process_expression(branch, &mut inner_scope)?;
                            let lowered_pattern = spanned(
                                clean::MatchArm::EnumConstructor {
                                    enum_name: enum_name.clone(),
                                    variant: variant.clone(),
                                    alias: alias.clone(),
                                },
                                pattern.span,
                            );
                            Ok(((lowered_pattern, branch_result.0), branch_result.1, false))
                        }
                        _ => Err(spanned(
                            format_smolstr!("Type {c_name} is not an enum"),
                            pattern.span,
                        )),
                    }
                } else {
                    Err(spanned(
                        format_smolstr!("Cannot resolve enum {c_name}"),
                        pattern.span,
                    ))
                }
            }
            _ => Err(spanned(
                "This branch is not compatible with enums".into(),
                pattern.span,
            )),
        }
    }

    fn check_branch_type(
        expected_type: &mut TypeInfo,
        branch_type: &Spanned<TypeInfo>,
        branch: &Spanned<Expr>,
        is_default: bool,
        all_branches_handled: &mut bool,
    ) -> Result<(), Spanned<SmolStr>> {
        if expected_type.kind() == &TypeKind::Never && !branch_type.inner.is_return() {
            *expected_type = branch_type.inner.clone();
        } else if expected_type.kind() != branch_type.inner.kind() && !branch_type.inner.is_return() {
            return Err(spanned(
                format_smolstr!(
                    "Match branches cannot have different return types: expected {}, got {}",
                    expected_type,
                    branch_type.inner
                ),
                branch.span
            ))
        }
        if is_default {
            match all_branches_handled {
                true => {
                    return Err(spanned(
                        "Cannot have multiple default branches in a match expression".into(),
                        branch.span
                    ))
                },
                false => *all_branches_handled = true
            }
        }
        Ok(())
    }

    fn process_enum_branch(
        branches: &Vec<(Spanned<MatchArm>, Spanned<Expr>)>,
        expected_type: &mut TypeInfo,
        all_branches_handled: &mut bool,
        lowered_branches: &mut Vec<(Spanned<clean::MatchArm>, Spanned<clean::Expr>)>,
        enum_name: &SmolStr,
        enum_variants: &HashMap<SmolStr, Spanned<TypeInfo>>,
        enum_generic_args: &Vec<Spanned<TypeInfo>>,
        env: &mut TypeEnv
    ) -> Result<(), Spanned<SmolStr>> {
        for (pattern, branch) in branches {
            let ((lowered_pattern, lowered_body), branch_type, is_default) = match_enum_branch(
                enum_name,
                enum_variants,
                enum_generic_args,
                pattern,
                branch,
                env
            )?;
            check_branch_type(
                expected_type,
                &branch_type,
                branch,
                is_default,
                all_branches_handled
            )?;
            lowered_branches.push((lowered_pattern, lowered_body));
        }
        let variant_count = match env.resolve_type(enum_name.as_str()).unwrap().inner.kind() {
            TypeKind::Enum { name: _, variants, generic_params: _ } => {
                variants.len()
            },
            _ => panic!()
        };
        if branches.len() == variant_count {
            *all_branches_handled = true;
        }
        Ok(())
    }

    let mut expected_type = TypeInfo::never();
    let mut all_branches_handled = false;
    let mut lowered_branches = Vec::new();

    match target_result.1.inner.kind() {
        TypeKind::Fun { params: _, return_type: _, generic_params: _ } => return Err(spanned(
            "Cannot apply match to a function".into(),
            target.span
        )),
        TypeKind::EnumInstance { enum_name, variants, generic_args } => {
            process_enum_branch(
                branches,
                &mut expected_type,
                &mut all_branches_handled,
                &mut lowered_branches,
                enum_name,
                variants,
                generic_args,
                env
            )?
        }
        //variant name here does not matter, as you can not construct a value if the variant name is incorrect
        TypeKind::EnumVariant { enum_name, variant: _, generic_args } => {
            process_enum_branch(
                branches,
                &mut expected_type,
                &mut all_branches_handled,
                &mut lowered_branches,
                enum_name,
                &HashMap::new(),
                generic_args,
                env
            )?
        },
        TypeKind::Enum { name, variants, generic_params: _ } => {
            process_enum_branch(
                branches,
                &mut expected_type,
                &mut all_branches_handled,
                &mut lowered_branches,
                name,
                variants,
                &Vec::new(),
                env
            )?
        }
        _ => {
            for (pattern, branch) in branches {
                let ((lowered_pattern, lowered_body), branch_type, is_default) = match_branch(
                    &target_result.1.inner,
                    pattern,
                    branch,
                    env
                )?;
                check_branch_type(
                    &mut expected_type,
                    &branch_type,
                    branch,
                    is_default,
                    &mut all_branches_handled
                )?;
                lowered_branches.push((lowered_pattern, lowered_body));
            }
        }
    }
    if all_branches_handled {
        Ok((
            spanned(
                clean::Expr::Match {
                    target: Box::new(target_result.0),
                    branches: lowered_branches,
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
}

pub fn lower_let_pattern(
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
                    lower_let_pattern(
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