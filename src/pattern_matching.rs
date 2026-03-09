use std::collections::HashMap;

use smol_str::{SmolStr, format_smolstr};

use crate::{ast::{clean::{self}, typed::{Expr, TypeInfo, TypeKind, MatchArm}}, env::TypeEnv, span::{Span, Spanned, spanned}, type_checker::{flatten_type, lower_expr, substitute_generic_params}};

pub fn match_expr(
    expr: &Spanned<Expr>,
    target: &Spanned<Expr>,
    branches: &Vec<(Spanned<MatchArm>, Spanned<Expr>)>,
    env: &mut TypeEnv
) -> Result<(Spanned<clean::Expr>, Spanned<TypeInfo>), Spanned<SmolStr>> {
    let target_result = lower_expr(target, env)?;

    fn match_branch(
        target: &TypeInfo, 
        pattern: &Spanned<MatchArm>,
        branch: &Spanned<Expr>,
        env: &mut TypeEnv
    ) -> Result<
        ((Spanned<clean::MatchArm>, Spanned<clean::Expr>), Spanned<TypeInfo>, bool),
        Spanned<SmolStr>
    > {
        match &pattern.inner {
            MatchArm::Conditional { alias, condition } => {
                let mut inner_env = env.enter_scope();
                inner_env.add_var_type(
                    alias.clone(),
                    spanned(
                        target.clone(),
                        Span::from(0..0)
                    ),
                    false
                );
                let cond_result = lower_expr(condition, &mut inner_env)?;
                match cond_result.1.inner.kind() {
                    TypeKind::Bool => (),
                    _ => return Err(spanned(
                        format_smolstr!(
                            "Match guard condition should return a Bool, found {} instead",
                            cond_result.1.inner
                        ),
                        condition.span
                    ))
                }
                let branch_result = lower_expr(branch, &mut inner_env)?;
                let lowered_pattern = spanned(
                    clean::MatchArm::Conditional {
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
                        format_smolstr!(
                            "Type mismatch in match branch: expected {}, got {}", 
                            target, 
                            pattern_result.1.inner
                        ),
                        e.span
                    ))
                }
                let branch_result = lower_expr(branch, env)?;
                let lowered_pattern = spanned(
                    clean::MatchArm::Value(pattern_result.0),
                    pattern.span
                );
                Ok(((lowered_pattern, branch_result.0), branch_result.1, false))
            },
            MatchArm::Default(alias) => {
                let mut inner_env = env.enter_scope();
                inner_env.add_var_type(
                    alias.clone(),
                    spanned(
                        target.clone(),
                        Span::from(0..0)
                    ),
                    false
                );
                let branch_result = lower_expr(branch, &mut inner_env)?;
                let lowered_pattern = spanned(
                    clean::MatchArm::Default(alias.clone()),
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

                let branch_result = lower_expr(branch, &mut inner_env)?;
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

                            let branch_result = lower_expr(branch, &mut inner_scope)?;
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
        if expected_type.kind() == &TypeKind::Never {
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