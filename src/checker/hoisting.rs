use std::{any::Any, collections::{HashMap, HashSet}};

use smol_str::{SmolStr, format_smolstr};

use crate::{ast::{clean, typed::{Method, MethodSignature, NativeMethod, NormalMethod, Statement, TypeInfo, TypeKind}}, checker::{expression::process_expression, flatten::flatten_type, statement::process_statement}, env::{MethodInfo, TypeEnv}, module::{GlobalRegistry, insert_type_module}, native::{NativeFun, get_native_fun}, span::{Span, Spanned, spanned}};

#[derive(Debug)]
enum FunctionStatement<'a> {
    Fun(&'a Spanned<Statement>),
    Methods {
        type_info: Spanned<TypeInfo>,
        name: &'a SmolStr,
        generic_params: &'a Vec<Spanned<SmolStr>>,
        interfaces: &'a Vec<Spanned<SmolStr>>,
        implementations: &'a HashMap<SmolStr, Vec<MethodSignature>>
    }
}

pub fn hoist(
    statements: &Vec<Spanned<Statement>>,
    env: &mut TypeEnv,
    path: &str,
) -> Result<
    (
        Vec<Spanned<clean::Statement>>, // ordered statements
        HashMap<SmolStr, Spanned<TypeInfo>>, // var_exports
        HashMap<SmolStr, Spanned<TypeInfo>>, // type_exports
    ),
    Spanned<SmolStr>,
> {
    let mut lowered_statements = Vec::new();
    let is_top_level = env.is_top_level();
    let mut var_exports = HashMap::new();
    let mut type_exports = HashMap::new();

    let mut functions: Vec<FunctionStatement> = Vec::new();
    let mut rest: Vec<&Spanned<Statement>> = Vec::new();

    // First pass: handle imports and separate the rest
    let mut new_statements = Vec::new();
    for st in statements {
        match &st.inner {
            Statement::Import { symbols: _, path: _ } => {
                let result = process_statement(st, env, path)?;
                if let Some(statement) = result.lowered_statement {
                    lowered_statements.push(statement);
                }
            }
            _ => new_statements.push(st),
        }
    }

    // Second pass: classify statements (types, functions, exports, etc.)
    for st in new_statements {
        match &st.inner {
            Statement::TypeDef {
                name,
                type_info: _,
                generic_params,
                implementation,
                interfaces,
            }
            | Statement::EnumDef {
                name,
                variants: _,
                generic_params,
                implementation,
                interfaces,
            } 
            | Statement::NativeType { 
                name, 
                generic_params, 
                implementation, 
                interfaces
            } => {
                let result = process_statement(st, env, path)?;

                // interfaces list does not account for the type's own methods,
                // while implementation map does, so we need to balance their lens
                let actual_len = implementation.len()
                    - if implementation.contains_key("+self".into()) {
                        1
                    } else {
                        0
                    };
                if actual_len > interfaces.len() {
                    return Err(spanned(
                        format_smolstr!(
                            "Type {name} implements too many interfaces: {} expected, {} implemented",
                            interfaces.len(),
                            actual_len
                        ),
                        st.span,
                    ));
                }

                if !implementation.is_empty() {
                    functions.push(FunctionStatement::Methods {
                        type_info: result.custom_type.clone().unwrap(),
                        name,
                        generic_params,
                        interfaces,
                        implementations: implementation,
                    });
                }

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
            Statement::Fun { .. } | Statement::NativeFun { .. } => {
                functions.push(FunctionStatement::Fun(st));
            }
            Statement::Export(statement) => {
                if !is_top_level {
                    return Err(spanned(
                        "Exports should be made at the top level".into(),
                        st.span,
                    ));
                }
                match &statement.inner {
                    Statement::TypeDef {
                        name,
                        type_info: _,
                        generic_params,
                        implementation,
                        interfaces,
                    }
                    | Statement::EnumDef {
                        name,
                        variants: _,
                        generic_params,
                        implementation,
                        interfaces,
                    }
                    | Statement::NativeType { 
                        name, 
                        generic_params, 
                        implementation, 
                        interfaces
                    } => {
                        let result = process_statement(st, env, path)?;

                        // interfaces list does not account for the type's own methods,
                        // while implementation map does, so we need to balance their lens
                        let actual_len = implementation.len()
                            - if implementation.contains_key("+self".into()) {
                                1
                            } else {
                                0
                            };
                        if actual_len > interfaces.len() {
                            return Err(spanned(
                                format_smolstr!(
                                    "Type {name} implements too many interfaces: {} expected, {} implemented",
                                    interfaces.len(),
                                    actual_len
                                ),
                                st.span,
                            ));
                        }

                        if !implementation.is_empty() {
                            functions.push(FunctionStatement::Methods {
                                type_info: result.custom_type.clone().unwrap(),
                                name,
                                generic_params,
                                interfaces,
                                implementations: implementation,
                            });
                        }

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
                    Statement::Fun { .. } | Statement::NativeFun { .. } => {
                        functions.push(FunctionStatement::Fun(st));
                    }
                    _ => rest.push(st),
                }
            }
            _ => rest.push(st),
        }
    }
    
    // Process functions in the order they were collected
    for st in functions {
        handle_fn(st, &mut var_exports, &mut type_exports, &mut lowered_statements, path, env)?
    }

    // Process the remaining statements in order
    for st in &rest {
        let result = process_statement(st, env, path)?;
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

    insert_type_module(
        &GlobalRegistry,
        var_exports.clone(),
        type_exports.clone(), 
        env.clone(), 
        path
    );

    Ok((lowered_statements, var_exports, type_exports))
}

fn handle_fn(
    st: FunctionStatement<'_>,
    var_exports: &mut HashMap<SmolStr, Spanned<TypeInfo>>,
    type_exports: &mut HashMap<SmolStr, Spanned<TypeInfo>>,
    lowered_statements: &mut Vec<Spanned<clean::Statement>>,
    path: &str,
    env: &mut TypeEnv
) -> Result<(), Spanned<SmolStr>> {
    match st {
        FunctionStatement::Fun(st) => {
            let result = process_statement(st, env, path)?;
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
        FunctionStatement::Methods {
            type_info: ti,
            name,
            generic_params,
            interfaces,
            implementations,
        } => {
            // Check that all declared interfaces are implemented
            for iface in interfaces {
                if !implementations.contains_key(&iface.inner) {
                    return Err(spanned(
                        format_smolstr!("Interface {} is not implemented", iface.inner),
                        iface.span,
                    ));
                }
            }

            let mut generic_scope = env.enter_scope();
            for param in generic_params {
                generic_scope.add_custom_type(
                    param.inner.clone(),
                    spanned(
                        TypeInfo::new(TypeKind::GenericParam(param.inner.clone())),
                        param.span,
                    ),
                );
            }

            let mut implemented_methods = HashSet::new();
            for (interface_name, methods) in implementations {
                env.add_interface_impl(interface_name.clone(), ti.inner.id());
                let mut interface_env = generic_scope.enter_scope();
                for m in methods {
                    let method = m.method.clone();
                    let is_static = m.is_static;
                    let is_mutating = m.is_mutating;
                    let method_info;
                    let type_template;
                    
                    let construct_type_template = |generic_params: &Vec<Spanned<SmolStr>>| {
                        match ti.inner.kind() {
                            TypeKind::TypeClosure { params, body } => {
                                Some(spanned(
                                    TypeInfo::new_with_id(
                                        TypeKind::TypeClosure { 
                                            params: params
                                                .iter()
                                                .cloned()
                                                .filter(
                                                    |elem| !generic_params.contains(elem)
                                                ).collect(),
                                            body: body.clone()
                                        },
                                        ti.inner.id()
                                    ),
                                    ti.span
                                ))
                            },
                            _ => None
                        }
                    };
                    
                    match &method.inner {
                        Method::Normal(m) => {
                            if implemented_methods.contains(&m.name) {
                                return Err(spanned(
                                    format_smolstr!("Method {} is defined multiple times", m.name),
                                    method.span,
                                ));
                            }
                            let mut inner_scope = interface_env.enter_scope();
                            if !is_static {
                                inner_scope.add_var_type("self".into(), ti.clone(), false);
                            }
    
                            let expected_type = m
                                .return_type
                                .clone()
                                .unwrap_or(spanned(TypeInfo::void(), Span::at(0)));
    
                            type_template = construct_type_template(&m.generic_params);
                            
                            let fun_type;
                            let method_result;
                            let expected_flat;
                            
                            if m.generic_params.is_empty() {
                                // Non‑generic method
                                let mut flat_params = Vec::new();
                                for (n, p_type) in &m.params {
                                    let flattened =
                                        flatten_type(p_type, &mut inner_scope)?.into_owned();
                                    inner_scope.add_var_type(n.1.clone(), flattened.clone(), false);
                                    flat_params.push((n.clone(), flattened));
                                }
                                let expected_type = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                                inner_scope.add_var_type("+return".into(), expected_type.clone(), false);
                                inner_scope.add_var_type(
                                    m.name.clone(),
                                    spanned(
                                        TypeInfo::new(TypeKind::Fun {
                                            params: flat_params.clone(),
                                            return_type: Box::new(expected_type.clone()),
                                            generic_params: generic_params.clone(),
                                        }),
                                        ti.span.clone(),
                                    ),
                                    false
                                );
    
                                fun_type = spanned(
                                    TypeInfo::new(TypeKind::Fun {
                                        params: flat_params,
                                        return_type: Box::new(expected_type.clone()),
                                        generic_params: Vec::new(),
                                    }),
                                    ti.span,
                                );
                                
                                expected_flat = expected_type;
                                
                                implemented_methods.insert(m.name.clone());
                            } else {
                                // Generic method
                                for generic in generic_params {
                                    inner_scope.add_custom_type(
                                        generic.inner.clone(),
                                        spanned(
                                            TypeInfo::new(TypeKind::GenericParam(generic.inner.clone())),
                                            generic.span,
                                        ),
                                    );
                                }
    
                                let mut flat_params = Vec::new();
                                for (n, p_type) in &m.params {
                                    let flattened =
                                        flatten_type(p_type, &mut inner_scope)?.into_owned();
                                    inner_scope.add_var_type(n.1.clone(), flattened.clone(), false);
                                    flat_params.push((n.clone(), flattened));
                                }
    
                                expected_flat = flatten_type(&expected_type, &mut inner_scope)?.into_owned();
                                inner_scope.add_var_type("+return".into(), expected_flat.clone(), false);
                                
                                inner_scope.add_var_type(
                                    name.clone(),
                                    spanned(
                                        TypeInfo::new(TypeKind::Fun {
                                            params: flat_params.clone(),
                                            return_type: Box::new(expected_flat.clone()),
                                            generic_params: generic_params.clone(),
                                        }),
                                        ti.span.clone(),
                                    ),
                                    false
                                );
                                
                                fun_type = spanned(
                                    TypeInfo::new(TypeKind::Fun {
                                        params: flat_params,
                                        return_type: Box::new(expected_flat.clone()),
                                        generic_params: generic_params.clone(),
                                    }),
                                    ti.span,
                                );
                                
                                implemented_methods.insert(m.name.clone());
                            }
                            
                            method_result = process_expression(&m.body, &mut inner_scope)?;
                            if method_result.1.inner != expected_flat.inner {
                                return Err(spanned(
                                    format_smolstr!(
                                        "Function return type mismatch: expected {}, got {}",
                                        expected_flat.inner,
                                        method_result.1.inner
                                    ),
                                    expected_flat.span,
                                ));
                            }
                            
                            method_info = (
                                m.name.clone(),
                                MethodInfo {
                                    lowered: spanned(
                                        clean::Expr::Fun {
                                            params: m
                                                .params
                                                .iter()
                                                .map(|(e, _)| e.clone())
                                                .collect(),
                                            body: Box::new(method_result.0),
                                        },
                                        method.span,
                                    ),
                                    type_info: fun_type,
                                    type_template,
                                    is_mutating
                                }
                            );
                        },
                        Method::Native(m) => {
                            let fun_type;
                            if let None = get_native_fun(path, m.name.as_str()) {
                                return Err(Spanned {
                                    inner: format_smolstr!(
                                        "Cannot find native function definition for {}",
                                        m.name
                                    ),
                                    span: method.span
                                })
                            }
                            let native_fun = get_native_fun(path, m.name.as_str()).unwrap();
                            let mut inner_scope = interface_env.enter_scope();
                            let r_type = m.return_type.clone().unwrap_or(spanned(
                                TypeInfo::void(),
                                Span::from(0..0)
                            ));
                            
                            type_template = construct_type_template(&m.generic_params);
                            
                            if m.generic_params.len() == 0 {
                                let mut flat_params = Vec::new();
                                for (n, p_type) in &m.params {
                                    flat_params.push((
                                        n.clone(),
                                        flatten_type(&p_type, &mut inner_scope)?.into_owned()
                                    ))
                                }
                                let r_type = flatten_type(&r_type, &mut inner_scope)?.into_owned();
                                fun_type = spanned(
                                    TypeInfo::new(TypeKind::Fun {
                                        params: flat_params, 
                                        return_type: Box::new(r_type),
                                        generic_params: Vec::new()
                                    }),
                                    method.span
                                );
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
                                for (n, p_type) in &m.params {
                                    let flattened = flatten_type(&p_type, &mut inner_scope)?.into_owned();
                                    flat_params.push((n.clone(), flattened));
                                }
                
                                // Flatten return type as well
                                let r_flat = flatten_type(&r_type, &mut inner_scope)?.into_owned();
                
                                // Create the type-closure representing the generic native function
                                fun_type = spanned(
                                    TypeInfo::new(TypeKind::Fun {
                                        params: flat_params.clone(),
                                        return_type: Box::new(r_flat.clone()),
                                        generic_params: generic_params.clone()
                                    }),
                                    method.span
                                );
                            }
                            
                            env.add_var_type(name.clone(), fun_type.clone(), false);
                            
                            method_info = (
                                m.name.clone(),
                                MethodInfo {
                                    lowered: spanned(
                                        clean::Expr::NativeFun {
                                            name: m.name.clone(),
                                            path: path.into(),
                                            native_fun,
                                        },
                                        method.span,
                                    ),
                                    type_info: fun_type,
                                    type_template,
                                    is_mutating
                                }
                            );
                        },
                    }
                    match is_static {
                        true => {
                            env.insert_static_method(
                                ti.inner.id(),
                                method_info,
                            );
                        },
                        false => {
                            env.insert_method(
                                ti.inner.id(),
                                method_info,
                            );
                        },
                    }
                }
                env.copy_methods(ti.inner.id());
            }
        }
    }
    Ok(())
}