use crate::{ast::{self, TypeInfo}, env::TypeEnv, ir};

pub fn hoist(statements: &[ast::Statement], env: &mut TypeEnv) -> Result<Vec<ast::Statement>, String> {
    let mut new_statements: Vec<ast::Statement> = Vec::new();
    for st in statements.iter() {
        match st {
            ast::Statement::TypeDef { name, type_info } => {
                let resolved_type = unwrap_custom_type(type_info.clone(), env)?;
                env.add_custom_type(name.clone(), resolved_type);
            },
            s => new_statements.push(s.clone()),
        }
    }
    for st in statements.iter() {
        match st {
            ast::Statement::Let { name, expr, type_info } => {
                let inferred_type = unwrap_custom_type(get_type(expr, env)?, env)?;
                match type_info {
                    Some(t) => {
                        let unwrapped_ti = unwrap_custom_type(t.clone(), env)?;
                        if unwrapped_ti == inferred_type {
                            env.add_var_type(name.clone(), unwrapped_ti);
                        } else {
                            return Err(
                                format!(r#"
                                    Type mismatch when declaring {name}:
                                    expected {unwrapped_ti:?}, but got {inferred_type:?}
                                "#).to_string()
                            )
                        }
                    },
                    None => env.add_var_type(name.clone(), inferred_type),
                }
            },
            ast::Statement::Fun { name, params, body, return_type } => {
                env.add_var_type(name.to_string(), TypeInfo::Fun { args: params.clone(), return_type: Box::new(return_type.clone().unwrap_or(TypeInfo::Void)) });
                
                let mut new_scope = env.enter_scope();
                for (param_name, param_type) in params.clone() {
                    new_scope.add_var_type(param_name.to_string(), param_type.clone());
                }
                let inferred_type = get_type(body, &mut new_scope)?;
                let r_type = return_type.clone().unwrap_or(TypeInfo::Void);
                if inferred_type != r_type {
                    return Err(format!("Expected {r_type:?} but got {inferred_type:?}"))
                }
            },
            _ => ()
        }
    }
    
    Ok(new_statements)
}

pub fn lower_statement(statement: &ast::Statement, env: &mut TypeEnv) -> Result<Option<ir::Statement>, String> {
    match statement {
        ast::Statement::TypeDef { name: _, type_info: _ } => Ok(None),
        ast::Statement::Expr(expr) => {
            Ok(Some(ir::Statement::Expr(lower_expr(expr, env)?)))
        },
        ast::Statement::Let { name, expr, type_info: _ } => {
            Ok(Some(ir::Statement::Let { name: name.clone(), expr: lower_expr(expr, env)? }))
        }
        ast::Statement::Fun { name, params, body, return_type } => {
            Ok(Some(ir::Statement::Let { 
                name: name.clone(),
                expr: lower_expr(&ast::Expr::Fun { params: params.clone(), body: Box::new(body.clone()), return_type: return_type.clone() }, env)?
            }))
        },
    }
}

pub fn lower_expr(expr: &ast::Expr, env: &mut TypeEnv) -> Result<ir::Expr, String> {
    match expr {
        ast::Expr::Bool(b) => Ok(ir::Expr::Bool(*b)),
        ast::Expr::Float(f) => Ok(ir::Expr::Float(*f)),
        ast::Expr::Int(i) => Ok(ir::Expr::Int(*i)),
        ast::Expr::Var(v) => Ok(ir::Expr::Var(v.clone())),
        ast::Expr::Binary { left, operation, right } => {
            let left_type = get_type(left, env)?;
            let right_type = get_type(right, env)?;
            if left_type == right_type {
                Ok(ir::Expr::Binary { 
                    left: Box::new(lower_expr(left, env)?),
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
                    right: Box::new(lower_expr(right, env)?)
                })
            } else {
                Err(format!("Type {left_type:?} is not assignable to type {right_type:?}"))
            }
        },
        ast::Expr::Block(statements, expr) => {
            let mut new_scope = env.enter_scope();
            let mut lowered_statements = Vec::new();
            for s in statements {
                match lower_statement(s, env) {
                    Ok(s) => match s {
                        Some(s) => lowered_statements.push(s),
                        None => (),
                    },
                    Err(e) => return Err(e),
                }
            }
            let final_expr = match expr {
                Some(e) => Some(Box::new(lower_expr(e, &mut new_scope)?)),
                None => None,
            };
            Ok(ir::Expr::Block(lowered_statements, final_expr))
        },
        ast::Expr::Fun { params, body, return_type } => {
            let mut new_scope = env.enter_scope();
            for (param_name, param_type) in params {
                new_scope.add_var_type(param_name.to_string(), param_type.clone());
            }
            let inferred_type = get_type(body, &mut new_scope)?;
            let r_type = return_type.clone().unwrap_or(TypeInfo::Void);
            if r_type == inferred_type {
                Ok(ir::Expr::Fun { params: params.iter().map(|(name, _)| name.to_string()).collect(), body: Box::new(lower_expr(body, &mut new_scope)?) })
            } else {
                Err(format!("Expected {r_type:?} but got {inferred_type:?}"))
            }
        },
        ast::Expr::Call { fun, args } => {
            match get_type(&**fun, env)? {
                TypeInfo::Fun { args: params, return_type: _ } => {
                    if params.get(0).unwrap_or(&("_".to_string(), TypeInfo::Void)).1 != TypeInfo::Any {
                        for ((_, ti), expr) in params.iter().zip(args.iter()) {
                            if unwrap_custom_type(ti.clone(), env)? != get_type(expr, env)? {
                                return Err("".to_string())
                            }
                        }
                    }
                    
                    Ok(ir::Expr::Call { 
                        fun: Box::new(lower_expr(fun, env)?), 
                        args: args.iter().map(|expr| lower_expr(expr, env).unwrap()).collect() 
                    })
                },
                t => Err(format!("Type {t:?} is not callable"))
            }
        },
        ast::Expr::Record(items) => Ok(ir::Expr::Record(items.iter().map(|(name, expr)| (name.to_string(), lower_expr(expr, env).unwrap())).collect())),
        ast::Expr::Get(object, property_name) => Ok(ir::Expr::Get(Box::new(lower_expr(object, env)?), property_name.to_string())),
        ast::Expr::Assign { target, value } => {
            let target_type = get_type(target, env)?;
            let value_type = get_type(value, env)?;
            if target_type == value_type {
                Ok(ir::Expr::Assign { target: Box::new(lower_expr(target, env)?), value: Box::new(lower_expr(value, env)?) })
            } else {
                Err(format!("Type {value_type:?} is not assignable to {target_type:?}"))
            }
        },
        ast::Expr::Unary(unary_op, expr) => {
            match unary_op {
                ast::UnaryOp::Negate => {
                    let expr_type = get_type(expr, env)?;
                    match expr_type {
                        TypeInfo::Int => Ok(ir::Expr::Unary(unary_op.clone(), Box::new(lower_expr(expr, env)?))),
                        TypeInfo::Float => Ok(ir::Expr::Unary(unary_op.clone(), Box::new(lower_expr(expr, env)?))),
                        _ => Err(format!("Type {expr_type:?} cannot be negated"))
                    }
                },
                ast::UnaryOp::Not => {
                    let expr_type = get_type(expr, env)?;
                    match expr_type {
                        TypeInfo::Bool => Ok(ir::Expr::Unary(unary_op.clone(), Box::new(lower_expr(expr, env)?))),
                        _ => Err(format!("Type {expr_type:?} cannot be inverted"))
                    }
                },
            }
        },
        ast::Expr::If { condition, body, else_block } => {
            if get_type(condition, env)? == TypeInfo::Bool {
                match else_block {
                    Some(b) => {
                        let body_type = get_type(body, env)?;
                        let else_type = get_type(b, env)?;
                        if body_type == else_type {
                            Ok(ir::Expr::If { 
                                condition: Box::new(lower_expr(condition, env)?),
                                body: Box::new(lower_expr(body, env)?),
                                else_block: Some(Box::new(lower_expr(b, env)?)) 
                            })
                        } else {
                            Err(format!("If branches have incompatible types: {body_type:?} and {else_type:?}"))
                        }
                    },
                    None => Ok(ir::Expr::If { 
                        condition: Box::new(lower_expr(condition, env)?),
                        body: Box::new(lower_expr(body, env)?),
                        else_block: None 
                    }),
                }
            } else {
                return Err("If condition should return Bool".to_string());
            }
        },
        ast::Expr::While { condition, body } => {
            if get_type(condition, env)? == TypeInfo::Bool && get_type(body, env)? == TypeInfo::Void {
                Ok(ir::Expr::While { condition: Box::new(lower_expr(condition, env)?), body: Box::new(lower_expr(body, env)?) })
            } else {
                Err("While condition should return Bool and the body should return Void".to_string())
            }
        },
        ast::Expr::String(s) => Ok(ir::Expr::String(s.clone())),
    }
}

fn get_type(expr: &ast::Expr, env: &mut TypeEnv) -> Result<TypeInfo, String> {
    match expr {
        ast::Expr::Bool(_) => Ok(TypeInfo::Bool),
        ast::Expr::Float(_) => Ok(TypeInfo::Float),
        ast::Expr::Int(_) => Ok(TypeInfo::Int),
        ast::Expr::Var(name) => {
            match env.get_var_type(name) {
                Some(t) => Ok(unwrap_custom_type(t, env)?),
                None => Err(format!("Type of \"{name}\" is unknown")),
            }
        },
        ast::Expr::Binary { left, operation, right } => {
            let left_type = unwrap_custom_type(get_type(left, env)?, env)?;
            let right_type = unwrap_custom_type(get_type(right, env)?, env)?;
            if left_type == right_type {
                match operation {
                    ast::Operation::Eq => Ok(TypeInfo::Bool),
                    ast::Operation::NotEq => Ok(TypeInfo::Bool),
                    ast::Operation::LessThan => Ok(TypeInfo::Bool),
                    ast::Operation::LessThanEq => Ok(TypeInfo::Bool),
                    ast::Operation::GreaterThan => Ok(TypeInfo::Bool),
                    ast::Operation::GreaterThanEq => Ok(TypeInfo::Bool),
                    ast::Operation::And => Ok(TypeInfo::Bool),
                    ast::Operation::Or => Ok(TypeInfo::Bool),
                    _ => Ok(left_type)
                }
            } else {
                Err(format!("Type {left_type:?} is not assignable to type {right_type:?} ({operation:?})"))
            }
        },
        ast::Expr::Block(statements, tail_expr) => {
            let mut inner_env = env.enter_scope(); 

            for stmt in statements {
                match stmt {
                    ast::Statement::Let { name, expr, type_info } => {
                        let expr_type = get_type(expr, &mut inner_env)?;

                        let final_type = match type_info {
                            Some(ann) => {
                                ann.clone() 
                            }
                            None => expr_type
                        };
                        inner_env.add_var_type(name.clone(), final_type);
                    },
                    ast::Statement::TypeDef { name, type_info } => {
                        inner_env.add_custom_type(name.clone(), type_info.clone());
                    },
                    _ => {} 
                }
            }

            match tail_expr {
                Some(e) => get_type(e, &mut inner_env),
                None => Ok(TypeInfo::Void),
            }
        },
        ast::Expr::Fun { params, body, return_type } => {
            let mut new_scope = env.enter_scope();
            for (param_name, param_type) in params {
                new_scope.add_var_type(param_name.to_string(), param_type.clone());
            }
            let inferred_type = unwrap_custom_type(get_type(body, &mut new_scope)?, &mut new_scope)?;
            let r_type = unwrap_custom_type(return_type.clone().or(Some(TypeInfo::Void)).unwrap(), env)?;
            if r_type != inferred_type {
                return Err(format!("Expected {r_type:?} but got {inferred_type:?}"))
            }
            Ok(TypeInfo::Fun { 
                args: params.iter().map(|(name, ti)| (name.to_string(), unwrap_custom_type(ti.clone(), &mut new_scope).unwrap())).collect(), 
                return_type: Box::new(r_type) 
            })
        },
        ast::Expr::Call { fun, args } => {
            let fun_type = unwrap_custom_type(get_type(fun, env)?, env)?;
            
            match fun_type {
                TypeInfo::Fun { args: params, return_type } => {
                    if params.get(0).unwrap_or(&("_".to_string(), TypeInfo::Void)).1 != TypeInfo::Any {
                        if params.len() != args.len() {
                            return Err(format!("Expected {} args, got {}", params.len(), args.len()));
                        }
                        for ((_, param_type), arg_expr) in params.iter().zip(args.iter()) {
                            let arg_type = get_type(arg_expr, env)?;
                            if unwrap_custom_type(param_type.clone(), env)? != unwrap_custom_type(arg_type, env)? {
                                return Err("Argument type mismatch".to_string());
                            }
                        }
                    }
                    Ok(*return_type)
                },
                _ => Err(format!("Attempted to call non-function type: {:?}", fun_type)),
            }
        },
        ast::Expr::Record(items) => {
            let mut record_type = Vec::new();
            for (property_name, expr) in items {
                record_type.push((property_name.to_string(), get_type(expr, env)?));
            }
            Ok(TypeInfo::Record(record_type))
        },
        ast::Expr::Get(expr, property_name) => {
            let record_type = get_type(expr, env)?;
            match unwrap_custom_type(record_type.clone(), env)? {
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
                        None => Err(format!("Property {property_name} does not exist on type {record_type:?}")),
                    }
                },
                t => Err(format!("Type {t:?} does not have property {property_name}"))
            }
        },
        ast::Expr::Assign { target: _, value: _ } => Ok(TypeInfo::Void),
        ast::Expr::Unary(_, expr) => Ok(get_type(expr, env)?),
        ast::Expr::If { condition, body, else_block } => {
            if get_type(condition, env)? == TypeInfo::Bool {
                let body_type = get_type(body, env)?;
                match else_block {
                    Some(b) => {
                        let else_type = get_type(b, env)?;
                        if body_type == else_type {
                            Ok(body_type)
                        } else {
                            Err(format!("If branches have incompatible types: {body_type:?} and {else_type:?}"))
                        }
                    },
                    None => Ok(body_type),
                }
            } else {
                return Err("If condition should return Bool".to_string());
            }
        },
        ast::Expr::While { condition, body } => Ok(TypeInfo::Void),
        ast::Expr::String(_) => Ok(TypeInfo::String),
    }
}

fn unwrap_custom_type(type_info: TypeInfo, env: &mut TypeEnv) -> Result<TypeInfo, String> {
    match type_info {
        TypeInfo::Custom(type_name) => {
            match env.resolve_type(type_name.as_str()) {
                Some(ti) => Ok(unwrap_custom_type(ti, env)?),
                None => Err(format!("Cannot resolve type {type_name}")),
            }
        },
        TypeInfo::Record(fields) => {
            let mut new_fields = Vec::new();
            for (name, ti) in fields {
                match unwrap_custom_type(ti, env) {
                    Ok(t) => new_fields.push((name, t)),
                    Err(e) => return Err(e),
                }
            }
            Ok(TypeInfo::Record(new_fields))
        },
        TypeInfo::Fun { args, return_type } => {
            let mut new_args = Vec::new();
            for (name, ti) in args {
                match unwrap_custom_type(ti.clone(), env) {
                    Ok(t) => new_args.push((name, t)),
                    Err(e) => return Err(e),
                }
            }
            let new_return_type = unwrap_custom_type(*return_type, env)?;
            Ok(TypeInfo::Fun { args: new_args, return_type: Box::new(new_return_type) })
        },
        _ => Ok(type_info)
    }
}