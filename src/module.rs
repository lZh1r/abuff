use std::{collections::HashMap, env::current_dir, fs, path::{Path, PathBuf}, sync::{Mutex, OnceLock}};

use chumsky::{Parser, span::SimpleSpan};

use crate::{ast::{self, Spanned, Statement, TypeInfo}, checker::{get_type, hoist, lower_statement, unwrap_custom_type}, env::{DEFAULT_ENVS, Env, TypeEnv, create_default_env}, error::build_report, interpreter::eval_expr, ir::{self, ControlFlow, Value}, main_parser::parser};

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleStatus {
    Evaluated,
    Loading
}

struct TypeModule {
    path: PathBuf,
    exports: HashMap<String, Spanned<TypeInfo>>,
    env: TypeEnv,
    status: ModuleStatus,
}

struct RuntimeModule {
    path: PathBuf,
    exports: HashMap<String, Value>,
    env: Env,
    status: ModuleStatus,
}

static TYPE_REGISTRY: OnceLock<Mutex<HashMap<PathBuf, TypeModule>>> = OnceLock::new();
static RUNTIME_REGISTRY: OnceLock<Mutex<HashMap<PathBuf, RuntimeModule>>> = OnceLock::new();

#[derive(Debug, Clone, PartialEq)]
pub enum RegistryType {
    Type,
    Runtime
}

trait ModuleRegistry {
    fn get_status(&self, registry_type: RegistryType, path: &Path) -> Option<ModuleStatus>;
    fn mark_loading(&self, registry_type: RegistryType, path: PathBuf) -> Result<(), Spanned<String>>;
    fn insert_runtime_module(&self, module: RuntimeModule);
    fn insert_type_module(&self, module: TypeModule);
    fn get_runtime_exports(&self, path: &Path) -> Option<HashMap<String, Value>>;
    fn get_type_exports(&self, path: &Path) -> Option<HashMap<String, Spanned<TypeInfo>>>;
}

pub struct GlobalRegistry;
impl ModuleRegistry for GlobalRegistry {
    fn get_status(&self, registry_type: RegistryType, path: &Path) -> Option<ModuleStatus> {
        match registry_type {
            RegistryType::Type => {
                let map = TYPE_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
                map.get(path).map(|m| m.status.clone())
            },
            RegistryType::Runtime => {
                let map = RUNTIME_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
                map.get(path).map(|m| m.status.clone())
            },
        }
    }
    
    fn mark_loading(&self, registry_type: RegistryType, path: PathBuf) -> Result<(), Spanned<String>> {
        match registry_type {
            RegistryType::Type => {
                let mut map = TYPE_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
                if map.contains_key(&path) {
                    return Err(Spanned {
                        inner: "Circular or duplicate load".into(),
                        span: SimpleSpan::from(0..0)
                    });
                } else {
                    map.insert(path.clone(), TypeModule {
                        path,
                        env: TypeEnv::new(),
                        exports: HashMap::new(),
                        status: ModuleStatus::Loading
                    });
                }
                Ok(())
            },
            RegistryType::Runtime => {
                let mut map = RUNTIME_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
                if map.contains_key(&path) {
                    return Err(Spanned {
                        inner: "Circular or duplicate load".into(),
                        span: SimpleSpan::from(0..0)
                    });
                } else {
                    map.insert(path.clone(), RuntimeModule {
                        path,
                        env: Env::new(),
                        exports: HashMap::new(),
                        status: ModuleStatus::Loading
                    });
                }
                Ok(())
            },
        }
    }
    
    fn insert_runtime_module(&self, module: RuntimeModule) {
        let mut map = RUNTIME_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
        map.insert(module.path.clone(), module);
    }
    
    fn insert_type_module(&self, module: TypeModule) {
        let mut map = TYPE_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
        map.insert(module.path.clone(), module);
    }
       
    fn get_runtime_exports(&self, path: &Path) -> Option<HashMap<String, Value>> {
        let map = RUNTIME_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
        match map.get(path) {
            Some(module) => Some(module.exports.clone()),
            None => None,
        } 
    }
    
    fn get_type_exports(&self, path: &Path) -> Option<HashMap<String, Spanned<TypeInfo>>> {
        let map = TYPE_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
        match map.get(path) {
            Some(module) => Some(module.exports.clone()),
            None => None,
        } 
    }
}

pub fn get_export_values<R: ModuleRegistry>(path: &str, registry: &R) -> Option<HashMap<String, Value>> {
    let path = Path::new(path);
    if registry.get_status(RegistryType::Runtime, path) == Some(ModuleStatus::Evaluated) {
        registry.get_runtime_exports(path)
    } else {None}
}

pub fn insert_type_module<R: ModuleRegistry>(registry: &R, exports: HashMap<String, Spanned<TypeInfo>>, env: TypeEnv) {
    let path = current_dir().unwrap();
    registry.insert_type_module(TypeModule {
        path,
        exports,
        env,
        status: ModuleStatus::Evaluated,
    });
}

pub fn eval_import<R: ModuleRegistry>(path: &str, registry: &R) -> Result<HashMap<String, Spanned<TypeInfo>>, Spanned<String>> {
    let path = Path::new(path);
    if registry.get_status(RegistryType::Type, path) == Some(ModuleStatus::Loading) ||
    registry.get_status(RegistryType::Runtime, path) == Some(ModuleStatus::Loading) {
        return Err(Spanned {
            inner: "Cycle detected".into(),
            span: SimpleSpan::from(0..0)
        });
    } else if registry.get_status(RegistryType::Type, path) == Some(ModuleStatus::Evaluated) {
        return Ok(registry.get_type_exports(path).unwrap())
    }
    
    registry.mark_loading(RegistryType::Type, path.to_path_buf())?;
    registry.mark_loading(RegistryType::Runtime, path.to_path_buf())?;
    
    let src;
    match fs::read_to_string(path) {
        Ok(s) => src = s,
        Err(_) => return Err(Spanned {
            inner: "File IO failed".into(),
            span: SimpleSpan::from(0..0)
        }),
    }
    let (mut module_env, mut module_type_env) = DEFAULT_ENVS.get_or_init(|| create_default_env()).clone();
    let parse_result = parser().parse(src.as_str()).into_result();
    match parse_result {
        Ok(_) => (),
        Err(errors) => {
            for e in &errors {
                build_report(Spanned {
                    inner: e.reason().to_string(),
                    span: e.span().clone()
                }, &src);
            }
            return Err(Spanned {
                inner: format!("Module parsing failed due to {} previous errors", errors.len()),
                span: SimpleSpan::from(0..0)
            })
        },
    }
    let statements = parse_result.unwrap();
    
    let mut type_exports: HashMap<String, Spanned<TypeInfo>> = HashMap::new();
    let mut new_statements: Vec<Spanned<ast::Statement>> = Vec::new();
    
    //HOISING AND IMPORT/EXPORT RESSOLUTION
    for st in statements.iter() {
        match &st.inner {
            ast::Statement::TypeDef { name, type_info } => {
                let resolved_type = unwrap_custom_type(type_info.clone(), &mut module_type_env)?;
                module_type_env.add_custom_type(name.clone(), resolved_type);
            },
            ast::Statement::Import { symbols, path } => {
                if module_type_env.is_top_level() {
                    let global_reg = GlobalRegistry;
                    let module_types = eval_import(path, &global_reg)?;
                    for (name, alias, is_type) in symbols {
                        let actual_name = alias.as_ref().unwrap_or(&name.inner);
                        if !module_types.contains_key(&name.inner) {
                            return Err(Spanned {
                                inner: format!("Cannot resolve import {} from {}", name.inner, path.to_string()),
                                span: name.span
                            })
                        }
                        let unwrapped_type = unwrap_custom_type(
                            module_types.get(actual_name).unwrap().clone(),
                            &mut module_type_env
                        )?;
                        if *is_type {
                            println!("{unwrapped_type:?}");
                            module_type_env.add_custom_type(actual_name.clone(), unwrapped_type);
                        } else {
                            module_type_env.add_var_type(actual_name.clone(), unwrapped_type);
                        }
                        
                    }
                    new_statements.push(st.clone());
                } else {
                    return Err(Spanned {
                        inner: "Non top level imports are prohibited".to_string(),
                        span: st.span
                    });
                }
            }
            _ => new_statements.push(st.clone()),
        }
    }
    for st in statements.iter() {
        match &st.inner {
            ast::Statement::Let { name, expr, type_info } => {
                let inferred_type = unwrap_custom_type(get_type(&expr, &mut module_type_env)?, &mut module_type_env)?;
                match type_info {
                    Some(t) => {
                        let unwrapped_ti = unwrap_custom_type(t.clone(), &mut module_type_env)?;
                        if unwrapped_ti.inner == inferred_type.inner {
                            module_type_env.add_var_type(name.clone(), unwrapped_ti);
                        } else {
                            return Err( Spanned {
                                inner: format!("Type mismatch when declaring {name}: expected {:?}, but got {:?}", unwrapped_ti.inner, inferred_type.inner),
                                span: st.span
                            })
                        }
                    },
                    None => module_type_env.add_var_type(name.clone(), inferred_type),
                }
            },
            ast::Statement::Fun { name, params, body, return_type } => {
                let r_type = unwrap_custom_type(return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: st.span}), &mut module_type_env)?;
                module_type_env.add_var_type(name.to_string(), Spanned {
                    inner: TypeInfo::Fun { 
                        args: params.clone(), 
                        return_type: Box::new(r_type.clone()) 
                    },
                    span: st.span
                });
    
                let mut new_scope = module_type_env.enter_scope();
                for (param_name, param_type) in params.clone() {
                    new_scope.add_var_type(param_name.to_string(), unwrap_custom_type(param_type.clone(), &mut module_type_env)?);
                }
                new_scope.add_var_type("&return".to_string(), r_type.clone());
                let inferred_type = get_type(&body, &mut new_scope)?;
                if inferred_type.inner != r_type.inner {
                    return Err(Spanned {
                        inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                        span: st.span
                    })
                }
            },
            ast::Statement::TypeDef { name: _, type_info: _ } => (), //already evaluated
            ast::Statement::Expr(_) => (), //later
            ast::Statement::Import { symbols: _, path: _ } => (), //already evaluated
            ast::Statement::Export(spanned) => {
                match &spanned.inner {
                    Statement::Let { name, expr, type_info } => {
                        let inferred_type = unwrap_custom_type(get_type(&expr, &mut module_type_env)?, &mut module_type_env)?;
                        match type_info { //copy paste but idc
                            Some(t) => {
                                let unwrapped_ti = unwrap_custom_type(t.clone(), &mut module_type_env)?;
                                if unwrapped_ti.inner == inferred_type.inner {
                                    module_type_env.add_var_type(name.clone(), unwrapped_ti);
                                } else {
                                    return Err( Spanned {
                                        inner: format!("Type mismatch when declaring {name}: expected {:?}, but got {:?}", unwrapped_ti.inner, inferred_type.inner),
                                        span: st.span
                                    })
                                }
                            },
                            None => module_type_env.add_var_type(name.clone(), inferred_type.clone()),
                        }
                        type_exports.insert(name.clone(), inferred_type);
                    },
                    Statement::Fun { name, params, body, return_type } => {
                        let r_type = unwrap_custom_type(
                            return_type.clone().unwrap_or(Spanned {inner: TypeInfo::Void, span: spanned.span}),
                            &mut module_type_env
                        )?;
                        
                        let mut unwrapped_params = Vec::new();
                        
                        for (pname, ti) in params {
                            unwrapped_params.push((pname.clone(), unwrap_custom_type(ti.clone(), &mut module_type_env)?))
                        }
                        
                        let function_type = Spanned {
                            inner: TypeInfo::Fun { 
                                args: unwrapped_params, 
                                return_type: Box::new(r_type.clone()) 
                            },
                            span: st.span
                        };
                        
                        module_type_env.add_var_type(name.to_string(), function_type.clone());
            
                        let mut new_scope = module_type_env.enter_scope();
                        for (param_name, param_type) in params.clone() {
                            new_scope.add_var_type(param_name.to_string(), unwrap_custom_type(param_type.clone(), &mut module_type_env)?);
                        }
                        new_scope.add_var_type("&return".to_string(), r_type.clone());
                        let inferred_type = get_type(&body, &mut new_scope)?;
                        if inferred_type.inner != r_type.inner {
                            return Err(Spanned {
                                inner: format!("Expected {:?}, but got {:?}", r_type.inner, inferred_type.inner),
                                span: st.span
                            })
                        }
                        
                        type_exports.insert(name.clone(), function_type);
                    },
                    Statement::TypeDef { name, type_info } => {
                        let resolved_type = unwrap_custom_type(type_info.clone(), &mut module_type_env)?;
                        module_type_env.add_custom_type(name.clone(), resolved_type.clone());
                        type_exports.insert(name.clone(), resolved_type);
                    },
                    Statement::Expr(_) => return Err(Spanned {
                        inner: "Cannot export expressions".to_string(),
                        span: spanned.span
                    }),
                    Statement::Import { symbols: _, path: _ } => panic!(), //should be caught by parser
                    Statement::Export(_) => panic!(), //should be caught by parser
                }
            },
        }
    }
    //At this point type_exports is formed and can be returned after the evaluation ends
    
    registry.insert_type_module(TypeModule { 
        path: path.to_path_buf(),
        exports: type_exports.clone(),
        env: module_type_env.clone(),
        status: ModuleStatus::Evaluated
    });
    
    let mut lowered_statements = Vec::new();
    
    for st in new_statements {
        match lower_statement(&st, &mut module_type_env)? {
            Some(s) => lowered_statements.push(s),
            None => continue,
        }
    }
    
    match run(&lowered_statements, &mut module_env, path.to_path_buf(), registry) {
        Ok(_) => (),
        Err(e) => return Err(e),
    }
    
    Ok(type_exports)
}

pub fn run<R: ModuleRegistry>(statements: &[Spanned<ir::Statement>], env: &mut Env, module_path: PathBuf, registry: &R) -> Result<ControlFlow, Spanned<String>> {
    let mut result = Ok(ControlFlow::Value(Value::Void));
    let mut value_exports = HashMap::new();
    for statement in statements {
        match statement.inner.clone() {
            ir::Statement::Let { name, expr } => {
                match eval_expr(&expr, env)? {
                    ControlFlow::Value(v) => env.add_variable(name.clone(), v),
                    cf => {
                        registry.insert_runtime_module(RuntimeModule { 
                            path: module_path.clone(), 
                            exports: value_exports,
                            env: env.clone(),
                            status: ModuleStatus::Evaluated
                        });
                        return Ok(cf)
                    },
                }
            },
            ir::Statement::Expr(expr) => {
                let cf = eval_expr(&expr, env)?;
                match cf {
                    ControlFlow::Value(_) => result = Ok(cf),
                    _ => {
                        registry.insert_runtime_module(RuntimeModule { 
                            path: module_path.clone(), 
                            exports: value_exports,
                            env: env.clone(),
                            status: ModuleStatus::Evaluated
                        });
                        return Ok(cf)
                    },
                }
            },
            ir::Statement::Import { symbols, path } => {
                let reg = GlobalRegistry;
                match get_export_values(&path.inner, &reg) {
                    Some(map) => {
                        for (name, alias) in symbols {
                            let actual_name = alias.clone().unwrap_or(name.inner.clone());
                            if map.contains_key(&name.inner) {
                                env.add_variable(actual_name, map.get(&name.inner).unwrap().clone());
                            } else {
                                return Err(Spanned {
                                    inner: format!("Cannot resolve import {} from {}", name.inner, path.to_string()),
                                    span: name.span
                                })
                            }
                        }
                    },
                    None => panic!(), //theoretically impossible since there are no circular imports
                }
            },
            ir::Statement::Export(spanned) => {
                match &spanned.inner {
                    ir::Statement::Let { name, expr } => {
                        match eval_expr(&expr, env)? {
                            ControlFlow::Value(v) => {
                                env.add_variable(name.clone(), v.clone());
                                value_exports.insert(name.clone(), v);
                            },
                            cf => {
                                registry.insert_runtime_module(RuntimeModule { 
                                    path: module_path.clone(), 
                                    exports: value_exports,
                                    env: env.clone(),
                                    status: ModuleStatus::Evaluated
                                });
                                return Ok(cf)
                            },
                        }
                        
                    },
                    ir::Statement::Expr(spanned) => {
                        return Err(Spanned {
                            inner: "Cannot export an expression".into(),
                            span: spanned.span
                        })
                    },
                    ir::Statement::Import { symbols: _, path: _ } => panic!(), //caught by parser
                    ir::Statement::Export(_) => panic!(), //caught by parser
                }
            },
        }
    };
    
    registry.insert_runtime_module(RuntimeModule { 
        path: module_path.clone(), 
        exports: value_exports,
        env: env.clone(),
        status: ModuleStatus::Evaluated
    });
    result
}