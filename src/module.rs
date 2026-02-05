use std::{collections::HashMap, env::current_dir, fs, path::{Path, PathBuf}, sync::{Mutex, OnceLock}};

use crate::{ast::{Span, Spanned, TypeInfo}, env::{DEFAULT_ENVS, Env, TypeEnv, create_default_env}, error::build_report, interpreter::eval_expr, ir::{self, ControlFlow, Value}, lexer::lex, main_parser::Parser, native::get_native_fun, type_checker::{hoist, lower_statement}};

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleStatus {
    Evaluated,
    Loading
}

struct TypeModule {
    path: PathBuf,
    var_exports: HashMap<String, Spanned<TypeInfo>>,
    type_exports: HashMap<String, Spanned<TypeInfo>>,
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
    fn get_var_exports(&self, path: &Path) -> Option<HashMap<String, Spanned<TypeInfo>>>;
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
                        span: Span::from(0..0)
                    });
                } else {
                    map.insert(path.clone(), TypeModule {
                        path,
                        env: TypeEnv::new(),
                        var_exports: HashMap::new(),
                        type_exports: HashMap::new(),
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
                        span: Span::from(0..0)
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
            Some(module) => Some(module.type_exports.clone()),
            None => None,
        } 
    }
    
    fn get_var_exports(&self, path: &Path) -> Option<HashMap<String, Spanned<TypeInfo>>> {
        let map = TYPE_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
        match map.get(path) {
            Some(module) => Some(module.var_exports.clone()),
            None => None,
        } 
    }
}

#[allow(private_bounds)]
pub fn get_export_values<R: ModuleRegistry>(path: &str, registry: &R) -> Option<HashMap<String, Value>> {
    let path = Path::new(path);
    if registry.get_status(RegistryType::Runtime, path) == Some(ModuleStatus::Evaluated) {
        registry.get_runtime_exports(path)
    } else {None}
}

#[allow(private_bounds)]
pub fn insert_type_module<R: ModuleRegistry>(
    registry: &R,
    var_exports: HashMap<String, Spanned<TypeInfo>>,
    type_exports: HashMap<String, Spanned<TypeInfo>>,
    env: TypeEnv,
    path: &str
) {
    let path = PathBuf::from(path);
    registry.insert_type_module(TypeModule {
        path,
        var_exports,
        type_exports,
        env,
        status: ModuleStatus::Evaluated,
    });
}

#[allow(private_bounds)]
pub fn get_module_envs<R: ModuleRegistry>(registry: &R, path: &str) -> Result<(Env, TypeEnv), String> {
    let path = Path::new(path);
    if registry.get_status(RegistryType::Type, path).unwrap() == ModuleStatus::Loading ||
    registry.get_status(RegistryType::Runtime, path).unwrap() == ModuleStatus::Loading {
        Err("Modules are still loading".into())
    } else {
        let map = TYPE_REGISTRY.get().unwrap().lock().unwrap();
        let target_module = match map.get(path) {
            Some(tm) => tm,
            None => return Err(format!("Module \"{}\"", path.to_str().unwrap().to_string())),
        };
        let type_env = target_module.env.clone();
        
        let map = RUNTIME_REGISTRY.get().unwrap().lock().unwrap();
        let target_module = match map.get(path) {
            Some(tm) => tm,
            None => return Err(format!("Module \"{}\"", path.to_str().unwrap().to_string())),
        };
        let env = target_module.env.clone();
        Ok((env, type_env))
    }
}

#[allow(private_bounds)]
pub fn eval_import<R: ModuleRegistry>(path: &str, registry: &R) -> Result<
    (HashMap<String, Spanned<TypeInfo>>, HashMap<String, Spanned<TypeInfo>>), Spanned<String>
> {
    let path = Path::new(path);
    if registry.get_status(RegistryType::Type, path) == Some(ModuleStatus::Loading) ||
    registry.get_status(RegistryType::Runtime, path) == Some(ModuleStatus::Loading) {
        return Err(Spanned {
            inner: "Cycle detected".into(),
            span: Span::from(0..0)
        });
    } else if registry.get_status(RegistryType::Type, path) == Some(ModuleStatus::Evaluated) {
        return Ok((registry.get_var_exports(path).unwrap(), registry.get_type_exports(path).unwrap()))
    }
    
    registry.mark_loading(RegistryType::Type, path.to_path_buf())?;
    registry.mark_loading(RegistryType::Runtime, path.to_path_buf())?;
    
    let src;
    match fs::read_to_string(path) {
        Ok(s) => src = s,
        Err(_) => return Err(Spanned {
            inner: "File IO failed".into(),
            span: Span::from(0..0)
        }),
    }
    let (mut module_env, mut module_type_env);
    if !path.to_str().unwrap().contains("std/builtins") {
        (module_env, module_type_env) = DEFAULT_ENVS.get_or_init(|| create_default_env()).clone();
    } else {
        (module_env, module_type_env) = (Env::new(), TypeEnv::new())
    }
    
    let parse_result = Parser::new(&lex(src.as_str())?).parse();
    let statements = match parse_result {
        Ok(s) => s,
        Err(errors) => {
            // for e in &errors {
            //     build_report(Spanned {
            //         inner: e.reason().to_string(),
            //         span: e.span().clone()
            //     }, &src);
            // }
            build_report(Spanned {
                inner: errors.inner,
                span: errors.span
            }, &src);
            return Err(Spanned {
                inner: format!("Module parsing failed"),
                span: Span::from(0..0)
            })
        },
    };
    
    let (ordered_statements, var_exports, type_exports) = hoist(&statements, &mut module_type_env, path.to_str().unwrap())?;
    
    let mut lowered_statements = Vec::new();
    
    for st in ordered_statements {
        match lower_statement(&st, &mut module_type_env)? {
            Some(s) => lowered_statements.push(s),
            None => continue,
        }
    }
    
    match run(&lowered_statements, &mut module_env, path.to_path_buf(), registry) {
        Ok(_) => (),
        Err(e) => return Err(e),
    }
    
    Ok((var_exports, type_exports))
}

#[allow(private_bounds)]
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
                            span: spanned.span.clone()
                        })
                    },
                    ir::Statement::Import { symbols: _, path: _ } => panic!(),
                    ir::Statement::Export(_) => panic!(),
                    ir::Statement::NativeFun(name) => {
                        let str_path = module_path.to_str().unwrap();
                        match get_native_fun(str_path, name) {
                            Some(ptr) => {
                                let fn_val = Value::NativeFun { 
                                    path: str_path.to_owned(),
                                    name: name.clone(), 
                                    pointer: ptr
                                };
                                value_exports.insert(name.clone(), fn_val.clone());
                                env.add_variable(name.clone(), fn_val);
                            },
                            None => return Err(Spanned {
                                inner: format!("Cannot find native function definition for {}", name),
                                span: statement.span.clone()
                            })
                        }
                        
                    },
                }
            },
            ir::Statement::NativeFun(_) => todo!(), //later
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