use std::{collections::HashMap, fs, sync::{Arc, OnceLock, RwLock}, time::Instant};

use smol_str::{SmolStr, ToSmolStr};

use crate::{ast::{Span, Spanned, TypeInfo}, error::build_report, ir::{self, ControlFlow, Value}, module::{GlobalRegistry, eval_import, get_module_envs}, native::register_fun};

#[derive(Debug, Clone)]
pub struct Scope {
    values: HashMap<SmolStr, Value>,
    parent: Option<Env>,
}

#[derive(Debug, Clone)]
pub struct Env (Arc<RwLock<Scope>>);

impl Env {
    pub fn new() -> Self {
        Env(Arc::new(RwLock::new(Scope {
            values: HashMap::new(),
            parent: None,
        })))
    }
    
    pub fn get(&self, name: &str) -> Option<Value> {
        let scope = self.0.read().unwrap();
        
        if let Some(val) = scope.values.get(name) {
            return Some(val.clone());
        }
        
        match &scope.parent {
            Some(parent) => parent.get(name),
            None => None
        }
    }
    
    pub fn enter_scope(&mut self) -> Self {
        Env(
            Arc::new(RwLock::new(Scope { 
                values: HashMap::new(), 
                parent: Some(self.clone())
            }))
        )
    }
    
    pub fn add_variable(&mut self, name: SmolStr, value: Value) -> () {
        self.0.write().unwrap().values.insert(name, value);
    }
    
    pub fn set_variable(&mut self, name: SmolStr, value: Value) -> Option<()> {
        let mut scope = self.0.write().unwrap();
        
        if let Some(_) = scope.values.get(&name) {
            scope.values.insert(name, value);
            return Some(());
        }
        
        match &mut scope.parent {
            Some(parent) => parent.set_variable(name, value),
            None => None
        }
    }
    
    pub fn is_top_level(&self) -> bool {
        self.0.read().unwrap().parent.is_none()
    }
}

#[derive(Debug, Clone)]
pub struct TypeScope {
    variable_types: HashMap<SmolStr, Spanned<TypeInfo>>,
    custom_types: HashMap<SmolStr, Spanned<TypeInfo>>,
    parent: Option<TypeEnv>,
    interface_implementations: HashMap<SmolStr, Vec<Spanned<TypeInfo>>>, //which types implement what
    method_map: HashMap<u32, HashMap<SmolStr, (Spanned<TypeInfo>, Spanned<ir::Expr>)>>, //for getting type signatures of specific method implementations
    static_method_map: HashMap<u32, HashMap<SmolStr, (Spanned<TypeInfo>, Spanned<ir::Expr>)>>,
    
}

#[derive(Debug, Clone)]
pub struct TypeEnv (Arc<RwLock<TypeScope>>);

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv(Arc::new(RwLock::new(TypeScope {
            variable_types: HashMap::new(),
            custom_types: HashMap::new(),
            parent: None,
            interface_implementations: HashMap::new(),
            method_map: HashMap::new(),
            static_method_map: HashMap::new()
        })))
    }
    
    pub fn get_var_type(&self, name: &str) -> Option<Spanned<TypeInfo>> {
        let scope = self.0.read().unwrap();
        
        // println!("Resolving type of {name}");
        // println!("Scope var types: {:?}", scope.variable_types.keys());
        
        if let Some(type_info) = scope.variable_types.get(name) {
            return Some(type_info.clone());
        }
        
        match &scope.parent {
            Some(parent) => parent.get_var_type(name),
            None => None
        }
    }
    
    pub fn resolve_type(&self, type_name: &str) -> Option<Spanned<TypeInfo>> {
        let scope = self.0.read().unwrap();
        
        if let Some(type_info) = scope.custom_types.get(type_name) {
            return Some(type_info.clone());
        }
        
        match &scope.parent {
            Some(parent) => parent.resolve_type(type_name),
            None => None
        }
    }
    
    pub fn enter_scope(&mut self) -> Self {
        TypeEnv(
            Arc::new(RwLock::new(TypeScope { 
                variable_types: HashMap::new(), 
                custom_types: HashMap::new(),
                interface_implementations: HashMap::new(), 
                method_map: HashMap::new(),
                parent: Some(self.clone()),
                static_method_map: HashMap::new()
            }))
        )
    }
    
    pub fn add_var_type(&mut self, name: SmolStr, type_info: Spanned<TypeInfo>) -> () {
        self.0.write().unwrap().variable_types.insert(name, type_info);
    }
    
    pub fn add_custom_type(&mut self, name: SmolStr, type_info: Spanned<TypeInfo>) -> () {
        self.0.write().unwrap().custom_types.insert(name, type_info);
    }
    
    pub fn add_interface_impl(&mut self, iname: SmolStr, type_info: Spanned<TypeInfo>) -> () {
        let mut scope = self.0.write().unwrap();

        match scope.interface_implementations.get_mut(&iname) {
            Some(entry) => {
                entry.push(type_info);
            },
            None => {
                scope.interface_implementations.insert(iname, vec![type_info]);
            },
        }
    }

    pub fn is_interface_implemented(&mut self, iname: SmolStr, type_info: Spanned<TypeInfo>) -> bool {
        let mut current_env = Some(self.clone());

        while let Some(env) = current_env {
            let scope = env.0.read().unwrap();

            if let Some(impls) = scope.interface_implementations.get(&iname) {
                for ti in impls {
                    if ti.inner == type_info.inner {
                        return true;
                    }
                }
            }

            current_env = scope.parent.clone();
        }

        false
    }
    
    pub fn insert_method(&mut self, type_id: u32, method: (SmolStr, (Spanned<TypeInfo>, Spanned<ir::Expr>))) {
        let mut scope = self.0.write().unwrap();
        scope.method_map.entry(type_id).or_default().insert(method.0, method.1);
    }

    pub fn get_method(&self, id: u32, name: &str) -> Option<(Spanned<TypeInfo>, Spanned<ir::Expr>)> {
        let mut current = Some(self.clone());
        while let Some(env) = current {
            let parent;
            let method = {
                let scope = env.0.read().unwrap();
                parent = scope.parent.clone();
                scope.method_map
                    .get(&id)
                    .and_then(|m| m.get(name).cloned())
            };
        
            if let Some(m) = method {
                return Some(m);
            }
        
            current = parent;
        }

        None
    }
    
    pub fn insert_static_method(&mut self, type_id: u32, method: (SmolStr, (Spanned<TypeInfo>, Spanned<ir::Expr>))) {
        let mut scope = self.0.write().unwrap();
        scope.static_method_map.entry(type_id).or_default().insert(method.0, method.1);
    }

    pub fn get_static_method(&self, id: u32, name: &str) -> Option<(Spanned<TypeInfo>, Spanned<ir::Expr>)> {
        let mut current = Some(self.clone());
        while let Some(env) = current {
            let parent;
            let method = {
                let scope = env.0.read().unwrap();
                parent = scope.parent.clone();
                scope.static_method_map
                    .get(&id)
                    .and_then(|m| m.get(name).cloned())
            };
        
            if let Some(m) = method {
                return Some(m);
            }
        
            current = parent;
        }

        None
    }
    
    pub fn is_top_level(&self) -> bool {
        self.0.read().unwrap().parent.is_none()
    }
}

static PROCESS_START: OnceLock<Instant> = OnceLock::new();

pub fn create_default_env() -> (Env, TypeEnv) {
    const BUILTINS_PATH: &str = "std/builtins";
    
    register_fun(BUILTINS_PATH, "print", |args| {
        let mut i = 0;
        let length = args.len();
        for a in args {
            print!("{a}");
            i += 1;
            if i < length {print!(", ")}
        }
        println!();
        Ok(ControlFlow::Value(Value::Void))
    });
    
    register_fun(BUILTINS_PATH, "clock", |_| {
        let instant = PROCESS_START.get_or_init(Instant::now);
        Ok(ControlFlow::Value(Value::Int(instant.elapsed().as_nanos() as i64)))
    });
    
    register_fun(BUILTINS_PATH, "len", |obj| {
        if obj.len() != 1 {
            return Err(Spanned {
                inner: format!("Expected 1 argument, but got {}", obj.len()).into(),
                span: Span::from(0..0)
            })
        }
        match obj.first().unwrap() {
            Value::Array(a) => Ok(ControlFlow::Value(Value::Int(a.len() as i64))),
            Value::String(s) => Ok(ControlFlow::Value(Value::Int(s.len() as i64))),
            v => Err(Spanned {
                inner: format!("Cannot measure length of {v}").into(),
                span: Span::from(0..0)
            })
        }
    });
        
    register_fun(BUILTINS_PATH, "readfile", |obj| {
        if obj.len() != 1 {
            return Err(Spanned {
                inner: format!("Expected 1 argument, but got {}", obj.len()).into(),
                span: Span::from(0..0)
            })
        }
        match obj.first().unwrap() {
            Value::String(s) => {
                let result = fs::read_to_string(s);
                match result { 
                    Ok(content) => {
                        Ok(ControlFlow::Value(Value::EnumVariant{
                            enum_name: "Result".into(),
                            variant: "Ok".into(),
                            value: Box::new(Value::String(content.into()))
                        }))
                    },
                    Err(e) => {
                        let mut map = HashMap::new();
                        map.insert("message".into(), Value::String(e.to_smolstr()));
                        Ok(ControlFlow::Value(Value::EnumVariant{
                            enum_name: "Result".into(),
                            variant: "Err".into(),
                            value: Box::new(Value::Record(map))
                        }))
                    }
                }
            },
            v => Err(Spanned {
                inner: format!("{v} is not a valid path").into(),
                span: Span::from(0..0)
            })
        }
    });
 
    register_fun(BUILTINS_PATH, "writefile", |args| {
        if args.len() != 2 {
            return Err(Spanned {
                inner: format!("Expected 2 arguments, got {}", args.len()).into(),
                span: Span::from(0..0)
            })
        }
        match (args.first().unwrap(), args.last().unwrap()) {
            (Value::String(path), Value::String(content)) => {
                match fs::write(path, content) {
                    Ok(_) => {
                        Ok(ControlFlow::Value(Value::EnumVariant{
                            enum_name: "Result".into(),
                            variant: "Ok".into(),
                            value: Box::new(Value::Void)
                        }))
                    },
                    Err(e) => {
                        let mut map = HashMap::new();
                        map.insert("message".into(), Value::String(e.to_smolstr()));
                        Ok(ControlFlow::Value(Value::EnumVariant{
                            enum_name: "Result".into(),
                            variant: "Err".into(),
                            value: Box::new(Value::Record(map))
                        }))
                    }
                }
            },
            (Value::String(_), v) | (v, Value::String(_)) => Err(Spanned {
                inner: format!("Expected a string, got {v} instead").into(),
                span: Span::from(0..0)
            }),
            (v1, v2) => Err(Spanned {
                inner: format!("Incorrect arguments provided {v1} and {v2}, expected strings").into(),
                span: Span::from(0..0)
            })
        }
    });
    
    let registry = GlobalRegistry;
    match eval_import(BUILTINS_PATH, &registry) {
        Ok(_) => (),
        Err(e) => {
            build_report(e, &fs::read_to_string(BUILTINS_PATH).unwrap());
        },
    };
    get_module_envs(&registry, BUILTINS_PATH).unwrap()
    
    // type_env.add_var_type("debug".to_string(), Spanned { 
    //     inner: TypeInfo::Fun { 
    //         args: vec![("args".to_string(), spanned(TypeInfo::Any))], 
    //         return_type: Box::new(spanned(TypeInfo::Void))
    //     },
    //     span: SimpleSpan::from(0..0)
    // });
    // env.add_variable("debug".to_string(), Value::NativeFun(ir::NativeFun { 
    //     name: "debug".to_string(),
    //     max_args: None,
    //     function: Arc::new(Box::new(
    //         |args: Vec<Value>| {
    //             let length = args.len();
    //             let mut i = 0;
    //             for a in args {
    //                 print!("{a:?}");
    //                 i += 1;
    //                 if i < length {
    //                     print!(", ")
    //                 }
    //             };
    //             println!();
    //             Ok(Value::Void)
    //         }
    //     ))
    // }));
    
    // type_env.add_var_type("str".to_string(), Spanned { 
    //     inner: TypeInfo::Fun {
    //         args: vec![("value".to_string(), spanned(TypeInfo::Any))],
    //         return_type: Box::new(spanned(TypeInfo::String)) 
    //     }, 
    //     span: SimpleSpan::from(0..0)
    // });
    // env.add_variable("str".to_string(), Value::NativeFun(ir::NativeFun { 
    //     name: "str".to_string(),
    //     max_args: Some(1),
    //     function: Arc::new(Box::new(
    //         |args: Vec<Value>| {
    //             let mut result = None;
    //             for a in args {
    //                 result = Some(a.to_string());
    //             };
    //             match result {
    //                 Some(s) => Ok(Value::String(s)),
    //                 None => Err("No arguments provided to \"str\"".to_string()),
    //             }
    //         }
    //     ))
    // }));
    
    // type_env.add_var_type("input".to_string(), Spanned { 
    //     inner: TypeInfo::Fun { 
    //         args: Vec::new(),
    //         return_type: Box::new(spanned(TypeInfo::String)) 
    //     },
    //     span: SimpleSpan::from(0..0)
    // });
    // env.add_variable("input".to_string(), Value::NativeFun(ir::NativeFun { 
    //     name: "input".to_string(),
    //     max_args: Some(0),
    //     function: Arc::new(Box::new(
    //         |_| {
    //             let mut buffer = String::new();
    //             let input_string = std::io::stdin().read_line(&mut buffer);
                
    //             buffer.pop(); // \n gets added at the ebd of the buffer, so we need to pop it
                
    //             match input_string {
    //                 Ok(_) => Ok(Value::String(buffer)),
    //                 Err(_) => Err("Input failed".to_string()),
    //             }
    //         }
    //     ))
    // }));
    
    // type_env.add_var_type("len".to_string(), Spanned { 
    //     inner: TypeInfo::Fun { 
    //         args: vec![("value".to_string(), spanned(TypeInfo::Any))],
    //         return_type: Box::new(spanned(TypeInfo::String)) 
    //     },
    //     span: SimpleSpan::from(0..0)
    // });
    // env.add_variable("len".to_string(), Value::NativeFun(ir::NativeFun { 
    //     name: "len".to_string(),
    //     max_args: Some(1),
    //     function: Arc::new(Box::new(
    //         |args: Vec<Value>| {
    //             let mut result = None;
    //             for a in args {
    //                 match a {
    //                     Value::String(s) => result = Some(s.len()),
    //                     Value::Record(hash_map) => result = Some(hash_map.len()),
    //                     v => return Err(format!("Cannot measure length of {v:?}"))
    //                 }
    //             };
    //             match result {
    //                 Some(l) => Ok(Value::Int(l as i64)),
    //                 None => Err("No arguments provided to \"str\"".to_string()),
    //             }
    //         }
    //     ))
    // }));
    
    // type_env.add_var_type("read".to_string(), Spanned {
    //     inner: TypeInfo::Fun { 
    //         args: vec![("path".to_string(), spanned(TypeInfo::String))],
    //         return_type: Box::new(spanned(TypeInfo::String)) 
    //     },
    //     span: SimpleSpan::from(0..0)
    // });
    // env.add_variable("read".to_string(), Value::NativeFun(ir::NativeFun { 
    //     name: "read".to_string(),
    //     max_args: Some(1),
    //     function: Arc::new(Box::new(
    //         |args: Vec<Value>| {
    //             let mut result = Err(Error::new(std::io::ErrorKind::InvalidData, "File IO failed"));
                
    //             if args.len() < 1 {
    //                 return Err("No arguments provided".to_string());
    //             }
                
    //             for a in args {
    //                 match a {
    //                     Value::String(s) => result = fs::read_to_string(s),
    //                     _ => return Err(format!(""))
    //                 }
    //             };
                
    //             match result {
    //                 Ok(s) => Ok(Value::String(s)),
    //                 Err(e) => Err(e.to_string()),
    //             }
    //         }
    //     ))
    // }));
    
    // type_env.add_var_type("write".to_string(), Spanned {
    //     inner: TypeInfo::Fun { 
    //         args: vec![
    //             ("path".to_string(), spanned(TypeInfo::String)),
    //             ("content".to_string(), spanned(TypeInfo::String))
    //         ],
    //         return_type: Box::new(spanned(TypeInfo::String)) 
    //     },
    //     span: SimpleSpan::from(0..0)
    // });
    // env.add_variable("write".to_string(), Value::NativeFun(ir::NativeFun { 
    //     name: "write".to_string(),
    //     max_args: Some(2),
    //     function: Arc::new(Box::new(
    //         |args: Vec<Value>| {
    //             if args.len() < 2 {
    //                 return Err(format!("Expected 2 arguments, but got {}", args.len()));
    //             }
                
    //             let path = match args.get(0) {
    //                 Some(v) => match v {
    //                     Value::String(s) => s,
    //                     a => return Err(format!("{a} is not a valid path"))
    //                 },
    //                 None => return Err("No path provided".to_string()),
    //             };
                
    //             let content = match args.get(1) {
    //                 Some(v) => match v {
    //                     Value::String(s) => s,
    //                     a => return Err(format!("{a} is not valid content"))
    //                 },
    //                 None => return Err("No content provided".to_string()),
    //             };
                
    //             let result = fs::write(path, content);
                
    //             match result {
    //                 Ok(_) => Ok(Value::Bool(true)),
    //                 Err(e) => Err(e.to_string()),
    //             }
    //         }
    //     ))
    // }));
    
    // (env, type_env)
}

pub static DEFAULT_ENVS: OnceLock<(Env, TypeEnv)> = OnceLock::new();
