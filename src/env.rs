use std::{collections::{HashMap, HashSet}, fs, sync::{Arc, OnceLock, RwLock}, time::Instant};

use smol_str::{SmolStr, StrExt, ToSmolStr, format_smolstr};

use crate::{ast::{Span, Spanned, TypeInfo, TypeKind}, error::build_report, ir::{self, ControlFlow, Value}, module::{GlobalRegistry, eval_import, get_module_envs}, native::{register_fun, register_type}};

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
    interface_implementations: HashMap<SmolStr, HashSet<u32>>, //interface name - typesthat implement it
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
    
    pub fn add_interface_impl(&mut self, iname: SmolStr, id: u32) -> () {
        let mut scope = self.0.write().unwrap();

        match scope.interface_implementations.get_mut(&iname) {
            Some(entry) => {
                entry.insert(id);
            },
            None => {
                let mut set = HashSet::new();
                set.insert(id);
                scope.interface_implementations.insert(iname, set);
            },
        }
    }

    pub fn is_interface_implemented(&mut self, iname: SmolStr, id: u32) -> bool {
        let mut current_env = Some(self.clone());

        while let Some(env) = current_env {
            let scope = env.0.read().unwrap();

            if let Some(impls) = scope.interface_implementations.get(&iname) {
                if impls.contains(&id) {
                    return true
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
    
    register_type(BUILTINS_PATH, "Array", TypeInfo::new_with_id(
        TypeKind::TypeClosure { 
            params: vec![Spanned{inner: "T".into(), span: Span::from(0..0)}],
            body: Box::new(Spanned {
                inner: TypeInfo::array(
                    Spanned { 
                        inner: TypeInfo::new(TypeKind::GenericParam("T".into())), 
                        span: Span::from(0..0)
                    }
                ),
                span: Span::from(0..0)
            })
        }, 
        10
    ));
    
    register_type(BUILTINS_PATH, "String", TypeInfo::string());
    register_type(BUILTINS_PATH, "Int", TypeInfo::int());
    register_type(BUILTINS_PATH, "Float", TypeInfo::float());
    register_type(BUILTINS_PATH, "Char", TypeInfo::char());
    
    register_fun(BUILTINS_PATH, "print", |(args, _)| {
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
    
    register_fun(BUILTINS_PATH, "len", |(_, this)| {
        match *this.unwrap() {
            Value::Array(a) => Ok(ControlFlow::Value(Value::Int(a.len() as i64))),
            Value::String(s) => Ok(ControlFlow::Value(Value::Int(s.len() as i64))),
            v => Err(Spanned {
                inner: format!("Cannot measure length of {v}").into(),
                span: Span::from(0..0)
            })
        }
    });
        
    register_fun(BUILTINS_PATH, "readfile", |(obj, _)| {
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
 
    register_fun(BUILTINS_PATH, "writefile", |(args, _)| {
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
    
    register_fun(BUILTINS_PATH, "toString", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::Char(char) => {
                Ok(ControlFlow::Value(Value::String(char.to_smolstr())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a char"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "get", |(args, this)| {
        if args.len() != 1 {
            return Err(Spanned {
                inner: format!("Expected 1 argument, got {}", args.len()).into(),
                span: Span::from(0..0)
            })
        }
        let index = match args.first().unwrap() {
            Value::Int(i) => i,
            _ => panic!()
        };
        let this = this.unwrap();
        match *this {
            Value::String(s) => {
                match s.chars().nth(*index as usize) {
                    Some(c) => Ok(
                        ControlFlow::Value(
                            Value::EnumVariant { 
                                enum_name: "Option".into(),
                                variant: "Some".into(),
                                value: Box::new(Value::Char(c))
                            }
                        )
                    ),
                    None => Ok(
                        ControlFlow::Value(
                            Value::EnumVariant { 
                                enum_name: "Option".into(),
                                variant: "None".into(),
                                value: Box::new(Value::Void)
                            }
                        )
                    )
                }
            },
            Value::Array(a) => {
                match a.get(*index as usize) {
                    Some(v) => Ok(
                        ControlFlow::Value(
                            Value::EnumVariant { 
                                enum_name: "Option".into(),
                                variant: "Some".into(),
                                value: Box::new(v.clone())
                            }
                        )
                    ),
                    None => Ok(
                        ControlFlow::Value(
                            Value::EnumVariant { 
                                enum_name: "Option".into(),
                                variant: "None".into(),
                                value: Box::new(Value::Void)
                            }
                        )
                    ),
                }
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a string"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "sort", |(_, this)| {
        let val = this.unwrap();
        match *val {
            Value::Array(ref a) => {
                if a.len() == 0 {
                    return Ok(ControlFlow::Value(*val.clone()))
                }
                let mut array = a.clone();
                array.sort_by(|a, b| {
                    match (a,b) {
                        (Value::Int(a), Value::Int(b)) => {
                            a.cmp(b)
                        },
                        (Value::U128(a), Value::U128(b)) => {
                            a.cmp(b)
                        },
                        (Value::Float(a), Value::Float(b)) => {
                            a.total_cmp(b)
                        },
                        (Value::String(a), Value::String(b)) => {
                            a.cmp(b)
                        },
                        _ => panic!()
                    }
                });
                
                return Ok(ControlFlow::Value(Value::Array(array)))
            },
            v => Err(Spanned {
                inner: format!("Cannot sort {v}").into(),
                span: Span::from(0..0)
            })
        }
    });

    register_fun(BUILTINS_PATH, "reverse", |(_, this)| {
        let val = this.unwrap();
        match *val {
            Value::Array(ref a) => {
                if a.len() == 0 {
                    return Ok(ControlFlow::Value(*val.clone()))
                }
                let mut array = a.clone();
                array.reverse();
                return Ok(ControlFlow::Value(Value::Array(array)))
            },
            Value::String(s) => {
                return Ok(ControlFlow::Value(Value::String(s.chars().rev().collect::<SmolStr>())))
            },
            v => Err(Spanned {
                inner: format!("Cannot sort {v}").into(),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "startsWith", |(args, this)| {
        if args.len() != 1 {
            return Err(Spanned {
                inner: format!("Expected 1 argument, got {}", args.len()).into(),
                span: Span::from(0..0)
            })
        }
        let this = this.unwrap();
        match *this {
            Value::String(string) => {
                match args.first().unwrap() {
                    Value::Char(c) => {
                        Ok(ControlFlow::Value(Value::Bool(string.starts_with(*c))))
                    },
                    Value::String(s) => {
                        Ok(ControlFlow::Value(Value::Bool(string.starts_with(s.as_str()))))
                    },
                    _ => panic!()
                }
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a string"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "endsWith", |(args, this)| {
        if args.len() != 1 {
            return Err(Spanned {
                inner: format!("Expected 1 argument, got {}", args.len()).into(),
                span: Span::from(0..0)
            })
        }
        let this = this.unwrap();
        match *this {
            Value::String(string) => {
                match args.first().unwrap() {
                    Value::Char(c) => {
                        Ok(ControlFlow::Value(Value::Bool(string.ends_with(*c))))
                    },
                    Value::String(s) => {
                        Ok(ControlFlow::Value(Value::Bool(string.ends_with(s.as_str()))))
                    },
                    _ => panic!()
                }
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a string"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "toUpperCase", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::String(string) => {
                Ok(ControlFlow::Value(Value::String(string.to_uppercase_smolstr())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a string"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "toLowerCase", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::String(string) => {
                Ok(ControlFlow::Value(Value::String(string.to_lowercase_smolstr())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a string"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "trim", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::String(string) => {
                Ok(ControlFlow::Value(Value::String(string.trim().into())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a string"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "isAlphabetic", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::Char(char) => {
                Ok(ControlFlow::Value(Value::Bool(char.is_alphabetic())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a char"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "isNumeric", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::Char(char) => {
                Ok(ControlFlow::Value(Value::Bool(char.is_numeric())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a char"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "isAlphanumeric", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::Char(char) => {
                Ok(ControlFlow::Value(Value::Bool(char.is_alphanumeric())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a char"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "isWhitespace", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::Char(char) => {
                Ok(ControlFlow::Value(Value::Bool(char.is_whitespace())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a char"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "isUpperCase", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::Char(char) => {
                Ok(ControlFlow::Value(Value::Bool(char.is_uppercase())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a char"),
                span: Span::from(0..0)
            })
        }
    });
    
    register_fun(BUILTINS_PATH, "isLowerCase", |(_, this)| {
        let this = this.unwrap();
        match *this {
            Value::Char(char) => {
                Ok(ControlFlow::Value(Value::Bool(char.is_lowercase())))
            },
            _ => Err(Spanned {
                inner: format_smolstr!("{this:?} is not a char"),
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
}

pub static DEFAULT_ENVS: OnceLock<(Env, TypeEnv)> = OnceLock::new();
