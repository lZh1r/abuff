use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::{Arc, RwLock}};

use crate::{ast::{Spanned, TypeInfo}, ir::Value};

#[derive(Debug, Clone)]
pub struct Scope {
    values: HashMap<String, Value>,
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
    
    pub fn add_variable(&mut self, name: String, value: Value) -> () {
        self.0.write().unwrap().values.insert(name, value);
    }
    
    pub fn set_variable(&mut self, name: String, value: Value) -> Option<()> {
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
    variable_types: HashMap<String, Spanned<TypeInfo>>,
    custom_types: HashMap<String, Spanned<TypeInfo>>,
    parent: Option<TypeEnv>,
}

#[derive(Debug, Clone)]
pub struct TypeEnv (Arc<RwLock<TypeScope>>);

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv(Arc::new(RwLock::new(TypeScope {
            variable_types: HashMap::new(),
            custom_types: HashMap::new(),
            parent: None,
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
                parent: Some(self.clone())
            }))
        )
    }
    
    pub fn add_var_type(&mut self, name: String, type_info: Spanned<TypeInfo>) -> () {
        self.0.write().unwrap().variable_types.insert(name, type_info);
    }
    
    pub fn add_custom_type(&mut self, name: String, type_info: Spanned<TypeInfo>) -> () {
        self.0.write().unwrap().custom_types.insert(name, type_info);
    }
    
    pub fn is_top_level(&self) -> bool {
        self.0.read().unwrap().parent.is_none()
    }
}