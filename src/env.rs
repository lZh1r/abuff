use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{ast::TypeInfo, ir::Value};

#[derive(Debug, Clone)]
pub struct Scope {
    values: HashMap<String, Value>,
    parent: Option<Env>,
}

#[derive(Debug, Clone)]
pub struct Env (Rc<RefCell<Scope>>);

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(Scope {
            values: HashMap::new(),
            parent: None,
        })))
    }
    
    pub fn get(&self, name: &str) -> Option<Value> {
        let scope = self.0.borrow();
        
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
            Rc::new(RefCell::new(Scope { 
                values: HashMap::new(), 
                parent: Some(self.clone())
            }))
        )
    }
    
    pub fn add_variable(&mut self, name: String, value: Value) -> () {
        self.0.borrow_mut().values.insert(name, value);
    }
}

#[derive(Debug, Clone)]
pub struct TypeScope {
    variable_types: HashMap<String, TypeInfo>,
    custom_types: HashMap<String, TypeInfo>,
    parent: Option<TypeEnv>,
}

#[derive(Debug, Clone)]
pub struct TypeEnv (Rc<RefCell<TypeScope>>);

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv(Rc::new(RefCell::new(TypeScope {
            variable_types: HashMap::new(),
            custom_types: HashMap::new(),
            parent: None,
        })))
    }
    
    pub fn get_var_type(&self, name: &str) -> Option<TypeInfo> {
        let scope = self.0.borrow();
        
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
    
    pub fn resolve_type(&self, type_name: &str) -> Option<TypeInfo> {
        let scope = self.0.borrow();
        
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
            Rc::new(RefCell::new(TypeScope { 
                variable_types: HashMap::new(), 
                custom_types: HashMap::new(),
                parent: Some(self.clone())
            }))
        )
    }
    
    pub fn add_var_type(&mut self, name: String, type_info: TypeInfo) -> () {
        self.0.borrow_mut().variable_types.insert(name, type_info);
    }
    
    pub fn add_custom_type(&mut self, name: String, type_info: TypeInfo) -> () {
        self.0.borrow_mut().custom_types.insert(name, type_info);
    }
}