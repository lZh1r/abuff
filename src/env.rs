use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ir::Value;

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