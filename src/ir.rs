use std::{collections::HashMap};

use crate::{ast::UnaryOp, env::Env};

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(String),
    Var(String),
    Binary {left: Box<Expr>, operation: Operation, right: Box<Expr>},
    Block(Vec<Statement>, Option<Box<Expr>>),
    Fun {params: Vec<String>, body: Box<Expr>},
    Call {fun: Box<Expr>, args: Vec<Expr>},
    Record(Vec<(String, Expr)>),
    Get(Box<Expr>, String),
    Assign {target: Box<Expr>, value: Box<Expr>},
    Unary(UnaryOp, Box<Expr>),
    If {condition: Box<Expr>, body: Box<Expr>, else_block: Option<Box<Expr>>},
    While {condition: Box<Expr>, body: Box<Expr>}
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let{name: String, expr: Expr},
    Expr(Expr)
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Eq,
    NotEq,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    And,
    Or
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64), 
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Record(HashMap<String, Value>), 
    Closure { params: Vec<String>, body: Box<Expr>, env: Env },
    Null,
    Void
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Record(a), Value::Record(b)) => a == b, 
            (Value::Closure { .. }, Value::Closure { .. }) => false,
            _ => false
        }
    }
}

impl Value {
    pub fn add(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
            (Value::Char(a), Value::Char(b)) => Ok(Value::String(format!("{}{}", a, b))),
            (a,b) => Err(format!("Cannot add {a:?} and {b:?}"))
        }
    }
    pub fn subtract(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (a,b) => Err(format!("Cannot subtract {a:?} from {b:?}"))
        }
    }
    pub fn multiply(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::String(a), Value::Int(b)) => Ok(Value::String(a.repeat(b as usize))),
            (Value::Int(a), Value::String(b)) => Ok(Value::String(b.repeat(a as usize))),
            (Value::Char(a), Value::Int(b)) => Ok(Value::String(a.to_string().repeat(b as usize))),
            (Value::Int(a), Value::Char(b)) => Ok(Value::String(b.to_string().repeat(a as usize))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (a,b) => Err(format!("Cannot multiply {a:?} by {b:?}"))
        }
    }
    pub fn divide(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 {return Err("Cannot devide by 0".to_string());}
                Ok(Value::Int(a / b))
            },
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 {return Err("Cannot devide by 0".to_string());}
                Ok(Value::Float(a / b))
            },
            (a,b) => Err(format!("Cannot devide {a:?} by {b:?}"))
        }
    }
    pub fn modulo(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a % b)),
            (a,b) => Err(format!("Cannot perform {a:?} mod {b:?}"))
        }
    }
    pub fn greater_than(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            (a,b) => Err(format!("Cannot compare {a:?} to {b:?}"))
        }
    }
    pub fn less_than(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (a,b) => Err(format!("Cannot compare {a:?} to {b:?}"))
        }
    }
    pub fn greater_than_eq(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            (a,b) => Err(format!("Cannot compare {a:?} to {b:?}"))
        }
    }
    pub fn less_than_eq(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (a,b) => Err(format!("Cannot compare {a:?} to {b:?}"))
        }
    }
    pub fn logic_and(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
            (a, b) => Err(format!("Cannot perform {a:?} AND {b:?}"))
        }
    }
    pub fn logic_or(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
            (a, b) => Err(format!("Cannot perform {a:?} OR {b:?}"))
        }
    }
}