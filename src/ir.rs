use core::fmt;
use std::{collections::HashMap};

use crate::{ast::{Spanned, UnaryOp}, env::Env, native::NativeFun};

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(String),
    Var(String),
    Array(Vec<Spanned<Expr>>),
    Binary {left: Box<Spanned<Expr>>, operation: Operation, right: Box<Spanned<Expr>>},
    Block(Vec<Spanned<Statement>>, Option<Box<Spanned<Expr>>>),
    Fun {params: Vec<(bool, String)>, body: Box<Spanned<Expr>>},
    Call {fun: Box<Spanned<Expr>>, args: Vec<Spanned<Expr>>},
    Record(Vec<(String, Spanned<Expr>)>),
    Get(Box<Spanned<Expr>>, String),
    Assign {target: Box<Spanned<Expr>>, value: Box<Spanned<Expr>>},
    Unary(UnaryOp, Box<Spanned<Expr>>),
    If {condition: Box<Spanned<Expr>>, body: Box<Spanned<Expr>>, else_block: Option<Box<Spanned<Expr>>>},
    While {condition: Box<Spanned<Expr>>, body: Box<Spanned<Expr>>},
    Break,
    Continue,
    Return(Box<Spanned<Expr>>)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let{name: String, expr: Spanned<Expr>},
    Expr(Spanned<Expr>),
    NativeFun(String),
    Import {symbols: Vec<(Spanned<String>, Option<String>)>, path: Spanned<String>},
    Export(Box<Spanned<Statement>>)
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
pub enum ControlFlow {
    Value(Value),
    Return(Value),
    Break,
    Continue
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64), 
    Float(f64),
    U128(u128),
    Bool(bool),
    String(String),
    Char(char),
    Array(Vec<Value>),
    Record(HashMap<String, Value>), 
    Closure { params: Vec<(bool, String)>, body: Box<Spanned<Expr>>, env: Env },
    NativeFun {path: String, name: String, pointer: NativeFun},
    Null,
    Void
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
            Value::U128(i) => write!(f, "{i}"),
            Value::Float(fl) => write!(f, "{fl}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Char(c) => write!(f, "{c}"),
            Value::Record(hash_map) => {
                let mut entries: Vec<_> = hash_map.iter().collect();
                entries.sort_by_key(|(k, _)| *k);
    
                write!(f, "{{")?;
                for (i, (key, val)) in entries.iter().enumerate() {
                    if i > 0 { 
                        write!(f, ", ")?; 
                    } else {
                        write!(f, " ")?;
                    }
                    write!(f, "{}: {}", key, val)?; 
                }

                if !entries.is_empty() {
                    write!(f, " ")?;
                }
                write!(f, "}}")
            },
            Value::Closure { params: _, body: _, env: _ } => write!(f, "closure"),
            Value::NativeFun {path, name, pointer: _} => write!(f, "native function \"{name}\" from \"{path}\""),
            Value::Null => write!(f, "null"),
            Value::Void => write!(f, "void"),
            Value::Array(values) => {
                write!(f, "[");
                for (i, v) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ");
                    }
                    write!(f, "{v}");
                }
                write!(f, "]")
            },
        }
    }
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
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
            (Value::Char(a), Value::Char(b)) => Ok(Value::String(format!("{}{}", a, b))),
            (a,b) => Err(format!("Cannot add {a:?} and {b:?}"))
        }
    }
    pub fn subtract(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (a,b) => Err(format!("Cannot subtract {a:?} from {b:?}"))
        }
    }
    pub fn multiply(self, other: Value) -> Result<Value, String> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a * b)),
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
            (Value::U128(a), Value::U128(b)) => {
                if b == 0 {return Err("Cannot devide by 0".to_string());}
                Ok(Value::U128(a / b))
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