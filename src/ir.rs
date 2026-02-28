use core::fmt;
use std::{collections::HashMap};

use smol_str::SmolStr;

use crate::{ast::{Operation, Spanned, UnaryOp}, env::Env, native::NativeFun};

#[allow(unpredictable_function_pointer_comparisons)]
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(SmolStr),
    Char(char),
    Var(SmolStr),
    Null,
    Void,
    Array(Vec<Spanned<Expr>>),
    Index(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Binary {left: Box<Spanned<Expr>>, operation: Operation, right: Box<Spanned<Expr>>},
    Block(Vec<Spanned<Statement>>, Option<Box<Spanned<Expr>>>),
    Fun {params: Vec<(bool, SmolStr)>, body: Box<Spanned<Expr>>},
    //helper for method calls
    Method {this: Box<Spanned<Expr>>, fun: Box<Spanned<Expr>>},
    //helper for native method calls
    NativeMethod {this: Box<Spanned<Expr>>, name: SmolStr, path: SmolStr, native_fun: NativeFun},
    //needed to store native function information as an expression
    NativeFun {name: SmolStr, path: SmolStr, native_fun: NativeFun},
    Call {fun: Box<Spanned<Expr>>, args: Vec<Spanned<Expr>>},
    Record(HashMap<SmolStr, Spanned<Expr>>),
    Get(Box<Spanned<Expr>>, SmolStr),
    Assign {target: Box<Spanned<Expr>>, value: Box<Spanned<Expr>>},
    Unary(UnaryOp, Box<Spanned<Expr>>),
    If {condition: Box<Spanned<Expr>>, body: Box<Spanned<Expr>>, else_block: Option<Box<Spanned<Expr>>>},
    While {condition: Box<Spanned<Expr>>, body: Box<Spanned<Expr>>},
    Break,
    Continue,
    Return(Box<Spanned<Expr>>),
    Panic(Option<Box<Spanned<Expr>>>),
    EnumConstructor {enum_name: SmolStr, variant: SmolStr, value: Box<Spanned<Expr>>},
    Match {target: Box<Spanned<Expr>>, branches: Vec<(Spanned<MatchArm>, Spanned<Expr>)>}
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchArm {
    Conditional {alias: SmolStr, condition: Spanned<Expr>},
    Value(Spanned<Expr>),
    Default(SmolStr),
    EnumConstructor {enum_name: SmolStr, variant: SmolStr, alias: SmolStr}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let{name: SmolStr, expr: Spanned<Expr>},
    Expr(Spanned<Expr>),
    NativeFun(SmolStr),
    Import {symbols: Vec<(Spanned<SmolStr>, Option<SmolStr>)>, path: Spanned<SmolStr>},
    Export(Box<Spanned<Statement>>)
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Value(Value),
    Return(Value),
    Panic(Option<SmolStr>),
    Break,
    Continue
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64), 
    Float(f64),
    U128(u128),
    Bool(bool),
    String(SmolStr),
    Char(char),
    Array(Vec<Value>),
    Record(HashMap<SmolStr, Value>), 
    Closure { params: Vec<(bool, SmolStr)>, body: Box<Spanned<Expr>>, env: Env },
    NativeFun {path: SmolStr, name: SmolStr, pointer: NativeFun, this: Option<Box<Value>>},
    Null,
    Void,
    EnumVariant {enum_name: SmolStr, variant: SmolStr, value: Box<Value>}
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
            Value::NativeFun {path, name, pointer: _, this: _} => write!(f, "native function \"{name}\" from \"{path}\""),
            Value::Null => write!(f, "null"),
            Value::Void => write!(f, "void"),
            Value::Array(values) => {
                write!(f, "[")?;
                for (i, v) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            },
            Value::EnumVariant { enum_name, variant, value } => write!(f, "{enum_name}.{variant}({value})"),
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
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::Null, Value::Null) => true,
            (Value::Closure { .. }, Value::Closure { .. }) => false,
            _ => false
        }
    }
}

impl Value {
    pub fn add(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b).into())),
            (Value::Char(a), Value::Char(b)) => Ok(Value::String(format!("{}{}", a, b).into())),
            (a,b) => Err(format!("Cannot add {a:?} and {b:?}").into())
        }
    }
    pub fn subtract(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (a,b) => Err(format!("Cannot subtract {a:?} from {b:?}").into())
        }
    }
    pub fn multiply(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a * b)),
            (Value::String(a), Value::Int(b)) => Ok(Value::String(a.repeat(b as usize).into())),
            (Value::Int(a), Value::String(b)) => Ok(Value::String(b.repeat(a as usize).into())),
            (Value::Char(a), Value::Int(b)) => Ok(Value::String(a.to_string().repeat(b as usize).into())),
            (Value::Int(a), Value::Char(b)) => Ok(Value::String(b.to_string().repeat(a as usize).into())),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (a,b) => Err(format!("Cannot multiply {a:?} by {b:?}").into())
        }
    }
    pub fn divide(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 {return Err("Cannot devide by 0".into());}
                Ok(Value::Int(a / b))
            },
            (Value::U128(a), Value::U128(b)) => {
                if b == 0 {return Err("Cannot devide by 0".into());}
                Ok(Value::U128(a / b))
            },
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 {return Err("Cannot devide by 0".into());}
                Ok(Value::Float(a / b))
            },
            (a,b) => Err(format!("Cannot devide {a:?} by {b:?}").into())
        }
    }
    pub fn modulo(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a % b)),
            (a,b) => Err(format!("Cannot perform {a:?} mod {b:?}").into())
        }
    }
    pub fn greater_than(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            (a,b) => Err(format!("Cannot compare {a:?} to {b:?}").into())
        }
    }
    pub fn less_than(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (a,b) => Err(format!("Cannot compare {a:?} to {b:?}").into())
        }
    }
    pub fn greater_than_eq(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            (a,b) => Err(format!("Cannot compare {a:?} to {b:?}").into())
        }
    }
    pub fn less_than_eq(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (a,b) => Err(format!("Cannot compare {a:?} to {b:?}").into())
        }
    }
    pub fn logic_and(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
            (a, b) => Err(format!("Cannot perform {a:?} AND {b:?}").into())
        }
    }
    pub fn logic_or(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
            (a, b) => Err(format!("Cannot perform {a:?} OR {b:?}").into())
        }
    }
    pub fn nullish_coalescing(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Null, o) => Ok(o),
            (v, _) => Ok(v)
        }
    }
    pub fn bitwise_and(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a & b)),
            (a, b) => Err(format!("Cannot perform {a:?} & {b:?}").into())
        }
    }
    pub fn bitwise_or(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a | b)),
            (a, b) => Err(format!("Cannot perform {a:?} | {b:?}").into())
        }
    }
    pub fn bitwise_xor(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a ^ b)),
            (a, b) => Err(format!("Cannot perform {a:?} ^ {b:?}").into())
        }
    }
    pub fn bitwise_left_shift(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a << b)),
            (a, b) => Err(format!("Cannot perform {a:?} << {b:?}").into())
        }
    }
    pub fn bitwise_right_shift(self, other: Value) -> Result<Value, SmolStr> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),
            (Value::U128(a), Value::U128(b)) => Ok(Value::U128(a >> b)),
            (a, b) => Err(format!("Cannot perform {a:?} >> {b:?}").into())
        }
    }
}