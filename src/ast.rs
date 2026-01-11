use std::{collections::HashMap};

use crate::Env;

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    Var(String),
    Binary{left: Box<Expr>, operation: Operation, right: Box<Expr>},
    Let{name: String, expr: Box<Expr>, body: Box<Expr>}
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64), 
    Float(f64),
    Bool(bool),
    Record(HashMap<String, Value>), 
    Closure { param: String, body: Box<Expr>, env: Env }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Devide
}