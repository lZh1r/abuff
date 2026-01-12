use std::{collections::HashMap};

use crate::env::Env;

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    Var(String),
    Binary {left: Box<Expr>, operation: Operation, right: Box<Expr>},
    Block(Vec<Statement>, Option<Box<Expr>>),
    Fun {param: String, body: Box<Expr>},
    Call {fun: Box<Expr>, arg: Box<Expr>}
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let{name: String, expr: Expr},
    Expr(Expr)
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64), 
    Float(f64),
    Bool(bool),
    Record(HashMap<String, Value>), 
    Closure { param: String, body: Box<Expr>, env: Env },
    Void
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Devide
}