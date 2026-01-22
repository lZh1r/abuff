use chumsky::prelude::SimpleSpan;
use chumsky::span::{Spanned as ChumskySpanned};

pub type Span = SimpleSpan;
pub type Spanned<T> = ChumskySpanned<T, Span>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(String),
    Var(String),
    Binary {left: Box<Spanned<Expr>>, operation: Operation, right: Box<Spanned<Expr>>},
    Block(Vec<Spanned<Statement>>, Option<Box<Spanned<Expr>>>),
    Fun {params: Vec<(String, Spanned<TypeInfo>)>, body: Box<Spanned<Expr>>, return_type: Option<Spanned<TypeInfo>>},
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

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {name: String, expr: Spanned<Expr>, type_info: Option<Spanned<TypeInfo>>},
    TypeDef {name: String, type_info: Spanned<TypeInfo>},
    Expr(Spanned<Expr>),
    Fun {name: String, params: Vec<(String, Spanned<TypeInfo>)>, body: Spanned<Expr>, return_type: Option<Spanned<TypeInfo>>},
    Import {symbols: Vec<(Spanned<String>, Option<String>, bool)>, path: Spanned<String>},
    Export(Box<Spanned<Statement>>)
}

//TODO: nullish coalescence
#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Int,
    Float,
    String,
    Char,
    Bool,
    Void,
    Null,
    Unknown,
    Any,
    Fun {args: Vec<(String, Spanned<TypeInfo>)>, return_type: Box<Spanned<TypeInfo>>},
    Record(Vec<(String, Spanned<TypeInfo>)>),
    Custom(String)
}

// Custom PartialEq that ignores spans when comparing TypeInfo
impl PartialEq for TypeInfo {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeInfo::Int, TypeInfo::Int) => true,
            (TypeInfo::Float, TypeInfo::Float) => true,
            (TypeInfo::String, TypeInfo::String) => true,
            (TypeInfo::Char, TypeInfo::Char) => true,
            (TypeInfo::Bool, TypeInfo::Bool) => true,
            (TypeInfo::Void, TypeInfo::Void) => true,
            (TypeInfo::Null, TypeInfo::Null) => true,
            (TypeInfo::Unknown, TypeInfo::Unknown) => true,
            (TypeInfo::Any, TypeInfo::Any) => true,
            (TypeInfo::Custom(a), TypeInfo::Custom(b)) => a == b,
            (TypeInfo::Fun { args: args_a, return_type: ret_a }, TypeInfo::Fun { args: args_b, return_type: ret_b }) => {
                // Compare return types (ignoring span)
                if ret_a.inner != ret_b.inner {
                    return false;
                }
                // Compare args length
                if args_a.len() != args_b.len() {
                    return false;
                }
                // Compare each arg (name and type, ignoring span)
                for ((name_a, type_a), (name_b, type_b)) in args_a.iter().zip(args_b.iter()) {
                    if name_a != name_b || type_a.inner != type_b.inner {
                        return false;
                    }
                }
                true
            },
            (TypeInfo::Record(fields_a), TypeInfo::Record(fields_b)) => {
                // Compare fields length
                if fields_a.len() != fields_b.len() {
                    return false;
                }
                // Compare each field (name and type, ignoring span)
                for ((name_a, type_a), (name_b, type_b)) in fields_a.iter().zip(fields_b.iter()) {
                    if name_a != name_b || type_a.inner != type_b.inner {
                        return false;
                    }
                }
                true
            },
            _ => false,
        }
    }
}