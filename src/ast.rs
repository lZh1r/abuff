use std::{collections::HashMap, fmt::Display, ops::Range};

use crate::env::TypeEnv;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize
}

impl Span {
    pub fn from(range: Range<usize>) -> Self {
        Span { start: range.start, end: range.end }
    }
    pub fn into_range(&self) -> Range<usize> {
        self.start..self.end
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span
}

impl Display for Spanned<String> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(String),
    Void,
    Var(String),
    Array(Vec<Spanned<Expr>>),
    Index(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Binary {left: Box<Spanned<Expr>>, operation: Operation, right: Box<Spanned<Expr>>},
    Block(Vec<Spanned<Statement>>, Option<Box<Spanned<Expr>>>),
    Fun {
        params: Vec<((bool, String), Spanned<TypeInfo>)>,
        body: Box<Spanned<Expr>>,
        return_type: Option<Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<String>>
    },
    Call {fun: Box<Spanned<Expr>>, args: Vec<Spanned<Expr>>, generic_args: Vec<Spanned<TypeInfo>>},
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
    TypeDef {
        name: String, 
        type_info: Spanned<TypeInfo>,
        generic_params: Vec<Spanned<String>>
    },
    Expr(Spanned<Expr>),
    Fun {
        name: String, 
        params: Vec<((bool, String), Spanned<TypeInfo>)>,
        body: Spanned<Expr>,
        return_type: Option<Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<String>>
    },
    NativeFun {
        name: String, 
        params: Vec<((bool, String), Spanned<TypeInfo>)>,
        return_type: Option<Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<String>>
    },
    EnumDef {
        name: String, 
        variants: Vec<(String, Option<Spanned<TypeInfo>>)>,
        generic_params: Vec<Spanned<String>>
    },
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
    Fun {
        params: Vec<((bool, String), Spanned<TypeInfo>)>,
        return_type: Box<Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<String>>
    },
    Record(Vec<(String, Spanned<TypeInfo>)>),
    Custom {name: String, generic_args: Vec<Spanned<TypeInfo>>},
    GenericParam(String),
    Array(Box<Spanned<TypeInfo>>),
    Enum {
        name: String,
        variants: HashMap<String, Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<String>>
    },
    EnumInstance {enum_name: String, variants: HashMap<String, Spanned<TypeInfo>>, generic_args: Vec<Spanned<TypeInfo>>},
    EnumVariant {enum_name: String, variant: String},
    TypeClosure {params: Vec<Spanned<String>>, env: TypeEnv, body: Box<Spanned<TypeInfo>>}
}

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
            (TypeInfo::Unknown, TypeInfo::Unknown) => false,
            (TypeInfo::Array(a1), TypeInfo::Array(a2)) => a1.inner == a2.inner,
            (TypeInfo::Custom {name: name1, generic_args: args1}, TypeInfo::Custom {name: name2, generic_args: args2}) => {
                match name1 == name2 {
                    true => {
                        for (t1,t2) in args1.iter().zip(args2.iter()) {
                            if t1.inner != t2.inner {
                                return false
                            }
                        }
                        true
                    },
                    false => false,
                }
            },
            (
                TypeInfo::Fun { params: args_a, return_type: ret_a, generic_params: g_a },
                TypeInfo::Fun { params: args_b, return_type: ret_b, generic_params: g_b }
            ) => {
                if ret_a.inner != ret_b.inner {
                    return false;
                }
                
                if args_a.len() != args_b.len() {
                    return false;
                }
                
                for (a,b) in g_a.iter().zip(g_b.iter()) {
                    if a.inner != b.inner {
                        return false;
                    }
                }
                
                for ((name_a, type_a), (name_b, type_b)) in args_a.iter().zip(args_b.iter()) {
                    if name_a != name_b || type_a.inner != type_b.inner {
                        return false;
                    }
                }
                true
            },
            (TypeInfo::Record(fields_a), TypeInfo::Record(fields_b)) => {
                if fields_a.len() != fields_b.len() {
                    return false;
                }
                
                for ((name_a, type_a), (name_b, type_b)) in fields_a.iter().zip(fields_b.iter()) {
                    if name_a != name_b || type_a.inner != type_b.inner {
                        return false;
                    }
                }
                true
            },
            (TypeInfo::Enum { name, variants, generic_params: _ }, TypeInfo::EnumVariant { enum_name, variant }) 
            | (TypeInfo::EnumVariant { enum_name, variant }, TypeInfo::Enum { name, variants, generic_params: _ }) => {
                match name == enum_name {
                    true => match variants.get(variant) {
                        Some(_) => true,
                        None => false,
                    },
                    false => false,
                }
            },
            (
                TypeInfo::Enum { name: name1, variants: variants1, generic_params: g_a },
                TypeInfo::Enum { name: name2, variants: variants2, generic_params: g_b }
            ) => {
                match name1 == name2 {
                    true => {
                        for ((name_a, type_a), (name_b, type_b)) in variants1.iter().zip(variants2.iter()) {
                            if name_a != name_b || type_a.inner != type_b.inner {
                                return false;
                            }
                        }
                        for (a,b) in g_a.iter().zip(g_b.iter()) {
                            if a.inner != b.inner {
                                return false;
                            }
                        }
                        true
                    },
                    false => false,
                }
            },
            (
                TypeInfo::EnumVariant { enum_name: name1, variant: v1 }, 
                TypeInfo::EnumVariant { enum_name: name2, variant: v2 }
            ) => {
                match name1 == name2 {
                    true => v1 == v2,
                    false => false,
                }
            },
            (TypeInfo::Enum { name, variants, generic_params }, TypeInfo::EnumInstance { enum_name, variants: enum_variants, generic_args }) 
            | (TypeInfo::EnumInstance { enum_name, variants: enum_variants, generic_args }, TypeInfo::Enum { name, variants, generic_params }) => {
                if generic_args.len() != generic_params.len() || variants.len() != enum_variants.len() {
                    false
                } else {
                    match name == enum_name {
                        true => {
                            for ((name_a, type_a), (name_b, type_b)) in variants.iter().zip(enum_variants.iter()) {
                                if name_a != name_b || type_a.inner != type_b.inner {
                                    return false;
                                }
                            }
                            true
                        },
                        false => false,
                    }
                }
            },
            (
                TypeInfo::EnumInstance { enum_name: name1, variants: variants1, generic_args: args1 },
                TypeInfo::EnumInstance { enum_name: name2, variants: variants2, generic_args: args2 }
            ) => {
                if name1 != name2 || variants1.len() != variants2.len() || args1.len() != args2.len() {
                    return false
                }
                for ((name_a, type_a), (name_b, type_b)) in variants1.iter().zip(variants2.iter()) {
                    if name_a != name_b || type_a.inner != type_b.inner {
                        return false;
                    }
                }
                for (a,b) in args1.iter().zip(args2.iter()) {
                    if a.inner != b.inner {
                        return false;
                    }
                }
                true
            },
            (TypeInfo::EnumInstance { enum_name: name1, variants, generic_args: _ }, TypeInfo::EnumVariant { enum_name: name2, variant }) 
            | (TypeInfo::EnumVariant { enum_name: name2, variant }, TypeInfo::EnumInstance { enum_name: name1, variants, generic_args: _ }) => {
                match name1 == name2 {
                    true => {
                        variants.get(variant).is_some()
                    },
                    false => false,
                }
            },
            (TypeInfo::GenericParam(a), TypeInfo::GenericParam(b)) => a == b,
            (TypeInfo::Any, _) | (_, TypeInfo::Any) => true,
            _ => false,
        }
    }
}