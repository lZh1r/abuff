use std::{collections::HashMap, fmt::Display, ops::Range};

use smol_str::SmolStr;

use crate::env::TypeEnv;

use std::sync::atomic::{AtomicU32, Ordering};

static TYPE_ID_COUNTER: AtomicU32 = AtomicU32::new(10);

pub fn next_type_id() -> u32 {
    TYPE_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

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

impl Display for Spanned<SmolStr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(SmolStr),
    Char(char),
    Void,
    Null,
    Var(SmolStr),
    Array(Vec<Spanned<Expr>>),
    Index(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Binary {left: Box<Spanned<Expr>>, operation: Operation, right: Box<Spanned<Expr>>},
    Block(Vec<Spanned<Statement>>, Option<Box<Spanned<Expr>>>),
    Fun {
        params: Vec<((bool, SmolStr), Spanned<TypeInfo>)>,
        body: Box<Spanned<Expr>>,
        return_type: Option<Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<SmolStr>>
    },
    Call {fun: Box<Spanned<Expr>>, args: Vec<Spanned<Expr>>, generic_args: Vec<Spanned<TypeInfo>>},
    Record(Vec<(SmolStr, Spanned<Expr>)>),
    Get(Box<Spanned<Expr>>, SmolStr),
    Assign {target: Box<Spanned<Expr>>, value: Box<Spanned<Expr>>},
    Unary(UnaryOp, Box<Spanned<Expr>>),
    If {condition: Box<Spanned<Expr>>, body: Box<Spanned<Expr>>, else_block: Option<Box<Spanned<Expr>>>},
    While {condition: Box<Spanned<Expr>>, body: Box<Spanned<Expr>>},
    Break,
    Continue,
    Return(Box<Spanned<Expr>>),
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
pub struct Method {
    pub name: SmolStr, 
    pub params: Vec<((bool, SmolStr), Spanned<TypeInfo>)>,
    pub body: Spanned<Expr>,
    pub return_type: Option<Spanned<TypeInfo>>,
    pub generic_params: Vec<Spanned<SmolStr>>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {name: SmolStr, expr: Spanned<Expr>, type_info: Option<Spanned<TypeInfo>>},
    TypeDef {
        name: SmolStr, 
        type_info: Spanned<TypeInfo>,
        generic_params: Vec<Spanned<SmolStr>>
    },
    Expr(Spanned<Expr>),
    Fun {
        name: SmolStr, 
        params: Vec<((bool, SmolStr), Spanned<TypeInfo>)>,
        body: Spanned<Expr>,
        return_type: Option<Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<SmolStr>>
    },
    NativeFun {
        name: SmolStr, 
        params: Vec<((bool, SmolStr), Spanned<TypeInfo>)>,
        return_type: Option<Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<SmolStr>>
    },
    EnumDef {
        name: SmolStr, 
        variants: Vec<(SmolStr, Option<Spanned<TypeInfo>>)>,
        generic_params: Vec<Spanned<SmolStr>>
    },
    Import {symbols: Vec<(Spanned<SmolStr>, Option<SmolStr>, bool)>, path: Spanned<SmolStr>},
    Export(Box<Spanned<Statement>>)
}

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
    Or,
    NullCoal,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    kind: TypeKind,
    id: u32
}

impl TypeInfo {
    pub fn int() -> Self {
        Self { kind: TypeKind::Int, id: 0 }
    }
    pub fn float() -> Self {
        Self { kind: TypeKind::Float, id: 1 }
    }
    pub fn string() -> Self {
        Self { kind: TypeKind::String, id: 2 }
    }
    pub fn char() -> Self {
        Self { kind: TypeKind::Char, id: 3 }
    }
    pub fn bool() -> Self {
        Self { kind: TypeKind::Bool, id: 4 }
    }
    pub fn void() -> Self {
        Self { kind: TypeKind::Void, id: 5 }
    }
    pub fn null() -> Self {
        Self { kind: TypeKind::Null, id: 6 }
    }
    pub fn unknown() -> Self {
        Self { kind: TypeKind::Unknown, id: 7 }
    }
    pub fn any() -> Self {
        Self { kind: TypeKind::Any, id: 8 }
    }
    pub fn new(kind: TypeKind) -> Self {
        Self { kind, id: next_type_id() }
    }
    pub fn kind(&self) -> &TypeKind {
        &self.kind
    }
}

impl PartialEq for TypeInfo {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id || self.kind == other.kind
    }
}

#[derive(Debug, Clone)]
pub enum TypeKind {
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
        params: Vec<((bool, SmolStr), Spanned<TypeInfo>)>,
        return_type: Box<Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<SmolStr>>
    },
    Record(Vec<(SmolStr, Spanned<TypeInfo>)>),
    Custom {name: SmolStr, generic_args: Vec<Spanned<TypeInfo>>},
    GenericParam(SmolStr),
    Array(Box<Spanned<TypeInfo>>),
    Enum {
        name: SmolStr,
        variants: HashMap<SmolStr, Spanned<TypeInfo>>,
        generic_params: Vec<Spanned<SmolStr>>
    },
    EnumInstance {enum_name: SmolStr, variants: HashMap<SmolStr, Spanned<TypeInfo>>, generic_args: Vec<Spanned<TypeInfo>>},
    EnumVariant {enum_name: SmolStr, variant: SmolStr, generic_args: Vec<TypeInfo>},
    TypeClosure {params: Vec<Spanned<SmolStr>>, env: TypeEnv, body: Box<Spanned<TypeInfo>>}
}

impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeKind::Int, TypeKind::Int) => true,
            (TypeKind::Float, TypeKind::Float) => true,
            (TypeKind::String, TypeKind::String) => true,
            (TypeKind::Char, TypeKind::Char) => true,
            (TypeKind::Bool, TypeKind::Bool) => true,
            (TypeKind::Void, TypeKind::Void) => true,
            (TypeKind::Null, TypeKind::Null) => true,
            (TypeKind::Unknown, TypeKind::Unknown) => false,
            (TypeKind::Array(a1), TypeKind::Array(a2)) => a1.inner == a2.inner,
            (TypeKind::Custom {name: name1, generic_args: args1}, TypeKind::Custom {name: name2, generic_args: args2}) => {
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
                TypeKind::Fun { params: args_a, return_type: ret_a, generic_params: g_a },
                TypeKind::Fun { params: args_b, return_type: ret_b, generic_params: g_b }
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
            (TypeKind::Record(fields_a), TypeKind::Record(fields_b)) => {
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
            (TypeKind::Enum { name, variants, generic_params: _ }, TypeKind::EnumVariant { enum_name, variant, generic_args: _ }) 
            | (TypeKind::EnumVariant { enum_name, variant, generic_args: _ }, TypeKind::Enum { name, variants, generic_params: _ }) => {
                name == enum_name && variants.get(variant).is_some()
            },
            (
                TypeKind::Enum { name: name1, variants: variants1, generic_params: g_a },
                TypeKind::Enum { name: name2, variants: variants2, generic_params: g_b }
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
                TypeKind::EnumVariant { enum_name: name1, variant: v1, generic_args: g_a }, 
                TypeKind::EnumVariant { enum_name: name2, variant: v2, generic_args: g_b }
            ) => {
                name1 == name2 && v1 == v2 && {
                    for (a,b) in g_a.iter().zip(g_b.iter()) {
                        if a != b {
                            return false;
                        }
                    }
                    true
                }
            },
            (TypeKind::Enum { name, variants, generic_params }, TypeKind::EnumInstance { enum_name, variants: enum_variants, generic_args }) 
            | (TypeKind::EnumInstance { enum_name, variants: enum_variants, generic_args }, TypeKind::Enum { name, variants, generic_params }) => {
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
                TypeKind::EnumInstance { enum_name: name1, variants: variants1, generic_args: args1 },
                TypeKind::EnumInstance { enum_name: name2, variants: variants2, generic_args: args2 }
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
            (TypeKind::EnumInstance { enum_name: name1, variants, generic_args: args1 }, TypeKind::EnumVariant { enum_name: name2, variant, generic_args: args2 }) 
            | (TypeKind::EnumVariant { enum_name: name2, variant, generic_args: args2 }, TypeKind::EnumInstance { enum_name: name1, variants, generic_args: args1 }) => {
                name1 == name2 && variants.get(variant).is_some() && {
                    {
                        if args1.len() != args2.len() {
                            false
                        } else {
                            args1.iter().zip(args2.iter()).all(|(a1, a2)| a1.inner == *a2)
                        }
                    }
                }
            },
            (TypeKind::GenericParam(a), TypeKind::GenericParam(b)) => a == b,
            (TypeKind::GenericParam(a), TypeKind::Custom { name, generic_args })
            | (TypeKind::Custom { name, generic_args }, TypeKind::GenericParam(a)) => a == name && generic_args.len() == 0,
            // (TypeInfo::EnumInstance{ enum_name, variants: _, generic_args }, TypeInfo::Custom { name, generic_args: g1 })
            // | (TypeInfo::Custom { name, generic_args: g1 }, TypeInfo::EnumInstance{ enum_name, variants: _, generic_args }) => {
            //     enum_name == name && {
            //         for (ti1, ti2) in generic_args.iter().zip(g1.iter()) {
            //             if ti1.inner != ti2.inner {
            //                 return false
            //             }
            //         }
            //         true
            //     }
            // },
            // (TypeInfo::EnumVariant{ enum_name, variant: _, generic_args }, TypeInfo::Custom { name, generic_args: g1 })
            // | (TypeInfo::Custom { name, generic_args: g1 }, TypeInfo::EnumVariant{ enum_name, variant: _, generic_args }) => {
            //     enum_name == name && {
            //         for (ti1, ti2) in generic_args.iter().zip(g1.iter()) {
            //             if *ti1 != ti2.inner {
            //                 return false
            //             }
            //         }
            //         true
            //     }
            // },
            (TypeKind::Any, _) | (_, TypeKind::Any) => true,
            _ => false,
        }
    }
}