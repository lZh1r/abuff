#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    Var(String),
    Type(String),
    Binary {left: Box<Expr>, operation: Operation, right: Box<Expr>},
    Block(Vec<Statement>, Option<Box<Expr>>),
    Fun {name: Option<String>, params: Vec<(String, TypeInfo)>, body: Box<Expr>, return_type: Option<TypeInfo>},
    Call {fun: Box<Expr>, args: Vec<Expr>},
    Record(Vec<(String, Expr)>),
    Get(Box<Expr>, String)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {name: String, expr: Expr, type_info: Option<TypeInfo>},
    TypeDef {name: String, type_info: TypeInfo},
    Expr(Expr)
}

#[derive(Debug, Clone)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide
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
    Fun {args: Vec<(String, TypeInfo)>, return_type: Box<TypeInfo>},
    Record(Vec<(String, TypeInfo)>),
    Custom(String)
}