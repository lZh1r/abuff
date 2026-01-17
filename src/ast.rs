#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    Float(f64),
    Int(i64),
    String(String),
    Var(String),
    Binary {left: Box<Expr>, operation: Operation, right: Box<Expr>},
    Block(Vec<Statement>, Option<Box<Expr>>),
    Fun {params: Vec<(String, TypeInfo)>, body: Box<Expr>, return_type: Option<TypeInfo>},
    Call {fun: Box<Expr>, args: Vec<Expr>},
    Record(Vec<(String, Expr)>),
    Get(Box<Expr>, String),
    Assign {target: Box<Expr>, value: Box<Expr>},
    Unary(UnaryOp, Box<Expr>),
    If {condition: Box<Expr>, body: Box<Expr>, else_block: Option<Box<Expr>>},
    While {condition: Box<Expr>, body: Box<Expr>}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {name: String, expr: Expr, type_info: Option<TypeInfo>},
    TypeDef {name: String, type_info: TypeInfo},
    Expr(Expr),
    Fun {name: String, params: Vec<(String, TypeInfo)>, body: Expr, return_type: Option<TypeInfo>}
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

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Int,
    Float,
    String,
    Char,
    Bool,
    Void,
    Null,
    Unknown,
    Fun {args: Vec<(String, TypeInfo)>, return_type: Box<TypeInfo>},
    Record(Vec<(String, TypeInfo)>),
    Custom(String)
}