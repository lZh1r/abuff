pub enum Expr {
    Int(i32),
    Var(String),
    Add(Box<Expr>, Box<Expr>),
    Let{name: String, expr: Box<Expr>, body: Box<Expr>}
}