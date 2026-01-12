use chumsky::Parser;
use gigalang::{ast::{Statement, Value}, env::Env, interpreter::eval_expr, parser::parse};

pub fn parse_code(src: &str) -> Vec<Statement> {
    parse().parse(src).into_result().expect("Failed to parse")
}

pub fn run(statements: &[Statement]) -> Result<Value, String> {
    let mut env = Env::new();
    let mut result = Ok(Value::Void);
    for statement in statements {
        match statement {
            Statement::Let { name, expr } => {
                let val = eval_expr(expr, &mut env);
                match val {
                    Ok(v) => env.add_variable(name.clone(), v),
                    Err(e) => return Err(e),
                }
            },
            Statement::Expr(expr) => {
                result = eval_expr(expr, &mut env);
            },
        }
    };
    
    result
}