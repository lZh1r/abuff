use chumsky::Parser;
use gigalang::{checker::hoist, env::{Env, TypeEnv}, interpreter::eval_expr, ir::{Statement, Value}, legacy_parser::parse, main_parser::parser};

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

pub fn run_typed(src: String) -> Result<Value, String> {
    let mut type_env = TypeEnv::new();
    
    let parsed_result = parser().parse(&src);
    
    let parsed = match parsed_result.into_result() {
        Ok(v) => v,
        Err(_) => return Err("Parsing failed".to_string()),
    };
    
    let res = hoist(&parsed, &mut type_env);
    match res {
        Err(e) => Err(e),
        Ok(result) => {
            match run(&result) {
                Err(e) => Err(e),
                Ok(result) => Ok(result)
            }
        }
    }
}
