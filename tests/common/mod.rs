use chumsky::Parser;
use gigalang::{checker::{hoist, lower_statement}, env::{Env, TypeEnv}, interpreter::eval_expr, ir::{ControlFlow, Statement, Value}, legacy_parser::parse, main_parser::parser};

pub fn parse_code(src: &str) -> Vec<Statement> {
    parse().parse(src).into_result().expect("Failed to parse")
}

pub fn run(statements: &[Statement]) -> Result<ControlFlow, String> {
    let mut env = Env::new();
    let mut result = Ok(ControlFlow::Value(Value::Void));
    for statement in statements {
        match statement {
            Statement::Let { name, expr } => {
                match eval_expr(expr, &mut env)? {
                    ControlFlow::Value(v) => env.add_variable(name.clone(), v),
                    cf => return Ok(cf),
                }
            },
            Statement::Expr(expr) => {
                let cf = eval_expr(expr, &mut env)?;
                match cf {
                    ControlFlow::Value(_) => result = Ok(cf),
                    _ => return Ok(cf),
                }
            },
        }
    };
    
    result
}

pub fn run_typed(src: String) -> Result<ControlFlow, String> {
    let mut type_env = TypeEnv::new();
    
    let parsed_result = parser().parse(&src);
    
    let parsed = match parsed_result.into_result() {
        Ok(v) => v,
        Err(_) => return Err("Parsing failed".to_string()),
    };
    
    let res = hoist(&parsed, &mut type_env);
    match res {
        Err(e) => Err(e),
        Ok(statements) => {
            let mut result = Vec::new();
            for s in statements {
                match lower_statement(&s, &mut type_env)? {
                    None => (),
                    Some(st) => result.push(st)
                }
            }
            match run(&result) {
                Err(e) => Err(e),
                Ok(result) => Ok(result)
            }
        }
    }
}