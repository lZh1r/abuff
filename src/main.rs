use gigalang::{checker::lower, env::{Env, TypeEnv}, interpreter::eval_expr, ir::{Statement, Value}, main_parser::parser};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::Parser;

fn run(statements: &[Statement], env: &mut Env) -> Result<Value, String> {
    let mut result = Ok(Value::Void);
    for statement in statements {
        match statement {
            Statement::Let { name, expr } => {
                let val = eval_expr(expr, env);
                match val {
                    Ok(v) => env.add_variable(name.clone(), v),
                    Err(e) => return Err(e),
                }
            },
            Statement::Expr(expr) => {
                result = eval_expr(expr, env);
            },
        }
    };
    
    result
}

fn main() {
    let mut type_env = TypeEnv::new();
    let mut stack = Env::new();
    
    loop {
        let mut src = String::new();
        
        let _ = std::io::stdin().read_line(&mut src);
        
        let parsed = parser().parse(&src);
        
        if parsed.has_errors() {
            for error in parsed.errors() {
                Report::build(ReportKind::Error, error.span().into_range())
                    .with_message(error.to_string())
                    .with_label(
                        Label::new(error.span().into_range())
                            .with_message(error.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print(Source::from(src.clone()))
                    .unwrap();
            }
            continue;
        }
        
        let res = lower(&parsed.unwrap(), &mut type_env);
        match res {
            Err(e) => println!("{e}"),
            Ok(result) => {
                match run(&result, &mut stack) {
                    Err(e) => println!("{e}"),
                    Ok(result) => {
                        match result {
                            Value::Int(i) => println!("{i}"),
                            Value::Bool(b) => println!("{b}"),
                            Value::Record(hash_map) => println!("{hash_map:?}"),
                            Value::Closure { params, body, env: _ } => println!("closure: {params:?}, {body:?}"),
                            Value::Float(f) => println!("{f}"),
                            Value::Void => println!("void"),
                            Value::String(s) => println!("\"{s}\""),
                            Value::Char(c) => println!("\'{c}\'"),
                            Value::Null => println!("null"),
                        }
                    }
                }
            }
        }
    }
}
