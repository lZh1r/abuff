use gigalang::{ir::{Statement, Value}, env::Env, interpreter::eval_expr, parser::parse};
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
    let mut stack = Env::new();
    
    loop {
        let mut src = String::new();
        
        std::io::stdin().read_line(&mut src);
        
        let parsed = parse().parse(&src);
        
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
        
        let res = run(&parsed.unwrap(), &mut stack);
        match res {
            Err(e) => println!("{e}"),
            Ok(result) => {
                match result {
                    Value::Int(i) => println!("{i}"),
                    Value::Bool(b) => println!("{b}"),
                    Value::Record(hash_map) => println!("record"),
                    Value::Closure { param, body, env } => println!("closure"),
                    Value::Float(f) => println!("{f}"),
                    Value::Void => println!("void"),
                }
            }
        }
    }
}
