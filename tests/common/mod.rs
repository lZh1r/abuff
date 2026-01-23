use std::env::current_dir;

use chumsky::{Parser, span::SimpleSpan};
use abuff::{ast::Spanned, checker::{hoist, lower_statement}, env::{create_default_env}, error::build_report, ir::{ControlFlow}, main_parser::parser, module::{GlobalRegistry, run}};

// fn run(statements: &[Spanned<Statement>], env: &mut Env) -> Result<ControlFlow, Spanned<String>> {
//     let mut result = Ok(ControlFlow::Value(Value::Void));
//     for statement in statements {
//         match statement.inner.clone() {
//             Statement::Let { name, expr } => {
//                 match eval_expr(&expr, env)? {
//                     ControlFlow::Value(v) => env.add_variable(name.clone(), v),
//                     cf => return Ok(cf),
//                 }
//             },
//             Statement::Expr(expr) => {
//                 let cf = eval_expr(&expr, env)?;
//                 match cf {
//                     ControlFlow::Value(_) => result = Ok(cf),
//                     _ => return Ok(cf),
//                 }
//             },
//         }
//     };
    
//     result
// }

pub fn run_typed(src: String) -> Result<ControlFlow, Spanned<String>> {
    let (mut env, mut type_env) = create_default_env();
    
    let parsed_result = parser().parse(&src);
    
    let parsed = match parsed_result.into_result() {
        Ok(v) => v,
        Err(errors) => {
            for e in errors {
                build_report(Spanned {
                    inner: e.reason().to_string(),
                    span: e.span().clone()
                }, &src);
            }
           
            return Err(Spanned {
                inner: "Parsing failed".to_string(),
                span: SimpleSpan::from(0..0)
            })
        },
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
            let reg = GlobalRegistry;
            match run(&result, &mut env, current_dir().unwrap(), &reg) {
                Err(e) => Err(e),
                Ok(result) => Ok(result)
            }
        }
    }
}