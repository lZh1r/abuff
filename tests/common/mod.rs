use std::env::current_dir;

use chumsky::{Parser, span::SimpleSpan};
use abuff::{ast::Spanned, checker::{hoist, lower_statement}, env::{create_default_env}, error::build_report, ir::{ControlFlow}, main_parser::parser, module::{GlobalRegistry, run}};

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