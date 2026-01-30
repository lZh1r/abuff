use std::env::current_dir;

use abuff::{ast::{Span, Spanned}, checker::{hoist, lower_statement}, env::{Env, TypeEnv, create_default_env}, error::build_report, ir::ControlFlow, lexer::lex, main_parser::Parser, module::{GlobalRegistry, run}};

pub fn run_typed(src: String) -> Result<ControlFlow, Spanned<String>> {
    // let (mut env, mut type_env) = create_default_env();
    let (mut env, mut type_env) = (Env::new(), TypeEnv::new());
    let parse_result = Parser::new(&lex(src.as_str())?).parse();
    
    let parsed = match parse_result {
        Ok(v) => v,
        Err(errors) => {
            // for e in errors {
            //     build_report(Spanned {
            //         inner: e.reason().to_string(),
            //         span: e.span().clone()
            //     }, &src);
            // }
           
            build_report(Spanned {
                inner: errors.inner,
                span: errors.span
            }, &src);
            
            return Err(Spanned {
                inner: "Parsing failed".to_string(),
                span: Span::from(0..0)
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