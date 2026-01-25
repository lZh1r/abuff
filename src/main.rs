use std::{env::current_dir};

use abuff::{checker::{hoist, lower_statement}, env::{DEFAULT_ENVS, create_default_env}, error::build_report, ir::ControlFlow, main_parser::parser, module::{GlobalRegistry, eval_import, run}};

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::Parser;

fn main() {
    let (mut stack, mut type_env) = DEFAULT_ENVS.get_or_init(|| create_default_env()).clone();
    
    loop {
        let mut src = String::new();
        
        // let src = r#"
        //     fun fib(x: Int) -> Int {
        //         if (x <= 1) 1
        //         else fib(x-2) + fib(x-1)
        //     };
        //     let t1 = clock();
        //     print(t1);
        //     let f = fib(30);
        //     let t2 = clock();
        //     print(t2);
        //     print(t2 - t1);
        // "#;
        
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
        
        let res = hoist(&parsed.unwrap(), &mut type_env);
        match res {
            Err(e) => build_report(e, &src),
            Ok(result) => {
                let mut lowered_statements = Vec::new();
                for s in result {
                    match lower_statement(&s, &mut type_env) {
                        Ok(st) => match st {
                            Some(st) => lowered_statements.push(st),
                            None => continue,
                        },
                        Err(e) => {
                            build_report(e, &src);
                            continue
                        },
                    }
                }
                let reg = GlobalRegistry;
                match run(&lowered_statements, &mut stack, current_dir().unwrap(), &reg) {
                    Err(e) => build_report(e, &src),
                    Ok(cf) => {
                        match cf {
                            ControlFlow::Value(_) => (),
                            ControlFlow::Return(_) => println!("Error: Return outside of function"),
                            ControlFlow::Break => println!("Error: Break outside of loop"),
                            ControlFlow::Continue => println!("Error: Continue outside of loop"),
                        }
                    }
                }
            }
        }
    }
}