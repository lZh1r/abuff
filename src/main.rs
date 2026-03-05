use std::{env::{self, current_dir}, fs};

use abuff::{
    span::Spanned,
    env::{DEFAULT_ENVS, create_default_env},
    error::build_report, 
    ast::clean::ControlFlow, 
    lexer::lex, 
    main_parser::Parser,
    module::{GlobalRegistry, run},
    type_checker::{hoist}
};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_to_read = args.get(1);
    match file_to_read {
        Some(file_path) => {
            let src = match fs::read_to_string(file_path) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Error while reading the file: {e}");
                    return;
                },
            };
            let (mut stack, mut type_env) = DEFAULT_ENVS.get_or_init(|| create_default_env()).clone();
            let parse_result = Parser::new(&lex(src.as_str()).unwrap()).parse();
            let parsed = match parse_result {
                Ok(s) => s,
                Err(errors) => {
                    println!("Parsing failed with following errors:");
                    build_report(Spanned {
                        inner: errors.inner,
                        span: errors.span
                    }, &src, file_path);
                    return 
                },
            };
            
            let res = hoist(&parsed, &mut type_env, current_dir().unwrap().to_str().unwrap());
            match res {
                Err(e) => {
                    println!("Type checking failed with following errors:");
                    build_report(e, &src, file_path)
                },
                Ok(result) => {
                    let lowered_statements = result.0;
                    let reg = GlobalRegistry;
                    match run(&lowered_statements, &mut stack, current_dir().unwrap(), &reg) {
                        Err(e) => {
                            println!("Runtime error occured:");
                            build_report(e, &src, file_path)
                        },
                        Ok(cf) => {
                            match cf {
                                ControlFlow::Value(_) => println!("Process exited successfully"),
                                ControlFlow::Return(_) => println!("Error: Return outside of function"),
                                ControlFlow::Break => println!("Error: Break outside of loop"),
                                ControlFlow::Continue => println!("Error: Continue outside of loop"),
                                ControlFlow::Panic(reason) => panic!("{}", reason.unwrap_or_default()),
                            }
                        }
                    }
                }
            }
        },
        None => {
            let (mut stack, mut type_env) = DEFAULT_ENVS.get_or_init(|| create_default_env()).clone();
            loop {
                let mut src = String::new();
                
                let _ = std::io::stdin().read_line(&mut src);
                
                let parse_result = Parser::new(&lex(src.as_str()).unwrap()).parse();
                let parsed = match parse_result {
                    Ok(s) => s,
                    Err(errors) => {
                        build_report(Spanned {
                            inner: errors.inner,
                            span: errors.span
                        }, &src, &"repl".to_string());
                        continue
                    },
                };
                
                let res = hoist(&parsed, &mut type_env, current_dir().unwrap().to_str().unwrap());
                match res {
                    Err(e) => build_report(e, &src, &"repl".to_string()),
                    Ok(result) => {
                        let lowered_statements = result.0;
                        let reg = GlobalRegistry;
                        match run(&lowered_statements, &mut stack, current_dir().unwrap(), &reg) {
                            Err(e) => build_report(e, &src, &"repl".to_string()),
                            Ok(cf) => {
                                match cf {
                                    ControlFlow::Value(_) => (),
                                    ControlFlow::Return(_) => println!("Error: Return outside of function"),
                                    ControlFlow::Break => println!("Error: Break outside of loop"),
                                    ControlFlow::Continue => println!("Error: Continue outside of loop"),
                                    ControlFlow::Panic(reason) => panic!("{}", reason.unwrap_or_default()),
                                }
                            }
                        }
                    }
                }
            }
        },
    }
    
    
    
    
}