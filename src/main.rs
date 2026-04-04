use std::{collections::HashMap, env::{self, current_dir}, fs};

use abuff::{
    ast::clean::ControlFlow, checker::hoisting::hoist_declarations, env::{DEFAULT_ENVS, create_default_env}, error::build_report, lexer::lex, main_parser::Parser, module::{GlobalRegistry, run}, span::Spanned
};

struct CommandInfo {
    description: String,
    function: fn(Vec<String>, HashMap<&str, CommandInfo>),
    min_arg_amount: u8
}

fn main() {
    let commands = HashMap::from([
        (
            "run",
            CommandInfo {
                description: "Executes a file".into(),
                function: |args, _| {
                    let file_path = args.get(2).unwrap();
                    let src = match fs::read_to_string(file_path) {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!("Error while reading the file: {e}");
                            return;
                        },
                    };
                    let (mut stack, mut type_env) = DEFAULT_ENVS.get_or_init(|| create_default_env()).clone();
                    let parse_result = match lex(src.as_str()) {
                        Ok(v) => Parser::new(&v).parse(),
                        Err(e) => return build_report(e, &src, file_path),
                    };
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
                    
                    let res = hoist_declarations(&parsed, &mut type_env, current_dir().unwrap().to_str().unwrap());
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
                min_arg_amount: 1
            }
        ),
        (
            "check",
            CommandInfo {
                description: "Runs static analysis on the provided file".into(),
                function: |args, _| {
                    let file_path = args.get(2).unwrap();
                    let src = match fs::read_to_string(file_path) {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!("Error while reading the file: {e}");
                            return;
                        },
                    };
                    let (_, mut type_env) = DEFAULT_ENVS.get_or_init(|| create_default_env()).clone();
                    let parse_result = match lex(src.as_str()) {
                        Ok(v) => Parser::new(&v).parse(),
                        Err(e) => return build_report(e, &src, file_path),
                    };
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
                    
                    let res = hoist_declarations(&parsed, &mut type_env, current_dir().unwrap().to_str().unwrap());
                    match res {
                        Err(e) => {
                            println!("Type checking failed with following errors:");
                            build_report(e, &src, file_path)
                        },
                        Ok(_) => {
                            println!("Static analysis passed.")
                        }
                    }
                },
                min_arg_amount: 1
            }
        ),
        (
            "help",
            CommandInfo {
                description: "Displays this message".into(),
                function: |args, commands| {
                    match args.get(2) {
                        Some(command) => {
                            match commands.get(command.as_str()) {
                                Some(cmd) => {
                                    println!("{}", cmd.description)
                                },
                                None => {
                                    eprintln!("Unknown command {command}")
                                }
                            }
                        },
                        None => {
                            for (k, v) in commands.iter() {
                                println!("\x1b[1m{k}\x1b[0m - {}", v.description);
                            }
                        }
                    }
                },
                min_arg_amount: 0
            }
        )
    ]);
    
    let args: Vec<String> = env::args().collect();
    let command = args.get(1);
    match command {
        Some(cmd) => {
            if commands.get(cmd.as_str()).is_none() {
                eprintln!("Unknown command {cmd}")
            }
            let command_info = commands.get(cmd.as_str()).unwrap();
            if command_info.min_arg_amount > (args.len() - 2) as u8 {
                eprintln!("Incorrect amount of arguments provided!");
                eprintln!("Expected {}, got {}", command_info.min_arg_amount, args.len());
            } else {
                (command_info.function)(args, commands)
            }
        },
        None => {
            let (mut stack, mut type_env) = DEFAULT_ENVS.get_or_init(|| create_default_env()).clone();
            loop {
                let mut src = String::new();
                
                let _ = std::io::stdin().read_line(&mut src);
                
                let parse_result = match lex(src.as_str()) {
                    Ok(v) => Parser::new(&v).parse(),
                    Err(e) => return build_report(e, &src, &"repl".to_string()),
                };
                
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
                
                let res = hoist_declarations(&parsed, &mut type_env, current_dir().unwrap().to_str().unwrap());
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
        }
    }
}