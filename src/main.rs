use std::{fs::{self, write}, io::Error, rc::Rc, sync::OnceLock, time::Instant};

use chumsky::span::{SimpleSpan, Span as SpanTrait};
use gigalang::{ast::{TypeInfo, Spanned, Span}, checker::{hoist, lower_statement}, env::{Env, TypeEnv}, interpreter::{eval_expr}, ir::{Statement, Value, ControlFlow}, main_parser::parser};

fn spanned<T>(inner: T) -> Spanned<T> {
    Spanned { inner, span: Span::new((), 0..0) }
}
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::Parser;

static PROCESS_START: OnceLock<Instant> = OnceLock::new();

fn run(statements: &[Statement], env: &mut Env) -> Result<ControlFlow, String> {
    let mut result = Ok(ControlFlow::Value(Value::Void));
    for statement in statements {
        match statement {
            Statement::Let { name, expr } => {
                match eval_expr(expr, env)? {
                    ControlFlow::Value(v) => env.add_variable(name.clone(), v),
                    cf => return Ok(cf),
                }
            },
            Statement::Expr(expr) => {
                let cf = eval_expr(expr, env)?;
                match cf {
                    ControlFlow::Value(_) => result = Ok(cf),
                    _ => return Ok(cf),
                }
            },
        }
    };
    
    result
}

fn create_global_env() -> (Env, TypeEnv) {
    let mut type_env = TypeEnv::new();
    let mut env = Env::new();
    
    type_env.add_var_type("print".to_string(), Spanned { 
        inner: TypeInfo::Fun {
            args: vec![("args".to_string(), spanned(TypeInfo::Any))], 
            return_type: Box::new(spanned(TypeInfo::Void)) 
        }, 
        span: SimpleSpan::from(0..0) 
    });
    env.add_variable("print".to_string(), Value::NativeFun(gigalang::ir::NativeFun { 
        name: "print".to_string(),
        max_args: None,
        function: Rc::new(Box::new(
            |args: Vec<Value>| {
                let length = args.len();
                let mut i = 0;
                for a in args {
                    print!("{a}");
                    i += 1;
                    if i < length {
                        print!(", ")
                    }
                };
                println!();
                Ok(Value::Void)
            }
        ))
    }));
    
    type_env.add_var_type("debug".to_string(), Spanned { 
        inner: TypeInfo::Fun { 
            args: vec![("args".to_string(), spanned(TypeInfo::Any))], 
            return_type: Box::new(spanned(TypeInfo::Void))
        },
        span: SimpleSpan::from(0..0)
    });
    env.add_variable("debug".to_string(), Value::NativeFun(gigalang::ir::NativeFun { 
        name: "debug".to_string(),
        max_args: None,
        function: Rc::new(Box::new(
            |args: Vec<Value>| {
                let length = args.len();
                let mut i = 0;
                for a in args {
                    print!("{a:?}");
                    i += 1;
                    if i < length {
                        print!(", ")
                    }
                };
                println!();
                Ok(Value::Void)
            }
        ))
    }));
    
    type_env.add_var_type("str".to_string(), Spanned { 
        inner: TypeInfo::Fun {
            args: vec![("value".to_string(), spanned(TypeInfo::Any))],
            return_type: Box::new(spanned(TypeInfo::String)) 
        }, 
        span: SimpleSpan::from(0..0)
    });
    env.add_variable("str".to_string(), Value::NativeFun(gigalang::ir::NativeFun { 
        name: "str".to_string(),
        max_args: Some(1),
        function: Rc::new(Box::new(
            |args: Vec<Value>| {
                let mut result = None;
                for a in args {
                    result = Some(a.to_string());
                };
                match result {
                    Some(s) => Ok(Value::String(s)),
                    None => Err("No arguments provided to \"str\"".to_string()),
                }
            }
        ))
    }));
    
    type_env.add_var_type("clock".to_string(), Spanned { 
        inner: TypeInfo::Fun { 
            args: Vec::new(),
            return_type: Box::new(spanned(TypeInfo::Int)) 
        },
        span: SimpleSpan::from(0..0)
    });
    env.add_variable("clock".to_string(), Value::NativeFun(gigalang::ir::NativeFun { 
        name: "clock".to_string(),
        max_args: Some(0),
        function: Rc::new(Box::new(
            |_| {
                let instant = PROCESS_START.get_or_init(Instant::now);
                Ok(Value::U128(instant.elapsed().as_nanos()))
            }
        ))
    }));
    
    type_env.add_var_type("input".to_string(), Spanned { 
        inner: TypeInfo::Fun { 
            args: Vec::new(),
            return_type: Box::new(spanned(TypeInfo::String)) 
        },
        span: SimpleSpan::from(0..0)
    });
    env.add_variable("input".to_string(), Value::NativeFun(gigalang::ir::NativeFun { 
        name: "input".to_string(),
        max_args: Some(0),
        function: Rc::new(Box::new(
            |_| {
                let mut buffer = String::new();
                let input_string = std::io::stdin().read_line(&mut buffer);
                
                buffer.pop(); // \n gets added at the ebd of the buffer, so we need to pop it
                
                match input_string {
                    Ok(_) => Ok(Value::String(buffer)),
                    Err(_) => Err("Input failed".to_string()),
                }
            }
        ))
    }));
    
    type_env.add_var_type("len".to_string(), Spanned { 
        inner: TypeInfo::Fun { 
            args: vec![("value".to_string(), spanned(TypeInfo::Any))],
            return_type: Box::new(spanned(TypeInfo::String)) 
        },
        span: SimpleSpan::from(0..0)
    });
    env.add_variable("len".to_string(), Value::NativeFun(gigalang::ir::NativeFun { 
        name: "len".to_string(),
        max_args: Some(1),
        function: Rc::new(Box::new(
            |args: Vec<Value>| {
                let mut result = None;
                for a in args {
                    match a {
                        Value::String(s) => result = Some(s.len()),
                        Value::Record(hash_map) => result = Some(hash_map.len()),
                        v => return Err(format!("Cannot measure length of {v:?}"))
                    }
                };
                match result {
                    Some(l) => Ok(Value::Int(l as i64)),
                    None => Err("No arguments provided to \"str\"".to_string()),
                }
            }
        ))
    }));
    
    type_env.add_var_type("read".to_string(), Spanned {
        inner: TypeInfo::Fun { 
            args: vec![("path".to_string(), spanned(TypeInfo::String))],
            return_type: Box::new(spanned(TypeInfo::String)) 
        },
        span: SimpleSpan::from(0..0)
    });
    env.add_variable("read".to_string(), Value::NativeFun(gigalang::ir::NativeFun { 
        name: "read".to_string(),
        max_args: Some(1),
        function: Rc::new(Box::new(
            |args: Vec<Value>| {
                let mut result = Err(Error::new(std::io::ErrorKind::InvalidData, "File IO failed"));
                
                if args.len() < 1 {
                    return Err("No arguments provided".to_string());
                }
                
                for a in args {
                    match a {
                        Value::String(s) => result = fs::read_to_string(s),
                        _ => return Err(format!(""))
                    }
                };
                
                match result {
                    Ok(s) => Ok(Value::String(s)),
                    Err(e) => Err(e.to_string()),
                }
            }
        ))
    }));
    
    type_env.add_var_type("write".to_string(), Spanned {
        inner: TypeInfo::Fun { 
            args: vec![
                ("path".to_string(), spanned(TypeInfo::String)),
                ("content".to_string(), spanned(TypeInfo::String))
            ],
            return_type: Box::new(spanned(TypeInfo::String)) 
        },
        span: SimpleSpan::from(0..0)
    });
    env.add_variable("write".to_string(), Value::NativeFun(gigalang::ir::NativeFun { 
        name: "write".to_string(),
        max_args: Some(2),
        function: Rc::new(Box::new(
            |args: Vec<Value>| {
                if args.len() < 2 {
                    return Err(format!("Expected 2 arguments, but got {}", args.len()));
                }
                
                let path = match args.get(0) {
                    Some(v) => match v {
                        Value::String(s) => s,
                        a => return Err(format!("{a} is not a valid path"))
                    },
                    None => return Err("No path provided".to_string()),
                };
                
                let content = match args.get(1) {
                    Some(v) => match v {
                        Value::String(s) => s,
                        a => return Err(format!("{a} is not valid content"))
                    },
                    None => return Err("No content provided".to_string()),
                };
                
                let result = write(path, content);
                
                match result {
                    Ok(_) => Ok(Value::Bool(true)),
                    Err(e) => Err(e.to_string()),
                }
            }
        ))
    }));
    
    (env, type_env)
}

fn main() {
    let (mut stack, mut type_env) = create_global_env();
    
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
            Err(e) => {Report::build(ReportKind::Error, e.span.into_range())
                .with_message(e.to_string())
                .with_label(
                    Label::new(e.span.into_range())
                        .with_message(e.inner)
                        .with_color(Color::Red),
                )
                .finish()
                .print(Source::from(src.clone()))
                .unwrap();},
            Ok(result) => {
                let mut lowered_statements = Vec::new();
                for s in result {
                    match lower_statement(&s, &mut type_env) {
                        Ok(st) => match st {
                            Some(st) => lowered_statements.push(st),
                            None => continue,
                        },
                        Err(e) => {
                            println!("{}", e.inner);
                            continue
                        },
                    }
                }
                match run(&lowered_statements, &mut stack) {
                    Err(e) => println!("{e}"),
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