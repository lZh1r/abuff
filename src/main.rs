use std::{path::Display, rc::Rc, sync::OnceLock, time::{self, Instant, SystemTime, UNIX_EPOCH}};

use gigalang::{ast::TypeInfo, checker::{hoist, lower_statement}, env::{Env, TypeEnv}, interpreter::{eval_closure, eval_expr}, ir::{Statement, Value}, main_parser::parser};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::Parser;

static PROCESS_START: OnceLock<Instant> = OnceLock::new();

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

fn create_global_env() -> (Env, TypeEnv) {
    let mut type_env = TypeEnv::new();
    let mut env = Env::new();
    
    type_env.add_var_type("print".to_string(), TypeInfo::Fun { args: vec![("args".to_string(), TypeInfo::Any)], return_type: Box::new(TypeInfo::Void) });
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
    
    type_env.add_var_type("debug".to_string(), TypeInfo::Fun { args: vec![("args".to_string(), TypeInfo::Any)], return_type: Box::new(TypeInfo::Void) });
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
    
    type_env.add_var_type("str".to_string(), TypeInfo::Fun { args: vec![("value".to_string(), TypeInfo::Any)], return_type: Box::new(TypeInfo::String) });
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
    
    type_env.add_var_type("clock".to_string(), TypeInfo::Fun { 
        args: Vec::new(),
        return_type: Box::new(TypeInfo::Int) }
    );
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
    
    type_env.add_var_type("input".to_string(), TypeInfo::Fun { 
        args: Vec::new(),
        return_type: Box::new(TypeInfo::String) }
    );
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
    
    type_env.add_var_type("len".to_string(), TypeInfo::Fun { 
        args: vec![("value".to_string(), TypeInfo::Any)],
        return_type: Box::new(TypeInfo::String) }
    );
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
                        v => return Err(format!("Cannot measure a length of {v:?}"))
                    }
                };
                match result {
                    Some(l) => Ok(Value::Int(l as i64)),
                    None => Err("No arguments provided to \"str\"".to_string()),
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
            Err(e) => println!("{e}"),
            Ok(result) => {
                let mut lowered_statements = Vec::new();
                for s in result {
                    match lower_statement(&s, &mut type_env) {
                        Ok(st) => match st {
                            Some(st) => lowered_statements.push(st),
                            None => continue,
                        },
                        Err(e) => {
                            println!("{e}");
                            return;
                        },
                    }
                }
                match run(&lowered_statements, &mut stack) {
                    Err(e) => println!("{e}"),
                    Ok(_) => {
                        // match result {
                        //     Value::Int(i) => println!("{i}"),
                        //     Value::Bool(b) => println!("{b}"),
                        //     Value::Record(hash_map) => println!("{hash_map:?}"),
                        //     Value::Closure { params, body, env: _ } => println!("closure: {params:?}, {body:?}"),
                        //     Value::Float(f) => println!("{f}"),
                        //     Value::Void => println!("void"),
                        //     Value::String(s) => println!("\"{s}\""),
                        //     Value::Char(c) => println!("\'{c}\'"),
                        //     Value::Null => println!("null"),
                        //     Value::NativeFun(native_fun) => println!("{native_fun:?}"),
                        // }
                    }
                }
            }
        }
    }
}
