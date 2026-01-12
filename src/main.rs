mod ast;
mod interpreter;
mod env;

use crate::{ast::{Expr, Operation, Statement, Value}, env::Env, interpreter::eval_expr};

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
    
    //AI slopo
    let program: Vec<Statement> = vec![
        // 1. let make_adder = fn(x) { fn(y) { x + y } };
        Statement::Let {
            name: "make_adder".to_string(),
            expr: Expr::Fun {
                param: "x".to_string(),
                body: Box::new(Expr::Fun { // The inner function
                    param: "y".to_string(),
                    body: Box::new(Expr::Binary {
                        left: Box::new(Expr::Var("x".to_string())), // Captured from outer
                        operation: Operation::Add,
                        right: Box::new(Expr::Var("y".to_string())), // Local param
                    })
                })
            }
        },
    
        // 2. let add10 = make_adder(10);
        Statement::Let {
            name: "add10".to_string(),
            expr: Expr::Call {
                fun: Box::new(Expr::Var("make_adder".to_string())),
                arg: Box::new(Expr::Int(10)),
            }
        },
    
        // 3. add10(5) -> Expect 15
        Statement::Expr(Expr::Call {
            fun: Box::new(Expr::Var("add10".to_string())),
            arg: Box::new(Expr::Int(5)),
        })
    ];

    
    let res = run(&program, &mut stack);
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
