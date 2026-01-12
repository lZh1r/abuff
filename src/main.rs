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
    let program = vec![
        // 1. let sq_len = fn(pt) { pt.x * pt.x + pt.y * pt.y };
        Statement::Let {
            name: "sq_len".to_string(),
            expr: Expr::Fun {
                param: "pt".to_string(),
                body: Box::new(Expr::Binary {
                    left: Box::new(Expr::Binary { // pt.x * pt.x
                        left: Box::new(Expr::Get(Box::new(Expr::Var("pt".to_string())), "x".to_string())),
                        operation: Operation::Multiply,
                        right: Box::new(Expr::Get(Box::new(Expr::Var("pt".to_string())), "x".to_string())),
                    }),
                    operation: Operation::Add,
                    right: Box::new(Expr::Binary { // pt.y * pt.y
                        left: Box::new(Expr::Get(Box::new(Expr::Var("pt".to_string())), "y".to_string())),
                        operation: Operation::Multiply,
                        right: Box::new(Expr::Get(Box::new(Expr::Var("pt".to_string())), "y".to_string())),
                    }),
                })
            }
        },
    
        // 2. let p = { x: 3, y: 4 };
        Statement::Let {
            name: "p".to_string(),
            expr: Expr::Record(vec![
                ("x".to_string(), Expr::Int(3)),
                ("y".to_string(), Expr::Int(4)),
            ])
        },
    
        // 3. sq_len(p) -> Expect 25 (9 + 16)
        Statement::Expr(Expr::Call {
            fun: Box::new(Expr::Var("sq_len".to_string())),
            arg: Box::new(Expr::Var("p".to_string())),
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
