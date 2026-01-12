use std::{collections::HashMap};


use crate::{ast::{Expr, Operation, Statement, Value}, interpreter::eval_expr};

#[derive(Debug, Clone)]
pub struct Env {
    stack: Vec<HashMap<String, Value>>,
}

impl Env {
    pub fn get(&self, name: &String) -> Option<Value> {
        for scope in self.stack.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val.clone());
            }
        }
        None
    }
    pub fn enter_scope(&mut self) -> () {
        let new_scope = HashMap::new();
        self.stack.push(new_scope);
    }
    pub fn exit_scope(&mut self) -> () {
        self.stack.pop();
    }
    pub fn add_variable(&mut self, name: String, value: Value) -> () {
        let last_scope = self.stack.last_mut();
        match last_scope {
            Some(scope) => scope.insert(name, value),
            None => panic!(),
        };
    }
}

mod ast;
mod interpreter;

fn run(statements: &[Statement], env: &mut Env) -> Result<Value, String> {
    env.enter_scope();
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
    env.exit_scope();
    
    result
}

fn main() {
    let mut stack: Env = {
        Env { stack: Vec::new() }
    };
    
    //AI slop (I am not writing it on my own)
    let program: Vec<Statement> = vec![
        // 1. let x = 10;
        Statement::Let {
            name: "x".to_string(),
            expr: Expr::Int(10),
        },
        
        // 2. let y = { let x = 2; x * 3 };
        Statement::Let {
            name: "y".to_string(),
            expr: Expr::Block(
                // Statements inside the block
                vec![
                    Statement::Let {
                        name: "x".to_string(), // Shadowing!
                        expr: Expr::Int(2),
                    }
                ],
                // Final expression of the block (implicit return)
                Some(Box::new(Expr::Binary {
                    left: Box::new(Expr::Var("x".to_string())),
                    operation: Operation::Multiply,
                    right: Box::new(Expr::Int(3)),
                }))
            ),
        },
    
        // 3. x + y (Expression Statement to verify result)
        Statement::Expr(Expr::Binary {
            left: Box::new(Expr::Var("x".to_string())), // Should be 10 (outer)
            operation: Operation::Add,
            right: Box::new(Expr::Var("y".to_string())), // Should be 6
        }),
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
