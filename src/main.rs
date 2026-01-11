use std::{collections::HashMap};

use crate::ast::Expr;

mod ast;

fn eval(expr: &Expr, env: &mut HashMap<String, i32>) -> i32 {
    match expr {
        Expr::Int(n) => *n,
        Expr::Var(v) => *(env.get(v).unwrap()),
        Expr::Add(expr, expr1) => eval(expr, env) + eval(expr1, env),
        Expr::Let{name, expr, body} => {
            let v = eval(expr, env);
            env.insert(name.clone(), v);
            let res = eval(body, env);
            env.remove(name);
            res
        },
    }
}

fn main() {
    let mut stack: HashMap<String, i32> = HashMap::new();
    let res = eval(
        &Expr::Let {
            name: "h".to_string(),
            expr: Box::new(Expr::Int(10)),
            body: Box::new(Expr::Add(
                Box::new(Expr::Var("h".to_string())), Box::new(Expr::Int(7)))
            )
        },
        &mut stack
    );
    println!("{}", res);
}
