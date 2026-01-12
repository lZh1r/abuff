use chumsky::{prelude::*};

use crate::ast::{Expr, Operation, Statement};

pub fn parse<'src>() -> impl Parser<'src, &'src str, Vec<Statement>, extra::Err<Rich<'src, char>>> {
    recursive(|statement| {
        let expr = recursive(|expr| {
            let val = text::int(10)
                    .map(|s: &str| Box::new(Expr::Int(s.parse().unwrap())))
                    .padded();
            let var = text::ident().map(|s: &str| Box::new(Expr::Var(s.to_string()))).padded();
            
            let block = statement.clone()
                .repeated()
                .collect()
                .then(expr.clone().or_not())
                .delimited_by(just('{'), just('}'))
                .map(|(statements, tail)| Box::new(Expr::Block(statements, tail)))
                .padded();
            
            let func = text::keyword("fun")
                .ignore_then(text::ident().delimited_by(just('('), just(')')))
                .then(expr.clone())
                .map(|(param, body): (&str, Box<Expr>)| Box::new(Expr::Fun { 
                    param: param.to_string(), 
                    body: Box::new(*body) 
                }));
            
            let atom = val
                .or(func)
                .or(block)
                .or(var)
                .or(expr.clone().delimited_by(just('('), just(')')))
                .padded();
            
            let call = atom.clone()
                .foldl(
                    expr.clone()
                        .delimited_by(just('('), just(')'))
                        .padded()
                        .repeated(), 
                    |func, arg| Box::new(Expr::Call { 
                        fun: func, 
                        arg: Box::new(*arg) 
                    })
                );
            
            let op_mul = just('*').to(Operation::Multiply).or(just('/').to(Operation::Divide));
            let product = call.clone().foldl(
                op_mul.then(call).repeated(),
                |l, (op, r)| Box::new(Expr::Binary { left: l, operation: op, right: r })
            );
            
            let op_add = just('+').to(Operation::Add).or(just('-').to(Operation::Subtract));
            let sum = product.clone().foldl(
                op_add.then(product).repeated(),
                |l, (op, r)| Box::new(Expr::Binary { left: l, operation: op, right: r })
            );
        
            sum
        });
        
        let let_stmt = text::keyword("let")
            .ignore_then(text::ident().padded())
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .map(|(name, e): (&str, Box<Expr>)| Statement::Let { name: name.to_string(), expr: *e });
        
        let expr_stmt = expr.clone()
            .then_ignore(just(';'))
            .map(|expr| Statement::Expr(*expr));
        
        let_stmt.or(expr_stmt).padded()
    })
    .repeated()
    .collect()
    .then_ignore(end())
}