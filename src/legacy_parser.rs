use chumsky::{prelude::*};

use crate::ir::{Expr, Operation, Statement};

enum PostfixOp<'a> {
    Call(Vec<Expr>),
    Access(&'a str),
}

pub fn parse<'src>() -> impl Parser<'src, &'src str, Vec<Statement>, extra::Err<Rich<'src, char>>> {
    recursive(|statement| {
        let expr = recursive(|expr| {
            let int = text::int(10)
                    .map(|s: &str| Expr::Int(s.parse().unwrap()))
                    .padded();
            
            let float = text::int(10)
                .then(just('.'))
                .then(text::int(10))
                .map(|((int_part, _), frac_part): ((&str, char), &str)| {
                    let s = format!("{}.{}", int_part, frac_part);
                    Expr::Float(s.parse().unwrap())
                })
                .padded();
            
            let var = text::ident().map(|s: &str| Expr::Var(s.to_string())).padded();
            
            let field = text::ident()
                .map(|s: &str| s.to_string())
                .then_ignore(just(':').padded())
                .then(expr.clone())
                .padded();
            
            let record = field
                .separated_by(just(','))
                .allow_trailing()
                .collect()
                .delimited_by(just('{'), just('}'))
                .map(Expr::Record)
                .padded();
            
            let block = statement.clone()
                .repeated()
                .collect()
                .then(expr.clone().or_not().map(|o| o.map(Box::new)))
                .delimited_by(just('{'), just('}'))
                .map(|(statements, tail)| Expr::Block(statements, tail))
                .padded();
            
            let func = text::keyword("fun")
                .padded()
                .ignore_then(
                    text::ident()
                        .padded()
                        .map(|s: &str| s.to_string())
                        .separated_by(just(','))
                        .allow_trailing()
                        .collect()
                        .delimited_by(just('(').padded(), just(')').padded()))
                .then(expr.clone())
                .map(|(param, body)| Expr::Fun { 
                    params: param, 
                    body: Box::new(body) 
                });
            
            let atom = float
                .or(int)
                .or(func)
                .or(record)
                .or(block)
                .or(var)
                .or(expr.clone().delimited_by(just('('), just(')')))
                .padded();
            
            let call = atom.clone()
                .foldl(
                    choice((
                        expr.clone()
                            .padded()
                            .separated_by(just(','))
                            .allow_trailing()
                            .collect()
                            .delimited_by(just('(').padded(), just(')').padded())
                            .map(PostfixOp::Call),
                        just('.')
                            .padded()
                            .ignore_then(text::ident().padded())
                            .map(PostfixOp::Access),
                    )).repeated(),
                    |parent, op| match op {
                        PostfixOp::Call(args) => Expr::Call { 
                            fun: Box::new(parent), 
                            args 
                        },
                        PostfixOp::Access(field) => Expr::Get(
                            Box::new(parent), 
                            field.to_string()
                        ),
                    }
                );
            
            let op_mul = just('*').to(Operation::Multiply).or(just('/').to(Operation::Divide));
            let product = call.clone().foldl(
                op_mul.then(call).repeated(),
                |l, (op, r)| Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) }
            );
            
            let op_add = just('+').to(Operation::Add).or(just('-').to(Operation::Subtract));
            let sum = product.clone().foldl(
                op_add.then(product).repeated(),
                |l, (op, r)| Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) }
            );
        
            sum
        });
        
        let let_stmt = text::keyword("let")
            .ignore_then(text::ident().padded())
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .map(|(name, e): (&str, Expr)| Statement::Let { name: name.to_string(), expr: e });
        
        let expr_stmt = expr.clone()
            .then_ignore(just(';'))
            .map(|expr| Statement::Expr(expr));
        
        let_stmt.or(expr_stmt).padded()
    })
    .repeated()
    .collect()
    .then_ignore(end())
}