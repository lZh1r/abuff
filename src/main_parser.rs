use chumsky::{prelude::*};

use crate::ast::{Expr, Operation, Statement, TypeInfo};

enum PostfixOp<'a> {
    Call(Vec<Expr>),
    Access(&'a str),
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Vec<Statement>, extra::Err<Rich<'src, char>>> {
    let type_parser = recursive(|ty| {
        let leaf = text::keyword("Int").map(|s: &str| s.to_string()).to(TypeInfo::Int)
            .or(text::keyword("Bool").map(|s: &str| s.to_string()).to(TypeInfo::Bool))
            .or(text::keyword("Float").map(|s: &str| s.to_string()).to(TypeInfo::Float))
            .or(text::keyword("String").map(|s: &str| s.to_string()).to(TypeInfo::String))
            .padded();
        
        let arg = text::ident().padded().map(ToString::to_string)
                        .then_ignore(just(':').padded())
                        .then(ty.clone()).padded();

        let fun = arg.separated_by(just(',')).allow_trailing().collect().or_not()
            .delimited_by(just('(').padded(), just(')').padded())
            .then_ignore(just("->").padded())
            .then(ty.clone())
            .map(|(input, output)| TypeInfo::Fun { args: input.unwrap_or_default(), return_type: Box::new(output) });
        
        let record = text::ident().map(|s: &str| s.to_string()).padded()
            .then_ignore(just(':').padded())
            .then(ty.clone())
            .separated_by(just(',').padded())
            .allow_trailing().collect()
            .delimited_by(just('{').padded(), just('}').padded())
            .map(TypeInfo::Record).padded();
        
        let custom = text::ident().padded().map(|s: &str| TypeInfo::Custom(s.to_string()));

        leaf.or(record).or(fun).or(custom)
    });
    
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
            
            let arg = text::ident().padded().map(ToString::to_string)
                            .then_ignore(just(':').padded())
                            .then(type_parser.clone());
            
            let func = text::keyword("fun").padded()
                .ignore_then(
                    arg.separated_by(just(',')).allow_trailing().collect()
                        .delimited_by(just('(').padded(), just(')').padded())
                )
                .then(
                    just("->").padded()
                        .ignore_then(type_parser.clone())
                        .or_not()
                )
                .then(expr.clone())
                .map(|((params, return_type), body): ((Vec<(String, TypeInfo)>, Option<TypeInfo>), Expr)| Expr::Fun { 
                    params, 
                    return_type,
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
            
            let op_mul = just('*').to(Operation::Multiply).or(just('/').to(Operation::Divide))
                .or(just('%').to(Operation::Modulo));
            let product = call.clone().foldl(
                op_mul.then(call).repeated(),
                |l, (op, r)| Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) }
            );
            
            let op_add = just('+').to(Operation::Add).or(just('-').to(Operation::Subtract));
            let sum = product.clone().foldl(
                op_add.then(product).repeated(),
                |l, (op, r)| Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) }
            );
            
            let op_comparison = just(">=").to(Operation::GreaterThanEq)
                .or(just("<=").to(Operation::LessThanEq))
                .or(just('>').to(Operation::GreaterThan))
                .or(just('<').to(Operation::LessThan))
                .or(just("&&").to(Operation::And))
                .or(just("||").to(Operation::Or))
                .or(just("==").to(Operation::Eq))
                .or(just("!=").to(Operation::NotEq));
            let comparison = sum.clone().foldl(
                op_comparison.then(sum).repeated(),
                |l, (op, r)| Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) }
            );
            
            let assign = comparison.clone()
                .then(
                    just('=').padded()
                        .ignore_then(expr.clone())
                        .or_not()
                )
                .map(|(lhs, rhs_opt)| match rhs_opt {
                        Some(rhs) => Expr::Assign { target: Box::new(lhs), value: Box::new(rhs) },
                        None => lhs,
                });
            
            assign
        });
        
        let let_stmt = text::keyword("let")
            .ignore_then(text::ident().padded())
            .then(just(':').padded().ignore_then(type_parser.clone()).or_not())
            .then_ignore(just('=').padded())
            .then(expr.clone())
            .then_ignore(just(';'))
            .map(|((name, type_name), e): ((&str, Option<TypeInfo>), Expr)| Statement::Let { name: name.to_string(), type_info: type_name, expr: e });
        
        let type_def = text::keyword("type")
            .ignore_then(text::ident().padded())
            .then_ignore(just('=').padded())
            .then(type_parser.clone())
            .then_ignore(just(';'))
            .map(|(type_name, type_info): (&str, TypeInfo)| Statement::TypeDef { name: type_name.to_string(), type_info });
        
        let expr_stmt = expr.clone()
            .then_ignore(just(';'))
            .map(|expr| Statement::Expr(expr));
        
        let_stmt.or(type_def).or(expr_stmt).padded()
    })
    .repeated()
    .collect()
    .then_ignore(end())
}