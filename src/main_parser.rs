use chumsky::prelude::*;

use crate::ast::{Expr, Operation, Statement, TypeInfo, UnaryOp, Spanned, Span};

enum PostfixOp {
    Call(Vec<Spanned<Expr>>),
    Access(String),
    Index(Box<Spanned<Expr>>)
}

fn single_line_comment<'src>() -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char>>> {
    just("//")
        .ignore_then(none_of("\n\r").repeated())
        .ignored()
}

fn multi_line_comment<'src>() -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char>>> {
    just("/*")
        .ignore_then(
            none_of("*")
                .or(just('*').ignore_then(none_of("/")))
                .repeated()
        )
        .ignore_then(just("*/"))
        .ignored()
}

//TODO: save these comments in their respective module for docs implementation
fn doc_comment<'src>() -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char>>> {
    just("///")
        .ignore_then(none_of("\n\r").repeated())
        .ignored()
}

fn comment<'src>() -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char>>> {
    choice((
        single_line_comment(),
        multi_line_comment(),
        doc_comment(),
    ))
}

fn whitespace_with_comments<'src>() -> Boxed<'src, 'src, &'src str, (), extra::Err<Rich<'src, char>>> {
    let ws = choice((
        just(' ').to(()),
        just('\t').to(()),
        just('\n').to(()),
        just('\r').to(()),
    ))
    .or(comment())
    .repeated()
    .ignored()
    .boxed();
    
    ws
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Statement>>, extra::Err<Rich<'src, char>>> {
    let type_parser = recursive(|ty| {
        let leaf = choice((
            text::keyword("Int").to(TypeInfo::Int),
            text::keyword("Bool").to(TypeInfo::Bool),
            text::keyword("Float").to(TypeInfo::Float),
            text::keyword("String").to(TypeInfo::String),
            text::keyword("Any").to(TypeInfo::Any),
        )).padded_by(whitespace_with_comments()).map_with(|s, extra| Spanned {
            inner: s,
            span: extra.span()
        });

        let arg = just("...").or_not().then(text::ident().padded_by(whitespace_with_comments()))
            .map(|(spread, name)| (spread.is_some(), name.to_string()))
            .then_ignore(just(':').padded_by(whitespace_with_comments()))
            .then(ty.clone()).padded_by(whitespace_with_comments());

        let fun = arg.separated_by(just(',')).allow_trailing().collect().or_not()
            .delimited_by(just('(').padded_by(whitespace_with_comments()), just(')').padded_by(whitespace_with_comments()))
            .then_ignore(just("->").padded_by(whitespace_with_comments()))
            .then(ty.clone())
            .map_with(|(input, output), extra| Spanned {
                inner: TypeInfo::Fun { params: input.unwrap_or_default(), return_type: Box::new(output) },
                span: extra.span()
            });

        let record = text::ident().map(|s: &str| s.to_string()).padded_by(whitespace_with_comments())
            .then_ignore(just(':').padded_by(whitespace_with_comments()))
            .then(ty.clone())
            .separated_by(just(',').padded_by(whitespace_with_comments()))
            .allow_trailing().collect()
            .delimited_by(just('{').padded_by(whitespace_with_comments()), just('}').padded_by(whitespace_with_comments()))
            .map_with(|s, extra| Spanned {
                inner: TypeInfo::Record(s),
                span: extra.span()
            }).padded_by(whitespace_with_comments());
        
        let enum_var = text::ident().padded_by(whitespace_with_comments())
            .then_ignore(just('.'))
            .then(text::ident().padded_by(whitespace_with_comments()))
            .map_with(|(enum_name, variant_name), extra| Spanned {
                inner: TypeInfo::EnumVariant { 
                    enum_name: enum_name.to_string(),
                    variant: variant_name.to_string()
                },
                span: extra.span()
            });

        let custom = text::ident().padded_by(whitespace_with_comments()).map_with(|s: &str, extra| Spanned {
            inner: TypeInfo::Custom(s.to_string()),
            span: extra.span()
        });

        let atom = choice((
            leaf,
            record,
            fun.clone().delimited_by(just('(').padded_by(whitespace_with_comments()), just(')').padded_by(whitespace_with_comments())),
            fun,
            enum_var,
            custom
        ));
        
        atom.foldl(
            just("[]").padded_by(whitespace_with_comments()).map_with(|_, e| e.span()).repeated(),
            |ty, bracket_span| Spanned {
                inner: TypeInfo::Array(Box::new(ty.clone())),
                span: (ty.span.start..bracket_span.end).into(), 
            }
        )
    });

    recursive(|statement| {
        let expr = recursive(|expr| {
            let string_literal = just('"')
                .ignore_then(
                    just('\\')
                        .ignore_then(
                            choice((
                                just('\\').to('\\'),
                                just('"').to('"'),
                                just('n').to('\n'),
                                just('r').to('\r'),
                                just('t').to('\t')
                            ))
                        )
                    .or(none_of("\\\"")) 
                    .repeated()
                    .collect::<String>()
                )
                .then_ignore(just('"'))
                .map_with(|s, extra| Spanned {
                    inner: Expr::String(s),
                    span: extra.span()
                })
                .padded_by(whitespace_with_comments());
            
            let boolean = choice((
                text::keyword("true").padded_by(whitespace_with_comments()).to(true),
                text::keyword("false").padded_by(whitespace_with_comments()).to(false)
            )).map_with(|b, extra| Spanned {
                inner: Expr::Bool(b),
                span: extra.span()
            });
            
            let int = text::int(10)
                .map_with(|i: &str, extra| Spanned {
                    inner: Expr::Int(i.parse().unwrap()),
                    span: extra.span()
                })
                .padded_by(whitespace_with_comments());

            let float = text::int(10)
                .then(just('.'))
                .then(text::int(10))
                .map_with(|((int_part, _), frac_part): ((&str, char), &str), extra| {
                    let s = format!("{}.{}", int_part, frac_part);
                    Spanned {
                        inner: Expr::Float(s.parse().unwrap()),
                        span: extra.span()
                    }
                    
                })
                .padded_by(whitespace_with_comments());

            let var = text::ident()
                .map_with(|s: &str, extra| Spanned {
                    inner: Expr::Var(s.to_string()),
                    span: extra.span()
                })
                .padded_by(whitespace_with_comments());

            let field = text::ident()
                .map(|s: &str| s.to_string())
                .then_ignore(just(':').padded_by(whitespace_with_comments()))
                .then(expr.clone())
                .padded_by(whitespace_with_comments());

            let record = field
                .separated_by(just(','))
                .allow_trailing()
                .collect()
                .delimited_by(just('{'), just('}'))
                .map_with(|fields, extra| Spanned {
                    inner: Expr::Record(fields),
                    span: extra.span()
                })
                .padded_by(whitespace_with_comments());

            let block = statement.clone()
                .repeated()
                .collect()
                .then(expr.clone().or_not().map(|o| o.map(Box::new)))
                .delimited_by(just('{'), just('}'))
                .map_with(|(statements, tail), extra| Spanned {
                    inner: Expr::Block(statements, tail),
                    span: extra.span()
                })
                .padded_by(whitespace_with_comments());

            let arg = just("...").or_not().then(text::ident().padded_by(whitespace_with_comments()))
                .map(|(spread, name)| (spread.is_some(), name.to_string()))
                .then_ignore(just(':').padded_by(whitespace_with_comments()))
                .then(type_parser.clone());

            let func = text::keyword("fun").padded_by(whitespace_with_comments())
                .ignore_then(
                    arg.separated_by(just(',')).allow_trailing().collect()
                        .delimited_by(just('(').padded_by(whitespace_with_comments()), just(')').padded_by(whitespace_with_comments()))
                )
                .then(
                    just(':').padded_by(whitespace_with_comments())
                        .ignore_then(type_parser.clone())
                        .or_not()
                )
                .then(block.clone())
                .map_with(|((params, return_type), body), extra| Spanned {
                    inner: Expr::Fun {
                        params,
                        return_type,
                        body: Box::new(body)
                    },
                    span: extra.span()
                });

            let if_else = text::keyword("if").padded_by(whitespace_with_comments())
                .ignore_then(expr.clone().delimited_by(just('(').padded_by(whitespace_with_comments()), just(')').padded_by(whitespace_with_comments())))
                .then(expr.clone())
                .then(text::keyword("else").ignore_then(expr.clone().map(Box::new)).or_not())
                .map_with(|((condition, body), else_block), extra| Spanned {
                    inner: Expr::If {condition: Box::new(condition), body: Box::new(body), else_block},
                    span: extra.span()
                });
            
            let while_loop = text::keyword("while").padded_by(whitespace_with_comments())
                .ignore_then(expr.clone().delimited_by(just('(').padded_by(whitespace_with_comments()), just(')').padded_by(whitespace_with_comments())))
                .then(expr.clone())
                .map_with(|(condition, body), extra| Spanned {
                    inner: Expr::While {condition: Box::new(condition), body: Box::new(body)},
                    span: extra.span()
                });

            let return_keyword = text::keyword("return").padded_by(whitespace_with_comments())
                .ignore_then(expr.clone())
                .map_with(|exp, extra| Spanned {
                    inner: Expr::Return(Box::new(exp)),
                    span: extra.span()
                });
            
            let control_flow = choice((
                text::keyword("break").padded_by(whitespace_with_comments()).map_with(|_, extra| Spanned {
                    inner: Expr::Break,
                    span: extra.span()
                }),
                text::keyword("continue").padded_by(whitespace_with_comments()).map_with(|_, extra| Spanned {
                    inner: Expr::Continue,
                    span: extra.span()
                })
            ));
            
            let array = just('[').padded()
                .ignore_then(
                    expr.clone().separated_by(just(',').padded()).allow_trailing().collect()
                ).then_ignore(just(']').padded())
                .map_with(|elements: Vec<Spanned<Expr>>, extra| Spanned {
                    inner: Expr::Array(elements),
                    span: extra.span()
                });
            
            let atom = choice((
                float,
                int,
                string_literal,
                boolean,
                return_keyword,
                control_flow,
                func,
                if_else,
                while_loop,
                record,
                block,
                array,
                var,
                expr.clone().delimited_by(just('('), just(')')).padded()
            )).boxed();
            
            // let index = atom.foldl(
            //     text::int(10).delimited_by(just('[').padded(), just(']').padded())
            //         .padded_by(whitespace_with_comments()).map_with(|i, e| Spanned {
            //             inner: i.parse().unwrap(),
            //             span: e.span()
            //         }).repeated(),
            //     |expr, index| Spanned {
            //         inner: Expr::Index(Box::new(expr), index.inner),
            //         span: index.span, 
            //     }
            // );

            let call = atom.clone()
                .foldl(
                    choice((
                        expr.clone()
                            .padded_by(whitespace_with_comments())
                            .separated_by(just(','))
                            .allow_trailing()
                            .collect()
                            .delimited_by(just('(').padded_by(whitespace_with_comments()), just(')').padded_by(whitespace_with_comments()))
                            .map_with(|args, extra| (PostfixOp::Call(args), extra.span())),
                        just('.')
                            .padded_by(whitespace_with_comments())
                            .ignore_then(text::ident().padded_by(whitespace_with_comments()))
                            .map_with(|field: &str, extra| (PostfixOp::Access(field.to_string()), extra.span())),
                        expr.clone()
                            .padded_by(whitespace_with_comments())
                            .delimited_by(just('[').padded_by(whitespace_with_comments()), just(']').padded_by(whitespace_with_comments()))
                            .map_with(|index_expr, extra| (PostfixOp::Index(Box::new(index_expr)), extra.span())),
                    )).repeated(),
                    |parent: Spanned<Expr>, (op, op_span): (PostfixOp, Span)| {
                        let new_span = Span::new((), parent.span.start..op_span.end);
                        match op {
                            PostfixOp::Call(args) => Spanned {
                                inner: Expr::Call {
                                    fun: Box::new(parent),
                                    args
                                },
                                span: new_span
                            },
                            PostfixOp::Access(field) => Spanned {
                                inner: Expr::Get(
                                    Box::new(parent),
                                    field
                                ),
                                span: new_span
                            },
                            PostfixOp::Index(index_expr) => Spanned {
                                inner: Expr::Index(
                                    Box::new(parent),
                                    index_expr
                                ),
                                span: new_span
                            },
                        }
                    }
                );

            let unary = choice((
                    just('-').to(UnaryOp::Negate),
                    just('!').to(UnaryOp::Not),
                ))
                .map_with(|op, extra| (op, extra.span()))
                .repeated()
                .foldr(call, |(op, op_span): (UnaryOp, Span), rhs: Spanned<Expr>| {
                    let new_span = Span::new((), op_span.start..rhs.span.end);
                    Spanned {
                        inner: Expr::Unary(op, Box::new(rhs)),
                        span: new_span
                    }
                });

            let op_mul = just('*').to(Operation::Multiply).or(just('/').to(Operation::Divide))
                .or(just('%').to(Operation::Modulo));
            let product = unary.clone().foldl(
                op_mul.then(unary).repeated(),
                |l: Spanned<Expr>, (op, r): (Operation, Spanned<Expr>)| {
                    let new_span = Span::new((), l.span.start..r.span.end);
                    Spanned {
                        inner: Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) },
                        span: new_span
                    }
                }
            );

            let op_add = just('+').to(Operation::Add).or(just('-').to(Operation::Subtract));
            let sum = product.clone().foldl(
                op_add.then(product).repeated(),
                |l: Spanned<Expr>, (op, r): (Operation, Spanned<Expr>)| {
                    let new_span = Span::new((), l.span.start..r.span.end);
                    Spanned {
                        inner: Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) },
                        span: new_span
                    }
                }
            );

            let op_comparison = just(">=").to(Operation::GreaterThanEq)
                .or(just("<=").to(Operation::LessThanEq))
                .or(just('>').to(Operation::GreaterThan))
                .or(just('<').to(Operation::LessThan))
                .or(just("==").to(Operation::Eq))
                .or(just("!=").to(Operation::NotEq));
            let comparison = sum.clone().foldl(
                op_comparison.then(sum).repeated(),
                |l: Spanned<Expr>, (op, r): (Operation, Spanned<Expr>)| {
                    let new_span = Span::new((), l.span.start..r.span.end);
                    Spanned {
                        inner: Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) },
                        span: new_span
                    }
                }
            );

            let and_logic = comparison.clone().foldl(
                just("&&").to(Operation::And).then(comparison).repeated(),
                |l: Spanned<Expr>, (op, r): (Operation, Spanned<Expr>)| {
                    let new_span = Span::new((), l.span.start..r.span.end);
                    Spanned {
                        inner: Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) },
                        span: new_span
                    }
                }
            );

            let or_logic = and_logic.clone().foldl(
                just("||").to(Operation::Or).then(and_logic).repeated(),
                |l: Spanned<Expr>, (op, r): (Operation, Spanned<Expr>)| {
                    let new_span = Span::new((), l.span.start..r.span.end);
                    Spanned {
                        inner: Expr::Binary { left: Box::new(l), operation: op, right: Box::new(r) },
                        span: new_span
                    }
                }
            );
            
            let math_assign = or_logic.clone()
                .then(
                    choice((
                        just("+=").padded().to(Operation::Add),
                        just("-=").padded().to(Operation::Subtract),
                        just("*=").padded().to(Operation::Multiply),
                        just("/=").padded().to(Operation::Divide),
                        just("%=").padded().to(Operation::Modulo),
                    ))
                    .then(expr.clone()).or_not()
                )
                .map_with(|(lhs, opt): (Spanned<Expr>, Option<(Operation, Spanned<Expr>)>), extra| {
                    match opt {
                        Some((op, rhs)) => {
                            let binary_span = Span::new((), lhs.span.start..rhs.span.end);
                            Spanned {
                                inner: Expr::Assign {
                                    target: Box::new(lhs.clone()),
                                    value: Box::new(Spanned {
                                        inner: Expr::Binary {
                                            left: Box::new(lhs),
                                            operation: op,
                                            right: Box::new(rhs)
                                        },
                                        span: binary_span
                                    }),
                                },
                                span: extra.span()
                            }
                        },
                        None => lhs
                    }
                });

            let assign = math_assign.clone()
                .then(
                    just('=').padded()
                        .ignore_then(expr.clone())
                        .or_not()
                )
                .map_with(|(lhs, rhs_opt): (Spanned<Expr>, Option<Spanned<Expr>>), extra| {
                    match rhs_opt {
                        Some(rhs) => Spanned {
                            inner: Expr::Assign { target: Box::new(lhs), value: Box::new(rhs) },
                            span: extra.span()
                        },
                        None => lhs,
                    }
                });

            assign
        });

        let let_stmt = text::keyword("let")
            .ignore_then(text::ident().padded_by(whitespace_with_comments()))
            .then(just(':').padded_by(whitespace_with_comments()).ignore_then(type_parser.clone()).or_not())
            .then_ignore(just('=').padded_by(whitespace_with_comments()))
            .then(expr.clone())
            .then_ignore(just(';'))
            .map_with(|((name, type_name), e): ((&str, Option<Spanned<TypeInfo>>), Spanned<Expr>), extra| Spanned {
                inner: Statement::Let { name: name.to_string(), type_info: type_name, expr: e },
                span: extra.span()
            });
        
        let enum_def = text::keyword("enum")
            .ignore_then(text::ident().padded_by(whitespace_with_comments()))
            .then(
                text::ident()
                    .then(
                        just(':')
                            .padded_by(whitespace_with_comments())
                            .ignore_then(type_parser.clone())
                            .or_not()
                    )
                    .map(|(variant_name, variant_type)| {
                        (variant_name.to_string(), variant_type) 
                    })
                    .separated_by(just(',').padded_by(whitespace_with_comments()))
                    .allow_trailing()
                    .collect()
                    .delimited_by(
                        just('{').padded_by(whitespace_with_comments()), 
                        just('}').padded_by(whitespace_with_comments())
                    )
            )
            .map_with(|(name, variants), extra| Spanned {
                inner: Statement::EnumDef { 
                    name: name.to_string(), 
                    variants: variants
                },
                span: extra.span()
            });

        let type_def = text::keyword("type")
            .ignore_then(text::ident().padded_by(whitespace_with_comments()))
            .then_ignore(just('=').padded_by(whitespace_with_comments()))
            .then(type_parser.clone())
            .then_ignore(just(';'))
            .map_with(|(type_name, type_info): (&str, Spanned<TypeInfo>), extra| Spanned {
                inner: Statement::TypeDef { name: type_name.to_string(), type_info },
                span: extra.span()
            });
        
        let arg = just("...").or_not().then(text::ident().padded_by(whitespace_with_comments()))
            .map(|(spread, name)| (spread.is_some(), name.to_string()))
            .then_ignore(just(':').padded_by(whitespace_with_comments()))
            .then(type_parser.clone());
        
        let fun_def = text::keyword("fun")
            .ignore_then(text::ident().padded_by(whitespace_with_comments()))
            .then(
                arg.clone().separated_by(just(',')).allow_trailing().collect()
                    .delimited_by(just('(').padded_by(whitespace_with_comments()), just(')').padded_by(whitespace_with_comments()))
            )
            .then(
                just(':').padded_by(whitespace_with_comments())
                    .ignore_then(type_parser.clone())
                    .or_not()
            )
            .then(expr.clone())
            .then_ignore(just(';').padded_by(whitespace_with_comments()))
            .map_with(|(((name, params), return_type), body): (((&str, Vec<((bool, String), Spanned<TypeInfo>)>), Option<Spanned<TypeInfo>>), Spanned<Expr>), extra| Spanned {
                inner: Statement::Fun {
                    name: name.to_string(),
                    params,
                    body,
                    return_type
                },
                span: extra.span()
            });

        let expr_stmt = expr.clone()
            .then_ignore(just(';'))
            .map_with(|expr, extra| Spanned {
                inner: Statement::Expr(expr),
                span: extra.span()
            });
        
        let import_stmt = text::keyword("import").padded_by(whitespace_with_comments())
            .ignore_then(
                    just('{').padded_by(whitespace_with_comments()).ignore_then(
                        just("type").map(|_| true).or_not()
                            .then(text::ident().map(ToString::to_string).padded_by(whitespace_with_comments()))
                            .then(
                                just("as").padded_by(whitespace_with_comments())
                                    .ignore_then(text::ident().map(ToString::to_string).padded_by(whitespace_with_comments())).or_not()
                            )
                            .map_with(|((is_type, symbol), alias), extra| (
                                Spanned {
                                    inner: symbol,
                                    span: extra.span()
                                },
                                alias,
                                match is_type {
                                    None => false,
                                    _ => true
                                }
                            ))
                            .separated_by(just(','))
                            .allow_trailing()
                            .collect()
                        )
                )
                .then_ignore(just('}').padded_by(whitespace_with_comments()))
                .then_ignore(just("from").padded_by(whitespace_with_comments()))
                .then(
                    just('"')
                        .ignore_then(
                            just('\\')
                                .ignore_then(
                                    choice((
                                        just('\\').to('\\'),
                                        just('"').to('"'),
                                        just('n').to('\n'),
                                        just('r').to('\r'),
                                        just('t').to('\t')
                                    ))
                                )
                                .or(none_of("\\\""))
                                .repeated()
                                .collect::<String>()
                        )
                        .then_ignore(just('"'))
                        .map_with(|s, extra| Spanned {
                            inner: s,
                            span: extra.span()
                        })
                        .padded_by(whitespace_with_comments())
                )
                .then_ignore(just(';').padded_by(whitespace_with_comments()))
                .map_with(|(symbols, src), extra| Spanned {
                    inner: Statement::Import {
                        symbols,
                        path: src
                    },
                    span: extra.span()
                });
        
        let native_fun = text::keyword("native").padded_by(whitespace_with_comments())
            .ignore_then(
                text::keyword("fun")
                    .ignore_then(text::ident().padded_by(whitespace_with_comments()))
                    .then(
                        arg.separated_by(just(',')).allow_trailing().collect()
                            .delimited_by(just('(').padded_by(whitespace_with_comments()), just(')').padded_by(whitespace_with_comments()))
                    )
                    .then(
                        just(':').padded_by(whitespace_with_comments())
                            .ignore_then(type_parser.clone())
                            .or_not()
                    )
                    .then_ignore(just(';').padded_by(whitespace_with_comments()))
            ).map_with(|((name, args), r_type): ((&str, Vec<((bool, String), Spanned<TypeInfo>)>), Option<Spanned<TypeInfo>>), extra| Spanned {
                inner: Statement::NativeFun {
                    name: name.into(),
                    params: args,
                    return_type: r_type
                },
                span: extra.span()
            });
        
        let export_stmt = text::keyword("export").padded_by(whitespace_with_comments())
            .ignore_then(choice((
                let_stmt.clone(),
                type_def.clone(),
                fun_def.clone(),
                enum_def.clone(),
                native_fun
            ))).map_with(|st, extra| Spanned {
                inner: Statement::Export(Box::new(st)),
                span: extra.span()
            });

        choice((
            import_stmt,
            export_stmt,
            let_stmt,
            fun_def,
            type_def,
            enum_def,
            expr_stmt
        )).padded_by(whitespace_with_comments()).boxed()
    })
    .repeated()
    .collect()
    .then_ignore(end())
}