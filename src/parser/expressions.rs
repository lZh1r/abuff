use smol_str::SmolStr;

use crate::{ast::{shared::{Operation, UnaryOp}, typed::{Expr, MatchArm, Statement, TypeInfo}}, lexer::Token, parser::main_parser::Parser, span::{Span, Spanned, spanned}};

impl<'a> Parser<'a> {
    pub(super) fn parse_expression(&mut self) -> Result<Spanned<Expr>, Spanned<SmolStr>> {
        self.parse_expression_bp(1)
    }

    fn parse_atom(&mut self) -> Result<Spanned<Expr>, Spanned<SmolStr>> {
        let prev_token_span = self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0));
        let token = self
            .advance()
            .ok_or(spanned(
                "Unexpected EOF".into(),
                prev_token_span
            ))?
            .clone();

        match token.inner.clone() {
            Token::If => {
                // parse: if (cond) body [else ...]
                // require parentheses around condition
                self.expect(&Token::LParen)?;
                let cond = self.parse_expression()?;
                self.expect(&Token::RParen)?;

                // parse body: either block or single expression
                let body = if self.check(&Token::LBrace) {
                    self.parse_block_expr()?
                } else {
                    let e = self.parse_expression()?;
                    e
                };

                // optional else
                let else_block = if self.check(&Token::Else) {
                    self.advance(); // consume else
                    if self.check(&Token::If) {
                        // else if -> parse as atom
                        let e = self.parse_atom()?;
                        Some(Box::new(e))
                    } else if self.check(&Token::LBrace) {
                        let b = self.parse_block_expr()?;
                        Some(Box::new(b))
                    } else {
                        let e = self.parse_expression()?;
                        Some(Box::new(e))
                    }
                } else {
                    None
                };

                let end_span = else_block
                    .as_ref()
                    .map(|b| b.span)
                    .unwrap_or(body.span);
                let combined_span = Span { start: token.span.start, end: end_span.end };
                Ok(spanned(
                    Expr::If {
                        condition: Box::new(cond),
                        body: Box::new(body),
                        else_block,
                    },
                    combined_span,
                ))
            }
            Token::While => {
                // parse: while (cond) body
                self.expect(&Token::LParen)?;
                let cond = self.parse_expression()?;
                self.expect(&Token::RParen)?;

                let body = if self.check(&Token::LBrace) {
                    self.parse_block_expr()?
                } else {
                    self.parse_expression()?
                };

                let combined_span = Span { start: token.span.start, end: body.span.end };
                Ok(spanned(
                    Expr::While {
                        condition: Box::new(cond),
                        body: Box::new(body),
                    },
                    combined_span,
                ))
            }
            Token::For => {
                //for (<pattern> in iterable) body
                self.expect(&Token::LParen)?;
                let start = self.peek_at(-1).unwrap().span.start;
                
                let pattern = self.parse_let_pattern()?;
                self.expect(&Token::In)?;
                let iterable = Box::new(self.parse_expression()?);
                self.expect(&Token::RParen)?;
                let body = Box::new(self.parse_expression()?);
                
                let end = body.span.end;
                
                Ok(spanned(
                    Expr::For { 
                        element: pattern,
                        iterable,
                        body
                    },
                    Span::from(start..end)
                ))
            }
            Token::Return => {
                // return expr
                if self.peek().is_some() {
                    let expr = self.parse_expression()?;
                    let combined_span = Span { start: token.span.start, end: expr.span.end };
                    Ok(spanned(Expr::Return(Box::new(expr)), combined_span))
                } else {
                    Err(spanned("Unexpected EOF".into(), Span::at(token.span.end)))
                }
            }
            Token::Break => Ok(spanned(Expr::Break, token.span)),
            Token::Continue => Ok(spanned(Expr::Continue, token.span)),
            Token::Panic => {
                if self.peek().is_some() {
                    if self.check(&Token::Semicolon) {
                        Ok(spanned(Expr::Panic(None), token.span))
                    } else {
                        let expr = self.parse_expression()?;
                        let combined_span = Span { start: token.span.start, end: expr.span.end };
                        Ok(spanned(Expr::Panic(Some(Box::new(expr))), combined_span))
                    }

                } else {
                    Err(spanned("Unexpected EOF".into(), Span::at(token.span.end)))
                }
            },
            Token::False => Ok(spanned(Expr::Bool(false), token.span)),
            Token::True => Ok(spanned(Expr::Bool(true), token.span)),
            Token::Null => Ok(spanned(Expr::Null, token.span)),
            Token::Ident(v) => Ok(spanned(Expr::Var(v), token.span)),
            Token::Int(i) => Ok(spanned(Expr::Int(i), token.span)),
            Token::Float(f) => Ok(spanned(Expr::Float(f), token.span)),
            Token::String(s) => Ok(spanned(Expr::String(s), token.span)),
            Token::Char(c) => Ok(spanned(Expr::Char(c), token.span)),
            Token::Minus => {
                if self.peek().is_some() {
                    let expr = self.parse_expression()?;
                    let combined_span = Span { start: token.span.start, end: expr.span.end };
                    Ok(spanned(Expr::Unary(UnaryOp::Negate, Box::new(expr)), combined_span))
                } else {
                    Err(spanned("Unexpected EOF".into(), Span::at(token.span.end)))
                }
            }
            Token::Bang => {
                if self.peek().is_some() {
                    let expr = self.parse_expression()?;
                    let combined_span = Span { start: token.span.start, end: expr.span.end };
                    Ok(spanned(Expr::Unary(UnaryOp::Not, Box::new(expr)), combined_span))
                } else {
                    Err(spanned("Unexpected EOF".into(), Span::at(token.span.end)))
                }
            }
            Token::LParen => {
                let expr = self.parse_expression()?;
                let prev_token_span = self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
                let rspan = self.peek().map(|s| s.span)
                    .ok_or(spanned("Unexpected EOF".into(), prev_token_span))?;
                if self.check(&Token::Comma) {
                    // tuple
                    self.advance();
                    let rspan = self.peek().map(|s| s.span)
                        .ok_or(spanned("Unexpected EOF".into(), prev_token_span))?;
                    if self.check(&Token::RParen) {
                        return Err(spanned(
                            "Expected an expression, found RParen".into(),
                            rspan
                        ))
                    }
                    let mut elements = vec![Box::new(expr)];

                    loop {
                        if self.check(&Token::RParen) {
                            break
                        }
                        let expr = self.parse_expression()?;
                        elements.push(Box::new(expr));
                        if self.check(&Token::Comma) {
                            self.advance();
                            let rspan = self.peek().map(|s| s.span)
                                .ok_or(spanned("Unexpected EOF".into(), prev_token_span))?;
                            if self.check(&Token::RParen) {
                                return Err(spanned(
                                    "Expected an expression, found RParen".into(),
                                    rspan
                                ))
                            }
                            continue
                        } else {
                            break
                        }
                    }

                    self.expect(&Token::RParen)?;
                    let combined_span = Span { start: token.span.start, end: rspan.end };
                    Ok(spanned(Expr::Tuple(elements), combined_span))
                } else {
                    // grouped expression
                    self.expect(&Token::RParen)?;
                    let combined_span = Span { start: token.span.start, end: rspan.end };
                    Ok(spanned(expr.inner, combined_span))
                }
            }
            Token::LBracket => {
                // array literal
                let mut elements: Vec<Spanned<Expr>> = Vec::new();
                if !self.check(&Token::RBracket) {
                    loop {
                        let e = self.parse_expression()?;
                        elements.push(e);
                        if self.check(&Token::Comma) {
                            self.advance();
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                let prev_token_span = self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
                let end_span = self.peek().map(|s| s.span)
                    .ok_or(spanned("Unexpected EOF".into(), prev_token_span))?;
                self.expect(&Token::RBracket)?;
                let combined_span = Span { start: token.span.start, end: end_span.end };
                Ok(spanned(Expr::Array(elements), combined_span))
            }
            Token::LBrace => {
                if let (Some(next), Some(next2)) = (self.peek(), self.peek_at(1)) {
                    match (&next.inner, &next2.inner) {
                        (Token::Ident(_), Token::Colon) => {
                            // record
                            let mut fields: Vec<(SmolStr, Spanned<Expr>)> = Vec::new();
                            while !self.check(&Token::RBrace) {
                                // expect identifier
                                let prev_token_span = self.peek_at(-1)
                                    .map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
                                let name_tok = self.advance()
                                    .ok_or(spanned("Unexpected EOF".into(), prev_token_span))?;
                                let name = match &name_tok.inner {
                                    Token::Ident(s) => s.clone(),
                                    _ => return Err(spanned(
                                        "Expected identifier in record".into(),
                                        name_tok.span
                                    )),
                                };
                                self.expect(&Token::Colon)?;
                                let val = self.parse_expression()?;
                                fields.push((name, val));
                                if self.check(&Token::Comma) {
                                    self.advance();
                                    continue;
                                } else {
                                    break;
                                }
                            }
                            let prev_token_span = self.peek_at(-1)
                                .map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
                            let end_span = self.peek().map(|s| s.span)
                                .ok_or(spanned("Unexpected EOF".into(), prev_token_span))?;
                            self.expect(&Token::RBrace)?;
                            let combined_span = Span { start: token.span.start, end: end_span.end };
                            Ok(spanned(Expr::Record(fields), combined_span))
                        }
                        _ => {
                            // block
                            self.cursor -= 1; //since we consume the token here AND the block parser does that we have to put this here
                            let block_expr = self.parse_block_expr()?;
                            Ok(block_expr)
                        }
                    }
                } else {
                    // if no lookahead, treat as block (likely error, but try)
                    self.cursor -= 1; //since we consume the token here AND the block parser does that we have to put this here
                    let block_expr = self.parse_block_expr()?;
                    Ok(block_expr)
                }
            }
            Token::Fun => {
                // function literal: fun [<T,U>] (params[: Type]) [: ReturnType] body
                // optional generic params: <T, U>
                let mut generic_params: Vec<Spanned<SmolStr>> = Vec::new();
                if self.check(&Token::Lt) {
                    // consume '<'
                    let lt = self.advance().unwrap().clone();
                    loop {
                        let g = self.advance().ok_or(spanned("Unexpected EOF in generic params".into(), lt.span))?.clone();
                        match g.inner {
                            Token::Ident(name) => generic_params.push(spanned(name, g.span)),
                            _ => return Err(spanned("Expected identifier in generic params".into(), g.span)),
                        }
                        if self.check(&Token::Comma) {
                            self.advance();
                            continue;
                        }
                        break;
                    }
                    // expect '>'
                    if !self.check(&Token::Gt) {
                        return Err(spanned("Expected > after generic params".into(), self.peek().unwrap().span));
                    }
                    let _gt = self.advance().unwrap().clone();
                }

                let params = self.parse_fn_params()?;

                // optional return type: ':' Type
                let return_type: Option<Spanned<TypeInfo>> = if self.check(&Token::Colon) {
                    self.advance(); // consume ':'
                    Some(self.parse_type()?)
                } else {
                    None
                };

                // parse body: either a block or a single expression
                let body = if self.check(&Token::LBrace) {
                    self.parse_block_expr()?
                } else {
                    self.parse_expression()?
                };

                let combined_span = Span { start: token.span.start, end: body.span.end };
                Ok(spanned(
                    Expr::Fun {
                        params,
                        body: Box::new(body),
                        return_type,
                        generic_params,
                    },
                    combined_span,
                ))
            },
            Token::Match => {
                let target = self.parse_expression()?;
                self.expect(&Token::LBrace)?;

                let mut branches = Vec::new();
                loop {
                    if self.check(&Token::RBrace) {
                        break
                    }
                    // simplified pattern parsing: delegate to helper
                    let pattern = self.parse_pattern()?;
                    self.expect(&Token::Arrow)?;
                    let branch = self.parse_expression()?;
                    branches.push((pattern, branch));
                    if !self.check(&Token::Comma) {
                        break
                    } else {
                        self.advance();
                    };
                }

                let end = self.advance().unwrap();
                Ok(spanned(
                    Expr::Match { target: Box::new(target), branches },
                    Span::from(token.span.start..end.span.end)
                ))
            },
            Token::Semicolon => Ok(spanned(Expr::Void, token.span)),
            // Token::DotDotDot => todo!(), TODO: spread operator later
            _ => Err(spanned(
                format!("Expected expression, got {:?}", token.inner).into(),
                token.span,
            )),
        }
    }

    // helper used when parsing match branches; handles tuples and nested
    // patterns such as enum constructors, guards, and simple bindings.
    fn parse_pattern(&mut self) -> Result<Spanned<MatchArm>, Spanned<SmolStr>> {
        let token = self.peek().ok_or(spanned(
            "Unexpected EOF in pattern".into(),
            Span::from(0..0),
        ))?.clone();
        match &token.inner {
            Token::Ident(ident) => {
                let start = self.advance().unwrap().clone();
                if self.check(&Token::Dot) {
                    // enum constructor with explicit enum name
                    self.advance(); // consume '.'
                    let variant_tok = self.advance()
                        .ok_or(spanned("Unexpected EOF in pattern".into(), start.span))?
                        .clone();
                    let variant = match variant_tok.inner {
                        Token::Ident(v) => v,
                        _ => {
                            return Err(spanned(
                                "Expected variant name in pattern".into(),
                                variant_tok.span,
                            ))
                        }
                    };
                    self.expect(&Token::LParen)?;
                    if self.peek().is_none() {
                        return Err(spanned(
                            "Unexpected EOF in pattern".into(),
                            Span::from(0..0),
                        ));
                    }
                    let alias_tok = self.advance().unwrap().clone();
                    let alias = match alias_tok.inner {
                        Token::Ident(a) => a,
                        _ => {
                            return Err(spanned(
                                "Expected identifier for alias in pattern".into(),
                                alias_tok.span,
                            ))
                        }
                    };
                    let end = self.peek()
                        .ok_or(spanned("Unexpected EOF in pattern".into(), alias_tok.span))?
                        .clone();
                    self.expect(&Token::RParen)?;
                    Ok(spanned(
                        MatchArm::EnumConstructor {
                            enum_name: Some(ident.clone()),
                            variant,
                            alias,
                        },
                        Span::from(start.span.start..end.span.end),
                    ))
                } else if self.check(&Token::If) {
                    self.advance();
                    let condition = self.parse_expression()?;
                    Ok(spanned(
                        MatchArm::Conditional {
                            alias: ident.clone(),
                            condition: condition.clone(),
                        },
                        Span::from(start.span.start..condition.span.end),
                    ))
                } else {
                    // simple binding identifier
                    Ok(spanned(MatchArm::Default(ident.clone()), start.span))
                }
            }
            Token::Dot => {
                // enum constructor without enum name
                let start = self.advance().unwrap().clone();
                if self.peek().is_none() {
                    return Err(spanned(
                        "Unexpected EOF in pattern".into(),
                        start.span,
                    ));
                }
                let variant_tok = self.advance().unwrap().clone();
                let variant = match variant_tok.inner {
                    Token::Ident(v) => v,
                    _ => {
                        return Err(spanned(
                            "Expected variant name in pattern".into(),
                            variant_tok.span,
                        ))
                    }
                };
                self.expect(&Token::LParen)?;
                if self.peek().is_none() {
                    return Err(spanned(
                        "Unexpected EOF in pattern".into(),
                        variant_tok.span,
                    ));
                }
                let alias_tok = self.advance().unwrap().clone();
                let alias = match alias_tok.inner {
                    Token::Ident(a) => a,
                    _ => {
                        return Err(spanned(
                            "Expected identifier for alias in pattern".into(),
                            alias_tok.span,
                        ))
                    }
                };
                let end = self.peek()
                    .ok_or(spanned("Unexpected EOF in pattern".into(), alias_tok.span))?
                    .clone();
                self.expect(&Token::RParen)?;
                Ok(spanned(
                    MatchArm::EnumConstructor {
                        enum_name: None,
                        variant,
                        alias,
                    },
                    Span::from(start.span.start..end.span.end),
                ))
            }
            Token::LParen => {
                // tuple pattern
                let start = self.advance().unwrap().clone();
                let mut elements = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        let pat = self.parse_pattern()?;
                        elements.push(pat);
                        if self.check(&Token::Comma) {
                            self.advance();
                            if self.check(&Token::RParen) {
                                break;
                            }
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                let end = self.peek().cloned();
                self.expect(&Token::RParen)?;
                Ok(spanned(
                    MatchArm::Tuple(elements),
                    Span::from(start.span.start..end.unwrap().span.end),
                ))
            }
            _ => {
                // fallback to a value pattern (expression)
                let expr = self.parse_expression()?;
                Ok(spanned(MatchArm::Value(expr.clone()), expr.span))
            }
        }
    }

    pub(super) fn parse_block_expr(&mut self) -> Result<Spanned<Expr>, Spanned<SmolStr>> {
        let start_span = self.peek().map(|s| s.span).unwrap();
        self.advance();
        let mut stmts: Vec<Spanned<Statement>> = Vec::new();
        let mut maybe_expr: Option<Box<Spanned<Expr>>> = None;

        while !self.check(&Token::RBrace) {
            if self.peek().is_none() {
                return Err(spanned("Unexpected EOF in block".into(), Span::at(start_span.end) ));
            }

            // parse a statement
            let stmt = self.parse_statement()?;

            // if statement is an Expr and next token is RBrace -> treat as final expr
            match &stmt.inner {
                Statement::Expr(e) => {
                    if self.check(&Token::RBrace) {
                        maybe_expr = Some(Box::new(e.clone()));
                        break;
                    } else {
                        // push as statement
                        stmts.push(stmt.clone());
                    }
                }
                _ => {
                    stmts.push(stmt.clone());
                }
            }

            // optionally consume semicolon after statement
            if self.check(&Token::Semicolon) {
                self.advance();
            } else {
                // If no semicolon, continue; next parse will decide
            }
        }

        let prev_token_span = self.peek_at(-1)
            .map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
        let end_span = self.peek().map(|s| s.span)
            .ok_or(spanned("Unexpected EOF in block".into(), prev_token_span))?;
        self.expect(&Token::RBrace)?;
        let combined_span = Span { start: start_span.start, end: end_span.end };

        Ok(spanned(Expr::Block(stmts, maybe_expr), combined_span))
    }

    fn parse_expression_bp(&mut self, min_prec: u8) -> Result<Spanned<Expr>, Spanned<SmolStr>> {
        let mut left = self.parse_atom()?;
        left = self.parse_postfix(left)?;
        loop {
            let op_sp = match self.peek() {
                Some(s) => s.clone(),
                None => break,
            };
            let prec = Parser::get_precedence(&op_sp.inner);
            if prec == 0 || prec < min_prec {
                break;
            }

            let is_right_assoc = Parser::is_assignment_token(&op_sp.inner);
            let next_min = if is_right_assoc { prec } else { prec + 1 };

            let op_token = self.advance().unwrap().clone();

            let mut right = self.parse_expression_bp(next_min)?;
            right = self.parse_postfix(right)?;

            // build node
            if Parser::is_assignment_token(&op_token.inner) {
                if !Parser::is_assignable(&left.inner) {
                    return Err(spanned(
                        "Invalid assignment target".into(),
                        left.span,
                    ));
                }

                let value = match &op_token.inner {
                    Token::Eq => right.clone(),
                    Token::PlusEq => {
                        let op = Operation::Add;
                        let bin_span = Span { start: left.span.start, end: right.span.end };
                        spanned(
                            Expr::Binary {
                                left: Box::new(left.clone()),
                                operation: op,
                                right: Box::new(right.clone()),
                            },
                            bin_span,
                        )
                    }
                    Token::MinusEq => {
                        let op = Operation::Subtract;
                        let bin_span = Span { start: left.span.start, end: right.span.end };
                        spanned(
                            Expr::Binary {
                                left: Box::new(left.clone()),
                                operation: op,
                                right: Box::new(right.clone()),
                            },
                            bin_span,
                        )
                    }
                    Token::StarEq => {
                        let op = Operation::Multiply;
                        let bin_span = Span { start: left.span.start, end: right.span.end };
                        spanned(
                            Expr::Binary {
                                left: Box::new(left.clone()),
                                operation: op,
                                right: Box::new(right.clone()),
                            },
                            bin_span,
                        )
                    }
                    Token::SlashEq => {
                        let op = Operation::Divide;
                        let bin_span = Span { start: left.span.start, end: right.span.end };
                        spanned(
                            Expr::Binary {
                                left: Box::new(left.clone()),
                                operation: op,
                                right: Box::new(right.clone()),
                            },
                            bin_span,
                        )
                    }
                    Token::PercentEq => {
                        let op = Operation::Modulo;
                        let bin_span = Span { start: left.span.start, end: right.span.end };
                        spanned(
                            Expr::Binary {
                                left: Box::new(left.clone()),
                                operation: op,
                                right: Box::new(right.clone()),
                            },
                            bin_span,
                        )
                    }
                    _ => right.clone(),
                };

                let assign_span = Span { start: left.span.start, end: value.span.end };
                left = spanned(
                    Expr::Assign {
                        target: Box::new(left.clone()),
                        value: Box::new(value),
                    },
                    assign_span,
                );
            } else {
                let op = Parser::token_to_operation(&op_token.inner).ok_or(spanned(
                    format!("Unsupported operator: {:?}", op_token.inner).into(),
                    op_token.span,
                ))?;

                let combined_span = Span { start: left.span.start, end: right.span.end };
                left = spanned(
                    Expr::Binary {
                        left: Box::new(left.clone()),
                        operation: op,
                        right: Box::new(right.clone()),
                    },
                    combined_span,
                );
            }
        }
        Ok(left)
    }

    fn parse_postfix(&mut self, mut expr: Spanned<Expr>) -> Result<Spanned<Expr>, Spanned<SmolStr>> {
        loop {
            match self.peek() {
                Some(sp) => match &sp.inner {
                    Token::LParen => {
                        // call
                        let start_span = expr.span;

                        self.advance();
                        let mut args: Vec<Spanned<Expr>> = Vec::new();
                        if !self.check(&Token::RParen) {
                            loop {
                                let arg = self.parse_expression()?;
                                args.push(arg);
                                if self.check(&Token::Comma) {
                                    self.advance();
                                    continue;
                                } else {
                                    break;
                                }
                            }
                        }

                        let end_span = if self.peek().is_some() {
                            let r = self.peek().unwrap().span;
                            self.expect(&Token::RParen)?;
                            r
                        } else {
                            let prev_token_span = self.peek_at(-1)
                                .map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
                            return Err(spanned("Unexpected EOF in call".into(), prev_token_span));
                        };
                        let call_span = Span { start: start_span.start, end: end_span.end };
                        expr = spanned(
                            Expr::Call {
                                fun: Box::new(expr.clone()),
                                args,
                                generic_args: Vec::new(),
                            },
                            call_span,
                        );
                    }
                    Token::LBracket => {
                        // indexing
                        let start_span = expr.span;
                        self.advance();
                        let index_expr = if !self.check(&Token::RBracket) {
                            let e = self.parse_expression()?;
                            e
                        } else {
                            return Err(spanned("Empty index".into(), self.peek().unwrap().span));
                        };
                        let end_span = if self.peek().is_some() {
                            let r = self.peek().unwrap().span;
                            self.expect(&Token::RBracket)?;
                            r
                        } else {
                            return Err(spanned(
                                "Unexpected EOF in index".into(),
                                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0))
                            ));
                        };
                        let idx_span = Span { start: start_span.start, end: end_span.end };
                        expr = spanned(
                            Expr::Index(Box::new(expr.clone()), Box::new(index_expr)),
                            idx_span,
                        );
                    }
                    Token::Dot => {
                        // field access
                        let start_span = expr.span;
                        self.advance();
                        match self.advance() {
                            Some(next) => match &next.inner {
                                Token::Ident(name) => {
                                    let access_span = Span { start: start_span.start, end: next.span.end };
                                    expr = spanned(Expr::Get(Box::new(expr.clone()), name.clone()), access_span);
                                }
                                _ => {
                                    return Err(spanned("Expected identifier after '.'".into(), next.span));
                                }
                            },
                            None => {
                                return Err(spanned(
                                    "Unexpected EOF after '.'".into(),
                                    self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0))
                                ));
                            }
                        }
                    },
                    Token::ColonColon => {
                        match &expr.inner {
                            Expr::Var(target) => {
                                self.advance();
                                let method = match self.advance() {
                                    Some(token) => {
                                        match &token.inner {
                                            Token::Ident(s) => spanned(s.clone(), token.span),
                                            _ => return Err(spanned(
                                                "".into(),
                                                expr.span
                                            ))
                                        }
                                    },
                                    None => return Err(
                                        spanned(
                                            "Unexpected EOF after ::".into(),
                                            self.peek_at(-1)
                                                .map(|s| Span::at(s.span.end))
                                                .unwrap_or(Span::at(0))
                                        )
                                    ),
                                };
                                expr = spanned(
                                    Expr::StaticMethod {
                                        target: spanned(target.clone(), expr.span),
                                        method: method.clone()

                                    },
                                    Span::from(expr.span.start..method.span.end)
                                )
                            },
                            _ => return Err(spanned(
                                "".into(),
                                expr.span
                            ))
                        }
                    },
                    //generic function call
                    Token::Lt => {
                        match expr.inner {
                            Expr::Var(_)
                            | Expr::Block(_, _)
                            | Expr::Call { fun: _, args: _, generic_args: _ }
                            | Expr::Fun { params: _, body: _, return_type: _, generic_params: _ }
                            | Expr::Index(_, _)
                            | Expr::Get(_, _)
                            | Expr::If { condition: _, body: _, else_block: _ }
                            | Expr::StaticMethod { target: _, method: _ } => {
                                let position = self.cursor;
                                match || -> Result<Spanned<Expr>, Spanned<SmolStr>> {
                                    let generics = {
                                        let mut args = Vec::new();
                                        self.advance();
                                        loop {
                                            args.push(self.parse_type()?);
                                            if self.check(&Token::Comma) {
                                                self.advance();
                                                continue;
                                            }
                                            break;
                                        }
                                        args
                                    };

                                    self.expect(&Token::Gt)?;
                                    self.expect(&Token::LParen)?;
                                    let mut args: Vec<Spanned<Expr>> = Vec::new();
                                    if !self.check(&Token::RParen) {
                                        loop {
                                            let arg = self.parse_expression()?;
                                            args.push(arg);
                                            if self.check(&Token::Comma) {
                                                self.advance();
                                                continue;
                                            } else {
                                                break;
                                            }
                                        }
                                    }

                                    let end_span = if self.peek().is_some() {
                                        let r = self.peek().unwrap().span;
                                        self.expect(&Token::RParen)?;
                                        r
                                    } else {
                                        return Err(spanned(
                                            "Unexpected EOF in call".into(),
                                            self.peek_at(-1)
                                                .map(|s| Span::at(s.span.end))
                                                .unwrap_or(Span::at(0))
                                        ));
                                    };

                                    Ok(spanned(Expr::Call {
                                        fun: Box::new(expr.clone()),
                                        args,
                                        generic_args: generics
                                    }, Span::from(position..end_span.end)))
                                }() {
                                    Ok(e) => expr = e,
                                    Err(_) => {
                                        self.cursor = position;
                                        break;
                                    },
                                }
                            }
                            _ => break,
                        }
                    }
                    _ => break,
                },
                None => break,
            }
        }
        Ok(expr)
    }
}