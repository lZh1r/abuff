use crate::{
    ast::{Expr, Span, Spanned, Statement, UnaryOp, TypeInfo},
    lexer::Token,
};

pub struct Parser<'a> {
    tokens: &'a [Spanned<Token>],
    cursor: usize,
}

fn spanned<T>(inner: T, span: Span) -> Spanned<T> {
    Spanned { inner, span }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Spanned<Token>]) -> Self {
        Self { tokens, cursor: 0 }
    }
    
    pub fn parse(&mut self) -> Result<Vec<Spanned<Statement>>, Spanned<String>> {
        let mut statements = Vec::new();
        
        while self.peek().is_some() {
            statements.push(self.parse_statement()?);
        }
        
        Ok(statements)
    }

    //=======
    //helpers
    //=======

    fn peek(&self) -> Option<&Spanned<Token>> {
        self.tokens.get(self.cursor)
    }

    fn peek_at(&self, offset: usize) -> Option<&Spanned<Token>> {
        self.tokens.get(self.cursor + offset)
    }

    fn advance(&mut self) -> Option<&Spanned<Token>> {
        let token = self.tokens.get(self.cursor);
        if token.is_some() {
            self.cursor += 1;
        }
        token
    }

    fn check(&self, token: &Token) -> bool {
        match self.peek() {
            Some(sp) => &sp.inner == token,
            None => false,
        }
    }

    fn expect(&mut self, token: &Token) -> Result<(), Spanned<String>> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(spanned(
                "Unexpected token".into(),
                self.peek().map(|s| s.span).unwrap_or(Span::from(0..0)),
            ))
        }
    }

    fn get_precedence(token: &Token) -> u8 {
        match token {
            Token::LParen | Token::LBracket | Token::Dot => 9,
            Token::Bang => 8,
            Token::Star | Token::Slash | Token::Percent => 7,
            Token::Plus | Token::Minus => 6,
            Token::Gt | Token::GtEq | Token::Lt | Token::LtEq => 5,
            Token::EqEq | Token::NotEq => 4,
            Token::AndAnd => 3,
            Token::OrOr => 2,
            Token::Eq
            | Token::MinusEq
            | Token::PlusEq
            | Token::StarEq
            | Token::SlashEq
            | Token::PercentEq => 1,
            _ => 0,
        }
    }

    // helpers for operations
    fn token_to_operation(token: &Token) -> Option<crate::ast::Operation> {
        use crate::ast::Operation::*;
        match token {
            Token::Plus => Some(Add),
            Token::Minus => Some(Subtract),
            Token::Star => Some(Multiply),
            Token::Slash => Some(Divide),
            Token::Percent => Some(Modulo),
            Token::EqEq => Some(Eq),
            Token::NotEq => Some(NotEq),
            Token::Lt => Some(LessThan),
            Token::LtEq => Some(LessThanEq),
            Token::Gt => Some(GreaterThan),
            Token::GtEq => Some(GreaterThanEq),
            Token::AndAnd => Some(And),
            Token::OrOr => Some(Or),
            _ => None,
        }
    }

    fn is_assignment_token(token: &Token) -> bool {
        matches!(
            token,
            Token::Eq
                | Token::PlusEq
                | Token::MinusEq
                | Token::StarEq
                | Token::SlashEq
                | Token::PercentEq
        )
    }

    fn is_assignable(expr: &Expr) -> bool {
        match expr {
            Expr::Var(_) | Expr::Get(_, _) | Expr::Index(_, _) => true,
            _ => false,
        }
    }

    //=======
    //parsers
    //=======

    fn parse_statement(&mut self) -> Result<Spanned<Statement>, Spanned<String>> {
        match &self.peek().unwrap().inner {
            Token::Let => todo!(),
            Token::Fun => todo!(),
            Token::Type => todo!(),
            Token::Enum => todo!(),
            Token::Import => todo!(),
            Token::Native => todo!(),
            Token::Export => {
                // consume Export
                let span = self.peek().unwrap().span;
                self.advance();
                Ok(spanned(
                    Statement::Export(Box::new(self.parse_statement()?)),
                    span,
                ))
            }
            _ => Ok(spanned(
                Statement::Expr(self.parse_expression()?),
                self.peek().map(|s| s.span).unwrap_or(Span::from(0..0)),
            )),
        }
    }

    fn parse_expression(&mut self) -> Result<Spanned<Expr>, Spanned<String>> {
        self.parse_expression_bp(1)
    }

    fn parse_expression_bp(&mut self, min_prec: u8) -> Result<Spanned<Expr>, Spanned<String>> {
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
                        let op = crate::ast::Operation::Add;
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
                        let op = crate::ast::Operation::Subtract;
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
                        let op = crate::ast::Operation::Multiply;
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
                        let op = crate::ast::Operation::Divide;
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
                        let op = crate::ast::Operation::Modulo;
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
                    format!("Unsupported operator: {:?}", op_token.inner),
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

    fn parse_postfix(&mut self, mut expr: Spanned<Expr>) -> Result<Spanned<Expr>, Spanned<String>> {
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
                            return Err(spanned("Unexpected EOF in call".into(), Span::from(0..0)));
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
                            return Err(spanned("Unexpected EOF in index".into(), Span::from(0..0)));
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
                                return Err(spanned("Unexpected EOF after '.'".into(), Span::from(0..0)));
                            }
                        }
                    }
                    _ => break,
                },
                None => break,
            }
        }
        Ok(expr)
    }

    fn parse_atom(&mut self) -> Result<Spanned<Expr>, Spanned<String>> {
        let token = self
            .advance()
            .ok_or(spanned("Unexpected EOF".into(), Span::from(0..0)))?
            .clone();

        match token.inner.clone() {
            Token::If => {
                // parse: if (cond) body [else ...]
                self.expect(&Token::LParen)?;
                let cond = self.parse_expression()?;
                self.expect(&Token::RParen)?;

                let body = if self.check(&Token::LBrace) {
                    self.parse_block_expr()?
                } else {
                    let e = self.parse_expression()?;
                    e
                };

                let else_block = if self.check(&Token::Else) {
                    self.advance();
                    if self.check(&Token::If) {
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
            Token::Return => {
                if self.peek().is_some() {
                    let expr = self.parse_expression()?;
                    let combined_span = Span { start: token.span.start, end: expr.span.end };
                    Ok(spanned(Expr::Return(Box::new(expr)), combined_span))
                } else {
                    Err(spanned("Unexpected EOF".into(), token.span))
                }
            }
            Token::Break => Ok(spanned(Expr::Break, token.span)),
            Token::Continue => Ok(spanned(Expr::Continue, token.span)),
            Token::False => Ok(spanned(Expr::Bool(false), token.span)),
            Token::True => Ok(spanned(Expr::Bool(true), token.span)),
            Token::Ident(v) => Ok(spanned(Expr::Var(v), token.span)),
            Token::Int(i) => Ok(spanned(Expr::Int(i), token.span)),
            Token::Float(f) => Ok(spanned(Expr::Float(f), token.span)),
            Token::String(s) => Ok(spanned(Expr::String(s), token.span)),
            Token::Minus => {
                if self.peek().is_some() {
                    let expr = self.parse_expression()?;
                    let combined_span = Span { start: token.span.start, end: expr.span.end };
                    Ok(spanned(Expr::Unary(UnaryOp::Negate, Box::new(expr)), combined_span))
                } else {
                    Err(spanned("Unexpected EOF".into(), token.span))
                }
            }
            Token::Bang => {
                if self.peek().is_some() {
                    let expr = self.parse_expression()?;
                    let combined_span = Span { start: token.span.start, end: expr.span.end };
                    Ok(spanned(Expr::Unary(UnaryOp::Not, Box::new(expr)), combined_span))
                } else {
                    Err(spanned("Unexpected EOF".into(), token.span))
                }
            }
            Token::LParen => {
                // grouped expression
                let expr = self.parse_expression()?;
                let rspan = self.peek().map(|s| s.span).ok_or(spanned("Unexpected EOF".into(), Span::from(0..0)))?;
                self.expect(&Token::RParen)?;
                let combined_span = Span { start: token.span.start, end: rspan.end };
                Ok(spanned(expr.inner, combined_span))
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
                let end_span = self.peek().map(|s| s.span).ok_or(spanned("Unexpected EOF".into(), Span::from(0..0)))?;
                self.expect(&Token::RBracket)?;
                let combined_span = Span { start: token.span.start, end: end_span.end };
                Ok(spanned(Expr::Array(elements), combined_span))
            }
            Token::LBrace => {
                if let (Some(next), Some(next2)) = (self.peek(), self.peek_at(1)) {
                    match (&next.inner, &next2.inner) {
                        (Token::Ident(_), Token::Colon) => {
                            // record
                            let mut fields: Vec<(String, Spanned<Expr>)> = Vec::new();
                            while !self.check(&Token::RBrace) {
                                let name_tok = self.advance().ok_or(spanned("Unexpected EOF".into(), Span::from(0..0)))?.clone();
                                let name = match name_tok.inner {
                                    Token::Ident(s) => s,
                                    _ => return Err(spanned("Expected identifier in record".into(), name_tok.span)),
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
                            let end_span = self.peek().map(|s| s.span).ok_or(spanned("Unexpected EOF".into(), Span::from(0..0)))?;
                            self.expect(&Token::RBrace)?;
                            let combined_span = Span { start: token.span.start, end: end_span.end };
                            Ok(spanned(Expr::Record(fields), combined_span))
                        }
                        _ => {
                            // block
                            let block_expr = self.parse_block_expr()?;
                            Ok(block_expr)
                        }
                    }
                } else {
                    let block_expr = self.parse_block_expr()?;
                    Ok(block_expr)
                }
            }
            Token::Fun => {
                //anonymos function
                self.expect(&Token::LParen)?;
                let mut params: Vec<((bool, String), Spanned<TypeInfo>)> = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        let mut variadic = false;
                        if self.check(&Token::DotDotDot) {
                            variadic = true;
                            self.advance();
                        }
                        let name_tok = self.advance().ok_or(spanned("Unexpected EOF in params".into(), Span::from(0..0)))?.clone();
                        let name = match name_tok.inner {
                            Token::Ident(s) => s,
                            _ => return Err(spanned("Expected identifier in parameters".into(), name_tok.span)),
                        };

                        let type_span = self.peek().map(|s| s.span).unwrap_or(name_tok.span);
                        let type_info_spanned = if self.check(&Token::Colon) {
                            // we don't have a full type parser here; use Unknown
                            self.advance();
                            let tspan = self.peek().map(|s| s.span).unwrap_or(type_span);
                            spanned(TypeInfo::Unknown, tspan)
                        } else {
                            spanned(TypeInfo::Unknown, type_span)
                        };

                        params.push(((variadic, name), type_info_spanned));

                        if self.check(&Token::Comma) {
                            self.advance();
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                self.expect(&Token::RParen)?;

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
                        return_type: None,
                        generic_params: Vec::new(),
                    },
                    combined_span,
                ))
            }
            // Token::DotDotDot => todo!(), TODO: spread operator later
            _ => Err(spanned(
                format!("Expected expression, got {:?}", token.inner),
                token.span,
            )),
        }
    }

    fn parse_block_expr(&mut self) -> Result<Spanned<Expr>, Spanned<String>> {
        let start_span = self.peek().map(|s| s.span).unwrap_or(Span::from(0..0));
        self.expect(&Token::LBrace)?;
        let mut stmts: Vec<Spanned<Statement>> = Vec::new();
        let mut maybe_expr: Option<Box<Spanned<Expr>>> = None;

        while !self.check(&Token::RBrace) {
            if self.peek().is_none() {
                return Err(spanned("Unexpected EOF in block".into(), Span::from(0..0)));
            }

            let stmt = self.parse_statement()?;

            match &stmt.inner {
                Statement::Expr(e) => {
                    if self.check(&Token::RBrace) {
                        // final expression without semicolon -> block returns this expression
                        maybe_expr = Some(Box::new(e.clone()));
                        break;
                    } else if self.check(&Token::Semicolon) {
                        // expression followed by semicolon -> treat as statement, block returns void
                        self.advance(); // consume semicolon
                        stmts.push(stmt.clone());
                    } else {
                        // expression not final and not terminated by semicolon -> error
                        return Err(spanned("Expected a semicolon".into(), self.peek().unwrap().span));
                    }
                }
                _ => {
                    // other statements must be terminated by semicolon
                    stmts.push(stmt.clone());
                    if self.check(&Token::Semicolon) {
                        self.advance();
                    } else {
                        return Err(spanned("Expected a semicolon".into(), self.peek().unwrap().span));
                    }
                }
            }
        }

        let end_span = self.peek().map(|s| s.span).ok_or(spanned("Unexpected EOF in block".into(), Span::from(0..0)))?;
        self.expect(&Token::RBrace)?;
        let combined_span = Span { start: start_span.start, end: end_span.end };

        Ok(spanned(Expr::Block(stmts, maybe_expr), combined_span))
    }
}