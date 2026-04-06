use smol_str::{SmolStr};

use crate::{
    ast::{shared::{Operation, UnaryOp}, typed::{Expr, FunctionParam, MatchArm, Statement, TypeInfo}},
    lexer::Token,
    span::spanned
};

use crate::span::{Span, Spanned};

pub struct Parser<'a> {
    tokens: &'a [Spanned<Token>],
    pub(super) cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Spanned<Token>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Spanned<Statement>>, Spanned<SmolStr>> {
        let mut statements = Vec::new();

        while self.peek().is_some() {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    //=======
    //helpers
    //=======

    pub(super) fn peek(&self) -> Option<&Spanned<Token>> {
        self.tokens.get(self.cursor)
    }

    pub(super) fn peek_at(&self, offset: i64) -> Option<&Spanned<Token>> {
        self.tokens.get((self.cursor as i64 + offset) as usize)
    }

    pub(super) fn advance(&mut self) -> Option<&Spanned<Token>> {
        let token = self.tokens.get(self.cursor);
        if token.is_some() {
            self.cursor += 1;
        }
        token
    }

    pub(super) fn check(&self, token: &Token) -> bool {
        match self.peek() {
            Some(sp) => &sp.inner == token,
            None => false,
        }
    }

    pub(super) fn expect(&mut self, token: &Token) -> Result<(), Spanned<SmolStr>> {
        if self.check(token) {
            self.advance();
            Ok(())
        } else {
            Err(spanned(
                format!("Unexpected token, expected {token:?}").into(),
                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0)),
            ))
        }
    }

    pub(super) fn get_precedence(token: &Token) -> u8 {
        match token {
            Token::LParen | Token::LBracket | Token::Dot => 20,
            Token::Bang => 19,
            Token::Star | Token::Slash | Token::Percent => 18,
            Token::Plus | Token::Minus => 17,
            Token::LeftShift | Token::RightShift => 10,
            Token::Ampersand => 9,
            Token::Caret => 8,
            Token::Pipe => 7,
            Token::Gt | Token::GtEq | Token::Lt | Token::LtEq => 6,
            Token::EqEq | Token::NotEq => 5,
            Token::AndAnd => 4,
            Token::OrOr => 3,
            Token::NullCoal => 2,
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
    pub(super) fn token_to_operation(token: &Token) -> Option<Operation> {
        use crate::ast::shared::Operation::*;
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
            Token::NullCoal => Some(NullCoal),
            Token::Ampersand => Some(BitwiseAnd),
            Token::Pipe => Some(BitwiseOr),
            Token::Caret => Some(BitwiseXor),
            Token::LeftShift => Some(BitwiseLeftShift),
            Token::RightShift => Some(BitwiseRightShift),
            _ => None,
        }
    }

    pub(super) fn is_assignment_token(token: &Token) -> bool {
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

    pub(super) fn is_assignable(expr: &Expr) -> bool {
        match expr {
            Expr::Var(_) | Expr::Get(_, _) | Expr::Index(_, _) => true,
            _ => false,
        }
    }
    
    pub(super) fn parse_fn_params(&mut self) -> Result<Vec<FunctionParam>, Spanned<SmolStr>> {
        self.expect(&Token::LParen)?;
        let mut fun_params: Vec<FunctionParam> = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                let mut is_mutable = false;
                if self.check(&Token::Mut) {
                    is_mutable = true;
                    self.advance();
                }
                
                // variadic marker: ... before ident
                let mut is_variadic = false;
                if self.check(&Token::DotDotDot) {
                    is_variadic = true;
                    self.advance();
                }
                let prev_token_span = self.peek_at(-1)
                    .map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0));
                // expect ident
                let name_tok = self.advance().ok_or(spanned(
                    "Unexpected EOF in params".into(),
                    prev_token_span
                ))?.clone();
                let name = match name_tok.inner {
                    Token::Ident(s) => s,
                    _ => return Err(spanned("Expected identifier in parameters".into(), name_tok.span)),
                };

                // type annotation required: ":" TypeInfo
                if !self.check(&Token::Colon) {
                    return Err(spanned(
                        "Expected : Type for function parameter".into(),
                        self.peek_at(-1)
                            .map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
                    ));
                }
                self.advance(); // consume ':'
                let type_info = self.parse_type()?;

                fun_params.push(
                    FunctionParam { 
                        name,
                        type_info,
                        is_variadic,
                        is_mutable
                    }
                );

                if self.check(&Token::Comma) {
                    self.advance();
                    continue;
                } else {
                    break;
                }
            }
        }
        self.expect(&Token::RParen)?;
        Ok(fun_params)
    }



    //-----------
    //expressions
    //-----------

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

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use crate::{ast::typed::{LetPattern, Method, MethodSignature, NormalMethod}, parser::test_helpers::test_helpers::{assert_stmts_eq_ignore_spans, parse_tokens, sp}};

    use super::*;
    
    #[test]
    fn let_simple() {
        let actual = parse_tokens(vec![
            Token::Let,
            Token::Ident(SmolStr::new("a")),
            Token::Eq,
            Token::Int(2),
            Token::Semicolon,
        ]);
    
        let expected = vec![Statement::Let {
            pattern: sp(LetPattern::Name(SmolStr::new("a"))),
            expr: sp(Expr::Int(2)),
            type_info: None,
            mutable: false,
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn let_mut_record_pattern_with_type() {
        let actual = parse_tokens(vec![
            Token::Let,
            Token::Mut,
            Token::LBrace,
            Token::Ident(SmolStr::new("a")),
            Token::Colon,
            Token::Ident(SmolStr::new("b")),
            Token::Comma,
            Token::Ident(SmolStr::new("c")),
            Token::RBrace,
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::Eq,
            Token::Int(3),
            Token::Semicolon,
        ]);
    
        let expected = vec![Statement::Let {
            pattern: sp(LetPattern::Record(vec![
                (sp(SmolStr::new("a")), Some(sp(SmolStr::new("b")))),
                (sp(SmolStr::new("c")), None),
            ])),
            expr: sp(Expr::Int(3)),
            type_info: Some(sp(TypeInfo::int())),
            mutable: true,
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn expr_binary_precedence() {
        let actual = parse_tokens(vec![Token::Int(2), Token::Plus, Token::Int(3), Token::Star, Token::Int(6)]);
    
        let expected = vec![Statement::Expr(sp(Expr::Binary {
            left: Box::new(sp(Expr::Int(2))),
            operation: Operation::Add,
            right: Box::new(sp(Expr::Binary {
                left: Box::new(sp(Expr::Int(3))),
                operation: Operation::Multiply,
                right: Box::new(sp(Expr::Int(6))),
            })),
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn expr_assignment_eq() {
        let actual = parse_tokens(vec![Token::Ident(SmolStr::new("a")), Token::Eq, Token::Int(1)]);
    
        let expected = vec![Statement::Expr(sp(Expr::Assign {
            target: Box::new(sp(Expr::Var(SmolStr::new("a")))),
            value: Box::new(sp(Expr::Int(1))),
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn expr_assignment_plus_eq() {
        let actual = parse_tokens(vec![Token::Ident(SmolStr::new("a")), Token::PlusEq, Token::Int(2)]);
    
        let expected = vec![Statement::Expr(sp(Expr::Assign {
            target: Box::new(sp(Expr::Var(SmolStr::new("a")))),
            value: Box::new(sp(Expr::Binary {
                left: Box::new(sp(Expr::Var(SmolStr::new("a")))),
                operation: Operation::Add,
                right: Box::new(sp(Expr::Int(2))),
            })),
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn expr_unary_not() {
        let actual = parse_tokens(vec![Token::Bang, Token::True]);
    
        let expected = vec![Statement::Expr(sp(Expr::Unary(
            UnaryOp::Not,
            Box::new(sp(Expr::Bool(true))),
        )))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn expr_if_else() {
        let actual = parse_tokens(vec![
            Token::If,
            Token::LParen,
            Token::Int(4),
            Token::Lt,
            Token::Int(3),
            Token::RParen,
            Token::Int(3),
            Token::Else,
            Token::Int(4),
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::If {
            condition: Box::new(sp(Expr::Binary {
                left: Box::new(sp(Expr::Int(4))),
                operation: Operation::LessThan,
                right: Box::new(sp(Expr::Int(3))),
            })),
            body: Box::new(sp(Expr::Int(3))),
            else_block: Some(Box::new(sp(Expr::Int(4)))),
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn fun_statement_block_expr_tail() {
        let actual = parse_tokens(vec![
            Token::Fun,
            Token::Ident(SmolStr::new("add")),
            Token::LParen,
            Token::Ident(SmolStr::new("x")),
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::Comma,
            Token::Mut,
            Token::Ident(SmolStr::new("y")),
            Token::Colon,
            Token::Ident(SmolStr::new("Float")),
            Token::RParen,
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::LBrace,
            Token::Ident(SmolStr::new("x")),
            Token::Plus,
            Token::Ident(SmolStr::new("y")),
            Token::RBrace,
        ]);
    
        let expected = vec![Statement::Fun {
            name: SmolStr::new("add"),
            params: vec![
                FunctionParam {
                    name: SmolStr::new("x"),
                    type_info: sp(TypeInfo::int()),
                    is_variadic: false,
                    is_mutable: false,
                },
                FunctionParam {
                    name: SmolStr::new("y"),
                    type_info: sp(TypeInfo::float()),
                    is_variadic: false,
                    is_mutable: true,
                },
            ],
            body: sp(Expr::Block(
                vec![],
                Some(Box::new(sp(Expr::Binary {
                    left: Box::new(sp(Expr::Var(SmolStr::new("x")))),
                    operation: Operation::Add,
                    right: Box::new(sp(Expr::Var(SmolStr::new("y")))),
                })))),
            ),
            return_type: Some(sp(TypeInfo::int())),
            generic_params: vec![],
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn record_literal_and_field_get() {
        let actual = parse_tokens(vec![
            Token::LBrace,
            Token::Ident(SmolStr::new("a")),
            Token::Colon,
            Token::Int(1),
            Token::Comma,
            Token::Ident(SmolStr::new("b")),
            Token::Colon,
            Token::Int(2),
            Token::RBrace,
            Token::Dot,
            Token::Ident(SmolStr::new("a")),
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::Get(
            Box::new(sp(Expr::Record(vec![
                (SmolStr::new("a"), sp(Expr::Int(1))),
                (SmolStr::new("b"), sp(Expr::Int(2))),
            ]))),
            SmolStr::new("a"),
        )))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn tuple_literal_and_indexing() {
        let actual = parse_tokens(vec![
            Token::LParen,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RParen,
            Token::LBracket,
            Token::Int(0),
            Token::RBracket,
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::Index(
            Box::new(sp(Expr::Tuple(vec![
                Box::new(sp(Expr::Int(1))),
                Box::new(sp(Expr::Int(2))),
            ]))),
            Box::new(sp(Expr::Int(0))),
        )))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn match_conditional_and_default() {
        let actual = parse_tokens(vec![
            Token::Match,
            Token::Ident(SmolStr::new("x")),
            Token::LBrace,
            Token::Ident(SmolStr::new("a")),
            Token::If,
            Token::Ident(SmolStr::new("x")),
            Token::Gt,
            Token::Int(0),
            Token::Arrow,
            Token::Int(1),
            Token::Comma,
            Token::Ident(SmolStr::new("b")),
            Token::Arrow,
            Token::Int(2),
            Token::RBrace,
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::Match {
            target: Box::new(sp(Expr::Var(SmolStr::new("x")))),
            branches: vec![
                (
                    sp(MatchArm::Conditional {
                        alias: SmolStr::new("a"),
                        condition: sp(Expr::Binary {
                            left: Box::new(sp(Expr::Var(SmolStr::new("x")))),
                            operation: Operation::GreaterThan,
                            right: Box::new(sp(Expr::Int(0))),
                        }),
                    }),
                    sp(Expr::Int(1)),
                ),
                (sp(MatchArm::Default(SmolStr::new("b"))), sp(Expr::Int(2))),
            ],
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn type_def_simple() {
        let actual = parse_tokens(vec![
            Token::Type,
            Token::Ident(SmolStr::new("MyType")),
            Token::Eq,
            Token::Ident(SmolStr::new("Int")),
            Token::Semicolon,
        ]);
    
        let mut impls: HashMap<SmolStr, Vec<MethodSignature>> = HashMap::new();
        impls.insert(SmolStr::new("+self"), vec![]);
    
        let expected = vec![Statement::TypeDef {
            name: SmolStr::new("MyType"),
            type_info: sp(TypeInfo::int()),
            generic_params: vec![],
            implementation: impls,
            interfaces: vec![],
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn enum_def_simple() {
        let actual = parse_tokens(vec![
            Token::Enum,
            Token::Ident(SmolStr::new("MyEnum")),
            Token::LBrace,
            Token::Ident(SmolStr::new("A")),
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::Comma,
            Token::Ident(SmolStr::new("B")),
            Token::RBrace,
            Token::Semicolon,
        ]);
    
        let mut impls: HashMap<SmolStr, Vec<MethodSignature>> = HashMap::new();
        impls.insert(SmolStr::new("+self"), vec![]);
    
        let expected = vec![Statement::EnumDef {
            name: SmolStr::new("MyEnum"),
            variants: vec![
                (SmolStr::new("A"), Some(sp(TypeInfo::int()))),
                (SmolStr::new("B"), None),
            ],
            generic_params: vec![],
            implementation: impls,
            interfaces: vec![],
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn import_statement_simple() {
        let actual = parse_tokens(vec![
            Token::Import,
            Token::LBrace,
            Token::Ident(SmolStr::new("a")),
            Token::Comma,
            Token::Ident(SmolStr::new("b")),
            Token::As,
            Token::Ident(SmolStr::new("c")),
            Token::Comma,
            Token::Type,
            Token::Ident(SmolStr::new("T")),
            Token::RBrace,
            Token::From,
            Token::String(SmolStr::new("mymod")),
            Token::Semicolon,
        ]);
    
        let expected = vec![Statement::Import {
            symbols: vec![
                (sp(SmolStr::new("a")), None, false),
                (sp(SmolStr::new("b")), Some(SmolStr::new("c")), false),
                (sp(SmolStr::new("T")), None, true),
            ],
            path: sp(SmolStr::new("mymod")),
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn export_statement_wraps_next() {
        let actual = parse_tokens(vec![
            Token::Export,
            Token::Let,
            Token::Ident(SmolStr::new("a")),
            Token::Eq,
            Token::Int(1),
            Token::Semicolon,
        ]);
    
        let expected_inner = Statement::Let {
            pattern: sp(LetPattern::Name(SmolStr::new("a"))),
            expr: sp(Expr::Int(1)),
            type_info: None,
            mutable: false,
        };
    
        let expected = vec![Statement::Export(Box::new(sp(expected_inner)))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn while_loop_break_in_block_tail() {
        let actual = parse_tokens(vec![
            Token::While,
            Token::LParen,
            Token::True,
            Token::RParen,
            Token::LBrace,
            Token::Break,
            Token::RBrace,
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::While {
            condition: Box::new(sp(Expr::Bool(true))),
            body: Box::new(sp(Expr::Block(
                vec![],
                Some(Box::new(sp(Expr::Break))),
            ))),
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn for_loop_simple() {
        let actual = parse_tokens(vec![
            Token::For,
            Token::LParen,
            Token::Ident(SmolStr::new("x")),
            Token::In,
            Token::Int(0),
            Token::RParen,
            Token::Ident(SmolStr::new("x")),
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::For {
            element: sp(LetPattern::Name(SmolStr::new("x"))),
            iterable: Box::new(sp(Expr::Int(0))),
            body: Box::new(sp(Expr::Var(SmolStr::new("x")))),
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn unary_negate() {
        let actual = parse_tokens(vec![Token::Minus, Token::Int(5)]);
    
        let expected = vec![Statement::Expr(sp(Expr::Unary(
            UnaryOp::Negate,
            Box::new(sp(Expr::Int(5))),
        )))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn panic_none_in_block_consumes_semicolon() {
        let actual = parse_tokens(vec![Token::LBrace, Token::Panic, Token::Semicolon, Token::RBrace]);
    
        let expected = vec![Statement::Expr(sp(Expr::Block(
            vec![sp(Statement::Expr(sp(Expr::Panic(None))))],
            None,
        )))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn match_tuple_pattern() {
        let actual = parse_tokens(vec![
            Token::Match,
            Token::Ident(SmolStr::new("x")),
            Token::LBrace,
            Token::LParen,
            Token::Ident(SmolStr::new("a")),
            Token::Comma,
            Token::Ident(SmolStr::new("b")),
            Token::RParen,
            Token::Arrow,
            Token::Ident(SmolStr::new("a")),
            Token::RBrace,
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::Match {
            target: Box::new(sp(Expr::Var(SmolStr::new("x")))),
            branches: vec![(
                sp(MatchArm::Tuple(vec![
                    sp(MatchArm::Default(SmolStr::new("a"))),
                    sp(MatchArm::Default(SmolStr::new("b"))),
                ])),
                sp(Expr::Var(SmolStr::new("a"))),
            )],
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn array_literal_expression() {
        let actual = parse_tokens(vec![
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RBracket,
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::Array(vec![
            sp(Expr::Int(1)),
            sp(Expr::Int(2)),
        ])))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn fun_literal_expression_simple() {
        let actual = parse_tokens(vec![
            Token::Fun,
            Token::LParen,
            Token::Ident(SmolStr::new("x")),
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::RParen,
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::LBrace,
            Token::Ident(SmolStr::new("x")),
            Token::RBrace,
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::Fun {
            params: vec![FunctionParam {
                name: SmolStr::new("x"),
                type_info: sp(TypeInfo::int()),
                is_variadic: false,
                is_mutable: false,
            }],
            body: Box::new(sp(Expr::Block(
                vec![],
                Some(Box::new(sp(Expr::Var(SmolStr::new("x"))))),
            ))),
            return_type: Some(sp(TypeInfo::int())),
            generic_params: vec![],
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn call_expression_simple() {
        let actual = parse_tokens(vec![
            Token::Ident(SmolStr::new("f")),
            Token::LParen,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RParen,
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::Call {
            fun: Box::new(sp(Expr::Var(SmolStr::new("f")))),
            args: vec![sp(Expr::Int(1)), sp(Expr::Int(2))],
            generic_args: vec![],
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn static_method_access() {
        let actual = parse_tokens(vec![
            Token::Ident(SmolStr::new("a")),
            Token::ColonColon,
            Token::Ident(SmolStr::new("m")),
        ]);
    
        let expected = vec![Statement::Expr(sp(Expr::StaticMethod {
            target: sp(SmolStr::new("a")),
            method: sp(SmolStr::new("m")),
        }))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn return_expression() {
        let actual = parse_tokens(vec![Token::Return, Token::Int(1)]);
    
        let expected = vec![Statement::Expr(sp(Expr::Return(Box::new(sp(Expr::Int(1))))))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn panic_expression_some() {
        let actual = parse_tokens(vec![Token::Panic, Token::Int(1)]);
    
        let expected = vec![Statement::Expr(sp(Expr::Panic(Some(Box::new(sp(Expr::Int(1)))))))];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn native_fun_statement_simple() {
        let actual = parse_tokens(vec![
            Token::Native,
            Token::Fun,
            Token::Ident(SmolStr::new("nf")),
            Token::LParen,
            Token::Ident(SmolStr::new("x")),
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::RParen,
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::Semicolon,
        ]);
    
        let expected = vec![Statement::NativeFun {
            name: SmolStr::new("nf"),
            params: vec![FunctionParam {
                name: SmolStr::new("x"),
                type_info: sp(TypeInfo::int()),
                is_variadic: false,
                is_mutable: false,
            }],
            return_type: Some(sp(TypeInfo::int())),
            generic_params: vec![],
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn native_type_statement_simple() {
        let actual = parse_tokens(vec![Token::Native, Token::Type, Token::Ident(SmolStr::new("NT")), Token::Semicolon]);
    
        let mut impls: HashMap<SmolStr, Vec<MethodSignature>> = HashMap::new();
        impls.insert(SmolStr::new("+self"), vec![]);
    
        let expected = vec![Statement::NativeType {
            name: SmolStr::new("NT"),
            generic_params: vec![],
            implementation: impls,
            interfaces: vec![],
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }
    
    #[test]
    fn type_def_impl_mutating_fun_method() {
        let actual = parse_tokens(vec![
            Token::Type,
            Token::Ident(SmolStr::new("T")),
            Token::Eq,
            Token::Ident(SmolStr::new("Int")),
            Token::Impl,
            Token::LBrace,
            Token::Mut,
            Token::Fun,
            Token::Ident(SmolStr::new("m")),
            Token::LParen,
            Token::Ident(SmolStr::new("x")),
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::RParen,
            Token::Colon,
            Token::Ident(SmolStr::new("Int")),
            Token::LBrace,
            Token::Ident(SmolStr::new("x")),
            Token::RBrace,
            Token::RBrace,
            Token::Semicolon,
        ]);
    
        let mut impls: HashMap<SmolStr, Vec<MethodSignature>> = HashMap::new();
        impls.insert(
            SmolStr::new("+self"),
            vec![MethodSignature {
                method: sp(Method::Normal(NormalMethod {
                    name: SmolStr::new("m"),
                    params: vec![FunctionParam {
                        name: SmolStr::new("x"),
                        type_info: sp(TypeInfo::int()),
                        is_variadic: false,
                        is_mutable: false,
                    }],
                    body: sp(Expr::Block(vec![], Some(Box::new(sp(Expr::Var(SmolStr::new("x"))))))),
                    return_type: Some(sp(TypeInfo::int())),
                    generic_params: vec![],
                })),
                is_static: false,
                is_mutating: true,
            }],
        );
    
        let expected = vec![Statement::TypeDef {
            name: SmolStr::new("T"),
            type_info: sp(TypeInfo::int()),
            generic_params: vec![],
            implementation: impls,
            interfaces: vec![],
        }];
    
        assert_stmts_eq_ignore_spans(&actual, &expected);
    }

}