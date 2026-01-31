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

    //----------
    //statements
    //----------
    
    fn parse_statement(&mut self) -> Result<Spanned<Statement>, Spanned<String>> {
        match &self.peek().unwrap().inner {
            Token::Let => self.let_statement(),
            Token::Fun => {
                let next_token = match self.peek_at(1) {
                    Some(t) => t,
                    None => return Err(spanned("Unexpected EOF in let declaration".into(), self.peek().unwrap().span)),
                };
                match next_token.inner {
                    Token::Lt | Token::LParen => {
                        let expr = self.parse_expression()?;
                        let span = expr.span.clone();
                        Ok(spanned(
                            Statement::Expr(expr),
                            span,
                        ))
                    },
                    _ => Ok(self.fun_statement()?)
                }
            },
            Token::Type => self.type_statement(),
            Token::Enum => self.enum_statement(),
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
            _ => {
                let expr = self.parse_expression()?;
                let span = expr.span.clone();
                Ok(spanned(
                    Statement::Expr(expr),
                    span,
                ))
            },
        }
    }
    
    fn let_statement(&mut self) -> Result<Spanned<Statement>, Spanned<String>> {
        let start_span = self.advance().unwrap().span; //consume let
        if self.peek().is_none() {
            return Err(spanned("Unexpected EOF in let declaration".into(), start_span))
        }
        
        let ident_token = self.advance().unwrap().clone();
        let var_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(format!("Expected identifier, got {t:?} instead"), ident_token.span))
        };
        
        let var_type = if self.check(&Token::Colon) {
            self.advance(); //consume colon
            Some(self.parse_type()?)
        } else {
            None
        };
        
        self.expect(&Token::Eq)?;
        let var_expr = self.parse_expression()?;
        
        let end_span = if self.check(&Token::Semicolon) {
            self.advance().unwrap().span
        } else {
            return Err(spanned(
                "Expected a semicolon at the end of the statement".into(),
                self.peek().unwrap().span
            ));
        };
        
        Ok(spanned(
            Statement::Let { name: var_name, expr: var_expr, type_info: var_type },
            Span::from(start_span.start..end_span.end)
        ))
    }
    
    fn fun_statement(&mut self) -> Result<Spanned<Statement>, Spanned<String>> {
        let start_span = self.advance().unwrap().span; //consume fun
        if self.peek().is_none() {
            return Err(spanned("Unexpected EOF in fun declaration".into(), start_span))
        }
        
        let ident_token = self.advance().unwrap().clone();
        let fun_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(format!("Expected identifier, got {t:?} instead"), ident_token.span))
        };
        
        let fun_generics = if self.check(&Token::Lt) {
            let mut params = Vec::new();
            let lt = self.advance().unwrap().clone();
            loop {
                let g = self.advance().ok_or(spanned("Unexpected EOF in generic params".into(), lt.span))?.clone();
                match g.inner {
                    Token::Ident(name) => params.push(spanned(name, g.span)),
                    _ => return Err(spanned("Expected identifier in generic params".into(), g.span)),
                }
                if self.check(&Token::Comma) {
                    self.advance();
                    continue;
                }
                break;
            }
            
            if !self.check(&Token::Gt) {
                return Err(spanned("Expected > after generic params".into(), self.peek().unwrap().span));
            }
            self.advance();
            params
        } else {
            Vec::new()
        };
        
        self.expect(&Token::LParen)?;
        let mut fun_params: Vec<((bool, String), Spanned<TypeInfo>)> = Vec::new();
        if !self.check(&Token::RParen) {
            loop {
                // variadic marker: ... before ident
                let mut variadic = false;
                if self.check(&Token::DotDotDot) {
                    variadic = true;
                    self.advance();
                }
                // expect ident
                let name_tok = self.advance().ok_or(spanned("Unexpected EOF in params".into(), Span::from(0..0)))?.clone();
                let name = match name_tok.inner {
                    Token::Ident(s) => s,
                    _ => return Err(spanned("Expected identifier in parameters".into(), name_tok.span)),
                };

                // type annotation required: ":" TypeInfo (like Rust)
                if !self.check(&Token::Colon) {
                    return Err(spanned("Expected : Type for function parameter".into(), self.peek().map(|s| s.span).unwrap_or(name_tok.span)));
                }
                self.advance(); // consume ':'
                let type_info_spanned = self.parse_type()?;

                fun_params.push(((variadic, name), type_info_spanned));

                if self.check(&Token::Comma) {
                    self.advance();
                    continue;
                } else {
                    break;
                }
            }
        }
        self.expect(&Token::RParen)?;
        
        let fun_type = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        
        let fun_body = if self.check(&Token::LBrace) {
            self.parse_block_expr()?
        } else {
            let body = self.parse_expression()?;
            self.expect(&Token::Semicolon)?;
            body
        };
        
        let end_span = fun_body.span.end;
        
        Ok(spanned(
            Statement::Fun { 
                name: fun_name, 
                params: fun_params, 
                body: fun_body, 
                return_type: fun_type, 
                generic_params: fun_generics 
            },
            Span::from(start_span.start..end_span)
        ))
    }
    
    fn type_statement(&mut self) -> Result<Spanned<Statement>, Spanned<String>> {
        let start_span = self.advance().unwrap().span; //consume type
        if self.peek().is_none() {
            return Err(spanned("Unexpected EOF in let declaration".into(), start_span))
        }
        
        let ident_token = self.advance().unwrap().clone();
        let type_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(format!("Expected identifier, got {t:?} instead"), ident_token.span))
        };
        
        let type_generics = if self.check(&Token::Lt) {
            let mut params = Vec::new();
            let lt = self.advance().unwrap().clone();
            loop {
                let g = self.advance().ok_or(spanned("Unexpected EOF in generic params".into(), lt.span))?.clone();
                match g.inner {
                    Token::Ident(name) => params.push(spanned(name, g.span)),
                    _ => return Err(spanned("Expected identifier in generic params".into(), g.span)),
                }
                if self.check(&Token::Comma) {
                    self.advance();
                    continue;
                }
                break;
            }
            
            if !self.check(&Token::Gt) {
                return Err(spanned("Expected > after generic params".into(), self.peek().unwrap().span));
            }
            self.advance();
            params
        } else {
            Vec::new()
        };
        
        self.expect(&Token::Eq)?;
        let type_expr = self.parse_type()?;
        
        let end_span = if self.check(&Token::Semicolon) {
            self.advance().unwrap().span
        } else {
            return Err(spanned(
                "Expected a semicolon at the end of the statement".into(),
                self.peek().unwrap().span
            ));
        };
        
        Ok(spanned(
            Statement::TypeDef { name: type_name, type_info: type_expr, generic_params: type_generics },
            Span::from(start_span.start..end_span.end)
        ))
    }
    
    fn enum_statement(&mut self) -> Result<Spanned<Statement>, Spanned<String>> {
        let start_span = self.advance().unwrap().span; //consume enum
        if self.peek().is_none() {
            return Err(spanned("Unexpected EOF in let declaration".into(), start_span))
        }
        
        let ident_token = self.advance().unwrap().clone();
        let enum_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(format!("Expected identifier, got {t:?} instead"), ident_token.span))
        };
        
        let enum_generics = if self.check(&Token::Lt) {
            let mut params = Vec::new();
            let lt = self.advance().unwrap().clone();
            loop {
                let g = self.advance().ok_or(spanned("Unexpected EOF in generic params".into(), lt.span))?.clone();
                match g.inner {
                    Token::Ident(name) => params.push(spanned(name, g.span)),
                    _ => return Err(spanned("Expected identifier in generic params".into(), g.span)),
                }
                if self.check(&Token::Comma) {
                    self.advance();
                    continue;
                }
                break;
            }
            
            if !self.check(&Token::Gt) {
                return Err(spanned("Expected > after generic params".into(), self.peek().unwrap().span));
            }
            self.advance();
            params
        } else {
            Vec::new()
        };
        
        let mut enum_variants = Vec::new();
        let brace = self.peek().cloned();
        self.expect(&Token::LBrace)?;
        let brace = brace.unwrap().span;
        while !self.check(&Token::RBrace) {
            let ident = self.advance().ok_or(spanned("Unexpected EOF in enum declaration".to_string(), brace))?;
            let variant_name = match ident.inner.clone() {
                Token::Ident(name) => name,
                t => return Err(spanned(format!("Expected identifier, got {:?} instead", t), ident.span))
            };
            
            let variant_type = if self.check(&Token::Colon) {
                self.advance();
                Some(self.parse_type()?)
            } else {
                None
            };
            
            enum_variants.push((variant_name, variant_type));
            
            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break
            }
        }
        let end_span = self.peek().unwrap().span;
        self.expect(&Token::RBrace)?;
        
        Ok(spanned(
            Statement::EnumDef { name: enum_name, variants: enum_variants, generic_params: enum_generics },
            Span::from(start_span.start..end_span.end)
        ))
    }
    
    //-----------
    //expressions
    //-----------

    fn parse_expression(&mut self) -> Result<Spanned<Expr>, Spanned<String>> {
        self.parse_expression_bp(1)
    }
    
    fn parse_atom(&mut self) -> Result<Spanned<Expr>, Spanned<String>> {
        let token = self
            .advance()
            .ok_or(spanned("Unexpected EOF".into(), Span::from(0..0)))?
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
            Token::Return => {
                // return expr
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
                                // expect identifier
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
                let mut generic_params: Vec<Spanned<String>> = Vec::new();
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
 
                // parse params
                self.expect(&Token::LParen)?;
                let mut params: Vec<((bool, String), Spanned<TypeInfo>)> = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        // variadic marker: ... before ident
                        let mut variadic = false;
                        if self.check(&Token::DotDotDot) {
                            variadic = true;
                            self.advance();
                        }
                        // expect ident
                        let name_tok = self.advance().ok_or(spanned("Unexpected EOF in params".into(), Span::from(0..0)))?.clone();
                        let name = match name_tok.inner {
                            Token::Ident(s) => s,
                            _ => return Err(spanned("Expected identifier in parameters".into(), name_tok.span)),
                        };
 
                        // type annotation required: ":" TypeInfo (like Rust)
                        if !self.check(&Token::Colon) {
                            return Err(spanned("Expected : Type for function parameter".into(), self.peek().map(|s| s.span).unwrap_or(name_tok.span)));
                        }
                        self.advance(); // consume ':'
                        let type_info_spanned = self.parse_type()?;

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
            Token::Semicolon => Ok(spanned(Expr::Void, token.span)),
            // Token::DotDotDot => todo!(), TODO: spread operator later
            _ => Err(spanned(
                format!("Expected expression, got {:?}", token.inner),
                token.span,
            )),
        }
    }

    fn parse_block_expr(&mut self) -> Result<Spanned<Expr>, Spanned<String>> {
        let start_span = self.peek().map(|s| s.span).unwrap();
        self.advance();
        let mut stmts: Vec<Spanned<Statement>> = Vec::new();
        let mut maybe_expr: Option<Box<Spanned<Expr>>> = None;

        while !self.check(&Token::RBrace) {
            if self.peek().is_none() {
                return Err(spanned("Unexpected EOF in block".into(), start_span ));
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

        let end_span = self.peek().map(|s| s.span).ok_or(spanned("Unexpected EOF in block".into(), Span::from(0..0)))?;
        self.expect(&Token::RBrace)?;
        let combined_span = Span { start: start_span.start, end: end_span.end };

        Ok(spanned(Expr::Block(stmts, maybe_expr), combined_span))
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
                    },
                    //generic function call
                    Token::Lt => {
                        match expr.inner {
                            Expr::Var(_) 
                            | Expr::Block(_, _) 
                            | Expr::Call { fun: _, args: _, generic_args: _ }
                            | Expr::Fun { params: _, body: _, return_type: _, generic_params: _ }
                            | Expr::Index(_, _)
                            | Expr::If { condition: _, body: _, else_block: _ } => {
                                let position = self.cursor;
                                match || -> Result<Spanned<Expr>, Spanned<String>> {
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
                                        return Err(spanned("Unexpected EOF in call".into(), Span::from(0..0)));
                                    };
                                    
                                    Ok(spanned(Expr::Call { 
                                        fun: Box::new(expr.clone()),
                                        args, 
                                        generic_args: generics
                                    }, Span::from(position..end_span.end)))
                                }() {
                                    Ok(e) => return Ok(e),
                                    Err(e) => {
                                        println!("{e:?}");
                                        self.cursor = position;
                                        break
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

    //-----
    //types
    //-----
    
    fn parse_type(&mut self) -> Result<Spanned<TypeInfo>, Spanned<String>> {
        // parse primary and apply postfix (array) operators
        let mut ty = self.parse_type_primary()?;
        // postfix arrays: Type[]
        loop {
            if self.check(&Token::LBracket) && self.peek_at(1).map(|t| matches!(t.inner, Token::RBracket)).unwrap_or(false) {
                // consume '[' and ']'
                let _l = self.advance().unwrap().clone();
                let r = self.advance().unwrap().clone(); // RBracket
                let combined = Span { start: ty.span.start, end: r.span.end };
                ty = spanned(TypeInfo::Array(Box::new(ty)), combined);
            } else {
                break;
            }
        }
        Ok(ty)
    }
 
    fn parse_type_ident_or_enumvariant(&mut self) -> Result<Spanned<TypeInfo>, Spanned<String>> {
        // Assumes next token is Ident
        let ident_sp = self.advance().ok_or(spanned("Unexpected EOF parsing type".into(), Span::from(0..0)))?.clone();
        let name = if let Token::Ident(s) = ident_sp.inner.clone() {
            s
        } else {
            return Err(spanned("Expected identifier in type".into(), ident_sp.span));
        };
        // enforce that type names start with a capital letter
        if name.chars().next().map(|c| !c.is_uppercase()).unwrap_or(true) {
            return Err(spanned("Type names must start with a capital letter".into(), ident_sp.span));
        }
        // enum variant: Ident . Ident
        if self.check(&Token::Dot) {
            // consume dot
            self.advance();
            let var_sp = self.advance().ok_or(spanned("Unexpected EOF in enum variant".into(), ident_sp.span))?.clone();
            if let Token::Ident(var_name) = var_sp.inner.clone() {
                let span = Span { start: ident_sp.span.start, end: var_sp.span.end };
                return Ok(spanned(TypeInfo::EnumVariant { enum_name: name, variant: var_name }, span));
            } else {
                return Err(spanned("Expected variant identifier after .".into(), var_sp.span));
            }
        }

        // custom type, possibly with generic args: Ident<...>
        let mut generic_args: Vec<Spanned<TypeInfo>> = Vec::new();
        if self.check(&Token::Lt) {
            // consume '<'
            self.advance();
            if !self.check(&Token::Gt) {
                loop {
                    let arg = self.parse_type()?;
                    generic_args.push(arg);
                    if self.check(&Token::Comma) {
                        self.advance();
                        continue;
                    } else {
                        break;
                    }
                }
            }
            // expect '>'
            if !self.check(&Token::Gt) {
                return Err(spanned("Expected > after generic args".into(), self.peek().unwrap().span));
            }
            let gt = self.advance().unwrap().clone();
            let span = Span { start: ident_sp.span.start, end: gt.span.end };
            return Ok(spanned(TypeInfo::Custom { name, generic_args }, span));
        }

        // primitives mapping (must start with capital letter)
        let ty = match name.as_str() {
            "Int" => TypeInfo::Int,
            "Float" => TypeInfo::Float,
            "String" => TypeInfo::String,
            "Char" => TypeInfo::Char,
            "Bool" => TypeInfo::Bool,
            "Void" => TypeInfo::Void,
            "Null" => TypeInfo::Null,
            "Any" => TypeInfo::Any,
            "Unknown" => TypeInfo::Unknown,
            _ => TypeInfo::Custom { name: name.clone(), generic_args: vec![] },
        };
        Ok(spanned(ty, ident_sp.span))
    }
 
    fn parse_type_primary(&mut self) -> Result<Spanned<TypeInfo>, Spanned<String>> {
        let t_peek = self.peek().cloned().ok_or(spanned("Unexpected EOF parsing type".into(), Span::from(0..0)))?;
        match &t_peek.inner {
            Token::LParen => {
                self.advance();
                let mut params: Vec<((bool, String), Spanned<TypeInfo>)> = Vec::new();
                if !self.check(&Token::RParen) {
                    loop {
                        let mut variadic = false;
                        if self.check(&Token::DotDotDot) {
                            variadic = true;
                            self.advance();
                        }
                        // expect name
                        let name_tok = self.advance().ok_or(spanned("Unexpected EOF in function type params".into(), Span::from(0..0)))?.clone();
                        let pname = if let Token::Ident(n) = name_tok.inner.clone() { n } else { return Err(spanned("Expected identifier in function type params".into(), name_tok.span)); };
                        // expect colon
                        self.expect(&Token::Colon)?;
                        // parse type
                        let ptype = self.parse_type()?;
                        params.push(((variadic, pname), ptype));
                        if self.check(&Token::Comma) {
                            self.advance();
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                let rpar = self.advance().ok_or(spanned("Unexpected EOF, expected )".into(), Span::from(0..0)))?.clone();
                // if arrow follows, it's a function type
                if self.check(&Token::Arrow) {
                    // consume arrow
                    self.advance();
                    // parse return type
                    let ret = self.parse_type()?;
                    let span = Span { start: t_peek.span.start, end: ret.span.end };
                    Ok(spanned(TypeInfo::Fun { params, return_type: Box::new(ret), generic_params: vec![] }, span))
                } else {
                    // grouped type -> ambiguous in this grammar
                    Err(spanned("Parenthesized types are only allowed for function types".into(), rpar.span))
                }
            }
            Token::LBrace => {
                // record type { name: Type, ... }
                self.advance(); // consume '{'
                let mut fields: Vec<(String, Spanned<TypeInfo>)> = Vec::new();
                if !self.check(&Token::RBrace) {
                    loop {
                        let name_tok = self.advance().ok_or(spanned("Unexpected EOF in record type".into(), Span::from(0..0)))?.clone();
                        let fname = if let Token::Ident(s) = name_tok.inner.clone() { s } else { return Err(spanned("Expected identifier in record type".into(), name_tok.span)); };
                        self.expect(&Token::Colon)?;
                        let ftype = self.parse_type()?;
                        fields.push((fname, ftype));
                        if self.check(&Token::Comma) {
                            self.advance();
                            continue;
                        } else {
                            break;
                        }
                    }
                }
                let r = self.advance().ok_or(spanned("Unexpected EOF in record type".into(), Span::from(0..0)))?.clone();
                let span = Span { start: t_peek.span.start, end: r.span.end };
                Ok(spanned(TypeInfo::Record(fields), span))
            }
            Token::Ident(_) => {
                self.parse_type_ident_or_enumvariant()
            }
            _ => Err(spanned(format!("Unexpected token in type: {:?}", t_peek.inner), t_peek.span)),
        }
    }
}