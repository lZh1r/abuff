use std::collections::HashMap;

use smol_str::{SmolStr, format_smolstr};

use crate::{ast::typed::{LetPattern, Method, MethodSignature, NativeMethod, NormalMethod, Statement}, lexer::Token, parser::main_parser::Parser, span::{Span, Spanned, spanned}};

impl<'a> Parser<'a> {
    pub(super) fn parse_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        match &self.peek().unwrap().inner {
            Token::Let => self.let_statement(),
            Token::Fun => {
                let next_token = match self.peek_at(1) {
                    Some(t) => t,
                    None => return Err(spanned(
                        "Unexpected EOF in let declaration".into(),
                        self.peek().map(|sp| Span::at(sp.span.end)).unwrap()
                    )),
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
            Token::Import => self.import_statement(),
            Token::Native => self.native_statement(),
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
    
    pub(super) fn parse_let_pattern(&mut self) -> Result<Spanned<LetPattern>, Spanned<SmolStr>> {
        let token = self.advance().cloned().ok_or_else(|| {
            let span = self.peek_at(-1)
                .map(|s| Span::at(s.span.end))
                .unwrap_or(Span::from(0..0));
            spanned("Unexpected EOF in pattern".into(), span)
        })?;

        match token.inner {
            Token::Ident(i) => Ok(spanned(LetPattern::Name(i), token.span)),
            Token::LParen => {
                let start_span = token.span;
                let mut patterns = Vec::new();

                // Check for empty tuple: ()
                if self.check(&Token::RParen) {
                    let end_span = self.advance().unwrap().span;
                    return Ok(spanned(
                        LetPattern::Tuple(patterns),
                        Span::from(start_span.start..end_span.end),
                    ));
                }

                // Parse first pattern
                patterns.push(self.parse_let_pattern()?);

                // Parse remaining patterns
                while self.check(&Token::Comma) {
                    self.advance(); // consume comma
                    // Allow trailing comma
                    if self.check(&Token::RParen) {
                        break;
                    }
                    patterns.push(self.parse_let_pattern()?);
                }

                let end_span = self.peek()
                    .map(|s| s.span)
                    .ok_or_else(|| {
                        spanned(
                            "Unexpected EOF in tuple pattern".into(),
                            Span::at(start_span.end),
                        )
                    })?;
                self.expect(&Token::RParen)?;

                Ok(spanned(
                    LetPattern::Tuple(patterns),
                    Span::from(start_span.start..end_span.end),
                ))
            },
            Token::LBrace => {
                let mut entries = Vec::new();
                loop {
                    if self.check(&Token::RBrace) {break}
                    
                    let prev_token_span = self.peek_at(-1)
                        .map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0));
                    let ident = match &self.peek().ok_or_else(|| {
                        spanned(
                            "Unexpected EOF in tuple pattern".into(),
                            Span::at(prev_token_span.end),
                        )
                    })?.inner {
                        Token::Ident(i) => spanned(
                            i.clone(),
                            self.peek().unwrap().span
                        ),
                        t => return Err(spanned(
                            format_smolstr!(
                                "Expected an identifier, a tuple or a record, got {t:?} instead"
                            ),
                            self.peek().unwrap().span
                        ))
                    };
                    self.advance();
                    
                    if self.check(&Token::Colon) {
                        let prev_token_span = Span::at(self.advance().unwrap().span.end);
                        let alias = match &self.peek().ok_or_else(|| {
                            spanned(
                                "Unexpected EOF in tuple pattern".into(),
                                Span::at(prev_token_span.end),
                            )
                        })?.inner {
                            Token::Ident(i) => spanned(
                                i.clone(),
                                self.peek().unwrap().span
                            ),
                            t => return Err(spanned(
                                format_smolstr!(
                                    "Expected an identifier, a tuple or a record, got {t:?} instead"
                                ),
                                self.peek().unwrap().span
                            ))
                        };
                        self.advance();
                        
                        entries.push((ident, Some(alias)));
                    } else {
                        entries.push((ident, None));
                    }
                    
                    if self.check(&Token::Comma) {
                        self.advance();
                        continue
                    }
                }
                let r_brace = self.peek().cloned();
                self.expect(&Token::RBrace)?;
                Ok(spanned(
                    LetPattern::Record(entries),
                    Span::from(token.span.start..r_brace.unwrap().span.end)
                ))
            },
            t => Err(spanned(
                format_smolstr!("Expected an identifier, a tuple or a record, got {t:?} instead"),
                token.span,
            )),
        }
    }

    fn let_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        let start_span = self.advance().unwrap().span; //consume let
        let mutable = if self.check(&Token::Mut) {
            self.advance();
            true
        } else {false};
        if self.peek().is_none() {
            return Err(spanned("Unexpected EOF in let declaration".into(), Span::at(start_span.end)))
        }
        
        let pattern = self.parse_let_pattern()?;

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
                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap()
            ));
        };

        Ok(spanned(
            Statement::Let { pattern, expr: var_expr, type_info: var_type, mutable },
            Span::from(start_span.start..end_span.end)
        ))
    }

    fn fun_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        let start_span = self.advance().unwrap().span; //consume fun
        if self.peek().is_none() {
            return Err(spanned("Unexpected EOF in fun declaration".into(), Span::at(start_span.end)))
        }

        let ident_token = self.advance().unwrap().clone();
        let fun_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(
                format_smolstr!("Expected identifier, got {t:?} instead"),
                ident_token.span
            ))
        };

        let fun_generics = if self.check(&Token::Lt) {
            let mut params = Vec::new();
            let lt = self.advance().unwrap().clone();
            loop {
                let g = self.advance().ok_or(spanned(
                    "Unexpected EOF in generic params".into(),
                    Span::at(lt.span.end)
                ))?.clone();
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
                return Err(spanned(
                    "Expected > after generic params".into(),
                    Span::at(self.peek_at(-1).unwrap().span.end)
                ));
            }
            self.advance();
            params
        } else {
            Vec::new()
        };

        let fun_params = self.parse_fn_params()?;

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

    fn parse_methods(
        &mut self,
        implementation: &mut HashMap<SmolStr, Vec<MethodSignature>>,
        own_methods: &mut Vec<MethodSignature>
    ) -> Result<(), Spanned<SmolStr>> {
        if self.check(&Token::Impl) {
            self.advance();
            self.expect(&Token::LBrace)?;
            loop {
                if self.check(&Token::RBrace) {
                    self.advance();
                    break
                }
                let (is_static, is_mutating) = match self.peek() {
                    Some(t) => {
                        match &t.inner {
                            Token::Mut => (false, true),
                            Token::Static => (true, false),
                            _ => (false, false)
                        }
                    },
                    None => return Err(spanned(
                        "Unexpected EOF in method declaration".into(),
                        Span::at(self.peek_at(-1).unwrap().span.end)
                    )),
                };
                if is_static | is_mutating {self.advance();}

                match &self.peek().ok_or(spanned(
                    "Unexpected EOF in impl block".into(),
                    self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
                ))?.inner.clone() {
                    Token::Ident(iface) => {
                        self.advance();
                        let mut methods = Vec::new();
                        self.expect(&Token::LBrace)?;
                        loop {
                            if self.check(&Token::RBrace) {
                                self.advance();
                                break
                            }

                            let (is_static, is_mutating) = match self.peek() {
                                Some(t) => {
                                    match &t.inner {
                                        Token::Mut => (false, true),
                                        Token::Static => (true, false),
                                        _ => (false, false)
                                    }
                                },
                                None => return Err(spanned(
                                    "Unexpected EOF in method declaration".into(),
                                    Span::at(self.peek_at(-1).unwrap().span.end)
                                )),
                            };
                            if is_static | is_mutating {self.advance();}

                            match &self.peek().ok_or(spanned(
                                "Unexpected EOF in impl block".into(),
                                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
                            ))?.inner.clone() {
                                Token::Fun => {
                                    let fun = self.fun_statement()?;
                                    let method = match fun.inner {
                                        Statement::Fun { name, params, body, return_type, generic_params } => {
                                            spanned(
                                                Method::Normal(
                                                    NormalMethod {
                                                        name: name,
                                                        params: params,
                                                        body: body,
                                                        return_type: return_type,
                                                        generic_params: generic_params,
                                                    }
                                                ),
                                                fun.span
                                            )
                                        },
                                        _ => panic!()
                                    };
                                    methods.push(
                                        MethodSignature {
                                            method,
                                            is_static,
                                            is_mutating,
                                        }
                                    );
                                },
                                Token::Native => {
                                    let native_fun = self.native_fun_statement()?;
                                    let method = match native_fun.inner {
                                        Statement::NativeFun { name, params, return_type, generic_params } => {
                                            spanned(
                                                Method::Native(
                                                    NativeMethod {
                                                        name: name,
                                                        params: params,
                                                        return_type: return_type,
                                                        generic_params: generic_params,
                                                    }
                                                ),
                                                native_fun.span
                                            )
                                        },
                                        _ => panic!()
                                    };
                                    own_methods.push(
                                        MethodSignature {
                                            method,
                                            is_static,
                                            is_mutating,
                                        }
                                    );
                                },
                                t => {
                                    return Err(spanned(
                                        format_smolstr!("Unexpected token: Expected identifier, got {t:?}"),
                                        self.peek().unwrap().span
                                    ))
                                }
                            }
                        }
                        implementation.insert(iface.clone(), methods);
                    },
                    Token::Fun => {
                        let fun = self.fun_statement()?;
                        let method = match fun.inner {
                            Statement::Fun { name, params, body, return_type, generic_params } => {
                                spanned(
                                    Method::Normal(
                                        NormalMethod {
                                            name: name,
                                            params: params,
                                            body: body,
                                            return_type: return_type,
                                            generic_params: generic_params,
                                        }
                                    ),
                                    fun.span
                                )
                            },
                            _ => panic!()
                        };
                        own_methods.push(
                            MethodSignature {
                                method,
                                is_static,
                                is_mutating,
                            }
                        );
                    },
                    Token::Native => {
                        let native_fun = self.native_fun_statement()?;
                        let method = match native_fun.inner {
                            Statement::NativeFun { name, params, return_type, generic_params } => {
                                spanned(
                                    Method::Native(
                                        NativeMethod {
                                            name: name,
                                            params: params,
                                            return_type: return_type,
                                            generic_params: generic_params,
                                        }
                                    ),
                                    native_fun.span
                                )
                            },
                            _ => panic!()
                        };
                        own_methods.push(
                            MethodSignature {
                                method,
                                is_static,
                                is_mutating,
                            }
                        );
                    },
                    t => {
                        return Err(spanned(
                            format_smolstr!("Unexpected token: Expected identifier, got {t:?}"),
                            self.peek().unwrap().span
                        ))
                    }
                }
            }
        }
        Ok(())
    }

    fn type_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        let start_span = self.advance().unwrap().span; //consume type
        if self.peek().is_none() {
            return Err(spanned("Unexpected EOF in type declaration".into(), start_span))
        }

        let ident_token = self.advance().unwrap().clone();
        let type_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(
                format_smolstr!("Expected identifier, got {t:?} instead"),
                ident_token.span
            ))
        };

        let type_generics = if self.check(&Token::Lt) {
            let mut params = Vec::new();
            let lt = self.advance().unwrap().clone();
            loop {
                let g = self.advance().ok_or(spanned(
                    "Unexpected EOF in generic params".into(),
                    lt.span
                ))?.clone();
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
                return Err(spanned(
                    "Expected > after generic params".into(),
                    self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
                ));
            }
            self.advance();
            params
        } else {
            Vec::new()
        };

        let mut interfaces = Vec::new();
        if self.check(&Token::Colon) {
            let start_span = self.advance().unwrap().span;
            loop {
                if self.check(&Token::Eq) {
                    break
                } else {
                    match self.advance() {
                        Some(token) => {
                            match &token.inner {
                                Token::Ident(i) => {
                                    interfaces.push(spanned(i.clone(), token.span));
                                    if !self.check(&Token::Comma) {
                                        break
                                    }
                                },
                                t => return Err(
                                    spanned(
                                        format_smolstr!("Expected identifier, got {t:?} instead"),
                                        token.span
                                    )
                                )
                            }
                        },
                        None => {
                            return Err(spanned(
                                "Unexpected EOF in type declaration".into(),
                                Span::at(start_span.end)
                            ))
                        },
                    }
                }
            }
        }

        self.expect(&Token::Eq)?;
        let type_expr = self.parse_type()?;

        let mut implementation = HashMap::new();
        let mut own_methods = Vec::new();

        self.parse_methods(&mut implementation, &mut own_methods)?;

        let end_span = if self.check(&Token::Semicolon) {
            self.advance().unwrap().span
        } else {
            return Err(spanned(
                "Expected a semicolon at the end of the statement".into(),
                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
            ));
        };

        implementation.insert("+self".into(), own_methods);

        Ok(spanned(
            Statement::TypeDef {
                name: type_name,
                type_info: type_expr,
                generic_params: type_generics,
                implementation,
                interfaces
            },
            Span::from(start_span.start..end_span.end)
        ))
    }

    fn enum_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        let start_span = self.advance().unwrap().span; //consume enum
        if self.peek().is_none() {
            return Err(spanned(
                "Unexpected EOF in enum declaration".into(),
                Span::at(start_span.end)
            ))
        }

        let mut interfaces = Vec::new();
        if self.check(&Token::Colon) {
            let start_span = self.advance().unwrap().span;
            loop {
                if self.check(&Token::Eq) {
                    break
                } else {
                    match self.advance() {
                        Some(token) => {
                            match &token.inner {
                                Token::Ident(i) => {
                                    interfaces.push(spanned(i.clone(), token.span));
                                    if !self.check(&Token::Comma) {
                                        break
                                    }
                                },
                                t => return Err(
                                    spanned(
                                        format_smolstr!("Expected identifier, got {t:?} instead"),
                                        token.span
                                    )
                                )
                            }
                        },
                        None => {
                            return Err(spanned(
                                "Unexpected EOF in type declaration".into(),
                                Span::at(start_span.end)
                            ))
                        },
                    }
                }
            }
        }

        let ident_token = self.advance().unwrap().clone();
        let enum_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(
                format_smolstr!("Expected identifier, got {t:?} instead"),
                ident_token.span
            ))
        };

        let enum_generics = if self.check(&Token::Lt) {
            let mut params = Vec::new();
            let lt = self.advance().unwrap().clone();
            loop {
                let g = self.advance().ok_or(spanned(
                    "Unexpected EOF in generic params".into(),
                    Span::at(lt.span.end)
                ))?.clone();
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
                return Err(spanned(
                    "Expected > after generic params".into(),
                    self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
                ));
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
            let ident = self.advance().ok_or(spanned(
                "Unexpected EOF in enum declaration".into(),
                Span::at(brace.end)
            ))?;
            let variant_name = match ident.inner.clone() {
                Token::Ident(name) => name,
                t => return Err(spanned(
                    format_smolstr!("Expected identifier, got {:?} instead", t),
                    ident.span
                ))
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
        let _end_span = self.peek().unwrap().span;
        self.expect(&Token::RBrace)?;

        let mut own_methods = Vec::new();
        let mut implementation = HashMap::new();

        self.parse_methods(&mut implementation, &mut own_methods)?;

        let end_span = if self.check(&Token::Semicolon) {
            self.advance().unwrap().span
        } else {
            return Err(spanned(
                "Expected a semicolon at the end of the statement".into(),
                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
            ));
        };

        implementation.insert("+self".into(), own_methods);

        Ok(spanned(
            Statement::EnumDef {
                name: enum_name,
                variants: enum_variants,
                generic_params: enum_generics,
                implementation,
                interfaces
            },
            Span::from(start_span.start..end_span.end)
        ))
    }

    fn import_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        let start_span = self.advance().unwrap().span; //consume import
        if self.peek().is_none() {
            return Err(spanned(
                "Unexpected EOF in import statement".into(),
                Span::at(start_span.end)
            ))
        }

        self.expect(&Token::LBrace)?;
        let mut prev_span = Span::from(0..0);
        let mut imports = Vec::new();
        while !self.check(&Token::RBrace) {
            let is_type = if self.check(&Token::Type) {
                prev_span = self.advance().unwrap().span;
                true
            } else {
                false
            };

            let import_name = match self.advance() {
                Some(t) => match t.inner.clone() {
                    Token::Ident(i) => spanned(i, t.span),
                    a => return Err(spanned(
                        format_smolstr!("Expected identifier, got {a:?} instead"),
                        t.span
                    ))
                },
                None => return Err(spanned(
                    "Unexpected EOF in import statement".into(),
                    Span::at(prev_span.end)
                )),
            };

            let alias = if self.check(&Token::As) {
                prev_span = self.advance().unwrap().span; //as
                match self.peek() {
                    Some(t) => match t.inner.clone() {
                        Token::Ident(i) => {
                            self.advance();
                            Some(i)
                        },
                        a => return Err(spanned(
                            format_smolstr!("Expected identifier, got {a:?} instead"),
                            t.span
                        ))
                    },
                    None => return Err(spanned(
                        "Unexpected EOF in import statement".into(),
                        Span::at(prev_span.end)
                    )),
                }
            } else {
                None
            };

            imports.push((import_name, alias, is_type));

            if self.check(&Token::Comma) {
                self.advance();
            } else {
                break
            }
        }
        self.advance(); // RBrace

        let from_span = self.peek().cloned();
        self.expect(&Token::From)?;
        let from_span = from_span.unwrap().span;
        let path = match self.advance() {
            Some(t) => match t.inner.clone() {
                Token::String(s) => spanned(s, t.span),
                a => return Err(spanned(format!("Expected file path, got {a:?} instead").into(), t.span))
            },
            None => return Err(spanned(
                "Unexpected EOF in import statement".into(),
                Span::at(from_span.end)
            )),
        };

        let end_span = if self.check(&Token::Semicolon) {
            self.advance().unwrap().span
        } else {
            return Err(spanned(
                "Expected a semicolon at the end of the statement".into(),
                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
            ));
        };

        Ok(spanned(
            Statement::Import { symbols: imports, path },
            Span::from(start_span.start..end_span.end)
        ))
    }

    fn native_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        match self.peek_at(1).ok_or(spanned(
            "Unexpected EOF in native statement".into(),
            self.peek().map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
        ))?.inner.clone() {
            Token::Fun => {
                self.native_fun_statement()
            },
            Token::Type => {
                self.native_type_statement()
            },
            _ => Err(spanned(
                "Unexpected token".into(),
                self.peek_at(1).unwrap().span
            ))
        }
    }

    fn native_type_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        let start_span = self.advance().unwrap().span; //consume native
        self.expect(&Token::Type)?;

        if self.peek().is_none() {
            return Err(spanned(
                "Unexpected EOF in type declaration".into(),
                Span::at(start_span.end)
            ))
        }

        let ident_token = self.advance().unwrap().clone();
        let type_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(
                format_smolstr!("Expected identifier, got {t:?} instead"),
                ident_token.span
            ))
        };



        let type_generics = if self.check(&Token::Lt) {
            let mut params = Vec::new();
            let lt = self.advance().unwrap().clone();
            loop {
                let g = self.advance().ok_or(spanned(
                    "Unexpected EOF in generic params".into(),
                    Span::at(lt.span.end)
                ))?.clone();
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
                return Err(spanned(
                    "Expected > after generic params".into(),
                    self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
                ));
            }
            self.advance();
            params
        } else {
            Vec::new()
        };

        let mut interfaces = Vec::new();
        if self.check(&Token::Colon) {
            let start_span = self.advance().unwrap().span;
            loop {
                if self.check(&Token::Semicolon) || self.check(&Token::Impl) {
                    break
                } else {
                    match self.advance() {
                        Some(token) => {
                            match &token.inner {
                                Token::Ident(i) => {
                                    interfaces.push(spanned(i.clone(), token.span));
                                    if !self.check(&Token::Comma) {
                                        break
                                    }
                                },
                                t => return Err(
                                    spanned(
                                        format_smolstr!("Expected identifier, got {t:?} instead"),
                                        token.span
                                    )
                                )
                            }
                        },
                        None => {
                            return Err(spanned(
                                "Unexpected EOF in type declaration".into(),
                                Span::at(start_span.end)
                            ))
                        },
                    }
                }
            }
        }

        let mut implementation = HashMap::new();
        let mut own_methods = Vec::new();

        self.parse_methods(&mut implementation, &mut own_methods)?;

        let end_span = if self.check(&Token::Semicolon) {
            self.advance().unwrap().span
        } else {
            return Err(spanned(
                "Expected a semicolon at the end of the statement".into(),
                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
            ));
        };

        implementation.insert("+self".into(), own_methods);

        Ok(spanned(
            Statement::NativeType {
                name: type_name,
                generic_params: type_generics,
                implementation,
                interfaces
            },
            Span::from(start_span.start..end_span.end)
        ))
    }

    fn native_fun_statement(&mut self) -> Result<Spanned<Statement>, Spanned<SmolStr>> {
        let start_span = self.advance().unwrap().span; //consume native
        self.expect(&Token::Fun)?;

        if self.peek().is_none() {
            return Err(spanned(
                "Unexpected EOF in fun declaration".into(),
                Span::at(start_span.end)
            ))
        }

        let ident_token = self.advance().unwrap().clone();
        let fun_name = match ident_token.inner {
            Token::Ident(i) => i,
            t => return Err(spanned(
                format_smolstr!("Expected identifier, got {t:?} instead"),
                ident_token.span
            ))
        };

        let fun_generics = if self.check(&Token::Lt) {
            let mut params = Vec::new();
            let lt = self.advance().unwrap().clone();
            loop {
                let g = self.advance().ok_or(spanned(
                    "Unexpected EOF in generic params".into(),
                    Span::at(lt.span.end)
                ))?.clone();
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
                return Err(spanned(
                    "Expected > after generic params".into(),
                    self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
                ));
            }
            self.advance();
            params
        } else {
            Vec::new()
        };

        let fun_params = self.parse_fn_params()?;

        let fun_type = if self.check(&Token::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let end_span = if self.check(&Token::Semicolon) {
            self.advance().unwrap().span
        } else {
            return Err(spanned(
                "Expected a semicolon at the end of the statement".into(),
                self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::from(0..0))
            ));
        };

        Ok(spanned(
            Statement::NativeFun {
                name: fun_name,
                params: fun_params,
                return_type: fun_type,
                generic_params: fun_generics
            },
            Span::from(start_span.start..end_span.end)
        ))
    }
}