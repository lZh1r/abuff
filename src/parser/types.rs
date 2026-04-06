use crate::{parser::main_parser::Parser, span::Spanned};

impl<'a> Parser<'a> {
    pub(super) fn parse_type_primary(&mut self) -> Result<Spanned<TypeInfo>, Spanned<SmolStr>> {
            let prev_token_span = self.peek_at(-1)
                .map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
            let t_peek = self.peek().cloned().ok_or(spanned(
                "Unexpected EOF parsing type".into(),
                prev_token_span
            ))?;
            match &t_peek.inner {
                Token::LParen => {
                    // plain function type without generic parameters
                    let checkpoint = self.cursor.clone();
                    match self.try_parse_function_type(prev_token_span) {
                        Ok(t) => return Ok(t),
                        Err(_) => {},
                    }
                    self.cursor = checkpoint;
                    self.advance();
                    let mut types = Vec::new();
                    loop {
                        // parse type
                        let ptype = self.parse_type()?;
                        types.push(Box::new(ptype));
                        if self.check(&Token::Comma) {
                            self.advance();
                            continue;
                        } else {
                            break;
                        }
                    }
                    let prev_token_span = self.peek_at(-1)
                        .map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
                    let r_paren_span = self.peek().cloned().ok_or(spanned(
                        "Unexpected EOF parsing type".into(),
                        prev_token_span
                    ))?.span;
                    self.expect(&Token::RParen)?;
                    let span = Span::from(t_peek.span.start..r_paren_span.end);
                    if types.len() == 0 {
                        Err(spanned(
                            "Empty tuple types are not allowed".into(),
                            span
                        ))
                    } else {
                        Ok(spanned(
                            TypeInfo::new(TypeKind::Tuple(types)),
                            span
                        ))
                    }
                }
                Token::Lt => {
                    // generic function type: <generic_params>(args) -> return_type
                    // parse generic parameters
                    let lt = self.advance().unwrap().clone(); // consume '<'
                    let mut generic_params: Vec<Spanned<SmolStr>> = Vec::new();
                    loop {
                        let g = self.advance().ok_or(spanned(
                            "Unexpected EOF in generic function type params".into(),
                            lt.span
                        ))?.clone();
                        match g.inner {
                            Token::Ident(name) => generic_params.push(spanned(name, g.span)),
                            _ => return Err(spanned(
                                "Expected identifier in generic function type params".into(),
                                g.span
                            )),
                        }
                        if self.check(&Token::Comma) {
                            self.advance();
                            continue;
                        }
                        break;
                    }
                    // expect closing '>'
                    if !self.check(&Token::Gt) {
                        return Err(spanned(
                            "Expected > after generic function type params".into(),
                            self.peek_at(-1).map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0))
                        ));
                    }
                    self.advance(); // consume '>'
    
                    let params = self.parse_fn_params()?;
    
                    // arrow and return type
                    self.expect(&Token::Arrow)?;
                    let ret = self.parse_type()?;
                    let span = Span { start: lt.span.start, end: ret.span.end };
                    Ok(spanned(
                        TypeInfo::new(TypeKind::Fun {
                            params,
                            return_type: Box::new(ret),
                            generic_params,
                        }),
                        span,
                    ))
                    
                }
                Token::LBrace => {
                    // record type { name: Type, ... }
                    self.advance(); // consume '{'
                    let mut fields = HashMap::new();
                    if !self.check(&Token::RBrace) {
                        loop {
                            let prev_token_span = self.peek_at(-1)
                                .map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
                            let name_tok = self.advance().ok_or(spanned(
                                "Unexpected EOF in record type".into(),
                                prev_token_span
                            ))?.clone();
                            let fname = if let Token::Ident(s) = name_tok.inner.clone() {
                                s
                            } else {
                                return Err(spanned(
                                    "Expected identifier in record type".into(),
                                    name_tok.span
                                ));
                            };
                            self.expect(&Token::Colon)?;
                            let ftype = self.parse_type()?;
                            fields.insert(fname, ftype);
                            if self.check(&Token::Comma) {
                                self.advance();
                                continue;
                            } else {
                                break;
                            }
                        }
                    }
                    let prev_token_span = self.peek_at(-1)
                        .map(|s| Span::at(s.span.end)).unwrap_or(Span::at(0));
                    let r = self.advance().ok_or(spanned(
                        "Unexpected EOF in record type".into(),
                        prev_token_span
                    ))?.clone();
                    let span = Span { start: t_peek.span.start, end: r.span.end };
                    Ok(spanned(TypeInfo::new(TypeKind::Record(fields)), span))
                }
                Token::Ident(_) => {
                    self.parse_type_ident_or_enumvariant()
                }
                _ => Err(spanned(format_smolstr!("Unexpected token in type: {:?}", t_peek.inner), t_peek.span)),
            }
        }
}