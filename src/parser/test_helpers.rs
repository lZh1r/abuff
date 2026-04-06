#[cfg(test)]
pub(crate) mod test_helpers {
    use crate::{ast::typed::{Expr, FunctionParam, LetPattern, MatchArm, Method, MethodSignature, Statement, TypeInfo}, lexer::Token, parser::main_parser::Parser, span::{Span, Spanned}};

    
    pub fn sp<T>(inner: T) -> Spanned<T> {
        Spanned {
            inner,
            span: Span { start: 0, end: 0 },
        }
    }
    
    pub fn parse_tokens(tokens: Vec<Token>) -> Vec<Statement> {
        let sp_tokens: Vec<Spanned<Token>> = tokens.into_iter().map(|t| sp(t)).collect();
        let mut parser = Parser::new(&sp_tokens);
        parser
            .parse()
            .expect("parser should succeed")
            .into_iter()
            .map(|s| s.inner)
            .collect()
    }
    
    pub fn assert_fn_params_eq_ignore_spans(a: &[FunctionParam], b: &[FunctionParam]) {
        assert_eq!(a.len(), b.len(), "params length mismatch");
        for (pa, pb) in a.iter().zip(b.iter()) {
            assert_eq!(pa.name, pb.name, "param name mismatch");
            assert_eq!(pa.type_info.inner, pb.type_info.inner, "param type mismatch");
            assert_eq!(pa.is_variadic, pb.is_variadic, "param variadic mismatch");
            assert_eq!(pa.is_mutable, pb.is_mutable, "param mutable mismatch");
        }
    }
    
    pub fn assert_let_pattern_eq_ignore_spans(a: &LetPattern, b: &LetPattern) {
        match (a, b) {
            (LetPattern::Name(na), LetPattern::Name(nb)) => assert_eq!(na, nb),
            (LetPattern::Record(fa), LetPattern::Record(fb)) => {
                assert_eq!(fa.len(), fb.len(), "record pattern length mismatch");
                for ((a_key, a_alias), (b_key, b_alias)) in fa.iter().zip(fb.iter()) {
                    assert_eq!(a_key.inner, b_key.inner, "record pattern key mismatch");
                    match (a_alias, b_alias) {
                        (None, None) => {}
                        (Some(a), Some(b)) => assert_eq!(a.inner, b.inner, "record alias mismatch"),
                        (x, y) => panic!("record alias option mismatch: {x:?} vs {y:?}"),
                    }
                }
            }
            (LetPattern::Tuple(ae), LetPattern::Tuple(be)) => {
                assert_eq!(ae.len(), be.len(), "tuple pattern length mismatch");
                for (aa, bb) in ae.iter().zip(be.iter()) {
                    assert_let_pattern_eq_ignore_spans(&aa.inner, &bb.inner);
                }
            }
            _ => panic!("let pattern mismatch: {a:?} vs {b:?}"),
        }
    }
    
    pub fn assert_match_arm_eq_ignore_spans(a: &MatchArm, b: &MatchArm) {
        match (a, b) {
            (
                MatchArm::Conditional {
                    alias: aa_alias,
                    condition: aa_cond,
                },
                MatchArm::Conditional {
                    alias: bb_alias,
                    condition: bb_cond,
                },
            ) => {
                assert_eq!(aa_alias, bb_alias, "conditional alias mismatch");
                assert_expr_eq_ignore_spans(&aa_cond.inner, &bb_cond.inner);
            }
            (MatchArm::Value(ae), MatchArm::Value(be)) => {
                assert_expr_eq_ignore_spans(&ae.inner, &be.inner);
            }
            (MatchArm::Default(ae), MatchArm::Default(be)) => assert_eq!(ae, be),
            (
                MatchArm::EnumConstructor {
                    enum_name: aa_enum,
                    variant: aa_var,
                    alias: aa_alias,
                },
                MatchArm::EnumConstructor {
                    enum_name: bb_enum,
                    variant: bb_var,
                    alias: bb_alias,
                },
            ) => {
                assert_eq!(aa_enum, bb_enum, "enum name mismatch");
                assert_eq!(aa_var, bb_var, "variant mismatch");
                assert_eq!(aa_alias, bb_alias, "alias mismatch");
            }
            (MatchArm::Tuple(ae), MatchArm::Tuple(be)) => {
                assert_eq!(ae.len(), be.len(), "tuple match-arm length mismatch");
                for (aa, bb) in ae.iter().zip(be.iter()) {
                    assert_match_arm_eq_ignore_spans(&aa.inner, &bb.inner);
                }
            }
            _ => panic!("match arm mismatch: {a:?} vs {b:?}"),
        }
    }
    
    pub fn assert_type_info_eq_ignore_spans(a: &TypeInfo, b: &TypeInfo) {
        // For the type shapes used in these tests (primitives), spans don't exist inside TypeInfo.
        assert_eq!(a, b);
    }
    
    pub fn assert_expr_eq_ignore_spans(a: &Expr, b: &Expr) {
        match (a, b) {
            (Expr::Bool(aa), Expr::Bool(bb)) => assert_eq!(aa, bb),
            (Expr::Int(aa), Expr::Int(bb)) => assert_eq!(aa, bb),
            (Expr::Float(aa), Expr::Float(bb)) => assert_eq!(aa, bb),
            (Expr::String(aa), Expr::String(bb)) => assert_eq!(aa, bb),
            (Expr::Char(aa), Expr::Char(bb)) => assert_eq!(aa, bb),
            (Expr::Void, Expr::Void) => {}
            (Expr::Null, Expr::Null) => {}
            (Expr::Var(aa), Expr::Var(bb)) => assert_eq!(aa, bb),
    
            (Expr::Unary(aop, ainner), Expr::Unary(bop, binn)) => {
                assert_eq!(aop, bop, "unary op mismatch");
                assert_expr_eq_ignore_spans(&ainner.inner, &binn.inner);
            }
    
            (Expr::Binary { left: aleft, operation: aop, right: aright }, Expr::Binary {
                left: bleft,
                operation: bop,
                right: bright,
            }) => {
                assert_eq!(aop, bop, "binary op mismatch");
                assert_expr_eq_ignore_spans(&aleft.inner, &bleft.inner);
                assert_expr_eq_ignore_spans(&aright.inner, &bright.inner);
            }
    
            (Expr::Assign { target: at, value: av }, Expr::Assign { target: bt, value: bv }) => {
                assert_expr_eq_ignore_spans(&at.inner, &bt.inner);
                assert_expr_eq_ignore_spans(&av.inner, &bv.inner);
            }
    
            (Expr::If {
                condition: acond,
                body: abody,
                else_block: aelse,
            }, Expr::If {
                condition: bcond,
                body: bbody,
                else_block: bels,
            }) => {
                assert_expr_eq_ignore_spans(&acond.inner, &bcond.inner);
                assert_expr_eq_ignore_spans(&abody.inner, &bbody.inner);
                match (aelse, bels) {
                    (None, None) => {}
                    (Some(ae), Some(be)) => assert_expr_eq_ignore_spans(&ae.inner, &be.inner),
                    (x, y) => panic!("if else option mismatch: {x:?} vs {y:?}"),
                }
            }
    
            (Expr::While { condition: acond, body: abody }, Expr::While {
                condition: bcond,
                body: bbody,
            }) => {
                assert_expr_eq_ignore_spans(&acond.inner, &bcond.inner);
                assert_expr_eq_ignore_spans(&abody.inner, &bbody.inner);
            }
    
            (Expr::Break, Expr::Break) => {}
            (Expr::Continue, Expr::Continue) => {}
    
            (Expr::Return(aret), Expr::Return(bret)) => {
                assert_expr_eq_ignore_spans(&aret.inner, &bret.inner);
            }
    
            (Expr::Panic(ap), Expr::Panic(bp)) => match (ap, bp) {
                (None, None) => {}
                (Some(ae), Some(be)) => assert_expr_eq_ignore_spans(&ae.inner, &be.inner),
                (x, y) => panic!("panic option mismatch: {x:?} vs {y:?}"),
            },
    
            (Expr::For { element: ael, iterable: aiter, body: abody }, Expr::For {
                element: bel,
                iterable: biter,
                body: bbody,
            }) => {
                assert_let_pattern_eq_ignore_spans(&ael.inner, &bel.inner);
                assert_expr_eq_ignore_spans(&aiter.inner, &biter.inner);
                assert_expr_eq_ignore_spans(&abody.inner, &bbody.inner);
            }
    
            (Expr::Block(astmts, amaybe), Expr::Block(bstmts, bmaybe)) => {
                assert_eq!(astmts.len(), bstmts.len(), "block statement length mismatch");
                for (sa, sb) in astmts.iter().zip(bstmts.iter()) {
                    assert_stmt_eq_ignore_spans(&sa.inner, &sb.inner);
                }
                match (amaybe, bmaybe) {
                    (None, None) => {}
                    (Some(ae), Some(be)) => assert_expr_eq_ignore_spans(&ae.inner, &be.inner),
                    (x, y) => panic!("block tail expr option mismatch: {x:?} vs {y:?}"),
                }
            }
    
            (Expr::Array(aelts), Expr::Array(belts)) => {
                assert_eq!(aelts.len(), belts.len(), "array element count mismatch");
                for (aa, bb) in aelts.iter().zip(belts.iter()) {
                    assert_expr_eq_ignore_spans(&aa.inner, &bb.inner);
                }
            }
    
            (Expr::Tuple(aelts), Expr::Tuple(belts)) => {
                assert_eq!(aelts.len(), belts.len(), "tuple element count mismatch");
                for (aa, bb) in aelts.iter().zip(belts.iter()) {
                    assert_expr_eq_ignore_spans(&aa.inner, &bb.inner);
                }
            }
    
            (Expr::Record(afields), Expr::Record(bfields)) => {
                assert_eq!(afields.len(), bfields.len(), "record field count mismatch");
                for ((ak, av), (bk, bv)) in afields.iter().zip(bfields.iter()) {
                    assert_eq!(ak, bk, "record field name mismatch");
                    assert_expr_eq_ignore_spans(&av.inner, &bv.inner);
                }
            }
    
            (Expr::Get(at, am), Expr::Get(bt, bm)) => {
                assert_eq!(am, bm, "field name mismatch");
                assert_expr_eq_ignore_spans(&at.inner, &bt.inner);
            }
    
            (Expr::Index(at, ai), Expr::Index(bt, bi)) => {
                assert_expr_eq_ignore_spans(&at.inner, &bt.inner);
                assert_expr_eq_ignore_spans(&ai.inner, &bi.inner);
            }
    
            (Expr::Fun {
                params: ap,
                body: abody,
                return_type: art,
                generic_params: agp,
            }, Expr::Fun {
                params: bp,
                body: bbody,
                return_type: brt,
                generic_params: bgp,
            }) => {
                assert_fn_params_eq_ignore_spans(ap, bp);
                assert_expr_eq_ignore_spans(&abody.inner, &bbody.inner);
                match (art.as_ref(), brt.as_ref()) {
                    (None, None) => {}
                    (Some(atv), Some(btv)) => assert_type_info_eq_ignore_spans(&atv.inner, &btv.inner),
                    (x, y) => panic!("fun return_type option mismatch: {x:?} vs {y:?}"),
                }
                assert_eq!(
                    agp.iter().map(|g| &g.inner).collect::<Vec<_>>(),
                    bgp.iter().map(|g| &g.inner).collect::<Vec<_>>()
                );
            }
    
            (Expr::Call {
                fun: afun,
                args: aargs,
                generic_args: agens,
            }, Expr::Call {
                fun: bfun,
                args: bargs,
                generic_args: bgens,
            }) => {
                assert_expr_eq_ignore_spans(&afun.inner, &bfun.inner);
    
                assert_eq!(aargs.len(), bargs.len(), "call arg length mismatch");
                for (aa, bb) in aargs.iter().zip(bargs.iter()) {
                    assert_expr_eq_ignore_spans(&aa.inner, &bb.inner);
                }
    
                assert_eq!(agens.len(), bgens.len(), "call generic arg length mismatch");
                for (a_t, b_t) in agens.iter().zip(bgens.iter()) {
                    assert_type_info_eq_ignore_spans(&a_t.inner, &b_t.inner);
                }
            }
    
            (Expr::StaticMethod { target: at, method: am }, Expr::StaticMethod {
                target: bt,
                method: bm,
            }) => {
                assert_eq!(at.inner, bt.inner, "static method target mismatch");
                assert_eq!(am.inner, bm.inner, "static method name mismatch");
            }
    
            (Expr::Match { target: at, branches: ab }, Expr::Match { target: bt, branches: bb }) => {
                assert_expr_eq_ignore_spans(&at.inner, &bt.inner);
                assert_eq!(ab.len(), bb.len(), "match branch count mismatch");
                for ((a_pat, a_expr), (b_pat, b_expr)) in ab.iter().zip(bb.iter()) {
                    assert_match_arm_eq_ignore_spans(&a_pat.inner, &b_pat.inner);
                    assert_expr_eq_ignore_spans(&a_expr.inner, &b_expr.inner);
                }
            }
    
            other => panic!("expr mismatch (unsupported variant or mismatch): {other:?}"),
        }
    }
    
    pub fn assert_method_eq_ignore_spans(a: &Method, b: &Method) {
        match (a, b) {
            (Method::Normal(na), Method::Normal(nb)) => {
                assert_eq!(na.name, nb.name);
                assert_fn_params_eq_ignore_spans(&na.params, &nb.params);
                assert_expr_eq_ignore_spans(&na.body.inner, &nb.body.inner);
                assert_eq!(na.return_type.as_ref().map(|t| &t.inner), nb.return_type.as_ref().map(|t| &t.inner));
                assert_eq!(
                    na.generic_params.iter().map(|g| &g.inner).collect::<Vec<_>>(),
                    nb.generic_params.iter().map(|g| &g.inner).collect::<Vec<_>>()
                );
            }
            (Method::Native(na), Method::Native(nb)) => {
                assert_eq!(na.name, nb.name);
                assert_fn_params_eq_ignore_spans(&na.params, &nb.params);
                assert_eq!(na.return_type.as_ref().map(|t| &t.inner), nb.return_type.as_ref().map(|t| &t.inner));
                assert_eq!(
                    na.generic_params.iter().map(|g| &g.inner).collect::<Vec<_>>(),
                    nb.generic_params.iter().map(|g| &g.inner).collect::<Vec<_>>()
                );
            }
            _ => panic!("method mismatch: {a:?} vs {b:?}"),
        }
    }
    
    pub fn assert_method_signature_eq_ignore_spans(a: &MethodSignature, b: &MethodSignature) {
        assert_eq!(a.is_static, b.is_static, "method is_static mismatch");
        assert_eq!(a.is_mutating, b.is_mutating, "method is_mutating mismatch");
        assert_method_eq_ignore_spans(&a.method.inner, &b.method.inner);
    }
    
    pub fn assert_stmt_eq_ignore_spans(a: &Statement, b: &Statement) {
        match (a, b) {
            (
                Statement::Let {
                    pattern: ap,
                    expr: ae,
                    type_info: at,
                    mutable: am,
                },
                Statement::Let {
                    pattern: bp,
                    expr: be,
                    type_info: bt,
                    mutable: bm,
                },
            ) => {
                assert_eq!(am, bm, "let mut mismatch");
                assert_let_pattern_eq_ignore_spans(&ap.inner, &bp.inner);
                assert_expr_eq_ignore_spans(&ae.inner, &be.inner);
                match (at.as_ref(), bt.as_ref()) {
                    (None, None) => {}
                    (Some(atv), Some(btv)) => assert_type_info_eq_ignore_spans(&atv.inner, &btv.inner),
                    (x, y) => panic!("let type_info option mismatch: {x:?} vs {y:?}"),
                }
            }
    
            (Statement::Expr(ae), Statement::Expr(be)) => {
                assert_expr_eq_ignore_spans(&ae.inner, &be.inner);
            }
    
            (
                Statement::Fun {
                    name: an,
                    params: ap,
                    body: ab,
                    return_type: art,
                    generic_params: agp,
                },
                Statement::Fun {
                    name: bn,
                    params: bp,
                    body: bb,
                    return_type: brt,
                    generic_params: bgp,
                },
            ) => {
                assert_eq!(an, bn, "fun name mismatch");
                assert_fn_params_eq_ignore_spans(ap, bp);
                assert_expr_eq_ignore_spans(&ab.inner, &bb.inner);
                match (art.as_ref(), brt.as_ref()) {
                    (None, None) => {}
                    (Some(atv), Some(btv)) => assert_type_info_eq_ignore_spans(&atv.inner, &btv.inner),
                    (x, y) => panic!("fun return_type option mismatch: {x:?} vs {y:?}"),
                }
                assert_eq!(
                    agp.iter().map(|g| &g.inner).collect::<Vec<_>>(),
                    bgp.iter().map(|g| &g.inner).collect::<Vec<_>>()
                );
            }
    
            (
                Statement::TypeDef {
                    name: an,
                    type_info: ati,
                    generic_params: agp,
                    implementation: aimpl,
                    interfaces: aifaces,
                },
                Statement::TypeDef {
                    name: bn,
                    type_info: bti,
                    generic_params: bgp,
                    implementation: bimpl,
                    interfaces: bifaces,
                },
            ) => {
                assert_eq!(an, bn, "type name mismatch");
                assert_type_info_eq_ignore_spans(&ati.inner, &bti.inner);
                assert_eq!(
                    agp.iter().map(|g| &g.inner).collect::<Vec<_>>(),
                    bgp.iter().map(|g| &g.inner).collect::<Vec<_>>()
                );
                assert_eq!(
                    aifaces.iter().map(|i| &i.inner).collect::<Vec<_>>(),
                    bifaces.iter().map(|i| &i.inner).collect::<Vec<_>>()
                );
    
                assert_eq!(aimpl.len(), bimpl.len(), "impl map length mismatch");
                for (k, av) in aimpl.iter() {
                    let bv = bimpl
                        .get(k)
                        .unwrap_or_else(|| panic!("missing impl for key {k:?}"));
                    assert_eq!(av.len(), bv.len(), "impl vec length mismatch for key {k:?}");
                    for (asig, bsig) in av.iter().zip(bv.iter()) {
                        assert_method_signature_eq_ignore_spans(asig, bsig);
                    }
                }
            }
    
            (
                Statement::EnumDef {
                    name: an,
                    variants: av,
                    generic_params: agp,
                    implementation: aimpl,
                    interfaces: aifaces,
                },
                Statement::EnumDef {
                    name: bn,
                    variants: bv,
                    generic_params: bgp,
                    implementation: bimpl,
                    interfaces: bifaces,
                },
            ) => {
                assert_eq!(an, bn, "enum name mismatch");
                assert_eq!(av.len(), bv.len(), "enum variant count mismatch");
                for ((a_name, a_ty), (b_name, b_ty)) in av.iter().zip(bv.iter()) {
                    assert_eq!(a_name, b_name, "enum variant name mismatch");
                    match (a_ty.as_ref(), b_ty.as_ref()) {
                        (None, None) => {}
                        (Some(atv), Some(btv)) => assert_type_info_eq_ignore_spans(&atv.inner, &btv.inner),
                        (x, y) => panic!("enum variant type option mismatch: {x:?} vs {y:?}"),
                    }
                }
                assert_eq!(
                    agp.iter().map(|g| &g.inner).collect::<Vec<_>>(),
                    bgp.iter().map(|g| &g.inner).collect::<Vec<_>>()
                );
    
                assert_eq!(aimpl.len(), bimpl.len(), "impl map length mismatch");
                for (k, avs) in aimpl.iter() {
                    let bvs = bimpl.get(k).expect("missing impl entry");
                    assert_eq!(avs.len(), bvs.len(), "impl vec length mismatch for key {k:?}");
                    for (asig, bsig) in avs.iter().zip(bvs.iter()) {
                        assert_method_signature_eq_ignore_spans(asig, bsig);
                    }
                }
    
                assert_eq!(
                    aifaces.iter().map(|i| &i.inner).collect::<Vec<_>>(),
                    bifaces.iter().map(|i| &i.inner).collect::<Vec<_>>()
                );
            }
    
            (
                Statement::Import {
                    symbols: asyms,
                    path: apath,
                },
                Statement::Import {
                    symbols: bsyms,
                    path: bpath,
                },
            ) => {
                assert_eq!(apath.inner, bpath.inner, "import path mismatch");
                assert_eq!(asyms.len(), bsyms.len(), "import symbol count mismatch");
                for ((ak, aalias, a_is_type), (bk, balias, b_is_type)) in asyms.iter().zip(bsyms.iter()) {
                    assert_eq!(ak.inner, bk.inner, "import symbol name mismatch");
                    assert_eq!(a_is_type, b_is_type, "import symbol kind mismatch");
                    assert_eq!(aalias, balias, "import alias mismatch");
                }
            }
    
            (
                Statement::NativeFun {
                    name: an,
                    params: ap,
                    return_type: art,
                    generic_params: agp,
                },
                Statement::NativeFun {
                    name: bn,
                    params: bp,
                    return_type: brt,
                    generic_params: bgp,
                },
            ) => {
                assert_eq!(an, bn, "native fun name mismatch");
                assert_fn_params_eq_ignore_spans(ap, bp);
                match (art.as_ref(), brt.as_ref()) {
                    (None, None) => {}
                    (Some(atv), Some(btv)) => assert_type_info_eq_ignore_spans(&atv.inner, &btv.inner),
                    (x, y) => panic!("native fun return_type option mismatch: {x:?} vs {y:?}"),
                }
                assert_eq!(
                    agp.iter().map(|g| &g.inner).collect::<Vec<_>>(),
                    bgp.iter().map(|g| &g.inner).collect::<Vec<_>>()
                );
            }
    
            (
                Statement::NativeType {
                    name: an,
                    generic_params: agp,
                    implementation: aimpl,
                    interfaces: aifaces,
                },
                Statement::NativeType {
                    name: bn,
                    generic_params: bgp,
                    implementation: bimpl,
                    interfaces: bifaces,
                },
            ) => {
                assert_eq!(an, bn, "native type name mismatch");
                assert_eq!(
                    agp.iter().map(|g| &g.inner).collect::<Vec<_>>(),
                    bgp.iter().map(|g| &g.inner).collect::<Vec<_>>()
                );
    
                assert_eq!(aifaces.iter().map(|i| &i.inner).collect::<Vec<_>>(), bifaces.iter().map(|i| &i.inner).collect::<Vec<_>>());
    
                assert_eq!(aimpl.len(), bimpl.len(), "native type impl map length mismatch");
                for (k, av) in aimpl.iter() {
                    let bv = bimpl
                        .get(k)
                        .unwrap_or_else(|| panic!("missing native impl for key {k:?}"));
                    assert_eq!(av.len(), bv.len(), "native impl vec length mismatch for key {k:?}");
                    for (asig, bsig) in av.iter().zip(bv.iter()) {
                        assert_method_signature_eq_ignore_spans(asig, bsig);
                    }
                }
            }
    
            (Statement::Export(ae), Statement::Export(be)) => {
                assert_stmt_eq_ignore_spans(&ae.inner, &be.inner);
            }
    
            _ => panic!("statement mismatch: {a:?} vs {b:?}"),
        }
    }
    
    pub fn assert_stmts_eq_ignore_spans(a: &[Statement], b: &[Statement]) {
        assert_eq!(a.len(), b.len(), "statement count mismatch");
        for (sa, sb) in a.iter().zip(b.iter()) {
            assert_stmt_eq_ignore_spans(sa, sb);
        }
    }
}

