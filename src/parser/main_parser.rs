use smol_str::{SmolStr};

use crate::{
    ast::{shared::{Operation}, typed::{Expr, FunctionParam, Statement}},
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
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use crate::{ast::{shared::UnaryOp, typed::{LetPattern, MatchArm, Method, MethodSignature, NormalMethod, TypeInfo}}, parser::test_helpers::test_helpers::{assert_stmts_eq_ignore_spans, parse_tokens, sp}};

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