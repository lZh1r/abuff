use logos::Logos;

use crate::ast::{Span, Spanned};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"///[^\n]*")] //TODO: actually parse this as docs
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    #[token("let")]
    Let,
        
    #[token("fun")]
    Fun,
    
    #[token("match")]
    Match,
    
    #[token("if")]
    If,
    
    #[token("else")]
    Else,
    
    #[token("while")]
    While,
    
    #[token("return")]
    Return,
    
    #[token("break")]
    Break,
    
    #[token("continue")]
    Continue,
    
    #[token("type")]
    Type,
    
    #[token("enum")]
    Enum,
    
    #[token("import")]
    Import,
    
    #[token("export")]
    Export,
    
    #[token("native")]
    Native,
    
    #[token("false")]
    False,
    
    #[token("true")]
    True,
    
    #[token("from")]
    From,
    
    #[token("as")]
    As,
    
    #[token("null")]
    Null,
    
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),
    
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Int(i64),
    
    #[regex(r"-?[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),
    
    #[regex(r#""([^"\\]|\\["\\nrt])*""#, parse_string)]
    String(String),
    
    #[regex("'.'", |lex| lex.slice().to_string().as_bytes()[1] as char)]
    Char(char),
    
    #[token("+")]
    Plus,
    
    #[token("-")]
    Minus,
    
    #[token("*")]
    Star,
    
    #[token("/")]
    Slash,
    
    #[token("%")]
    Percent,
    
    #[token("=")]
    Eq,
    
    #[token("==")]
    EqEq,
    
    #[token("!=")]
    NotEq,
    
    #[token("<")]
    Lt,
    
    #[token("<=")]
    LtEq,
    
    #[token(">")]
    Gt,
    
    #[token(">=")]
    GtEq,
    
    #[token("&&")]
    AndAnd,
    
    #[token("??")]
    NullCoal,
    
    #[token("||")]
    OrOr,
    
    #[token("!")]
    Bang,
    
    #[token("+=")]
    PlusEq,
    
    #[token("-=")]
    MinusEq,
    
    #[token("*=")]
    StarEq,
    
    #[token("/=")]
    SlashEq,
    
    #[token("%=")]
    PercentEq,
    
    #[token("->")]
    Arrow,
    
    #[token("(")]
    LParen,
    
    #[token(")")]
    RParen,
    
    #[token("{")]
    LBrace,
    
    #[token("}")]
    RBrace,
    
    #[token("[")]
    LBracket,
    
    #[token("]")]
    RBracket,
    
    #[token(",")]
    Comma,
    
    #[token(";")]
    Semicolon,
    
    #[token(":")]
    Colon,
    
    #[token(".")]
    Dot,
    
    #[token("...")]
    DotDotDot,
}

fn parse_string(lex: &mut logos::Lexer<Token>) -> String {
    let s = lex.slice();
    s[1..s.len()-1]
        .replace(r"\\", "\\")
        .replace(r#"\""#, "\"")
        .replace(r"\n", "\n")
        .replace(r"\r", "\r")
        .replace(r"\t", "\t")
}

pub fn lex(src: &str) -> Result<Vec<Spanned<Token>>, Spanned<String>> {
    let mut tokens = Vec::new();
    let mut lexer = Token::lexer(src);
    
    while let Some(result) = lexer.next() {
        match result {
            Ok(t) => tokens.push(Spanned {
                inner: t,
                span: Span::from(lexer.span())
            }),
            Err(_) => return Err(Spanned {
                inner: "Lexer Error".into(),
                span: Span::from(lexer.span())
            }),
        }
    }
    
    Ok(tokens)
}