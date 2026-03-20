use logos::Logos;
use smol_str::SmolStr;

use crate::span::{Span, Spanned};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"///[^\n]*")] //TODO: actually parse this as docs
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    #[token("let")]
    Let,
    
    #[token("mut")]
    Mut,
        
    #[token("fun")]
    Fun,
    
    #[token("match")]
    Match,
    
    #[token("if")]
    If,
    
    #[token("else")]
    Else,
    
    #[token("for")]
    For,
    
    #[token("in")]
    In,
    
    #[token("while")]
    While,
    
    #[token("return")]
    Return,
    
    #[token("break")]
    Break,
    
    #[token("continue")]
    Continue,
    
    #[token("panic")]
    Panic,
    
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
    
    #[token("impl")]
    Impl,
    
    #[token("static")]
    Static,
    
    #[token("::")]
    ColonColon,
    
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| SmolStr::new(lex.slice()))]
    Ident(SmolStr),
    
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Int(i64),
    
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),
    
    #[regex(r#""([^"\\]|\\["\\nrt]|\\x[0-9a-fA-F]{2}|\\u\{[0-9a-fA-F]+\})*""#, parse_string)]
    String(SmolStr),
    
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
    
    #[token("&")]
    Ampersand,
    
    #[token("|")]
    Pipe,
    
    #[token("^")]
    Caret,
    
    #[token("<<")]
    LeftShift,
    
    #[token(">>")]
    RightShift,
    
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

fn parse_string(lex: &mut logos::Lexer<Token>) -> SmolStr {
    let s = lex.slice();
    let raw = &s[1..s.len() - 1]; // strip surrounding quotes

    let mut out = String::new();
    let mut it = raw.chars().peekable();

    while let Some(c) = it.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }

        // We saw a backslash, so an escape must follow (your regex guarantees this)
        let esc = it.next().unwrap();

        match esc {
            '\\' => out.push('\\'),
            '"' => out.push('"'),
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            't' => out.push('\t'),

            // \xHH where HH are exactly 2 hex digits
            'x' => {
                let h1 = it.next().unwrap();
                let h2 = it.next().unwrap();
                let hex = format!("{}{}", h1, h2);
                let byte = u8::from_str_radix(&hex, 16).unwrap();
                out.push(byte as char); // ASCII control codes like 0x1B will work
            }

            // \u{...} where ... is 1+ hex digits
            'u' => {
                // expect '{'
                let brace = it.next().unwrap();
                if brace != '{' {
                    // unreachable if regex is correct, but keep behavior defined
                    out.push('u');
                    continue;
                }

                let mut hex = String::new();
                while let Some(&d) = it.peek() {
                    if d == '}' {
                        break;
                    }
                    hex.push(d);
                    it.next();
                }

                // consume '}'
                it.next();

                let cp = u32::from_str_radix(&hex, 16).unwrap();
                out.push(char::from_u32(cp).unwrap_or('\u{FFFD}'));
            }
            
            other => out.push(other),
        }
    }

    SmolStr::new(out)
}

pub fn lex(src: &str) -> Result<Vec<Spanned<Token>>, Spanned<SmolStr>> {
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