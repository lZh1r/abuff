use std::{fmt::Display, ops::Range};

use smol_str::SmolStr;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize
}

impl Span {
    pub fn from(range: Range<usize>) -> Self {
        Span { start: range.start, end: range.end }
    }
    pub fn into_range(&self) -> Range<usize> {
        self.start..self.end
    }
    pub fn at(point: usize) -> Self {
        Span { start: point, end: point }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span
}

impl Display for Spanned<SmolStr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}


pub fn spanned<T>(inner: T, span: Span) -> Spanned<T> {
    Spanned { inner, span }
}