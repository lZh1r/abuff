use ariadne::{Color, Label, Report, ReportKind, Source};
use smol_str::SmolStr;

use crate::ast::Spanned;

pub fn build_report(e: Spanned<SmolStr>, src: &String) {
    Report::build(ReportKind::Error, e.span.into_range())
        .with_message(e.inner.clone())
        .with_label(
            Label::new(e.span.into_range())
                .with_message(e.inner)
                .with_color(Color::Red),
        )
        .finish()
        .print(Source::from(src.clone()))
        .unwrap();
}