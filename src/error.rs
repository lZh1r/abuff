use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::ast::Spanned;

pub fn build_report(e: Spanned<String>, src: &String) {
    Report::build(ReportKind::Error, e.span.into_range())
        .with_message(e.to_string())
        .with_label(
            Label::new(e.span.into_range())
                .with_message(e.inner)
                .with_color(Color::Red),
        )
        .finish()
        .print(Source::from(src.clone()))
        .unwrap();
}