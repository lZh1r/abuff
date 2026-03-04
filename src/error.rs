use ariadne::{Color, Label, Report, ReportKind, Source};
use smol_str::SmolStr;

use crate::span::Spanned;

pub fn build_report(e: Spanned<SmolStr>, src: &String, path: &String) {
    Report::build(ReportKind::Error, (path, e.span.into_range()))
        .with_message(e.inner.clone())
        .with_label(
            Label::new((path, e.span.into_range()))
                .with_message(e.inner)
                .with_color(Color::Red),
        )
        .finish()
        .print((path, Source::from(src)))
        .unwrap();
}