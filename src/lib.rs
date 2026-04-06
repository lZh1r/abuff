pub mod ast {
    pub mod typed;
    pub mod clean;
    pub mod shared;
}
pub mod checker {
    pub mod flatten;
    pub mod mutability;
    pub mod statement;
    pub mod expression;
    pub mod hoisting;
    pub mod pattern_matching;
    pub mod function;
}
pub mod parser {
    pub mod main_parser;
    pub(crate) mod types;
    pub(crate) mod statements;
    pub(crate) mod test_helpers;
}
pub mod interpreter;
pub mod env;
pub mod error;
pub mod module;
pub mod native;
pub mod lexer;
pub mod span;
