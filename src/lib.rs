pub mod ast {
    pub mod typed;
    pub mod clean;
    pub mod shared;
}
pub mod checker {
    pub mod flatten;
    pub mod mutability;
}
pub mod interpreter;
pub mod env;
pub mod error;
pub mod module;
pub mod native;
pub mod lexer;
pub mod main_parser;
pub mod type_checker;
pub mod span;
pub mod pattern_matching;