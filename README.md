# ABuff 
A statically typed interpreted language inspired by TypeScript and Rust.
## Overview
The language is designed to be a blend of TypeScript and Rust. It takes interpretability, most of the type system and garbage collection from the former, while taking enums, pattern matcing and trait system from the latter.

**Current status:** Experimental
### Why?
I was bored and decided to write a language from scratch (except for lexer which is made with `logos`).
## Features
### Type System
- Static typing with type inference
- Compile time type erasure (subject to change)
- Generics with explicit and implicit type parameters 
- Rust-like enums
- Type aliasing
### Module System
- JavaScript-style export/import statements
- Path based import resolution (subject to change)
### Misc
- REPL
- Error reporting with `ariadne`
## Installation
```bash
# Build from source
cargo build --release

# Run the REPL
cargo run

# Run tests
# Note: cargo test runs tests in parallel by default which breaks module system (I'm too lazy to fix that now)
cargo test -- --test-threads=1
```
## Roadmap
- [ ] Improve error spans
- [ ] Pattern matching
- [ ] Generic constraints
- [ ] Interface system
- [ ] Type level operations
- [ ] Performance
- [ ] Caching
- [ ] Expand standard library
- [ ] Native FFI
- [ ] Async module loading
- [ ] Tooling