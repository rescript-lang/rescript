//! ReScript parser implementation.
//!
//! This module provides the ReScript parser, converting source code into an
//! abstract syntax tree (AST). The parser is designed to be:
//!
//! - **Fast**: Efficient single-pass parsing with minimal allocations
//! - **Accurate**: Preserves source locations for all AST nodes
//! - **Recoverable**: Can continue parsing after encountering errors
//!
//! # Architecture
//!
//! The parser is organized into several submodules:
//!
//! - [`ast`] - Abstract syntax tree definitions
//! - [`comment`] - Comment representation
//! - [`token`] - Token definitions
//! - [`utf8`] - UTF-8 encoding/decoding utilities
//! - [`diagnostics`] - Parser diagnostics and error reporting
//! - [`scanner`] - Lexical analysis
//! - [`grammar`] - Grammar definitions for parsing contexts
//! - [`state`] - Parser state management
//! - [`core`] - Core parsing utilities and helpers
//! - [`expr`] - Expression parsing
//! - [`pattern`] - Pattern parsing
//! - [`typ`] - Type parsing
//! - [`module`] - Module and structure parsing
//!
//! # Example
//!
//! ```rust
//! use rescript_compiler::parser::{Token, Scanner};
//!
//! let mut scanner = Scanner::new("test.res", "let x = 42");
//! let result = scanner.scan();
//! assert!(matches!(result.token, Token::Let { unwrap: false }));
//! ```

pub mod ast;
#[cfg(test)]
pub mod benchmark;
pub mod code_frame;
pub mod comment;
pub mod core;
pub mod diagnostics;
pub mod expr;
pub mod grammar;
pub mod longident;
pub mod module;
pub mod pattern;
pub mod printer;
pub mod scanner;
pub mod state;
pub mod token;
pub mod typ;
pub mod utf8;
pub mod sexp;

// Re-exports
pub use ast::*;
pub use comment::{Comment, CommentStyle};
pub use diagnostics::{DiagnosticCategory, ParserDiagnostic};
pub use grammar::Grammar;
pub use longident::{Longident, LongidentLoc};
pub use printer::{
    Printer, print_core_type, print_expression, print_pattern, print_signature, print_structure,
    print_structure_with_comments, print_structure_with_comments_and_width, print_structure_with_width,
};
pub use scanner::{ScanResult, Scanner, ScannerMode, ScannerSnapshot};
pub use state::{Parser, ParserMode, RegionStatus};
pub use token::Token;
