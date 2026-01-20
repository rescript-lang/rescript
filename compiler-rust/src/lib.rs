//! ReScript Compiler - Rust Implementation
//!
//! This crate provides the ReScript compiler implementation in Rust,
//! designed for concurrent compilation with no global mutable state.
//!
//! # Architecture
//!
//! The compiler is organized around explicit context objects that are
//! passed through the compilation pipeline:
//!
//! - [`CompilerConfig`] - Immutable configuration shared across compilations
//! - [`CompilationContext`] - Per-compilation context with ID generation and diagnostics
//! - [`TypeContext`] - Per-type-checking context with type arena (future)
//! - [`ModuleCache`] - Thread-safe cache for compiled module interfaces
//!
//! # Concurrency Model
//!
//! Multiple files can be compiled in parallel because:
//! 1. No global mutable state - all state is in explicit contexts
//! 2. Each compilation gets its own `CompilationContext`
//! 3. Shared data uses thread-safe structures (`Arc`, `DashMap`)
//!
//! ```rust,ignore
//! use rescript_compiler::{CompilationContext, CompilerConfig, ModuleCache};
//! use rayon::prelude::*;
//!
//! let config = Arc::new(CompilerConfig::default());
//! let cache = Arc::new(ModuleCache::new());
//!
//! modules.par_iter().map(|module| {
//!     let ctx = CompilationContext::new(config.clone(), cache.clone());
//!     ctx.compile_module(module)
//! }).collect()
//! ```

#![warn(missing_docs)]
#![warn(rust_2018_idioms)]
#![allow(clippy::uninlined_format_args)] // Style preference, not a correctness issue

pub mod binary_ast;
pub mod cache;
pub mod config;
pub mod context;
pub mod diagnostics;
#[allow(unsafe_code)] // FFI requires unsafe
pub mod ffi;
pub mod ident;
pub mod js_ir;
pub mod lambda;
pub mod location;
pub mod parser;
pub mod driver;
pub mod types;

// Re-exports for convenience
pub use cache::ModuleCache;
pub use config::CompilerConfig;
pub use context::{CompilationContext, IdGenerator};
pub use diagnostics::DiagnosticsContext;
pub use ffi::{FfiBuffer, FfiResult, FfiStatus};
pub use ident::Ident;
pub use lambda::Lambda;
pub use location::Location;
pub use types::{Path, TypeContext, TypeExpr, Variance};

// Binary AST exports
pub use binary_ast::{Marshal, MarshalWriter};
