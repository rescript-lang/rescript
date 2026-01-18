//! Compilation context types.
//!
//! This module provides the context types that replace global mutable state
//! in the OCaml compiler. Each compilation unit gets its own context,
//! enabling concurrent compilation.
//!
//! # Context Hierarchy
//!
//! ```text
//! CompilerConfig (immutable, shared)
//!        │
//!        ▼
//! CompilationContext (per-compilation)
//!        │
//!        ├── IdGenerator (thread-safe stamp generation)
//!        ├── SourceContext (current file info)
//!        └── DiagnosticsContext (warnings/errors)
//!        │
//!        ▼
//! TypeContext (per-type-checking, future)
//! ```

use std::sync::Arc;
use std::sync::atomic::{AtomicI32, Ordering};

use crate::cache::ModuleCache;
use crate::config::CompilerConfig;
use crate::diagnostics::DiagnosticsContext;
use crate::ident::{Ident, IdentFlags};
use crate::location::Location;

/// Thread-safe identifier and type ID generator.
///
/// This replaces the global `Ident.currentstamp` from the OCaml implementation.
/// Multiple threads can safely generate unique identifiers concurrently.
///
/// # Example
///
/// ```rust
/// use rescript_compiler::context::IdGenerator;
///
/// let id_gen = IdGenerator::new();
/// let id1 = id_gen.create("x");
/// let id2 = id_gen.create("y");
///
/// assert_ne!(id1.stamp(), id2.stamp());
/// ```
#[derive(Debug)]
pub struct IdGenerator {
    /// Next stamp for identifiers. Starts at 1 (0 is reserved for persistent).
    next_stamp: AtomicI32,
    /// Next ID for type expressions.
    next_type_id: AtomicI32,
    /// Reinit level for reset functionality (-1 means not set).
    reinit_level: AtomicI32,
}

impl IdGenerator {
    /// Create a new ID generator with stamps starting at 1.
    pub fn new() -> Self {
        Self {
            next_stamp: AtomicI32::new(1),
            next_type_id: AtomicI32::new(1),
            reinit_level: AtomicI32::new(-1),
        }
    }

    /// Create a new ID generator starting at a specific stamp.
    ///
    /// Useful for resuming compilation or testing.
    pub fn with_start_stamp(start: i32) -> Self {
        Self {
            next_stamp: AtomicI32::new(start.max(1)),
            next_type_id: AtomicI32::new(1),
            reinit_level: AtomicI32::new(-1),
        }
    }

    /// Get the next stamp value and increment the counter.
    #[inline]
    pub fn next_stamp(&self) -> i32 {
        self.next_stamp.fetch_add(1, Ordering::Relaxed)
    }

    /// Get the next type ID and increment the counter.
    #[inline]
    pub fn next_type_id(&self) -> i32 {
        self.next_type_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Get the current stamp value without incrementing.
    #[inline]
    pub fn current_stamp(&self) -> i32 {
        self.next_stamp.load(Ordering::Relaxed)
    }

    /// Set the current stamp to at least the given value.
    ///
    /// This is used to synchronize stamps when loading compiled modules.
    #[inline]
    pub fn set_current_stamp(&self, t: i32) {
        // Update to max of current and t
        let mut current = self.next_stamp.load(Ordering::Relaxed);
        while t > current {
            match self.next_stamp.compare_exchange_weak(
                current,
                t,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(c) => current = c,
            }
        }
    }

    /// Initialize the reinit level if not already set.
    pub fn init_reinit_level(&self) {
        let current = self.reinit_level.load(Ordering::Relaxed);
        if current < 0 {
            let stamp = self.next_stamp.load(Ordering::Relaxed);
            self.reinit_level.store(stamp, Ordering::Relaxed);
        }
    }

    /// Reset the stamp counter to the reinit level.
    ///
    /// Does nothing if reinit level was never initialized.
    pub fn reinit(&self) {
        let level = self.reinit_level.load(Ordering::Relaxed);
        if level >= 0 {
            self.next_stamp.store(level, Ordering::Relaxed);
        }
    }

    /// Create a new local identifier.
    #[inline]
    pub fn create(&self, name: &str) -> Ident {
        let stamp = self.next_stamp();
        Ident::new(name, stamp, IdentFlags::NONE)
    }

    /// Create a new predefined exception identifier.
    #[inline]
    pub fn create_predef_exn(&self, name: &str) -> Ident {
        let stamp = self.next_stamp();
        Ident::new(name, stamp, IdentFlags::PREDEF_EXN)
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for IdGenerator {
    /// Creates a new generator with the same current state.
    ///
    /// Note: The cloned generator will produce different stamps than the original
    /// after cloning, as they maintain independent counters.
    fn clone(&self) -> Self {
        Self {
            next_stamp: AtomicI32::new(self.next_stamp.load(Ordering::Relaxed)),
            next_type_id: AtomicI32::new(self.next_type_id.load(Ordering::Relaxed)),
            reinit_level: AtomicI32::new(self.reinit_level.load(Ordering::Relaxed)),
        }
    }
}

/// Source file context for the current compilation.
///
/// This replaces `Location.input_name` from the OCaml implementation.
#[derive(Debug, Clone)]
pub struct SourceContext {
    /// The input file name.
    pub input_name: String,
    /// The module name derived from the file.
    pub module_name: String,
    /// The output prefix for generated files.
    pub output_prefix: String,
}

impl SourceContext {
    /// Create a new source context for a file.
    pub fn new(input_name: impl Into<String>) -> Self {
        let input_name = input_name.into();
        let module_name = derive_module_name(&input_name);
        let output_prefix = derive_output_prefix(&input_name);
        Self {
            input_name,
            module_name,
            output_prefix,
        }
    }

    /// Create a dummy context for testing.
    pub fn dummy() -> Self {
        Self {
            input_name: "_none_".into(),
            module_name: "".into(),
            output_prefix: "".into(),
        }
    }

    /// Create a location in this source file.
    pub fn location(&self, start: usize, end: usize) -> Location {
        Location::new(&self.input_name, start, end)
    }
}

impl Default for SourceContext {
    fn default() -> Self {
        Self::dummy()
    }
}

/// Derive module name from file path.
fn derive_module_name(path: &str) -> String {
    std::path::Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| {
            // Capitalize first letter
            let mut chars = s.chars();
            match chars.next() {
                None => String::new(),
                Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
            }
        })
        .unwrap_or_default()
}

/// Derive output prefix from file path.
fn derive_output_prefix(path: &str) -> String {
    std::path::Path::new(path)
        .with_extension("")
        .to_string_lossy()
        .into_owned()
}

/// Per-compilation context.
///
/// This is the main context passed through the compilation pipeline.
/// Each compilation unit (file) gets its own `CompilationContext`.
///
/// # Thread Safety
///
/// `CompilationContext` is designed to be used by a single thread,
/// but multiple contexts can be used concurrently. The `IdGenerator`
/// is thread-safe if you need to share it across contexts.
///
/// # Example
///
/// ```rust
/// use rescript_compiler::{CompilationContext, CompilerConfig, ModuleCache};
/// use std::sync::Arc;
///
/// let config = Arc::new(CompilerConfig::default());
/// let cache = Arc::new(ModuleCache::new());
/// let ctx = CompilationContext::new(config, cache, "src/MyModule.res");
///
/// let id = ctx.create_ident("x");
/// assert_eq!(id.name(), "x");
/// ```
#[derive(Debug)]
pub struct CompilationContext {
    /// Compiler configuration (immutable, shared).
    config: Arc<CompilerConfig>,
    /// Module cache (thread-safe, shared).
    module_cache: Arc<ModuleCache>,
    /// ID generator for this compilation.
    id_gen: IdGenerator,
    /// Source file information.
    source: SourceContext,
    /// Diagnostics collector.
    diagnostics: DiagnosticsContext,
}

impl CompilationContext {
    /// Create a new compilation context for a source file.
    pub fn new(
        config: Arc<CompilerConfig>,
        module_cache: Arc<ModuleCache>,
        input_file: impl Into<String>,
    ) -> Self {
        Self {
            config,
            module_cache,
            id_gen: IdGenerator::new(),
            source: SourceContext::new(input_file),
            diagnostics: DiagnosticsContext::new(),
        }
    }

    /// Create a context with a specific ID generator.
    ///
    /// Useful when you need to control stamp generation.
    pub fn with_id_gen(
        config: Arc<CompilerConfig>,
        module_cache: Arc<ModuleCache>,
        id_gen: IdGenerator,
        input_file: impl Into<String>,
    ) -> Self {
        Self {
            config,
            module_cache,
            id_gen,
            source: SourceContext::new(input_file),
            diagnostics: DiagnosticsContext::new(),
        }
    }

    /// Get the compiler configuration.
    #[inline]
    pub fn config(&self) -> &CompilerConfig {
        &self.config
    }

    /// Get the module cache.
    #[inline]
    pub fn module_cache(&self) -> &ModuleCache {
        &self.module_cache
    }

    /// Get the ID generator.
    #[inline]
    pub fn id_gen(&self) -> &IdGenerator {
        &self.id_gen
    }

    /// Get the source context.
    #[inline]
    pub fn source(&self) -> &SourceContext {
        &self.source
    }

    /// Get the diagnostics context.
    #[inline]
    pub fn diagnostics(&self) -> &DiagnosticsContext {
        &self.diagnostics
    }

    /// Get mutable access to the diagnostics context.
    #[inline]
    pub fn diagnostics_mut(&mut self) -> &mut DiagnosticsContext {
        &mut self.diagnostics
    }

    /// Get the input file name.
    #[inline]
    pub fn input_name(&self) -> &str {
        &self.source.input_name
    }

    /// Get the module name.
    #[inline]
    pub fn module_name(&self) -> &str {
        &self.source.module_name
    }

    /// Create a new identifier.
    #[inline]
    pub fn create_ident(&self, name: &str) -> Ident {
        self.id_gen.create(name)
    }

    /// Create a persistent identifier.
    #[inline]
    pub fn create_persistent_ident(name: &str) -> Ident {
        Ident::create_persistent(name)
    }

    /// Check if we're in debug mode.
    #[inline]
    pub fn debug(&self) -> bool {
        self.config.debug
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_id_generator_sequential() {
        let id_gen = IdGenerator::new();
        let stamps: Vec<i32> = (0..100).map(|_| id_gen.next_stamp()).collect();

        // All stamps should be unique and increasing
        for i in 1..stamps.len() {
            assert!(stamps[i] > stamps[i - 1]);
        }
    }

    #[test]
    fn test_id_generator_concurrent() {
        use std::thread;

        let id_gen = Arc::new(IdGenerator::new());
        let mut handles = vec![];

        for _ in 0..10 {
            let id_gen = id_gen.clone();
            handles.push(thread::spawn(move || {
                (0..1000).map(|_| id_gen.next_stamp()).collect::<Vec<_>>()
            }));
        }

        let all_stamps: Vec<i32> = handles
            .into_iter()
            .flat_map(|h| h.join().unwrap())
            .collect();

        // All stamps should be unique
        let unique: std::collections::HashSet<_> = all_stamps.iter().collect();
        assert_eq!(unique.len(), all_stamps.len());
    }

    #[test]
    fn test_source_context() {
        let ctx = SourceContext::new("src/MyModule.res");
        assert_eq!(ctx.input_name, "src/MyModule.res");
        assert_eq!(ctx.module_name, "MyModule");
        assert_eq!(ctx.output_prefix, "src/MyModule");
    }

    #[test]
    fn test_compilation_context() {
        let config = Arc::new(CompilerConfig::default());
        let cache = Arc::new(ModuleCache::new());
        let ctx = CompilationContext::new(config, cache, "test.res");

        let id1 = ctx.create_ident("x");
        let id2 = ctx.create_ident("y");

        assert_ne!(id1.stamp(), id2.stamp());
        assert_eq!(ctx.module_name(), "Test");
    }
}
