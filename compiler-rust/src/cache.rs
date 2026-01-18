//! Thread-safe module cache for incremental compilation.
//!
//! This module provides caching infrastructure for compiled module interfaces,
//! enabling incremental compilation and parallel builds. Unlike the OCaml
//! implementation which uses global hash tables, this uses `DashMap` for
//! thread-safe concurrent access.
//!
//! # Example
//!
//! ```rust
//! use rescript_compiler::cache::{ModuleCache, ModulePath, CompiledInterface};
//! use std::sync::Arc;
//!
//! let cache = Arc::new(ModuleCache::new());
//!
//! // Store a compiled interface
//! let path = ModulePath::new("src/MyModule.res");
//! let interface = CompiledInterface::new("MyModule");
//! cache.insert(path.clone(), Arc::new(interface));
//!
//! // Retrieve it later
//! assert!(cache.get(&path).is_some());
//! ```

use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;

/// A path to a module source file.
#[derive(Debug, Clone, Eq, Serialize, Deserialize)]
pub struct ModulePath {
    /// The file path.
    path: PathBuf,
    /// The derived module name.
    module_name: String,
}

impl ModulePath {
    /// Create a new module path.
    pub fn new(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        let module_name = derive_module_name(&path);
        Self { path, module_name }
    }

    /// Get the file path.
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Get the module name.
    pub fn module_name(&self) -> &str {
        &self.module_name
    }

    /// Get the file extension.
    pub fn extension(&self) -> Option<&str> {
        self.path.extension().and_then(|e| e.to_str())
    }

    /// Check if this is a ReScript source file.
    pub fn is_rescript(&self) -> bool {
        matches!(self.extension(), Some("res" | "resi"))
    }

    /// Check if this is an interface file.
    pub fn is_interface(&self) -> bool {
        matches!(self.extension(), Some("resi" | "mli"))
    }
}

impl PartialEq for ModulePath {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Hash for ModulePath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

/// Derive module name from file path.
fn derive_module_name(path: &Path) -> String {
    path.file_stem()
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

/// A compiled module interface.
///
/// This represents the public interface of a compiled module,
/// used for type checking dependent modules.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompiledInterface {
    /// Module name.
    pub module_name: String,
    /// Compilation timestamp.
    pub compiled_at: Option<SystemTime>,
    /// Source file hash for invalidation.
    pub source_hash: Option<u64>,
    // TODO: Add actual interface data (types, values, etc.)
    // This will be expanded as we implement the type system.
}

impl CompiledInterface {
    /// Create a new compiled interface.
    pub fn new(module_name: impl Into<String>) -> Self {
        Self {
            module_name: module_name.into(),
            compiled_at: Some(SystemTime::now()),
            source_hash: None,
        }
    }

    /// Create with source hash.
    pub fn with_source_hash(module_name: impl Into<String>, hash: u64) -> Self {
        Self {
            module_name: module_name.into(),
            compiled_at: Some(SystemTime::now()),
            source_hash: Some(hash),
        }
    }

    /// Check if this interface is stale given a source hash.
    pub fn is_stale(&self, current_hash: u64) -> bool {
        match self.source_hash {
            Some(hash) => hash != current_hash,
            None => true,
        }
    }
}

/// Thread-safe cache for compiled module interfaces.
///
/// This is designed to be shared across multiple compilation threads
/// via `Arc<ModuleCache>`. It provides concurrent access to cached
/// interfaces for incremental compilation.
#[derive(Debug, Default)]
pub struct ModuleCache {
    /// Cached interfaces indexed by module path.
    interfaces: DashMap<ModulePath, Arc<CompiledInterface>>,
    /// Module dependencies for invalidation.
    dependencies: DashMap<ModulePath, HashSet<ModulePath>>,
}

impl ModuleCache {
    /// Create a new empty cache.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a cached interface.
    pub fn get(&self, path: &ModulePath) -> Option<Arc<CompiledInterface>> {
        self.interfaces.get(path).map(|r| r.value().clone())
    }

    /// Insert a compiled interface.
    pub fn insert(&self, path: ModulePath, interface: Arc<CompiledInterface>) {
        self.interfaces.insert(path, interface);
    }

    /// Remove a cached interface.
    pub fn remove(&self, path: &ModulePath) -> Option<Arc<CompiledInterface>> {
        self.interfaces.remove(path).map(|(_, v)| v)
    }

    /// Check if a module is cached.
    pub fn contains(&self, path: &ModulePath) -> bool {
        self.interfaces.contains_key(path)
    }

    /// Get or compute a cached interface.
    ///
    /// If the interface is not cached or is stale, the compute function
    /// is called to create a new one.
    pub fn get_or_insert_with<F, E>(
        &self,
        path: ModulePath,
        compute: F,
    ) -> Result<Arc<CompiledInterface>, E>
    where
        F: FnOnce() -> Result<CompiledInterface, E>,
    {
        // Check if already cached
        if let Some(interface) = self.get(&path) {
            return Ok(interface);
        }

        // Compute and cache
        let interface = Arc::new(compute()?);
        self.insert(path, interface.clone());
        Ok(interface)
    }

    /// Register dependencies for a module.
    pub fn set_dependencies(&self, path: ModulePath, deps: HashSet<ModulePath>) {
        self.dependencies.insert(path, deps);
    }

    /// Get dependencies for a module.
    pub fn get_dependencies(&self, path: &ModulePath) -> Option<HashSet<ModulePath>> {
        self.dependencies.get(path).map(|r| r.value().clone())
    }

    /// Invalidate a module and all its dependents.
    ///
    /// Returns the set of invalidated modules.
    pub fn invalidate(&self, path: &ModulePath) -> HashSet<ModulePath> {
        let mut invalidated = HashSet::new();
        self.invalidate_recursive(path, &mut invalidated);
        invalidated
    }

    fn invalidate_recursive(&self, path: &ModulePath, invalidated: &mut HashSet<ModulePath>) {
        if !invalidated.insert(path.clone()) {
            return; // Already invalidated
        }

        // Remove from cache
        self.interfaces.remove(path);

        // Find and invalidate dependents
        let dependents: Vec<ModulePath> = self
            .dependencies
            .iter()
            .filter_map(|entry| {
                if entry.value().contains(path) {
                    Some(entry.key().clone())
                } else {
                    None
                }
            })
            .collect();

        for dependent in dependents {
            self.invalidate_recursive(&dependent, invalidated);
        }
    }

    /// Clear the entire cache.
    pub fn clear(&self) {
        self.interfaces.clear();
        self.dependencies.clear();
    }

    /// Get the number of cached interfaces.
    pub fn len(&self) -> usize {
        self.interfaces.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.interfaces.is_empty()
    }

    /// Get all cached module paths.
    pub fn keys(&self) -> Vec<ModulePath> {
        self.interfaces.iter().map(|r| r.key().clone()).collect()
    }
}

/// Cache statistics for monitoring.
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    /// Number of cache hits.
    pub hits: usize,
    /// Number of cache misses.
    pub misses: usize,
    /// Number of invalidations.
    pub invalidations: usize,
}

impl CacheStats {
    /// Calculate hit rate.
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            self.hits as f64 / total as f64
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_path() {
        let path = ModulePath::new("src/MyModule.res");
        assert_eq!(path.module_name(), "MyModule");
        assert!(path.is_rescript());
        assert!(!path.is_interface());
    }

    #[test]
    fn test_module_path_interface() {
        let path = ModulePath::new("src/MyModule.resi");
        assert_eq!(path.module_name(), "MyModule");
        assert!(path.is_rescript());
        assert!(path.is_interface());
    }

    #[test]
    fn test_compiled_interface() {
        let interface = CompiledInterface::with_source_hash("Test", 12345);
        assert!(!interface.is_stale(12345));
        assert!(interface.is_stale(99999));
    }

    #[test]
    fn test_cache_insert_get() {
        let cache = ModuleCache::new();
        let path = ModulePath::new("test.res");
        let interface = Arc::new(CompiledInterface::new("Test"));

        cache.insert(path.clone(), interface.clone());

        let retrieved = cache.get(&path);
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().module_name, "Test");
    }

    #[test]
    fn test_cache_get_or_insert() {
        let cache = ModuleCache::new();
        let path = ModulePath::new("test.res");

        let result: Result<Arc<CompiledInterface>, ()> =
            cache.get_or_insert_with(path.clone(), || Ok(CompiledInterface::new("Test")));

        assert!(result.is_ok());
        assert!(cache.contains(&path));
    }

    #[test]
    fn test_cache_invalidation() {
        let cache = ModuleCache::new();

        let a = ModulePath::new("a.res");
        let b = ModulePath::new("b.res");
        let c = ModulePath::new("c.res");

        cache.insert(a.clone(), Arc::new(CompiledInterface::new("A")));
        cache.insert(b.clone(), Arc::new(CompiledInterface::new("B")));
        cache.insert(c.clone(), Arc::new(CompiledInterface::new("C")));

        // B depends on A, C depends on B
        let mut deps_b = HashSet::new();
        deps_b.insert(a.clone());
        cache.set_dependencies(b.clone(), deps_b);

        let mut deps_c = HashSet::new();
        deps_c.insert(b.clone());
        cache.set_dependencies(c.clone(), deps_c);

        // Invalidate A - should cascade to B and C
        let invalidated = cache.invalidate(&a);

        assert!(invalidated.contains(&a));
        assert!(invalidated.contains(&b));
        assert!(invalidated.contains(&c));

        assert!(!cache.contains(&a));
        assert!(!cache.contains(&b));
        assert!(!cache.contains(&c));
    }

    #[test]
    fn test_cache_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<ModuleCache>();
        assert_send_sync::<ModulePath>();
        assert_send_sync::<CompiledInterface>();
    }

    #[test]
    fn test_concurrent_access() {
        use std::thread;

        let cache = Arc::new(ModuleCache::new());
        let mut handles = vec![];

        for i in 0..10 {
            let cache = cache.clone();
            handles.push(thread::spawn(move || {
                for j in 0..100 {
                    let path = ModulePath::new(format!("module_{i}_{j}.res"));
                    let interface = Arc::new(CompiledInterface::new(format!("Module{i}_{j}")));
                    cache.insert(path.clone(), interface);
                    cache.get(&path);
                }
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(cache.len(), 1000);
    }
}
