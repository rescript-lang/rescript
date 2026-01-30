//! String interning for efficient storage and sharing.
//!
//! This module provides string interning, which stores each unique string
//! only once and returns a lightweight index. When the same string is
//! interned multiple times, the same index is returned.
//!
//! This is critical for matching OCaml's behavior where identifier strings
//! are interned, causing Marshal to share them via back-references.
//!
//! # Example
//!
//! ```ignore
//! use rescript_compiler::intern::Interner;
//!
//! let mut interner = Interner::new();
//! let idx1 = interner.intern("hello");
//! let idx2 = interner.intern("hello");
//! assert_eq!(idx1, idx2);  // Same string = same index
//!
//! let idx3 = interner.intern("world");
//! assert_ne!(idx1, idx3);  // Different string = different index
//!
//! assert_eq!(interner.get(idx1), "hello");
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// String Index
// ============================================================================

/// Index into the string interner.
///
/// This is a lightweight 4-byte value that can be copied freely.
/// Two StrIdx values are equal if and only if they refer to the same string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StrIdx(u32);

impl StrIdx {
    /// Create a StrIdx from a raw u32 value.
    #[inline]
    pub fn from_raw(idx: u32) -> Self {
        StrIdx(idx)
    }

    /// Get the raw u32 value.
    #[inline]
    pub fn raw(self) -> u32 {
        self.0
    }
}

// ============================================================================
// String Interner
// ============================================================================

/// String interner that stores unique strings and returns indices.
///
/// This provides O(1) lookup of previously interned strings and guarantees
/// that identical strings get the same index.
#[derive(Debug)]
pub struct Interner {
    /// Maps string content to its index
    map: HashMap<String, StrIdx>,
    /// Stores the actual strings, indexed by StrIdx
    strings: Vec<String>,
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

impl Interner {
    /// Create a new empty interner.
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            strings: Vec::new(),
        }
    }

    /// Create a new interner with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: HashMap::with_capacity(capacity),
            strings: Vec::with_capacity(capacity),
        }
    }

    /// Intern a string, returning its index.
    ///
    /// If the string was previously interned, returns the existing index.
    /// Otherwise, stores the string and returns a new index.
    pub fn intern(&mut self, s: &str) -> StrIdx {
        if let Some(&idx) = self.map.get(s) {
            return idx;
        }

        let idx = StrIdx(self.strings.len() as u32);
        self.strings.push(s.to_string());
        self.map.insert(s.to_string(), idx);
        idx
    }

    /// Intern an owned string, returning its index.
    ///
    /// This avoids an allocation if the string is not already interned.
    pub fn intern_owned(&mut self, s: String) -> StrIdx {
        if let Some(&idx) = self.map.get(&s) {
            return idx;
        }

        let idx = StrIdx(self.strings.len() as u32);
        self.map.insert(s.clone(), idx);
        self.strings.push(s);
        idx
    }

    /// Push a string without deduplication, always creating a new entry.
    ///
    /// Use this for dynamic strings (from user input/tokens) that should NOT
    /// be shared during marshalling. Each call returns a unique index.
    ///
    /// For static strings that should be shared, use `intern()` instead.
    pub fn push(&mut self, s: String) -> StrIdx {
        let idx = StrIdx(self.strings.len() as u32);
        self.strings.push(s);
        // Note: We don't add to the map, so this string won't be found by intern()
        idx
    }

    /// Get the string for an index.
    ///
    /// # Panics
    ///
    /// Panics if the index is invalid.
    pub fn get(&self, idx: StrIdx) -> &str {
        &self.strings[idx.0 as usize]
    }

    /// Get the string for an index, returning None if invalid.
    pub fn try_get(&self, idx: StrIdx) -> Option<&str> {
        self.strings.get(idx.0 as usize).map(|s| s.as_str())
    }

    /// Return the number of unique strings interned.
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Check if the interner is empty.
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern_returns_same_idx_for_same_string() {
        let mut interner = Interner::new();
        let idx1 = interner.intern("hello");
        let idx2 = interner.intern("hello");
        assert_eq!(idx1, idx2);
    }

    #[test]
    fn test_intern_returns_different_idx_for_different_string() {
        let mut interner = Interner::new();
        let idx1 = interner.intern("hello");
        let idx2 = interner.intern("world");
        assert_ne!(idx1, idx2);
    }

    #[test]
    fn test_get_returns_interned_string() {
        let mut interner = Interner::new();
        let idx = interner.intern("hello");
        assert_eq!(interner.get(idx), "hello");
    }

    #[test]
    fn test_intern_owned() {
        let mut interner = Interner::new();
        let idx1 = interner.intern_owned("hello".to_string());
        let idx2 = interner.intern("hello");
        assert_eq!(idx1, idx2);
    }

    #[test]
    fn test_len() {
        let mut interner = Interner::new();
        assert_eq!(interner.len(), 0);
        interner.intern("a");
        assert_eq!(interner.len(), 1);
        interner.intern("b");
        assert_eq!(interner.len(), 2);
        interner.intern("a"); // Already interned
        assert_eq!(interner.len(), 2);
    }
}
