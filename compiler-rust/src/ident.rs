//! Identifier types and operations.
//!
//! This module provides the core [`Ident`] type used throughout the compiler
//! to represent identifiers with unique stamps for disambiguation.
//!
//! # Concurrency
//!
//! Unlike the OCaml implementation which uses a global `currentstamp` counter,
//! this implementation requires an [`IdGenerator`] to be passed explicitly.
//! This enables concurrent compilation of multiple files.
//!
//! # Example
//!
//! ```rust
//! use rescript_compiler::ident::Ident;
//! use rescript_compiler::context::IdGenerator;
//!
//! let id_gen = IdGenerator::new();
//! let id1 = id_gen.create("x");
//! let id2 = id_gen.create("x");
//!
//! // Same name but different stamps
//! assert_eq!(id1.name(), "x");
//! assert_eq!(id2.name(), "x");
//! assert_ne!(id1.stamp(), id2.stamp());
//! assert!(!id1.same(&id2));  // Different identifiers
//! ```

use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::fmt;
use std::hash::{Hash, Hasher};

/// Flags for identifier properties.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct IdentFlags(u8);

impl IdentFlags {
    /// No flags set.
    pub const NONE: Self = Self(0);
    /// Identifier is global.
    pub const GLOBAL: Self = Self(1);
    /// Identifier is a predefined exception.
    pub const PREDEF_EXN: Self = Self(2);

    /// Check if the global flag is set.
    #[inline]
    pub fn is_global(self) -> bool {
        self.0 & Self::GLOBAL.0 != 0
    }

    /// Check if the predefined exception flag is set.
    #[inline]
    pub fn is_predef_exn(self) -> bool {
        self.0 & Self::PREDEF_EXN.0 != 0
    }

    /// Set the global flag.
    #[inline]
    pub fn with_global(self) -> Self {
        Self(self.0 | Self::GLOBAL.0)
    }

    /// Set the predefined exception flag.
    #[inline]
    pub fn with_predef_exn(self) -> Self {
        Self(self.0 | Self::PREDEF_EXN.0)
    }
}

/// A unique identifier with a name and stamp.
///
/// Identifiers are used throughout the compiler to represent variables,
/// types, modules, and other named entities. Each identifier has:
///
/// - A `name`: The string representation
/// - A `stamp`: A unique integer for disambiguation
/// - `flags`: Properties like global or predefined exception
///
/// # Stamp Semantics
///
/// - `stamp > 0`: A local identifier created by `create`
/// - `stamp = 0`: A persistent (global) identifier
/// - `stamp = -1`: A hidden identifier
///
/// # Equality
///
/// Two identifiers are considered the "same" (`same` method) if:
/// - Both have non-zero stamps and the stamps are equal, OR
/// - Both have stamp 0 and the names are equal
#[derive(Clone, Serialize, Deserialize)]
pub struct Ident {
    name: SmolStr,
    stamp: i32,
    flags: IdentFlags,
}

impl Ident {
    /// Create a new identifier with explicit stamp and flags.
    ///
    /// Prefer using [`IdGenerator::create`] for normal identifier creation.
    #[inline]
    pub fn new(name: impl Into<SmolStr>, stamp: i32, flags: IdentFlags) -> Self {
        Self {
            name: name.into(),
            stamp,
            flags,
        }
    }

    /// Create a persistent (global) identifier with stamp 0.
    ///
    /// Persistent identifiers represent globally visible names like
    /// module names or external declarations.
    #[inline]
    pub fn create_persistent(name: impl Into<SmolStr>) -> Self {
        Self {
            name: name.into(),
            stamp: 0,
            flags: IdentFlags::GLOBAL,
        }
    }

    /// Create a local identifier with an auto-incrementing stamp.
    ///
    /// This uses a thread-safe global counter for stamp generation.
    /// For concurrent compilation, prefer using [`IdGenerator`] explicitly.
    ///
    /// This is mainly useful for tests and simple cases where
    /// thread isolation isn't required.
    #[inline]
    pub fn create_local(name: impl Into<SmolStr>) -> Self {
        use std::sync::atomic::{AtomicI32, Ordering};
        static COUNTER: AtomicI32 = AtomicI32::new(1);
        Self {
            name: name.into(),
            stamp: COUNTER.fetch_add(1, Ordering::Relaxed),
            flags: IdentFlags::NONE,
        }
    }

    /// Get the name of this identifier.
    #[inline]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the stamp of this identifier.
    #[inline]
    pub fn stamp(&self) -> i32 {
        self.stamp
    }

    /// Get the flags of this identifier.
    #[inline]
    pub fn flags(&self) -> IdentFlags {
        self.flags
    }

    /// Create a unique name by combining name and stamp.
    ///
    /// Format: `name_stamp`
    #[inline]
    pub fn unique_name(&self) -> String {
        format!("{}_{}", self.name, self.stamp)
    }

    /// Create a unique toplevel name.
    ///
    /// Format: `name/stamp`
    #[inline]
    pub fn unique_toplevel_name(&self) -> String {
        format!("{}/{}", self.name, self.stamp)
    }

    /// Check if this is a persistent identifier (stamp = 0).
    #[inline]
    pub fn is_persistent(&self) -> bool {
        self.stamp == 0
    }

    /// Check if this is a hidden identifier (stamp = -1).
    #[inline]
    pub fn is_hidden(&self) -> bool {
        self.stamp == -1
    }

    /// Check if the global flag is set.
    #[inline]
    pub fn is_global(&self) -> bool {
        self.flags.is_global()
    }

    /// Check if this is a predefined exception.
    #[inline]
    pub fn is_predef_exn(&self) -> bool {
        self.flags.is_predef_exn()
    }

    /// Get the binding time (same as stamp).
    #[inline]
    pub fn binding_time(&self) -> i32 {
        self.stamp
    }

    /// Check if two identifiers have the same name (ignoring stamp).
    #[inline]
    pub fn equal(&self, other: &Self) -> bool {
        self.name == other.name
    }

    /// Check if two identifiers are the same.
    ///
    /// Two identifiers are the same if:
    /// - Both have non-zero stamps and the stamps are equal, OR
    /// - Both have stamp 0 and the names are equal
    #[inline]
    pub fn same(&self, other: &Self) -> bool {
        if self.stamp != 0 {
            self.stamp == other.stamp
        } else {
            other.stamp == 0 && self.name == other.name
        }
    }

    /// Create a new identifier with the same name but with global flag set.
    #[inline]
    pub fn with_global(&self) -> Self {
        Self {
            name: self.name.clone(),
            stamp: self.stamp,
            flags: self.flags.with_global(),
        }
    }

    /// Create a hidden version of this identifier (stamp = -1).
    #[inline]
    pub fn hide(&self) -> Self {
        Self {
            name: self.name.clone(),
            stamp: -1,
            flags: self.flags,
        }
    }

    /// Rename this identifier with a new stamp from the generator.
    ///
    /// This creates a new identifier with the same name and flags
    /// but a fresh stamp.
    #[inline]
    pub fn rename(&self, id_gen: &super::context::IdGenerator) -> Self {
        let stamp = id_gen.next_stamp();
        Self {
            name: self.name.clone(),
            stamp,
            flags: self.flags,
        }
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.stamp {
            0 => write!(f, "{}!", self.name),
            -1 => write!(f, "{}#", self.name),
            n => {
                write!(f, "{}/{}", self.name, n)?;
                if self.is_global() {
                    write!(f, "g")?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl PartialEq for Ident {
    /// Equality based on `same` semantics.
    fn eq(&self, other: &Self) -> bool {
        self.same(other)
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash based on first character and stamp for efficiency
        // This matches the OCaml implementation
        if let Some(c) = self.name.chars().next() {
            (c as u32 ^ self.stamp as u32).hash(state);
        } else {
            self.stamp.hash(state);
        }
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // First compare by stamp
        match self.stamp.cmp(&other.stamp) {
            std::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        // Then by name
        match self.name.cmp(&other.name) {
            std::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        // Finally by flags
        self.flags.0.cmp(&other.flags.0)
    }
}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// A table mapping identifiers to values.
///
/// This is implemented using an IndexMap for efficient iteration
/// while maintaining insertion order within each name.
///
/// Unlike the OCaml AVL tree implementation, this uses standard
/// Rust collections. The tradeoff is slightly different performance
/// characteristics but much simpler code.
pub type IdentTable<T> = indexmap::IndexMap<Ident, T, ahash::RandomState>;

/// Create a new identifier table.
pub fn new_ident_table<T>() -> IdentTable<T> {
    IdentTable::with_hasher(ahash::RandomState::new())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::IdGenerator;

    #[test]
    fn test_create_persistent() {
        let id = Ident::create_persistent("String");
        assert_eq!(id.name(), "String");
        assert_eq!(id.stamp(), 0);
        assert!(id.is_persistent());
        assert!(id.is_global());
    }

    #[test]
    fn test_create_with_generator() {
        let id_gen = IdGenerator::new();
        let id1 = id_gen.create("x");
        let id2 = id_gen.create("x");
        let id3 = id_gen.create("y");

        assert_eq!(id1.name(), "x");
        assert_eq!(id2.name(), "x");
        assert_eq!(id3.name(), "y");

        // All have different stamps
        assert_ne!(id1.stamp(), id2.stamp());
        assert_ne!(id1.stamp(), id3.stamp());
        assert_ne!(id2.stamp(), id3.stamp());

        // Same name but not same identifier
        assert!(id1.equal(&id2));
        assert!(!id1.same(&id2));
    }

    #[test]
    fn test_same_persistent() {
        let id1 = Ident::create_persistent("Mod");
        let id2 = Ident::create_persistent("Mod");
        let id3 = Ident::create_persistent("Other");

        // Persistent identifiers with same name are same
        assert!(id1.same(&id2));
        assert!(!id1.same(&id3));
    }

    #[test]
    fn test_hide() {
        let id_gen = IdGenerator::new();
        let id = id_gen.create("x");
        let hidden = id.hide();

        assert_eq!(hidden.stamp(), -1);
        assert!(hidden.is_hidden());
        assert_eq!(hidden.name(), id.name());
    }

    #[test]
    fn test_rename() {
        let id_gen = IdGenerator::new();
        let id1 = id_gen.create("x");
        let id2 = id1.rename(&id_gen);

        assert_eq!(id1.name(), id2.name());
        assert_ne!(id1.stamp(), id2.stamp());
        assert!(!id1.same(&id2));
    }

    #[test]
    fn test_unique_names() {
        let id_gen = IdGenerator::new();
        let id = id_gen.create("foo");

        let unique = id.unique_name();
        let toplevel = id.unique_toplevel_name();

        assert!(unique.starts_with("foo_"));
        assert!(toplevel.starts_with("foo/"));
    }

    #[test]
    fn test_debug_format() {
        let persistent = Ident::create_persistent("Mod");
        let id_gen = IdGenerator::new();
        let local = id_gen.create("x");
        let hidden = local.hide();
        let global = local.with_global();

        assert!(format!("{:?}", persistent).contains("!"));
        assert!(format!("{:?}", hidden).contains("#"));
        assert!(format!("{:?}", global).contains("g"));
    }

    #[test]
    fn test_concurrent_generation() {
        use std::sync::Arc;
        use std::thread;

        let id_gen = Arc::new(IdGenerator::new());
        let mut handles = vec![];

        for _ in 0..10 {
            let id_gen = id_gen.clone();
            handles.push(thread::spawn(move || {
                let mut ids = vec![];
                for i in 0..100 {
                    ids.push(id_gen.create(&format!("var{}", i)));
                }
                ids
            }));
        }

        let all_ids: Vec<Ident> = handles
            .into_iter()
            .flat_map(|h| h.join().unwrap())
            .collect();

        // All stamps should be unique
        let stamps: std::collections::HashSet<i32> = all_ids.iter().map(|id| id.stamp()).collect();
        assert_eq!(stamps.len(), all_ids.len());
    }
}
