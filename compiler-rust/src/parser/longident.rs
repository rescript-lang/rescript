//! Longident - Long identifiers for module paths.
//!
//! This module defines `Longident`, which represents dotted identifiers
//! like `Foo.Bar.baz`. It mirrors `Longident.t` from OCaml.
//!
//! Strings are stored as `StrIdx` indices into a string arena, enabling:
//! - Efficient storage (4 bytes per string reference)
//! - Identity-based sharing during marshalling (same StrIdx = same object)

use crate::intern::StrIdx;
use crate::location::Location;
use serde::{Deserialize, Serialize};

/// A long identifier, representing a dotted path like `Foo.Bar.baz`.
///
/// Strings are stored as `StrIdx` indices into the arena's string interner.
/// This enables efficient sharing during marshalling: same StrIdx = same object.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Longident {
    /// A simple identifier: `foo`
    Lident(StrIdx),
    /// A dotted path: `Foo.bar` is `Ldot(Lident("Foo"), "bar")`
    Ldot(Box<Longident>, StrIdx),
    /// An application (for functors): `F(X)` is `Lapply(Lident("F"), Lident("X"))`
    Lapply(Box<Longident>, Box<Longident>),
}

impl Longident {
    /// Create a simple identifier from a StrIdx.
    pub fn lident(name: StrIdx) -> Self {
        Longident::Lident(name)
    }

    /// Create a dotted identifier from a StrIdx.
    pub fn ldot(prefix: Longident, name: StrIdx) -> Self {
        Longident::Ldot(Box::new(prefix), name)
    }

    /// Create a functor application.
    pub fn lapply(functor: Longident, arg: Longident) -> Self {
        Longident::Lapply(Box::new(functor), Box::new(arg))
    }

    /// Get the last component's StrIdx.
    pub fn last_idx(&self) -> StrIdx {
        match self {
            Longident::Lident(s) => *s,
            Longident::Ldot(_, s) => *s,
            Longident::Lapply(_, arg) => arg.last_idx(),
        }
    }

    /// Flatten to a list of StrIdx.
    pub fn flatten_idx(&self) -> Vec<StrIdx> {
        match self {
            Longident::Lident(s) => vec![*s],
            Longident::Ldot(prefix, s) => {
                let mut v = prefix.flatten_idx();
                v.push(*s);
                v
            }
            Longident::Lapply(f, _) => f.flatten_idx(),
        }
    }

    /// Check if this is a simple (non-dotted) identifier.
    pub fn is_simple(&self) -> bool {
        matches!(self, Longident::Lident(_))
    }
}

impl std::fmt::Display for Longident {
    /// Display shows StrIdx values. For actual string content, use arena lookup.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Longident::Lident(s) => write!(f, "Lident(#{})", s.raw()),
            Longident::Ldot(prefix, s) => write!(f, "{}.#{}", prefix, s.raw()),
            Longident::Lapply(func, arg) => write!(f, "{}({})", func, arg),
        }
    }
}

/// A Longident with location information.
pub type LongidentLoc = crate::location::Located<Longident>;

/// Helper to create a located longident.
pub fn mkloc(lid: Longident, loc: Location) -> LongidentLoc {
    crate::location::Located { txt: lid, loc }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intern::Interner;

    #[test]
    fn test_lident() {
        let mut interner = Interner::new();
        let foo_idx = interner.intern("foo");
        let lid = Longident::lident(foo_idx);
        assert_eq!(lid.last_idx(), foo_idx);
        assert!(lid.is_simple());
        assert_eq!(interner.get(lid.last_idx()), "foo");
    }

    #[test]
    fn test_ldot() {
        let mut interner = Interner::new();
        let foo_idx = interner.intern("Foo");
        let bar_idx = interner.intern("bar");
        let lid = Longident::ldot(Longident::lident(foo_idx), bar_idx);
        assert_eq!(lid.last_idx(), bar_idx);
        assert!(!lid.is_simple());
    }

    #[test]
    fn test_lapply() {
        let mut interner = Interner::new();
        let f_idx = interner.intern("F");
        let x_idx = interner.intern("X");
        let lid = Longident::lapply(Longident::lident(f_idx), Longident::lident(x_idx));
        assert!(!lid.is_simple());
    }

    #[test]
    fn test_flatten() {
        let mut interner = Interner::new();
        let foo_idx = interner.intern("Foo");
        let bar_idx = interner.intern("Bar");
        let baz_idx = interner.intern("baz");
        let lid = Longident::ldot(
            Longident::ldot(Longident::lident(foo_idx), bar_idx),
            baz_idx,
        );
        let indices = lid.flatten_idx();
        assert_eq!(indices.len(), 3);
        assert_eq!(interner.get(indices[0]), "Foo");
        assert_eq!(interner.get(indices[1]), "Bar");
        assert_eq!(interner.get(indices[2]), "baz");
    }

    #[test]
    fn test_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Longident>();
    }
}
