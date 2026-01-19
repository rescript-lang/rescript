//! Longident - Long identifiers for module paths.
//!
//! This module defines `Longident`, which represents dotted identifiers
//! like `Foo.Bar.baz`. It mirrors `Longident.t` from OCaml.

use crate::location::Location;
use serde::{Deserialize, Serialize};

/// A long identifier, representing a dotted path like `Foo.Bar.baz`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Longident {
    /// A simple identifier: `foo`
    Lident(String),
    /// A dotted path: `Foo.bar` is `Ldot(Lident("Foo"), "bar")`
    Ldot(Box<Longident>, String),
    /// An application (for functors): `F(X)` is `Lapply(Lident("F"), Lident("X"))`
    Lapply(Box<Longident>, Box<Longident>),
}

impl Longident {
    /// Create a simple identifier.
    pub fn lident(name: impl Into<String>) -> Self {
        Longident::Lident(name.into())
    }

    /// Create a dotted identifier.
    pub fn ldot(prefix: Longident, name: impl Into<String>) -> Self {
        Longident::Ldot(Box::new(prefix), name.into())
    }

    /// Create a functor application.
    pub fn lapply(functor: Longident, arg: Longident) -> Self {
        Longident::Lapply(Box::new(functor), Box::new(arg))
    }

    /// Parse a string like "Foo.Bar.baz" into a Longident.
    pub fn parse(s: &str) -> Self {
        let parts: Vec<&str> = s.split('.').collect();
        let mut result = Longident::Lident(parts[0].to_string());
        for part in &parts[1..] {
            result = Longident::Ldot(Box::new(result), (*part).to_string());
        }
        result
    }

    /// Get the last component of the identifier.
    pub fn last(&self) -> &str {
        match self {
            Longident::Lident(s) => s,
            Longident::Ldot(_, s) => s,
            Longident::Lapply(_, arg) => arg.last(),
        }
    }

    /// Flatten to a list of strings.
    pub fn flatten(&self) -> Vec<&str> {
        match self {
            Longident::Lident(s) => vec![s.as_str()],
            Longident::Ldot(prefix, s) => {
                let mut v = prefix.flatten();
                v.push(s.as_str());
                v
            }
            Longident::Lapply(f, _) => f.flatten(),
        }
    }

    /// Check if this is a simple (non-dotted) identifier.
    pub fn is_simple(&self) -> bool {
        matches!(self, Longident::Lident(_))
    }
}

impl std::fmt::Display for Longident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Longident::Lident(s) => write!(f, "{}", s),
            Longident::Ldot(prefix, s) => write!(f, "{}.{}", prefix, s),
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

    #[test]
    fn test_lident() {
        let lid = Longident::lident("foo");
        assert_eq!(lid.last(), "foo");
        assert!(lid.is_simple());
        assert_eq!(lid.to_string(), "foo");
    }

    #[test]
    fn test_ldot() {
        let lid = Longident::ldot(Longident::lident("Foo"), "bar");
        assert_eq!(lid.last(), "bar");
        assert!(!lid.is_simple());
        assert_eq!(lid.to_string(), "Foo.bar");
    }

    #[test]
    fn test_parse() {
        let lid = Longident::parse("Foo.Bar.baz");
        assert_eq!(lid.last(), "baz");
        assert_eq!(lid.to_string(), "Foo.Bar.baz");
        assert_eq!(lid.flatten(), vec!["Foo", "Bar", "baz"]);
    }

    #[test]
    fn test_lapply() {
        let lid = Longident::lapply(Longident::lident("F"), Longident::lident("X"));
        assert_eq!(lid.to_string(), "F(X)");
    }

    #[test]
    fn test_display() {
        let lid = Longident::parse("Array.map");
        assert_eq!(format!("{}", lid), "Array.map");
    }

    #[test]
    fn test_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Longident>();
    }
}
