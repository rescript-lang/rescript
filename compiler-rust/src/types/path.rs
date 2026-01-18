//! Module paths for the type system.
//!
//! This module provides the `Path` type, which represents qualified names
//! in the type system. Unlike `Longident` which is for parsing, `Path` uses
//! `Ident` with stamps for unique identification.
//!
//! # Example
//!
//! ```rust
//! use rescript_compiler::ident::Ident;
//! use rescript_compiler::types::Path;
//!
//! // A simple identifier path
//! let id = Ident::create_persistent("String");
//! let path = Path::pident(id);
//!
//! // A dotted path: Array.t
//! let array_id = Ident::create_persistent("Array");
//! let array_t = Path::pdot(Path::pident(array_id), "t");
//!
//! assert_eq!(array_t.name(), "Array.t");
//! ```

use crate::ident::Ident;
use serde::{Deserialize, Serialize};
use std::fmt;

/// A module path in the type system.
///
/// Paths are used to refer to types, modules, and values throughout
/// the type checker. Unlike `Longident`, they use `Ident` with stamps
/// for unique identification.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Path {
    /// A simple identifier: `String`, `int`
    Pident(Ident),
    /// A dotted access: `Array.t` is `Pdot(Pident(Array), "t", -1)`
    /// The integer is a position hint (usually -1 for no hint).
    Pdot(Box<Path>, String, i32),
    /// Functor application: `F(X)` is `Papply(F, X)`
    Papply(Box<Path>, Box<Path>),
}

/// Position constant for no position hint.
pub const NOPOS: i32 = -1;

impl Path {
    /// Create a path from a simple identifier.
    #[inline]
    pub fn pident(id: Ident) -> Self {
        Path::Pident(id)
    }

    /// Create a path from a longident.
    pub fn from_longident(lid: &crate::parser::longident::Longident) -> Self {
        use crate::parser::longident::Longident;
        match lid {
            Longident::Lident(name) => Path::pident(Ident::create_local(name)),
            Longident::Ldot(prefix, name) => {
                let prefix_path = Path::from_longident(prefix);
                Path::pdot(prefix_path, name)
            }
            Longident::Lapply(_, _) => {
                // Placeholder - proper implementation would handle functor application
                Path::pident(Ident::create_local("apply"))
            }
        }
    }

    /// Create a dotted path.
    #[inline]
    pub fn pdot(prefix: Path, name: impl Into<String>) -> Self {
        Path::Pdot(Box::new(prefix), name.into(), NOPOS)
    }

    /// Create a dotted path with position.
    #[inline]
    pub fn pdot_pos(prefix: Path, name: impl Into<String>, pos: i32) -> Self {
        Path::Pdot(Box::new(prefix), name.into(), pos)
    }

    /// Create a functor application path.
    #[inline]
    pub fn papply(functor: Path, arg: Path) -> Self {
        Path::Papply(Box::new(functor), Box::new(arg))
    }

    /// Check if two paths refer to the same entity.
    ///
    /// This compares paths structurally, using `Ident::same` for identifiers.
    pub fn same(&self, other: &Path) -> bool {
        match (self, other) {
            (Path::Pident(id1), Path::Pident(id2)) => id1.same(id2),
            (Path::Pdot(p1, s1, _), Path::Pdot(p2, s2, _)) => s1 == s2 && p1.same(p2),
            (Path::Papply(f1, a1), Path::Papply(f2, a2)) => f1.same(f2) && a1.same(a2),
            _ => false,
        }
    }

    /// Check if an identifier is free (appears) in this path.
    pub fn is_free(&self, id: &Ident) -> bool {
        match self {
            Path::Pident(id2) => id.same(id2),
            Path::Pdot(p, _, _) => p.is_free(id),
            Path::Papply(p1, p2) => p1.is_free(id) || p2.is_free(id),
        }
    }

    /// Get the binding time of this path.
    ///
    /// This is the minimum stamp among all identifiers in the path.
    pub fn binding_time(&self) -> i32 {
        match self {
            Path::Pident(id) => id.binding_time(),
            Path::Pdot(p, _, _) => p.binding_time(),
            Path::Papply(p1, p2) => p1.binding_time().max(p2.binding_time()),
        }
    }

    /// Get the string name of this path.
    pub fn name(&self) -> String {
        self.name_with_paren(|_| false)
    }

    /// Get the string name with parenthesization for operators.
    pub fn name_with_paren(&self, paren: impl Fn(&str) -> bool + Copy) -> String {
        match self {
            Path::Pident(id) => id.name().to_string(),
            Path::Pdot(p, s, _) => {
                let prefix = p.name_with_paren(paren);
                if paren(s) {
                    format!("{}.( {} )", prefix, s)
                } else {
                    format!("{}.{}", prefix, s)
                }
            }
            Path::Papply(p1, p2) => {
                format!(
                    "{}({})",
                    p1.name_with_paren(paren),
                    p2.name_with_paren(paren)
                )
            }
        }
    }

    /// Get the head identifier of this path.
    ///
    /// # Panics
    ///
    /// Panics if the path is a `Papply`.
    pub fn head(&self) -> &Ident {
        match self {
            Path::Pident(id) => id,
            Path::Pdot(p, _, _) => p.head(),
            Path::Papply(_, _) => panic!("Path::head called on Papply"),
        }
    }

    /// Get the last component of this path.
    pub fn last(&self) -> &str {
        match self {
            Path::Pident(id) => id.name(),
            Path::Pdot(_, s, _) => s,
            Path::Papply(_, p) => p.last(),
        }
    }

    /// Flatten this path to an identifier and list of component names.
    ///
    /// Returns `Err(())` if the path contains a `Papply`.
    pub fn flatten(&self) -> Result<(&Ident, Vec<&str>), ()> {
        fn go<'a>(path: &'a Path, acc: &mut Vec<&'a str>) -> Result<&'a Ident, ()> {
            match path {
                Path::Pident(id) => Ok(id),
                Path::Pdot(p, s, _) => {
                    let id = go(p, acc)?;
                    acc.push(s);
                    Ok(id)
                }
                Path::Papply(_, _) => Err(()),
            }
        }

        let mut components = Vec::new();
        let id = go(self, &mut components)?;
        Ok((id, components))
    }

    /// Get all head identifiers in the path (including through `Papply`).
    pub fn heads(&self) -> Vec<&Ident> {
        let mut result = Vec::new();
        self.collect_heads(&mut result);
        result
    }

    fn collect_heads<'a>(&'a self, acc: &mut Vec<&'a Ident>) {
        match self {
            Path::Pident(id) => acc.push(id),
            Path::Pdot(p, _, _) => p.collect_heads(acc),
            Path::Papply(p1, p2) => {
                p1.collect_heads(acc);
                p2.collect_heads(acc);
            }
        }
    }

    /// Check if the last component starts with an uppercase letter.
    pub fn is_uident(&self) -> bool {
        let last = self.last();
        !last.is_empty()
            && last
                .chars()
                .next()
                .map(|c| c.is_ascii_uppercase())
                .unwrap_or(false)
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Path::Pident(id1), Path::Pident(id2)) => id1.cmp(id2),
            (Path::Pdot(p1, s1, _), Path::Pdot(p2, s2, _)) => match p1.cmp(p2) {
                std::cmp::Ordering::Equal => s1.cmp(s2),
                other => other,
            },
            (Path::Papply(f1, a1), Path::Papply(f2, a2)) => match f1.cmp(f2) {
                std::cmp::Ordering::Equal => a1.cmp(a2),
                other => other,
            },
            // Ordering: Pident < Pdot < Papply
            (Path::Pident(_), _) => std::cmp::Ordering::Less,
            (_, Path::Pident(_)) => std::cmp::Ordering::Greater,
            (Path::Pdot(_, _, _), Path::Papply(_, _)) => std::cmp::Ordering::Less,
            (Path::Papply(_, _), Path::Pdot(_, _, _)) => std::cmp::Ordering::Greater,
        }
    }
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// Categorization of paths for constructor handling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Typath {
    /// Regular type path
    Regular(Path),
    /// Extension type: `A.B` where B is uppercase (extension constructor)
    Ext(Path, String),
    /// Local extension: single uppercase identifier
    LocalExt(Ident),
    /// Constructor path: `A.B` where A is lowercase, B is uppercase
    Cstr(Path, String),
}

impl Path {
    /// Categorize this path for constructor handling.
    pub fn constructor_typath(&self) -> Typath {
        match self {
            Path::Pident(id) if is_uident_str(id.name()) => Typath::LocalExt(id.clone()),
            Path::Pdot(ty_path, s, _) if is_uident_str(s) => {
                if is_uident_str(ty_path.last()) {
                    Typath::Ext((**ty_path).clone(), s.clone())
                } else {
                    Typath::Cstr((**ty_path).clone(), s.clone())
                }
            }
            p => Typath::Regular(p.clone()),
        }
    }

    /// Check if this path refers to a constructor type.
    pub fn is_constructor_typath(&self) -> bool {
        !matches!(self.constructor_typath(), Typath::Regular(_))
    }
}

/// Check if a string starts with an uppercase letter.
fn is_uident_str(s: &str) -> bool {
    !s.is_empty()
        && s.chars()
            .next()
            .map(|c| c.is_ascii_uppercase())
            .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pident() {
        let id = Ident::create_persistent("String");
        let path = Path::pident(id.clone());

        assert_eq!(path.name(), "String");
        assert_eq!(path.last(), "String");
        assert_eq!(path.head(), &id);
    }

    #[test]
    fn test_pdot() {
        let array_id = Ident::create_persistent("Array");
        let path = Path::pdot(Path::pident(array_id.clone()), "t");

        assert_eq!(path.name(), "Array.t");
        assert_eq!(path.last(), "t");
        assert_eq!(path.head(), &array_id);
    }

    #[test]
    fn test_papply() {
        let f_id = Ident::create_persistent("F");
        let x_id = Ident::create_persistent("X");
        let path = Path::papply(Path::pident(f_id), Path::pident(x_id));

        assert_eq!(path.name(), "F(X)");
    }

    #[test]
    fn test_same() {
        let id1 = Ident::create_persistent("String");
        let id2 = Ident::create_persistent("String");
        let id3 = Ident::create_persistent("Int");

        let p1 = Path::pident(id1);
        let p2 = Path::pident(id2);
        let p3 = Path::pident(id3);

        assert!(p1.same(&p2));
        assert!(!p1.same(&p3));
    }

    #[test]
    fn test_flatten() {
        let array_id = Ident::create_persistent("Array");
        let path = Path::pdot(Path::pident(array_id.clone()), "t");

        let (id, components) = path.flatten().unwrap();
        assert!(id.same(&array_id));
        assert_eq!(components, vec!["t"]);
    }

    #[test]
    fn test_is_uident() {
        let upper = Path::pident(Ident::create_persistent("String"));
        let lower = Path::pident(Ident::create_persistent("string"));

        assert!(upper.is_uident());
        assert!(!lower.is_uident());
    }

    #[test]
    fn test_constructor_typath() {
        let id = Ident::create_persistent("Foo");
        let path = Path::pident(id);

        match path.constructor_typath() {
            Typath::LocalExt(_) => {}
            _ => panic!("Expected LocalExt"),
        }

        let module_id = Ident::create_persistent("Array");
        let t_path = Path::pdot(Path::pident(module_id), "t");

        match t_path.constructor_typath() {
            Typath::Regular(_) => {}
            _ => panic!("Expected Regular"),
        }
    }

    #[test]
    fn test_display() {
        let array_id = Ident::create_persistent("Belt");
        let map_id = Path::pdot(Path::pident(array_id), "Map");
        let int_id = Path::pdot(map_id, "Int");

        assert_eq!(format!("{}", int_id), "Belt.Map.Int");
    }

    #[test]
    fn test_is_free() {
        let id = Ident::create_persistent("X");
        let other = Ident::create_persistent("Y");
        let path = Path::pdot(Path::pident(id.clone()), "t");

        assert!(path.is_free(&id));
        assert!(!path.is_free(&other));
    }

    #[test]
    fn test_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Path>();
    }
}
