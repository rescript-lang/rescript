//! Auxiliary AST types used by parsetree and typedtree.
//!
//! These types are shared between the surface syntax (Parsetree) and
//! the typed syntax (Typedtree).

use crate::location::Location;
use serde::{Deserialize, Serialize};

/// Constant values in the AST.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Constant {
    /// Integer constant
    Int(i64),
    /// Character constant (Unicode codepoint)
    Char(u32),
    /// String constant with optional delimiter
    String(String, Option<String>),
    /// Float constant (stored as string for precision)
    Float(String),
    /// 32-bit integer
    Int32(i32),
    /// 64-bit integer
    Int64(i64),
    /// Big integer (sign, digits as string)
    BigInt(bool, String),
}

/// Recursive flag for let bindings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RecFlag {
    /// Non-recursive binding
    Nonrecursive,
    /// Recursive binding
    Recursive,
}

impl Default for RecFlag {
    fn default() -> Self {
        RecFlag::Nonrecursive
    }
}

/// Direction flag for for loops.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DirectionFlag {
    /// Counting up
    Upto,
    /// Counting down
    Downto,
}

/// Private/public flag for type definitions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum PrivateFlag {
    /// Private type (cannot be constructed outside the module)
    Private,
    /// Public type
    Public,
}

impl Default for PrivateFlag {
    fn default() -> Self {
        PrivateFlag::Public
    }
}

/// Mutable/immutable flag for record fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MutableFlag {
    /// Immutable field
    Immutable,
    /// Mutable field
    Mutable,
}

impl Default for MutableFlag {
    fn default() -> Self {
        MutableFlag::Immutable
    }
}

/// Virtual/concrete flag for class methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum VirtualFlag {
    /// Virtual method
    Virtual,
    /// Concrete method
    Concrete,
}

/// Override/fresh flag for class methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OverrideFlag {
    /// Overriding method
    Override,
    /// Fresh method
    Fresh,
}

/// Closed/open flag for polymorphic variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ClosedFlag {
    /// Closed variant
    Closed,
    /// Open variant
    Open,
}

/// A label (field name, method name, etc.).
pub type Label = String;

/// Arity of a function (None means unknown).
pub type Arity = Option<i32>;

/// Variance annotation for type parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum VarianceAnnotation {
    /// Covariant (+)
    Covariant,
    /// Contravariant (-)
    Contravariant,
    /// Invariant
    Invariant,
}

/// Argument label for function parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArgLabel {
    /// No label: `x => ...`
    Nolabel,
    /// Labeled argument: `~label => ...`
    Labelled(Located<String>),
    /// Optional argument: `~(label=e) => ...`
    Optional(Located<String>),
}

impl ArgLabel {
    /// Create a labeled argument.
    pub fn labelled(name: impl Into<String>, loc: Location) -> Self {
        ArgLabel::Labelled(Located {
            txt: name.into(),
            loc,
        })
    }

    /// Create an optional argument.
    pub fn optional(name: impl Into<String>, loc: Location) -> Self {
        ArgLabel::Optional(Located {
            txt: name.into(),
            loc,
        })
    }

    /// Create a labeled argument with no location.
    pub fn labelled_noloc(name: impl Into<String>) -> Self {
        ArgLabel::Labelled(Located {
            txt: name.into(),
            loc: Location::none(),
        })
    }

    /// Create an optional argument with no location.
    pub fn optional_noloc(name: impl Into<String>) -> Self {
        ArgLabel::Optional(Located {
            txt: name.into(),
            loc: Location::none(),
        })
    }

    /// Get the label name, if any.
    pub fn label_name(&self) -> Option<&str> {
        match self {
            ArgLabel::Nolabel => None,
            ArgLabel::Labelled(l) => Some(&l.txt),
            ArgLabel::Optional(l) => Some(&l.txt),
        }
    }

    /// Check if this is a labeled (non-optional) argument.
    pub fn is_labelled(&self) -> bool {
        matches!(self, ArgLabel::Labelled(_))
    }

    /// Check if this is an optional argument.
    pub fn is_optional(&self) -> bool {
        matches!(self, ArgLabel::Optional(_))
    }

    /// Get the location of the label, if any.
    pub fn loc(&self) -> Location {
        match self {
            ArgLabel::Nolabel => Location::none(),
            ArgLabel::Labelled(l) => l.loc.clone(),
            ArgLabel::Optional(l) => l.loc.clone(),
        }
    }

    /// Check if two arg labels have the same label name.
    pub fn same_label(&self, other: &ArgLabel) -> bool {
        match (self, other) {
            (ArgLabel::Nolabel, ArgLabel::Nolabel) => true,
            (ArgLabel::Labelled(l1), ArgLabel::Labelled(l2)) => l1.txt == l2.txt,
            (ArgLabel::Optional(l1), ArgLabel::Optional(l2)) => l1.txt == l2.txt,
            _ => false,
        }
    }
}

impl Default for ArgLabel {
    fn default() -> Self {
        ArgLabel::Nolabel
    }
}

/// A value with source location.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Located<T> {
    /// The value.
    pub txt: T,
    /// The source location.
    pub loc: Location,
}

impl<T> Located<T> {
    /// Create a new located value.
    pub fn new(txt: T, loc: Location) -> Self {
        Located { txt, loc }
    }

    /// Create a located value with no location.
    pub fn noloc(txt: T) -> Self {
        Located {
            txt,
            loc: Location::none(),
        }
    }

    /// Map a function over the contained value.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Located<U> {
        Located {
            txt: f(self.txt),
            loc: self.loc,
        }
    }

    /// Get a reference to the contained value.
    pub fn as_ref(&self) -> Located<&T> {
        Located {
            txt: &self.txt,
            loc: self.loc.clone(),
        }
    }
}

/// Argument label without location (for internal use).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArgLabelNoloc {
    /// No label
    Nolabel,
    /// Labeled
    Labelled(String),
    /// Optional
    Optional(String),
}

impl ArgLabelNoloc {
    /// Convert to ArgLabel with a location.
    pub fn to_arg_label(&self, loc: Location) -> ArgLabel {
        match self {
            ArgLabelNoloc::Nolabel => ArgLabel::Nolabel,
            ArgLabelNoloc::Labelled(s) => ArgLabel::labelled(s.clone(), loc),
            ArgLabelNoloc::Optional(s) => ArgLabel::optional(s.clone(), loc),
        }
    }

    /// Convert to ArgLabel with no location.
    pub fn to_arg_label_noloc(&self) -> ArgLabel {
        self.to_arg_label(Location::none())
    }
}

impl From<&ArgLabel> for ArgLabelNoloc {
    fn from(label: &ArgLabel) -> Self {
        match label {
            ArgLabel::Nolabel => ArgLabelNoloc::Nolabel,
            ArgLabel::Labelled(l) => ArgLabelNoloc::Labelled(l.txt.clone()),
            ArgLabel::Optional(l) => ArgLabelNoloc::Optional(l.txt.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant() {
        let c = Constant::Int(42);
        assert_eq!(c, Constant::Int(42));

        let s = Constant::String("hello".to_string(), None);
        assert_eq!(s, Constant::String("hello".to_string(), None));
    }

    #[test]
    fn test_arg_label() {
        let nolabel = ArgLabel::Nolabel;
        assert!(nolabel.label_name().is_none());

        let labelled = ArgLabel::labelled_noloc("foo");
        assert_eq!(labelled.label_name(), Some("foo"));
        assert!(labelled.is_labelled());

        let optional = ArgLabel::optional_noloc("bar");
        assert_eq!(optional.label_name(), Some("bar"));
        assert!(optional.is_optional());
    }

    #[test]
    fn test_same_label() {
        let l1 = ArgLabel::labelled_noloc("x");
        let l2 = ArgLabel::labelled_noloc("x");
        let l3 = ArgLabel::labelled_noloc("y");
        let o1 = ArgLabel::optional_noloc("x");

        assert!(l1.same_label(&l2));
        assert!(!l1.same_label(&l3));
        assert!(!l1.same_label(&o1)); // Labelled vs Optional
    }

    #[test]
    fn test_located() {
        let loc = Located::new(42, Location::none());
        assert_eq!(loc.txt, 42);

        let mapped = loc.map(|x| x * 2);
        assert_eq!(mapped.txt, 84);
    }

    #[test]
    fn test_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Constant>();
        assert_send_sync::<ArgLabel>();
        assert_send_sync::<Located<String>>();
    }
}
