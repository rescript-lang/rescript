//! Lambda constants - Compile-time constant values.
//!
//! This module defines constant values that can appear in Lambda IR.
//! These include primitive values like integers and strings, as well
//! as compound structures like blocks (for variants and records).

use super::tag_info::TagInfo;

/// String delimiter information for template literals
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StringDelim {
    /// No quotes (standard string)
    NoQuotes,
    /// Template literal backtick
    Backtick,
}

/// Pointer info for tracking constructor origins
#[derive(Debug, Clone, PartialEq)]
pub enum PointerInfo {
    /// No specific info
    None,
    /// Constructor with tag information
    Constructor(ConstructorTag),
    /// Assert false (for exhaustiveness)
    AssertFalse,
    /// Named variant
    Some(String),
}

/// Constructor tag information
#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorTag {
    /// Constructor name
    pub name: String,
    /// Number of constant constructors
    pub const_: i32,
    /// Number of non-constant constructors
    pub non_const: i32,
}

/// Lambda constant values
///
/// These represent compile-time constant values that can be embedded
/// directly in the generated JavaScript.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// JavaScript null
    JsNull,

    /// JavaScript undefined (with unit flag)
    JsUndefined {
        /// Is this the unit value `()`?
        is_unit: bool,
    },

    /// JavaScript true
    JsTrue,

    /// JavaScript false
    JsFalse,

    /// Integer constant
    Int {
        /// The integer value (32-bit)
        i: i32,
        /// Optional comment/info about the integer
        comment: Option<PointerInfo>,
    },

    /// Character constant (stored as Unicode code point)
    Char(i32),

    /// String constant
    String {
        /// The string value
        s: String,
        /// Optional delimiter (for template literals)
        delim: Option<StringDelim>,
    },

    /// Float constant (stored as string to preserve precision)
    Float(String),

    /// BigInt constant
    BigInt {
        /// Is this negative?
        negative: bool,
        /// The BigInt value as a string
        value: String,
    },

    /// Pointer constant (for polyvar tags etc.)
    Pointer(String),

    /// Block constant (for variants, records, tuples)
    Block {
        /// Block tag
        tag: i32,
        /// Tag info for debugging/codegen
        tag_info: TagInfo,
        /// Block elements
        elements: Vec<Constant>,
    },

    /// Some value (for option type)
    Some(Box<Constant>),

    /// Module alias marker
    ModuleAlias,
}

impl Constant {
    /// Create an integer constant
    pub fn int(i: i32) -> Self {
        Constant::Int { i, comment: None }
    }

    /// Create an integer constant with a comment
    pub fn int_with_comment(i: i32, comment: PointerInfo) -> Self {
        Constant::Int {
            i,
            comment: Some(comment),
        }
    }

    /// Create a string constant
    pub fn string(s: impl Into<String>) -> Self {
        Constant::String {
            s: s.into(),
            delim: None,
        }
    }

    /// Create a string constant with delimiter
    pub fn string_with_delim(s: impl Into<String>, delim: StringDelim) -> Self {
        Constant::String {
            s: s.into(),
            delim: Some(delim),
        }
    }

    /// Create a float constant
    pub fn float(s: impl Into<String>) -> Self {
        Constant::Float(s.into())
    }

    /// Create a char constant
    pub fn char(c: char) -> Self {
        Constant::Char(c as i32)
    }

    /// Create a bigint constant
    pub fn bigint(negative: bool, value: impl Into<String>) -> Self {
        Constant::BigInt {
            negative,
            value: value.into(),
        }
    }

    /// Check if this constant is the "none" option value
    pub fn is_none(&self) -> bool {
        matches!(self, Constant::JsUndefined { is_unit: false })
    }

    /// Create the "none" option value
    pub fn none() -> Self {
        Constant::JsUndefined { is_unit: false }
    }

    /// Check if this constant requires allocation at runtime
    pub fn is_allocating(&self) -> bool {
        match self {
            Constant::Block { .. } | Constant::Some(_) => true,
            Constant::JsNull
            | Constant::JsUndefined { .. }
            | Constant::JsTrue
            | Constant::JsFalse
            | Constant::Int { .. }
            | Constant::Char(_)
            | Constant::String { .. }
            | Constant::Float(_)
            | Constant::BigInt { .. }
            | Constant::Pointer(_)
            | Constant::ModuleAlias => false,
        }
    }

    /// Approximate equality check (for optimization)
    pub fn eq_approx(&self, other: &Constant) -> bool {
        match (self, other) {
            (Constant::JsNull, Constant::JsNull) => true,
            (Constant::JsUndefined { is_unit: a }, Constant::JsUndefined { is_unit: b }) => a == b,
            (Constant::JsTrue, Constant::JsTrue) => true,
            (Constant::JsFalse, Constant::JsFalse) => true,
            (Constant::Int { i: a, .. }, Constant::Int { i: b, .. }) => a == b,
            (Constant::Char(a), Constant::Char(b)) => a == b,
            (Constant::String { s: a, delim: da }, Constant::String { s: b, delim: db }) => {
                a == b && da == db
            }
            (Constant::Float(a), Constant::Float(b)) => a == b,
            (
                Constant::BigInt {
                    negative: na,
                    value: va,
                },
                Constant::BigInt {
                    negative: nb,
                    value: vb,
                },
            ) => na == nb && va == vb,
            (Constant::Pointer(a), Constant::Pointer(b)) => a == b,
            (
                Constant::Block {
                    tag: ta,
                    elements: ea,
                    ..
                },
                Constant::Block {
                    tag: tb,
                    elements: eb,
                    ..
                },
            ) => {
                ta == tb
                    && ea.len() == eb.len()
                    && ea.iter().zip(eb.iter()).all(|(a, b)| a.eq_approx(b))
            }
            (Constant::Some(a), Constant::Some(b)) => a.eq_approx(b),
            (Constant::ModuleAlias, Constant::ModuleAlias) => true,
            _ => false,
        }
    }

    /// Evaluate this constant as a boolean (for constant folding)
    pub fn eval_as_bool(&self) -> bool {
        match self {
            Constant::Int { i, .. } => *i != 0,
            Constant::Char(c) => *c != 0,
            Constant::JsFalse
            | Constant::JsNull
            | Constant::ModuleAlias
            | Constant::JsUndefined { .. } => false,
            Constant::JsTrue
            | Constant::String { .. }
            | Constant::Pointer(_)
            | Constant::Float(_)
            | Constant::BigInt { .. }
            | Constant::Block { .. } => true,
            Constant::Some(inner) => inner.eval_as_bool(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_constant() {
        let c = Constant::int(42);
        match c {
            Constant::Int {
                i: 42,
                comment: None,
            } => {}
            _ => panic!("Expected Int(42)"),
        }
    }

    #[test]
    fn test_string_constant() {
        let c = Constant::string("hello");
        match c {
            Constant::String { s, delim: None } => assert_eq!(s, "hello"),
            _ => panic!("Expected String"),
        }
    }

    #[test]
    fn test_eq_approx() {
        assert!(Constant::int(42).eq_approx(&Constant::int(42)));
        assert!(!Constant::int(42).eq_approx(&Constant::int(43)));

        assert!(Constant::string("hello").eq_approx(&Constant::string("hello")));
        assert!(!Constant::string("hello").eq_approx(&Constant::string("world")));

        assert!(Constant::JsTrue.eq_approx(&Constant::JsTrue));
        assert!(!Constant::JsTrue.eq_approx(&Constant::JsFalse));
    }

    #[test]
    fn test_eval_as_bool() {
        assert!(Constant::JsTrue.eval_as_bool());
        assert!(!Constant::JsFalse.eval_as_bool());
        assert!(Constant::int(1).eval_as_bool());
        assert!(!Constant::int(0).eval_as_bool());
        assert!(Constant::string("hello").eval_as_bool());
        assert!(!Constant::JsNull.eval_as_bool());
    }

    #[test]
    fn test_is_allocating() {
        assert!(!Constant::int(42).is_allocating());
        assert!(!Constant::string("hello").is_allocating());
        assert!(Constant::Some(Box::new(Constant::int(42))).is_allocating());
        assert!(
            Constant::Block {
                tag: 0,
                tag_info: TagInfo::Tuple,
                elements: vec![Constant::int(1), Constant::int(2)],
            }
            .is_allocating()
        );
    }
}
