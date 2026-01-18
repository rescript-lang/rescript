//! JavaScript operators and types.
//!
//! This module defines JavaScript-specific operators and types used
//! in the JS IR, including binary operators, property access, and
//! number representations.

use crate::lambda::compat::LetKind;

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    /// Assignment (=)
    Eq,
    /// Logical or (||)
    Or,
    /// Logical and (&&)
    And,
    /// Strict equality (===)
    EqEqEq,
    /// Strict inequality (!==)
    NotEqEq,
    /// Less than (<)
    Lt,
    /// Less than or equal (<=)
    Le,
    /// Greater than (>)
    Gt,
    /// Greater than or equal (>=)
    Ge,
    /// Bitwise not (~)
    Bnot,
    /// Bitwise or (|)
    Bor,
    /// Bitwise xor (^)
    Bxor,
    /// Bitwise and (&)
    Band,
    /// Left shift (<<)
    Lsl,
    /// Logical right shift (>>>)
    Lsr,
    /// Arithmetic right shift (>>)
    Asr,
    /// Addition (+)
    Plus,
    /// Subtraction (-)
    Minus,
    /// Multiplication (*)
    Mul,
    /// Division (/)
    Div,
    /// Modulo (%)
    Mod,
    /// Exponentiation (**)
    Pow,
    /// instanceof operator
    InstanceOf,
}

impl BinOp {
    /// Get the JavaScript string representation
    pub fn to_js_string(self) -> &'static str {
        match self {
            BinOp::Eq => "=",
            BinOp::Or => "||",
            BinOp::And => "&&",
            BinOp::EqEqEq => "===",
            BinOp::NotEqEq => "!==",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::Bnot => "~",
            BinOp::Bor => "|",
            BinOp::Bxor => "^",
            BinOp::Band => "&",
            BinOp::Lsl => "<<",
            BinOp::Lsr => ">>>",
            BinOp::Asr => ">>",
            BinOp::Plus => "+",
            BinOp::Minus => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Pow => "**",
            BinOp::InstanceOf => "instanceof",
        }
    }

    /// Get the operator precedence (higher = binds tighter)
    pub fn precedence(self) -> u8 {
        match self {
            BinOp::Eq => 2,
            BinOp::Or => 3,
            BinOp::And => 4,
            BinOp::Bor => 5,
            BinOp::Bxor => 6,
            BinOp::Band => 7,
            BinOp::EqEqEq | BinOp::NotEqEq => 8,
            BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge | BinOp::InstanceOf => 9,
            BinOp::Lsl | BinOp::Lsr | BinOp::Asr => 10,
            BinOp::Plus | BinOp::Minus => 11,
            BinOp::Mul | BinOp::Div | BinOp::Mod => 12,
            BinOp::Pow => 13,
            BinOp::Bnot => 14, // Unary, but included for completeness
        }
    }

    /// Is this a comparison operator?
    pub fn is_comparison(self) -> bool {
        matches!(
            self,
            BinOp::EqEqEq
                | BinOp::NotEqEq
                | BinOp::Lt
                | BinOp::Le
                | BinOp::Gt
                | BinOp::Ge
                | BinOp::InstanceOf
        )
    }

    /// Is this a logical operator?
    pub fn is_logical(self) -> bool {
        matches!(self, BinOp::Or | BinOp::And)
    }

    /// Is this an arithmetic operator?
    pub fn is_arithmetic(self) -> bool {
        matches!(
            self,
            BinOp::Plus | BinOp::Minus | BinOp::Mul | BinOp::Div | BinOp::Mod | BinOp::Pow
        )
    }

    /// Is this a bitwise operator?
    pub fn is_bitwise(self) -> bool {
        matches!(
            self,
            BinOp::Bor | BinOp::Bxor | BinOp::Band | BinOp::Lsl | BinOp::Lsr | BinOp::Asr
        )
    }
}

/// Integer-specific operators (for 32-bit operations)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntOp {
    /// Bitwise or (|)
    Bor,
    /// Bitwise xor (^)
    Bxor,
    /// Bitwise and (&)
    Band,
    /// Left shift (<<)
    Lsl,
    /// Logical right shift (>>>)
    Lsr,
    /// Arithmetic right shift (>>)
    Asr,
    /// Addition (+)
    Plus,
    /// Subtraction (-)
    Minus,
    /// Multiplication (*)
    Mul,
    /// Division (/)
    Div,
    /// Modulo (%)
    Mod,
    /// Exponentiation (**)
    Pow,
}

/// Log level for console output
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Log,
    Info,
    Warn,
    Error,
}

/// Module kind
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    /// OCaml/ReScript module
    Ml,
    /// Runtime module
    Runtime,
    /// External JavaScript module
    External {
        name: String,
        default: bool,
        import_attributes: Option<ImportAttributes>,
    },
}

/// Import attributes for ES modules
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportAttributes {
    /// Attribute type (e.g., "json")
    pub type_: String,
}

/// Variable property (let binding semantics)
pub type Property = LetKind;

/// Property name in object literals
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyName {
    /// Literal string property
    Lit(String),
    /// Symbol property
    Symbol,
}

impl PropertyName {
    /// Create a literal property name
    pub fn lit(s: impl Into<String>) -> Self {
        PropertyName::Lit(s.into())
    }

    /// Check if this is a valid JavaScript identifier
    pub fn is_valid_identifier(&self) -> bool {
        match self {
            PropertyName::Lit(s) => is_valid_js_identifier(s),
            PropertyName::Symbol => false,
        }
    }
}

/// Check if a string is a valid JavaScript identifier
fn is_valid_js_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let mut chars = s.chars();

    // First character must be letter, underscore, or dollar sign
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' || c == '$' => {}
        _ => return false,
    }

    // Subsequent characters can also include digits
    for c in chars {
        if !c.is_ascii_alphanumeric() && c != '_' && c != '$' {
            return false;
        }
    }

    // Check if it's not a reserved word
    !is_reserved_word(s)
}

/// Check if a string is a JavaScript reserved word
fn is_reserved_word(s: &str) -> bool {
    matches!(
        s,
        "break"
            | "case"
            | "catch"
            | "continue"
            | "debugger"
            | "default"
            | "delete"
            | "do"
            | "else"
            | "finally"
            | "for"
            | "function"
            | "if"
            | "in"
            | "instanceof"
            | "new"
            | "return"
            | "switch"
            | "this"
            | "throw"
            | "try"
            | "typeof"
            | "var"
            | "void"
            | "while"
            | "with"
            | "class"
            | "const"
            | "enum"
            | "export"
            | "extends"
            | "import"
            | "super"
            | "implements"
            | "interface"
            | "let"
            | "package"
            | "private"
            | "protected"
            | "public"
            | "static"
            | "yield"
            | "await"
            | "null"
            | "true"
            | "false"
    )
}

/// Float literal
#[derive(Debug, Clone, PartialEq)]
pub struct FloatLit {
    /// The float value as a string (preserves precision)
    pub f: String,
}

/// BigInt literal
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BigIntLit {
    /// Is this a positive number?
    pub positive: bool,
    /// The value as a string
    pub value: String,
}

/// Number literal
#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    /// Float number
    Float(FloatLit),
    /// Integer with optional character representation
    Int {
        /// The integer value
        i: i32,
        /// Optional character code point
        c: Option<i32>,
    },
    /// BigInt
    BigInt(BigIntLit),
}

impl Number {
    /// Create an integer number
    pub fn int(i: i32) -> Self {
        Number::Int { i, c: None }
    }

    /// Create a character number
    pub fn char(c: char) -> Self {
        Number::Int {
            i: c as i32,
            c: Some(c as i32),
        }
    }

    /// Create a float number
    pub fn float(f: impl Into<String>) -> Self {
        Number::Float(FloatLit { f: f.into() })
    }

    /// Create a bigint number
    pub fn bigint(positive: bool, value: impl Into<String>) -> Self {
        Number::BigInt(BigIntLit {
            positive,
            value: value.into(),
        })
    }
}

/// Mutable flag for arrays/objects
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MutableFlag {
    /// Mutable
    Mutable,
    /// Immutable
    #[default]
    Immutable,
    /// Not applicable
    NA,
}

/// Recursive info for bindings
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecursiveInfo {
    SingleRecursive,
    NonRecursive,
    NA,
}

/// Used statistics for dead code elimination
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum UsedStats {
    /// Dead and pure (can be removed)
    DeadPure,
    /// Dead but has side effects
    DeadNonPure,
    /// Exported (cannot be removed)
    Exported,
    /// Used once and pure (candidate for inlining)
    OncePure,
    /// Used
    #[default]
    Used,
    /// Currently scanning (pure)
    ScanningPure,
    /// Currently scanning (non-pure)
    ScanningNonPure,
    /// Not applicable
    NA,
}

/// Identifier usage info
#[derive(Debug, Clone, Default)]
pub struct IdentInfo {
    /// Usage statistics
    pub used_stats: UsedStats,
}

/// Length object type (for .length property)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LengthObject {
    Array,
    String,
    Bytes,
    Function,
    CamlBlock,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binop_to_string() {
        assert_eq!(BinOp::Plus.to_js_string(), "+");
        assert_eq!(BinOp::EqEqEq.to_js_string(), "===");
        assert_eq!(BinOp::And.to_js_string(), "&&");
    }

    #[test]
    fn test_binop_precedence() {
        assert!(BinOp::Mul.precedence() > BinOp::Plus.precedence());
        assert!(BinOp::And.precedence() > BinOp::Or.precedence());
        assert!(BinOp::EqEqEq.precedence() > BinOp::And.precedence());
    }

    #[test]
    fn test_binop_classification() {
        assert!(BinOp::EqEqEq.is_comparison());
        assert!(BinOp::And.is_logical());
        assert!(BinOp::Plus.is_arithmetic());
        assert!(BinOp::Bor.is_bitwise());
    }

    #[test]
    fn test_property_name() {
        let lit = PropertyName::lit("foo");
        assert!(lit.is_valid_identifier());

        let reserved = PropertyName::lit("function");
        assert!(!reserved.is_valid_identifier());

        let invalid = PropertyName::lit("123abc");
        assert!(!invalid.is_valid_identifier());

        let symbol = PropertyName::Symbol;
        assert!(!symbol.is_valid_identifier());
    }

    #[test]
    fn test_number_creation() {
        let int = Number::int(42);
        match int {
            Number::Int { i: 42, c: None } => {}
            _ => panic!("Expected Int(42)"),
        }

        let char_num = Number::char('A');
        match char_num {
            Number::Int { i: 65, c: Some(65) } => {}
            _ => panic!("Expected Int(65)"),
        }

        let float = Number::float("3.14");
        match float {
            Number::Float(FloatLit { ref f }) => assert_eq!(f, "3.14"),
            _ => panic!("Expected Float"),
        }
    }

    #[test]
    fn test_is_valid_js_identifier() {
        assert!(is_valid_js_identifier("foo"));
        assert!(is_valid_js_identifier("_private"));
        assert!(is_valid_js_identifier("$jquery"));
        assert!(is_valid_js_identifier("camelCase123"));

        assert!(!is_valid_js_identifier(""));
        assert!(!is_valid_js_identifier("123abc"));
        assert!(!is_valid_js_identifier("foo-bar"));
        assert!(!is_valid_js_identifier("function"));
        assert!(!is_valid_js_identifier("class"));
    }
}
