//! Lambda compatibility types.
//!
//! This module contains types that are shared between Lambda IR and other
//! parts of the compiler, maintaining compatibility with the OCaml types.

use crate::parser::ast::MutableFlag;

/// Comparison operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comparison {
    /// Equal
    Eq,
    /// Not equal
    Neq,
    /// Less than
    Lt,
    /// Greater than
    Gt,
    /// Less than or equal
    Le,
    /// Greater than or equal
    Ge,
}

impl Comparison {
    /// Compare two i32 values
    pub fn cmp_int32(self, a: i32, b: i32) -> bool {
        match self {
            Comparison::Eq => a == b,
            Comparison::Neq => a != b,
            Comparison::Lt => a < b,
            Comparison::Gt => a > b,
            Comparison::Le => a <= b,
            Comparison::Ge => a >= b,
        }
    }

    /// Compare two f64 values
    pub fn cmp_float(self, a: f64, b: f64) -> bool {
        match self {
            Comparison::Eq => a == b,
            Comparison::Neq => a != b,
            Comparison::Lt => a < b,
            Comparison::Gt => a > b,
            Comparison::Le => a <= b,
            Comparison::Ge => a >= b,
        }
    }

    /// Compare two i32 values (same as cmp_int32)
    pub fn cmp_int(self, a: i32, b: i32) -> bool {
        self.cmp_int32(a, b)
    }
}

/// Let binding kind
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LetKind {
    /// Strict evaluation (evaluated immediately)
    Strict,
    /// Alias (may be inlined)
    Alias,
    /// Strict evaluation with optional unused binding
    StrictOpt,
    /// Variable (mutable)
    Variable,
}

/// Field debug info for record/tuple field access
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldDbgInfo {
    /// Record field
    Record {
        name: String,
        mutable_flag: MutableFlag,
    },
    /// Module field
    Module { name: String },
    /// Inlined record field
    RecordInline { name: String },
    /// Record extension field
    RecordExtension { name: String },
    /// Tuple field
    Tuple,
    /// Poly variant tag
    PolyVarTag,
    /// Poly variant content
    PolyVarContent,
    /// Extension field
    Extension,
    /// Variant field
    Variant,
    /// List cons field
    Cons,
}

impl FieldDbgInfo {
    /// Get the field name if available
    pub fn name(&self) -> Option<&str> {
        match self {
            FieldDbgInfo::Record { name, .. }
            | FieldDbgInfo::Module { name }
            | FieldDbgInfo::RecordInline { name }
            | FieldDbgInfo::RecordExtension { name } => Some(name),
            FieldDbgInfo::Tuple
            | FieldDbgInfo::PolyVarTag
            | FieldDbgInfo::PolyVarContent
            | FieldDbgInfo::Extension
            | FieldDbgInfo::Variant
            | FieldDbgInfo::Cons => None,
        }
    }
}

/// Set field debug info for record field mutation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SetFieldDbgInfo {
    /// Record set
    RecordSet(String),
    /// Inlined record set
    RecordInlineSet(String),
    /// Record extension set
    RecordExtensionSet(String),
}

impl SetFieldDbgInfo {
    /// Get the field name
    pub fn name(&self) -> &str {
        match self {
            SetFieldDbgInfo::RecordSet(name)
            | SetFieldDbgInfo::RecordInlineSet(name)
            | SetFieldDbgInfo::RecordExtensionSet(name) => name,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comparison_int32() {
        assert!(Comparison::Eq.cmp_int32(5, 5));
        assert!(!Comparison::Eq.cmp_int32(5, 6));

        assert!(Comparison::Neq.cmp_int32(5, 6));
        assert!(!Comparison::Neq.cmp_int32(5, 5));

        assert!(Comparison::Lt.cmp_int32(5, 6));
        assert!(!Comparison::Lt.cmp_int32(6, 5));

        assert!(Comparison::Gt.cmp_int32(6, 5));
        assert!(!Comparison::Gt.cmp_int32(5, 6));

        assert!(Comparison::Le.cmp_int32(5, 5));
        assert!(Comparison::Le.cmp_int32(5, 6));
        assert!(!Comparison::Le.cmp_int32(6, 5));

        assert!(Comparison::Ge.cmp_int32(5, 5));
        assert!(Comparison::Ge.cmp_int32(6, 5));
        assert!(!Comparison::Ge.cmp_int32(5, 6));
    }

    #[test]
    fn test_comparison_float() {
        assert!(Comparison::Eq.cmp_float(5.0, 5.0));
        assert!(!Comparison::Eq.cmp_float(5.0, 5.1));

        assert!(Comparison::Lt.cmp_float(5.0, 5.1));
        assert!(Comparison::Gt.cmp_float(5.1, 5.0));
    }

    #[test]
    fn test_field_dbg_info_name() {
        let record = FieldDbgInfo::Record {
            name: "field".to_string(),
            mutable_flag: MutableFlag::Immutable,
        };
        assert_eq!(record.name(), Some("field"));

        let tuple = FieldDbgInfo::Tuple;
        assert_eq!(tuple.name(), None);
    }

    #[test]
    fn test_set_field_dbg_info_name() {
        let set = SetFieldDbgInfo::RecordSet("contents".to_string());
        assert_eq!(set.name(), "contents");
    }
}
