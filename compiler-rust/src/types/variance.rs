//! Variance analysis for type parameters.
//!
//! Variance describes how a type parameter appears in a type:
//! - **Covariant**: The type parameter appears only in "positive" positions
//! - **Contravariant**: The type parameter appears only in "negative" positions
//! - **Invariant**: The type parameter appears in both positions
//! - **Bivariant**: The type parameter doesn't appear (can be treated as either)
//!
//! # Example
//!
//! ```rust
//! use rescript_compiler::types::Variance;
//!
//! // Covariant type parameter
//! let v = Variance::covariant();
//! assert!(v.is_covariant());
//!
//! // Combining variances
//! let v1 = Variance::covariant();
//! let v2 = Variance::contravariant();
//! let combined = v1.union(&v2);
//! assert!(combined.is_invariant());
//! ```

use serde::{Deserialize, Serialize};
use std::fmt;

/// Variance flags stored as a bitfield.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct Variance(u8);

/// Individual variance flags.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarianceFlag {
    /// May appear in positive (covariant) positions
    MayPos,
    /// May appear in negative (contravariant) positions
    MayNeg,
    /// May be weakly polymorphic
    MayWeak,
    /// Injective (type preserves identity)
    Inj,
    /// Definitely appears in positive positions
    Pos,
    /// Definitely appears in negative positions
    Neg,
    /// Invariant
    Inv,
}

impl VarianceFlag {
    /// Get the bit value for this flag.
    fn bit(self) -> u8 {
        match self {
            VarianceFlag::MayPos => 1,
            VarianceFlag::MayNeg => 2,
            VarianceFlag::MayWeak => 4,
            VarianceFlag::Inj => 8,
            VarianceFlag::Pos => 16,
            VarianceFlag::Neg => 32,
            VarianceFlag::Inv => 64,
        }
    }
}

impl Variance {
    /// No variance (null).
    pub const NULL: Variance = Variance(0);

    /// May be invariant (may_pos | may_neg | may_weak).
    pub const MAY_INV: Variance = Variance(7);

    /// Full variance (all flags set).
    pub const FULL: Variance = Variance(127);

    /// Create a null (empty) variance.
    #[inline]
    pub fn null() -> Self {
        Self::NULL
    }

    /// Create a full (invariant) variance.
    #[inline]
    pub fn full() -> Self {
        Self::FULL
    }

    /// Create a covariant variance (may_pos | pos | inj).
    #[inline]
    pub fn covariant() -> Self {
        Variance(VarianceFlag::MayPos.bit() | VarianceFlag::Pos.bit() | VarianceFlag::Inj.bit())
    }

    /// Create a contravariant variance.
    #[inline]
    pub fn contravariant() -> Self {
        Variance(VarianceFlag::MayNeg.bit() | VarianceFlag::Neg.bit() | VarianceFlag::Inj.bit())
    }

    /// Create an invariant variance.
    #[inline]
    pub fn invariant() -> Self {
        Self::FULL
    }

    /// Create variance with a single flag.
    #[inline]
    pub fn single(flag: VarianceFlag) -> Self {
        Variance(flag.bit())
    }

    /// Union of two variances.
    #[inline]
    pub fn union(&self, other: &Variance) -> Self {
        Variance(self.0 | other.0)
    }

    /// Intersection of two variances.
    #[inline]
    pub fn inter(&self, other: &Variance) -> Self {
        Variance(self.0 & other.0)
    }

    /// Check if this variance is a subset of another.
    #[inline]
    pub fn subset(&self, other: &Variance) -> bool {
        (self.0 & other.0) == self.0
    }

    /// Set a flag on or off.
    #[inline]
    pub fn set(&self, flag: VarianceFlag, value: bool) -> Self {
        if value {
            Variance(self.0 | flag.bit())
        } else {
            Variance(self.0 & !flag.bit())
        }
    }

    /// Check if a flag is set.
    #[inline]
    pub fn mem(&self, flag: VarianceFlag) -> bool {
        (self.0 & flag.bit()) != 0
    }

    /// Swap two flags.
    #[inline]
    pub fn swap(&self, f1: VarianceFlag, f2: VarianceFlag) -> Self {
        let has_f1 = self.mem(f1);
        let has_f2 = self.mem(f2);
        self.set(f1, has_f2).set(f2, has_f1)
    }

    /// Conjugate the variance (swap positive and negative).
    #[inline]
    pub fn conjugate(&self) -> Self {
        self.swap(VarianceFlag::MayPos, VarianceFlag::MayNeg)
            .swap(VarianceFlag::Pos, VarianceFlag::Neg)
    }

    /// Get the upper bounds (may_pos, may_neg).
    #[inline]
    pub fn get_upper(&self) -> (bool, bool) {
        (
            self.mem(VarianceFlag::MayPos),
            self.mem(VarianceFlag::MayNeg),
        )
    }

    /// Get the lower bounds (pos, neg, inv, inj).
    #[inline]
    pub fn get_lower(&self) -> (bool, bool, bool, bool) {
        (
            self.mem(VarianceFlag::Pos),
            self.mem(VarianceFlag::Neg),
            self.mem(VarianceFlag::Inv),
            self.mem(VarianceFlag::Inj),
        )
    }

    /// Check if this is covariant (pos and not neg).
    #[inline]
    pub fn is_covariant(&self) -> bool {
        self.mem(VarianceFlag::Pos) && !self.mem(VarianceFlag::Neg)
    }

    /// Check if this is contravariant (neg and not pos).
    #[inline]
    pub fn is_contravariant(&self) -> bool {
        self.mem(VarianceFlag::Neg) && !self.mem(VarianceFlag::Pos)
    }

    /// Check if this is invariant (both pos and neg, or inv flag).
    #[inline]
    pub fn is_invariant(&self) -> bool {
        self.mem(VarianceFlag::Inv) || (self.mem(VarianceFlag::Pos) && self.mem(VarianceFlag::Neg))
    }

    /// Check if this is bivariant (neither pos nor neg).
    #[inline]
    pub fn is_bivariant(&self) -> bool {
        !self.mem(VarianceFlag::Pos) && !self.mem(VarianceFlag::Neg)
    }

    /// Check if this is injective.
    #[inline]
    pub fn is_injective(&self) -> bool {
        self.mem(VarianceFlag::Inj)
    }

    /// Get the raw bits value.
    #[inline]
    pub fn bits(&self) -> u8 {
        self.0
    }

    /// Create from raw bits.
    #[inline]
    pub fn from_bits(bits: u8) -> Self {
        Variance(bits)
    }
}

impl fmt::Display for Variance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_invariant() {
            write!(f, "invariant")
        } else if self.is_covariant() {
            write!(f, "covariant")
        } else if self.is_contravariant() {
            write!(f, "contravariant")
        } else if self.is_bivariant() {
            write!(f, "bivariant")
        } else {
            write!(f, "variance({})", self.0)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_null() {
        let v = Variance::null();
        assert_eq!(v, Variance::NULL);
        assert!(v.is_bivariant());
    }

    #[test]
    fn test_covariant() {
        let v = Variance::covariant();
        assert!(v.is_covariant());
        assert!(!v.is_contravariant());
        assert!(!v.is_invariant());
        assert!(v.is_injective());
    }

    #[test]
    fn test_contravariant() {
        let v = Variance::contravariant();
        assert!(!v.is_covariant());
        assert!(v.is_contravariant());
        assert!(!v.is_invariant());
        assert!(v.is_injective());
    }

    #[test]
    fn test_union() {
        let covar = Variance::covariant();
        let contravar = Variance::contravariant();
        let combined = covar.union(&contravar);

        // Union of covariant and contravariant should be invariant
        assert!(combined.mem(VarianceFlag::Pos));
        assert!(combined.mem(VarianceFlag::Neg));
    }

    #[test]
    fn test_conjugate() {
        let covar = Variance::covariant();
        let conjugated = covar.conjugate();

        // Conjugate of covariant should be contravariant
        assert!(conjugated.mem(VarianceFlag::Neg));
        assert!(conjugated.mem(VarianceFlag::MayNeg));
    }

    #[test]
    fn test_get_bounds() {
        let v = Variance::covariant();
        let (may_pos, may_neg) = v.get_upper();
        assert!(may_pos);
        assert!(!may_neg);

        let (pos, neg, inv, inj) = v.get_lower();
        assert!(pos);
        assert!(!neg);
        assert!(!inv);
        assert!(inj);
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Variance::covariant()), "covariant");
        assert_eq!(format!("{}", Variance::contravariant()), "contravariant");
        assert_eq!(format!("{}", Variance::invariant()), "invariant");
        assert_eq!(format!("{}", Variance::null()), "bivariant");
    }

    #[test]
    fn test_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Variance>();
    }
}
