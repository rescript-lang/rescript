//! Lambda primitives - Built-in operations.
//!
//! This module defines primitive operations that are directly compiled
//! to JavaScript operations. These include arithmetic, comparisons,
//! array/string operations, and FFI calls.

use super::compat::{Comparison, FieldDbgInfo, SetFieldDbgInfo};
use super::tag_info::TagInfo;

/// Record representation for optimization purposes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordRepresentation {
    /// Regular record
    Regular,
    /// Inlined record (variant payload)
    Inlined {
        tag: i32,
        name: String,
        num_nonconsts: i32,
    },
    /// Extension record
    Extension,
}

/// Raw JavaScript code info
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsRawInfo {
    /// The JavaScript code
    pub code: String,
    /// Is this a statement or expression?
    pub is_stmt: bool,
}

/// External argument specification
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternalArgSpec {
    // Placeholder - actual structure from External_arg_spec
    pub label: Option<String>,
}

/// External FFI specification
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternalFfiSpec {
    // Placeholder - actual structure from External_ffi_types
    pub module_name: Option<String>,
}

/// Primitive operations
///
/// These are built-in operations that compile directly to JavaScript.
#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    // ==================== Block Operations ====================
    /// Make a block (tuple, record, variant)
    Pmakeblock(i32, TagInfo, Mutable),

    /// Get a field from a block
    Pfield(i32, FieldDbgInfo),

    /// Set a field in a block
    Psetfield(i32, SetFieldDbgInfo),

    /// Duplicate a record
    Pduprecord,

    // ==================== External Call ====================
    /// JavaScript FFI call
    PjsCall {
        prim_name: String,
        arg_types: Vec<ExternalArgSpec>,
        ffi: ExternalFfiSpec,
        dynamic_import: bool,
        transformed_jsx: bool,
    },

    /// Create a JavaScript object
    PjsObjectCreate(Vec<ExternalArgSpec>),

    // ==================== Exceptions ====================
    /// Raise an exception
    Praise,

    // ==================== Object Primitives ====================
    /// Object comparison
    Pobjcomp(Comparison),
    /// Object ordering
    Pobjorder,
    /// Object min
    Pobjmin,
    /// Object max
    Pobjmax,
    /// Get object tag
    Pobjtag,
    /// Get object size
    Pobjsize,

    // ==================== Boolean Primitives ====================
    /// Boolean and (short-circuit)
    Psequand,
    /// Boolean or (short-circuit)
    Psequor,
    /// Boolean not
    Pnot,
    /// Boolean comparison
    Pboolcomp(Comparison),
    /// Boolean ordering
    Pboolorder,
    /// Boolean min
    Pboolmin,
    /// Boolean max
    Pboolmax,

    // ==================== Integer Primitives ====================
    /// Is integer (vs block)
    Pisint,
    /// Integer negation
    Pnegint,
    /// Integer addition
    Paddint,
    /// Integer subtraction
    Psubint,
    /// Integer multiplication
    Pmulint,
    /// Integer division
    Pdivint,
    /// Integer modulo
    Pmodint,
    /// Integer power
    Ppowint,
    /// Bitwise and
    Pandint,
    /// Bitwise or
    Porint,
    /// Bitwise xor
    Pxorint,
    /// Bitwise not
    Pnotint,
    /// Left shift
    Plslint,
    /// Logical right shift
    Plsrint,
    /// Arithmetic right shift
    Pasrint,
    /// Add offset to integer
    Poffsetint(i32),
    /// Add offset to reference
    Poffsetref(i32),
    /// Integer comparison
    Pintcomp(Comparison),
    /// Integer ordering
    Pintorder,
    /// Integer min
    Pintmin,
    /// Integer max
    Pintmax,

    // ==================== Float Primitives ====================
    /// Integer of float
    Pintoffloat,
    /// Float of integer
    Pfloatofint,
    /// Float negation
    Pnegfloat,
    /// Float addition
    Paddfloat,
    /// Float subtraction
    Psubfloat,
    /// Float multiplication
    Pmulfloat,
    /// Float division
    Pdivfloat,
    /// Float modulo
    Pmodfloat,
    /// Float power
    Ppowfloat,
    /// Float comparison
    Pfloatcomp(Comparison),
    /// Float ordering
    Pfloatorder,
    /// Float min
    Pfloatmin,
    /// Float max
    Pfloatmax,

    // ==================== BigInt Primitives ====================
    /// BigInt negation
    Pnegbigint,
    /// BigInt addition
    Paddbigint,
    /// BigInt subtraction
    Psubbigint,
    /// BigInt multiplication
    Pmulbigint,
    /// BigInt division
    Pdivbigint,
    /// BigInt modulo
    Pmodbigint,
    /// BigInt power
    Ppowbigint,
    /// BigInt bitwise and
    Pandbigint,
    /// BigInt bitwise or
    Porbigint,
    /// BigInt bitwise xor
    Pxorbigint,
    /// BigInt bitwise not
    Pnotbigint,
    /// BigInt left shift
    Plslbigint,
    /// BigInt arithmetic right shift
    Pasrbigint,
    /// BigInt comparison
    Pbigintcomp(Comparison),
    /// BigInt ordering
    Pbigintorder,
    /// BigInt min
    Pbigintmin,
    /// BigInt max
    Pbigintmax,

    // ==================== String Primitives ====================
    /// String length
    Pstringlength,
    /// String char at (unsafe)
    Pstringrefu,
    /// String char at (safe)
    Pstringrefs,
    /// String concatenation
    Pstringadd,
    /// String comparison
    Pstringcomp(Comparison),
    /// String ordering
    Pstringorder,
    /// String min
    Pstringmin,
    /// String max
    Pstringmax,

    // ==================== Array Primitives ====================
    /// Make array
    Pmakearray,
    /// Array length
    Parraylength,
    /// Array get (unsafe)
    Parrayrefu,
    /// Array set (unsafe)
    Parraysetu,
    /// Array get (safe)
    Parrayrefs,
    /// Array set (safe)
    Parraysets,

    // ==================== List Primitives ====================
    /// Make list
    Pmakelist,

    // ==================== Dict Primitives ====================
    /// Make dict
    Pmakedict,
    /// Dict has key
    PdictHas,

    // ==================== Promise Primitives ====================
    /// Await promise
    Pawait,

    // ==================== Misc Primitives ====================
    /// Check if polyvar block
    PisPolyVarBlock,
    /// Check if out of range
    Pisout(i32),
    /// JavaScript comparison
    Pjscomp(Comparison),
    /// Apply function
    PjsApply,
    /// Runtime apply
    PjsRuntimeApply,
    /// Debugger statement
    Pdebugger,
    /// Unsafe downgrade (method call)
    PjsUnsafeDowngrade { name: String, setter: bool },
    /// Initialize module
    PinitMod,
    /// Update module
    PupdateMod,
    /// Raw JavaScript code
    PrawJsCode(JsRawInfo),
    /// Make function with arity
    PjsFnMake(i32),
    /// Make unit function
    PjsFnMakeUnit,
    /// Method
    PjsFnMethod,
    /// Null to option
    PnullToOpt,
    /// Null or undefined to option
    PnullUndefinedToOpt,
    /// Is null
    PisNull,
    /// Is undefined
    PisUndefined,
    /// Is null or undefined
    PisNullUndefined,
    /// Dynamic import
    Pimport,
    /// Typeof
    Ptypeof,
    /// Function arity
    PfnArity,
    /// Wrap exception
    PwrapExn,
    /// Create extension
    PcreateExtension(String),
    /// Is not none
    PisNotNone,
    /// Value from option
    PvalFromOption,
    /// Value from option (not nested)
    PvalFromOptionNotNest,
    /// Some
    Psome,
    /// Some (not nested)
    PsomeNotNest,
    /// Hash
    Phash,
    /// Hash mix string
    PhashMixstring,
    /// Hash mix int
    PhashMixint,
    /// Hash final mix
    PhashFinalmix,
}

/// Mutable flag
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutable {
    Mutable,
    Immutable,
}

impl Primitive {
    /// Check if two primitives are approximately equal (for optimization)
    pub fn eq_approx(&self, other: &Primitive) -> bool {
        match (self, other) {
            // Simple primitives
            (Primitive::Praise, Primitive::Praise)
            | (Primitive::Psequand, Primitive::Psequand)
            | (Primitive::Psequor, Primitive::Psequor)
            | (Primitive::Pnot, Primitive::Pnot)
            | (Primitive::Pisint, Primitive::Pisint)
            | (Primitive::Pnegint, Primitive::Pnegint)
            | (Primitive::Paddint, Primitive::Paddint)
            | (Primitive::Psubint, Primitive::Psubint)
            | (Primitive::Pmulint, Primitive::Pmulint)
            | (Primitive::Pdivint, Primitive::Pdivint)
            | (Primitive::Pmodint, Primitive::Pmodint)
            | (Primitive::Pandint, Primitive::Pandint)
            | (Primitive::Porint, Primitive::Porint)
            | (Primitive::Pxorint, Primitive::Pxorint)
            | (Primitive::Plslint, Primitive::Plslint)
            | (Primitive::Plsrint, Primitive::Plsrint)
            | (Primitive::Pasrint, Primitive::Pasrint)
            | (Primitive::Pstringlength, Primitive::Pstringlength)
            | (Primitive::Pstringrefu, Primitive::Pstringrefu)
            | (Primitive::Pstringrefs, Primitive::Pstringrefs)
            | (Primitive::Pstringadd, Primitive::Pstringadd)
            | (Primitive::Pmakearray, Primitive::Pmakearray)
            | (Primitive::Parraylength, Primitive::Parraylength)
            | (Primitive::Parrayrefu, Primitive::Parrayrefu)
            | (Primitive::Parraysetu, Primitive::Parraysetu)
            | (Primitive::Parrayrefs, Primitive::Parrayrefs)
            | (Primitive::Parraysets, Primitive::Parraysets)
            | (Primitive::Pduprecord, Primitive::Pduprecord) => true,

            // Comparison primitives
            (Primitive::Pintcomp(a), Primitive::Pintcomp(b))
            | (Primitive::Pfloatcomp(a), Primitive::Pfloatcomp(b))
            | (Primitive::Pbigintcomp(a), Primitive::Pbigintcomp(b))
            | (Primitive::Pstringcomp(a), Primitive::Pstringcomp(b))
            | (Primitive::Pobjcomp(a), Primitive::Pobjcomp(b))
            | (Primitive::Pboolcomp(a), Primitive::Pboolcomp(b))
            | (Primitive::Pjscomp(a), Primitive::Pjscomp(b)) => a == b,

            // Field operations
            (Primitive::Pfield(n1, info1), Primitive::Pfield(n2, info2)) => {
                n1 == n2 && info1 == info2
            }
            (Primitive::Psetfield(n1, info1), Primitive::Psetfield(n2, info2)) => {
                n1 == n2 && info1 == info2
            }

            // Block operations
            (
                Primitive::Pmakeblock(tag1, info1, mut1),
                Primitive::Pmakeblock(tag2, info2, mut2),
            ) => tag1 == tag2 && info1 == info2 && mut1 == mut2,

            // Offset operations
            (Primitive::Poffsetint(n1), Primitive::Poffsetint(n2))
            | (Primitive::Poffsetref(n1), Primitive::Poffsetref(n2))
            | (Primitive::Pisout(n1), Primitive::Pisout(n2))
            | (Primitive::PjsFnMake(n1), Primitive::PjsFnMake(n2)) => n1 == n2,

            // Extension
            (Primitive::PcreateExtension(a), Primitive::PcreateExtension(b)) => a == b,

            // Unsafe downgrade
            (
                Primitive::PjsUnsafeDowngrade {
                    name: n1,
                    setter: s1,
                },
                Primitive::PjsUnsafeDowngrade {
                    name: n2,
                    setter: s2,
                },
            ) => n1 == n2 && s1 == s2,

            // Raw JS code - never equal (side effects)
            (Primitive::PrawJsCode(_), Primitive::PrawJsCode(_)) => false,

            // All other cases
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_eq_approx() {
        assert!(Primitive::Paddint.eq_approx(&Primitive::Paddint));
        assert!(!Primitive::Paddint.eq_approx(&Primitive::Psubint));

        assert!(
            Primitive::Pintcomp(Comparison::Eq).eq_approx(&Primitive::Pintcomp(Comparison::Eq))
        );
        assert!(
            !Primitive::Pintcomp(Comparison::Eq).eq_approx(&Primitive::Pintcomp(Comparison::Neq))
        );

        assert!(Primitive::Poffsetint(5).eq_approx(&Primitive::Poffsetint(5)));
        assert!(!Primitive::Poffsetint(5).eq_approx(&Primitive::Poffsetint(10)));
    }

    #[test]
    fn test_raw_js_never_equal() {
        let raw1 = Primitive::PrawJsCode(JsRawInfo {
            code: "console.log(1)".to_string(),
            is_stmt: true,
        });
        let raw2 = Primitive::PrawJsCode(JsRawInfo {
            code: "console.log(1)".to_string(),
            is_stmt: true,
        });
        assert!(!raw1.eq_approx(&raw2));
    }
}
