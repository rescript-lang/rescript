//! Type declarations and signatures.
//!
//! This module defines the types for type declarations, value descriptions,
//! module types, and signatures - the building blocks of module interfaces.

use super::asttypes::{Located, MutableFlag, PrivateFlag};
use super::path::Path;
use super::type_expr::TypeExprRef;
use super::variance::Variance;
use crate::ident::Ident;
use crate::location::Location;
use serde::{Deserialize, Serialize};

// ============================================================================
// Value Descriptions
// ============================================================================

/// A value description (type of a let-bound value).
#[derive(Debug, Clone)]
pub struct ValueDescription {
    /// Type of the value
    pub val_type: TypeExprRef,
    /// Kind of value (regular or primitive)
    pub val_kind: ValueKind,
    /// Source location
    pub val_loc: Location,
    /// Attributes
    pub val_attributes: Vec<Attribute>,
}

/// Kind of value.
#[derive(Debug, Clone)]
pub enum ValueKind {
    /// Regular value
    ValReg,
    /// Primitive value
    ValPrim(PrimitiveDescription),
}

/// Description of a primitive operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrimitiveDescription {
    /// Primitive name
    pub prim_name: String,
    /// Arity
    pub prim_arity: i32,
    /// Native name (if different)
    pub prim_native_name: String,
    /// Native float flag
    pub prim_native_float: bool,
}

// ============================================================================
// Type Declarations
// ============================================================================

/// A type declaration (`type t = ...`).
#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    /// Type parameters
    pub type_params: Vec<TypeExprRef>,
    /// Number of type parameters
    pub type_arity: i32,
    /// Kind of type (abstract, record, variant, open)
    pub type_kind: TypeKind,
    /// Private flag
    pub type_private: PrivateFlag,
    /// Manifest type (for type aliases)
    pub type_manifest: Option<TypeExprRef>,
    /// Variance of type parameters
    pub type_variance: Vec<Variance>,
    /// Newtype level (for local abstract types)
    pub type_newtype_level: Option<(i32, i32)>,
    /// Source location
    pub type_loc: Location,
    /// Attributes
    pub type_attributes: Vec<Attribute>,
    /// Is this type immediate (unboxed)?
    pub type_immediate: bool,
    /// Unboxed status
    pub type_unboxed: UnboxedStatus,
    /// Inlined types (for record printing)
    pub type_inlined_types: Vec<TypeInlinedType>,
}

/// An inlined type representation.
#[derive(Debug, Clone)]
pub enum TypeInlinedType {
    /// Inlined record
    Record {
        type_name: String,
        labels: Vec<LabelDeclaration>,
    },
}

/// Kind of type definition.
#[derive(Debug, Clone)]
pub enum TypeKind {
    /// Abstract type
    TypeAbstract,
    /// Record type
    TypeRecord(Vec<LabelDeclaration>, RecordRepresentation),
    /// Variant type
    TypeVariant(Vec<ConstructorDeclaration>),
    /// Open type (extensible)
    TypeOpen,
}

/// Record representation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordRepresentation {
    /// Regular record (all fields boxed/tagged)
    RecordRegular,
    /// Float record (unused, kept for compatibility)
    RecordFloatUnused,
    /// Unboxed single-field record
    RecordUnboxed(bool),
    /// Inlined record (in variant constructor)
    RecordInlined {
        tag: i32,
        name: String,
        num_nonconsts: i32,
        attrs: Vec<Attribute>,
    },
    /// Extension record
    RecordExtension,
}

/// Unboxed status for types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnboxedStatus {
    /// Is the type unboxed?
    pub unboxed: bool,
    /// Is this the default (unannotated)?
    pub default: bool,
}

impl UnboxedStatus {
    pub const FALSE_FALSE: UnboxedStatus = UnboxedStatus {
        unboxed: false,
        default: false,
    };
    pub const FALSE_TRUE: UnboxedStatus = UnboxedStatus {
        unboxed: false,
        default: true,
    };
    pub const TRUE_FALSE: UnboxedStatus = UnboxedStatus {
        unboxed: true,
        default: false,
    };
    pub const TRUE_TRUE: UnboxedStatus = UnboxedStatus {
        unboxed: true,
        default: true,
    };
}

impl Default for UnboxedStatus {
    fn default() -> Self {
        Self::FALSE_TRUE
    }
}

/// A label (field) declaration in a record.
#[derive(Debug, Clone)]
pub struct LabelDeclaration {
    /// Label identifier
    pub ld_id: Ident,
    /// Is the field mutable?
    pub ld_mutable: MutableFlag,
    /// Is the field optional?
    pub ld_optional: bool,
    /// Field type
    pub ld_type: TypeExprRef,
    /// Source location
    pub ld_loc: Location,
    /// Attributes
    pub ld_attributes: Vec<Attribute>,
}

/// A constructor declaration in a variant.
#[derive(Debug, Clone)]
pub struct ConstructorDeclaration {
    /// Constructor identifier
    pub cd_id: Ident,
    /// Constructor arguments
    pub cd_args: ConstructorArguments,
    /// Result type (for GADTs)
    pub cd_res: Option<TypeExprRef>,
    /// Source location
    pub cd_loc: Location,
    /// Attributes
    pub cd_attributes: Vec<Attribute>,
}

/// Constructor arguments.
#[derive(Debug, Clone)]
pub enum ConstructorArguments {
    /// Tuple arguments
    CstrTuple(Vec<TypeExprRef>),
    /// Inline record arguments
    CstrRecord(Vec<LabelDeclaration>),
}

// ============================================================================
// Extension Constructors
// ============================================================================

/// An extension constructor.
#[derive(Debug, Clone)]
pub struct ExtensionConstructor {
    /// Path to the extensible type
    pub ext_type_path: Path,
    /// Type parameters
    pub ext_type_params: Vec<TypeExprRef>,
    /// Constructor arguments
    pub ext_args: ConstructorArguments,
    /// Result type (for GADTs)
    pub ext_ret_type: Option<TypeExprRef>,
    /// Private flag
    pub ext_private: PrivateFlag,
    /// Source location
    pub ext_loc: Location,
    /// Attributes
    pub ext_attributes: Vec<Attribute>,
    /// Is this an exception?
    pub ext_is_exception: bool,
}

// ============================================================================
// Constructor and Label Descriptions
// ============================================================================

/// A constructor description (used in typing environment).
#[derive(Debug, Clone)]
pub struct ConstructorDescription {
    /// Constructor name
    pub cstr_name: String,
    /// Result type
    pub cstr_res: TypeExprRef,
    /// Existential type variables
    pub cstr_existentials: Vec<TypeExprRef>,
    /// Argument types
    pub cstr_args: Vec<TypeExprRef>,
    /// Number of arguments
    pub cstr_arity: i32,
    /// Constructor tag
    pub cstr_tag: ConstructorTag,
    /// Number of constant constructors in the type
    pub cstr_consts: i32,
    /// Number of non-constant constructors
    pub cstr_nonconsts: i32,
    /// Is this a GADT constructor?
    pub cstr_generalized: bool,
    /// Private flag
    pub cstr_private: PrivateFlag,
    /// Source location
    pub cstr_loc: Location,
    /// Attributes
    pub cstr_attributes: Vec<Attribute>,
    /// Inlined record declaration (if any)
    pub cstr_inlined: Option<Box<TypeDeclaration>>,
}

/// Constructor tag for heap blocks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstructorTag {
    /// Constant constructor (an int)
    CstrConstant(i32),
    /// Regular constructor (a block)
    CstrBlock(i32),
    /// Unboxed constructor
    CstrUnboxed,
    /// Extension constructor
    CstrExtension(Path),
}

impl ConstructorTag {
    /// Check if two tags are equal.
    pub fn equal(&self, other: &ConstructorTag) -> bool {
        match (self, other) {
            (ConstructorTag::CstrConstant(i1), ConstructorTag::CstrConstant(i2)) => i1 == i2,
            (ConstructorTag::CstrBlock(i1), ConstructorTag::CstrBlock(i2)) => i1 == i2,
            (ConstructorTag::CstrUnboxed, ConstructorTag::CstrUnboxed) => true,
            (ConstructorTag::CstrExtension(p1), ConstructorTag::CstrExtension(p2)) => p1.same(p2),
            _ => false,
        }
    }
}

/// A label description (used in typing environment).
#[derive(Debug, Clone)]
pub struct LabelDescription {
    /// Short name
    pub lbl_name: String,
    /// Result type (the record type)
    pub lbl_res: TypeExprRef,
    /// Argument type (the field type)
    pub lbl_arg: TypeExprRef,
    /// Is this mutable?
    pub lbl_mut: MutableFlag,
    /// Is this optional?
    pub lbl_optional: bool,
    /// Position in block
    pub lbl_pos: i32,
    /// All labels in this type (index reference)
    pub lbl_all: LabelArrayRef,
    /// Record representation
    pub lbl_repres: RecordRepresentation,
    /// Private flag
    pub lbl_private: PrivateFlag,
    /// Source location
    pub lbl_loc: Location,
    /// Attributes
    pub lbl_attributes: Vec<Attribute>,
}

/// Reference to an array of label descriptions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LabelArrayRef(pub usize);

// ============================================================================
// Module Types
// ============================================================================

/// A module type.
#[derive(Debug, Clone)]
pub enum ModuleType {
    /// Module type identifier
    MtyIdent(Path),
    /// Signature
    MtySignature(Signature),
    /// Functor
    MtyFunctor {
        param: Ident,
        param_type: Option<Box<ModuleType>>,
        result: Box<ModuleType>,
    },
    /// Module alias
    MtyAlias(AliasPresence, Path),
}

/// Alias presence flag.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AliasPresence {
    /// Alias is present
    MtaPresent,
    /// Alias is absent
    MtaAbsent,
}

/// A signature (list of signature items).
pub type Signature = Vec<SignatureItem>;

/// A signature item.
#[derive(Debug, Clone)]
pub enum SignatureItem {
    /// Value in signature
    SigValue(Ident, ValueDescription),
    /// Type in signature
    SigType(Ident, TypeDeclaration, RecStatus),
    /// Type extension in signature
    SigTypext(Ident, ExtensionConstructor, ExtStatus),
    /// Module in signature
    SigModule(Ident, ModuleDeclaration, RecStatus),
    /// Module type in signature
    SigModtype(Ident, ModtypeDeclaration),
    /// Class (dummy for OCaml compatibility)
    SigClass,
    /// Class type (dummy for OCaml compatibility)
    SigClassType,
}

/// A module declaration.
#[derive(Debug, Clone)]
pub struct ModuleDeclaration {
    /// Module type
    pub md_type: ModuleType,
    /// Attributes
    pub md_attributes: Vec<Attribute>,
    /// Source location
    pub md_loc: Location,
}

/// A module type declaration.
#[derive(Debug, Clone)]
pub struct ModtypeDeclaration {
    /// Module type (None for abstract)
    pub mtd_type: Option<ModuleType>,
    /// Attributes
    pub mtd_attributes: Vec<Attribute>,
    /// Source location
    pub mtd_loc: Location,
}

/// Recursive status for type/module declarations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecStatus {
    /// Not first in a nonrecursive group
    TrecNot,
    /// First in a recursive group
    TrecFirst,
    /// Not first in a recursive/nonrecursive group
    TrecNext,
}

/// Extension status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtStatus {
    /// First constructor of an extension
    TextFirst,
    /// Not first constructor
    TextNext,
    /// An exception
    TextException,
}

// ============================================================================
// Attributes
// ============================================================================

/// An attribute.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Attribute {
    /// Attribute name
    pub attr_name: Located<String>,
    /// Attribute payload (simplified)
    pub attr_payload: AttributePayload,
    /// Source location
    pub attr_loc: Location,
}

/// Attribute payload.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AttributePayload {
    /// Empty payload
    Empty,
    /// String payload
    Str(String),
    /// Identifier payload
    Ident(String),
    /// Structure payload (list of string pairs)
    Structure(Vec<(String, String)>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constructor_tag_equality() {
        let t1 = ConstructorTag::CstrConstant(0);
        let t2 = ConstructorTag::CstrConstant(0);
        let t3 = ConstructorTag::CstrConstant(1);

        assert!(t1.equal(&t2));
        assert!(!t1.equal(&t3));
    }

    #[test]
    fn test_unboxed_status() {
        let s = UnboxedStatus::default();
        assert!(!s.unboxed);
        assert!(s.default);
    }

    #[test]
    fn test_record_representation() {
        let r1 = RecordRepresentation::RecordRegular;
        let r2 = RecordRepresentation::RecordRegular;
        assert_eq!(r1, r2);
    }

    #[test]
    fn test_value_kind() {
        let vk = ValueKind::ValReg;
        assert!(matches!(vk, ValueKind::ValReg));
    }
}
