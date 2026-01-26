//! Typed Abstract Syntax Tree for ReScript.
//!
//! This module defines the typed AST produced by the type checker,
//! corresponding to OCaml's `Typedtree` module.
//!
//! Key differences from the parse tree (`Parsetree`):
//! - Every identifier has a resolved [`Path`]
//! - Every expression and pattern has a [`TypeExprRef`]
//! - Type annotations are type-checked [`CoreType`] values
//!
//! # Architecture
//!
//! The typed tree uses reference types (`TypeExprRef`) instead of owning types,
//! requiring a [`TypeContext`] for allocation. This enables efficient sharing
//! and modification during type checking.

#![allow(missing_docs)]
#![allow(non_camel_case_types)]

use crate::ident::Ident;
use crate::location::{Located, Location};
use crate::parser::ast::{
    ArgLabel, Arity, Attributes, ClosedFlag, DirectionFlag, MutableFlag, OverrideFlag, PrivateFlag,
    RecFlag,
};
use crate::parser::longident::Longident;
use crate::types::{
    ConstructorDescription, LabelDescription, Path, RowDesc, TypeExprRef, ValueDescription,
};

/// A located value (convenience alias).
pub type Loc<T> = Located<T>;

// ============================================================================
// Constants
// ============================================================================

/// Typed constants (internal representation after parsing).
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// Integer constant.
    Int(i32),
    /// Character constant (Unicode codepoint).
    Char(i32),
    /// String constant with optional delimiter.
    String(String, Option<String>),
    /// Float constant (as string to preserve precision).
    Float(String),
    /// BigInt constant (sign, digits).
    BigInt(bool, String),
}

// ============================================================================
// Partiality
// ============================================================================

/// Indicates whether a pattern match is exhaustive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Partial {
    /// Match is partial (not all cases covered).
    Partial,
    /// Match is total (exhaustive).
    Total,
}

// ============================================================================
// Patterns
// ============================================================================

/// A typed pattern.
#[derive(Debug, Clone)]
pub struct Pattern {
    /// Pattern description.
    pub pat_desc: PatternDesc,
    /// Source location.
    pub pat_loc: Location,
    /// Extra information (constraints, unpacks, etc.).
    pub pat_extra: Vec<(PatExtra, Location, Attributes)>,
    /// Type of the pattern.
    pub pat_type: TypeExprRef,
    /// Environment at this pattern.
    pub pat_env: EnvRef,
    /// Attributes on the pattern.
    pub pat_attributes: Attributes,
}

/// Extra information on a pattern.
#[derive(Debug, Clone)]
pub enum PatExtra {
    /// Type constraint: `P : T`.
    Tpat_constraint(TypedCoreType),
    /// Type pattern: `#tconst`.
    Tpat_type(Path, Loc<Longident>),
    /// Open pattern: `M.(P)`.
    Tpat_open(Path, Loc<Longident>, EnvRef),
    /// Unpack pattern: `(module P)`.
    Tpat_unpack,
}

/// Pattern description.
#[derive(Debug, Clone)]
pub enum PatternDesc {
    /// Wildcard: `_`.
    Tpat_any,

    /// Variable: `x`.
    Tpat_var(Ident, Loc<String>),

    /// Alias: `P as x`.
    Tpat_alias(Box<Pattern>, Ident, Loc<String>),

    /// Constant: `1`, `'a'`, `"hello"`, `1.0`.
    Tpat_constant(Constant),

    /// Tuple: `(P1, ..., Pn)`.
    /// Invariant: length >= 2.
    Tpat_tuple(Vec<Pattern>),

    /// Constructor: `C`, `C(P)`, `C(P1, ..., Pn)`.
    Tpat_construct(Loc<Longident>, ConstructorDescription, Vec<Pattern>),

    /// Variant: `` `A ``, `` `A(P) ``.
    Tpat_variant(String, Option<Box<Pattern>>, RowDescRef),

    /// Record: `{l1: P1, ..., ln: Pn}` or `{l1: P1, ..., _}`.
    /// The bool indicates whether the field is optional.
    Tpat_record(
        Vec<(Loc<Longident>, LabelDescription, Pattern, bool)>,
        ClosedFlag,
    ),

    /// Array: `[| P1, ..., Pn |]`.
    Tpat_array(Vec<Pattern>),

    /// Or-pattern: `P1 | P2`.
    /// The RowDesc is Some when translating type patterns.
    Tpat_or(Box<Pattern>, Box<Pattern>, Option<RowDesc>),
}

// ============================================================================
// Expressions
// ============================================================================

/// A typed expression.
#[derive(Debug, Clone)]
pub struct Expression {
    /// Expression description.
    pub exp_desc: ExpressionDesc,
    /// Source location.
    pub exp_loc: Location,
    /// Extra information (constraints, opens, newtypes, etc.).
    pub exp_extra: Vec<(ExpExtra, Location, Attributes)>,
    /// Type of the expression.
    pub exp_type: TypeExprRef,
    /// Environment at this expression.
    pub exp_env: EnvRef,
    /// Attributes on the expression.
    pub exp_attributes: Attributes,
}

/// Extra information on an expression.
#[derive(Debug, Clone)]
pub enum ExpExtra {
    /// Type constraint: `E : T`.
    Texp_constraint(TypedCoreType),
    /// Coercion: `(E : T1 :> T2)` or `(E :> T)`.
    /// First is optional source type, second is required target type.
    Texp_coerce(Option<TypedCoreType>, TypedCoreType),
    /// Open: `let open M in E`.
    Texp_open(OverrideFlag, Path, Loc<Longident>, EnvRef),
    /// Newtype: `fun (type t) -> E`.
    Texp_newtype(String),
}

/// Expression description.
#[derive(Debug, Clone)]
pub enum ExpressionDesc {
    /// Identifier: `x`, `M.x`.
    Texp_ident(Path, Loc<Longident>, ValueDescription),

    /// Constant: `1`, `'a'`, `"hello"`, `1.0`.
    Texp_constant(Constant),

    /// Let binding: `let P = E in E'`.
    Texp_let(RecFlag, Vec<ValueBinding>, Box<Expression>),

    /// Function: `fun P -> E`.
    /// Uses the new multi-parameter representation with FunctionParam.
    Texp_function {
        params: Vec<FunctionParam>,
        body: Vec<Case>,
        partial: Partial,
        arity: Arity,
        /// Whether this is an async function.
        async_: bool,
    },

    /// Application: `E(E1, ..., En)`.
    /// Uses the ApplyArg representation.
    Texp_apply {
        funct: Box<Expression>,
        args: Vec<ApplyArg>,
        /// Whether this is a partial application.
        partial: bool,
        /// Whether this was transformed from JSX.
        transformed_jsx: bool,
    },

    /// Match: `switch E { | P1 => E1 | ... }`.
    /// The two case lists are (value cases, exception cases).
    Texp_match(Box<Expression>, Vec<Case>, Vec<Case>, Partial),

    /// Try: `try E catch { | P1 => E1 | ... }`.
    Texp_try(Box<Expression>, Vec<Case>),

    /// Tuple: `(E1, ..., En)`.
    Texp_tuple(Vec<Expression>),

    /// Constructor: `C`, `C(E)`, `C(E1, ..., En)`.
    Texp_construct(Loc<Longident>, ConstructorDescription, Vec<Expression>),

    /// Variant: `` `A ``, `` `A(E) ``.
    Texp_variant(String, Option<Box<Expression>>),

    /// Record: `{l1: E1, ..., ln: En}` or `{...E0, l1: E1, ...}`.
    /// Fields are (lid, label_desc, definition).
    Texp_record {
        fields: Vec<(Loc<Longident>, LabelDescription, RecordLabelDefinition)>,
        extended_expression: Option<Box<Expression>>,
    },

    /// Field access: `E.l`.
    Texp_field(Box<Expression>, Loc<Longident>, LabelDescription),

    /// Field mutation: `E.l = E'`.
    Texp_setfield(
        Box<Expression>,
        Loc<Longident>,
        LabelDescription,
        Box<Expression>,
    ),

    /// Array literal: `[E1, ..., En]`.
    Texp_array(Vec<Expression>),

    /// If-then-else: `if E1 then E2 else E3`.
    Texp_ifthenelse(Box<Expression>, Box<Expression>, Option<Box<Expression>>),

    /// Sequence: `E1; E2`.
    Texp_sequence(Box<Expression>, Box<Expression>),

    /// While loop: `while E1 do E2 done`.
    Texp_while(Box<Expression>, Box<Expression>),

    /// For loop: `for i = E1 to/downto E2 do E3 done`.
    Texp_for(
        Ident,
        Box<Pattern>,
        Box<Expression>,
        Box<Expression>,
        DirectionFlag,
        Box<Expression>,
    ),

    /// Send: `E#m` (object method call).
    Texp_send(Box<Expression>, MethKind),

    /// Let module: `let module M = ME in E`.
    Texp_letmodule(Ident, Loc<String>, Box<ModuleExpr>, Box<Expression>),

    /// Let exception: `let exception C of T in E`.
    Texp_letexception(ExtensionConstructor, Box<Expression>),

    /// Assert: `assert E`.
    Texp_assert(Box<Expression>),

    /// Pack: `(module M : MT)`.
    Texp_pack(Box<ModuleExpr>),
}

// DirectionFlag and Arity are re-exported from parser::ast

/// Method kind for object method calls.
#[derive(Debug, Clone)]
pub enum MethKind {
    /// Direct method with known type.
    Tmeth_name(String),
    /// Virtual method (looked up at runtime).
    Tmeth_val(Ident),
}

/// Record label definition in a record expression.
#[derive(Debug, Clone)]
pub enum RecordLabelDefinition {
    /// Label is kept from extended expression.
    Kept(TypeExprRef),
    /// Label is overridden with new expression.
    Overridden(Loc<Longident>, Expression),
}

/// An argument in a function application.
#[derive(Debug, Clone)]
pub struct ApplyArg {
    /// Argument label.
    pub label: ArgLabel,
    /// Argument expression.
    pub expression: Expression,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct FunctionParam {
    /// Parameter label.
    pub label: ArgLabel,
    /// Default value for optional parameter.
    pub default: Option<Box<Expression>>,
    /// Parameter pattern.
    pub pat: FunctionParamPattern,
}

/// Pattern for a function parameter.
#[derive(Debug, Clone)]
pub enum FunctionParamPattern {
    /// Simple pattern binding.
    Simple(Pattern),
    /// Nested function parameter.
    Nested(Vec<FunctionParam>),
}

// ============================================================================
// Cases and Bindings
// ============================================================================

/// A case in a match expression.
#[derive(Debug, Clone)]
pub struct Case {
    /// Left-hand side pattern.
    pub c_lhs: Pattern,
    /// Optional guard: `when E`.
    pub c_guard: Option<Expression>,
    /// Right-hand side expression.
    pub c_rhs: Expression,
}

/// A value binding in a let expression.
#[derive(Debug, Clone)]
pub struct ValueBinding {
    /// Binding pattern.
    pub vb_pat: Pattern,
    /// Binding expression.
    pub vb_expr: Expression,
    /// Attributes on the binding.
    pub vb_attributes: Attributes,
    /// Location of the binding.
    pub vb_loc: Location,
}

// ============================================================================
// Type Expressions (Typed Core Types)
// ============================================================================

/// A typed core type (type expression after type checking).
#[derive(Debug, Clone)]
pub struct TypedCoreType {
    /// Type description.
    pub ctyp_desc: TypedCoreTypeDesc,
    /// Resolved type.
    pub ctyp_type: TypeExprRef,
    /// Environment at this type.
    pub ctyp_env: EnvRef,
    /// Source location.
    pub ctyp_loc: Location,
    /// Attributes.
    pub ctyp_attributes: Attributes,
}

/// Typed core type description.
#[derive(Debug, Clone)]
pub enum TypedCoreTypeDesc {
    /// Type variable: `'a`.
    Ttyp_var(Option<String>),
    /// Arrow type: `T1 -> T2`.
    Ttyp_arrow(ArgLabel, Box<TypedCoreType>, Box<TypedCoreType>),
    /// Tuple type: `(T1, ..., Tn)`.
    Ttyp_tuple(Vec<TypedCoreType>),
    /// Type constructor: `t`, `M.t`, `('a, 'b) t`.
    Ttyp_constr(Path, Loc<Longident>, Vec<TypedCoreType>),
    /// Object type: `< m1: T1; ...; mn: Tn >`.
    Ttyp_object(Vec<ObjectField>, ClosedFlag),
    /// Polymorphic type: `'a 'b. T`.
    Ttyp_poly(Vec<String>, Box<TypedCoreType>),
    /// Type alias: `T as 'a`.
    Ttyp_alias(Box<TypedCoreType>, String),
    /// Variant type: `[< `A | `B ]`.
    Ttyp_variant(Vec<RowFieldType>, ClosedFlag, Option<Vec<String>>),
    /// Package type: `(module MT)`.
    Ttyp_package(PackageType),
}

/// An object field type.
#[derive(Debug, Clone)]
pub struct ObjectField {
    /// Field name or inherit.
    pub of_desc: ObjectFieldDesc,
    /// Field location.
    pub of_loc: Location,
    /// Field attributes.
    pub of_attributes: Attributes,
}

/// Object field description.
#[derive(Debug, Clone)]
pub enum ObjectFieldDesc {
    /// Named field.
    OTtag(Loc<String>, TypedCoreType),
    /// Inherited type.
    OTinherit(TypedCoreType),
}

/// A row field type (for polymorphic variants).
#[derive(Debug, Clone)]
pub struct RowFieldType {
    /// Field description.
    pub rf_desc: RowFieldDesc,
    /// Field location.
    pub rf_loc: Location,
    /// Field attributes.
    pub rf_attributes: Attributes,
}

/// Row field description.
#[derive(Debug, Clone)]
pub enum RowFieldDesc {
    /// Tag: `` `A `` or `` `A of T ``.
    Rtag(Loc<String>, bool, Vec<TypedCoreType>),
    /// Inherit: `T`.
    Rinherit(TypedCoreType),
}

/// A package type constraint.
#[derive(Debug, Clone)]
pub struct PackageType {
    /// Module type path.
    pub pack_path: Path,
    /// Module type identifier.
    pub pack_txt: Loc<Longident>,
    /// Type constraints.
    pub pack_fields: Vec<(Loc<Longident>, TypedCoreType)>,
}

// ============================================================================
// Modules (Placeholder types)
// ============================================================================

/// A module expression (placeholder).
#[derive(Debug, Clone)]
pub struct ModuleExpr {
    /// Module description.
    pub mod_desc: ModuleExprDesc,
    /// Module location.
    pub mod_loc: Location,
    /// Module type.
    pub mod_type: ModuleType,
    /// Module environment.
    pub mod_env: EnvRef,
    /// Module attributes.
    pub mod_attributes: Attributes,
}

/// Module expression description (placeholder).
#[derive(Debug, Clone)]
pub enum ModuleExprDesc {
    /// Identifier.
    Tmod_ident(Path, Loc<Longident>),
    /// Structure.
    Tmod_structure(Structure),
    /// Functor application.
    Tmod_apply(Box<ModuleExpr>, Box<ModuleExpr>, ModuleCoercion),
    /// Functor.
    Tmod_functor(Ident, Loc<String>, Option<Box<ModuleType>>, Box<ModuleExpr>),
    /// Constraint.
    Tmod_constraint(
        Box<ModuleExpr>,
        ModuleType,
        ModuleTypeConstraint,
        ModuleCoercion,
    ),
    /// Unpack.
    Tmod_unpack(Expression, ModuleType),
}

/// Module type (placeholder - full definition in decl.rs).
#[derive(Debug, Clone, Default)]
pub struct ModuleType {
    // Placeholder - actual module type comes from decl.rs
    _placeholder: (),
}

impl ModuleType {
    /// Create a new placeholder module type.
    pub fn placeholder() -> Self {
        Self { _placeholder: () }
    }
}

/// Module type constraint.
#[derive(Debug, Clone)]
pub enum ModuleTypeConstraint {
    /// Explicit signature.
    Tmodtype_explicit(Box<ModuleType>),
    /// Implicit (no constraint).
    Tmodtype_implicit,
}

/// Module coercion.
#[derive(Debug, Clone)]
pub enum ModuleCoercion {
    /// No coercion needed.
    Tcoerce_none,
    /// Structural coercion.
    Tcoerce_structure(
        Vec<(usize, ModuleCoercion)>,
        Vec<(Ident, usize, ModuleCoercion)>,
    ),
    /// Functor coercion.
    Tcoerce_functor(Box<ModuleCoercion>, Box<ModuleCoercion>),
    /// Primitive coercion.
    Tcoerce_primitive(PrimitiveCoercion),
    /// Alias coercion.
    Tcoerce_alias(EnvRef, Path, Box<ModuleCoercion>),
}

/// Primitive coercion.
#[derive(Debug, Clone)]
pub struct PrimitiveCoercion {
    /// Primitive name.
    pub pc_name: String,
    /// External name (for FFI).
    pub pc_ext_name: String,
    /// Native name.
    pub pc_native_name: String,
    /// Allocation info.
    pub pc_alloc: i32,
}

/// A structure (module definition body).
#[derive(Debug, Clone)]
pub struct Structure {
    /// Structure items.
    pub str_items: Vec<StructureItem>,
    /// Structure type.
    pub str_type: Vec<SignatureItem>,
    /// Final environment.
    pub str_final_env: EnvRef,
}

/// A structure item.
#[derive(Debug, Clone)]
pub struct StructureItem {
    /// Item description.
    pub str_desc: StructureItemDesc,
    /// Item location.
    pub str_loc: Location,
    /// Item environment.
    pub str_env: EnvRef,
}

/// Structure item description.
#[derive(Debug, Clone)]
pub enum StructureItemDesc {
    /// Evaluated expression: `E`.
    Tstr_eval(Expression, Attributes),
    /// Value binding: `let P = E`.
    Tstr_value(RecFlag, Vec<ValueBinding>),
    /// Primitive declaration: `external f : t = "..."`.
    Tstr_primitive(ValueDescription),
    /// Type declaration: `type t = ...`.
    Tstr_type(RecFlag, Vec<TypeDeclaration>),
    /// Type extension: `type t += ...`.
    Tstr_typext(TypeExtension),
    /// Exception declaration: `exception C`.
    Tstr_exception(ExtensionConstructor),
    /// Module binding: `module M = ME`.
    Tstr_module(ModuleBinding),
    /// Recursive modules: `module rec M = ...`.
    Tstr_recmodule(Vec<ModuleBinding>),
    /// Module type declaration: `module type S = MT`.
    Tstr_modtype(ModuleTypeDeclaration),
    /// Open declaration: `open M`.
    Tstr_open(OpenDeclaration),
    /// Include declaration: `include ME`.
    Tstr_include(IncludeDeclaration),
    /// Attribute: `@@attr`.
    Tstr_attribute(crate::parser::ast::Attribute),
}

// Re-export TypeDeclaration from decl for convenience
pub use crate::types::TypeDeclaration;

/// A type extension.
#[derive(Debug, Clone)]
pub struct TypeExtension {
    /// Extended type path.
    pub tyext_path: Path,
    /// Extended type longident.
    pub tyext_txt: Loc<Longident>,
    /// Type parameters.
    pub tyext_params: Vec<(TypedCoreType, Variance)>,
    /// Extension constructors.
    pub tyext_constructors: Vec<ExtensionConstructor>,
    /// Private flag.
    pub tyext_private: PrivateFlag,
    /// Location.
    pub tyext_loc: Location,
    /// Attributes.
    pub tyext_attributes: Attributes,
}

/// Use Variance from crate::types
use crate::types::Variance;

/// A module binding.
#[derive(Debug, Clone)]
pub struct ModuleBinding {
    /// Module identifier.
    pub mb_id: Ident,
    /// Module name with location.
    pub mb_name: Loc<String>,
    /// Module expression.
    pub mb_expr: ModuleExpr,
    /// Module attributes.
    pub mb_attributes: Attributes,
    /// Location.
    pub mb_loc: Location,
}

/// A module type declaration.
#[derive(Debug, Clone)]
pub struct ModuleTypeDeclaration {
    /// Module type identifier.
    pub mtd_id: Ident,
    /// Module type name with location.
    pub mtd_name: Loc<String>,
    /// Module type (None for abstract).
    pub mtd_type: Option<ModuleType>,
    /// Attributes.
    pub mtd_attributes: Attributes,
    /// Location.
    pub mtd_loc: Location,
}

/// An open declaration.
#[derive(Debug, Clone)]
pub struct OpenDeclaration {
    /// Opened module expression.
    pub open_expr: ModuleExpr,
    /// Open override flag.
    pub open_override: OverrideFlag,
    /// Environment after open.
    pub open_env: EnvRef,
    /// Attributes.
    pub open_attributes: Attributes,
    /// Location.
    pub open_loc: Location,
}

/// An include declaration.
#[derive(Debug, Clone)]
pub struct IncludeDeclaration {
    /// Included module expression.
    pub incl_mod: ModuleExpr,
    /// Included module type.
    pub incl_type: Vec<SignatureItem>,
    /// Attributes.
    pub incl_attributes: Attributes,
    /// Location.
    pub incl_loc: Location,
}

/// Signature item (placeholder).
#[derive(Debug, Clone)]
pub struct SignatureItem {
    // Placeholder
    _placeholder: (),
}

/// Extension constructor.
#[derive(Debug, Clone)]
pub struct ExtensionConstructor {
    /// Constructor identifier.
    pub ext_id: Ident,
    /// Constructor name with location.
    pub ext_name: Loc<String>,
    /// Constructor type.
    pub ext_type: crate::types::ExtensionConstructor,
    /// Constructor kind.
    pub ext_kind: ExtensionConstructorKind,
    /// Location.
    pub ext_loc: Location,
    /// Attributes.
    pub ext_attributes: Attributes,
}

/// Extension constructor kind.
#[derive(Debug, Clone)]
pub enum ExtensionConstructorKind {
    /// Declaration with argument types and optional result type.
    Text_decl(Vec<ConstructorArgument>, Option<TypedCoreType>),
    /// Rebind to another constructor.
    Text_rebind(Path, Loc<Longident>),
}

/// Constructor argument in extension declaration.
#[derive(Debug, Clone)]
pub enum ConstructorArgument {
    /// Tuple arguments.
    Cstr_tuple(Vec<TypedCoreType>),
    /// Record arguments.
    Cstr_record(Vec<LabelDeclaration>),
}

/// Label declaration in a constructor record.
#[derive(Debug, Clone)]
pub struct LabelDeclaration {
    /// Label identifier.
    pub ld_id: Ident,
    /// Label name with location.
    pub ld_name: Loc<String>,
    /// Whether the label is mutable.
    pub ld_mutable: MutableFlag,
    /// Label type.
    pub ld_type: TypedCoreType,
    /// Location.
    pub ld_loc: Location,
    /// Attributes.
    pub ld_attributes: Attributes,
}

// ============================================================================
// Environment and Row Reference Types
// ============================================================================

/// Reference to an environment (placeholder).
/// In the full implementation, this would be an index or reference to the Env.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EnvRef(pub usize);

/// Reference to a row description.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RowDescRef(pub usize);

// ============================================================================
// Helper Functions
// ============================================================================

impl Pattern {
    /// Create a new pattern with all fields.
    pub fn new(
        desc: PatternDesc,
        loc: Location,
        ty: TypeExprRef,
        env: EnvRef,
        attributes: Attributes,
    ) -> Self {
        Pattern {
            pat_desc: desc,
            pat_loc: loc,
            pat_extra: Vec::new(),
            pat_type: ty,
            pat_env: env,
            pat_attributes: attributes,
        }
    }

    /// Add extra information to this pattern (constraint, unpack, etc.).
    pub fn with_pat_extra(mut self, extra: PatExtra, loc: Location, attrs: Attributes) -> Self {
        self.pat_extra.push((extra, loc, attrs));
        self
    }
}

impl Expression {
    /// Create a new expression with all fields.
    pub fn new(
        desc: ExpressionDesc,
        loc: Location,
        ty: TypeExprRef,
        env: EnvRef,
        attributes: Attributes,
    ) -> Self {
        Expression {
            exp_desc: desc,
            exp_loc: loc,
            exp_extra: Vec::new(),
            exp_type: ty,
            exp_env: env,
            exp_attributes: attributes,
        }
    }

    /// Add extra information to this expression.
    pub fn with_exp_extra(mut self, extra: ExpExtra, loc: Location, attrs: Attributes) -> Self {
        self.exp_extra.push((extra, loc, attrs));
        self
    }
}

impl Case {
    /// Create a new case with pattern and expression (no guard).
    pub fn simple(lhs: Pattern, rhs: Expression) -> Self {
        Case {
            c_lhs: lhs,
            c_guard: None,
            c_rhs: rhs,
        }
    }
}

impl ValueBinding {
    /// Create a new value binding.
    pub fn new(pat: Pattern, expr: Expression, loc: Location) -> Self {
        ValueBinding {
            vb_pat: pat,
            vb_expr: expr,
            vb_attributes: Vec::new(),
            vb_loc: loc,
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
        assert!(matches!(s, Constant::String(_, None)));
    }

    #[test]
    fn test_partial() {
        assert_ne!(Partial::Partial, Partial::Total);
    }

    #[test]
    fn test_case_simple() {
        // This test just verifies the structure compiles
        // Actual construction requires type context
    }

    #[test]
    fn test_direction_flag() {
        assert_ne!(DirectionFlag::Upto, DirectionFlag::Downto);
    }

    #[test]
    fn test_arity() {
        assert_eq!(Arity::Full(2), Arity::Full(2));
        assert_ne!(Arity::Full(2), Arity::Unknown);
    }
}
