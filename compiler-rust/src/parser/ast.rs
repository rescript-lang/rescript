//! Abstract Syntax Tree types for ReScript.
//!
//! This module defines the AST produced by the parser, mirroring
//! OCaml's `Parsetree` with ReScript-specific extensions.
//!
//! Note: We use `Pexp_`, `Ppat_`, etc. naming convention to match
//! OCaml's Parsetree for easier cross-referencing.

#![allow(non_camel_case_types)]
#![allow(missing_docs)]

use crate::location::Position;
use crate::parse_arena::{Located, LocIdx};
use serde::{Deserialize, Serialize};

use super::longident::Longident;

/// Re-export Located for convenience.
/// Note: Located<T> uses LocIdx for efficient arena-based storage.
pub type Loc<T> = Located<T>;

/// Location type alias - uses arena index for efficient storage.
pub type Location = LocIdx;

/// A string with location information.
pub type StringLoc = Located<String>;

/// Arity information for functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum Arity {
    /// Full arity (all arguments known).
    Full(usize),
    /// Unknown arity.
    #[default]
    Unknown,
}

/// Recursive flag for let bindings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RecFlag {
    /// Non-recursive binding.
    Nonrecursive,
    /// Recursive binding.
    Recursive,
}

/// Direction flag for for-loops.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DirectionFlag {
    /// Counting upward.
    Upto,
    /// Counting downward.
    Downto,
}

/// Closed flag for record patterns and types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ClosedFlag {
    /// Closed (exact match).
    Closed,
    /// Open (allows extra fields).
    Open,
}

/// Override flag for open statements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OverrideFlag {
    /// Normal open.
    Fresh,
    /// Override open.
    Override,
}

/// Mutable flag for record fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MutableFlag {
    /// Immutable field.
    Immutable,
    /// Mutable field.
    Mutable,
}

/// Private flag for type definitions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PrivateFlag {
    /// Public type.
    Public,
    /// Private type.
    Private,
}

/// Variance for type parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Variance {
    /// Covariant (+).
    Covariant,
    /// Contravariant (-).
    Contravariant,
    /// Invariant (no annotation).
    Invariant,
}

/// Argument label for function parameters.
/// Note: Labelled and Optional include a location to match OCaml's `Labelled of string loc`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ArgLabel {
    /// No label.
    Nolabel,
    /// Labeled argument: `~foo` with location.
    Labelled(Located<String>),
    /// Optional argument: `?foo` with location.
    Optional(Located<String>),
}

impl ArgLabel {
    /// Check if this is an unlabeled argument.
    pub fn is_nolabel(&self) -> bool {
        matches!(self, ArgLabel::Nolabel)
    }

    /// Check if this is an optional argument.
    pub fn is_optional(&self) -> bool {
        matches!(self, ArgLabel::Optional(_))
    }

    /// Get the label name, if any.
    pub fn name(&self) -> Option<&str> {
        match self {
            ArgLabel::Nolabel => None,
            ArgLabel::Labelled(s) | ArgLabel::Optional(s) => Some(&s.txt),
        }
    }

    /// Get the located string, if any.
    pub fn located(&self) -> Option<&Located<String>> {
        match self {
            ArgLabel::Nolabel => None,
            ArgLabel::Labelled(s) | ArgLabel::Optional(s) => Some(s),
        }
    }
}

/// A label (used for variant constructors, object methods, etc.).
pub type Label = String;

// ============================================================================
// Constants
// ============================================================================

/// Constant values.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Constant {
    /// Integer constant with optional suffix (l, L, n).
    Integer(String, Option<char>),
    /// Character constant (Unicode codepoint).
    Char(i32),
    /// String constant with optional delimiter.
    String(String, Option<String>),
    /// Float constant with optional suffix.
    Float(String, Option<char>),
}

// ============================================================================
// Attributes and Extensions
// ============================================================================

/// An attribute: `[@id payload]`.
pub type Attribute = (StringLoc, Payload);

/// A list of attributes.
pub type Attributes = Vec<Attribute>;

/// An extension point: `[%id payload]`.
pub type Extension = (StringLoc, Payload);

/// Payload of an attribute or extension.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Payload {
    /// Structure payload: `[@@attr structure]`.
    PStr(Vec<StructureItem>),
    /// Signature payload: `[@@attr: signature]`.
    PSig(Vec<SignatureItem>),
    /// Type payload: `[@@attr: type]`.
    PTyp(Box<CoreType>),
    /// Pattern payload with optional guard: `[@@attr? pattern when guard]`.
    PPat(Box<Pattern>, Option<Box<Expression>>),
}

// ============================================================================
// Core Types
// ============================================================================

/// A core type (type expression).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CoreType {
    /// Type description.
    pub ptyp_desc: CoreTypeDesc,
    /// Location.
    pub ptyp_loc: Location,
    /// Attributes.
    pub ptyp_attributes: Attributes,
}

/// Argument in an arrow type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeArg {
    /// Attributes on the argument.
    pub attrs: Attributes,
    /// Argument label.
    pub lbl: ArgLabel,
    /// Argument type.
    pub typ: CoreType,
}

/// Core type descriptions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CoreTypeDesc {
    /// Any type: `_`.
    Ptyp_any,
    /// Type variable: `'a`.
    Ptyp_var(String),
    /// Arrow type: `T1 -> T2`.
    Ptyp_arrow {
        arg: Box<TypeArg>,
        ret: Box<CoreType>,
        arity: Arity,
    },
    /// Tuple type: `(T1, T2, ..., Tn)`.
    Ptyp_tuple(Vec<CoreType>),
    /// Type constructor: `tconstr` or `T tconstr` or `(T1, ..., Tn) tconstr`.
    Ptyp_constr(Loc<Longident>, Vec<CoreType>),
    /// Object type: `{. field: T, ... }`.
    Ptyp_object(Vec<ObjectField>, ClosedFlag),
    /// Type alias: `T as 'a`.
    Ptyp_alias(Box<CoreType>, String),
    /// Polymorphic variant: `[> \`A | \`B ]`.
    Ptyp_variant(Vec<RowField>, ClosedFlag, Option<Vec<Label>>),
    /// Polymorphic type: `'a 'b. T`.
    Ptyp_poly(Vec<StringLoc>, Box<CoreType>),
    /// Package type: `module S`.
    Ptyp_package(PackageType),
    /// Extension: `[%id]`.
    Ptyp_extension(Extension),
}

/// Package type: `module S with type t = T`.
pub type PackageType = (Loc<Longident>, Vec<(Loc<Longident>, CoreType)>);

/// Row field in a polymorphic variant.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RowField {
    /// Tag: `\`A of T1 & T2`.
    Rtag(Loc<Label>, Attributes, bool, Vec<CoreType>),
    /// Inherited type: `[T]`.
    Rinherit(CoreType),
}

/// Object field.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ObjectField {
    /// Named field: `name: T`.
    Otag(Loc<Label>, Attributes, CoreType),
    /// Inherited type.
    Oinherit(CoreType),
}

// ============================================================================
// Patterns
// ============================================================================

/// A pattern.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Pattern {
    /// Pattern description.
    pub ppat_desc: PatternDesc,
    /// Location.
    pub ppat_loc: Location,
    /// Attributes.
    pub ppat_attributes: Attributes,
}

/// Pattern descriptions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PatternDesc {
    /// Wildcard: `_`.
    Ppat_any,
    /// Variable: `x`.
    Ppat_var(StringLoc),
    /// Alias: `P as x`.
    Ppat_alias(Box<Pattern>, StringLoc),
    /// Constant: `1`, `'a'`, `"hello"`.
    Ppat_constant(Constant),
    /// Interval: `'a'..'z'`.
    Ppat_interval(Constant, Constant),
    /// Tuple: `(P1, P2, ..., Pn)`.
    Ppat_tuple(Vec<Pattern>),
    /// Constructor: `C` or `C(P)`.
    Ppat_construct(Loc<Longident>, Option<Box<Pattern>>),
    /// Variant: `\`A` or `\`A(P)`.
    Ppat_variant(Label, Option<Box<Pattern>>),
    /// Record: `{l1: P1, ..., ln: Pn}`.
    Ppat_record(Vec<PatternRecordField>, ClosedFlag),
    /// Array: `[P1, ..., Pn]`.
    Ppat_array(Vec<Pattern>),
    /// Or-pattern: `P1 | P2`.
    Ppat_or(Box<Pattern>, Box<Pattern>),
    /// Constraint: `(P: T)`.
    Ppat_constraint(Box<Pattern>, CoreType),
    /// Type pattern: `#tconstr`.
    Ppat_type(Loc<Longident>),
    /// Unpack: `module(P)`.
    Ppat_unpack(StringLoc),
    /// Exception: `exception P`.
    Ppat_exception(Box<Pattern>),
    /// Extension: `[%id]`.
    Ppat_extension(Extension),
    /// Open: `M.(P)`.
    Ppat_open(Loc<Longident>, Box<Pattern>),
}

/// A record field in a pattern.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PatternRecordField {
    /// Field name.
    pub lid: Loc<Longident>,
    /// Pattern.
    pub pat: Pattern,
    /// Whether this is optional.
    pub opt: bool,
}

// ============================================================================
// Expressions
// ============================================================================

/// An expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Expression {
    /// Expression description.
    pub pexp_desc: ExpressionDesc,
    /// Location.
    pub pexp_loc: Location,
    /// Attributes.
    pub pexp_attributes: Attributes,
}

/// Expression descriptions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExpressionDesc {
    /// Identifier: `x` or `M.x`.
    Pexp_ident(Loc<Longident>),
    /// Constant: `1`, `'a'`, `"hello"`.
    Pexp_constant(Constant),
    /// Let binding: `let P = E in E'`.
    Pexp_let(RecFlag, Vec<ValueBinding>, Box<Expression>),
    /// Function: `(P) => E`.
    Pexp_fun {
        arg_label: ArgLabel,
        default: Option<Box<Expression>>,
        lhs: Pattern,
        rhs: Box<Expression>,
        arity: Arity,
        is_async: bool,
    },
    /// Application: `E(E1, E2, ...)`.
    Pexp_apply {
        funct: Box<Expression>,
        args: Vec<(ArgLabel, Expression)>,
        partial: bool,
        transformed_jsx: bool,
    },
    /// Match: `switch E { | P1 => E1 | ... }`.
    Pexp_match(Box<Expression>, Vec<Case>),
    /// Try: `try E { | P1 => E1 | ... }`.
    Pexp_try(Box<Expression>, Vec<Case>),
    /// Tuple: `(E1, E2, ..., En)`.
    Pexp_tuple(Vec<Expression>),
    /// Constructor: `C` or `C(E)`.
    Pexp_construct(Loc<Longident>, Option<Box<Expression>>),
    /// Variant: `\`A` or `\`A(E)`.
    Pexp_variant(Label, Option<Box<Expression>>),
    /// Record: `{l1: E1, ..., ln: En}` or `{...E, l1: E1, ...}`.
    Pexp_record(Vec<ExpressionRecordField>, Option<Box<Expression>>),
    /// Field access: `E.l`.
    Pexp_field(Box<Expression>, Loc<Longident>),
    /// Field set: `E.l = E'`.
    Pexp_setfield(Box<Expression>, Loc<Longident>, Box<Expression>),
    /// Array: `[E1, ..., En]`.
    Pexp_array(Vec<Expression>),
    /// If-then-else: `if E1 { E2 } else { E3 }`.
    Pexp_ifthenelse(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    /// Sequence: `E1; E2`.
    Pexp_sequence(Box<Expression>, Box<Expression>),
    /// While: `while E1 { E2 }`.
    Pexp_while(Box<Expression>, Box<Expression>),
    /// For: `for i in E1 to E2 { E3 }`.
    Pexp_for(
        Pattern,
        Box<Expression>,
        Box<Expression>,
        DirectionFlag,
        Box<Expression>,
    ),
    /// Constraint: `(E: T)`.
    Pexp_constraint(Box<Expression>, CoreType),
    /// Coercion: `(E :> T)`.
    Pexp_coerce(Box<Expression>, Option<CoreType>, CoreType),
    /// Send: `E#m`.
    Pexp_send(Box<Expression>, Loc<Label>),
    /// Let module: `{ module M = ME; E }`.
    Pexp_letmodule(StringLoc, ModuleExpr, Box<Expression>),
    /// Let exception: `{ exception C; E }`.
    Pexp_letexception(ExtensionConstructor, Box<Expression>),
    /// Assert: `assert(E)`.
    Pexp_assert(Box<Expression>),
    /// Newtype: `(type t) => E`.
    Pexp_newtype(StringLoc, Box<Expression>),
    /// Pack: `module(ME)`.
    Pexp_pack(ModuleExpr),
    /// Open: `M.{ E }` or `open M; E`.
    Pexp_open(OverrideFlag, Loc<Longident>, Box<Expression>),
    /// Extension: `[%id]`.
    Pexp_extension(Extension),
    /// Await: `await E`.
    Pexp_await(Box<Expression>),
    /// JSX element.
    Pexp_jsx_element(JsxElement),
}

/// A record field in an expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExpressionRecordField {
    /// Field name.
    pub lid: Loc<Longident>,
    /// Expression.
    pub expr: Expression,
    /// Whether this is optional.
    pub opt: bool,
}

/// A case in a match or try expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Case {
    /// Position of the `|` bar.
    pub pc_bar: Option<Position>,
    /// Left-hand side pattern.
    pub pc_lhs: Pattern,
    /// Optional guard expression.
    pub pc_guard: Option<Expression>,
    /// Right-hand side expression.
    pub pc_rhs: Expression,
}

/// A value binding: `let P = E`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueBinding {
    /// Pattern.
    pub pvb_pat: Pattern,
    /// Expression.
    pub pvb_expr: Expression,
    /// Attributes.
    pub pvb_attributes: Attributes,
    /// Location.
    pub pvb_loc: Location,
}

// ============================================================================
// JSX
// ============================================================================

/// A JSX element.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[allow(clippy::large_enum_variant)]
pub enum JsxElement {
    /// Fragment: `<> ... </>`.
    Fragment(JsxFragment),
    /// Unary element: `<div />`.
    Unary(JsxUnaryElement),
    /// Container element: `<div> ... </div>`.
    Container(JsxContainerElement),
}

/// JSX tag name.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum JsxTagName {
    /// Lowercase tag: `div`.
    Lower(String),
    /// Qualified lowercase: `Mod.div`.
    QualifiedLower { path: Longident, name: String },
    /// Uppercase tag (component): `Button`.
    Upper(Longident),
    /// Invalid tag name (for error recovery).
    Invalid(String),
}

/// JSX fragment: `<> children </>`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JsxFragment {
    /// Opening `>` position.
    pub opening: Position,
    /// Children.
    pub children: Vec<Expression>,
    /// Closing `</` position.
    pub closing: Position,
}

/// JSX unary element: `<div ... />`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JsxUnaryElement {
    /// Tag name.
    pub tag_name: Loc<JsxTagName>,
    /// Props.
    pub props: Vec<JsxProp>,
}

/// JSX container element: `<div ...> children </div>`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JsxContainerElement {
    /// Opening tag name.
    pub tag_name_start: Loc<JsxTagName>,
    /// Opening `>` position.
    pub opening_end: Position,
    /// Props.
    pub props: Vec<JsxProp>,
    /// Children.
    pub children: Vec<Expression>,
    /// Closing tag.
    pub closing_tag: Option<JsxClosingTag>,
}

/// JSX closing tag: `</div>`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JsxClosingTag {
    /// `</` position.
    pub start: Position,
    /// Tag name.
    pub name: Loc<JsxTagName>,
    /// `>` position.
    pub end: Position,
}

/// JSX prop.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum JsxProp {
    /// Punning: `disabled` or `?disabled`.
    Punning { optional: bool, name: StringLoc },
    /// Value: `onClick={handler}` or `key=?{maybeKey}`.
    Value {
        name: StringLoc,
        optional: bool,
        value: Expression,
    },
    /// Spreading: `{...props}`.
    Spreading { loc: Location, expr: Expression },
}

// ============================================================================
// Modules
// ============================================================================

/// A module expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleExpr {
    /// Module expression description.
    pub pmod_desc: ModuleExprDesc,
    /// Location.
    pub pmod_loc: Location,
    /// Attributes.
    pub pmod_attributes: Attributes,
}

/// Module expression descriptions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ModuleExprDesc {
    /// Module identifier: `M`.
    Pmod_ident(Loc<Longident>),
    /// Module structure: `{ ... }`.
    Pmod_structure(Vec<StructureItem>),
    /// Functor: `(X: S) => ME`.
    Pmod_functor(StringLoc, Option<Box<ModuleType>>, Box<ModuleExpr>),
    /// Functor application: `F(M)`.
    Pmod_apply(Box<ModuleExpr>, Box<ModuleExpr>),
    /// Constraint: `(ME: MT)`.
    Pmod_constraint(Box<ModuleExpr>, Box<ModuleType>),
    /// Unpack: `unpack(E)`.
    Pmod_unpack(Box<Expression>),
    /// Extension: `[%id]`.
    Pmod_extension(Extension),
}

/// A module type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleType {
    /// Module type description.
    pub pmty_desc: ModuleTypeDesc,
    /// Location.
    pub pmty_loc: Location,
    /// Attributes.
    pub pmty_attributes: Attributes,
}

/// Module type descriptions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ModuleTypeDesc {
    /// Module type identifier: `S`.
    Pmty_ident(Loc<Longident>),
    /// Signature: `{ ... }`.
    Pmty_signature(Vec<SignatureItem>),
    /// Functor type: `(X: S) => MT`.
    Pmty_functor(StringLoc, Option<Box<ModuleType>>, Box<ModuleType>),
    /// With constraint: `MT with type t = T`.
    Pmty_with(Box<ModuleType>, Vec<WithConstraint>),
    /// Type of: `module type of ME`.
    Pmty_typeof(Box<ModuleExpr>),
    /// Extension: `[%id]`.
    Pmty_extension(Extension),
    /// Alias: `module M`.
    Pmty_alias(Loc<Longident>),
}

/// With constraint.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum WithConstraint {
    /// Type constraint: `type t = T`.
    Pwith_type(Loc<Longident>, TypeDeclaration),
    /// Module constraint: `module M = M'`.
    Pwith_module(Loc<Longident>, Loc<Longident>),
    /// Type substitution: `type t := T`.
    Pwith_typesubst(Loc<Longident>, TypeDeclaration),
    /// Module substitution: `module M := M'`.
    Pwith_modsubst(Loc<Longident>, Loc<Longident>),
}

// ============================================================================
// Structure and Signature Items
// ============================================================================

/// A structure item.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StructureItem {
    /// Structure item description.
    pub pstr_desc: StructureItemDesc,
    /// Location.
    pub pstr_loc: Location,
}

/// Structure item descriptions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StructureItemDesc {
    /// Expression: `E`.
    Pstr_eval(Expression, Attributes),
    /// Value: `let P = E`.
    Pstr_value(RecFlag, Vec<ValueBinding>),
    /// Primitive declaration: `external`.
    Pstr_primitive(ValueDescription),
    /// Type declaration: `type t = ...`.
    Pstr_type(RecFlag, Vec<TypeDeclaration>),
    /// Type extension: `type t += ...`.
    Pstr_typext(TypeExtension),
    /// Exception: `exception C`.
    Pstr_exception(ExtensionConstructor),
    /// Module: `module M = ...`.
    Pstr_module(ModuleBinding),
    /// Recursive modules: `module rec M = ...`.
    Pstr_recmodule(Vec<ModuleBinding>),
    /// Module type: `module type S = ...`.
    Pstr_modtype(ModuleTypeDeclaration),
    /// Open: `open M`.
    Pstr_open(OpenDescription),
    /// Include: `include M`.
    Pstr_include(IncludeDeclaration),
    /// Attribute: `@@attr`.
    Pstr_attribute(Attribute),
    /// Extension: `[%%id]`.
    Pstr_extension(Extension, Attributes),
}

/// A signature item.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SignatureItem {
    /// Signature item description.
    pub psig_desc: SignatureItemDesc,
    /// Location.
    pub psig_loc: Location,
}

/// Signature item descriptions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SignatureItemDesc {
    /// Value: `let x: T`.
    Psig_value(ValueDescription),
    /// Type declaration: `type t = ...`.
    Psig_type(RecFlag, Vec<TypeDeclaration>),
    /// Type extension: `type t += ...`.
    Psig_typext(TypeExtension),
    /// Exception: `exception C`.
    Psig_exception(ExtensionConstructor),
    /// Module: `module M: S`.
    Psig_module(ModuleDeclaration),
    /// Recursive modules: `module rec M: S`.
    Psig_recmodule(Vec<ModuleDeclaration>),
    /// Module type: `module type S = ...`.
    Psig_modtype(ModuleTypeDeclaration),
    /// Open: `open M`.
    Psig_open(OpenDescription),
    /// Include: `include S`.
    Psig_include(IncludeDescription),
    /// Attribute: `@@attr`.
    Psig_attribute(Attribute),
    /// Extension: `[%%id]`.
    Psig_extension(Extension, Attributes),
}

// ============================================================================
// Declarations
// ============================================================================

/// Value description: `external` or `let` in signature.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueDescription {
    /// Name.
    pub pval_name: StringLoc,
    /// Type.
    pub pval_type: CoreType,
    /// Primitive names (for externals).
    pub pval_prim: Vec<String>,
    /// Attributes.
    pub pval_attributes: Attributes,
    /// Location.
    pub pval_loc: Location,
}

/// Type declaration: `type t = ...`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeDeclaration {
    /// Name.
    pub ptype_name: StringLoc,
    /// Type parameters.
    pub ptype_params: Vec<(CoreType, Variance)>,
    /// Constraints.
    pub ptype_cstrs: Vec<(CoreType, CoreType, Location)>,
    /// Kind.
    pub ptype_kind: TypeKind,
    /// Private flag.
    pub ptype_private: PrivateFlag,
    /// Manifest.
    pub ptype_manifest: Option<CoreType>,
    /// Attributes.
    pub ptype_attributes: Attributes,
    /// Location.
    pub ptype_loc: Location,
}

/// Type kind.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeKind {
    /// Abstract type.
    Ptype_abstract,
    /// Variant type.
    Ptype_variant(Vec<ConstructorDeclaration>),
    /// Record type.
    Ptype_record(Vec<LabelDeclaration>),
    /// Open type.
    Ptype_open,
}

/// Constructor declaration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstructorDeclaration {
    /// Name.
    pub pcd_name: StringLoc,
    /// Arguments.
    pub pcd_args: ConstructorArguments,
    /// Return type.
    pub pcd_res: Option<CoreType>,
    /// Location.
    pub pcd_loc: Location,
    /// Attributes.
    pub pcd_attributes: Attributes,
}

/// Constructor arguments.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConstructorArguments {
    /// Tuple arguments: `C(T1, T2)`.
    Pcstr_tuple(Vec<CoreType>),
    /// Record arguments: `C({field: T})`.
    Pcstr_record(Vec<LabelDeclaration>),
}

/// Label declaration (record field).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LabelDeclaration {
    /// Name.
    pub pld_name: StringLoc,
    /// Mutable flag.
    pub pld_mutable: MutableFlag,
    /// Type.
    pub pld_type: CoreType,
    /// Location.
    pub pld_loc: Location,
    /// Attributes.
    pub pld_attributes: Attributes,
    /// Optional flag.
    pub pld_optional: bool,
}

/// Type extension: `type t += ...`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeExtension {
    /// Extended type.
    pub ptyext_path: Loc<Longident>,
    /// Type parameters.
    pub ptyext_params: Vec<(CoreType, Variance)>,
    /// Constructors.
    pub ptyext_constructors: Vec<ExtensionConstructor>,
    /// Private flag.
    pub ptyext_private: PrivateFlag,
    /// Attributes.
    pub ptyext_attributes: Attributes,
}

/// Extension constructor.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExtensionConstructor {
    /// Name.
    pub pext_name: StringLoc,
    /// Kind.
    pub pext_kind: ExtensionConstructorKind,
    /// Location.
    pub pext_loc: Location,
    /// Attributes.
    pub pext_attributes: Attributes,
}

/// Extension constructor kind.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExtensionConstructorKind {
    /// Declaration: `C of T`.
    Pext_decl(ConstructorArguments, Option<CoreType>),
    /// Rebind: `C = D`.
    Pext_rebind(Loc<Longident>),
}

/// Module binding: `module M = ME`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleBinding {
    /// Name.
    pub pmb_name: StringLoc,
    /// Expression.
    pub pmb_expr: ModuleExpr,
    /// Attributes.
    pub pmb_attributes: Attributes,
    /// Location.
    pub pmb_loc: Location,
}

/// Module declaration: `module M: S`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleDeclaration {
    /// Name.
    pub pmd_name: StringLoc,
    /// Type.
    pub pmd_type: ModuleType,
    /// Attributes.
    pub pmd_attributes: Attributes,
    /// Location.
    pub pmd_loc: Location,
}

/// Module type declaration: `module type S = MT`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleTypeDeclaration {
    /// Name.
    pub pmtd_name: StringLoc,
    /// Type.
    pub pmtd_type: Option<ModuleType>,
    /// Attributes.
    pub pmtd_attributes: Attributes,
    /// Location.
    pub pmtd_loc: Location,
}

/// Open description: `open M`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OpenDescription {
    /// Opened module.
    pub popen_lid: Loc<Longident>,
    /// Override flag.
    pub popen_override: OverrideFlag,
    /// Location.
    pub popen_loc: Location,
    /// Attributes.
    pub popen_attributes: Attributes,
}

/// Include declaration: `include ME`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IncludeDeclaration {
    /// Included module.
    pub pincl_mod: ModuleExpr,
    /// Location.
    pub pincl_loc: Location,
    /// Attributes.
    pub pincl_attributes: Attributes,
}

/// Include description: `include S`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IncludeDescription {
    /// Included module type.
    pub pincl_mod: ModuleType,
    /// Location.
    pub pincl_loc: Location,
    /// Attributes.
    pub pincl_attributes: Attributes,
}

// ============================================================================
// Top-level structure
// ============================================================================

/// A complete structure (module implementation).
pub type Structure = Vec<StructureItem>;

/// A complete signature (module interface).
pub type Signature = Vec<SignatureItem>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arg_label() {
        assert!(ArgLabel::Nolabel.is_nolabel());
        assert!(!ArgLabel::Labelled(Located::mknoloc("x".to_string())).is_nolabel());
        assert!(ArgLabel::Optional(Located::mknoloc("x".to_string())).is_optional());
        assert_eq!(ArgLabel::Labelled(Located::mknoloc("x".to_string())).name(), Some("x"));
    }

    #[test]
    fn test_constant() {
        let c = Constant::Integer("42".to_string(), None);
        assert!(matches!(c, Constant::Integer(..)));

        let c = Constant::String("hello".to_string(), None);
        assert!(matches!(c, Constant::String(..)));
    }

    #[test]
    fn test_types_are_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Constant>();
        assert_send_sync::<ArgLabel>();
        assert_send_sync::<RecFlag>();
        assert_send_sync::<ClosedFlag>();
    }
}
