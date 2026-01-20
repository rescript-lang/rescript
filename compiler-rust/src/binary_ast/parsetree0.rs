//! Parsetree0 - Frozen PPX-compatible AST types
//!
//! This module defines the parsetree0 AST types that are used for binary AST
//! serialization. These types are "frozen" to maintain PPX compatibility.
//!
//! The types here mirror `compiler/ml/parsetree0.ml` exactly.

use crate::location::{Located, Location};
use crate::parser::longident::Longident;

// ========== Asttypes (flags and labels) ==========

/// Recursion flag: whether a let binding is recursive
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecFlag {
    Nonrecursive = 0,
    Recursive = 1,
}

/// Direction flag for for-loops
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectionFlag {
    Upto = 0,
    Downto = 1,
}

/// Private flag for type declarations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrivateFlag {
    Private = 0,
    Public = 1,
}

/// Mutability flag for record fields
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MutableFlag {
    Immutable = 0,
    Mutable = 1,
}

/// Virtual flag (unused in ReScript, kept for OCaml compatibility)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VirtualFlag {
    Virtual = 0,
    Concrete = 1,
}

/// Override flag for open statements
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OverrideFlag {
    Override = 0,
    Fresh = 1,
}

/// Closed flag for record patterns and polymorphic variants
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClosedFlag {
    Closed = 0,
    Open = 1,
}

/// Variance for type parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variance {
    Covariant = 0,
    Contravariant = 1,
    Invariant = 2,
}

/// A label (string)
pub type Label = String;

/// Argument label for function parameters (parsetree0 uses Noloc version)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgLabel {
    /// Unlabeled argument
    Nolabel,
    /// Labeled argument (~label)
    Labelled(String),
    /// Optional argument (?label)
    Optional(String),
}

/// Arity annotation for functions (None = inferred, Some(n) = explicit arity)
pub type Arity = Option<i32>;

/// Record element for both patterns and expressions (current parsetree format)
/// Used in Ppat_record and Pexp_record
#[derive(Debug, Clone, PartialEq)]
pub struct RecordElement<T> {
    /// Field name
    pub lid: Located<Longident>,
    /// Field value (pattern or expression)
    pub x: T,
    /// Whether this field is optional
    pub opt: bool,
}

// ========== Constants ==========

/// Constant values in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// Integer constant: 3, 3l, 3L, 3n
    Integer(String, Option<char>),
    /// Character constant: 'c'
    Char(i32),
    /// String constant: "str" or {delim|str|delim}
    String(String, Option<String>),
    /// Float constant: 3.14, 2e5
    Float(String, Option<char>),
}

// ========== Attributes and Extensions ==========

/// An attribute: [@id ARG] or [@@id ARG]
pub type Attribute = (Located<String>, Payload);

/// An extension point: [%id ARG] or [%%id ARG]
pub type Extension = (Located<String>, Payload);

/// A list of attributes
pub type Attributes = Vec<Attribute>;

/// Payload of an attribute or extension
#[derive(Debug, Clone, PartialEq)]
pub enum Payload {
    /// Structure payload
    PStr(Structure),
    /// Signature payload: : SIG
    PSig(Signature),
    /// Type payload: : T
    PTyp(Box<CoreType>),
    /// Pattern payload: ? P or ? P when E
    PPat(Box<Pattern>, Option<Box<Expression>>),
}

// ========== Core Types ==========

/// A type expression
#[derive(Debug, Clone, PartialEq)]
pub struct CoreType {
    pub ptyp_desc: CoreTypeDesc,
    pub ptyp_loc: Location,
    pub ptyp_attributes: Attributes,
}

/// Core type description
#[derive(Debug, Clone, PartialEq)]
pub enum CoreTypeDesc {
    /// Wildcard: _
    Any,
    /// Type variable: 'a
    Var(String),
    /// Arrow type: T1 -> T2
    Arrow(ArgLabel, Box<CoreType>, Box<CoreType>),
    /// Tuple type: T1 * T2 * ... * Tn
    Tuple(Vec<CoreType>),
    /// Type constructor: tconstr, T tconstr, (T1, ..., Tn) tconstr
    Constr(Located<Longident>, Vec<CoreType>),
    /// Object type: < l1:T1; ...; ln:Tn > or < l1:T1; ...; ln:Tn; .. >
    Object(Vec<ObjectField>, ClosedFlag),
    /// Class type (dummy, not used in ReScript)
    Class,
    /// Type alias: T as 'a
    Alias(Box<CoreType>, String),
    /// Polymorphic variant: [ `A | `B ]
    Variant(Vec<RowField>, ClosedFlag, Option<Vec<Label>>),
    /// Polymorphic type: 'a1 ... 'an. T
    Poly(Vec<Located<String>>, Box<CoreType>),
    /// First-class module type: (module S)
    Package(PackageType),
    /// Extension: [%id]
    Extension(Extension),
}

/// Package type: (module S with type t1 = T1 and ... and tn = Tn)
pub type PackageType = (Located<Longident>, Vec<(Located<Longident>, CoreType)>);

/// Row field in a polymorphic variant
#[derive(Debug, Clone, PartialEq)]
pub enum RowField {
    /// Tag: `A, `A of T, etc.
    Rtag(Located<Label>, Attributes, bool, Vec<CoreType>),
    /// Inherited type: [ T ]
    Rinherit(CoreType),
}

/// Object field
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectField {
    /// Named field: l: T
    Otag(Located<Label>, Attributes, CoreType),
    /// Inherited type
    Oinherit(CoreType),
}

// ========== Patterns ==========

/// A pattern
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub ppat_desc: PatternDesc,
    pub ppat_loc: Location,
    pub ppat_attributes: Attributes,
}

/// Pattern description
#[derive(Debug, Clone, PartialEq)]
pub enum PatternDesc {
    /// Wildcard: _
    Any,
    /// Variable: x
    Var(Located<String>),
    /// Alias: P as 'a
    Alias(Box<Pattern>, Located<String>),
    /// Constant: 1, 'a', "str", etc.
    Constant(Constant),
    /// Interval: 'a'..'z'
    Interval(Constant, Constant),
    /// Tuple: (P1, ..., Pn)
    Tuple(Vec<Pattern>),
    /// Constructor: C or C P
    Construct(Located<Longident>, Option<Box<Pattern>>),
    /// Polymorphic variant: `A or `A P
    Variant(Label, Option<Box<Pattern>>),
    /// Record: { l1=P1; ...; ln=Pn } or { l1=P1; ...; ln=Pn; _ }
    /// Uses RecordElement with 3 fields (lid, x, opt) in current parsetree
    Record(Vec<RecordElement<Pattern>>, ClosedFlag),
    /// Array: [| P1; ...; Pn |]
    Array(Vec<Pattern>),
    /// Or pattern: P1 | P2
    Or(Box<Pattern>, Box<Pattern>),
    /// Constraint: (P : T)
    Constraint(Box<Pattern>, Box<CoreType>),
    /// Type pattern: #tconstr
    Type(Located<Longident>),
    /// Lazy pattern: lazy P
    Lazy(Box<Pattern>),
    /// Unpack: (module P)
    Unpack(Located<String>),
    /// Exception: exception P
    Exception(Box<Pattern>),
    /// Extension: [%id]
    Extension(Extension),
    /// Open: M.(P)
    Open(Located<Longident>, Box<Pattern>),
}

// ========== Expressions ==========

/// An expression
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub pexp_desc: ExpressionDesc,
    pub pexp_loc: Location,
    pub pexp_attributes: Attributes,
}

/// Expression description
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionDesc {
    /// Identifier: x, M.x
    Ident(Located<Longident>),
    /// Constant: 1, 'a', "str", etc.
    Constant(Constant),
    /// Let binding: let P = E in E
    Let(RecFlag, Vec<ValueBinding>, Box<Expression>),
    /// Fun: fun P -> E (current parsetree format with arity and async)
    Fun {
        arg_label: ArgLabel,
        default: Option<Box<Expression>>,
        lhs: Box<Pattern>,
        rhs: Box<Expression>,
        arity: Arity,
        is_async: bool,
    },
    /// Application: E0 ~l1:E1 ... ~ln:En (current parsetree format)
    Apply {
        funct: Box<Expression>,
        args: Vec<(ArgLabel, Expression)>,
        partial: bool,
        transformed_jsx: bool,
    },
    /// Match: match E with P1 -> E1 | ... | Pn -> En
    Match(Box<Expression>, Vec<Case>),
    /// Try: try E with P1 -> E1 | ... | Pn -> En
    Try(Box<Expression>, Vec<Case>),
    /// Tuple: (E1, ..., En)
    Tuple(Vec<Expression>),
    /// Constructor: C or C E
    Construct(Located<Longident>, Option<Box<Expression>>),
    /// Polymorphic variant: `A or `A E
    Variant(Label, Option<Box<Expression>>),
    /// Record: { l1=E1; ...; ln=En } or { E with l1=E1; ...; ln=En }
    /// Uses RecordElement with 3 fields (lid, x, opt) in current parsetree
    Record(Vec<RecordElement<Expression>>, Option<Box<Expression>>),
    /// Field access: E.l
    Field(Box<Expression>, Located<Longident>),
    /// Field update: E1.l <- E2
    Setfield(Box<Expression>, Located<Longident>, Box<Expression>),
    /// Array: [| E1; ...; En |]
    Array(Vec<Expression>),
    /// If-then-else: if E1 then E2 else E3
    Ifthenelse(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    /// Sequence: E1; E2
    Sequence(Box<Expression>, Box<Expression>),
    /// While loop: while E1 do E2 done
    While(Box<Expression>, Box<Expression>),
    /// For loop: for i = E1 to/downto E2 do E3 done
    For(Box<Pattern>, Box<Expression>, Box<Expression>, DirectionFlag, Box<Expression>),
    /// Type constraint: (E : T)
    Constraint(Box<Expression>, Box<CoreType>),
    /// Coercion: (E :> T)
    Coerce(Box<Expression>, (), Box<CoreType>),
    /// Method send: E # m
    Send(Box<Expression>, Located<Label>),
    /// New object: new M.c
    New(Located<Longident>),
    /// Instance variable assignment: x <- E
    Setinstvar(Located<Label>, Box<Expression>),
    /// Override: {< x1 = E1; ...; xn = En >}
    Override(Vec<(Located<Label>, Expression)>),
    /// Let module: let module M = ME in E
    Letmodule(Located<String>, Box<ModuleExpr>, Box<Expression>),
    /// Let exception: let exception C in E
    Letexception(ExtensionConstructor, Box<Expression>),
    /// Assert: assert E
    Assert(Box<Expression>),
    /// Lazy: lazy E
    Lazy(Box<Expression>),
    /// Poly: used for method bodies
    Poly(Box<Expression>, Option<Box<CoreType>>),
    /// Object (dummy, not used in ReScript)
    Object,
    /// New type: fun (type t) -> E
    Newtype(Located<String>, Box<Expression>),
    /// Pack: (module ME)
    Pack(Box<ModuleExpr>),
    /// Open: M.(E) or let open M in E
    Open(OverrideFlag, Located<Longident>, Box<Expression>),
    /// Extension: [%id]
    Extension(Extension),
    /// Unreachable: .
    Unreachable,
}

/// A case in a match or function expression
#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub pc_lhs: Pattern,
    pub pc_guard: Option<Expression>,
    pub pc_rhs: Expression,
}

// ========== Value bindings ==========

/// A value binding: let P = E
#[derive(Debug, Clone, PartialEq)]
pub struct ValueBinding {
    pub pvb_pat: Pattern,
    pub pvb_expr: Expression,
    pub pvb_attributes: Attributes,
    pub pvb_loc: Location,
}

/// A value description: val x: T or external x: T = "s1" ... "sn"
#[derive(Debug, Clone, PartialEq)]
pub struct ValueDescription {
    pub pval_name: Located<String>,
    pub pval_type: CoreType,
    pub pval_prim: Vec<String>,
    pub pval_attributes: Attributes,
    pub pval_loc: Location,
}

// ========== Type declarations ==========

/// A type declaration
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDeclaration {
    pub ptype_name: Located<String>,
    pub ptype_params: Vec<(CoreType, Variance)>,
    pub ptype_cstrs: Vec<(CoreType, CoreType, Location)>,
    pub ptype_kind: TypeKind,
    pub ptype_private: PrivateFlag,
    pub ptype_manifest: Option<CoreType>,
    pub ptype_attributes: Attributes,
    pub ptype_loc: Location,
}

/// Type kind
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// Abstract type
    Abstract,
    /// Variant type
    Variant(Vec<ConstructorDeclaration>),
    /// Record type
    Record(Vec<LabelDeclaration>),
    /// Open type (extensible)
    Open,
}

/// A label declaration (record field)
#[derive(Debug, Clone, PartialEq)]
pub struct LabelDeclaration {
    pub pld_name: Located<String>,
    pub pld_mutable: MutableFlag,
    pub pld_type: CoreType,
    pub pld_loc: Location,
    pub pld_attributes: Attributes,
}

/// A constructor declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDeclaration {
    pub pcd_name: Located<String>,
    pub pcd_args: ConstructorArguments,
    pub pcd_res: Option<CoreType>,
    pub pcd_loc: Location,
    pub pcd_attributes: Attributes,
}

/// Constructor arguments
#[derive(Debug, Clone, PartialEq)]
pub enum ConstructorArguments {
    /// Tuple arguments: C of T1 * T2
    Tuple(Vec<CoreType>),
    /// Record arguments: C of { l1: T1; l2: T2 }
    Record(Vec<LabelDeclaration>),
}

// ========== Type extensions ==========

/// A type extension: type t += ...
#[derive(Debug, Clone, PartialEq)]
pub struct TypeExtension {
    pub ptyext_path: Located<Longident>,
    pub ptyext_params: Vec<(CoreType, Variance)>,
    pub ptyext_constructors: Vec<ExtensionConstructor>,
    pub ptyext_private: PrivateFlag,
    pub ptyext_attributes: Attributes,
}

/// An extension constructor
#[derive(Debug, Clone, PartialEq)]
pub struct ExtensionConstructor {
    pub pext_name: Located<String>,
    pub pext_kind: ExtensionConstructorKind,
    pub pext_loc: Location,
    pub pext_attributes: Attributes,
}

/// Extension constructor kind
#[derive(Debug, Clone, PartialEq)]
pub enum ExtensionConstructorKind {
    /// Declaration: | C of T1 * ... * Tn
    Decl(ConstructorArguments, Option<CoreType>),
    /// Rebind: | C = D
    Rebind(Located<Longident>),
}

// ========== Module types ==========

/// A module type
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleType {
    pub pmty_desc: ModuleTypeDesc,
    pub pmty_loc: Location,
    pub pmty_attributes: Attributes,
}

/// Module type description
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleTypeDesc {
    /// Identifier: S
    Ident(Located<Longident>),
    /// Signature: sig ... end
    Signature(Signature),
    /// Functor: functor(X : MT1) -> MT2
    Functor(Located<String>, Option<Box<ModuleType>>, Box<ModuleType>),
    /// With constraint: MT with ...
    With(Box<ModuleType>, Vec<WithConstraint>),
    /// Typeof: module type of ME
    Typeof(Box<ModuleExpr>),
    /// Extension: [%id]
    Extension(Extension),
    /// Alias: (module M)
    Alias(Located<Longident>),
}

/// With constraint
#[derive(Debug, Clone, PartialEq)]
pub enum WithConstraint {
    /// Type constraint: with type X.t = T
    Type(Located<Longident>, TypeDeclaration),
    /// Module constraint: with module X.Y = Z
    Module(Located<Longident>, Located<Longident>),
    /// Type substitution: with type X.t := T
    TypeSubst(Located<Longident>, TypeDeclaration),
    /// Module substitution: with module X.Y := Z
    ModSubst(Located<Longident>, Located<Longident>),
}

// ========== Signatures ==========

/// A signature
pub type Signature = Vec<SignatureItem>;

/// A signature item
#[derive(Debug, Clone, PartialEq)]
pub struct SignatureItem {
    pub psig_desc: SignatureItemDesc,
    pub psig_loc: Location,
}

/// Signature item description
#[derive(Debug, Clone, PartialEq)]
pub enum SignatureItemDesc {
    /// Value: val x: T
    Value(ValueDescription),
    /// Type: type t1 = ... and ... and tn = ...
    Type(RecFlag, Vec<TypeDeclaration>),
    /// Type extension: type t += ...
    Typext(TypeExtension),
    /// Exception: exception C of T
    Exception(ExtensionConstructor),
    /// Module: module X : MT
    Module(ModuleDeclaration),
    /// Recursive modules: module rec X1 : MT1 and ... and Xn : MTn
    Recmodule(Vec<ModuleDeclaration>),
    /// Module type: module type S = MT
    Modtype(ModuleTypeDeclaration),
    /// Open: open X
    Open(OpenDescription),
    /// Include: include MT
    Include(IncludeDescription),
    /// Class (dummy)
    Class,
    /// Class type (dummy)
    ClassType,
    /// Attribute: [@@@id]
    Attribute(Attribute),
    /// Extension: [%%id]
    Extension(Extension, Attributes),
}

/// Module declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDeclaration {
    pub pmd_name: Located<String>,
    pub pmd_type: ModuleType,
    pub pmd_attributes: Attributes,
    pub pmd_loc: Location,
}

/// Module type declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleTypeDeclaration {
    pub pmtd_name: Located<String>,
    pub pmtd_type: Option<ModuleType>,
    pub pmtd_attributes: Attributes,
    pub pmtd_loc: Location,
}

/// Open description
#[derive(Debug, Clone, PartialEq)]
pub struct OpenDescription {
    pub popen_lid: Located<Longident>,
    pub popen_override: OverrideFlag,
    pub popen_loc: Location,
    pub popen_attributes: Attributes,
}

/// Include infos (generic)
#[derive(Debug, Clone, PartialEq)]
pub struct IncludeInfos<A> {
    pub pincl_mod: A,
    pub pincl_loc: Location,
    pub pincl_attributes: Attributes,
}

/// Include description: include MT
pub type IncludeDescription = IncludeInfos<ModuleType>;

/// Include declaration: include ME
pub type IncludeDeclaration = IncludeInfos<ModuleExpr>;

// ========== Module expressions ==========

/// A module expression
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleExpr {
    pub pmod_desc: ModuleExprDesc,
    pub pmod_loc: Location,
    pub pmod_attributes: Attributes,
}

/// Module expression description
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleExprDesc {
    /// Identifier: X
    Ident(Located<Longident>),
    /// Structure: struct ... end
    Structure(Structure),
    /// Functor: functor(X : MT1) -> ME
    Functor(Located<String>, Option<Box<ModuleType>>, Box<ModuleExpr>),
    /// Application: ME1(ME2)
    Apply(Box<ModuleExpr>, Box<ModuleExpr>),
    /// Constraint: (ME : MT)
    Constraint(Box<ModuleExpr>, Box<ModuleType>),
    /// Unpack: (val E)
    Unpack(Box<Expression>),
    /// Extension: [%id]
    Extension(Extension),
}

// ========== Structures ==========

/// A structure
pub type Structure = Vec<StructureItem>;

/// A structure item
#[derive(Debug, Clone, PartialEq)]
pub struct StructureItem {
    pub pstr_desc: StructureItemDesc,
    pub pstr_loc: Location,
}

/// Structure item description
#[derive(Debug, Clone, PartialEq)]
pub enum StructureItemDesc {
    /// Evaluation: E
    Eval(Expression, Attributes),
    /// Value: let P1 = E1 and ... and Pn = En
    Value(RecFlag, Vec<ValueBinding>),
    /// Primitive: val x: T or external x: T = "s1" ... "sn"
    Primitive(ValueDescription),
    /// Type: type t1 = ... and ... and tn = ...
    Type(RecFlag, Vec<TypeDeclaration>),
    /// Type extension: type t += ...
    Typext(TypeExtension),
    /// Exception: exception C of T
    Exception(ExtensionConstructor),
    /// Module: module X = ME
    Module(ModuleBinding),
    /// Recursive modules: module rec X1 = ME1 and ... and Xn = MEn
    Recmodule(Vec<ModuleBinding>),
    /// Module type: module type S = MT
    Modtype(ModuleTypeDeclaration),
    /// Open: open X
    Open(OpenDescription),
    /// Class (dummy)
    Class,
    /// Class type (dummy)
    ClassType,
    /// Include: include ME
    Include(IncludeDeclaration),
    /// Attribute: [@@@id]
    Attribute(Attribute),
    /// Extension: [%%id]
    Extension(Extension, Attributes),
}

/// A module binding: X = ME
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleBinding {
    pub pmb_name: Located<String>,
    pub pmb_expr: ModuleExpr,
    pub pmb_attributes: Attributes,
    pub pmb_loc: Location,
}
