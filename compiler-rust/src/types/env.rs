//! Type environment.
//!
//! This module provides `Env`, the typing environment that tracks all
//! bindings during type checking. The environment is immutable and uses
//! a persistent structure for efficient scope management.
//!
//! # Design
//!
//! The environment is modeled as a linked list of bindings, allowing
//! efficient shadowing and scope nesting. Each binding type (values,
//! types, modules) is tracked separately for fast lookup.
//!
//! # Key Components
//!
//! - `Env` - The main environment type
//! - `EnvSummary` - A summary of bindings for debugging
//! - Lookup functions for values, types, and modules

use super::context::TypeContext;
use super::asttypes::PrivateFlag;
use super::decl::{
    ConstructorDescription, ConstructorTag, LabelDescription, ModtypeDeclaration,
    ModuleDeclaration, PrimitiveDescription, TypeDeclaration, ValueDescription, ValueKind,
};
use super::path::Path;
use super::type_expr::TypeExprRef;
use crate::ident::Ident;
use crate::location::Location;
use std::collections::HashMap;
use std::sync::Arc;

// ============================================================================
// Environment Errors
// ============================================================================

/// Errors that can occur during environment operations.
#[derive(Debug, Clone)]
pub enum EnvError {
    /// Value not found.
    ValueNotFound(String),
    /// Type not found.
    TypeNotFound(String),
    /// Module not found.
    ModuleNotFound(String),
    /// Constructor not found.
    ConstructorNotFound(String),
    /// Label not found.
    LabelNotFound(String),
    /// Inconsistent import.
    InconsistentImport {
        name: String,
        source1: String,
        source2: String,
    },
    /// Illegal value name.
    IllegalValueName(Location, String),
}

/// Result type for environment operations.
pub type EnvResult<T> = Result<T, EnvError>;

// ============================================================================
// Environment Summary
// ============================================================================

/// Summary of environment bindings.
///
/// This is used for debugging and for computing the environment
/// delta when creating module interfaces.
#[derive(Debug, Clone)]
pub enum EnvSummary {
    /// Empty environment.
    Empty,
    /// Value binding.
    Value(Arc<EnvSummary>, Ident, Arc<ValueDescription>),
    /// Type binding.
    Type(Arc<EnvSummary>, Ident, Arc<TypeDeclaration>),
    /// Module binding.
    Module(Arc<EnvSummary>, Ident, Arc<ModuleDeclaration>),
    /// Module type binding.
    ModType(Arc<EnvSummary>, Ident, Arc<ModtypeDeclaration>),
    /// Open a module.
    Open(Arc<EnvSummary>, Path),
    /// Functor argument.
    FunctorArg(Arc<EnvSummary>, Ident),
}

// ============================================================================
// Binding Tables
// ============================================================================

/// A table of bindings indexed by identifier.
#[derive(Debug, Clone)]
pub struct BindingTable<T> {
    /// Bindings indexed by name.
    by_name: HashMap<String, Vec<(Ident, T)>>,
    /// Bindings indexed by identifier stamp.
    by_stamp: HashMap<i32, T>,
}

impl<T: Clone> BindingTable<T> {
    /// Create a new empty binding table.
    pub fn new() -> Self {
        BindingTable {
            by_name: HashMap::new(),
            by_stamp: HashMap::new(),
        }
    }

    /// Add a binding.
    pub fn add(&mut self, id: Ident, value: T) {
        let name = id.name().to_string();
        let stamp = id.stamp();

        self.by_stamp.insert(stamp, value.clone());

        self.by_name.entry(name).or_default().push((id, value));
    }

    /// Find a binding by name.
    pub fn find_by_name(&self, name: &str) -> Option<&T> {
        self.by_name
            .get(name)
            .and_then(|entries| entries.last())
            .map(|(_, v)| v)
    }

    /// Find a binding by name, returning both the identifier and value.
    pub fn find_by_name_with_id(&self, name: &str) -> Option<(&Ident, &T)> {
        self.by_name
            .get(name)
            .and_then(|entries| entries.last())
            .map(|(id, v)| (id, v))
    }

    /// Find a binding by identifier.
    pub fn find_by_ident(&self, id: &Ident) -> Option<&T> {
        self.by_stamp.get(&id.stamp())
    }

    /// Get all bindings with a given name.
    pub fn find_all(&self, name: &str) -> Vec<&T> {
        self.by_name
            .get(name)
            .map(|entries| entries.iter().map(|(_, v)| v).collect())
            .unwrap_or_default()
    }

    /// Check if a binding exists.
    pub fn contains(&self, name: &str) -> bool {
        self.by_name.contains_key(name)
    }

    /// Iterate over all bindings.
    pub fn iter(&self) -> impl Iterator<Item = (&Ident, &T)> {
        self.by_name
            .values()
            .flat_map(|entries| entries.iter().map(|(id, v)| (id, v)))
    }
}

impl<T: Clone> Default for BindingTable<T> {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// The Environment
// ============================================================================

/// The typing environment.
///
/// This tracks all bindings visible during type checking.
/// The environment is immutable - extending it creates a new environment.
#[derive(Debug, Clone)]
pub struct Env {
    /// Value bindings.
    values: BindingTable<Arc<ValueDescription>>,

    /// Type bindings.
    types: BindingTable<Arc<TypeDeclaration>>,

    /// Constructor bindings (for variant types).
    constrs: BindingTable<Arc<ConstructorDescription>>,

    /// Label bindings (for record fields).
    labels: BindingTable<Arc<LabelDescription>>,

    /// Module bindings.
    modules: BindingTable<Arc<ModuleDeclaration>>,

    /// Module type bindings.
    modtypes: BindingTable<Arc<ModtypeDeclaration>>,

    /// Summary of bindings.
    summary: Arc<EnvSummary>,

    /// Local constraints (for GADTs).
    local_constraints: HashMap<Path, Arc<TypeDeclaration>>,

    /// GADT instance levels.
    gadt_instances: Vec<(i32, Vec<TypeExprRef>)>,

    /// Environment flags.
    flags: EnvFlags,
}

/// Environment flags.
#[derive(Debug, Clone, Copy, Default)]
pub struct EnvFlags {
    /// Check for dependency cycles.
    pub check_dependency: bool,
    /// Allow hiding of bindings.
    pub allow_hiding: bool,
}

impl Env {
    /// Create an empty environment.
    pub fn empty() -> Self {
        Env {
            values: BindingTable::new(),
            types: BindingTable::new(),
            constrs: BindingTable::new(),
            labels: BindingTable::new(),
            modules: BindingTable::new(),
            modtypes: BindingTable::new(),
            summary: Arc::new(EnvSummary::Empty),
            local_constraints: HashMap::new(),
            gadt_instances: Vec::new(),
            flags: EnvFlags::default(),
        }
    }

    /// Get the environment summary.
    pub fn summary(&self) -> &EnvSummary {
        &self.summary
    }

    // ========================================================================
    // Value Bindings
    // ========================================================================

    /// Add a value binding.
    pub fn add_value(&mut self, id: Ident, desc: ValueDescription) {
        let desc = Arc::new(desc);
        self.values.add(id.clone(), desc.clone());
        self.summary = Arc::new(EnvSummary::Value(self.summary.clone(), id, desc));
    }

    /// Find a value by name.
    pub fn find_value(&self, name: &str) -> EnvResult<&ValueDescription> {
        self.values
            .find_by_name(name)
            .map(|v| v.as_ref())
            .ok_or_else(|| EnvError::ValueNotFound(name.to_string()))
    }

    /// Find a value by name, returning both the identifier and description.
    pub fn find_value_with_id(&self, name: &str) -> EnvResult<(&Ident, &ValueDescription)> {
        self.values
            .find_by_name_with_id(name)
            .map(|(id, v)| (id, v.as_ref()))
            .ok_or_else(|| EnvError::ValueNotFound(name.to_string()))
    }

    /// Find a value by path.
    pub fn lookup_value(&self, path: &Path) -> EnvResult<&ValueDescription> {
        match path {
            Path::Pident(id) => self
                .values
                .find_by_ident(id)
                .map(|v| v.as_ref())
                .ok_or_else(|| EnvError::ValueNotFound(id.name().to_string())),
            Path::Pdot(_, name, _) => self.find_value(name),
            Path::Papply(_, _) => Err(EnvError::ValueNotFound("(functor application)".to_string())),
        }
    }

    // ========================================================================
    // Type Bindings
    // ========================================================================

    /// Add a type binding.
    pub fn add_type(&mut self, id: Ident, decl: TypeDeclaration) {
        let decl = Arc::new(decl);
        self.types.add(id.clone(), decl.clone());
        self.summary = Arc::new(EnvSummary::Type(self.summary.clone(), id, decl));
    }

    /// Find a type by name.
    pub fn find_type(&self, name: &str) -> EnvResult<&TypeDeclaration> {
        self.types
            .find_by_name(name)
            .map(|t| t.as_ref())
            .ok_or_else(|| EnvError::TypeNotFound(name.to_string()))
    }

    /// Find a type by path.
    pub fn lookup_type(&self, path: &Path) -> EnvResult<&TypeDeclaration> {
        match path {
            Path::Pident(id) => self
                .types
                .find_by_ident(id)
                .map(|t| t.as_ref())
                .ok_or_else(|| EnvError::TypeNotFound(id.name().to_string())),
            Path::Pdot(_, name, _) => self.find_type(name),
            Path::Papply(_, _) => Err(EnvError::TypeNotFound("(functor application)".to_string())),
        }
    }

    // ========================================================================
    // Constructor Bindings
    // ========================================================================

    /// Add a constructor binding.
    pub fn add_constructor(&mut self, id: Ident, desc: ConstructorDescription) {
        let desc = Arc::new(desc);
        self.constrs.add(id, desc);
    }

    /// Find a constructor by name.
    pub fn find_constructor(&self, name: &str) -> EnvResult<&ConstructorDescription> {
        self.constrs
            .find_by_name(name)
            .map(|c| c.as_ref())
            .ok_or_else(|| EnvError::ConstructorNotFound(name.to_string()))
    }

    /// Find all constructors with a given name.
    pub fn find_all_constructors(&self, name: &str) -> Vec<&ConstructorDescription> {
        self.constrs
            .find_all(name)
            .into_iter()
            .map(|c| c.as_ref())
            .collect()
    }

    // ========================================================================
    // Label Bindings
    // ========================================================================

    /// Add a label binding.
    pub fn add_label(&mut self, id: Ident, desc: LabelDescription) {
        let desc = Arc::new(desc);
        self.labels.add(id, desc);
    }

    /// Find a label by name.
    pub fn find_label(&self, name: &str) -> EnvResult<&LabelDescription> {
        self.labels
            .find_by_name(name)
            .map(|l| l.as_ref())
            .ok_or_else(|| EnvError::LabelNotFound(name.to_string()))
    }

    /// Find all labels with a given name.
    pub fn find_all_labels(&self, name: &str) -> Vec<&LabelDescription> {
        self.labels
            .find_all(name)
            .into_iter()
            .map(|l| l.as_ref())
            .collect()
    }

    // ========================================================================
    // Module Bindings
    // ========================================================================

    /// Add a module binding.
    pub fn add_module(&mut self, id: Ident, decl: ModuleDeclaration) {
        let decl = Arc::new(decl);
        self.modules.add(id.clone(), decl.clone());
        self.summary = Arc::new(EnvSummary::Module(self.summary.clone(), id, decl));
    }

    /// Find a module by name.
    pub fn find_module(&self, name: &str) -> EnvResult<&ModuleDeclaration> {
        self.modules
            .find_by_name(name)
            .map(|m| m.as_ref())
            .ok_or_else(|| EnvError::ModuleNotFound(name.to_string()))
    }

    /// Find a module by path.
    pub fn lookup_module(&self, path: &Path) -> EnvResult<&ModuleDeclaration> {
        match path {
            Path::Pident(id) => self
                .modules
                .find_by_ident(id)
                .map(|m| m.as_ref())
                .ok_or_else(|| EnvError::ModuleNotFound(id.name().to_string())),
            Path::Pdot(_, name, _) => self.find_module(name),
            Path::Papply(_, _) => {
                // For functor application, would need to evaluate the application
                Err(EnvError::ModuleNotFound(
                    "(functor application)".to_string(),
                ))
            }
        }
    }

    // ========================================================================
    // Module Type Bindings
    // ========================================================================

    /// Add a module type binding.
    pub fn add_modtype(&mut self, id: Ident, decl: ModtypeDeclaration) {
        let decl = Arc::new(decl);
        self.modtypes.add(id.clone(), decl.clone());
        self.summary = Arc::new(EnvSummary::ModType(self.summary.clone(), id, decl));
    }

    /// Find a module type by name.
    pub fn find_modtype(&self, name: &str) -> EnvResult<&ModtypeDeclaration> {
        self.modtypes
            .find_by_name(name)
            .map(|m| m.as_ref())
            .ok_or_else(|| EnvError::ModuleNotFound(name.to_string()))
    }

    // ========================================================================
    // GADT Support
    // ========================================================================

    /// Check if the environment has local constraints.
    pub fn has_local_constraints(&self) -> bool {
        !self.local_constraints.is_empty()
    }

    /// Add a local constraint.
    pub fn add_local_constraint(&mut self, path: Path, decl: TypeDeclaration) {
        self.local_constraints.insert(path, Arc::new(decl));
    }

    /// Find a local constraint.
    pub fn find_local_constraint(&self, path: &Path) -> Option<&TypeDeclaration> {
        self.local_constraints.get(path).map(|d| d.as_ref())
    }

    /// Add a GADT instance level.
    pub fn add_gadt_instance_level(&mut self, level: i32, types: Vec<TypeExprRef>) {
        self.gadt_instances.push((level, types));
    }

    /// Get GADT instance level for a type.
    pub fn gadt_instance_level(&self, _ty: TypeExprRef) -> Option<i32> {
        // TODO: Implement proper GADT instance tracking
        None
    }

    // ========================================================================
    // Environment Extension
    // ========================================================================

    /// Create a new environment with an additional value.
    pub fn with_value(&self, id: Ident, desc: ValueDescription) -> Env {
        let mut env = self.clone();
        env.add_value(id, desc);
        env
    }

    /// Create a new environment with an additional type.
    pub fn with_type(&self, id: Ident, decl: TypeDeclaration) -> Env {
        let mut env = self.clone();
        env.add_type(id, decl);
        env
    }

    /// Create a new environment with an additional module.
    pub fn with_module(&self, id: Ident, decl: ModuleDeclaration) -> Env {
        let mut env = self.clone();
        env.add_module(id, decl);
        env
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::empty()
    }
}

// ============================================================================
// Initial Environment
// ============================================================================

/// Create the initial environment with built-in types.
pub fn initial_env(ctx: &TypeContext<'_>) -> Env {
    let mut env = Env::empty();

    // Add built-in types
    // These are the primitive types that exist in ReScript
    let builtins = [
        ("int", true),     // immediate
        ("float", false),
        ("bool", true),    // immediate
        ("string", false),
        ("unit", true),    // immediate
        ("array", false),
        ("list", false),
        ("option", false),
        ("char", true),    // immediate
    ];

    for (name, immediate) in builtins {
        let id = Ident::create_persistent(name);
        let decl = TypeDeclaration {
            type_params: vec![],
            type_arity: if name == "array" || name == "list" || name == "option" { 1 } else { 0 },
            type_kind: super::decl::TypeKind::TypeAbstract,
            type_private: super::asttypes::PrivateFlag::Public,
            type_manifest: None,
            type_variance: vec![],
            type_newtype_level: None,
            type_loc: Location::none(),
            type_attributes: vec![],
            type_immediate: immediate,
            type_unboxed: super::decl::UnboxedStatus::default(),
            type_inlined_types: vec![],
        };
        env.add_type(id, decl);
    }

    // Create type references for primitives
    let int_ty = ctx.new_constr(Path::pident(Ident::create_persistent("int")), vec![]);
    let float_ty = ctx.new_constr(Path::pident(Ident::create_persistent("float")), vec![]);
    let bool_ty = ctx.new_constr(Path::pident(Ident::create_persistent("bool")), vec![]);
    let string_ty = ctx.new_constr(Path::pident(Ident::create_persistent("string")), vec![]);
    let unit_ty = ctx.new_constr(Path::pident(Ident::create_persistent("unit")), vec![]);

    // Add boolean constructors: true and false
    // false has tag 0, true has tag 1
    let false_id = Ident::create_persistent("false");
    let false_cstr = ConstructorDescription {
        cstr_name: "false".to_string(),
        cstr_res: bool_ty,
        cstr_existentials: vec![],
        cstr_args: vec![],
        cstr_arity: 0,
        cstr_tag: ConstructorTag::CstrConstant(0),
        cstr_consts: 2,
        cstr_nonconsts: 0,
        cstr_generalized: false,
        cstr_private: PrivateFlag::Public,
        cstr_loc: Location::none(),
        cstr_attributes: vec![],
        cstr_inlined: None,
    };
    env.add_constructor(false_id, false_cstr);

    let true_id = Ident::create_persistent("true");
    let true_cstr = ConstructorDescription {
        cstr_name: "true".to_string(),
        cstr_res: bool_ty,
        cstr_existentials: vec![],
        cstr_args: vec![],
        cstr_arity: 0,
        cstr_tag: ConstructorTag::CstrConstant(1),
        cstr_consts: 2,
        cstr_nonconsts: 0,
        cstr_generalized: false,
        cstr_private: PrivateFlag::Public,
        cstr_loc: Location::none(),
        cstr_attributes: vec![],
        cstr_inlined: None,
    };
    env.add_constructor(true_id, true_cstr);

    // Add arithmetic operators for int
    // Note: In ReScript, + is polymorphic and specialized by the type checker,
    // but for simplicity we'll add int versions directly
    add_binary_primitive(&mut env, ctx, "+", "%addint", int_ty, int_ty, int_ty);
    add_binary_primitive(&mut env, ctx, "-", "%subint", int_ty, int_ty, int_ty);
    add_binary_primitive(&mut env, ctx, "*", "%mulint", int_ty, int_ty, int_ty);
    add_binary_primitive(&mut env, ctx, "/", "%divint", int_ty, int_ty, int_ty);
    add_binary_primitive(&mut env, ctx, "mod", "%modint", int_ty, int_ty, int_ty);

    // Add arithmetic operators for float
    add_binary_primitive(&mut env, ctx, "+.", "%addfloat", float_ty, float_ty, float_ty);
    add_binary_primitive(&mut env, ctx, "-.", "%subfloat", float_ty, float_ty, float_ty);
    add_binary_primitive(&mut env, ctx, "*.", "%mulfloat", float_ty, float_ty, float_ty);
    add_binary_primitive(&mut env, ctx, "/.", "%divfloat", float_ty, float_ty, float_ty);
    add_binary_primitive(&mut env, ctx, "**", "%powfloat", float_ty, float_ty, float_ty);

    // Add comparison operators (polymorphic - using int for now)
    add_binary_primitive(&mut env, ctx, "==", "%eq", int_ty, int_ty, bool_ty);
    add_binary_primitive(&mut env, ctx, "!=", "%noteq", int_ty, int_ty, bool_ty);
    add_binary_primitive(&mut env, ctx, "<", "%ltint", int_ty, int_ty, bool_ty);
    add_binary_primitive(&mut env, ctx, ">", "%gtint", int_ty, int_ty, bool_ty);
    add_binary_primitive(&mut env, ctx, "<=", "%leint", int_ty, int_ty, bool_ty);
    add_binary_primitive(&mut env, ctx, ">=", "%geint", int_ty, int_ty, bool_ty);

    // Add boolean operators
    add_binary_primitive(&mut env, ctx, "&&", "%sequand", bool_ty, bool_ty, bool_ty);
    add_binary_primitive(&mut env, ctx, "||", "%sequor", bool_ty, bool_ty, bool_ty);
    add_unary_primitive(&mut env, ctx, "not", "%boolnot", bool_ty, bool_ty);

    // Add unary minus
    add_unary_primitive(&mut env, ctx, "~-", "%negint", int_ty, int_ty);
    add_unary_primitive(&mut env, ctx, "~-.", "%negfloat", float_ty, float_ty);

    // Add string concatenation
    add_binary_primitive(&mut env, ctx, "++", "%string_concat", string_ty, string_ty, string_ty);

    // Add ignore function
    let alpha = ctx.new_var(None);
    add_unary_primitive(&mut env, ctx, "ignore", "%ignore", alpha, unit_ty);

    env
}

/// Helper to add a binary primitive operator
fn add_binary_primitive(
    env: &mut Env,
    ctx: &TypeContext<'_>,
    name: &str,
    prim_name: &str,
    arg1_ty: TypeExprRef,
    arg2_ty: TypeExprRef,
    ret_ty: TypeExprRef,
) {
    // Inner arrow: arg2 -> ret, arity None (partial application gives arity 1)
    let inner_ty = ctx.new_arrow_simple(arg2_ty, ret_ty);
    // Outer arrow: arg1 -> (arg2 -> ret), arity 2 (full application)
    let func_ty = ctx.new_arrow(crate::types::ArgLabel::Nolabel, arg1_ty, inner_ty, Some(2));
    let id = Ident::create_persistent(name);
    // Primitives come from Pervasives module
    let pervasives_id = Ident::create_persistent("Pervasives");
    let path = Path::pdot(Path::pident(pervasives_id), name.to_string());
    let desc = ValueDescription {
        val_type: func_ty,
        val_kind: ValueKind::ValPrim(PrimitiveDescription {
            prim_name: prim_name.to_string(),
            prim_arity: 2,
            prim_native_name: String::new(),
            prim_native_float: false,
        }),
        val_loc: Location::none(),
        val_attributes: vec![],
        val_path: Some(path),
    };
    env.add_value(id, desc);
}

/// Helper to add a unary primitive operator
fn add_unary_primitive(
    env: &mut Env,
    ctx: &TypeContext<'_>,
    name: &str,
    prim_name: &str,
    arg_ty: TypeExprRef,
    ret_ty: TypeExprRef,
) {
    // Unary operators have arity 1
    let func_ty = ctx.new_arrow(crate::types::ArgLabel::Nolabel, arg_ty, ret_ty, Some(1));
    let id = Ident::create_persistent(name);
    // Primitives come from Pervasives module
    let pervasives_id = Ident::create_persistent("Pervasives");
    let path = Path::pdot(Path::pident(pervasives_id), name.to_string());
    let desc = ValueDescription {
        val_type: func_ty,
        val_kind: ValueKind::ValPrim(PrimitiveDescription {
            prim_name: prim_name.to_string(),
            prim_arity: 1,
            prim_native_name: String::new(),
            prim_native_float: false,
        }),
        val_loc: Location::none(),
        val_attributes: vec![],
        val_path: Some(path),
    };
    env.add_value(id, desc);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_env() {
        let env = Env::empty();
        assert!(env.find_value("x").is_err());
        assert!(env.find_type("t").is_err());
    }

    #[test]
    fn test_add_and_find_value() {
        let mut env = Env::empty();
        let id = Ident::create_persistent("x");
        let desc = ValueDescription {
            val_type: TypeExprRef(0),
            val_kind: super::super::decl::ValueKind::ValReg,
            val_loc: Location::none(),
            val_attributes: vec![],
            val_path: None,
        };
        env.add_value(id.clone(), desc);

        assert!(env.find_value("x").is_ok());
        assert!(env.find_value("y").is_err());
    }

    #[test]
    fn test_add_and_find_type() {
        let mut env = Env::empty();
        let id = Ident::create_persistent("t");
        let decl = TypeDeclaration {
            type_params: vec![],
            type_arity: 0,
            type_kind: super::super::decl::TypeKind::TypeAbstract,
            type_private: super::super::asttypes::PrivateFlag::Public,
            type_manifest: None,
            type_variance: vec![],
            type_newtype_level: None,
            type_loc: Location::none(),
            type_attributes: vec![],
            type_immediate: false,
            type_unboxed: super::super::decl::UnboxedStatus::default(),
            type_inlined_types: vec![],
        };
        env.add_type(id.clone(), decl);

        assert!(env.find_type("t").is_ok());
        assert!(env.find_type("u").is_err());
    }

    #[test]
    fn test_shadowing() {
        let mut env = Env::empty();
        let id1 = Ident::create_local("x");
        let id2 = Ident::create_local("x");

        let desc1 = ValueDescription {
            val_type: TypeExprRef(1),
            val_kind: super::super::decl::ValueKind::ValReg,
            val_loc: Location::none(),
            val_attributes: vec![],
            val_path: None,
        };
        let desc2 = ValueDescription {
            val_type: TypeExprRef(2),
            val_kind: super::super::decl::ValueKind::ValReg,
            val_loc: Location::none(),
            val_attributes: vec![],
            val_path: None,
        };

        env.add_value(id1, desc1);
        env.add_value(id2, desc2);

        // Should find the most recent binding
        let found = env.find_value("x").unwrap();
        assert_eq!(found.val_type, TypeExprRef(2));
    }

    #[test]
    fn test_with_value() {
        let env = Env::empty();
        let id = Ident::create_persistent("x");
        let desc = ValueDescription {
            val_type: TypeExprRef(0),
            val_kind: super::super::decl::ValueKind::ValReg,
            val_loc: Location::none(),
            val_attributes: vec![],
            val_path: None,
        };

        let env2 = env.with_value(id, desc);

        // Original env unchanged
        assert!(env.find_value("x").is_err());
        // New env has the binding
        assert!(env2.find_value("x").is_ok());
    }

    #[test]
    fn test_lookup_by_path() {
        let mut env = Env::empty();
        let id = Ident::create_persistent("x");
        let desc = ValueDescription {
            val_type: TypeExprRef(0),
            val_kind: super::super::decl::ValueKind::ValReg,
            val_loc: Location::none(),
            val_attributes: vec![],
            val_path: None,
        };
        env.add_value(id.clone(), desc);

        let path = Path::pident(id);
        assert!(env.lookup_value(&path).is_ok());
    }
}
