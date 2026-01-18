//! Type checking context.
//!
//! This module provides `TypeContext`, the core context for type checking.
//! It replaces the global state from the OCaml implementation with explicit,
//! per-compilation state.
//!
//! # Design
//!
//! The `TypeContext` owns all type expressions through a `TypeArena`.
//! It maintains the type checking state including:
//! - Current generalization level
//! - Type variable tracking
//! - Unification state
//!
//! # Concurrency
//!
//! `TypeContext` is NOT thread-safe - it uses `RefCell` internally.
//! However, multiple `TypeContext` instances can exist concurrently,
//! one per compilation unit.

use super::asttypes::ArgLabel;
use super::path::Path;
use super::type_expr::{
    AbbrevMemo, AbbrevMemoRef, Commutable, CommutableRef, FieldKind, FieldKindRef, GENERIC_LEVEL,
    RowDesc, RowField, RowFieldRef, TypeArg, TypeDesc, TypeExpr, TypeExprRef,
};
use crate::context::IdGenerator;
use std::cell::{Cell, RefCell};
use typed_arena::Arena;

/// Type checking context.
///
/// This context is created for each type checking session (typically one
/// per source file). It owns all type expressions and maintains the state
/// needed for type inference.
///
/// # Example
///
/// ```rust,ignore
/// use rescript_compiler::types::TypeContext;
/// use rescript_compiler::context::IdGenerator;
///
/// let id_gen = IdGenerator::new();
/// let mut ctx = TypeContext::new(&id_gen);
///
/// // Create a type variable
/// let tvar = ctx.new_var(None);
///
/// // Create an arrow type
/// let int_type = ctx.new_constr(path_int, vec![]);
/// let func_type = ctx.new_arrow(tvar, int_type);
/// ```
pub struct TypeContext<'a> {
    /// Type expression arena - all types are allocated here
    arena: Arena<TypeExpr>,

    /// Abbreviation memo arena
    abbrev_arena: Arena<RefCell<AbbrevMemo>>,

    /// Row field arena
    row_field_arena: Arena<RefCell<Option<RowField>>>,

    /// Commutable arena
    commutable_arena: Arena<RefCell<Commutable>>,

    /// Field kind arena
    field_kind_arena: Arena<RefCell<Option<FieldKind>>>,

    /// ID generator for type IDs
    id_gen: &'a IdGenerator,

    /// Current generalization level
    current_level: Cell<i32>,

    /// Non-generalizable level
    nongen_level: Cell<i32>,

    /// Global level
    global_level: Cell<i32>,

    /// Saved levels stack
    saved_levels: RefCell<Vec<(i32, i32)>>,

    /// GADT instance tracking flag
    trace_gadt_instances: Cell<bool>,

    /// Simple abbreviations memo
    simple_abbrevs: RefCell<Option<AbbrevMemoRef>>,

    /// Type expression storage (for indexed access)
    types: RefCell<Vec<&'a TypeExpr>>,

    /// Abbreviation memo storage
    abbrevs: RefCell<Vec<&'a RefCell<AbbrevMemo>>>,

    /// Row field storage
    row_fields: RefCell<Vec<&'a RefCell<Option<RowField>>>>,

    /// Commutable storage
    commutables: RefCell<Vec<&'a RefCell<Commutable>>>,

    /// Field kind storage
    field_kinds: RefCell<Vec<&'a RefCell<Option<FieldKind>>>>,
}

impl<'a> TypeContext<'a> {
    /// Create a new type context.
    pub fn new(id_gen: &'a IdGenerator) -> Self {
        TypeContext {
            arena: Arena::new(),
            abbrev_arena: Arena::new(),
            row_field_arena: Arena::new(),
            commutable_arena: Arena::new(),
            field_kind_arena: Arena::new(),
            id_gen,
            current_level: Cell::new(0),
            nongen_level: Cell::new(0),
            global_level: Cell::new(1),
            saved_levels: RefCell::new(Vec::new()),
            trace_gadt_instances: Cell::new(false),
            simple_abbrevs: RefCell::new(None),
            types: RefCell::new(Vec::new()),
            abbrevs: RefCell::new(Vec::new()),
            row_fields: RefCell::new(Vec::new()),
            commutables: RefCell::new(Vec::new()),
            field_kinds: RefCell::new(Vec::new()),
        }
    }

    // ========================================================================
    // Type Creation
    // ========================================================================

    /// Create a new type expression.
    fn alloc_type(&self, desc: TypeDesc, level: i32) -> TypeExprRef {
        let id = self.id_gen.next_type_id();
        let ty = self.arena.alloc(TypeExpr::new(desc, level, id));

        // Safety: We're extending the lifetime to 'a which is the arena lifetime.
        // This is safe because the arena owns the type expression.
        let ty_ref: &'a TypeExpr = unsafe { std::mem::transmute(ty) };

        let mut types = self.types.borrow_mut();
        let idx = types.len();
        types.push(ty_ref);
        TypeExprRef(idx)
    }

    /// Create a new type at the current level.
    pub fn new_type(&self, desc: TypeDesc) -> TypeExprRef {
        self.alloc_type(desc, self.current_level.get())
    }

    /// Create a new type at the generic level.
    pub fn new_generic_type(&self, desc: TypeDesc) -> TypeExprRef {
        self.alloc_type(desc, GENERIC_LEVEL)
    }

    /// Create a new type variable.
    pub fn new_var(&self, name: Option<String>) -> TypeExprRef {
        self.new_type(TypeDesc::Tvar(name))
    }

    /// Create a new generic type variable.
    pub fn new_generic_var(&self, name: Option<String>) -> TypeExprRef {
        self.new_generic_type(TypeDesc::Tvar(name))
    }

    /// Create an arrow (function) type.
    pub fn new_arrow(
        &self,
        arg_label: ArgLabel,
        arg_type: TypeExprRef,
        ret_type: TypeExprRef,
        arity: Option<i32>,
    ) -> TypeExprRef {
        let commutable = self.alloc_commutable(Commutable::Cok);
        self.new_type(TypeDesc::Tarrow {
            arg: TypeArg::new(arg_label, arg_type),
            ret: ret_type,
            commutable: Commutable::Clink(commutable),
            arity,
        })
    }

    /// Create an unlabeled arrow type.
    pub fn new_arrow_simple(&self, arg: TypeExprRef, ret: TypeExprRef) -> TypeExprRef {
        self.new_arrow(ArgLabel::Nolabel, arg, ret, None)
    }

    /// Create a tuple type.
    pub fn new_tuple(&self, types: Vec<TypeExprRef>) -> TypeExprRef {
        self.new_type(TypeDesc::Ttuple(types))
    }

    /// Create a type constructor application.
    pub fn new_constr(&self, path: Path, args: Vec<TypeExprRef>) -> TypeExprRef {
        let abbrev = self.alloc_abbrev(AbbrevMemo::Mnil);
        self.new_type(TypeDesc::Tconstr { path, args, abbrev })
    }

    /// Create a link type (for unification).
    pub fn new_link(&self, target: TypeExprRef) -> TypeExprRef {
        self.new_type(TypeDesc::Tlink(target))
    }

    /// Create a polymorphic type.
    pub fn new_poly(&self, body: TypeExprRef, vars: Vec<TypeExprRef>) -> TypeExprRef {
        self.new_type(TypeDesc::Tpoly { body, vars })
    }

    /// Create a universal type variable.
    pub fn new_univar(&self, name: Option<String>) -> TypeExprRef {
        self.new_type(TypeDesc::Tunivar(name))
    }

    /// Create a variant type.
    pub fn new_variant(&self, row: RowDesc) -> TypeExprRef {
        self.new_type(TypeDesc::Tvariant(row))
    }

    // ========================================================================
    // Type Access
    // ========================================================================

    /// Get a type expression by reference.
    pub fn get_type(&self, r: TypeExprRef) -> &TypeExpr {
        self.types.borrow()[r.0]
    }

    /// Get the descriptor of a type.
    pub fn get_desc(&self, r: TypeExprRef) -> std::cell::Ref<'_, TypeDesc> {
        self.get_type(r).desc()
    }

    /// Get the level of a type.
    pub fn get_level(&self, r: TypeExprRef) -> i32 {
        self.get_type(r).level()
    }

    /// Set the level of a type.
    pub fn set_level(&self, r: TypeExprRef, level: i32) {
        self.get_type(r).set_level(level);
    }

    /// Set the descriptor of a type.
    pub fn set_desc(&self, r: TypeExprRef, desc: TypeDesc) {
        self.get_type(r).set_desc(desc);
    }

    // ========================================================================
    // Abbreviation Memos
    // ========================================================================

    /// Allocate an abbreviation memo.
    fn alloc_abbrev(&self, memo: AbbrevMemo) -> AbbrevMemoRef {
        let m = self.abbrev_arena.alloc(RefCell::new(memo));
        let m_ref: &'a RefCell<AbbrevMemo> = unsafe { std::mem::transmute(m) };

        let mut abbrevs = self.abbrevs.borrow_mut();
        let idx = abbrevs.len();
        abbrevs.push(m_ref);
        AbbrevMemoRef(idx)
    }

    // ========================================================================
    // Row Fields
    // ========================================================================

    /// Allocate a row field reference.
    fn alloc_row_field(&self, field: Option<RowField>) -> RowFieldRef {
        let f = self.row_field_arena.alloc(RefCell::new(field));
        let f_ref: &'a RefCell<Option<RowField>> = unsafe { std::mem::transmute(f) };

        let mut row_fields = self.row_fields.borrow_mut();
        let idx = row_fields.len();
        row_fields.push(f_ref);
        RowFieldRef(idx)
    }

    // ========================================================================
    // Commutables
    // ========================================================================

    /// Allocate a commutable reference.
    fn alloc_commutable(&self, c: Commutable) -> CommutableRef {
        let c_alloc = self.commutable_arena.alloc(RefCell::new(c));
        let c_ref: &'a RefCell<Commutable> = unsafe { std::mem::transmute(c_alloc) };

        let mut commutables = self.commutables.borrow_mut();
        let idx = commutables.len();
        commutables.push(c_ref);
        CommutableRef(idx)
    }

    // ========================================================================
    // Field Kinds
    // ========================================================================

    /// Allocate a field kind reference.
    fn alloc_field_kind(&self, kind: Option<FieldKind>) -> FieldKindRef {
        let k = self.field_kind_arena.alloc(RefCell::new(kind));
        let k_ref: &'a RefCell<Option<FieldKind>> = unsafe { std::mem::transmute(k) };

        let mut field_kinds = self.field_kinds.borrow_mut();
        let idx = field_kinds.len();
        field_kinds.push(k_ref);
        FieldKindRef(idx)
    }

    // ========================================================================
    // Level Management
    // ========================================================================

    /// Get the current level.
    pub fn current_level(&self) -> i32 {
        self.current_level.get()
    }

    /// Get the non-generalizable level.
    pub fn nongen_level(&self) -> i32 {
        self.nongen_level.get()
    }

    /// Get the global level.
    pub fn global_level(&self) -> i32 {
        self.global_level.get()
    }

    /// Initialize definition level.
    pub fn init_def(&self, level: i32) {
        self.current_level.set(level);
        self.nongen_level.set(level);
    }

    /// Begin a new definition scope.
    pub fn begin_def(&self) {
        let current = self.current_level.get();
        let nongen = self.nongen_level.get();
        self.saved_levels.borrow_mut().push((current, nongen));
        self.current_level.set(current + 1);
        self.nongen_level.set(current + 1);
    }

    /// Begin a class definition scope.
    pub fn begin_class_def(&self) {
        let current = self.current_level.get();
        let nongen = self.nongen_level.get();
        self.saved_levels.borrow_mut().push((current, nongen));
        self.current_level.set(current + 1);
    }

    /// Raise the non-generalizable level.
    pub fn raise_nongen_level(&self) {
        let current = self.current_level.get();
        let nongen = self.nongen_level.get();
        self.saved_levels.borrow_mut().push((current, nongen));
        self.nongen_level.set(current);
    }

    /// End a definition scope.
    pub fn end_def(&self) {
        if let Some((current, nongen)) = self.saved_levels.borrow_mut().pop() {
            self.current_level.set(current);
            self.nongen_level.set(nongen);
        }
    }

    /// Reset the global level.
    pub fn reset_global_level(&self) {
        self.global_level.set(self.current_level.get() + 1);
    }

    /// Increase the global level and return the old value.
    pub fn increase_global_level(&self) -> i32 {
        let old = self.global_level.get();
        self.global_level.set(self.current_level.get());
        old
    }

    /// Restore the global level.
    pub fn restore_global_level(&self, level: i32) {
        self.global_level.set(level);
    }

    // ========================================================================
    // GADT Tracking
    // ========================================================================

    /// Check and set GADT instance tracing.
    pub fn check_trace_gadt_instances(&self, has_constraints: bool) -> bool {
        if !self.trace_gadt_instances.get() && has_constraints {
            self.trace_gadt_instances.set(true);
            true
        } else {
            false
        }
    }

    /// Reset GADT instance tracing.
    pub fn reset_trace_gadt_instances(&self, was_set: bool) {
        if was_set {
            self.trace_gadt_instances.set(false);
        }
    }

    // ========================================================================
    // Type Representative (Union-Find)
    // ========================================================================

    /// Get the representative of a type (follow Tlink chains).
    ///
    /// This uses path compression to update intermediate links
    /// to point directly to the representative, improving
    /// subsequent lookup performance.
    pub fn repr(&self, t: TypeExprRef) -> TypeExprRef {
        // Check the desc and extract the next reference if it's a link
        let next = {
            let desc = self.get_desc(t);
            match &*desc {
                TypeDesc::Tlink(t2) => Some(*t2),
                TypeDesc::Tfield {
                    kind: FieldKind::Fabsent,
                    rest,
                    ..
                } => Some(*rest),
                _ => None,
            }
        };

        match next {
            Some(next_ref) => {
                // Recursively find the representative
                let rep = self.repr(next_ref);

                // Path compression: if we traversed more than one link,
                // update this link to point directly to the representative
                if rep != next_ref {
                    self.set_desc(t, TypeDesc::Tlink(rep));
                }

                rep
            }
            None => t,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_var() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let v1 = ctx.new_var(Some("a".to_string()));
        let v2 = ctx.new_var(None);

        assert_ne!(v1, v2);
    }

    #[test]
    fn test_new_arrow() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let t1 = ctx.new_var(None);
        let t2 = ctx.new_var(None);
        let arrow = ctx.new_arrow_simple(t1, t2);

        let desc = ctx.get_desc(arrow);
        assert!(matches!(*desc, TypeDesc::Tarrow { .. }));
    }

    #[test]
    fn test_level_management() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        assert_eq!(ctx.current_level(), 0);

        ctx.begin_def();
        assert_eq!(ctx.current_level(), 1);

        ctx.begin_def();
        assert_eq!(ctx.current_level(), 2);

        ctx.end_def();
        assert_eq!(ctx.current_level(), 1);

        ctx.end_def();
        assert_eq!(ctx.current_level(), 0);
    }

    #[test]
    fn test_repr() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let t1 = ctx.new_var(None);
        let t2 = ctx.new_link(t1);
        let t3 = ctx.new_link(t2);

        // repr should follow the chain
        assert_eq!(ctx.repr(t3), t1);
        assert_eq!(ctx.repr(t2), t1);
        assert_eq!(ctx.repr(t1), t1);
    }

    #[test]
    fn test_repr_path_compression() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let t1 = ctx.new_var(None);
        let t2 = ctx.new_link(t1);
        let t3 = ctx.new_link(t2);

        // First call to repr(t3) should do path compression
        assert_eq!(ctx.repr(t3), t1);

        // After path compression, t3 should now point directly to t1
        let desc = ctx.get_desc(t3);
        match &*desc {
            TypeDesc::Tlink(target) => assert_eq!(*target, t1),
            _ => panic!("Expected Tlink after path compression"),
        }
    }

    #[test]
    fn test_tuple() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let t1 = ctx.new_var(None);
        let t2 = ctx.new_var(None);
        let tuple = ctx.new_tuple(vec![t1, t2]);

        let desc = ctx.get_desc(tuple);
        match &*desc {
            TypeDesc::Ttuple(types) => assert_eq!(types.len(), 2),
            _ => panic!("Expected tuple"),
        }
    }
}
