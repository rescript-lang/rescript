//! Basic type operations.
//!
//! This module provides fundamental operations on types that are used
//! throughout the type checker. It includes:
//!
//! - Type representative (union-find with path compression)
//! - Type traversal utilities
//! - Row field operations
//! - Type copying utilities
//!
//! These operations are the building blocks for more complex type
//! operations like unification and generalization.

use super::context::TypeContext;
use super::decl::{ConstructorArguments, TypeKind};
use super::path::Path;
use super::type_expr::{
    AbbrevMemoRef, GENERIC_LEVEL, LOWEST_LEVEL, PIVOT_LEVEL, RowDesc, RowField, TypeDesc,
    TypeExprRef,
};

// ============================================================================
// Type Level Constants
// ============================================================================

/// Check if a type is at the generic level.
pub fn is_generic(ctx: &TypeContext<'_>, t: TypeExprRef) -> bool {
    ctx.get_level(t) == GENERIC_LEVEL
}

/// Check if a type's level is >= lowest_level (not marked).
pub fn is_unmarked(ctx: &TypeContext<'_>, t: TypeExprRef) -> bool {
    ctx.get_level(t) >= LOWEST_LEVEL
}

// ============================================================================
// Type Predicates
// ============================================================================

/// Check if a type is a type variable (Tvar).
pub fn is_tvar(ctx: &TypeContext<'_>, t: TypeExprRef) -> bool {
    let t = ctx.repr(t);
    matches!(*ctx.get_desc(t), TypeDesc::Tvar(_))
}

/// Check if a type is a universal type variable (Tunivar).
pub fn is_tunivar(ctx: &TypeContext<'_>, t: TypeExprRef) -> bool {
    let t = ctx.repr(t);
    matches!(*ctx.get_desc(t), TypeDesc::Tunivar(_))
}

/// Check if a type is a type constructor (Tconstr).
pub fn is_tconstr(ctx: &TypeContext<'_>, t: TypeExprRef) -> bool {
    let t = ctx.repr(t);
    matches!(*ctx.get_desc(t), TypeDesc::Tconstr { .. })
}

// ============================================================================
// Row Field Operations
// ============================================================================

/// Get the representative of a row field.
///
/// This follows the `link` chain in `Reither` fields.
pub fn row_field_repr(field: &RowField) -> RowField {
    row_field_repr_aux(&[], field)
}

fn row_field_repr_aux(tl: &[TypeExprRef], field: &RowField) -> RowField {
    match field {
        RowField::Reither {
            constant,
            types,
            matched,
            link,
        } => {
            // In OCaml this follows the link - for now we just return the field
            // TODO: Implement proper link following when we have mutable row field refs
            let mut combined_types = tl.to_vec();
            combined_types.extend(types.iter().copied());
            RowField::Reither {
                constant: *constant,
                types: combined_types,
                matched: *matched,
                link: *link,
            }
        }
        RowField::Rpresent(Some(_)) if !tl.is_empty() => RowField::Rpresent(Some(tl[0])),
        other => other.clone(),
    }
}

/// Get the "more" type variable from a row.
///
/// This follows variant type chains to find the underlying type variable.
pub fn row_more(ctx: &TypeContext<'_>, row: &RowDesc) -> TypeExprRef {
    let more = ctx.repr(row.row_more);
    match &*ctx.get_desc(more) {
        TypeDesc::Tvariant(inner_row) => row_more(ctx, inner_row),
        _ => more,
    }
}

/// Get the row representative.
///
/// Flattens nested variant rows by combining their fields.
pub fn row_repr(ctx: &TypeContext<'_>, row: &RowDesc) -> RowDesc {
    row_repr_aux(ctx, &[], row)
}

fn row_repr_aux(ctx: &TypeContext<'_>, ll: &[Vec<(String, RowField)>], row: &RowDesc) -> RowDesc {
    let more = ctx.repr(row.row_more);
    match &*ctx.get_desc(more) {
        TypeDesc::Tvariant(inner_row) => {
            let f = &row.row_fields;
            let new_ll: Vec<Vec<(String, RowField)>> = if f.is_empty() {
                ll.to_vec()
            } else {
                let mut new_ll = vec![f.clone()];
                new_ll.extend(ll.iter().cloned());
                new_ll
            };
            row_repr_aux(ctx, &new_ll, inner_row)
        }
        _ => {
            if ll.is_empty() {
                row.clone()
            } else {
                let mut combined_fields = row.row_fields.clone();
                for fields in ll.iter().rev() {
                    combined_fields.extend(fields.iter().cloned());
                }
                RowDesc {
                    row_fields: combined_fields,
                    row_more: row.row_more,
                    row_closed: row.row_closed,
                    row_fixed: row.row_fixed,
                    row_name: row.row_name.clone(),
                }
            }
        }
    }
}

/// Get a specific field from a row by its tag.
pub fn row_field(ctx: &TypeContext<'_>, tag: &str, row: &RowDesc) -> RowField {
    // First check the current row's fields
    for (field_tag, field) in &row.row_fields {
        if field_tag == tag {
            return row_field_repr(field);
        }
    }

    // Check nested variant
    let more = ctx.repr(row.row_more);
    match &*ctx.get_desc(more) {
        TypeDesc::Tvariant(inner_row) => row_field(ctx, tag, inner_row),
        _ => RowField::Rabsent,
    }
}

/// Check if a row is fixed.
pub fn row_fixed(ctx: &TypeContext<'_>, row: &RowDesc) -> bool {
    let row = row_repr(ctx, row);
    if row.row_fixed {
        return true;
    }

    let more = ctx.repr(row.row_more);
    match &*ctx.get_desc(more) {
        TypeDesc::Tvar(_) | TypeDesc::Tnil => false,
        TypeDesc::Tunivar(_) | TypeDesc::Tconstr { .. } => true,
        _ => false,
    }
}

/// Check if a row is static (closed with no Reither fields).
pub fn static_row(ctx: &TypeContext<'_>, row: &RowDesc) -> bool {
    let row = row_repr(ctx, row);
    row.row_closed
        && row
            .row_fields
            .iter()
            .all(|(_, f)| !matches!(row_field_repr(f), RowField::Reither { .. }))
}

// ============================================================================
// Type Traversal
// ============================================================================

/// Iterate over all type expressions in a row.
pub fn iter_row<F>(ctx: &TypeContext<'_>, row: &RowDesc, mut f: F)
where
    F: FnMut(TypeExprRef),
{
    for (_, field) in &row.row_fields {
        match row_field_repr(field) {
            RowField::Rpresent(Some(ty)) => f(ty),
            RowField::Reither { types, .. } => {
                for ty in types {
                    f(ty);
                }
            }
            _ => {}
        }
    }

    let more = ctx.repr(row.row_more);
    match &*ctx.get_desc(more) {
        TypeDesc::Tvariant(inner_row) => iter_row(ctx, inner_row, f),
        TypeDesc::Tvar(_)
        | TypeDesc::Tunivar(_)
        | TypeDesc::Tsubst(_)
        | TypeDesc::Tconstr { .. }
        | TypeDesc::Tnil => {
            if let Some((_, types)) = &row.row_name {
                for ty in types {
                    f(*ty);
                }
            }
        }
        _ => {}
    }
}

/// Iterate over all immediate child type expressions.
pub fn iter_type_expr<F>(ctx: &TypeContext<'_>, ty: TypeExprRef, mut f: F)
where
    F: FnMut(TypeExprRef),
{
    // Extract children first, then process them
    enum Children {
        None,
        One(TypeExprRef),
        Two(TypeExprRef, TypeExprRef),
        Many(Vec<TypeExprRef>),
        Variant(RowDesc, TypeExprRef),
        Object(TypeExprRef, Option<Vec<TypeExprRef>>),
        Poly(TypeExprRef, Vec<TypeExprRef>),
    }

    let children = {
        let desc = ctx.get_desc(ty);
        match &*desc {
            TypeDesc::Tvar(_) => Children::None,
            TypeDesc::Tarrow { arg, ret, .. } => Children::Two(arg.typ, *ret),
            TypeDesc::Ttuple(types) => Children::Many(types.clone()),
            TypeDesc::Tconstr { args, .. } => Children::Many(args.clone()),
            TypeDesc::Tobject { fields, name, .. } => {
                let name_types = name.as_ref().map(|(_, types)| types.clone());
                Children::Object(*fields, name_types)
            }
            TypeDesc::Tvariant(row) => {
                let row_clone = row.clone();
                let more = row.row_more;
                Children::Variant(row_clone, more)
            }
            TypeDesc::Tfield { typ, rest, .. } => Children::Two(*typ, *rest),
            TypeDesc::Tnil => Children::None,
            TypeDesc::Tlink(t) => Children::One(*t),
            TypeDesc::Tsubst(t) => Children::One(*t),
            TypeDesc::Tunivar(_) => Children::None,
            TypeDesc::Tpoly { body, vars } => Children::Poly(*body, vars.clone()),
            TypeDesc::Tpackage { args, .. } => Children::Many(args.clone()),
        }
    };

    // Now process children without holding the borrow
    match children {
        Children::None => {}
        Children::One(t) => f(t),
        Children::Two(t1, t2) => {
            f(t1);
            f(t2);
        }
        Children::Many(types) => {
            for t in types {
                f(t);
            }
        }
        Children::Variant(row, _more) => {
            iter_row(ctx, &row, &mut f);
            f(row_more(ctx, &row));
        }
        Children::Object(fields, name_types) => {
            f(fields);
            if let Some(types) = name_types {
                for t in types {
                    f(t);
                }
            }
        }
        Children::Poly(body, vars) => {
            f(body);
            for v in vars {
                f(v);
            }
        }
    }
}

/// Iterate over abbreviation memo.
pub fn iter_abbrev<F>(ctx: &TypeContext<'_>, memo: AbbrevMemoRef, f: F)
where
    F: FnMut(TypeExprRef),
{
    // TODO: Implement when we have access to the abbrev arena
    let _ = (ctx, memo, f);
}

// ============================================================================
// Type Kind Traversal
// ============================================================================

/// Iterate over constructor arguments.
pub fn iter_type_expr_cstr_args<F>(args: &ConstructorArguments, mut f: F)
where
    F: FnMut(TypeExprRef),
{
    match args {
        ConstructorArguments::CstrTuple(types) => {
            for t in types {
                f(*t);
            }
        }
        ConstructorArguments::CstrRecord(labels) => {
            for label in labels {
                f(label.ld_type);
            }
        }
    }
}

/// Iterate over type kind.
pub fn iter_type_expr_kind<F>(kind: &TypeKind, mut f: F)
where
    F: FnMut(TypeExprRef),
{
    match kind {
        TypeKind::TypeAbstract => {}
        TypeKind::TypeVariant(constructors) => {
            for cstr in constructors {
                iter_type_expr_cstr_args(&cstr.cd_args, &mut f);
                if let Some(res) = cstr.cd_res {
                    f(res);
                }
            }
        }
        TypeKind::TypeRecord(labels, _) => {
            for label in labels {
                f(label.ld_type);
            }
        }
        TypeKind::TypeOpen => {}
    }
}

// ============================================================================
// Type Marking (for traversal)
// ============================================================================

/// Mark a type by negating its level around pivot_level.
pub fn mark_type(ctx: &TypeContext<'_>, ty: TypeExprRef) {
    let ty = ctx.repr(ty);
    let level = ctx.get_level(ty);
    if level >= LOWEST_LEVEL {
        ctx.set_level(ty, PIVOT_LEVEL - level);
        iter_type_expr(ctx, ty, |t| mark_type(ctx, t));
    }
}

/// Mark just the type node (not its children).
pub fn mark_type_node(ctx: &TypeContext<'_>, ty: TypeExprRef) {
    let ty = ctx.repr(ty);
    let level = ctx.get_level(ty);
    if level >= LOWEST_LEVEL {
        ctx.set_level(ty, PIVOT_LEVEL - level);
    }
}

/// Mark type parameters (children only).
pub fn mark_type_params(ctx: &TypeContext<'_>, ty: TypeExprRef) {
    iter_type_expr(ctx, ty, |t| mark_type(ctx, t));
}

/// Unmark a type by reversing the mark operation.
pub fn unmark_type(ctx: &TypeContext<'_>, ty: TypeExprRef) {
    let ty = ctx.repr(ty);
    let level = ctx.get_level(ty);
    if level < LOWEST_LEVEL {
        ctx.set_level(ty, PIVOT_LEVEL - level);
        iter_type_expr(ctx, ty, |t| unmark_type(ctx, t));
    }
}

// ============================================================================
// Proxy and Row Utilities
// ============================================================================

/// Get the proxy for a type (used for object types).
pub fn proxy(ctx: &TypeContext<'_>, ty: TypeExprRef) -> TypeExprRef {
    let ty0 = ctx.repr(ty);

    // Extract what we need in a separate scope
    enum ProxyAction {
        VariantRow(RowDesc),
        ObjectFields(TypeExprRef),
        Return,
    }

    let action = {
        let desc = ctx.get_desc(ty0);
        match &*desc {
            TypeDesc::Tvariant(row) => {
                // Clone the row to check static_row outside the borrow
                ProxyAction::VariantRow(row.clone())
            }
            TypeDesc::Tobject { fields, .. } => ProxyAction::ObjectFields(*fields),
            _ => ProxyAction::Return,
        }
    };

    match action {
        ProxyAction::VariantRow(row) => {
            if !static_row(ctx, &row) {
                row_more(ctx, &row)
            } else {
                ty0
            }
        }
        ProxyAction::ObjectFields(fields) => proxy_obj(ctx, fields, ty0),
        ProxyAction::Return => ty0,
    }
}

fn proxy_obj(ctx: &TypeContext<'_>, ty: TypeExprRef, original: TypeExprRef) -> TypeExprRef {
    let desc = ctx.get_desc(ty);
    match &*desc {
        TypeDesc::Tfield { rest, .. } => {
            let rest = *rest;
            drop(desc);
            proxy_obj(ctx, rest, original)
        }
        TypeDesc::Tlink(t) => {
            let t = *t;
            drop(desc);
            proxy_obj(ctx, t, original)
        }
        TypeDesc::Tvar(_) | TypeDesc::Tunivar(_) | TypeDesc::Tconstr { .. } => ty,
        TypeDesc::Tnil => original,
        _ => original,
    }
}

/// Get the row type from a type.
pub fn row_of_type(ctx: &TypeContext<'_>, t: TypeExprRef) -> TypeExprRef {
    let t = ctx.repr(t);

    // Extract what we need in a separate scope
    enum RowAction {
        ObjectFields(TypeExprRef),
        VariantRow(RowDesc),
        Return,
    }

    let action = {
        let desc = ctx.get_desc(t);
        match &*desc {
            TypeDesc::Tobject { fields, .. } => RowAction::ObjectFields(*fields),
            TypeDesc::Tvariant(row) => RowAction::VariantRow(row.clone()),
            _ => RowAction::Return,
        }
    };

    match action {
        RowAction::ObjectFields(fields) => get_row(ctx, fields),
        RowAction::VariantRow(row) => row_more(ctx, &row),
        RowAction::Return => t,
    }
}

fn get_row(ctx: &TypeContext<'_>, t: TypeExprRef) -> TypeExprRef {
    let t = ctx.repr(t);
    let desc = ctx.get_desc(t);
    match &*desc {
        TypeDesc::Tfield { rest, .. } => {
            let rest = *rest;
            drop(desc);
            get_row(ctx, rest)
        }
        _ => t,
    }
}

/// Check if a type has a constructor row.
pub fn has_constr_row(ctx: &TypeContext<'_>, t: TypeExprRef) -> bool {
    !is_tconstr(ctx, t) && is_tconstr(ctx, row_of_type(ctx, t))
}

/// Check if a string is a row name (ends with "#row").
pub fn is_row_name(s: &str) -> bool {
    s.len() >= 4 && s.ends_with("#row")
}

/// Check if a type is a constructor row type.
pub fn is_constr_row(ctx: &TypeContext<'_>, t: TypeExprRef, allow_ident: bool) -> bool {
    let desc = ctx.get_desc(t);
    match &*desc {
        TypeDesc::Tconstr { path, .. } => match path {
            Path::Pident(id) if allow_ident => is_row_name(id.name()),
            Path::Pdot(_, s, _) => is_row_name(s),
            _ => false,
        },
        _ => false,
    }
}

// ============================================================================
// Hash Variant
// ============================================================================

/// Hash a variant tag name to an integer.
///
/// This is used for polymorphic variant tag representation.
pub fn hash_variant(s: &str) -> i32 {
    let mut accu: u32 = 0;
    for c in s.bytes() {
        accu = accu.wrapping_mul(223).wrapping_add(c as u32);
    }
    // Reduce to 31 bits
    accu &= (1 << 31) - 1;
    // Make it signed for 64-bit architectures
    if accu > 0x3FFFFFFF {
        (accu as i32) - (1 << 31)
    } else {
        accu as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::IdGenerator;

    #[test]
    fn test_is_tvar() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let tvar = ctx.new_var(Some("a".to_string()));
        assert!(is_tvar(&ctx, tvar));

        let tuple = ctx.new_tuple(vec![tvar]);
        assert!(!is_tvar(&ctx, tuple));
    }

    #[test]
    fn test_hash_variant() {
        // Test some known hash values
        assert_eq!(hash_variant("None"), 870530776);
        assert_eq!(hash_variant("Some"), 925978388);

        // Hash should be deterministic
        assert_eq!(hash_variant("foo"), hash_variant("foo"));

        // Different strings should have different hashes (usually)
        assert_ne!(hash_variant("foo"), hash_variant("bar"));
    }

    #[test]
    fn test_is_row_name() {
        assert!(is_row_name("foo#row"));
        assert!(is_row_name("#row"));
        assert!(!is_row_name("row"));
        assert!(!is_row_name("foo"));
        assert!(!is_row_name(""));
    }

    #[test]
    fn test_mark_unmark() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let tvar = ctx.new_var(None);
        let original_level = ctx.get_level(tvar);

        mark_type_node(&ctx, tvar);
        assert!(ctx.get_level(tvar) < LOWEST_LEVEL);

        unmark_type(&ctx, tvar);
        assert_eq!(ctx.get_level(tvar), original_level);
    }

    #[test]
    fn test_iter_type_expr_tuple() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let t1 = ctx.new_var(None);
        let t2 = ctx.new_var(None);
        let tuple = ctx.new_tuple(vec![t1, t2]);

        let mut visited = Vec::new();
        iter_type_expr(&ctx, tuple, |t| visited.push(t));

        assert_eq!(visited.len(), 2);
        assert!(visited.contains(&t1));
        assert!(visited.contains(&t2));
    }

    #[test]
    fn test_iter_type_expr_arrow() {
        let id_gen = IdGenerator::new();
        let ctx = TypeContext::new(&id_gen);

        let t1 = ctx.new_var(None);
        let t2 = ctx.new_var(None);
        let arrow = ctx.new_arrow_simple(t1, t2);

        let mut visited = Vec::new();
        iter_type_expr(&ctx, arrow, |t| visited.push(t));

        assert_eq!(visited.len(), 2);
    }
}
