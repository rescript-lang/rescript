//! Sexp printer for Typedtree - used for parity testing between OCaml and Rust compilers

use crate::ident::Ident;
use crate::location::Location;
use crate::parser::ast::{ArgLabel, Arity, Attribute, ClosedFlag, DirectionFlag, MutableFlag, OverrideFlag, Payload, PrivateFlag, RecFlag};
use crate::parser::longident::Longident;
use crate::types::asttypes::ArgLabel as TypeArgLabel;
use crate::types::context::TypeContext;
use crate::types::path::Path;
use crate::types::type_expr::{FieldKind, RowDesc, RowField, TypeDesc, TypeExprRef};
use crate::types::typedtree::{
    Case, Constant, Expression, ExpressionDesc, ExtensionConstructor,
    ExtensionConstructorKind, FunctionParam, FunctionParamPattern, ModuleBinding, ModuleExpr, ModuleExprDesc,
    ModuleTypeConstraint, OpenDeclaration, Partial, Pattern, PatternDesc,
    RecordLabelDefinition, Structure, StructureItem, StructureItemDesc,
    TypedCoreType, TypedCoreTypeDesc, ValueBinding, TypeExtension,
    TypedTypeDeclaration, TypedTypeKind, TypedConstructorDeclaration, TypedConstructorArguments,
    TypedLabelDeclaration,
};
use crate::types::decl::{
    TypeDeclaration, TypeKind, ConstructorDeclaration, LabelDeclaration, ConstructorArguments,
    Attribute as DeclAttribute,
};
use crate::types::asttypes::{PrivateFlag as DeclPrivateFlag, MutableFlag as DeclMutableFlag};
use crate::types::variance::Variance;
use crate::types::btype;
use std::io::Write;

// ============================================================================
// Sexp Module - simple string-based pretty printing
// ============================================================================

/// S-expression type
#[derive(Debug, Clone)]
pub enum Sexp {
    Atom(String),
    List(Vec<Sexp>),
}

impl Sexp {
    pub fn atom(s: &str) -> Self {
        Sexp::Atom(s.to_string())
    }

    pub fn list(items: Vec<Sexp>) -> Self {
        Sexp::List(items)
    }

    fn to_string_indent(&self, indent: usize) -> String {
        let spaces: String = " ".repeat(indent * 2);
        match self {
            Sexp::Atom(s) => s.clone(),
            Sexp::List(items) if items.is_empty() => "()".to_string(),
            Sexp::List(items) if items.len() == 1 => {
                if let Sexp::Atom(s) = &items[0] {
                    format!("({})", s)
                } else {
                    format!("({})", items[0].to_string_indent(indent))
                }
            }
            Sexp::List(items) => {
                let inner: Vec<String> = items
                    .iter()
                    .map(|item| item.to_string_indent(indent + 1))
                    .collect();
                let sep = format!("\n{}  ", spaces);
                format!("({})", inner.join(&sep))
            }
        }
    }

    pub fn to_string(&self) -> String {
        self.to_string_indent(0)
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn string(txt: &str) -> Sexp {
    Sexp::atom(&format!("\"{}\"", txt))
}

fn opt_string(opt: &Option<String>) -> Sexp {
    match opt {
        None => Sexp::atom("None"),
        Some(s) => Sexp::list(vec![Sexp::atom("Some"), string(s)]),
    }
}

fn opt<T, F>(f: F, opt: &Option<T>) -> Sexp
where
    F: Fn(&T) -> Sexp,
{
    match opt {
        None => Sexp::atom("None"),
        Some(x) => Sexp::list(vec![Sexp::atom("Some"), f(x)]),
    }
}

fn location(with_locs: bool, loc: &Location) -> Sexp {
    if with_locs {
        Sexp::list(vec![
            Sexp::atom("loc"),
            Sexp::atom(&loc.loc_start.line.to_string()),
            Sexp::atom(&loc.loc_start.column().to_string()),
            Sexp::atom(&loc.loc_end.line.to_string()),
            Sexp::atom(&loc.loc_end.column().to_string()),
        ])
    } else {
        Sexp::list(vec![])
    }
}

// ============================================================================
// Path Printer
// ============================================================================

fn sexp_path(p: &Path) -> Sexp {
    match p {
        Path::Pident(id) => Sexp::list(vec![Sexp::atom("Pident"), string(id.name())]),
        Path::Pdot(parent, s, _) => {
            Sexp::list(vec![Sexp::atom("Pdot"), sexp_path(parent), string(s)])
        }
        Path::Papply(p1, p2) => {
            Sexp::list(vec![Sexp::atom("Papply"), sexp_path(p1), sexp_path(p2)])
        }
    }
}

// ============================================================================
// Longident Printer
// ============================================================================

fn sexp_longident(li: &Longident) -> Sexp {
    match li {
        Longident::Lident(s) => Sexp::list(vec![Sexp::atom("Lident"), string(s)]),
        Longident::Ldot(parent, s) => {
            Sexp::list(vec![Sexp::atom("Ldot"), sexp_longident(parent), string(s)])
        }
        Longident::Lapply(l1, l2) => {
            Sexp::list(vec![Sexp::atom("Lapply"), sexp_longident(l1), sexp_longident(l2)])
        }
    }
}

// ============================================================================
// Ident Printer - just the name, skip stamp for determinism
// ============================================================================

fn sexp_ident(id: &Ident) -> Sexp {
    string(id.name())
}

// ============================================================================
// Constant Printer
// ============================================================================

fn sexp_constant(c: &Constant) -> Sexp {
    match c {
        Constant::Int(i) => Sexp::list(vec![Sexp::atom("Const_int"), Sexp::atom(&i.to_string())]),
        Constant::Char(c) => Sexp::list(vec![Sexp::atom("Const_char"), Sexp::atom(&c.to_string())]),
        Constant::String(s, delim) => {
            Sexp::list(vec![Sexp::atom("Const_string"), string(s), opt(|d: &String| string(d), delim)])
        }
        Constant::Float(s) => Sexp::list(vec![Sexp::atom("Const_float"), string(s)]),
        Constant::BigInt(positive, s) => {
            let sign_str = if *positive { "+" } else { "-" };
            Sexp::list(vec![Sexp::atom("Const_bigint"), Sexp::atom(sign_str), string(s)])
        }
    }
}

// ============================================================================
// Flags
// ============================================================================

fn sexp_rec_flag(flag: &RecFlag) -> Sexp {
    match flag {
        RecFlag::Nonrecursive => Sexp::atom("Nonrecursive"),
        RecFlag::Recursive => Sexp::atom("Recursive"),
    }
}

fn sexp_direction_flag(flag: &DirectionFlag) -> Sexp {
    match flag {
        DirectionFlag::Upto => Sexp::atom("Upto"),
        DirectionFlag::Downto => Sexp::atom("Downto"),
    }
}

fn sexp_private_flag(flag: &PrivateFlag) -> Sexp {
    match flag {
        PrivateFlag::Public => Sexp::atom("Public"),
        PrivateFlag::Private => Sexp::atom("Private"),
    }
}

fn sexp_mutable_flag(flag: &MutableFlag) -> Sexp {
    match flag {
        MutableFlag::Immutable => Sexp::atom("Immutable"),
        MutableFlag::Mutable => Sexp::atom("Mutable"),
    }
}

// Versions for decl.rs types (asttypes module)
fn sexp_decl_private_flag(flag: &DeclPrivateFlag) -> Sexp {
    match flag {
        DeclPrivateFlag::Public => Sexp::atom("Public"),
        DeclPrivateFlag::Private => Sexp::atom("Private"),
    }
}

fn sexp_decl_mutable_flag(flag: &DeclMutableFlag) -> Sexp {
    match flag {
        DeclMutableFlag::Immutable => Sexp::atom("Immutable"),
        DeclMutableFlag::Mutable => Sexp::atom("Mutable"),
    }
}

/// Convert Arity to OCaml-style Option<int> sexp
fn sexp_arity(arity: &Arity) -> Sexp {
    match arity {
        Arity::Full(n) => Sexp::list(vec![Sexp::atom("Some"), Sexp::atom(&n.to_string())]),
        Arity::Unknown => Sexp::atom("None"),
    }
}

/// Extract the param ident from a function param pattern.
/// Returns the ident from Tpat_var, or creates a dummy ident for other patterns.
fn extract_param_ident(param: &FunctionParam) -> Ident {
    match &param.pat {
        FunctionParamPattern::Simple(pat) => {
            match &pat.pat_desc {
                PatternDesc::Tpat_var(id, _) => id.clone(),
                PatternDesc::Tpat_alias(_, id, _) => id.clone(),
                // For other patterns, create a synthetic ident
                _ => Ident::create_persistent("param"),
            }
        }
        FunctionParamPattern::Nested(_) => Ident::create_persistent("param"),
    }
}

// Attribute helper for decl.rs Attribute type
fn sexp_decl_attributes(_with_locs: bool, attrs: &[DeclAttribute]) -> Sexp {
    if attrs.is_empty() {
        Sexp::list(vec![Sexp::atom("attributes")])
    } else {
        Sexp::list(vec![
            Sexp::atom("attributes"),
            Sexp::list(attrs.iter().map(|attr| {
                Sexp::list(vec![
                    string(&attr.attr_name.txt),
                    Sexp::atom("<payload>"),  // Simplified payload
                ])
            }).collect()),
        ])
    }
}

fn sexp_override_flag(flag: &OverrideFlag) -> Sexp {
    match flag {
        OverrideFlag::Override => Sexp::atom("Override"),
        OverrideFlag::Fresh => Sexp::atom("Fresh"),
    }
}

fn sexp_closed_flag(flag: &ClosedFlag) -> Sexp {
    match flag {
        ClosedFlag::Closed => Sexp::atom("Closed"),
        ClosedFlag::Open => Sexp::atom("Open"),
    }
}

fn sexp_arg_label(lbl: &ArgLabel) -> Sexp {
    match lbl {
        ArgLabel::Nolabel => Sexp::atom("Nolabel"),
        ArgLabel::Labelled(s) => Sexp::list(vec![Sexp::atom("Labelled"), string(&s.txt)]),
        ArgLabel::Optional(s) => Sexp::list(vec![Sexp::atom("Optional"), string(&s.txt)]),
    }
}

// Separate function for type-level ArgLabel (from asttypes)
fn sexp_type_arg_label(lbl: &TypeArgLabel) -> Sexp {
    match lbl {
        TypeArgLabel::Nolabel => Sexp::atom("Nolabel"),
        TypeArgLabel::Labelled(s) => Sexp::list(vec![Sexp::atom("Labelled"), string(&s.txt)]),
        TypeArgLabel::Optional(s) => Sexp::list(vec![Sexp::atom("Optional"), string(&s.txt)]),
    }
}

fn sexp_variance(v: &Variance) -> Sexp {
    if v.is_covariant() {
        Sexp::atom("Covariant")
    } else if v.is_contravariant() {
        Sexp::atom("Contravariant")
    } else {
        Sexp::atom("Invariant")
    }
}

fn sexp_partial(p: &Partial) -> Sexp {
    match p {
        Partial::Partial => Sexp::atom("Partial"),
        Partial::Total => Sexp::atom("Total"),
    }
}

// ============================================================================
// Type Expression Printer - follows Tlink chains via ctx.repr
// ============================================================================

fn sexp_type_expr(ctx: &TypeContext<'_>, te: TypeExprRef) -> Sexp {
    let te = ctx.repr(te); // Follow Tlink chains
    let desc = ctx.get_desc(te);
    match &*desc {
        TypeDesc::Tvar(name) => Sexp::list(vec![Sexp::atom("Tvar"), opt_string(name)]),
        TypeDesc::Tarrow { arg, ret, arity, .. } => {
            Sexp::list(vec![
                Sexp::atom("Tarrow"),
                sexp_type_arg_label(&arg.lbl),
                sexp_type_expr(ctx, arg.typ),
                sexp_type_expr(ctx, *ret),
                opt(|a: &i32| Sexp::atom(&a.to_string()), arity),
            ])
        }
        TypeDesc::Ttuple(types) => {
            let mut items = vec![Sexp::atom("Ttuple")];
            items.extend(types.iter().map(|t| sexp_type_expr(ctx, *t)));
            Sexp::list(items)
        }
        TypeDesc::Tconstr { path, args, .. } => {
            Sexp::list(vec![
                Sexp::atom("Tconstr"),
                sexp_path(path),
                Sexp::list(args.iter().map(|t| sexp_type_expr(ctx, *t)).collect()),
            ])
        }
        TypeDesc::Tobject { fields, .. } => {
            Sexp::list(vec![Sexp::atom("Tobject"), sexp_type_expr(ctx, *fields)])
        }
        TypeDesc::Tfield { name, kind, typ, rest } => {
            let kind_str = match kind {
                FieldKind::Fpresent => "Fpresent",
                FieldKind::Fabsent => "Fabsent",
                FieldKind::Fvar(_) => "Fvar",
            };
            Sexp::list(vec![
                Sexp::atom("Tfield"),
                string(name),
                Sexp::atom(kind_str),
                sexp_type_expr(ctx, *typ),
                sexp_type_expr(ctx, *rest),
            ])
        }
        TypeDesc::Tnil => Sexp::atom("Tnil"),
        TypeDesc::Tlink(_) => Sexp::atom("Tlink"), // Should not happen after ctx.repr
        TypeDesc::Tsubst(ty) => Sexp::list(vec![Sexp::atom("Tsubst"), sexp_type_expr(ctx, *ty)]),
        TypeDesc::Tvariant(row) => sexp_row_desc(ctx, row),
        TypeDesc::Tunivar(name) => Sexp::list(vec![Sexp::atom("Tunivar"), opt_string(name)]),
        TypeDesc::Tpoly { body, vars } => {
            Sexp::list(vec![
                Sexp::atom("Tpoly"),
                sexp_type_expr(ctx, *body),
                Sexp::list(vars.iter().map(|t| sexp_type_expr(ctx, *t)).collect()),
            ])
        }
        TypeDesc::Tpackage { path, lids, args } => {
            Sexp::list(vec![
                Sexp::atom("Tpackage"),
                sexp_path(path),
                Sexp::list(lids.iter().map(sexp_longident).collect()),
                Sexp::list(args.iter().map(|t| sexp_type_expr(ctx, *t)).collect()),
            ])
        }
    }
}

fn sexp_row_desc(ctx: &TypeContext<'_>, row: &RowDesc) -> Sexp {
    let row = btype::row_repr(ctx, row);
    Sexp::list(vec![
        Sexp::atom("Tvariant"),
        Sexp::list(
            row.row_fields
                .iter()
                .map(|(l, rf)| Sexp::list(vec![string(l), sexp_row_field(ctx, rf)]))
                .collect(),
        ),
        Sexp::atom(if row.row_closed { "closed" } else { "open" }),
        Sexp::atom(if row.row_fixed { "fixed" } else { "not_fixed" }),
    ])
}

fn sexp_row_field(ctx: &TypeContext<'_>, rf: &RowField) -> Sexp {
    let rf = btype::row_field_repr(rf);
    match rf {
        RowField::Rpresent(None) => Sexp::atom("Rpresent_none"),
        RowField::Rpresent(Some(ty)) => {
            Sexp::list(vec![Sexp::atom("Rpresent"), sexp_type_expr(ctx, ty)])
        }
        RowField::Reither { constant, types, .. } => {
            Sexp::list(vec![
                Sexp::atom("Reither"),
                Sexp::atom(if constant { "const" } else { "non_const" }),
                Sexp::list(types.iter().map(|t| sexp_type_expr(ctx, *t)).collect()),
            ])
        }
        RowField::Rabsent => Sexp::atom("Rabsent"),
    }
}

// ============================================================================
// Pattern
// ============================================================================

fn sexp_pattern(ctx: &TypeContext<'_>, with_locs: bool, p: &Pattern) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &p.pat_loc)]
    } else {
        vec![]
    };
    let extras: Vec<Sexp> = p.pat_extra.iter().map(|(extra, loc, attrs)| {
        sexp_pat_extra(ctx, with_locs, extra, loc, attrs)
    }).collect();
    let desc = sexp_pattern_desc(ctx, with_locs, &p.pat_desc);
    let ty = sexp_type_expr(ctx, p.pat_type);

    let mut items = vec![Sexp::atom("pattern")];
    items.extend(loc_sexp);
    items.extend(extras);
    items.push(desc);
    items.push(Sexp::list(vec![Sexp::atom("type"), ty]));
    Sexp::list(items)
}

fn sexp_pat_extra(
    ctx: &TypeContext<'_>,
    with_locs: bool,
    extra: &crate::types::typedtree::PatExtra,
    loc: &Location,
    attrs: &[Attribute],
) -> Sexp {
    use crate::types::typedtree::PatExtra;

    let loc_sexp = if with_locs {
        vec![location(with_locs, loc)]
    } else {
        vec![]
    };
    let attrs_sexp = sexp_attributes(with_locs, attrs);

    match extra {
        PatExtra::Tpat_constraint(ct) => {
            let mut items = vec![Sexp::atom("Tpat_constraint"), sexp_core_type(ctx, with_locs, ct)];
            items.extend(loc_sexp);
            items.push(attrs_sexp);
            Sexp::list(items)
        }
        PatExtra::Tpat_type(p, _) => {
            let mut items = vec![Sexp::atom("Tpat_type"), sexp_path(p)];
            items.extend(loc_sexp);
            items.push(attrs_sexp);
            Sexp::list(items)
        }
        PatExtra::Tpat_open(p, _, _) => {
            let mut items = vec![Sexp::atom("Tpat_open"), sexp_path(p)];
            items.extend(loc_sexp);
            items.push(attrs_sexp);
            Sexp::list(items)
        }
        PatExtra::Tpat_unpack => {
            let mut items = vec![Sexp::atom("Tpat_unpack")];
            items.extend(loc_sexp);
            items.push(attrs_sexp);
            Sexp::list(items)
        }
    }
}

fn sexp_pattern_desc(ctx: &TypeContext<'_>, with_locs: bool, desc: &PatternDesc) -> Sexp {
    match desc {
        PatternDesc::Tpat_any => Sexp::atom("Tpat_any"),
        PatternDesc::Tpat_var(id, _) => {
            Sexp::list(vec![Sexp::atom("Tpat_var"), sexp_ident(id)])
        }
        PatternDesc::Tpat_alias(p, id, _) => {
            Sexp::list(vec![
                Sexp::atom("Tpat_alias"),
                sexp_pattern(ctx, with_locs, p),
                sexp_ident(id),
            ])
        }
        PatternDesc::Tpat_constant(c) => {
            Sexp::list(vec![Sexp::atom("Tpat_constant"), sexp_constant(c)])
        }
        PatternDesc::Tpat_tuple(pats) => {
            let mut items = vec![Sexp::atom("Tpat_tuple")];
            items.extend(pats.iter().map(|p| sexp_pattern(ctx, with_locs, p)));
            Sexp::list(items)
        }
        PatternDesc::Tpat_construct(li, _, args) => {
            Sexp::list(vec![
                Sexp::atom("Tpat_construct"),
                sexp_longident(&li.txt),
                Sexp::list(args.iter().map(|p| sexp_pattern(ctx, with_locs, p)).collect()),
            ])
        }
        PatternDesc::Tpat_variant(label, arg, _) => {
            Sexp::list(vec![
                Sexp::atom("Tpat_variant"),
                string(label),
                opt(|p: &Box<Pattern>| sexp_pattern(ctx, with_locs, p), arg),
            ])
        }
        PatternDesc::Tpat_record(fields, flag) => {
            Sexp::list(vec![
                Sexp::atom("Tpat_record"),
                Sexp::list(
                    fields
                        .iter()
                        .map(|(li, _, p, opt_flag)| {
                            Sexp::list(vec![
                                sexp_longident(&li.txt),
                                sexp_pattern(ctx, with_locs, p),
                                Sexp::atom(if *opt_flag { "optional" } else { "required" }),
                            ])
                        })
                        .collect(),
                ),
                sexp_closed_flag(flag),
            ])
        }
        PatternDesc::Tpat_array(pats) => {
            let mut items = vec![Sexp::atom("Tpat_array")];
            items.extend(pats.iter().map(|p| sexp_pattern(ctx, with_locs, p)));
            Sexp::list(items)
        }
        PatternDesc::Tpat_or(p1, p2, _) => {
            Sexp::list(vec![
                Sexp::atom("Tpat_or"),
                sexp_pattern(ctx, with_locs, p1),
                sexp_pattern(ctx, with_locs, p2),
            ])
        }
    }
}

// ============================================================================
// Expression
// ============================================================================

fn sexp_expression(ctx: &TypeContext<'_>, with_locs: bool, e: &Expression) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &e.exp_loc)]
    } else {
        vec![]
    };
    let extras: Vec<Sexp> = e.exp_extra.iter().map(|(extra, loc, attrs)| {
        sexp_exp_extra(ctx, with_locs, extra, loc, attrs)
    }).collect();
    let desc = sexp_expression_desc(ctx, with_locs, &e.exp_desc);
    let ty = sexp_type_expr(ctx, e.exp_type);
    let attrs = sexp_attributes(with_locs, &e.exp_attributes);

    let mut items = vec![Sexp::atom("expression")];
    items.extend(loc_sexp);
    items.extend(extras);
    items.push(desc);
    items.push(Sexp::list(vec![Sexp::atom("type"), ty]));
    items.push(attrs);
    Sexp::list(items)
}

fn sexp_exp_extra(
    ctx: &TypeContext<'_>,
    with_locs: bool,
    extra: &crate::types::typedtree::ExpExtra,
    loc: &Location,
    attrs: &[Attribute],
) -> Sexp {
    use crate::types::typedtree::ExpExtra;

    let loc_sexp = if with_locs {
        vec![location(with_locs, loc)]
    } else {
        vec![]
    };
    let attrs_sexp = sexp_attributes(with_locs, attrs);

    match extra {
        ExpExtra::Texp_constraint(ct) => {
            let mut items = vec![Sexp::atom("Texp_constraint"), sexp_core_type(ctx, with_locs, ct)];
            items.extend(loc_sexp);
            items.push(attrs_sexp);
            Sexp::list(items)
        }
        ExpExtra::Texp_coerce(ct1, ct2) => {
            let mut items = vec![
                Sexp::atom("Texp_coerce"),
                opt(|c: &TypedCoreType| sexp_core_type(ctx, with_locs, c), ct1),
                sexp_core_type(ctx, with_locs, ct2),
            ];
            items.extend(loc_sexp);
            items.push(attrs_sexp);
            Sexp::list(items)
        }
        ExpExtra::Texp_open(flag, p, _, _) => {
            let mut items = vec![Sexp::atom("Texp_open"), sexp_override_flag(flag), sexp_path(p)];
            items.extend(loc_sexp);
            items.push(attrs_sexp);
            Sexp::list(items)
        }
        ExpExtra::Texp_newtype(s) => {
            let mut items = vec![Sexp::atom("Texp_newtype"), string(s)];
            items.extend(loc_sexp);
            items.push(attrs_sexp);
            Sexp::list(items)
        }
    }
}

fn sexp_expression_desc(ctx: &TypeContext<'_>, with_locs: bool, desc: &ExpressionDesc) -> Sexp {
    match desc {
        ExpressionDesc::Texp_ident(p, li, _) => {
            Sexp::list(vec![Sexp::atom("Texp_ident"), sexp_path(p), sexp_longident(&li.txt)])
        }
        ExpressionDesc::Texp_constant(c) => {
            Sexp::list(vec![Sexp::atom("Texp_constant"), sexp_constant(c)])
        }
        ExpressionDesc::Texp_let(flag, vbs, body) => {
            Sexp::list(vec![
                Sexp::atom("Texp_let"),
                sexp_rec_flag(flag),
                Sexp::list(vbs.iter().map(|vb| sexp_value_binding(ctx, with_locs, vb)).collect()),
                sexp_expression(ctx, with_locs, body),
            ])
        }
        ExpressionDesc::Texp_function { params, body, partial, arity, async_ } => {
            // OCaml Texp_function has: arg_label, arity (Option<int>), param (Ident), case, partial, async
            // Rust has: params (Vec), body (Vec<Case>), partial, arity, async_
            // For parity, we need to output in OCaml's format

            // Handle single-param single-case (common case)
            if params.len() == 1 && body.len() == 1 {
                let param = &params[0];
                let case = &body[0];
                let param_ident = extract_param_ident(param);

                Sexp::list(vec![
                    Sexp::atom("Texp_function"),
                    sexp_arg_label(&param.label),
                    sexp_arity(arity),
                    sexp_ident(&param_ident),
                    sexp_case(ctx, with_locs, case),
                    sexp_partial(partial),
                    Sexp::atom(if *async_ { "async" } else { "sync" }),
                ])
            } else {
                // Fallback for complex cases - output in a way that shows the structure
                // This won't match OCaml for multi-param functions, but provides debugging info
                Sexp::list(vec![
                    Sexp::atom("Texp_function"),
                    Sexp::list(params.iter().map(|p| {
                        Sexp::list(vec![
                            sexp_arg_label(&p.label),
                            opt(|e: &Box<Expression>| sexp_expression(ctx, with_locs, e), &p.default),
                        ])
                    }).collect()),
                    Sexp::list(body.iter().map(|c| sexp_case(ctx, with_locs, c)).collect()),
                    sexp_partial(partial),
                    sexp_arity(arity),
                    Sexp::atom(if *async_ { "async" } else { "sync" }),
                ])
            }
        }
        ExpressionDesc::Texp_apply { funct, args, partial, transformed_jsx } => {
            Sexp::list(vec![
                Sexp::atom("Texp_apply"),
                sexp_expression(ctx, with_locs, funct),
                Sexp::list(
                    args.iter()
                        .map(|arg| {
                            // OCaml has (arg_label * expression option), so we wrap expression in Some
                            Sexp::list(vec![
                                sexp_arg_label(&arg.label),
                                Sexp::list(vec![
                                    Sexp::atom("Some"),
                                    sexp_expression(ctx, with_locs, &arg.expression),
                                ]),
                            ])
                        })
                        .collect(),
                ),
                Sexp::atom(if *partial { "partial" } else { "total" }),
                Sexp::atom(if *transformed_jsx { "jsx" } else { "not_jsx" }),
            ])
        }
        ExpressionDesc::Texp_match(e, cases, exn_cases, p) => {
            Sexp::list(vec![
                Sexp::atom("Texp_match"),
                sexp_expression(ctx, with_locs, e),
                Sexp::list(cases.iter().map(|c| sexp_case(ctx, with_locs, c)).collect()),
                Sexp::list(exn_cases.iter().map(|c| sexp_case(ctx, with_locs, c)).collect()),
                sexp_partial(p),
            ])
        }
        ExpressionDesc::Texp_try(e, cases) => {
            Sexp::list(vec![
                Sexp::atom("Texp_try"),
                sexp_expression(ctx, with_locs, e),
                Sexp::list(cases.iter().map(|c| sexp_case(ctx, with_locs, c)).collect()),
            ])
        }
        ExpressionDesc::Texp_tuple(exprs) => {
            let mut items = vec![Sexp::atom("Texp_tuple")];
            items.extend(exprs.iter().map(|e| sexp_expression(ctx, with_locs, e)));
            Sexp::list(items)
        }
        ExpressionDesc::Texp_construct(li, _, args) => {
            Sexp::list(vec![
                Sexp::atom("Texp_construct"),
                sexp_longident(&li.txt),
                Sexp::list(args.iter().map(|e| sexp_expression(ctx, with_locs, e)).collect()),
            ])
        }
        ExpressionDesc::Texp_variant(label, arg) => {
            Sexp::list(vec![
                Sexp::atom("Texp_variant"),
                string(label),
                opt(|e: &Box<Expression>| sexp_expression(ctx, with_locs, e), arg),
            ])
        }
        ExpressionDesc::Texp_record { fields, extended_expression } => {
            Sexp::list(vec![
                Sexp::atom("Texp_record"),
                Sexp::list(
                    fields
                        .iter()
                        .map(|(_li, ld, def)| {
                            let def_sexp = match def {
                                RecordLabelDefinition::Kept(ty) => {
                                    Sexp::list(vec![Sexp::atom("Kept"), sexp_type_expr(ctx, *ty)])
                                }
                                RecordLabelDefinition::Overridden(li, e) => {
                                    Sexp::list(vec![
                                        Sexp::atom("Overridden"),
                                        sexp_longident(&li.txt),
                                        sexp_expression(ctx, with_locs, e),
                                    ])
                                }
                            };
                            // OCaml format: (lbl_name def_sexp "optional"/"required")
                            Sexp::list(vec![
                                string(&ld.lbl_name),
                                def_sexp,
                                Sexp::atom(if ld.lbl_optional { "optional" } else { "required" }),
                            ])
                        })
                        .collect(),
                ),
                opt(|e: &Box<Expression>| sexp_expression(ctx, with_locs, e), extended_expression),
            ])
        }
        ExpressionDesc::Texp_field(e, li, _) => {
            Sexp::list(vec![
                Sexp::atom("Texp_field"),
                sexp_expression(ctx, with_locs, e),
                sexp_longident(&li.txt),
            ])
        }
        ExpressionDesc::Texp_setfield(e1, li, _, e2) => {
            Sexp::list(vec![
                Sexp::atom("Texp_setfield"),
                sexp_expression(ctx, with_locs, e1),
                sexp_longident(&li.txt),
                sexp_expression(ctx, with_locs, e2),
            ])
        }
        ExpressionDesc::Texp_array(exprs) => {
            let mut items = vec![Sexp::atom("Texp_array")];
            items.extend(exprs.iter().map(|e| sexp_expression(ctx, with_locs, e)));
            Sexp::list(items)
        }
        ExpressionDesc::Texp_ifthenelse(e1, e2, e3) => {
            Sexp::list(vec![
                Sexp::atom("Texp_ifthenelse"),
                sexp_expression(ctx, with_locs, e1),
                sexp_expression(ctx, with_locs, e2),
                opt(|e: &Box<Expression>| sexp_expression(ctx, with_locs, e), e3),
            ])
        }
        ExpressionDesc::Texp_sequence(e1, e2) => {
            Sexp::list(vec![
                Sexp::atom("Texp_sequence"),
                sexp_expression(ctx, with_locs, e1),
                sexp_expression(ctx, with_locs, e2),
            ])
        }
        ExpressionDesc::Texp_while(e1, e2) => {
            Sexp::list(vec![
                Sexp::atom("Texp_while"),
                sexp_expression(ctx, with_locs, e1),
                sexp_expression(ctx, with_locs, e2),
            ])
        }
        ExpressionDesc::Texp_for(id, _, e1, e2, flag, e3) => {
            Sexp::list(vec![
                Sexp::atom("Texp_for"),
                sexp_ident(id),
                sexp_expression(ctx, with_locs, e1),
                sexp_expression(ctx, with_locs, e2),
                sexp_direction_flag(flag),
                sexp_expression(ctx, with_locs, e3),
            ])
        }
        ExpressionDesc::Texp_send(e, meth) => {
            use crate::types::typedtree::MethKind;
            let meth_sexp = match meth {
                MethKind::Tmeth_name(name) => string(name),
                MethKind::Tmeth_val(id) => sexp_ident(id),
            };
            Sexp::list(vec![Sexp::atom("Texp_send"), sexp_expression(ctx, with_locs, e), meth_sexp])
        }
        ExpressionDesc::Texp_letmodule(id, _, me, e) => {
            Sexp::list(vec![
                Sexp::atom("Texp_letmodule"),
                sexp_ident(id),
                sexp_module_expr(ctx, with_locs, me),
                sexp_expression(ctx, with_locs, e),
            ])
        }
        ExpressionDesc::Texp_letexception(ec, e) => {
            Sexp::list(vec![
                Sexp::atom("Texp_letexception"),
                sexp_extension_constructor(ctx, with_locs, ec),
                sexp_expression(ctx, with_locs, e),
            ])
        }
        ExpressionDesc::Texp_assert(e) => {
            Sexp::list(vec![Sexp::atom("Texp_assert"), sexp_expression(ctx, with_locs, e)])
        }
        ExpressionDesc::Texp_pack(me) => {
            Sexp::list(vec![Sexp::atom("Texp_pack"), sexp_module_expr(ctx, with_locs, me)])
        }
    }
}

fn sexp_case(ctx: &TypeContext<'_>, with_locs: bool, c: &Case) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("case"),
        sexp_pattern(ctx, with_locs, &c.c_lhs),
        opt(|e: &Expression| sexp_expression(ctx, with_locs, e), &c.c_guard),
        sexp_expression(ctx, with_locs, &c.c_rhs),
    ])
}

fn sexp_value_binding(ctx: &TypeContext<'_>, with_locs: bool, vb: &ValueBinding) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &vb.vb_loc)]
    } else {
        vec![]
    };
    let mut items = vec![Sexp::atom("value_binding")];
    items.extend(loc_sexp);
    items.push(sexp_pattern(ctx, with_locs, &vb.vb_pat));
    items.push(sexp_expression(ctx, with_locs, &vb.vb_expr));
    items.push(sexp_attributes(with_locs, &vb.vb_attributes));
    Sexp::list(items)
}

// ============================================================================
// Core Type
// ============================================================================

fn sexp_core_type(ctx: &TypeContext<'_>, with_locs: bool, ct: &TypedCoreType) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &ct.ctyp_loc)]
    } else {
        vec![]
    };
    let desc = sexp_core_type_desc(ctx, with_locs, &ct.ctyp_desc);
    let ty = sexp_type_expr(ctx, ct.ctyp_type);
    let attrs = sexp_attributes(with_locs, &ct.ctyp_attributes);

    let mut items = vec![Sexp::atom("core_type")];
    items.extend(loc_sexp);
    items.push(desc);
    items.push(Sexp::list(vec![Sexp::atom("type"), ty]));
    items.push(attrs);
    Sexp::list(items)
}

fn sexp_core_type_desc(ctx: &TypeContext<'_>, with_locs: bool, desc: &TypedCoreTypeDesc) -> Sexp {
    match desc {
        TypedCoreTypeDesc::Ttyp_var(s) => {
            match s {
                None => Sexp::atom("Ttyp_any"),
                Some(s) => Sexp::list(vec![Sexp::atom("Ttyp_var"), string(s)]),
            }
        }
        TypedCoreTypeDesc::Ttyp_arrow(arg, ret, arity) => {
            Sexp::list(vec![
                Sexp::atom("Ttyp_arrow"),
                sexp_arg_label(&arg.lbl),
                sexp_attributes(with_locs, &arg.attrs),
                sexp_core_type(ctx, with_locs, &*arg.typ),
                sexp_core_type(ctx, with_locs, ret),
                opt(|a: &i32| Sexp::atom(&a.to_string()), arity),
            ])
        }
        TypedCoreTypeDesc::Ttyp_tuple(types) => {
            let mut items = vec![Sexp::atom("Ttyp_tuple")];
            items.extend(types.iter().map(|t| sexp_core_type(ctx, with_locs, t)));
            Sexp::list(items)
        }
        TypedCoreTypeDesc::Ttyp_constr(p, _, args) => {
            Sexp::list(vec![
                Sexp::atom("Ttyp_constr"),
                sexp_path(p),
                Sexp::list(args.iter().map(|t| sexp_core_type(ctx, with_locs, t)).collect()),
            ])
        }
        TypedCoreTypeDesc::Ttyp_object(fields, flag) => {
            Sexp::list(vec![
                Sexp::atom("Ttyp_object"),
                Sexp::list(fields.iter().map(|f| sexp_object_field(ctx, with_locs, f)).collect()),
                sexp_closed_flag(flag),
            ])
        }
        TypedCoreTypeDesc::Ttyp_alias(ct, s) => {
            Sexp::list(vec![
                Sexp::atom("Ttyp_alias"),
                sexp_core_type(ctx, with_locs, ct),
                string(s),
            ])
        }
        TypedCoreTypeDesc::Ttyp_variant(fields, flag, labels) => {
            Sexp::list(vec![
                Sexp::atom("Ttyp_variant"),
                Sexp::list(fields.iter().map(|f| sexp_row_field_type(ctx, with_locs, f)).collect()),
                sexp_closed_flag(flag),
                opt(
                    |l: &Vec<String>| Sexp::list(l.iter().map(|s| string(s)).collect()),
                    labels,
                ),
            ])
        }
        TypedCoreTypeDesc::Ttyp_poly(vars, ct) => {
            Sexp::list(vec![
                Sexp::atom("Ttyp_poly"),
                Sexp::list(vars.iter().map(|s| string(s)).collect()),
                sexp_core_type(ctx, with_locs, ct),
            ])
        }
        TypedCoreTypeDesc::Ttyp_package(pkg) => {
            Sexp::list(vec![
                Sexp::atom("Ttyp_package"),
                sexp_path(&pkg.pack_path),
                Sexp::list(
                    pkg.pack_fields
                        .iter()
                        .map(|(li, ct)| {
                            Sexp::list(vec![sexp_longident(&li.txt), sexp_core_type(ctx, with_locs, ct)])
                        })
                        .collect(),
                ),
            ])
        }
    }
}

fn sexp_object_field(
    ctx: &TypeContext<'_>,
    with_locs: bool,
    field: &crate::types::typedtree::ObjectField,
) -> Sexp {
    use crate::types::typedtree::ObjectFieldDesc;
    match &field.of_desc {
        ObjectFieldDesc::OTtag(name, ct) => {
            Sexp::list(vec![
                Sexp::atom("OTtag"),
                string(&name.txt),
                sexp_attributes(with_locs, &field.of_attributes),
                sexp_core_type(ctx, with_locs, ct),
            ])
        }
        ObjectFieldDesc::OTinherit(ct) => {
            Sexp::list(vec![Sexp::atom("OTinherit"), sexp_core_type(ctx, with_locs, ct)])
        }
    }
}

fn sexp_row_field_type(
    ctx: &TypeContext<'_>,
    with_locs: bool,
    field: &crate::types::typedtree::RowFieldType,
) -> Sexp {
    use crate::types::typedtree::RowFieldDesc;
    match &field.rf_desc {
        RowFieldDesc::Rtag(name, constant, args) => {
            Sexp::list(vec![
                Sexp::atom("Ttag"),
                string(&name.txt),
                sexp_attributes(with_locs, &field.rf_attributes),
                Sexp::atom(if *constant { "const" } else { "non_const" }),
                Sexp::list(args.iter().map(|ct| sexp_core_type(ctx, with_locs, ct)).collect()),
            ])
        }
        RowFieldDesc::Rinherit(ct) => {
            Sexp::list(vec![Sexp::atom("Tinherit"), sexp_core_type(ctx, with_locs, ct)])
        }
    }
}

// ============================================================================
// Attributes
// ============================================================================

fn sexp_payload(payload: &Payload) -> Sexp {
    match payload {
        Payload::PStr(_) => Sexp::list(vec![Sexp::atom("PStr"), Sexp::atom("<structure>")]),
        Payload::PSig(_) => Sexp::list(vec![Sexp::atom("PSig"), Sexp::atom("<signature>")]),
        Payload::PTyp(_) => Sexp::list(vec![Sexp::atom("PTyp"), Sexp::atom("<core_type>")]),
        Payload::PPat(_, _) => Sexp::list(vec![Sexp::atom("PPat"), Sexp::atom("<pattern>")]),
    }
}

fn sexp_attribute(with_locs: bool, attr: &Attribute) -> Sexp {
    let _ = with_locs;
    let (name, payload) = attr;
    Sexp::list(vec![
        Sexp::atom("attribute"),
        string(&name.txt),
        sexp_payload(payload),
    ])
}

fn sexp_attributes(with_locs: bool, attrs: &[Attribute]) -> Sexp {
    let mut items = vec![Sexp::atom("attributes")];
    items.extend(attrs.iter().map(|a| sexp_attribute(with_locs, a)));
    Sexp::list(items)
}

// ============================================================================
// Module Expression
// ============================================================================

fn sexp_module_expr(ctx: &TypeContext<'_>, with_locs: bool, me: &ModuleExpr) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &me.mod_loc)]
    } else {
        vec![]
    };
    let desc = sexp_module_expr_desc(ctx, with_locs, &me.mod_desc);
    let attrs = sexp_attributes(with_locs, &me.mod_attributes);

    let mut items = vec![Sexp::atom("module_expr")];
    items.extend(loc_sexp);
    items.push(desc);
    items.push(attrs);
    Sexp::list(items)
}

fn sexp_module_expr_desc(ctx: &TypeContext<'_>, with_locs: bool, desc: &ModuleExprDesc) -> Sexp {
    match desc {
        ModuleExprDesc::Tmod_ident(p, _) => {
            Sexp::list(vec![Sexp::atom("Tmod_ident"), sexp_path(p)])
        }
        ModuleExprDesc::Tmod_structure(str) => {
            Sexp::list(vec![Sexp::atom("Tmod_structure"), sexp_structure(ctx, with_locs, str)])
        }
        ModuleExprDesc::Tmod_functor(id, _, mt_arg, me) => {
            Sexp::list(vec![
                Sexp::atom("Tmod_functor"),
                sexp_ident(id),
                Sexp::atom("<module_type>"), // Simplified
                sexp_module_expr(ctx, with_locs, me),
            ])
        }
        ModuleExprDesc::Tmod_apply(me1, me2, _) => {
            Sexp::list(vec![
                Sexp::atom("Tmod_apply"),
                sexp_module_expr(ctx, with_locs, me1),
                sexp_module_expr(ctx, with_locs, me2),
            ])
        }
        ModuleExprDesc::Tmod_constraint(me, _, constraint_, _) => {
            let constraint_sexp = match constraint_ {
                ModuleTypeConstraint::Tmodtype_implicit => Sexp::atom("implicit"),
                ModuleTypeConstraint::Tmodtype_explicit(_) => {
                    Sexp::list(vec![Sexp::atom("explicit"), Sexp::atom("<module_type>")])
                }
            };
            Sexp::list(vec![
                Sexp::atom("Tmod_constraint"),
                sexp_module_expr(ctx, with_locs, me),
                constraint_sexp,
            ])
        }
        ModuleExprDesc::Tmod_unpack(e, _) => {
            Sexp::list(vec![Sexp::atom("Tmod_unpack"), sexp_expression(ctx, with_locs, e)])
        }
    }
}

// ============================================================================
// Type Declarations (from decl.rs)
// ============================================================================

fn sexp_type_declaration(ctx: &TypeContext<'_>, with_locs: bool, td: &TypeDeclaration) -> Sexp {
    // Note: This prints TypeDeclaration from decl.rs, which is the internal representation.
    // OCaml's Typedtree.type_declaration has more fields (typ_id, typ_name, typ_params as core_type, etc.)
    // We print what we have available.
    let loc_sexp = if with_locs {
        vec![location(with_locs, &td.type_loc)]
    } else {
        vec![]
    };
    let mut items = vec![Sexp::atom("type_declaration")];
    items.extend(loc_sexp);
    items.push(Sexp::list(vec![
        Sexp::atom("params"),
        Sexp::list(td.type_params.iter().map(|te| sexp_type_expr(ctx, *te)).collect()),
    ]));
    items.push(Sexp::list(vec![
        Sexp::atom("kind"),
        sexp_type_kind(ctx, with_locs, &td.type_kind),
    ]));
    items.push(Sexp::list(vec![
        Sexp::atom("private"),
        sexp_decl_private_flag(&td.type_private),
    ]));
    items.push(Sexp::list(vec![
        Sexp::atom("manifest"),
        opt(|te: &TypeExprRef| sexp_type_expr(ctx, *te), &td.type_manifest),
    ]));
    items.push(sexp_decl_attributes(with_locs, &td.type_attributes));
    Sexp::list(items)
}

fn sexp_type_kind(ctx: &TypeContext<'_>, with_locs: bool, kind: &TypeKind) -> Sexp {
    match kind {
        TypeKind::TypeAbstract => Sexp::atom("Ttype_abstract"),
        TypeKind::TypeVariant(cds) => {
            let mut items = vec![Sexp::atom("Ttype_variant")];
            items.extend(cds.iter().map(|cd| sexp_constructor_decl(ctx, with_locs, cd)));
            Sexp::list(items)
        }
        TypeKind::TypeRecord(lds, _repr) => {
            let mut items = vec![Sexp::atom("Ttype_record")];
            items.extend(lds.iter().map(|ld| sexp_label_decl(ctx, with_locs, ld)));
            Sexp::list(items)
        }
        TypeKind::TypeOpen => Sexp::atom("Ttype_open"),
    }
}

fn sexp_constructor_decl(ctx: &TypeContext<'_>, with_locs: bool, cd: &ConstructorDeclaration) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &cd.cd_loc)]
    } else {
        vec![]
    };
    let mut items = vec![
        Sexp::atom("constructor_declaration"),
        sexp_ident(&cd.cd_id),
        string(&cd.cd_id.name()),
    ];
    items.extend(loc_sexp);
    items.push(sexp_constructor_args(ctx, with_locs, &cd.cd_args));
    items.push(opt(|te: &TypeExprRef| sexp_type_expr(ctx, *te), &cd.cd_res));
    items.push(sexp_decl_attributes(with_locs, &cd.cd_attributes));
    Sexp::list(items)
}

fn sexp_constructor_args(ctx: &TypeContext<'_>, with_locs: bool, args: &ConstructorArguments) -> Sexp {
    match args {
        ConstructorArguments::CstrTuple(types) => {
            let mut items = vec![Sexp::atom("Cstr_tuple")];
            items.extend(types.iter().map(|te| sexp_type_expr(ctx, *te)));
            Sexp::list(items)
        }
        ConstructorArguments::CstrRecord(lds) => {
            let mut items = vec![Sexp::atom("Cstr_record")];
            items.extend(lds.iter().map(|ld| sexp_label_decl(ctx, with_locs, ld)));
            Sexp::list(items)
        }
    }
}

fn sexp_label_decl(ctx: &TypeContext<'_>, with_locs: bool, ld: &LabelDeclaration) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &ld.ld_loc)]
    } else {
        vec![]
    };
    let mut items = vec![
        Sexp::atom("label_declaration"),
        sexp_ident(&ld.ld_id),
        string(&ld.ld_id.name()),
    ];
    items.extend(loc_sexp);
    items.push(sexp_decl_mutable_flag(&ld.ld_mutable));
    items.push(Sexp::atom(if ld.ld_optional { "optional" } else { "required" }));
    items.push(sexp_type_expr(ctx, ld.ld_type));
    items.push(sexp_decl_attributes(with_locs, &ld.ld_attributes));
    Sexp::list(items)
}

// ============================================================================
// Typed Tree Type Declarations (from typedtree.rs)
// ============================================================================

fn sexp_typed_type_declaration(ctx: &TypeContext<'_>, with_locs: bool, td: &TypedTypeDeclaration) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &td.typ_loc)]
    } else {
        vec![]
    };
    let mut items = vec![
        Sexp::atom("type_declaration"),
        sexp_ident(&td.typ_id),
        string(&td.typ_name.txt),
    ];
    items.extend(loc_sexp);
    items.push(Sexp::list(vec![
        Sexp::atom("params"),
        Sexp::list(td.typ_params.iter().map(|(ct, v)| {
            Sexp::list(vec![sexp_core_type(ctx, with_locs, ct), sexp_variance(v)])
        }).collect()),
    ]));
    items.push(Sexp::list(vec![
        Sexp::atom("cstrs"),
        Sexp::list(td.typ_cstrs.iter().map(|(ct1, ct2, _loc)| {
            Sexp::list(vec![
                sexp_core_type(ctx, with_locs, ct1),
                sexp_core_type(ctx, with_locs, ct2),
            ])
        }).collect()),
    ]));
    items.push(Sexp::list(vec![
        Sexp::atom("kind"),
        sexp_typed_type_kind(ctx, with_locs, &td.typ_kind),
    ]));
    items.push(Sexp::list(vec![
        Sexp::atom("private"),
        sexp_private_flag(&td.typ_private),
    ]));
    items.push(Sexp::list(vec![
        Sexp::atom("manifest"),
        opt(|ct: &TypedCoreType| sexp_core_type(ctx, with_locs, ct), &td.typ_manifest),
    ]));
    items.push(sexp_attributes(with_locs, &td.typ_attributes));
    Sexp::list(items)
}

fn sexp_typed_type_kind(ctx: &TypeContext<'_>, with_locs: bool, kind: &TypedTypeKind) -> Sexp {
    match kind {
        TypedTypeKind::Ttype_abstract => Sexp::atom("Ttype_abstract"),
        TypedTypeKind::Ttype_variant(cds) => {
            let mut items = vec![Sexp::atom("Ttype_variant")];
            items.extend(cds.iter().map(|cd| sexp_typed_constructor_decl(ctx, with_locs, cd)));
            Sexp::list(items)
        }
        TypedTypeKind::Ttype_record(lds) => {
            let mut items = vec![Sexp::atom("Ttype_record")];
            items.extend(lds.iter().map(|ld| sexp_typed_label_decl(ctx, with_locs, ld)));
            Sexp::list(items)
        }
        TypedTypeKind::Ttype_open => Sexp::atom("Ttype_open"),
    }
}

fn sexp_typed_constructor_decl(ctx: &TypeContext<'_>, with_locs: bool, cd: &TypedConstructorDeclaration) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &cd.cd_loc)]
    } else {
        vec![]
    };
    let mut items = vec![
        Sexp::atom("constructor_declaration"),
        sexp_ident(&cd.cd_id),
        string(&cd.cd_name.txt),
    ];
    items.extend(loc_sexp);
    items.push(sexp_typed_constructor_args(ctx, with_locs, &cd.cd_args));
    items.push(opt(|ct: &TypedCoreType| sexp_core_type(ctx, with_locs, ct), &cd.cd_res));
    items.push(sexp_attributes(with_locs, &cd.cd_attributes));
    Sexp::list(items)
}

fn sexp_typed_constructor_args(ctx: &TypeContext<'_>, with_locs: bool, args: &TypedConstructorArguments) -> Sexp {
    match args {
        TypedConstructorArguments::Cstr_tuple(types) => {
            let mut items = vec![Sexp::atom("Cstr_tuple")];
            items.extend(types.iter().map(|ct| sexp_core_type(ctx, with_locs, ct)));
            Sexp::list(items)
        }
        TypedConstructorArguments::Cstr_record(lds) => {
            let mut items = vec![Sexp::atom("Cstr_record")];
            items.extend(lds.iter().map(|ld| sexp_typed_label_decl(ctx, with_locs, ld)));
            Sexp::list(items)
        }
    }
}

fn sexp_typed_label_decl(ctx: &TypeContext<'_>, with_locs: bool, ld: &TypedLabelDeclaration) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &ld.ld_loc)]
    } else {
        vec![]
    };
    let mut items = vec![
        Sexp::atom("label_declaration"),
        sexp_ident(&ld.ld_id),
        string(&ld.ld_name.txt),
    ];
    items.extend(loc_sexp);
    items.push(sexp_mutable_flag(&ld.ld_mutable));
    items.push(Sexp::atom(if ld.ld_optional { "optional" } else { "required" }));
    items.push(sexp_core_type(ctx, with_locs, &ld.ld_type));
    items.push(sexp_attributes(with_locs, &ld.ld_attributes));
    Sexp::list(items)
}

fn sexp_type_extension(ctx: &TypeContext<'_>, with_locs: bool, te: &TypeExtension) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &te.tyext_loc)]
    } else {
        vec![]
    };
    let mut items = vec![
        Sexp::atom("type_extension"),
        sexp_path(&te.tyext_path),
        sexp_longident(&te.tyext_txt.txt),
    ];
    items.extend(loc_sexp);
    items.push(Sexp::list(vec![
        Sexp::atom("params"),
        Sexp::list(te.tyext_params.iter().map(|(ct, v)| {
            Sexp::list(vec![sexp_core_type(ctx, with_locs, ct), sexp_variance(v)])
        }).collect()),
    ]));
    items.push(Sexp::list(vec![
        Sexp::atom("constructors"),
        Sexp::list(te.tyext_constructors.iter().map(|ec| sexp_extension_constructor(ctx, with_locs, ec)).collect()),
    ]));
    items.push(sexp_private_flag(&te.tyext_private));
    items.push(sexp_attributes(with_locs, &te.tyext_attributes));
    Sexp::list(items)
}

// ============================================================================
// Extension Constructor
// ============================================================================

fn sexp_extension_constructor(ctx: &TypeContext<'_>, with_locs: bool, ec: &ExtensionConstructor) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &ec.ext_loc)]
    } else {
        vec![]
    };
    let kind = match &ec.ext_kind {
        ExtensionConstructorKind::Text_decl(args, ret) => {
            // args is Vec<ConstructorArgument> - print each argument
            let args_sexp = Sexp::list(
                args.iter()
                    .map(|arg| sexp_constructor_argument(ctx, with_locs, arg))
                    .collect(),
            );
            Sexp::list(vec![
                Sexp::atom("Text_decl"),
                args_sexp,
                opt(|ct: &TypedCoreType| sexp_core_type(ctx, with_locs, ct), ret),
            ])
        }
        ExtensionConstructorKind::Text_rebind(p, _) => {
            Sexp::list(vec![Sexp::atom("Text_rebind"), sexp_path(p)])
        }
    };

    let mut items = vec![
        Sexp::atom("extension_constructor"),
        sexp_ident(&ec.ext_id),
        string(&ec.ext_name.txt),
    ];
    items.extend(loc_sexp);
    items.push(kind);
    items.push(sexp_attributes(with_locs, &ec.ext_attributes));
    Sexp::list(items)
}

fn sexp_constructor_argument(
    ctx: &TypeContext<'_>,
    with_locs: bool,
    arg: &crate::types::typedtree::ConstructorArgument,
) -> Sexp {
    use crate::types::typedtree::ConstructorArgument;
    match arg {
        ConstructorArgument::Cstr_tuple(cts) => {
            let mut items = vec![Sexp::atom("Cstr_tuple")];
            items.extend(cts.iter().map(|ct| sexp_core_type(ctx, with_locs, ct)));
            Sexp::list(items)
        }
        ConstructorArgument::Cstr_record(lds) => {
            let mut items = vec![Sexp::atom("Cstr_record")];
            items.extend(lds.iter().map(|ld| sexp_label_declaration(ctx, with_locs, ld)));
            Sexp::list(items)
        }
    }
}

fn sexp_label_declaration(
    ctx: &TypeContext<'_>,
    with_locs: bool,
    ld: &crate::types::typedtree::LabelDeclaration,
) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &ld.ld_loc)]
    } else {
        vec![]
    };
    let mut items = vec![
        Sexp::atom("label_declaration"),
        sexp_ident(&ld.ld_id),
        string(&ld.ld_name.txt),
    ];
    items.extend(loc_sexp);
    items.push(sexp_mutable_flag(&ld.ld_mutable));
    items.push(sexp_core_type(ctx, with_locs, &ld.ld_type));
    items.push(sexp_attributes(with_locs, &ld.ld_attributes));
    Sexp::list(items)
}

// ============================================================================
// Structure
// ============================================================================

fn sexp_structure(ctx: &TypeContext<'_>, with_locs: bool, str: &Structure) -> Sexp {
    let mut items = vec![Sexp::atom("structure")];
    items.extend(
        str.str_items
            .iter()
            .map(|si| sexp_structure_item(ctx, with_locs, si)),
    );
    Sexp::list(items)
}

fn sexp_structure_item(ctx: &TypeContext<'_>, with_locs: bool, si: &StructureItem) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &si.str_loc)]
    } else {
        vec![]
    };
    let desc = sexp_structure_item_desc(ctx, with_locs, &si.str_desc);
    let mut items = vec![Sexp::atom("structure_item")];
    items.extend(loc_sexp);
    items.push(desc);
    Sexp::list(items)
}

fn sexp_structure_item_desc(ctx: &TypeContext<'_>, with_locs: bool, desc: &StructureItemDesc) -> Sexp {
    match desc {
        StructureItemDesc::Tstr_eval(e, attrs) => {
            Sexp::list(vec![
                Sexp::atom("Tstr_eval"),
                sexp_expression(ctx, with_locs, e),
                sexp_attributes(with_locs, attrs),
            ])
        }
        StructureItemDesc::Tstr_value(flag, vbs) => {
            Sexp::list(vec![
                Sexp::atom("Tstr_value"),
                sexp_rec_flag(flag),
                Sexp::list(vbs.iter().map(|vb| sexp_value_binding(ctx, with_locs, vb)).collect()),
            ])
        }
        StructureItemDesc::Tstr_primitive(vd) => {
            // Simplified - would need full value_description handling
            Sexp::list(vec![Sexp::atom("Tstr_primitive"), Sexp::atom("<value_description>")])
        }
        StructureItemDesc::Tstr_type(flag, tds) => {
            Sexp::list(vec![
                Sexp::atom("Tstr_type"),
                sexp_rec_flag(flag),
                Sexp::list(tds.iter().map(|td| sexp_typed_type_declaration(ctx, with_locs, td)).collect()),
            ])
        }
        StructureItemDesc::Tstr_typext(te) => {
            Sexp::list(vec![Sexp::atom("Tstr_typext"), sexp_type_extension(ctx, with_locs, te)])
        }
        StructureItemDesc::Tstr_exception(ec) => {
            Sexp::list(vec![
                Sexp::atom("Tstr_exception"),
                sexp_extension_constructor(ctx, with_locs, ec),
            ])
        }
        StructureItemDesc::Tstr_module(mb) => {
            Sexp::list(vec![
                Sexp::atom("Tstr_module"),
                sexp_module_binding(ctx, with_locs, mb),
            ])
        }
        StructureItemDesc::Tstr_recmodule(mbs) => {
            Sexp::list(vec![
                Sexp::atom("Tstr_recmodule"),
                Sexp::list(mbs.iter().map(|mb| sexp_module_binding(ctx, with_locs, mb)).collect()),
            ])
        }
        StructureItemDesc::Tstr_modtype(_mtd) => {
            Sexp::list(vec![Sexp::atom("Tstr_modtype"), Sexp::atom("<module_type_declaration>")])
        }
        StructureItemDesc::Tstr_open(od) => {
            Sexp::list(vec![
                Sexp::atom("Tstr_open"),
                sexp_open_declaration(ctx, with_locs, od),
            ])
        }
        StructureItemDesc::Tstr_include(_incl) => {
            Sexp::list(vec![Sexp::atom("Tstr_include"), Sexp::atom("<include_declaration>")])
        }
        StructureItemDesc::Tstr_attribute(attr) => {
            Sexp::list(vec![Sexp::atom("Tstr_attribute"), sexp_attribute(with_locs, attr)])
        }
    }
}

fn sexp_module_binding(ctx: &TypeContext<'_>, with_locs: bool, mb: &ModuleBinding) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &mb.mb_loc)]
    } else {
        vec![]
    };
    let mut items = vec![
        Sexp::atom("module_binding"),
        sexp_ident(&mb.mb_id),
        string(&mb.mb_name.txt),
    ];
    items.extend(loc_sexp);
    items.push(sexp_module_expr(ctx, with_locs, &mb.mb_expr));
    items.push(sexp_attributes(with_locs, &mb.mb_attributes));
    Sexp::list(items)
}

fn sexp_open_declaration(ctx: &TypeContext<'_>, with_locs: bool, od: &OpenDeclaration) -> Sexp {
    let loc_sexp = if with_locs {
        vec![location(with_locs, &od.open_loc)]
    } else {
        vec![]
    };
    let mut items = vec![Sexp::atom("open_declaration")];
    items.extend(loc_sexp);
    items.push(sexp_module_expr(ctx, with_locs, &od.open_expr));
    items.push(sexp_override_flag(&od.open_override));
    items.push(sexp_attributes(with_locs, &od.open_attributes));
    Sexp::list(items)
}

// ============================================================================
// Public API
// ============================================================================

/// Print a typed structure as sexp (without locations)
pub fn print_typed_structure<W: Write>(ctx: &TypeContext<'_>, str: &Structure, out: &mut W) -> std::io::Result<()> {
    let sexp = sexp_structure(ctx, false, str);
    writeln!(out, "{}", sexp.to_string())
}

/// Print a typed structure as sexp (with locations)
pub fn print_typed_structure_with_locs<W: Write>(ctx: &TypeContext<'_>, str: &Structure, out: &mut W) -> std::io::Result<()> {
    let sexp = sexp_structure(ctx, true, str);
    writeln!(out, "{}", sexp.to_string())
}
