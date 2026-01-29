//! S-expression printer for AST with location information
//!
//! This module produces sexp output that includes location information,
//! matching OCaml res_ast_debugger.ml SexpAstWithLocs output exactly.
//! Used for debugging position parity between the Rust and OCaml parsers.

use crate::parse_arena::{Located, LocIdx, ParseArena};
use crate::parser::ast::*;
use crate::parser::longident::Longident;
use crate::parser::sexp::{Sexp, string_to_latin1_bytes};
use std::io::Write;

// ============================================================================
// Location helpers
// ============================================================================

fn location(loc: LocIdx, arena: &ParseArena) -> Sexp {
    let loc_start = arena.loc_start(loc);
    let loc_end = arena.loc_end(loc);
    let start_col = loc_start.cnum - loc_start.bol;
    let end_col = loc_end.cnum - loc_end.bol;
    let mut items = vec![
        Sexp::atom("loc"),
        Sexp::atom(&loc_start.line.to_string()),
        Sexp::atom(&start_col.to_string()),
        Sexp::atom(&loc_end.line.to_string()),
        Sexp::atom(&end_col.to_string()),
    ];
    if arena.loc_ghost(loc) {
        items.push(Sexp::atom("ghost"));
    }
    Sexp::list(items)
}

// ============================================================================
// String quoting (reuse from sexp module patterns)
// ============================================================================

fn quote_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    result.push_str(s);
    result.push('"');
    result
}

fn quote_string_tagged(s: &str) -> String {
    // Same as quote_string for now - template literals don't need escaping
    quote_string(s)
}

fn map_empty<T, F>(items: &[T], f: F) -> Vec<Sexp>
where
    F: Fn(&T) -> Sexp,
{
    if items.is_empty() {
        vec![Sexp::list(vec![])]
    } else {
        items.iter().map(f).collect()
    }
}

fn map_empty_with_arena<'a, T, F>(items: &'a [T], arena: &'a ParseArena, f: F) -> Vec<Sexp>
where
    F: Fn(&'a T, &'a ParseArena) -> Sexp,
{
    if items.is_empty() {
        vec![Sexp::list(vec![])]
    } else {
        items.iter().map(|item| f(item, arena)).collect()
    }
}

// ============================================================================
// Longident
// ============================================================================

fn longident_inner(lid: &Longident) -> Sexp {
    match lid {
        Longident::Lident(name) => Sexp::list(vec![Sexp::atom("Lident"), Sexp::atom(&quote_string(name))]),
        Longident::Ldot(prefix, name) => Sexp::list(vec![
            Sexp::atom("Ldot"),
            longident_inner(prefix),
            Sexp::atom(&quote_string(name)),
        ]),
        Longident::Lapply(l1, l2) => {
            Sexp::list(vec![Sexp::atom("Lapply"), longident_inner(l1), longident_inner(l2)])
        }
    }
}

/// Returns (longident ...) without location - location is added separately at call sites
fn longident(lid: &Longident) -> Sexp {
    Sexp::list(vec![Sexp::atom("longident"), longident_inner(lid)])
}

// ============================================================================
// Flags
// ============================================================================

fn rec_flag(flag: &RecFlag) -> Sexp {
    Sexp::atom(match flag {
        RecFlag::Recursive => "Recursive",
        RecFlag::Nonrecursive => "Nonrecursive",
    })
}

fn closed_flag(flag: &ClosedFlag) -> Sexp {
    Sexp::atom(match flag {
        ClosedFlag::Closed => "Closed",
        ClosedFlag::Open => "Open",
    })
}

fn direction_flag(flag: &DirectionFlag) -> Sexp {
    Sexp::atom(match flag {
        DirectionFlag::Upto => "Upto",
        DirectionFlag::Downto => "Downto",
    })
}

fn override_flag(flag: &OverrideFlag) -> Sexp {
    Sexp::atom(match flag {
        OverrideFlag::Override => "Override",
        OverrideFlag::Fresh => "Fresh",
    })
}

fn private_flag(flag: &PrivateFlag) -> Sexp {
    Sexp::atom(match flag {
        PrivateFlag::Private => "Private",
        PrivateFlag::Public => "Public",
    })
}

fn mutable_flag(flag: &MutableFlag) -> Sexp {
    Sexp::atom(match flag {
        MutableFlag::Mutable => "Mutable",
        MutableFlag::Immutable => "Immutable",
    })
}

fn variance(v: &Variance) -> Sexp {
    Sexp::atom(match v {
        Variance::Covariant => "Covariant",
        Variance::Contravariant => "Contravariant",
        Variance::Invariant => "Invariant",
    })
}

fn arity(a: &Arity) -> Sexp {
    match a {
        Arity::Full(n) => Sexp::list(vec![Sexp::atom("Some"), Sexp::atom(&n.to_string())]),
        Arity::Unknown => Sexp::atom("None"),
    }
}

fn position_opt(pos: &Option<crate::location::Position>) -> Sexp {
    match pos {
        None => Sexp::atom("None"),
        Some(p) => {
            let col = p.cnum - p.bol;
            Sexp::list(vec![
                Sexp::atom("Some"),
                Sexp::list(vec![
                    Sexp::atom("pos"),
                    Sexp::atom(&p.line.to_string()),
                    Sexp::atom(&col.to_string()),
                ]),
            ])
        }
    }
}

fn arg_label(lbl: &ArgLabel, arena: &ParseArena) -> Sexp {
    match lbl {
        ArgLabel::Nolabel => Sexp::atom("Nolabel"),
        ArgLabel::Labelled(s) => Sexp::list(vec![
            Sexp::atom("Labelled"),
            Sexp::atom(&quote_string(&s.txt)),
            location(s.loc, arena),
        ]),
        ArgLabel::Optional(s) => Sexp::list(vec![
            Sexp::atom("Optional"),
            Sexp::atom(&quote_string(&s.txt)),
            location(s.loc, arena),
        ]),
    }
}

// ============================================================================
// Constants
// ============================================================================

fn opt_char(oc: &Option<char>) -> Sexp {
    match oc {
        None => Sexp::atom("None"),
        Some(c) => Sexp::list(vec![Sexp::atom("Some"), Sexp::atom(&format!("'{}'", c))]),
    }
}

fn constant_inner(c: &Constant) -> Sexp {
    match c {
        Constant::Integer(txt, tag) => {
            Sexp::list(vec![Sexp::atom("Pconst_integer"), Sexp::atom(&quote_string(txt)), opt_char(tag)])
        }
        Constant::Char(_) => Sexp::list(vec![Sexp::atom("Pconst_char")]),
        Constant::String(txt, tag) => {
            if tag.as_ref().map(|t| t.as_str()) == Some("INTERNAL_RES_CHAR_CONTENTS") {
                Sexp::list(vec![Sexp::atom("Pconst_char")])
            } else {
                let quoted = if tag.is_some() {
                    quote_string_tagged(txt)
                } else {
                    quote_string(txt)
                };
                Sexp::list(vec![
                    Sexp::atom("Pconst_string"),
                    Sexp::atom(&quoted),
                    match tag {
                        Some(t) => Sexp::list(vec![Sexp::atom("Some"), Sexp::atom(&quote_string(t))]),
                        None => Sexp::atom("None"),
                    },
                ])
            }
        }
        Constant::Float(txt, tag) => {
            Sexp::list(vec![Sexp::atom("Pconst_float"), Sexp::atom(&quote_string(txt)), opt_char(tag)])
        }
    }
}

fn constant(c: &Constant) -> Sexp {
    Sexp::list(vec![Sexp::atom("constant"), constant_inner(c)])
}

// ============================================================================
// Attributes
// ============================================================================

fn payload(p: &Payload, arena: &ParseArena) -> Sexp {
    match p {
        Payload::PStr(items) => {
            let mut parts = vec![Sexp::atom("PStr")];
            parts.extend(map_empty_with_arena(items, arena, structure_item));
            Sexp::list(parts)
        }
        Payload::PSig(items) => Sexp::list(vec![Sexp::atom("PSig"), signature(items, arena)]),
        Payload::PTyp(ct) => Sexp::list(vec![Sexp::atom("PTyp"), core_type(ct, arena)]),
        Payload::PPat(pat, opt_expr) => Sexp::list(vec![
            Sexp::atom("PPat"),
            pattern(pat, arena),
            match opt_expr {
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(e, arena)]),
                None => Sexp::atom("None"),
            },
        ]),
    }
}

fn attribute(attr: &Attribute, arena: &ParseArena) -> Sexp {
    let (name, p) = attr;
    Sexp::list(vec![Sexp::atom("attribute"), Sexp::atom(&name.txt), location(name.loc, arena), payload(p, arena)])
}

fn extension(ext: &Extension, arena: &ParseArena) -> Sexp {
    let (name, p) = ext;
    Sexp::list(vec![Sexp::atom("extension"), Sexp::atom(&name.txt), location(name.loc, arena), payload(p, arena)])
}

fn attributes(attrs: &[Attribute], arena: &ParseArena) -> Sexp {
    let mut parts = vec![Sexp::atom("attributes")];
    parts.extend(map_empty_with_arena(attrs, arena, attribute));
    Sexp::list(parts)
}

// ============================================================================
// Types
// ============================================================================

fn core_type(typ: &CoreType, arena: &ParseArena) -> Sexp {
    let desc = match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => Sexp::atom("Ptyp_any"),
        CoreTypeDesc::Ptyp_var(var) => Sexp::list(vec![Sexp::atom("Ptyp_var"), Sexp::atom(&quote_string(var))]),
        CoreTypeDesc::Ptyp_arrow { arg, ret, arity: ar } => {
            // OCaml's attribute placement depends on whether there's a label:
            // - For Nolabel: attributes go on the type
            // - For Labelled/Optional: attributes stay in the arrow slot
            let is_labeled = !matches!(arg.lbl, ArgLabel::Nolabel);
            let (arrow_attrs, arg_typ_sexp) = if arg.attrs.is_empty() {
                (attributes(&[], arena), core_type(&arg.typ, arena))
            } else if is_labeled {
                // Labeled: attributes stay in arrow slot
                (attributes(&arg.attrs, arena), core_type(&arg.typ, arena))
            } else {
                // Nolabel: merge attributes onto the type
                let mut merged_attrs = arg.attrs.clone();
                merged_attrs.extend(arg.typ.ptyp_attributes.clone());
                let merged_typ = CoreType {
                    ptyp_desc: arg.typ.ptyp_desc.clone(),
                    ptyp_loc: arg.typ.ptyp_loc,
                    ptyp_attributes: merged_attrs,
                };
                (attributes(&[], arena), core_type(&merged_typ, arena))
            };
            Sexp::list(vec![
                Sexp::atom("Ptyp_arrow"),
                arg_label(&arg.lbl, arena),
                arrow_attrs,
                arg_typ_sexp,
                core_type(ret, arena),
                arity(ar),
            ])
        }
        CoreTypeDesc::Ptyp_tuple(types) => {
            Sexp::list(vec![Sexp::atom("Ptyp_tuple"), Sexp::list(map_empty_with_arena(types, arena, core_type))])
        }
        CoreTypeDesc::Ptyp_constr(lid, types) => Sexp::list(vec![
            Sexp::atom("Ptyp_constr"),
            longident(&lid.txt),
            location(lid.loc, arena),
            Sexp::list(map_empty_with_arena(types, arena, core_type)),
        ]),
        CoreTypeDesc::Ptyp_object(fields, flag) => Sexp::list(vec![
            Sexp::atom("Ptyp_object"),
            closed_flag(flag),
            Sexp::list(map_empty_with_arena(fields, arena, object_field)),
        ]),
        CoreTypeDesc::Ptyp_alias(t, alias) => Sexp::list(vec![
            Sexp::atom("Ptyp_alias"),
            core_type(t, arena),
            Sexp::atom(&quote_string(alias)),
        ]),
        CoreTypeDesc::Ptyp_variant(rows, flag, opt_labels) => Sexp::list(vec![
            Sexp::atom("Ptyp_variant"),
            Sexp::list(map_empty_with_arena(rows, arena, row_field)),
            closed_flag(flag),
            match opt_labels {
                None => Sexp::atom("None"),
                Some(lbls) => Sexp::list(map_empty(lbls, |s| Sexp::atom(&quote_string(s)))),
            },
        ]),
        CoreTypeDesc::Ptyp_poly(lbls, t) => Sexp::list(vec![
            Sexp::atom("Ptyp_poly"),
            // OCaml includes location for each type variable: (("a" (loc ...)))
            Sexp::list(if lbls.is_empty() {
                vec![Sexp::list(vec![])]
            } else {
                lbls.iter().map(|l| Sexp::list(vec![
                    Sexp::atom(&quote_string(&l.txt)),
                    location(l.loc, arena),
                ])).collect()
            }),
            core_type(t, arena),
        ]),
        CoreTypeDesc::Ptyp_package((mod_name, constraints)) => Sexp::list(vec![
            Sexp::atom("Ptyp_package"),
            package_type(mod_name, constraints, arena),
        ]),
        CoreTypeDesc::Ptyp_extension(ext) => Sexp::list(vec![Sexp::atom("Ptyp_extension"), extension(ext, arena)]),
    };
    Sexp::list(vec![Sexp::atom("core_type"), location(typ.ptyp_loc, arena), desc, attributes(&typ.ptyp_attributes, arena)])
}

fn object_field(field: &ObjectField, arena: &ParseArena) -> Sexp {
    match field {
        ObjectField::Otag(lbl, attrs, t) => Sexp::list(vec![
            Sexp::atom("Otag"),
            Sexp::atom(&quote_string(&lbl.txt)),
            location(lbl.loc, arena),
            attributes(attrs, arena),
            core_type(t, arena),
        ]),
        ObjectField::Oinherit(t) => Sexp::list(vec![Sexp::atom("Oinherit"), core_type(t, arena)]),
    }
}

fn row_field(field: &RowField, arena: &ParseArena) -> Sexp {
    match field {
        RowField::Rtag(lbl, attrs, empty, types) => Sexp::list(vec![
            Sexp::atom("Rtag"),
            Sexp::atom(&quote_string(&lbl.txt)),
            location(lbl.loc, arena),
            attributes(attrs, arena),
            Sexp::atom(if *empty { "true" } else { "false" }),
            Sexp::list(map_empty_with_arena(types, arena, core_type)),
        ]),
        RowField::Rinherit(t) => Sexp::list(vec![Sexp::atom("Rinherit"), core_type(t, arena)]),
    }
}

fn package_type(mod_name: &Located<Longident>, constraints: &[(Located<Longident>, CoreType)], arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("package_type"),
        longident(&mod_name.txt),
        location(mod_name.loc, arena),
        Sexp::list(if constraints.is_empty() {
            vec![Sexp::list(vec![])]
        } else {
            constraints.iter().map(|(lid, t)| {
                Sexp::list(vec![longident(&lid.txt), location(lid.loc, arena), core_type(t, arena)])
            }).collect()
        }),
    ])
}

// ============================================================================
// Patterns
// ============================================================================

fn pattern(p: &Pattern, arena: &ParseArena) -> Sexp {
    let descr = match &p.ppat_desc {
        PatternDesc::Ppat_any => Sexp::atom("Ppat_any"),
        PatternDesc::Ppat_var(var) => {
            Sexp::list(vec![Sexp::atom("Ppat_var"), Sexp::atom(&quote_string(&var.txt)), location(var.loc, arena)])
        }
        PatternDesc::Ppat_alias(pat, alias) => Sexp::list(vec![
            Sexp::atom("Ppat_alias"),
            pattern(pat, arena),
            Sexp::atom(&quote_string(&alias.txt)),
            location(alias.loc, arena),
        ]),
        PatternDesc::Ppat_constant(c) => Sexp::list(vec![Sexp::atom("Ppat_constant"), constant(c)]),
        PatternDesc::Ppat_interval(lo, hi) => {
            Sexp::list(vec![Sexp::atom("Ppat_interval"), constant(lo), constant(hi)])
        }
        PatternDesc::Ppat_tuple(pats) => {
            Sexp::list(vec![Sexp::atom("Ppat_tuple"), Sexp::list(map_empty_with_arena(pats, arena, pattern))])
        }
        PatternDesc::Ppat_construct(lid, opt_pat) => Sexp::list(vec![
            Sexp::atom("Ppat_construct"),
            longident(&lid.txt),
            location(lid.loc, arena),
            match opt_pat {
                None => Sexp::atom("None"),
                Some(p) => Sexp::list(vec![Sexp::atom("some"), pattern(p, arena)]),
            },
        ]),
        PatternDesc::Ppat_variant(lbl, opt_pat) => Sexp::list(vec![
            Sexp::atom("Ppat_variant"),
            Sexp::atom(&quote_string(lbl)),
            match opt_pat {
                None => Sexp::atom("None"),
                Some(p) => Sexp::list(vec![Sexp::atom("Some"), pattern(p, arena)]),
            },
        ]),
        PatternDesc::Ppat_record(fields, flag) => Sexp::list(vec![
            Sexp::atom("Ppat_record"),
            closed_flag(flag),
            Sexp::list(if fields.is_empty() {
                vec![Sexp::list(vec![])]
            } else {
                fields.iter().map(|f| {
                    Sexp::list(vec![
                        longident(&f.lid.txt),
                        location(f.lid.loc, arena),
                        pattern(&f.pat, arena),
                        Sexp::atom(if f.opt { "true" } else { "false" }),
                    ])
                }).collect()
            }),
        ]),
        PatternDesc::Ppat_array(pats) => {
            Sexp::list(vec![Sexp::atom("Ppat_array"), Sexp::list(map_empty_with_arena(pats, arena, pattern))])
        }
        PatternDesc::Ppat_or(p1, p2) => {
            Sexp::list(vec![Sexp::atom("Ppat_or"), pattern(p1, arena), pattern(p2, arena)])
        }
        PatternDesc::Ppat_constraint(pat, t) => {
            Sexp::list(vec![Sexp::atom("Ppat_constraint"), pattern(pat, arena), core_type(t, arena)])
        }
        PatternDesc::Ppat_type(lid) => Sexp::list(vec![Sexp::atom("Ppat_type"), longident(&lid.txt), location(lid.loc, arena)]),
        PatternDesc::Ppat_unpack(name) => {
            Sexp::list(vec![Sexp::atom("Ppat_unpack"), Sexp::atom(&quote_string(&name.txt)), location(name.loc, arena)])
        }
        PatternDesc::Ppat_exception(pat) => Sexp::list(vec![Sexp::atom("Ppat_exception"), pattern(pat, arena)]),
        PatternDesc::Ppat_extension(ext) => Sexp::list(vec![Sexp::atom("Ppat_extension"), extension(ext, arena)]),
        PatternDesc::Ppat_open(lid, pat) => {
            Sexp::list(vec![Sexp::atom("Ppat_open"), longident(&lid.txt), location(lid.loc, arena), pattern(pat, arena)])
        }
    };
    Sexp::list(vec![Sexp::atom("pattern"), location(p.ppat_loc, arena), descr, attributes(&p.ppat_attributes, arena)])
}

// ============================================================================
// Expressions
// ============================================================================

fn expression(expr: &Expression, arena: &ParseArena) -> Sexp {
    let desc = match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => Sexp::list(vec![Sexp::atom("Pexp_ident"), longident(&lid.txt), location(lid.loc, arena)]),
        ExpressionDesc::Pexp_constant(c) => Sexp::list(vec![Sexp::atom("Pexp_constant"), constant(c)]),
        ExpressionDesc::Pexp_let(flag, vbs, body) => Sexp::list(vec![
            Sexp::atom("Pexp_let"),
            rec_flag(flag),
            Sexp::list(map_empty_with_arena(vbs, arena, value_binding)),
            expression(body, arena),
        ]),
        ExpressionDesc::Pexp_fun {
            arg_label: lbl,
            default: opt_default,
            lhs: pat,
            rhs: body,
            arity: ar,
            is_async,
        } => Sexp::list(vec![
            Sexp::atom("Pexp_fun"),
            arg_label(lbl, arena),
            match opt_default {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(e, arena)]),
            },
            pattern(pat, arena),
            expression(body, arena),
            arity(ar),
            Sexp::atom(if *is_async { "true" } else { "false" }),
        ]),
        ExpressionDesc::Pexp_apply { funct, args, partial: is_partial, transformed_jsx } => Sexp::list(vec![
            Sexp::atom("Pexp_apply"),
            expression(funct, arena),
            Sexp::list(if args.is_empty() {
                vec![Sexp::list(vec![])]
            } else {
                args.iter().map(|(lbl, e)| Sexp::list(vec![arg_label(lbl, arena), expression(e, arena)])).collect()
            }),
            Sexp::atom(if *is_partial { "true" } else { "false" }),
            Sexp::atom(if *transformed_jsx { "true" } else { "false" }),
        ]),
        ExpressionDesc::Pexp_match(scrutinee, cases) => Sexp::list(vec![
            Sexp::atom("Pexp_match"),
            expression(scrutinee, arena),
            Sexp::list(map_empty_with_arena(cases, arena, case)),
        ]),
        ExpressionDesc::Pexp_try(body, cases) => Sexp::list(vec![
            Sexp::atom("Pexp_try"),
            expression(body, arena),
            Sexp::list(map_empty_with_arena(cases, arena, case)),
        ]),
        ExpressionDesc::Pexp_tuple(exprs) => {
            Sexp::list(vec![Sexp::atom("Pexp_tuple"), Sexp::list(map_empty_with_arena(exprs, arena, expression))])
        }
        ExpressionDesc::Pexp_construct(lid, opt_expr) => Sexp::list(vec![
            Sexp::atom("Pexp_construct"),
            longident(&lid.txt),
            location(lid.loc, arena),
            match opt_expr {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(e, arena)]),
            },
        ]),
        ExpressionDesc::Pexp_variant(lbl, opt_expr) => Sexp::list(vec![
            Sexp::atom("Pexp_variant"),
            Sexp::atom(&quote_string(lbl)),
            match opt_expr {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(e, arena)]),
            },
        ]),
        ExpressionDesc::Pexp_record(fields, opt_base) => Sexp::list(vec![
            Sexp::atom("Pexp_record"),
            Sexp::list(if fields.is_empty() {
                vec![Sexp::list(vec![])]
            } else {
                fields.iter().map(|f| {
                    Sexp::list(vec![
                        longident(&f.lid.txt),
                        location(f.lid.loc, arena),
                        expression(&f.expr, arena),
                        Sexp::atom(if f.opt { "true" } else { "false" }),
                    ])
                }).collect()
            }),
            match opt_base {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(e, arena)]),
            },
        ]),
        ExpressionDesc::Pexp_field(e, lid) => {
            Sexp::list(vec![Sexp::atom("Pexp_field"), expression(e, arena), longident(&lid.txt), location(lid.loc, arena)])
        }
        ExpressionDesc::Pexp_setfield(e1, lid, e2) => Sexp::list(vec![
            Sexp::atom("Pexp_setfield"),
            expression(e1, arena),
            longident(&lid.txt),
            location(lid.loc, arena),
            expression(e2, arena),
        ]),
        ExpressionDesc::Pexp_array(exprs) => {
            Sexp::list(vec![Sexp::atom("Pexp_array"), Sexp::list(map_empty_with_arena(exprs, arena, expression))])
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_e, opt_else) => Sexp::list(vec![
            Sexp::atom("Pexp_ifthenelse"),
            expression(cond, arena),
            expression(then_e, arena),
            match opt_else {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(e, arena)]),
            },
        ]),
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            Sexp::list(vec![Sexp::atom("Pexp_sequence"), expression(e1, arena), expression(e2, arena)])
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            Sexp::list(vec![Sexp::atom("Pexp_while"), expression(cond, arena), expression(body, arena)])
        }
        ExpressionDesc::Pexp_for(pat, e1, e2, flag, body) => Sexp::list(vec![
            Sexp::atom("Pexp_for"),
            pattern(pat, arena),
            expression(e1, arena),
            expression(e2, arena),
            direction_flag(flag),
            expression(body, arena),
        ]),
        ExpressionDesc::Pexp_constraint(e, t) => {
            Sexp::list(vec![Sexp::atom("Pexp_constraint"), expression(e, arena), core_type(t, arena)])
        }
        ExpressionDesc::Pexp_coerce(e, _t1, t2) => {
            Sexp::list(vec![Sexp::atom("Pexp_coerce"), expression(e, arena), core_type(t2, arena)])
        }
        ExpressionDesc::Pexp_send(obj, method_name) => Sexp::list(vec![
            Sexp::atom("Pexp_send"),
            Sexp::atom(&quote_string(&method_name.txt)),
            location(method_name.loc, arena),
            expression(obj, arena),
        ]),
        ExpressionDesc::Pexp_letmodule(name, me, body) => Sexp::list(vec![
            Sexp::atom("Pexp_letmodule"),
            Sexp::atom(&quote_string(&name.txt)),
            location(name.loc, arena),
            module_expression(me, arena),
            expression(body, arena),
        ]),
        ExpressionDesc::Pexp_letexception(ec, body) => Sexp::list(vec![
            Sexp::atom("Pexp_letexception"),
            extension_constructor(ec, arena),
            expression(body, arena),
        ]),
        ExpressionDesc::Pexp_assert(e) => Sexp::list(vec![Sexp::atom("Pexp_assert"), expression(e, arena)]),
        ExpressionDesc::Pexp_newtype(name, body) => Sexp::list(vec![
            Sexp::atom("Pexp_newtype"),
            Sexp::atom(&quote_string(&name.txt)),
            location(name.loc, arena),
            expression(body, arena),
        ]),
        ExpressionDesc::Pexp_pack(me) => Sexp::list(vec![Sexp::atom("Pexp_pack"), module_expression(me, arena)]),
        ExpressionDesc::Pexp_open(flag, lid, body) => Sexp::list(vec![
            Sexp::atom("Pexp_open"),
            override_flag(flag),
            longident(&lid.txt),
            location(lid.loc, arena),
            expression(body, arena),
        ]),
        ExpressionDesc::Pexp_extension(ext) => Sexp::list(vec![Sexp::atom("Pexp_extension"), extension(ext, arena)]),
        ExpressionDesc::Pexp_await(e) => Sexp::list(vec![Sexp::atom("Pexp_await"), expression(e, arena)]),
        ExpressionDesc::Pexp_jsx_element(jsx) => jsx_element(jsx, arena),
    };
    Sexp::list(vec![Sexp::atom("expression"), location(expr.pexp_loc, arena), desc, attributes(&expr.pexp_attributes, arena)])
}

fn jsx_element(jsx: &JsxElement, arena: &ParseArena) -> Sexp {
    match jsx {
        JsxElement::Fragment(frag) => Sexp::list(vec![
            Sexp::atom("Pexp_jsx_fragment"),
            Sexp::list(map_empty_with_arena(&frag.children, arena, expression)),
        ]),
        JsxElement::Unary(elem) => Sexp::list(vec![
            Sexp::atom("Pexp_jsx_unary_element"),
            Sexp::list(map_empty_with_arena(&elem.props, arena, jsx_prop)),
        ]),
        JsxElement::Container(elem) => Sexp::list(vec![
            Sexp::atom("Pexp_jsx_container_element"),
            Sexp::list(map_empty_with_arena(&elem.props, arena, jsx_prop)),
            Sexp::list(map_empty_with_arena(&elem.children, arena, expression)),
        ]),
    }
}

fn jsx_prop(prop: &JsxProp, arena: &ParseArena) -> Sexp {
    match prop {
        JsxProp::Punning { name, .. } => Sexp::list(vec![Sexp::atom(&name.txt), location(name.loc, arena)]),
        JsxProp::Value { name, value, .. } => {
            Sexp::list(vec![Sexp::atom(&name.txt), location(name.loc, arena), expression(value, arena)])
        }
        JsxProp::Spreading { loc, expr } => Sexp::list(vec![Sexp::atom("..."), location(*loc, arena), expression(expr, arena)]),
    }
}

fn case(c: &Case, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("case"),
        Sexp::list(vec![Sexp::atom("pc_bar"), position_opt(&c.pc_bar)]),
        Sexp::list(vec![Sexp::atom("pc_lhs"), pattern(&c.pc_lhs, arena)]),
        Sexp::list(vec![
            Sexp::atom("pc_guard"),
            match &c.pc_guard {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(e, arena)]),
            },
        ]),
        Sexp::list(vec![Sexp::atom("pc_rhs"), expression(&c.pc_rhs, arena)]),
    ])
}

fn value_binding(vb: &ValueBinding, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("value_binding"),
        location(vb.pvb_loc, arena),
        pattern(&vb.pvb_pat, arena),
        expression(&vb.pvb_expr, arena),
        attributes(&vb.pvb_attributes, arena),
    ])
}

// ============================================================================
// Type declarations
// ============================================================================

fn type_declaration(td: &TypeDeclaration, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("type_declaration"),
        location(td.ptype_loc, arena),
        Sexp::atom(&quote_string(&td.ptype_name.txt)),
        location(td.ptype_name.loc, arena),
        Sexp::list(vec![
            Sexp::atom("ptype_params"),
            Sexp::list(if td.ptype_params.is_empty() {
                vec![Sexp::list(vec![])]
            } else {
                td.ptype_params.iter().map(|(t, v)| {
                    Sexp::list(vec![core_type(t, arena), variance(v)])
                }).collect()
            }),
        ]),
        Sexp::list(vec![
            Sexp::atom("ptype_cstrs"),
            Sexp::list(if td.ptype_cstrs.is_empty() {
                vec![Sexp::list(vec![])]
            } else {
                td.ptype_cstrs.iter().map(|(t1, t2, loc)| {
                    Sexp::list(vec![core_type(t1, arena), core_type(t2, arena), location(*loc, arena)])
                }).collect()
            }),
        ]),
        Sexp::list(vec![Sexp::atom("ptype_kind"), type_kind(&td.ptype_kind, arena)]),
        Sexp::list(vec![
            Sexp::atom("ptype_manifest"),
            match &td.ptype_manifest {
                None => Sexp::atom("None"),
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(t, arena)]),
            },
        ]),
        Sexp::list(vec![Sexp::atom("ptype_private"), private_flag(&td.ptype_private)]),
        attributes(&td.ptype_attributes, arena),
    ])
}

fn type_kind(kind: &TypeKind, arena: &ParseArena) -> Sexp {
    match kind {
        TypeKind::Ptype_abstract => Sexp::atom("Ptype_abstract"),
        TypeKind::Ptype_variant(ctors) => Sexp::list(vec![
            Sexp::atom("Ptype_variant"),
            Sexp::list(map_empty_with_arena(ctors, arena, constructor_declaration)),
        ]),
        TypeKind::Ptype_record(fields) => Sexp::list(vec![
            Sexp::atom("Ptype_record"),
            Sexp::list(map_empty_with_arena(fields, arena, label_declaration)),
        ]),
        TypeKind::Ptype_open => Sexp::atom("Ptype_open"),
    }
}

fn constructor_declaration(cd: &ConstructorDeclaration, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("constructor_declaration"),
        location(cd.pcd_loc, arena),
        Sexp::atom(&quote_string(&cd.pcd_name.txt)),
        location(cd.pcd_name.loc, arena),
        Sexp::list(vec![Sexp::atom("pcd_args"), constructor_arguments(&cd.pcd_args, arena)]),
        Sexp::list(vec![
            Sexp::atom("pcd_res"),
            match &cd.pcd_res {
                None => Sexp::atom("None"),
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(t, arena)]),
            },
        ]),
        attributes(&cd.pcd_attributes, arena),
    ])
}

fn constructor_arguments(args: &ConstructorArguments, arena: &ParseArena) -> Sexp {
    match args {
        ConstructorArguments::Pcstr_tuple(types) => {
            Sexp::list(vec![Sexp::atom("Pcstr_tuple"), Sexp::list(map_empty_with_arena(types, arena, core_type))])
        }
        ConstructorArguments::Pcstr_record(fields) => {
            Sexp::list(vec![Sexp::atom("Pcstr_record"), Sexp::list(map_empty_with_arena(fields, arena, label_declaration))])
        }
    }
}

fn label_declaration(ld: &LabelDeclaration, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("label_declaration"),
        location(ld.pld_loc, arena),
        Sexp::atom(&quote_string(&ld.pld_name.txt)),
        location(ld.pld_name.loc, arena),
        mutable_flag(&ld.pld_mutable),
        Sexp::atom(if ld.pld_optional { "true" } else { "false" }),
        core_type(&ld.pld_type, arena),
        attributes(&ld.pld_attributes, arena),
    ])
}

fn extension_constructor(ec: &ExtensionConstructor, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("extension_constructor"),
        location(ec.pext_loc, arena),
        Sexp::atom(&quote_string(&ec.pext_name.txt)),
        location(ec.pext_name.loc, arena),
        extension_constructor_kind(&ec.pext_kind, arena),
        attributes(&ec.pext_attributes, arena),
    ])
}

fn extension_constructor_kind(kind: &ExtensionConstructorKind, arena: &ParseArena) -> Sexp {
    match kind {
        ExtensionConstructorKind::Pext_decl(args, opt_res) => Sexp::list(vec![
            Sexp::atom("Pext_decl"),
            constructor_arguments(args, arena),
            match opt_res {
                None => Sexp::atom("None"),
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(t, arena)]),
            },
        ]),
        ExtensionConstructorKind::Pext_rebind(lid) => {
            Sexp::list(vec![Sexp::atom("Pext_rebind"), longident(&lid.txt), location(lid.loc, arena)])
        }
    }
}

fn type_extension(te: &TypeExtension, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("type_extension"),
        Sexp::list(vec![Sexp::atom("ptyext_path"), longident(&te.ptyext_path.txt), location(te.ptyext_path.loc, arena)]),
        Sexp::list(vec![
            Sexp::atom("ptyext_parms"),
            Sexp::list(if te.ptyext_params.is_empty() {
                vec![Sexp::list(vec![])]
            } else {
                te.ptyext_params.iter().map(|(t, v)| {
                    Sexp::list(vec![core_type(t, arena), variance(v)])
                }).collect()
            }),
        ]),
        Sexp::list(vec![
            Sexp::atom("ptyext_constructors"),
            Sexp::list(map_empty_with_arena(&te.ptyext_constructors, arena, extension_constructor)),
        ]),
        Sexp::list(vec![Sexp::atom("ptyext_private"), private_flag(&te.ptyext_private)]),
        attributes(&te.ptyext_attributes, arena),
    ])
}

// ============================================================================
// Value descriptions
// ============================================================================

fn value_description(vd: &ValueDescription, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("value_description"),
        location(vd.pval_loc, arena),
        Sexp::atom(&quote_string(&vd.pval_name.txt)),
        location(vd.pval_name.loc, arena),
        core_type(&vd.pval_type, arena),
        Sexp::list(map_empty(&vd.pval_prim, |s| Sexp::atom(&quote_string(s)))),
        attributes(&vd.pval_attributes, arena),
    ])
}

// ============================================================================
// Modules
// ============================================================================

fn module_expression(me: &ModuleExpr, arena: &ParseArena) -> Sexp {
    let desc = match &me.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => Sexp::list(vec![Sexp::atom("Pmod_ident"), longident(&lid.txt), location(lid.loc, arena)]),
        ModuleExprDesc::Pmod_structure(items) => {
            Sexp::list(vec![Sexp::atom("Pmod_structure"), structure(items, arena)])
        }
        ModuleExprDesc::Pmod_functor(name, opt_mt, body) => Sexp::list(vec![
            Sexp::atom("Pmod_functor"),
            Sexp::atom(&quote_string(&name.txt)),
            location(name.loc, arena),
            match opt_mt {
                None => Sexp::atom("None"),
                Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(mt, arena)]),
            },
            module_expression(body, arena),
        ]),
        ModuleExprDesc::Pmod_apply(me1, me2) => Sexp::list(vec![
            Sexp::atom("Pmod_apply"),
            module_expression(me1, arena),
            module_expression(me2, arena),
        ]),
        ModuleExprDesc::Pmod_constraint(me, mt) => Sexp::list(vec![
            Sexp::atom("Pmod_constraint"),
            module_expression(me, arena),
            module_type(mt, arena),
        ]),
        ModuleExprDesc::Pmod_unpack(e) => Sexp::list(vec![Sexp::atom("Pmod_unpack"), expression(e, arena)]),
        ModuleExprDesc::Pmod_extension(ext) => Sexp::list(vec![Sexp::atom("Pmod_extension"), extension(ext, arena)]),
    };
    Sexp::list(vec![Sexp::atom("module_expr"), location(me.pmod_loc, arena), desc, attributes(&me.pmod_attributes, arena)])
}

fn module_type(mt: &ModuleType, arena: &ParseArena) -> Sexp {
    let desc = match &mt.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => Sexp::list(vec![Sexp::atom("Pmty_ident"), longident(&lid.txt), location(lid.loc, arena)]),
        ModuleTypeDesc::Pmty_signature(items) => {
            Sexp::list(vec![Sexp::atom("Pmty_signature"), signature(items, arena)])
        }
        ModuleTypeDesc::Pmty_functor(name, opt_mt, body) => Sexp::list(vec![
            Sexp::atom("Pmty_functor"),
            Sexp::atom(&quote_string(&name.txt)),
            location(name.loc, arena),
            match opt_mt {
                None => Sexp::atom("None"),
                Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(mt, arena)]),
            },
            module_type(body, arena),
        ]),
        ModuleTypeDesc::Pmty_with(mt, constraints) => Sexp::list(vec![
            Sexp::atom("Pmty_with"),
            module_type(mt, arena),
            Sexp::list(map_empty_with_arena(constraints, arena, with_constraint)),
        ]),
        ModuleTypeDesc::Pmty_typeof(me) => {
            Sexp::list(vec![Sexp::atom("Pmty_typeof"), module_expression(me, arena)])
        }
        ModuleTypeDesc::Pmty_extension(ext) => {
            Sexp::list(vec![Sexp::atom("Pmty_extension"), extension(ext, arena)])
        }
        ModuleTypeDesc::Pmty_alias(lid) => Sexp::list(vec![Sexp::atom("Pmty_alias"), longident(&lid.txt), location(lid.loc, arena)]),
    };
    Sexp::list(vec![Sexp::atom("module_type"), location(mt.pmty_loc, arena), desc, attributes(&mt.pmty_attributes, arena)])
}

fn with_constraint(wc: &WithConstraint, arena: &ParseArena) -> Sexp {
    match wc {
        WithConstraint::Pwith_type(lid, td) => Sexp::list(vec![
            Sexp::atom("Pmty_with"),
            longident(&lid.txt),
            location(lid.loc, arena),
            type_declaration(td, arena),
        ]),
        WithConstraint::Pwith_module(l1, l2) => {
            Sexp::list(vec![Sexp::atom("Pwith_module"), longident(&l1.txt), location(l1.loc, arena), longident(&l2.txt), location(l2.loc, arena)])
        }
        WithConstraint::Pwith_typesubst(lid, td) => Sexp::list(vec![
            Sexp::atom("Pwith_typesubst"),
            longident(&lid.txt),
            location(lid.loc, arena),
            type_declaration(td, arena),
        ]),
        WithConstraint::Pwith_modsubst(l1, l2) => {
            Sexp::list(vec![Sexp::atom("Pwith_modsubst"), longident(&l1.txt), location(l1.loc, arena), longident(&l2.txt), location(l2.loc, arena)])
        }
    }
}

fn module_binding(mb: &ModuleBinding, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_binding"),
        location(mb.pmb_loc, arena),
        Sexp::atom(&quote_string(&mb.pmb_name.txt)),
        location(mb.pmb_name.loc, arena),
        module_expression(&mb.pmb_expr, arena),
        attributes(&mb.pmb_attributes, arena),
    ])
}

fn module_declaration(md: &ModuleDeclaration, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_declaration"),
        location(md.pmd_loc, arena),
        Sexp::atom(&quote_string(&md.pmd_name.txt)),
        location(md.pmd_name.loc, arena),
        module_type(&md.pmd_type, arena),
        attributes(&md.pmd_attributes, arena),
    ])
}

fn module_type_declaration(mtd: &ModuleTypeDeclaration, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_type_declaration"),
        location(mtd.pmtd_loc, arena),
        Sexp::atom(&quote_string(&mtd.pmtd_name.txt)),
        location(mtd.pmtd_name.loc, arena),
        match &mtd.pmtd_type {
            None => Sexp::atom("None"),
            Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(mt, arena)]),
        },
        attributes(&mtd.pmtd_attributes, arena),
    ])
}

fn open_description(od: &OpenDescription, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("open_description"),
        location(od.popen_loc, arena),
        longident(&od.popen_lid.txt),
        location(od.popen_lid.loc, arena),
        override_flag(&od.popen_override),
        attributes(&od.popen_attributes, arena),
    ])
}

fn include_declaration(id: &IncludeDeclaration, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("include_declaration"),
        location(id.pincl_loc, arena),
        module_expression(&id.pincl_mod, arena),
        attributes(&id.pincl_attributes, arena),
    ])
}

fn include_description(id: &IncludeDescription, arena: &ParseArena) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("include_description"),
        location(id.pincl_loc, arena),
        module_type(&id.pincl_mod, arena),
        attributes(&id.pincl_attributes, arena),
    ])
}

// ============================================================================
// Structure
// ============================================================================

pub fn structure(items: &[StructureItem], arena: &ParseArena) -> Sexp {
    let mut parts = vec![Sexp::atom("structure")];
    parts.extend(items.iter().map(|si| structure_item(si, arena)));
    Sexp::list(parts)
}

fn structure_item(si: &StructureItem, arena: &ParseArena) -> Sexp {
    let desc = match &si.pstr_desc {
        StructureItemDesc::Pstr_eval(e, attrs) => {
            Sexp::list(vec![Sexp::atom("Pstr_eval"), expression(e, arena), attributes(attrs, arena)])
        }
        StructureItemDesc::Pstr_value(flag, vbs) => Sexp::list(vec![
            Sexp::atom("Pstr_value"),
            rec_flag(flag),
            Sexp::list(map_empty_with_arena(vbs, arena, value_binding)),
        ]),
        StructureItemDesc::Pstr_primitive(vd) => {
            Sexp::list(vec![Sexp::atom("Pstr_primitive"), value_description(vd, arena)])
        }
        StructureItemDesc::Pstr_type(flag, tds) => Sexp::list(vec![
            Sexp::atom("Pstr_type"),
            rec_flag(flag),
            Sexp::list(map_empty_with_arena(tds, arena, type_declaration)),
        ]),
        StructureItemDesc::Pstr_typext(te) => {
            Sexp::list(vec![Sexp::atom("Pstr_type"), type_extension(te, arena)])
        }
        StructureItemDesc::Pstr_exception(ec) => {
            Sexp::list(vec![Sexp::atom("Pstr_exception"), extension_constructor(ec, arena)])
        }
        StructureItemDesc::Pstr_module(mb) => {
            Sexp::list(vec![Sexp::atom("Pstr_module"), module_binding(mb, arena)])
        }
        StructureItemDesc::Pstr_recmodule(mbs) => Sexp::list(vec![
            Sexp::atom("Pstr_recmodule"),
            Sexp::list(map_empty_with_arena(mbs, arena, module_binding)),
        ]),
        StructureItemDesc::Pstr_modtype(mtd) => {
            Sexp::list(vec![Sexp::atom("Pstr_modtype"), module_type_declaration(mtd, arena)])
        }
        StructureItemDesc::Pstr_open(od) => {
            Sexp::list(vec![Sexp::atom("Pstr_open"), open_description(od, arena)])
        }
        StructureItemDesc::Pstr_include(id) => {
            Sexp::list(vec![Sexp::atom("Pstr_include"), include_declaration(id, arena)])
        }
        StructureItemDesc::Pstr_attribute(attr) => {
            Sexp::list(vec![Sexp::atom("Pstr_attribute"), attribute(attr, arena)])
        }
        StructureItemDesc::Pstr_extension(ext, attrs) => {
            Sexp::list(vec![Sexp::atom("Pstr_extension"), extension(ext, arena), attributes(attrs, arena)])
        }
    };
    Sexp::list(vec![Sexp::atom("structure_item"), location(si.pstr_loc, arena), desc])
}

// ============================================================================
// Signature
// ============================================================================

pub fn signature(items: &[SignatureItem], arena: &ParseArena) -> Sexp {
    let mut parts = vec![Sexp::atom("signature")];
    parts.extend(items.iter().map(|si| signature_item(si, arena)));
    Sexp::list(parts)
}

fn signature_item(si: &SignatureItem, arena: &ParseArena) -> Sexp {
    let desc = match &si.psig_desc {
        SignatureItemDesc::Psig_value(vd) => {
            Sexp::list(vec![Sexp::atom("Psig_value"), value_description(vd, arena)])
        }
        SignatureItemDesc::Psig_type(flag, tds) => Sexp::list(vec![
            Sexp::atom("Psig_type"),
            rec_flag(flag),
            Sexp::list(map_empty_with_arena(tds, arena, type_declaration)),
        ]),
        SignatureItemDesc::Psig_typext(te) => {
            Sexp::list(vec![Sexp::atom("Psig_typext"), type_extension(te, arena)])
        }
        SignatureItemDesc::Psig_exception(ec) => {
            Sexp::list(vec![Sexp::atom("Psig_exception"), extension_constructor(ec, arena)])
        }
        SignatureItemDesc::Psig_module(md) => {
            Sexp::list(vec![Sexp::atom("Psig_module"), module_declaration(md, arena)])
        }
        SignatureItemDesc::Psig_recmodule(mds) => Sexp::list(vec![
            Sexp::atom("Psig_recmodule"),
            Sexp::list(map_empty_with_arena(mds, arena, module_declaration)),
        ]),
        SignatureItemDesc::Psig_modtype(mtd) => {
            Sexp::list(vec![Sexp::atom("Psig_modtype"), module_type_declaration(mtd, arena)])
        }
        SignatureItemDesc::Psig_open(od) => {
            Sexp::list(vec![Sexp::atom("Psig_open"), open_description(od, arena)])
        }
        SignatureItemDesc::Psig_include(id) => {
            Sexp::list(vec![Sexp::atom("Psig_include"), include_description(id, arena)])
        }
        SignatureItemDesc::Psig_attribute(attr) => {
            Sexp::list(vec![Sexp::atom("Psig_attribute"), attribute(attr, arena)])
        }
        SignatureItemDesc::Psig_extension(ext, attrs) => {
            Sexp::list(vec![Sexp::atom("Psig_extension"), extension(ext, arena), attributes(attrs, arena)])
        }
    };
    Sexp::list(vec![Sexp::atom("signature_item"), location(si.psig_loc, arena), desc])
}

// ============================================================================
// Public API
// ============================================================================

pub fn print_structure(items: &[StructureItem], arena: &ParseArena, out: &mut impl Write) {
    let sexp = structure(items, arena);
    let _ = out.write_all(&string_to_latin1_bytes(&sexp.to_string()));
}

pub fn print_signature(items: &[SignatureItem], arena: &ParseArena, out: &mut impl Write) {
    let sexp = signature(items, arena);
    let _ = out.write_all(&string_to_latin1_bytes(&sexp.to_string()));
}
