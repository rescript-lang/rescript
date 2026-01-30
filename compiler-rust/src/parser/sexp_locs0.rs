//! S-expression printer for parsetree0 AST with location information
//!
//! This module produces sexp output that includes location information for
//! parsetree0 types (the frozen PPX-compatible format).
//! Matches OCaml res_ast_debugger.ml SexpAst0WithLocs output exactly.
//! Used for debugging position parity between the Rust and OCaml parsers.

use crate::binary_ast::parsetree0::*;
use crate::location::{Located, Location};
use crate::parse_arena::ParseArena;
use crate::parser::longident::Longident;
use crate::parser::sexp::{Sexp, string_to_latin1_bytes};
use std::io::Write;

// ============================================================================
// Location helpers
// ============================================================================

fn location(loc: &Location) -> Sexp {
    let start_col = loc.loc_start.cnum - loc.loc_start.bol;
    let end_col = loc.loc_end.cnum - loc.loc_end.bol;
    let mut items = vec![
        Sexp::atom("loc"),
        Sexp::atom(&loc.loc_start.line.to_string()),
        Sexp::atom(&start_col.to_string()),
        Sexp::atom(&loc.loc_end.line.to_string()),
        Sexp::atom(&end_col.to_string()),
    ];
    if loc.loc_ghost {
        items.push(Sexp::atom("ghost"));
    }
    Sexp::list(items)
}

// ============================================================================
// String quoting
// ============================================================================

fn quote_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    result.push_str(s);
    result.push('"');
    result
}

fn quote_string_tagged(s: &str) -> String {
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

// ============================================================================
// Longident
// ============================================================================

fn longident_inner(arena: &crate::parse_arena::ParseArena, lid: &Longident) -> Sexp {
    match lid {
        Longident::Lident(name_idx) => {
            let name = arena.get_string(*name_idx);
            Sexp::list(vec![Sexp::atom("Lident"), Sexp::atom(&quote_string(name))])
        }
        Longident::Ldot(prefix, name_idx) => {
            let name = arena.get_string(*name_idx);
            Sexp::list(vec![
                Sexp::atom("Ldot"),
                longident_inner(arena, prefix),
                Sexp::atom(&quote_string(name)),
            ])
        }
        Longident::Lapply(l1, l2) => {
            Sexp::list(vec![Sexp::atom("Lapply"), longident_inner(arena, l1), longident_inner(arena, l2)])
        }
    }
}

fn longident(arena: &crate::parse_arena::ParseArena, lid: &Longident) -> Sexp {
    Sexp::list(vec![Sexp::atom("longident"), longident_inner(arena, lid)])
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

// parsetree0 ArgLabel has no location (unlike current parsetree)
fn arg_label(arena: &ParseArena, lbl: &ArgLabel) -> Sexp {
    match lbl {
        ArgLabel::Nolabel => Sexp::atom("Nolabel"),
        ArgLabel::Labelled(s) => Sexp::list(vec![
            Sexp::atom("Labelled"),
            Sexp::atom(&quote_string(arena.get_string(*s))),
        ]),
        ArgLabel::Optional(s) => Sexp::list(vec![
            Sexp::atom("Optional"),
            Sexp::atom(&quote_string(arena.get_string(*s))),
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

fn payload(arena: &ParseArena, p: &Payload) -> Sexp {
    match p {
        Payload::PStr(items) => {
            let mut parts = vec![Sexp::atom("PStr")];
            parts.extend(map_empty(items, |si| structure_item(arena, si)));
            Sexp::list(parts)
        }
        Payload::PSig(items) => Sexp::list(vec![Sexp::atom("PSig"), signature(arena, items)]),
        Payload::PTyp(ct) => Sexp::list(vec![Sexp::atom("PTyp"), core_type(arena, ct)]),
        Payload::PPat(pat, opt_expr) => Sexp::list(vec![
            Sexp::atom("PPat"),
            pattern(arena, pat),
            match opt_expr {
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
                None => Sexp::atom("None"),
            },
        ]),
    }
}

fn attribute(arena: &ParseArena, attr: &Attribute) -> Sexp {
    let (name, p) = attr;
    Sexp::list(vec![Sexp::atom("attribute"), Sexp::atom(&name.txt), location(&name.loc), payload(arena, p)])
}

fn extension(arena: &ParseArena, ext: &Extension) -> Sexp {
    let (name, p) = ext;
    Sexp::list(vec![Sexp::atom("extension"), Sexp::atom(&name.txt), location(&name.loc), payload(arena, p)])
}

fn attributes(arena: &ParseArena, attrs: &[Attribute]) -> Sexp {
    let mut parts = vec![Sexp::atom("attributes")];
    parts.extend(map_empty(attrs, |a| attribute(arena, a)));
    Sexp::list(parts)
}

// ============================================================================
// Types
// ============================================================================

fn core_type(arena: &ParseArena, typ: &CoreType) -> Sexp {
    let desc = match &typ.ptyp_desc {
        CoreTypeDesc::Any => Sexp::atom("Ptyp_any"),
        CoreTypeDesc::Var(var) => Sexp::list(vec![Sexp::atom("Ptyp_var"), Sexp::atom(&quote_string(var))]),
        // parsetree0 Arrow has no arity field
        CoreTypeDesc::Arrow(label, arg, ret) => Sexp::list(vec![
            Sexp::atom("Ptyp_arrow"),
            arg_label(arena, label),
            core_type(arena, arg),
            core_type(arena, ret),
        ]),
        CoreTypeDesc::Tuple(types) => {
            Sexp::list(vec![Sexp::atom("Ptyp_tuple"), Sexp::list(map_empty(types, |t| core_type(arena, t)))])
        }
        CoreTypeDesc::Constr(lid, types) => Sexp::list(vec![
            Sexp::atom("Ptyp_constr"),
            longident(arena, &lid.txt),
            location(&lid.loc),
            Sexp::list(map_empty(types, |t| core_type(arena, t))),
        ]),
        CoreTypeDesc::Object(fields, flag) => Sexp::list(vec![
            Sexp::atom("Ptyp_object"),
            closed_flag(flag),
            Sexp::list(map_empty(fields, |f| object_field(arena, f))),
        ]),
        CoreTypeDesc::Class => Sexp::atom("Ptyp_class"),
        CoreTypeDesc::Alias(t, alias) => Sexp::list(vec![
            Sexp::atom("Ptyp_alias"),
            core_type(arena, t),
            Sexp::atom(&quote_string(alias)),
        ]),
        CoreTypeDesc::Variant(rows, flag, opt_labels) => Sexp::list(vec![
            Sexp::atom("Ptyp_variant"),
            Sexp::list(map_empty(rows, |r| row_field(arena, r))),
            closed_flag(flag),
            match opt_labels {
                Some(labels) => {
                    let mut parts = vec![Sexp::atom("Some")];
                    parts.extend(map_empty(labels, |l| Sexp::atom(&quote_string(l))));
                    Sexp::list(parts)
                }
                None => Sexp::atom("None"),
            },
        ]),
        CoreTypeDesc::Poly(vars, t) => Sexp::list(vec![
            Sexp::atom("Ptyp_poly"),
            Sexp::list(map_empty(vars, |v| {
                Sexp::list(vec![Sexp::atom(&quote_string(&v.txt)), location(&v.loc)])
            })),
            core_type(arena, t),
        ]),
        CoreTypeDesc::Package((lid, constraints)) => Sexp::list(vec![
            Sexp::atom("Ptyp_package"),
            longident(arena, &lid.txt),
            location(&lid.loc),
            Sexp::list(map_empty(constraints, |(lid, typ)| {
                Sexp::list(vec![
                    longident(arena, &lid.txt),
                    location(&lid.loc),
                    core_type(arena, typ),
                ])
            })),
        ]),
        CoreTypeDesc::Extension(ext) => Sexp::list(vec![Sexp::atom("Ptyp_extension"), extension(arena, ext)]),
    };
    Sexp::list(vec![Sexp::atom("core_type"), location(&typ.ptyp_loc), desc, attributes(arena, &typ.ptyp_attributes)])
}

fn row_field(arena: &ParseArena, rf: &RowField) -> Sexp {
    match rf {
        RowField::Rtag(label, attrs, is_constant, types) => Sexp::list(vec![
            Sexp::atom("Rtag"),
            Sexp::atom(&quote_string(&label.txt)),
            location(&label.loc),
            attributes(arena, attrs),
            Sexp::atom(if *is_constant { "true" } else { "false" }),
            Sexp::list(map_empty(types, |t| core_type(arena, t))),
        ]),
        RowField::Rinherit(typ) => Sexp::list(vec![Sexp::atom("Rinherit"), core_type(arena, typ)]),
    }
}

fn object_field(arena: &ParseArena, field: &ObjectField) -> Sexp {
    match field {
        ObjectField::Otag(label, attrs, typ) => Sexp::list(vec![
            Sexp::atom("Otag"),
            Sexp::atom(&quote_string(&label.txt)),
            location(&label.loc),
            attributes(arena, attrs),
            core_type(arena, typ),
        ]),
        ObjectField::Oinherit(typ) => Sexp::list(vec![Sexp::atom("Oinherit"), core_type(arena, typ)]),
    }
}

// ============================================================================
// Patterns
// ============================================================================

fn pattern(arena: &ParseArena, pat: &Pattern) -> Sexp {
    let desc = match &pat.ppat_desc {
        PatternDesc::Any => Sexp::atom("Ppat_any"),
        PatternDesc::Var(v) => Sexp::list(vec![Sexp::atom("Ppat_var"), Sexp::atom(&quote_string(&v.txt)), location(&v.loc)]),
        PatternDesc::Alias(p, alias) => Sexp::list(vec![
            Sexp::atom("Ppat_alias"),
            pattern(arena, p),
            Sexp::atom(&quote_string(&alias.txt)),
            location(&alias.loc),
        ]),
        PatternDesc::Constant(c) => Sexp::list(vec![Sexp::atom("Ppat_constant"), constant(c)]),
        PatternDesc::Interval(c1, c2) => {
            Sexp::list(vec![Sexp::atom("Ppat_interval"), constant(c1), constant(c2)])
        }
        PatternDesc::Tuple(pats) => {
            Sexp::list(vec![Sexp::atom("Ppat_tuple"), Sexp::list(map_empty(pats, |p| pattern(arena, p)))])
        }
        PatternDesc::Construct(lid, opt_pat) => Sexp::list(vec![
            Sexp::atom("Ppat_construct"),
            longident(arena, &lid.txt),
            location(&lid.loc),
            match opt_pat {
                Some(p) => Sexp::list(vec![Sexp::atom("Some"), pattern(arena, p)]),
                None => Sexp::atom("None"),
            },
        ]),
        PatternDesc::Variant(label, opt_pat) => Sexp::list(vec![
            Sexp::atom("Ppat_variant"),
            Sexp::atom(&quote_string(label)),
            match opt_pat {
                Some(p) => Sexp::list(vec![Sexp::atom("Some"), pattern(arena, p)]),
                None => Sexp::atom("None"),
            },
        ]),
        PatternDesc::Record(fields, flag) => Sexp::list(vec![
            Sexp::atom("Ppat_record"),
            Sexp::list(map_empty(fields, |RecordElement { lid, x, opt }| {
                Sexp::list(vec![
                    longident(arena, &lid.txt),
                    location(&lid.loc),
                    pattern(arena, x),
                    Sexp::atom(if *opt { "true" } else { "false" }),
                ])
            })),
            closed_flag(flag),
        ]),
        PatternDesc::Array(pats) => {
            Sexp::list(vec![Sexp::atom("Ppat_array"), Sexp::list(map_empty(pats, |p| pattern(arena, p)))])
        }
        PatternDesc::Or(p1, p2) => Sexp::list(vec![Sexp::atom("Ppat_or"), pattern(arena, p1), pattern(arena, p2)]),
        PatternDesc::Constraint(p, typ) => {
            Sexp::list(vec![Sexp::atom("Ppat_constraint"), pattern(arena, p), core_type(arena, typ)])
        }
        PatternDesc::Type(lid) => Sexp::list(vec![
            Sexp::atom("Ppat_type"),
            longident(arena, &lid.txt),
            location(&lid.loc),
        ]),
        PatternDesc::Lazy(p) => Sexp::list(vec![Sexp::atom("Ppat_lazy"), pattern(arena, p)]),
        PatternDesc::Unpack(name) => Sexp::list(vec![
            Sexp::atom("Ppat_unpack"),
            Sexp::atom(&quote_string(&name.txt)),
            location(&name.loc),
        ]),
        PatternDesc::Exception(p) => Sexp::list(vec![Sexp::atom("Ppat_exception"), pattern(arena, p)]),
        PatternDesc::Extension(ext) => Sexp::list(vec![Sexp::atom("Ppat_extension"), extension(arena, ext)]),
        PatternDesc::Open(lid, p) => Sexp::list(vec![
            Sexp::atom("Ppat_open"),
            longident(arena, &lid.txt),
            location(&lid.loc),
            pattern(arena, p),
        ]),
    };
    Sexp::list(vec![Sexp::atom("pattern"), location(&pat.ppat_loc), desc, attributes(arena, &pat.ppat_attributes)])
}

// ============================================================================
// Expressions
// ============================================================================

fn expression(arena: &ParseArena, expr: &Expression) -> Sexp {
    let desc = match &expr.pexp_desc {
        ExpressionDesc::Ident(lid) => Sexp::list(vec![
            Sexp::atom("Pexp_ident"),
            longident(arena, &lid.txt),
            location(&lid.loc),
        ]),
        ExpressionDesc::Constant(c) => Sexp::list(vec![Sexp::atom("Pexp_constant"), constant(c)]),
        ExpressionDesc::Let(flag, bindings, body) => Sexp::list(vec![
            Sexp::atom("Pexp_let"),
            rec_flag(flag),
            Sexp::list(map_empty(bindings, |vb| value_binding(arena, vb))),
            expression(arena, body),
        ]),
        ExpressionDesc::Fun { arg_label: lbl, default: def, lhs, rhs, arity: ar, is_async } => {
            // OCaml parsetree0 uses:
            // - [@res.async] attribute on the Pexp_fun when is_async
            // - Pexp_construct(Function$, Some(Pexp_fun)) with [@res.arity N] when arity is present

            // Build the inner Pexp_fun sexp
            let fun_desc = Sexp::list(vec![
                Sexp::atom("Pexp_fun"),
                arg_label(arena, lbl),
                match def {
                    Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
                    None => Sexp::atom("None"),
                },
                pattern(arena, lhs),
                expression(arena, rhs),
            ]);

            // Build attributes for the inner function (including res.async if needed)
            let mut inner_attrs: Vec<&Attribute> = expr.pexp_attributes.iter().collect();
            let async_attr: Attribute;
            if *is_async {
                async_attr = (
                    Located::new("res.async".to_string(), Location::none()),
                    Payload::PStr(vec![]),
                );
                inner_attrs.insert(0, &async_attr);
            }

            if let Some(arity_val) = ar {
                // Wrap in Construct(Function$, Some(inner_fun)) with [@res.arity N] attribute
                // Build the inner attributes sexp properly: (attributes () or (attributes (attr1) (attr2) ...))
                let inner_attrs_sexp = {
                    let mut parts = vec![Sexp::atom("attributes")];
                    if inner_attrs.is_empty() {
                        parts.push(Sexp::list(vec![]));
                    } else {
                        parts.extend(inner_attrs.iter().map(|a| attribute(arena, a)));
                    }
                    Sexp::list(parts)
                };
                let inner_fun_sexp = Sexp::list(vec![
                    Sexp::atom("expression"),
                    location(&expr.pexp_loc),
                    fun_desc,
                    inner_attrs_sexp,
                ]);

                // loc for Function$ and arity attribute is Location.none (line 1, col -1)
                // OCaml's Lexing.dummy_pos has lnum=1, cnum=-1, bol=0, so col=-1
                let none_loc_sexp = Sexp::list(vec![
                    Sexp::atom("loc"),
                    Sexp::atom("1"),
                    Sexp::atom("-1"),
                    Sexp::atom("1"),
                    Sexp::atom("-1"),
                ]);

                // Build the arity attribute sexp: (attribute res.arity (loc -1 -1 -1 -1) (PStr ...))
                let arity_attr_sexp = Sexp::list(vec![
                    Sexp::atom("attribute"),
                    Sexp::atom("res.arity"),
                    none_loc_sexp.clone(),
                    Sexp::list(vec![
                        Sexp::atom("PStr"),
                        Sexp::list(vec![
                            Sexp::atom("structure_item"),
                            none_loc_sexp.clone(),
                            Sexp::list(vec![
                                Sexp::atom("Pstr_eval"),
                                Sexp::list(vec![
                                    Sexp::atom("expression"),
                                    none_loc_sexp.clone(),
                                    Sexp::list(vec![
                                        Sexp::atom("Pexp_constant"),
                                        Sexp::list(vec![
                                            Sexp::atom("constant"),
                                            Sexp::list(vec![
                                                Sexp::atom("Pconst_integer"),
                                                Sexp::atom(&quote_string(&arity_val.to_string())),
                                                Sexp::atom("None"),
                                            ]),
                                        ]),
                                    ]),
                                    Sexp::list(vec![Sexp::atom("attributes"), Sexp::list(vec![])]),
                                ]),
                                Sexp::list(vec![Sexp::atom("attributes"), Sexp::list(vec![])]),
                            ]),
                        ]),
                    ]),
                ]);

                // Return Pexp_construct with Function$ and the inner function
                return Sexp::list(vec![
                    Sexp::atom("expression"),
                    none_loc_sexp.clone(),
                    Sexp::list(vec![
                        Sexp::atom("Pexp_construct"),
                        Sexp::list(vec![Sexp::atom("longident"), Sexp::list(vec![Sexp::atom("Lident"), Sexp::atom(&quote_string("Function$"))])]),
                        location(&expr.pexp_loc),
                        Sexp::list(vec![Sexp::atom("Some"), inner_fun_sexp]),
                    ]),
                    Sexp::list(vec![Sexp::atom("attributes"), arity_attr_sexp]),
                ]);
            } else {
                // No arity - just output the Pexp_fun with attributes
                fun_desc
            }
        }
        ExpressionDesc::Apply { funct, args, partial: _partial, transformed_jsx: _ } => {
            // parsetree0 uses Pexp_apply tuple format (func, [(label, expr)])
            Sexp::list(vec![
                Sexp::atom("Pexp_apply"),
                expression(arena, funct),
                Sexp::list(map_empty(args, |(lbl, e)| {
                    Sexp::list(vec![arg_label(arena, lbl), expression(arena, e)])
                })),
            ])
        }
        ExpressionDesc::Match(e, cases) => Sexp::list(vec![
            Sexp::atom("Pexp_match"),
            expression(arena, e),
            Sexp::list(map_empty(cases, |c| case(arena, c))),
        ]),
        ExpressionDesc::Try(e, cases) => Sexp::list(vec![
            Sexp::atom("Pexp_try"),
            expression(arena, e),
            Sexp::list(map_empty(cases, |c| case(arena, c))),
        ]),
        ExpressionDesc::Tuple(exprs) => {
            Sexp::list(vec![Sexp::atom("Pexp_tuple"), Sexp::list(map_empty(exprs, |e| expression(arena, e)))])
        }
        ExpressionDesc::Construct(lid, opt_expr) => Sexp::list(vec![
            Sexp::atom("Pexp_construct"),
            longident(arena, &lid.txt),
            location(&lid.loc),
            match opt_expr {
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
                None => Sexp::atom("None"),
            },
        ]),
        ExpressionDesc::Variant(label, opt_expr) => Sexp::list(vec![
            Sexp::atom("Pexp_variant"),
            Sexp::atom(&quote_string(label)),
            match opt_expr {
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
                None => Sexp::atom("None"),
            },
        ]),
        ExpressionDesc::Record(fields, opt_expr) => Sexp::list(vec![
            Sexp::atom("Pexp_record"),
            // parsetree0 doesn't have the opt field, just (longident, expr) pairs
            Sexp::list(map_empty(fields, |RecordElement { lid, x, .. }| {
                Sexp::list(vec![
                    longident(arena, &lid.txt),
                    location(&lid.loc),
                    expression(arena, x),
                ])
            })),
            match opt_expr {
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
                None => Sexp::atom("None"),
            },
        ]),
        ExpressionDesc::Field(e, lid) => Sexp::list(vec![
            Sexp::atom("Pexp_field"),
            expression(arena, e),
            longident(arena, &lid.txt),
            location(&lid.loc),
        ]),
        ExpressionDesc::Setfield(e1, lid, e2) => Sexp::list(vec![
            Sexp::atom("Pexp_setfield"),
            expression(arena, e1),
            longident(arena, &lid.txt),
            location(&lid.loc),
            expression(arena, e2),
        ]),
        ExpressionDesc::Array(exprs) => {
            Sexp::list(vec![Sexp::atom("Pexp_array"), Sexp::list(map_empty(exprs, |e| expression(arena, e)))])
        }
        ExpressionDesc::Ifthenelse(cond, then_e, else_e) => Sexp::list(vec![
            Sexp::atom("Pexp_ifthenelse"),
            expression(arena, cond),
            expression(arena, then_e),
            match else_e {
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
                None => Sexp::atom("None"),
            },
        ]),
        ExpressionDesc::Sequence(e1, e2) => {
            Sexp::list(vec![Sexp::atom("Pexp_sequence"), expression(arena, e1), expression(arena, e2)])
        }
        ExpressionDesc::While(cond, body) => {
            Sexp::list(vec![Sexp::atom("Pexp_while"), expression(arena, cond), expression(arena, body)])
        }
        ExpressionDesc::For(pat, start, end_, dir, body) => Sexp::list(vec![
            Sexp::atom("Pexp_for"),
            pattern(arena, pat),
            expression(arena, start),
            expression(arena, end_),
            direction_flag(dir),
            expression(arena, body),
        ]),
        ExpressionDesc::Constraint(e, typ) => {
            Sexp::list(vec![Sexp::atom("Pexp_constraint"), expression(arena, e), core_type(arena, typ)])
        }
        ExpressionDesc::Coerce(e, _, typ) => {
            Sexp::list(vec![Sexp::atom("Pexp_coerce"), expression(arena, e), Sexp::atom("None"), core_type(arena, typ)])
        }
        ExpressionDesc::Send(e, label) => Sexp::list(vec![
            Sexp::atom("Pexp_send"),
            expression(arena, e),
            Sexp::atom(&quote_string(&label.txt)),
            location(&label.loc),
        ]),
        ExpressionDesc::New(lid) => Sexp::list(vec![
            Sexp::atom("Pexp_new"),
            longident(arena, &lid.txt),
            location(&lid.loc),
        ]),
        ExpressionDesc::Setinstvar(label, e) => Sexp::list(vec![
            Sexp::atom("Pexp_setinstvar"),
            Sexp::atom(&quote_string(&label.txt)),
            location(&label.loc),
            expression(arena, e),
        ]),
        ExpressionDesc::Override(fields) => Sexp::list(vec![
            Sexp::atom("Pexp_override"),
            Sexp::list(map_empty(fields, |(label, e)| {
                Sexp::list(vec![
                    Sexp::atom(&quote_string(&label.txt)),
                    location(&label.loc),
                    expression(arena, e),
                ])
            })),
        ]),
        ExpressionDesc::Letmodule(name, mod_expr, body) => Sexp::list(vec![
            Sexp::atom("Pexp_letmodule"),
            Sexp::atom(&quote_string(&name.txt)),
            location(&name.loc),
            module_expr(arena, mod_expr),
            expression(arena, body),
        ]),
        ExpressionDesc::Letexception(ext_constr, body) => Sexp::list(vec![
            Sexp::atom("Pexp_letexception"),
            extension_constructor(arena, ext_constr),
            expression(arena, body),
        ]),
        ExpressionDesc::Assert(e) => Sexp::list(vec![Sexp::atom("Pexp_assert"), expression(arena, e)]),
        ExpressionDesc::Lazy(e) => Sexp::list(vec![Sexp::atom("Pexp_lazy"), expression(arena, e)]),
        ExpressionDesc::Poly(e, opt_typ) => Sexp::list(vec![
            Sexp::atom("Pexp_poly"),
            expression(arena, e),
            match opt_typ {
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(arena, t)]),
                None => Sexp::atom("None"),
            },
        ]),
        ExpressionDesc::Object => Sexp::atom("Pexp_object"),
        ExpressionDesc::Newtype(name, body) => Sexp::list(vec![
            Sexp::atom("Pexp_newtype"),
            Sexp::atom(&quote_string(&name.txt)),
            location(&name.loc),
            expression(arena, body),
        ]),
        ExpressionDesc::Pack(mod_expr) => Sexp::list(vec![Sexp::atom("Pexp_pack"), module_expr(arena, mod_expr)]),
        ExpressionDesc::Open(flag, lid, body) => Sexp::list(vec![
            Sexp::atom("Pexp_open"),
            override_flag(flag),
            longident(arena, &lid.txt),
            location(&lid.loc),
            expression(arena, body),
        ]),
        ExpressionDesc::Extension(ext) => Sexp::list(vec![Sexp::atom("Pexp_extension"), extension(arena, ext)]),
        ExpressionDesc::Unreachable => Sexp::atom("Pexp_unreachable"),
    };
    Sexp::list(vec![Sexp::atom("expression"), location(&expr.pexp_loc), desc, attributes(arena, &expr.pexp_attributes)])
}

fn case(arena: &ParseArena, c: &Case) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("case"),
        pattern(arena, &c.pc_lhs),
        match &c.pc_guard {
            Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
            None => Sexp::atom("None"),
        },
        expression(arena, &c.pc_rhs),
    ])
}

fn value_binding(arena: &ParseArena, vb: &ValueBinding) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("value_binding"),
        location(&vb.pvb_loc),
        pattern(arena, &vb.pvb_pat),
        expression(arena, &vb.pvb_expr),
        attributes(arena, &vb.pvb_attributes),
    ])
}

// ============================================================================
// Module types
// ============================================================================

fn module_type(arena: &ParseArena, mt: &ModuleType) -> Sexp {
    let desc = match &mt.pmty_desc {
        ModuleTypeDesc::Ident(lid) => Sexp::list(vec![
            Sexp::atom("Pmty_ident"),
            longident(arena, &lid.txt),
            location(&lid.loc),
        ]),
        ModuleTypeDesc::Signature(sig) => Sexp::list(vec![Sexp::atom("Pmty_signature"), signature(arena, sig)]),
        ModuleTypeDesc::Functor(name, opt_mt, body) => Sexp::list(vec![
            Sexp::atom("Pmty_functor"),
            Sexp::atom(&quote_string(&name.txt)),
            location(&name.loc),
            match opt_mt {
                Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(arena, mt)]),
                None => Sexp::atom("None"),
            },
            module_type(arena, body),
        ]),
        ModuleTypeDesc::With(mt, constraints) => Sexp::list(vec![
            Sexp::atom("Pmty_with"),
            module_type(arena, mt),
            Sexp::list(map_empty(constraints, |wc| with_constraint(arena, wc))),
        ]),
        ModuleTypeDesc::Typeof(me) => Sexp::list(vec![Sexp::atom("Pmty_typeof"), module_expr(arena, me)]),
        ModuleTypeDesc::Extension(ext) => Sexp::list(vec![Sexp::atom("Pmty_extension"), extension(arena, ext)]),
        ModuleTypeDesc::Alias(lid) => Sexp::list(vec![
            Sexp::atom("Pmty_alias"),
            longident(arena, &lid.txt),
            location(&lid.loc),
        ]),
    };
    Sexp::list(vec![Sexp::atom("module_type"), location(&mt.pmty_loc), desc, attributes(arena, &mt.pmty_attributes)])
}

fn with_constraint(arena: &ParseArena, wc: &WithConstraint) -> Sexp {
    match wc {
        WithConstraint::Type(lid, td) => Sexp::list(vec![
            Sexp::atom("Pwith_type"),
            longident(arena, &lid.txt),
            location(&lid.loc),
            type_declaration(arena, td),
        ]),
        WithConstraint::Module(lid1, lid2) => Sexp::list(vec![
            Sexp::atom("Pwith_module"),
            longident(arena, &lid1.txt),
            location(&lid1.loc),
            longident(arena, &lid2.txt),
            location(&lid2.loc),
        ]),
        WithConstraint::TypeSubst(lid, td) => Sexp::list(vec![
            Sexp::atom("Pwith_typesubst"),
            longident(arena, &lid.txt),
            location(&lid.loc),
            type_declaration(arena, td),
        ]),
        WithConstraint::ModSubst(lid1, lid2) => Sexp::list(vec![
            Sexp::atom("Pwith_modsubst"),
            longident(arena, &lid1.txt),
            location(&lid1.loc),
            longident(arena, &lid2.txt),
            location(&lid2.loc),
        ]),
    }
}

// ============================================================================
// Module expressions
// ============================================================================

fn module_expr(arena: &ParseArena, me: &ModuleExpr) -> Sexp {
    let desc = match &me.pmod_desc {
        ModuleExprDesc::Ident(lid) => Sexp::list(vec![
            Sexp::atom("Pmod_ident"),
            longident(arena, &lid.txt),
            location(&lid.loc),
        ]),
        ModuleExprDesc::Structure(str) => Sexp::list(vec![Sexp::atom("Pmod_structure"), structure(arena, str)]),
        ModuleExprDesc::Functor(name, opt_mt, body) => Sexp::list(vec![
            Sexp::atom("Pmod_functor"),
            Sexp::atom(&quote_string(&name.txt)),
            location(&name.loc),
            match opt_mt {
                Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(arena, mt)]),
                None => Sexp::atom("None"),
            },
            module_expr(arena, body),
        ]),
        ModuleExprDesc::Apply(me1, me2) => Sexp::list(vec![Sexp::atom("Pmod_apply"), module_expr(arena, me1), module_expr(arena, me2)]),
        ModuleExprDesc::Constraint(me, mt) => {
            Sexp::list(vec![Sexp::atom("Pmod_constraint"), module_expr(arena, me), module_type(arena, mt)])
        }
        ModuleExprDesc::Unpack(e) => Sexp::list(vec![Sexp::atom("Pmod_unpack"), expression(arena, e)]),
        ModuleExprDesc::Extension(ext) => Sexp::list(vec![Sexp::atom("Pmod_extension"), extension(arena, ext)]),
    };
    Sexp::list(vec![Sexp::atom("module_expr"), location(&me.pmod_loc), desc, attributes(arena, &me.pmod_attributes)])
}

// ============================================================================
// Declarations
// ============================================================================

fn type_declaration(arena: &ParseArena, td: &TypeDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("type_declaration"),
        location(&td.ptype_loc),
        Sexp::atom(&quote_string(&td.ptype_name.txt)),
        location(&td.ptype_name.loc),
        // OCaml wraps each field with its name: (ptype_params (...))
        Sexp::list(vec![
            Sexp::atom("ptype_params"),
            Sexp::list(map_empty(&td.ptype_params, |(typ, var)| {
                Sexp::list(vec![core_type(arena, typ), variance(var)])
            })),
        ]),
        Sexp::list(vec![
            Sexp::atom("ptype_cstrs"),
            Sexp::list(map_empty(&td.ptype_cstrs, |(t1, t2, loc)| {
                Sexp::list(vec![core_type(arena, t1), core_type(arena, t2), location(loc)])
            })),
        ]),
        Sexp::list(vec![Sexp::atom("ptype_kind"), type_kind(arena, &td.ptype_kind)]),
        Sexp::list(vec![
            Sexp::atom("ptype_manifest"),
            match &td.ptype_manifest {
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(arena, t)]),
                None => Sexp::atom("None"),
            },
        ]),
        Sexp::list(vec![Sexp::atom("ptype_private"), private_flag(&td.ptype_private)]),
        attributes(arena, &td.ptype_attributes),
    ])
}

fn type_kind(arena: &ParseArena, tk: &TypeKind) -> Sexp {
    match tk {
        TypeKind::Abstract => Sexp::atom("Ptype_abstract"),
        TypeKind::Variant(constrs) => {
            Sexp::list(vec![Sexp::atom("Ptype_variant"), Sexp::list(map_empty(constrs, |cd| constructor_declaration(arena, cd)))])
        }
        TypeKind::Record(fields) => {
            Sexp::list(vec![Sexp::atom("Ptype_record"), Sexp::list(map_empty(fields, |ld| label_declaration(arena, ld)))])
        }
        TypeKind::Open => Sexp::atom("Ptype_open"),
    }
}

fn constructor_declaration(arena: &ParseArena, cd: &ConstructorDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("constructor_declaration"),
        location(&cd.pcd_loc),
        Sexp::atom(&quote_string(&cd.pcd_name.txt)),
        location(&cd.pcd_name.loc),
        // OCaml wraps these with field names
        Sexp::list(vec![Sexp::atom("pcd_args"), constructor_arguments(arena, &cd.pcd_args)]),
        Sexp::list(vec![
            Sexp::atom("pcd_res"),
            match &cd.pcd_res {
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(arena, t)]),
                None => Sexp::atom("None"),
            },
        ]),
        attributes(arena, &cd.pcd_attributes),
    ])
}

fn constructor_arguments(arena: &ParseArena, ca: &ConstructorArguments) -> Sexp {
    match ca {
        ConstructorArguments::Tuple(types) => {
            Sexp::list(vec![Sexp::atom("Pcstr_tuple"), Sexp::list(map_empty(types, |t| core_type(arena, t)))])
        }
        ConstructorArguments::Record(fields) => {
            Sexp::list(vec![Sexp::atom("Pcstr_record"), Sexp::list(map_empty(fields, |ld| label_declaration(arena, ld)))])
        }
    }
}

fn label_declaration(arena: &ParseArena, ld: &LabelDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("label_declaration"),
        location(&ld.pld_loc),
        Sexp::atom(&quote_string(&ld.pld_name.txt)),
        location(&ld.pld_name.loc),
        mutable_flag(&ld.pld_mutable),
        core_type(arena, &ld.pld_type),
        attributes(arena, &ld.pld_attributes),
    ])
}

fn type_extension(arena: &ParseArena, te: &TypeExtension) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("type_extension"),
        longident(arena, &te.ptyext_path.txt),
        location(&te.ptyext_path.loc),
        Sexp::list(map_empty(&te.ptyext_params, |(typ, var)| {
            Sexp::list(vec![core_type(arena, typ), variance(var)])
        })),
        Sexp::list(map_empty(&te.ptyext_constructors, |ec| extension_constructor(arena, ec))),
        private_flag(&te.ptyext_private),
        attributes(arena, &te.ptyext_attributes),
    ])
}

fn extension_constructor(arena: &ParseArena, ec: &ExtensionConstructor) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("extension_constructor"),
        location(&ec.pext_loc),
        Sexp::atom(&quote_string(&ec.pext_name.txt)),
        location(&ec.pext_name.loc),
        extension_constructor_kind(arena, &ec.pext_kind),
        attributes(arena, &ec.pext_attributes),
    ])
}

fn extension_constructor_kind(arena: &ParseArena, eck: &ExtensionConstructorKind) -> Sexp {
    match eck {
        ExtensionConstructorKind::Decl(args, opt_res) => Sexp::list(vec![
            Sexp::atom("Pext_decl"),
            constructor_arguments(arena, args),
            match opt_res {
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(arena, t)]),
                None => Sexp::atom("None"),
            },
        ]),
        ExtensionConstructorKind::Rebind(lid) => Sexp::list(vec![
            Sexp::atom("Pext_rebind"),
            longident(arena, &lid.txt),
            location(&lid.loc),
        ]),
    }
}

fn value_description(arena: &ParseArena, vd: &ValueDescription) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("value_description"),
        location(&vd.pval_loc),
        Sexp::atom(&quote_string(&vd.pval_name.txt)),
        location(&vd.pval_name.loc),
        core_type(arena, &vd.pval_type),
        Sexp::list(map_empty(&vd.pval_prim, |s| Sexp::atom(&quote_string(s)))),
        attributes(arena, &vd.pval_attributes),
    ])
}

fn module_declaration(arena: &ParseArena, md: &ModuleDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_declaration"),
        location(&md.pmd_loc),
        Sexp::atom(&quote_string(&md.pmd_name.txt)),
        location(&md.pmd_name.loc),
        module_type(arena, &md.pmd_type),
        attributes(arena, &md.pmd_attributes),
    ])
}

fn module_type_declaration(arena: &ParseArena, mtd: &ModuleTypeDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_type_declaration"),
        location(&mtd.pmtd_loc),
        Sexp::atom(&quote_string(&mtd.pmtd_name.txt)),
        location(&mtd.pmtd_name.loc),
        match &mtd.pmtd_type {
            Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(arena, mt)]),
            None => Sexp::atom("None"),
        },
        attributes(arena, &mtd.pmtd_attributes),
    ])
}

fn open_description(arena: &ParseArena, od: &OpenDescription) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("open_description"),
        location(&od.popen_loc),
        longident(arena, &od.popen_lid.txt),
        location(&od.popen_lid.loc),
        override_flag(&od.popen_override),
        attributes(arena, &od.popen_attributes),
    ])
}

fn include_description(arena: &ParseArena, id: &IncludeDescription) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("include_description"),
        location(&id.pincl_loc),
        module_type(arena, &id.pincl_mod),
        attributes(arena, &id.pincl_attributes),
    ])
}

fn include_declaration(arena: &ParseArena, id: &IncludeDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("include_declaration"),
        location(&id.pincl_loc),
        module_expr(arena, &id.pincl_mod),
        attributes(arena, &id.pincl_attributes),
    ])
}

fn module_binding(arena: &ParseArena, mb: &ModuleBinding) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_binding"),
        location(&mb.pmb_loc),
        Sexp::atom(&quote_string(&mb.pmb_name.txt)),
        location(&mb.pmb_name.loc),
        module_expr(arena, &mb.pmb_expr),
        attributes(arena, &mb.pmb_attributes),
    ])
}

// ============================================================================
// Signature
// ============================================================================

fn signature(arena: &ParseArena, sig: &Signature) -> Sexp {
    Sexp::list(std::iter::once(Sexp::atom("signature")).chain(sig.iter().map(|si| signature_item(arena, si))).collect())
}

fn signature_item(arena: &ParseArena, si: &SignatureItem) -> Sexp {
    let desc = match &si.psig_desc {
        SignatureItemDesc::Value(vd) => Sexp::list(vec![Sexp::atom("Psig_value"), value_description(arena, vd)]),
        SignatureItemDesc::Type(flag, decls) => Sexp::list(vec![
            Sexp::atom("Psig_type"),
            rec_flag(flag),
            Sexp::list(map_empty(decls, |td| type_declaration(arena, td))),
        ]),
        SignatureItemDesc::Typext(te) => Sexp::list(vec![Sexp::atom("Psig_typext"), type_extension(arena, te)]),
        SignatureItemDesc::Exception(ec) => Sexp::list(vec![Sexp::atom("Psig_exception"), extension_constructor(arena, ec)]),
        SignatureItemDesc::Module(md) => Sexp::list(vec![Sexp::atom("Psig_module"), module_declaration(arena, md)]),
        SignatureItemDesc::Recmodule(mds) => {
            Sexp::list(vec![Sexp::atom("Psig_recmodule"), Sexp::list(map_empty(mds, |md| module_declaration(arena, md)))])
        }
        SignatureItemDesc::Modtype(mtd) => Sexp::list(vec![Sexp::atom("Psig_modtype"), module_type_declaration(arena, mtd)]),
        SignatureItemDesc::Open(od) => Sexp::list(vec![Sexp::atom("Psig_open"), open_description(arena, od)]),
        SignatureItemDesc::Include(id) => Sexp::list(vec![Sexp::atom("Psig_include"), include_description(arena, id)]),
        SignatureItemDesc::Class => Sexp::atom("Psig_class"),
        SignatureItemDesc::ClassType => Sexp::atom("Psig_class_type"),
        SignatureItemDesc::Attribute(attr) => Sexp::list(vec![Sexp::atom("Psig_attribute"), attribute(arena, attr)]),
        SignatureItemDesc::Extension(ext, attrs) => {
            Sexp::list(vec![Sexp::atom("Psig_extension"), extension(arena, ext), attributes(arena, attrs)])
        }
    };
    Sexp::list(vec![Sexp::atom("signature_item"), location(&si.psig_loc), desc])
}

// ============================================================================
// Structure
// ============================================================================

fn structure(arena: &ParseArena, str: &Structure) -> Sexp {
    Sexp::list(std::iter::once(Sexp::atom("structure")).chain(str.iter().map(|si| structure_item(arena, si))).collect())
}

fn structure_item(arena: &ParseArena, si: &StructureItem) -> Sexp {
    let desc = match &si.pstr_desc {
        StructureItemDesc::Eval(e, attrs) => {
            Sexp::list(vec![Sexp::atom("Pstr_eval"), expression(arena, e), attributes(arena, attrs)])
        }
        StructureItemDesc::Value(flag, bindings) => Sexp::list(vec![
            Sexp::atom("Pstr_value"),
            rec_flag(flag),
            Sexp::list(map_empty(bindings, |vb| value_binding(arena, vb))),
        ]),
        StructureItemDesc::Primitive(vd) => Sexp::list(vec![Sexp::atom("Pstr_primitive"), value_description(arena, vd)]),
        StructureItemDesc::Type(flag, decls) => Sexp::list(vec![
            Sexp::atom("Pstr_type"),
            rec_flag(flag),
            Sexp::list(map_empty(decls, |td| type_declaration(arena, td))),
        ]),
        StructureItemDesc::Typext(te) => Sexp::list(vec![Sexp::atom("Pstr_typext"), type_extension(arena, te)]),
        StructureItemDesc::Exception(ec) => Sexp::list(vec![Sexp::atom("Pstr_exception"), extension_constructor(arena, ec)]),
        StructureItemDesc::Module(mb) => Sexp::list(vec![Sexp::atom("Pstr_module"), module_binding(arena, mb)]),
        StructureItemDesc::Recmodule(mbs) => {
            Sexp::list(vec![Sexp::atom("Pstr_recmodule"), Sexp::list(map_empty(mbs, |mb| module_binding(arena, mb)))])
        }
        StructureItemDesc::Modtype(mtd) => Sexp::list(vec![Sexp::atom("Pstr_modtype"), module_type_declaration(arena, mtd)]),
        StructureItemDesc::Open(od) => Sexp::list(vec![Sexp::atom("Pstr_open"), open_description(arena, od)]),
        StructureItemDesc::Class => Sexp::atom("Pstr_class"),
        StructureItemDesc::ClassType => Sexp::atom("Pstr_class_type"),
        StructureItemDesc::Include(id) => Sexp::list(vec![Sexp::atom("Pstr_include"), include_declaration(arena, id)]),
        StructureItemDesc::Attribute(attr) => Sexp::list(vec![Sexp::atom("Pstr_attribute"), attribute(arena, attr)]),
        StructureItemDesc::Extension(ext, attrs) => {
            Sexp::list(vec![Sexp::atom("Pstr_extension"), extension(arena, ext), attributes(arena, attrs)])
        }
    };
    Sexp::list(vec![Sexp::atom("structure_item"), location(&si.pstr_loc), desc])
}

// ============================================================================
// Public API
// ============================================================================

/// Print a structure (parsetree0) to sexp format with locations
pub fn print_structure(arena: &ParseArena, str: &Structure, out: &mut impl Write) {
    let sexp = structure(arena, str);
    let bytes = string_to_latin1_bytes(&sexp.to_string());
    let _ = out.write_all(&bytes);
}

/// Print a signature (parsetree0) to sexp format with locations
pub fn print_signature(arena: &ParseArena, sig: &Signature, out: &mut impl Write) {
    let sexp = signature(arena, sig);
    let bytes = string_to_latin1_bytes(&sexp.to_string());
    let _ = out.write_all(&bytes);
}
