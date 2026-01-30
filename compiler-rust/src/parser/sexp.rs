//! S-expression printer for AST - matches OCaml res_ast_debugger.ml output exactly
//!
//! This module produces sexp output that is byte-for-byte identical to the OCaml
//! `res_parser -print sexp` output, enabling AST parity testing between the
//! Rust and OCaml parsers.
//!
//! Uses the same Doc-based pretty printing algorithm as OCaml's Res_doc module.

use crate::parse_arena::ParseArena;
use crate::parser::ast::*;
use crate::parser::longident::Longident;
use std::io::Write;

const WIDTH: i32 = 80;

/// Document type matching OCaml's Res_doc.t
#[derive(Clone)]
enum Doc {
    Nil,
    Text(String),
    Concat(Vec<Doc>),
    Indent(Box<Doc>),
    Line,  // Becomes space if group fits, newline otherwise
    Group(Box<Doc>),
}

/// Mode for rendering: Flat (lines become spaces) or Break (lines become newlines)
#[derive(Clone, Copy, PartialEq, Debug)]
enum Mode {
    Flat,
    Break,
}

impl Doc {
    fn text(s: &str) -> Doc {
        Doc::Text(s.to_string())
    }

    fn concat(docs: Vec<Doc>) -> Doc {
        Doc::Concat(docs)
    }

    fn indent(doc: Doc) -> Doc {
        Doc::Indent(Box::new(doc))
    }

    fn group(doc: Doc) -> Doc {
        Doc::Group(Box::new(doc))
    }

    fn join(sep: Doc, docs: Vec<Doc>) -> Doc {
        let mut result = Vec::new();
        for (i, doc) in docs.into_iter().enumerate() {
            if i > 0 {
                result.push(sep.clone());
            }
            result.push(doc);
        }
        Doc::Concat(result)
    }

    /// Check if the documents on the stack fit within the remaining width
    /// This matches OCaml's fits function behavior exactly
    fn fits(mut width: i32, stack: &[(i32, Mode, &Doc)]) -> bool {
        // Use a work stack to process docs in order (like OCaml's recursive approach)
        // Key difference from naive iterative: we process Concat children in-order,
        // and continue with the outer stack AFTER all children are done.

        // Work items: (indent, mode, doc, continuation_stack_idx)
        // When a doc is fully processed, we continue with continuation_stack_idx
        let mut work: Vec<(i32, Mode, &Doc)> = Vec::new();

        // Initialize with stack items in reverse order (so first is processed first)
        for item in stack.iter().rev() {
            work.push(*item);
        }

        while let Some((indent, mode, doc)) = work.pop() {
            if width < 0 {
                return false;
            }

            match doc {
                Doc::Nil => {}
                Doc::Text(s) => {
                    // Use char count to match OCaml's String.length behavior.
                    // With Latin-1 encoding, each original byte becomes one char,
                    // so chars().count() gives the original byte count.
                    width -= s.chars().count() as i32;
                }
                Doc::Concat(docs) => {
                    // Push children in reverse order so first child is processed first
                    for d in docs.iter().rev() {
                        work.push((indent, mode, d));
                    }
                }
                Doc::Indent(d) => {
                    work.push((indent + 2, mode, d.as_ref()));
                }
                Doc::Line => {
                    match mode {
                        Mode::Flat => width -= 1,
                        Mode::Break => return true, // Line break = we fit
                    }
                }
                Doc::Group(d) => {
                    // Groups inherit the current mode in fits check
                    work.push((indent, mode, d.as_ref()));
                }
            }
        }

        width >= 0
    }

    /// Convert to string with pretty printing using stack-based algorithm
    /// This matches OCaml's Res_doc.to_string algorithm
    fn to_string(&self) -> String {
        let mut out = String::new();
        let mut col: i32 = 0;

        // Stack of (indent, mode, doc)
        let mut stack: Vec<(i32, Mode, &Doc)> = vec![(0, Mode::Break, self)];

        while let Some((indent, mode, doc)) = stack.pop() {
            match doc {
                Doc::Nil => {}
                Doc::Text(s) => {
                    out.push_str(s);
                    // Use char count to match OCaml's String.length behavior.
                    // With Latin-1 encoding, each original byte becomes one char.
                    col += s.chars().count() as i32;
                }
                Doc::Concat(docs) => {
                    // Push in reverse order so first element is processed first
                    for d in docs.iter().rev() {
                        stack.push((indent, mode, d));
                    }
                }
                Doc::Indent(d) => {
                    stack.push((indent + 2, mode, d));
                }
                Doc::Line => {
                    match mode {
                        Mode::Flat => {
                            out.push(' ');
                            col += 1;
                        }
                        Mode::Break => {
                            out.push('\n');
                            for _ in 0..indent {
                                out.push(' ');
                            }
                            col = indent;
                        }
                    }
                }
                Doc::Group(d) => {
                    // Check if the group fits WITH the rest of the stack
                    // Build check stack: this group in flat mode, plus remaining stack (in top-to-bottom order)
                    let mut check_stack: Vec<(i32, Mode, &Doc)> = vec![(indent, Mode::Flat, d.as_ref())];
                    // Stack items are added from top (last index) to bottom (first index)
                    // so they're processed in the correct order after we reverse in fits
                    for item in stack.iter().rev() {
                        check_stack.push(*item);
                    }

                    let remaining = WIDTH - col;
                    let does_fit = Self::fits(remaining, &check_stack);

                    if does_fit {
                        // Fits: render in flat mode
                        stack.push((indent, Mode::Flat, d));
                    } else {
                        // Doesn't fit: render in break mode
                        stack.push((indent, Mode::Break, d));
                    }
                }
            }
        }

        out
    }
}

/// Pretty-printed sexp with proper indentation (matches OCaml Res_doc output)
#[derive(Clone)]
pub enum Sexp {
    Atom(String),
    List(Vec<Sexp>),
}

impl Sexp {
    pub fn atom(s: &str) -> Sexp {
        Sexp::Atom(s.to_string())
    }

    pub fn list(items: Vec<Sexp>) -> Sexp {
        Sexp::List(items)
    }

    pub fn to_string(&self) -> String {
        self.to_doc().to_string()
    }

    /// Convert Sexp to Doc, matching OCaml's res_ast_debugger to_doc function
    fn to_doc(&self) -> Doc {
        match self {
            Sexp::Atom(s) => Doc::text(s),
            Sexp::List(items) if items.is_empty() => Doc::text("()"),
            Sexp::List(items) if items.len() == 1 => {
                Doc::concat(vec![
                    Doc::text("("),
                    items[0].to_doc(),
                    Doc::text(")"),
                ])
            }
            Sexp::List(items) => {
                let head = items[0].to_doc();
                let tail: Vec<Doc> = items[1..].iter().map(|s| s.to_doc()).collect();
                Doc::group(Doc::concat(vec![
                    Doc::text("("),
                    head,
                    Doc::indent(Doc::concat(vec![
                        Doc::Line,
                        Doc::join(Doc::Line, tail),
                    ])),
                    Doc::text(")"),
                ]))
            }
        }
    }
}

/// Quote a string for sexp output.
/// If `escape_quotes` is true, escape quotes with backslash (for regular strings).
/// If false, output quotes as-is (for tagged/template strings).
fn quote_string_impl(s: &str, _escape_quotes: bool) -> String {
    // The scanner now keeps escape sequences as text (e.g., \n is two chars: \ and n).
    // The sexp printer just wraps the string in quotes without additional escaping.
    // OCaml's output is simply the string contents surrounded by double quotes.
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    result.push_str(s);
    result.push('"');
    result
}

fn quote_string(s: &str) -> String {
    // Default: escape quotes (for regular strings)
    quote_string_impl(s, true)
}

fn quote_string_tagged(s: &str) -> String {
    // For tagged strings (template literals): don't escape quotes
    quote_string_impl(s, false)
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

fn longident_idx(arena: &ParseArena, idx: crate::parse_arena::LidentIdx) -> Sexp {
    longident(arena, arena.get_longident(idx))
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

fn arg_label(arena: &ParseArena, lbl: &ArgLabel) -> Sexp {
    match lbl {
        ArgLabel::Nolabel => Sexp::atom("Nolabel"),
        ArgLabel::Labelled(s) => Sexp::list(vec![Sexp::atom("Labelled"), Sexp::atom(&quote_string(arena.get_string(s.txt)))]),
        ArgLabel::Optional(s) => Sexp::list(vec![Sexp::atom("Optional"), Sexp::atom(&quote_string(arena.get_string(s.txt)))]),
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
            // Check for internal char marker
            if tag.as_ref().map(|t| t.as_str()) == Some("INTERNAL_RES_CHAR_CONTENTS") {
                Sexp::list(vec![Sexp::atom("Pconst_char")])
            } else {
                // Tagged strings (template literals) don't escape quotes in sexp output
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
        Payload::PSig(items) => Sexp::list(vec![Sexp::atom("PSig"), signature_inner(arena, items)]),
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
    Sexp::list(vec![Sexp::atom("attribute"), Sexp::atom(&name.txt), payload(arena, p)])
}

fn extension(arena: &ParseArena, ext: &Extension) -> Sexp {
    let (name, p) = ext;
    Sexp::list(vec![Sexp::atom("extension"), Sexp::atom(&name.txt), payload(arena, p)])
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
        CoreTypeDesc::Ptyp_any => Sexp::atom("Ptyp_any"),
        CoreTypeDesc::Ptyp_var(var) => Sexp::list(vec![Sexp::atom("Ptyp_var"), Sexp::atom(&quote_string(var))]),
        CoreTypeDesc::Ptyp_arrow { arg, ret, .. } => Sexp::list(vec![
            Sexp::atom("Ptyp_arrow"),
            arg_label(arena, &arg.lbl),
            core_type(arena, &arg.typ),
            core_type(arena, ret),
        ]),
        CoreTypeDesc::Ptyp_tuple(types) => {
            Sexp::list(vec![Sexp::atom("Ptyp_tuple"), Sexp::list(map_empty(types, |t| core_type(arena, t)))])
        }
        CoreTypeDesc::Ptyp_constr(lid, types) => Sexp::list(vec![
            Sexp::atom("Ptyp_constr"),
            longident_idx(arena, lid.txt),
            Sexp::list(map_empty(types, |t| core_type(arena, t))),
        ]),
        CoreTypeDesc::Ptyp_object(fields, flag) => Sexp::list(vec![
            Sexp::atom("Ptyp_object"),
            closed_flag(flag),
            Sexp::list(map_empty(fields, |f| object_field(arena, f))),
        ]),
        CoreTypeDesc::Ptyp_alias(t, alias) => Sexp::list(vec![
            Sexp::atom("Ptyp_alias"),
            core_type(arena, t),
            Sexp::atom(&quote_string(alias)),
        ]),
        CoreTypeDesc::Ptyp_variant(rows, flag, opt_labels) => Sexp::list(vec![
            Sexp::atom("Ptyp_variant"),
            Sexp::list(map_empty(rows, |r| row_field(arena, r))),
            closed_flag(flag),
            match opt_labels {
                None => Sexp::atom("None"),
                Some(lbls) => Sexp::list(map_empty(lbls, |s| Sexp::atom(&quote_string(s)))),
            },
        ]),
        CoreTypeDesc::Ptyp_poly(lbls, t) => Sexp::list(vec![
            Sexp::atom("Ptyp_poly"),
            Sexp::list(map_empty(lbls, |l| Sexp::atom(&quote_string(&l.txt)))),
            core_type(arena, t),
        ]),
        CoreTypeDesc::Ptyp_package((mod_name, constraints)) => Sexp::list(vec![
            Sexp::atom("Ptyp_package"),
            package_type(arena, mod_name, constraints),
        ]),
        CoreTypeDesc::Ptyp_extension(ext) => Sexp::list(vec![Sexp::atom("Ptyp_extension"), extension(arena, ext)]),
    };
    Sexp::list(vec![Sexp::atom("core_type"), desc])
}

fn object_field(arena: &ParseArena, field: &ObjectField) -> Sexp {
    match field {
        ObjectField::Otag(lbl, attrs, t) => Sexp::list(vec![
            Sexp::atom("Otag"),
            Sexp::atom(&quote_string(&lbl.txt)),
            attributes(arena, attrs),
            core_type(arena, t),
        ]),
        ObjectField::Oinherit(t) => Sexp::list(vec![Sexp::atom("Oinherit"), core_type(arena, t)]),
    }
}

fn row_field(arena: &ParseArena, field: &RowField) -> Sexp {
    match field {
        RowField::Rtag(lbl, attrs, empty, types) => Sexp::list(vec![
            Sexp::atom("Rtag"),
            Sexp::atom(&quote_string(&lbl.txt)),
            attributes(arena, attrs),
            Sexp::atom(if *empty { "true" } else { "false" }),
            Sexp::list(map_empty(types, |t| core_type(arena, t))),
        ]),
        RowField::Rinherit(t) => Sexp::list(vec![Sexp::atom("Rinherit"), core_type(arena, t)]),
    }
}

fn package_type(arena: &ParseArena, mod_name: &Loc<crate::parse_arena::LidentIdx>, constraints: &[(Loc<crate::parse_arena::LidentIdx>, CoreType)]) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("package_type"),
        longident_idx(arena, mod_name.txt),
        Sexp::list(map_empty(constraints, |(lid, t)| {
            Sexp::list(vec![longident_idx(arena, lid.txt), core_type(arena, t)])
        })),
    ])
}

// ============================================================================
// Patterns
// ============================================================================

fn pattern(arena: &ParseArena, p: &Pattern) -> Sexp {
    let descr = match &p.ppat_desc {
        PatternDesc::Ppat_any => Sexp::atom("Ppat_any"),
        PatternDesc::Ppat_var(var) => {
            Sexp::list(vec![Sexp::atom("Ppat_var"), Sexp::atom(&quote_string(&var.txt))])
        }
        PatternDesc::Ppat_alias(pat, alias) => Sexp::list(vec![
            Sexp::atom("Ppat_alias"),
            pattern(arena, pat),
            Sexp::atom(&quote_string(&alias.txt)),
        ]),
        PatternDesc::Ppat_constant(c) => Sexp::list(vec![Sexp::atom("Ppat_constant"), constant(c)]),
        PatternDesc::Ppat_interval(lo, hi) => {
            Sexp::list(vec![Sexp::atom("Ppat_interval"), constant(lo), constant(hi)])
        }
        PatternDesc::Ppat_tuple(pats) => {
            Sexp::list(vec![Sexp::atom("Ppat_tuple"), Sexp::list(map_empty(pats, |p| pattern(arena, p)))])
        }
        PatternDesc::Ppat_construct(lid, opt_pat) => Sexp::list(vec![
            Sexp::atom("Ppat_construct"),
            longident_idx(arena, lid.txt),
            match opt_pat {
                None => Sexp::atom("None"),
                Some(p) => Sexp::list(vec![Sexp::atom("some"), pattern(arena, p)]), // Note: lowercase "some" matches OCaml
            },
        ]),
        PatternDesc::Ppat_variant(lbl, opt_pat) => Sexp::list(vec![
            Sexp::atom("Ppat_variant"),
            Sexp::atom(&quote_string(lbl)),
            match opt_pat {
                None => Sexp::atom("None"),
                Some(p) => Sexp::list(vec![Sexp::atom("Some"), pattern(arena, p)]),
            },
        ]),
        PatternDesc::Ppat_record(fields, flag) => Sexp::list(vec![
            Sexp::atom("Ppat_record"),
            closed_flag(flag),
            Sexp::list(map_empty(fields, |f| {
                Sexp::list(vec![longident_idx(arena, f.lid.txt), pattern(arena, &f.pat)])
            })),
        ]),
        PatternDesc::Ppat_array(pats) => {
            Sexp::list(vec![Sexp::atom("Ppat_array"), Sexp::list(map_empty(pats, |p| pattern(arena, p)))])
        }
        PatternDesc::Ppat_or(p1, p2) => {
            Sexp::list(vec![Sexp::atom("Ppat_or"), pattern(arena, p1), pattern(arena, p2)])
        }
        PatternDesc::Ppat_constraint(pat, t) => {
            Sexp::list(vec![Sexp::atom("Ppat_constraint"), pattern(arena, pat), core_type(arena, t)])
        }
        PatternDesc::Ppat_type(lid) => Sexp::list(vec![Sexp::atom("Ppat_type"), longident_idx(arena, lid.txt)]),
        PatternDesc::Ppat_unpack(name) => {
            Sexp::list(vec![Sexp::atom("Ppat_unpack"), Sexp::atom(&quote_string(&name.txt))])
        }
        PatternDesc::Ppat_exception(pat) => Sexp::list(vec![Sexp::atom("Ppat_exception"), pattern(arena, pat)]),
        PatternDesc::Ppat_extension(ext) => Sexp::list(vec![Sexp::atom("Ppat_extension"), extension(arena, ext)]),
        PatternDesc::Ppat_open(lid, pat) => {
            Sexp::list(vec![Sexp::atom("Ppat_open"), longident_idx(arena, lid.txt), pattern(arena, pat)])
        }
    };
    Sexp::list(vec![Sexp::atom("pattern"), descr])
}

// ============================================================================
// Expressions
// ============================================================================

fn expression(arena: &ParseArena, expr: &Expression) -> Sexp {
    let desc = match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => Sexp::list(vec![Sexp::atom("Pexp_ident"), longident_idx(arena, lid.txt)]),
        ExpressionDesc::Pexp_constant(c) => Sexp::list(vec![Sexp::atom("Pexp_constant"), constant(c)]),
        ExpressionDesc::Pexp_let(flag, vbs, body) => Sexp::list(vec![
            Sexp::atom("Pexp_let"),
            rec_flag(flag),
            Sexp::list(map_empty(vbs, |vb| value_binding(arena, vb))),
            expression(arena, body),
        ]),
        ExpressionDesc::Pexp_fun {
            arg_label: lbl,
            default: opt_default,
            lhs: pat,
            rhs: body,
            ..
        } => Sexp::list(vec![
            Sexp::atom("Pexp_fun"),
            arg_label(arena, lbl),
            match opt_default {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
            },
            pattern(arena, pat),
            expression(arena, body),
        ]),
        ExpressionDesc::Pexp_apply { funct, args, .. } => Sexp::list(vec![
            Sexp::atom("Pexp_apply"),
            expression(arena, funct),
            Sexp::list(map_empty(args, |(lbl, e)| Sexp::list(vec![arg_label(arena, lbl), expression(arena, e)]))),
        ]),
        ExpressionDesc::Pexp_match(scrutinee, cases) => Sexp::list(vec![
            Sexp::atom("Pexp_match"),
            expression(arena, scrutinee),
            Sexp::list(map_empty(cases, |c| case(arena, c))),
        ]),
        ExpressionDesc::Pexp_try(body, cases) => Sexp::list(vec![
            Sexp::atom("Pexp_try"),
            expression(arena, body),
            Sexp::list(map_empty(cases, |c| case(arena, c))),
        ]),
        ExpressionDesc::Pexp_tuple(exprs) => {
            Sexp::list(vec![Sexp::atom("Pexp_tuple"), Sexp::list(map_empty(exprs, |e| expression(arena, e)))])
        }
        ExpressionDesc::Pexp_construct(lid, opt_expr) => Sexp::list(vec![
            Sexp::atom("Pexp_construct"),
            longident_idx(arena, lid.txt),
            match opt_expr {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
            },
        ]),
        ExpressionDesc::Pexp_variant(lbl, opt_expr) => Sexp::list(vec![
            Sexp::atom("Pexp_variant"),
            Sexp::atom(&quote_string(lbl)),
            match opt_expr {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
            },
        ]),
        ExpressionDesc::Pexp_record(fields, opt_base) => Sexp::list(vec![
            Sexp::atom("Pexp_record"),
            Sexp::list(map_empty(fields, |f| {
                Sexp::list(vec![longident_idx(arena, f.lid.txt), expression(arena, &f.expr)])
            })),
            match opt_base {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
            },
        ]),
        ExpressionDesc::Pexp_field(e, lid) => {
            Sexp::list(vec![Sexp::atom("Pexp_field"), expression(arena, e), longident_idx(arena, lid.txt)])
        }
        ExpressionDesc::Pexp_setfield(e1, lid, e2) => Sexp::list(vec![
            Sexp::atom("Pexp_setfield"),
            expression(arena, e1),
            longident_idx(arena, lid.txt),
            expression(arena, e2),
        ]),
        ExpressionDesc::Pexp_array(exprs) => {
            Sexp::list(vec![Sexp::atom("Pexp_array"), Sexp::list(map_empty(exprs, |e| expression(arena, e)))])
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_e, opt_else) => Sexp::list(vec![
            Sexp::atom("Pexp_ifthenelse"),
            expression(arena, cond),
            expression(arena, then_e),
            match opt_else {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
            },
        ]),
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            Sexp::list(vec![Sexp::atom("Pexp_sequence"), expression(arena, e1), expression(arena, e2)])
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            Sexp::list(vec![Sexp::atom("Pexp_while"), expression(arena, cond), expression(arena, body)])
        }
        ExpressionDesc::Pexp_for(pat, e1, e2, flag, body) => Sexp::list(vec![
            Sexp::atom("Pexp_for"),
            pattern(arena, pat),
            expression(arena, e1),
            expression(arena, e2),
            direction_flag(flag),
            expression(arena, body),
        ]),
        ExpressionDesc::Pexp_constraint(e, t) => {
            Sexp::list(vec![Sexp::atom("Pexp_constraint"), expression(arena, e), core_type(arena, t)])
        }
        ExpressionDesc::Pexp_coerce(e, _t1, t2) => {
            // OCaml doesn't include t1 in coerce sexp
            Sexp::list(vec![Sexp::atom("Pexp_coerce"), expression(arena, e), core_type(arena, t2)])
        }
        ExpressionDesc::Pexp_send(obj, method_name) => Sexp::list(vec![
            Sexp::atom("Pexp_send"),
            Sexp::atom(&quote_string(&method_name.txt)),
            expression(arena, obj),
        ]),
        ExpressionDesc::Pexp_letmodule(name, me, body) => Sexp::list(vec![
            Sexp::atom("Pexp_letmodule"),
            Sexp::atom(&quote_string(&name.txt)),
            module_expression(arena, me),
            expression(arena, body),
        ]),
        ExpressionDesc::Pexp_letexception(ec, body) => Sexp::list(vec![
            Sexp::atom("Pexp_letexception"),
            extension_constructor(arena, ec),
            expression(arena, body),
        ]),
        ExpressionDesc::Pexp_assert(e) => Sexp::list(vec![Sexp::atom("Pexp_assert"), expression(arena, e)]),
        ExpressionDesc::Pexp_newtype(name, body) => Sexp::list(vec![
            Sexp::atom("Pexp_newtype"),
            Sexp::atom(&quote_string(&name.txt)),
            expression(arena, body),
        ]),
        ExpressionDesc::Pexp_pack(me) => Sexp::list(vec![Sexp::atom("Pexp_pack"), module_expression(arena, me)]),
        ExpressionDesc::Pexp_open(flag, lid, body) => Sexp::list(vec![
            Sexp::atom("Pexp_open"),
            override_flag(flag),
            longident_idx(arena, lid.txt),
            expression(arena, body),
        ]),
        ExpressionDesc::Pexp_extension(ext) => Sexp::list(vec![Sexp::atom("Pexp_extension"), extension(arena, ext)]),
        ExpressionDesc::Pexp_await(e) => Sexp::list(vec![Sexp::atom("Pexp_await"), expression(arena, e)]),
        ExpressionDesc::Pexp_jsx_element(jsx) => jsx_element(arena, jsx),
    };
    Sexp::list(vec![Sexp::atom("expression"), desc])
}

fn jsx_element(arena: &ParseArena, jsx: &JsxElement) -> Sexp {
    match jsx {
        JsxElement::Fragment(frag) => Sexp::list(vec![
            Sexp::atom("Pexp_jsx_fragment"),
            Sexp::list(map_empty(&frag.children, |e| expression(arena, e))),
        ]),
        JsxElement::Unary(elem) => Sexp::list(vec![
            Sexp::atom("Pexp_jsx_unary_element"),
            Sexp::list(map_empty(&elem.props, |p| jsx_prop(arena, p))),
        ]),
        JsxElement::Container(elem) => Sexp::list(vec![
            Sexp::atom("Pexp_jsx_container_element"),
            Sexp::list(map_empty(&elem.props, |p| jsx_prop(arena, p))),
            Sexp::list(map_empty(&elem.children, |e| expression(arena, e))),
        ]),
    }
}

fn jsx_prop(arena: &ParseArena, prop: &JsxProp) -> Sexp {
    match prop {
        JsxProp::Punning { name, .. } => Sexp::atom(&name.txt),
        JsxProp::Value { name, value, .. } => {
            Sexp::list(vec![Sexp::atom(&name.txt), expression(arena, value)])
        }
        JsxProp::Spreading { expr, .. } => expression(arena, expr),
    }
}

fn case(arena: &ParseArena, c: &Case) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("case"),
        Sexp::list(vec![Sexp::atom("pc_lhs"), pattern(arena, &c.pc_lhs)]),
        Sexp::list(vec![
            Sexp::atom("pc_guard"),
            match &c.pc_guard {
                None => Sexp::atom("None"),
                Some(e) => Sexp::list(vec![Sexp::atom("Some"), expression(arena, e)]),
            },
        ]),
        Sexp::list(vec![Sexp::atom("pc_rhs"), expression(arena, &c.pc_rhs)]),
    ])
}

fn value_binding(arena: &ParseArena, vb: &ValueBinding) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("value_binding"),
        pattern(arena, &vb.pvb_pat),
        expression(arena, &vb.pvb_expr),
        attributes(arena, &vb.pvb_attributes),
    ])
}

// ============================================================================
// Type declarations
// ============================================================================

fn type_declaration(arena: &ParseArena, td: &TypeDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("type_declaration"),
        Sexp::atom(&quote_string(&td.ptype_name.txt)),
        Sexp::list(vec![
            Sexp::atom("ptype_params"),
            Sexp::list(map_empty(&td.ptype_params, |(t, v)| {
                Sexp::list(vec![core_type(arena, t), variance(v)])
            })),
        ]),
        Sexp::list(vec![
            Sexp::atom("ptype_cstrs"),
            Sexp::list(map_empty(&td.ptype_cstrs, |(t1, t2, _)| {
                Sexp::list(vec![core_type(arena, t1), core_type(arena, t2)])
            })),
        ]),
        Sexp::list(vec![Sexp::atom("ptype_kind"), type_kind(arena, &td.ptype_kind)]),
        Sexp::list(vec![
            Sexp::atom("ptype_manifest"),
            match &td.ptype_manifest {
                None => Sexp::atom("None"),
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(arena, t)]),
            },
        ]),
        Sexp::list(vec![Sexp::atom("ptype_private"), private_flag(&td.ptype_private)]),
        attributes(arena, &td.ptype_attributes),
    ])
}

fn type_kind(arena: &ParseArena, kind: &TypeKind) -> Sexp {
    match kind {
        TypeKind::Ptype_abstract => Sexp::atom("Ptype_abstract"),
        TypeKind::Ptype_variant(ctors) => Sexp::list(vec![
            Sexp::atom("Ptype_variant"),
            Sexp::list(map_empty(ctors, |cd| constructor_declaration(arena, cd))),
        ]),
        TypeKind::Ptype_record(fields) => Sexp::list(vec![
            Sexp::atom("Ptype_record"),
            Sexp::list(map_empty(fields, |ld| label_declaration(arena, ld))),
        ]),
        TypeKind::Ptype_open => Sexp::atom("Ptype_open"),
    }
}

fn constructor_declaration(arena: &ParseArena, cd: &ConstructorDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("constructor_declaration"),
        Sexp::atom(&quote_string(&cd.pcd_name.txt)),
        Sexp::list(vec![Sexp::atom("pcd_args"), constructor_arguments(arena, &cd.pcd_args)]),
        Sexp::list(vec![
            Sexp::atom("pcd_res"),
            match &cd.pcd_res {
                None => Sexp::atom("None"),
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(arena, t)]),
            },
        ]),
        attributes(arena, &cd.pcd_attributes),
    ])
}

fn constructor_arguments(arena: &ParseArena, args: &ConstructorArguments) -> Sexp {
    match args {
        ConstructorArguments::Pcstr_tuple(types) => {
            Sexp::list(vec![Sexp::atom("Pcstr_tuple"), Sexp::list(map_empty(types, |t| core_type(arena, t)))])
        }
        ConstructorArguments::Pcstr_record(fields) => {
            Sexp::list(vec![Sexp::atom("Pcstr_record"), Sexp::list(map_empty(fields, |ld| label_declaration(arena, ld)))])
        }
    }
}

fn label_declaration(arena: &ParseArena, ld: &LabelDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("label_declaration"),
        Sexp::atom(&quote_string(&ld.pld_name.txt)),
        mutable_flag(&ld.pld_mutable),
        core_type(arena, &ld.pld_type),
        attributes(arena, &ld.pld_attributes),
    ])
}

fn extension_constructor(arena: &ParseArena, ec: &ExtensionConstructor) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("extension_constructor"),
        Sexp::atom(&quote_string(&ec.pext_name.txt)),
        extension_constructor_kind(arena, &ec.pext_kind),
        attributes(arena, &ec.pext_attributes),
    ])
}

fn extension_constructor_kind(arena: &ParseArena, kind: &ExtensionConstructorKind) -> Sexp {
    match kind {
        ExtensionConstructorKind::Pext_decl(args, opt_res) => Sexp::list(vec![
            Sexp::atom("Pext_decl"),
            constructor_arguments(arena, args),
            match opt_res {
                None => Sexp::atom("None"),
                Some(t) => Sexp::list(vec![Sexp::atom("Some"), core_type(arena, t)]),
            },
        ]),
        ExtensionConstructorKind::Pext_rebind(lid) => {
            Sexp::list(vec![Sexp::atom("Pext_rebind"), longident_idx(arena, lid.txt)])
        }
    }
}

fn type_extension(arena: &ParseArena, te: &TypeExtension) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("type_extension"),
        Sexp::list(vec![Sexp::atom("ptyext_path"), longident_idx(arena, te.ptyext_path.txt)]),
        Sexp::list(vec![
            Sexp::atom("ptyext_parms"),
            Sexp::list(map_empty(&te.ptyext_params, |(t, v)| {
                Sexp::list(vec![core_type(arena, t), variance(v)])
            })),
        ]),
        Sexp::list(vec![
            Sexp::atom("ptyext_constructors"),
            Sexp::list(map_empty(&te.ptyext_constructors, |ec| extension_constructor(arena, ec))),
        ]),
        Sexp::list(vec![Sexp::atom("ptyext_private"), private_flag(&te.ptyext_private)]),
        attributes(arena, &te.ptyext_attributes),
    ])
}

// ============================================================================
// Value descriptions
// ============================================================================

fn value_description(arena: &ParseArena, vd: &ValueDescription) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("value_description"),
        Sexp::atom(&quote_string(&vd.pval_name.txt)),
        core_type(arena, &vd.pval_type),
        Sexp::list(map_empty(&vd.pval_prim, |s| Sexp::atom(&quote_string(s)))),
        attributes(arena, &vd.pval_attributes),
    ])
}

// ============================================================================
// Modules
// ============================================================================

fn module_expression(arena: &ParseArena, me: &ModuleExpr) -> Sexp {
    let desc = match &me.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => Sexp::list(vec![Sexp::atom("Pmod_ident"), longident_idx(arena, lid.txt)]),
        ModuleExprDesc::Pmod_structure(items) => {
            Sexp::list(vec![Sexp::atom("Pmod_structure"), structure_inner(arena, items)])
        }
        ModuleExprDesc::Pmod_functor(name, opt_mt, body) => Sexp::list(vec![
            Sexp::atom("Pmod_functor"),
            Sexp::atom(&quote_string(&name.txt)),
            match opt_mt {
                None => Sexp::atom("None"),
                Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(arena, mt)]),
            },
            module_expression(arena, body),
        ]),
        ModuleExprDesc::Pmod_apply(me1, me2) => Sexp::list(vec![
            Sexp::atom("Pmod_apply"),
            module_expression(arena, me1),
            module_expression(arena, me2),
        ]),
        ModuleExprDesc::Pmod_constraint(me, mt) => Sexp::list(vec![
            Sexp::atom("Pmod_constraint"),
            module_expression(arena, me),
            module_type(arena, mt),
        ]),
        ModuleExprDesc::Pmod_unpack(e) => Sexp::list(vec![Sexp::atom("Pmod_unpack"), expression(arena, e)]),
        ModuleExprDesc::Pmod_extension(ext) => Sexp::list(vec![Sexp::atom("Pmod_extension"), extension(arena, ext)]),
    };
    Sexp::list(vec![Sexp::atom("module_expr"), desc, attributes(arena, &me.pmod_attributes)])
}

fn module_type(arena: &ParseArena, mt: &ModuleType) -> Sexp {
    let desc = match &mt.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => Sexp::list(vec![Sexp::atom("Pmty_ident"), longident_idx(arena, lid.txt)]),
        ModuleTypeDesc::Pmty_signature(items) => {
            Sexp::list(vec![Sexp::atom("Pmty_signature"), signature_inner(arena, items)])
        }
        ModuleTypeDesc::Pmty_functor(name, opt_mt, body) => Sexp::list(vec![
            Sexp::atom("Pmty_functor"),
            Sexp::atom(&quote_string(&name.txt)),
            match opt_mt {
                None => Sexp::atom("None"),
                Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(arena, mt)]),
            },
            module_type(arena, body),
        ]),
        ModuleTypeDesc::Pmty_with(mt, constraints) => Sexp::list(vec![
            Sexp::atom("Pmty_with"),
            module_type(arena, mt),
            Sexp::list(map_empty(constraints, |wc| with_constraint(arena, wc))),
        ]),
        ModuleTypeDesc::Pmty_typeof(me) => {
            Sexp::list(vec![Sexp::atom("Pmty_typeof"), module_expression(arena, me)])
        }
        ModuleTypeDesc::Pmty_extension(ext) => {
            Sexp::list(vec![Sexp::atom("Pmty_extension"), extension(arena, ext)])
        }
        ModuleTypeDesc::Pmty_alias(lid) => Sexp::list(vec![Sexp::atom("Pmty_alias"), longident_idx(arena, lid.txt)]),
    };
    Sexp::list(vec![Sexp::atom("module_type"), desc, attributes(arena, &mt.pmty_attributes)])
}

fn with_constraint(arena: &ParseArena, wc: &WithConstraint) -> Sexp {
    match wc {
        WithConstraint::Pwith_type(lid, td) => Sexp::list(vec![
            Sexp::atom("Pmty_with"),
            longident_idx(arena, lid.txt),
            type_declaration(arena, td),
        ]),
        WithConstraint::Pwith_module(l1, l2) => {
            Sexp::list(vec![Sexp::atom("Pwith_module"), longident_idx(arena, l1.txt), longident_idx(arena, l2.txt)])
        }
        WithConstraint::Pwith_typesubst(lid, td) => Sexp::list(vec![
            Sexp::atom("Pwith_typesubst"),
            longident_idx(arena, lid.txt),
            type_declaration(arena, td),
        ]),
        WithConstraint::Pwith_modsubst(l1, l2) => {
            Sexp::list(vec![Sexp::atom("Pwith_modsubst"), longident_idx(arena, l1.txt), longident_idx(arena, l2.txt)])
        }
    }
}

fn module_binding(arena: &ParseArena, mb: &ModuleBinding) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_binding"),
        Sexp::atom(&quote_string(&mb.pmb_name.txt)),
        module_expression(arena, &mb.pmb_expr),
        attributes(arena, &mb.pmb_attributes),
    ])
}

fn module_declaration(arena: &ParseArena, md: &ModuleDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_declaration"),
        Sexp::atom(&quote_string(&md.pmd_name.txt)),
        module_type(arena, &md.pmd_type),
        attributes(arena, &md.pmd_attributes),
    ])
}

fn module_type_declaration(arena: &ParseArena, mtd: &ModuleTypeDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("module_type_declaration"),
        Sexp::atom(&quote_string(&mtd.pmtd_name.txt)),
        match &mtd.pmtd_type {
            None => Sexp::atom("None"),
            Some(mt) => Sexp::list(vec![Sexp::atom("Some"), module_type(arena, mt)]),
        },
        attributes(arena, &mtd.pmtd_attributes),
    ])
}

fn open_description(arena: &ParseArena, od: &OpenDescription) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("open_description"),
        longident_idx(arena, od.popen_lid.txt),
        attributes(arena, &od.popen_attributes),
    ])
}

fn include_declaration(arena: &ParseArena, id: &IncludeDeclaration) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("include_declaration"),
        module_expression(arena, &id.pincl_mod),
        attributes(arena, &id.pincl_attributes),
    ])
}

fn include_description(arena: &ParseArena, id: &IncludeDescription) -> Sexp {
    Sexp::list(vec![
        Sexp::atom("include_description"),
        module_type(arena, &id.pincl_mod),
        attributes(arena, &id.pincl_attributes),
    ])
}

// ============================================================================
// Structure
// ============================================================================

pub fn structure(arena: &ParseArena, items: &[StructureItem]) -> Sexp {
    structure_inner(arena, items)
}

fn structure_inner(arena: &ParseArena, items: &[StructureItem]) -> Sexp {
    let mut parts = vec![Sexp::atom("structure")];
    parts.extend(items.iter().map(|i| structure_item(arena, i)));
    Sexp::list(parts)
}

fn structure_item(arena: &ParseArena, si: &StructureItem) -> Sexp {
    let desc = match &si.pstr_desc {
        StructureItemDesc::Pstr_eval(e, attrs) => {
            Sexp::list(vec![Sexp::atom("Pstr_eval"), expression(arena, e), attributes(arena, attrs)])
        }
        StructureItemDesc::Pstr_value(flag, vbs) => Sexp::list(vec![
            Sexp::atom("Pstr_value"),
            rec_flag(flag),
            Sexp::list(map_empty(vbs, |vb| value_binding(arena, vb))),
        ]),
        StructureItemDesc::Pstr_primitive(vd) => {
            Sexp::list(vec![Sexp::atom("Pstr_primitive"), value_description(arena, vd)])
        }
        StructureItemDesc::Pstr_type(flag, tds) => Sexp::list(vec![
            Sexp::atom("Pstr_type"),
            rec_flag(flag),
            Sexp::list(map_empty(tds, |td| type_declaration(arena, td))),
        ]),
        StructureItemDesc::Pstr_typext(te) => {
            // OCaml outputs "Pstr_type" for type extensions (not "Pstr_typext")
            Sexp::list(vec![Sexp::atom("Pstr_type"), type_extension(arena, te)])
        }
        StructureItemDesc::Pstr_exception(ec) => {
            Sexp::list(vec![Sexp::atom("Pstr_exception"), extension_constructor(arena, ec)])
        }
        StructureItemDesc::Pstr_module(mb) => {
            Sexp::list(vec![Sexp::atom("Pstr_module"), module_binding(arena, mb)])
        }
        StructureItemDesc::Pstr_recmodule(mbs) => Sexp::list(vec![
            Sexp::atom("Pstr_recmodule"),
            Sexp::list(map_empty(mbs, |mb| module_binding(arena, mb))),
        ]),
        StructureItemDesc::Pstr_modtype(mtd) => {
            Sexp::list(vec![Sexp::atom("Pstr_modtype"), module_type_declaration(arena, mtd)])
        }
        StructureItemDesc::Pstr_open(od) => {
            Sexp::list(vec![Sexp::atom("Pstr_open"), open_description(arena, od)])
        }
        StructureItemDesc::Pstr_include(id) => {
            Sexp::list(vec![Sexp::atom("Pstr_include"), include_declaration(arena, id)])
        }
        StructureItemDesc::Pstr_attribute(attr) => {
            Sexp::list(vec![Sexp::atom("Pstr_attribute"), attribute(arena, attr)])
        }
        StructureItemDesc::Pstr_extension(ext, attrs) => {
            Sexp::list(vec![Sexp::atom("Pstr_extension"), extension(arena, ext), attributes(arena, attrs)])
        }
    };
    Sexp::list(vec![Sexp::atom("structure_item"), desc])
}

// ============================================================================
// Signature
// ============================================================================

pub fn signature(arena: &ParseArena, items: &[SignatureItem]) -> Sexp {
    signature_inner(arena, items)
}

fn signature_inner(arena: &ParseArena, items: &[SignatureItem]) -> Sexp {
    let mut parts = vec![Sexp::atom("signature")];
    parts.extend(items.iter().map(|si| signature_item(arena, si)));
    Sexp::list(parts)
}

fn signature_item(arena: &ParseArena, si: &SignatureItem) -> Sexp {
    let desc = match &si.psig_desc {
        SignatureItemDesc::Psig_value(vd) => {
            Sexp::list(vec![Sexp::atom("Psig_value"), value_description(arena, vd)])
        }
        SignatureItemDesc::Psig_type(flag, tds) => Sexp::list(vec![
            Sexp::atom("Psig_type"),
            rec_flag(flag),
            Sexp::list(map_empty(tds, |td| type_declaration(arena, td))),
        ]),
        SignatureItemDesc::Psig_typext(te) => {
            Sexp::list(vec![Sexp::atom("Psig_typext"), type_extension(arena, te)])
        }
        SignatureItemDesc::Psig_exception(ec) => {
            Sexp::list(vec![Sexp::atom("Psig_exception"), extension_constructor(arena, ec)])
        }
        SignatureItemDesc::Psig_module(md) => {
            Sexp::list(vec![Sexp::atom("Psig_module"), module_declaration(arena, md)])
        }
        SignatureItemDesc::Psig_recmodule(mds) => Sexp::list(vec![
            Sexp::atom("Psig_recmodule"),
            Sexp::list(map_empty(mds, |md| module_declaration(arena, md))),
        ]),
        SignatureItemDesc::Psig_modtype(mtd) => {
            Sexp::list(vec![Sexp::atom("Psig_modtype"), module_type_declaration(arena, mtd)])
        }
        SignatureItemDesc::Psig_open(od) => {
            Sexp::list(vec![Sexp::atom("Psig_open"), open_description(arena, od)])
        }
        SignatureItemDesc::Psig_include(id) => {
            Sexp::list(vec![Sexp::atom("Psig_include"), include_description(arena, id)])
        }
        SignatureItemDesc::Psig_attribute(attr) => {
            Sexp::list(vec![Sexp::atom("Psig_attribute"), attribute(arena, attr)])
        }
        SignatureItemDesc::Psig_extension(ext, attrs) => {
            Sexp::list(vec![Sexp::atom("Psig_extension"), extension(arena, ext), attributes(arena, attrs)])
        }
    };
    Sexp::list(vec![Sexp::atom("signature_item"), desc])
}

// ============================================================================
// Public API
// ============================================================================

/// Convert a String to bytes using Latin-1 encoding (each char becomes a byte).
/// This matches OCaml's behavior where strings are byte sequences.
/// All chars in the string should have code points 0-255 (from Latin-1 file reading).
pub fn string_to_latin1_bytes(s: &str) -> Vec<u8> {
    s.chars().map(|c| c as u8).collect()
}

pub fn print_structure(arena: &ParseArena, items: &[StructureItem], out: &mut impl Write) {
    let sexp = structure(arena, items);
    // Output as Latin-1 bytes to match OCaml's byte-based string handling
    let _ = out.write_all(&string_to_latin1_bytes(&sexp.to_string()));
}

pub fn print_signature(arena: &ParseArena, items: &[SignatureItem], out: &mut impl Write) {
    let sexp = signature(arena, items);
    // Output as Latin-1 bytes to match OCaml's byte-based string handling
    let _ = out.write_all(&string_to_latin1_bytes(&sexp.to_string()));
}
