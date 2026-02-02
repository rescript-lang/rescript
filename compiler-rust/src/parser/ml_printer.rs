//! ML Printer (OCaml-compatible output)
//!
//! This module provides printing functions that output AST in a format
//! compatible with OCaml's Pprintast module. Uses the OCaml-compatible
//! Format module for proper line breaking with box-based formatting.

use std::io::Write;

use crate::parse_arena::{LidentIdx, Located, ParseArena};
use super::ast::*;
use super::formatter::{BoxKind, Formatter};
use super::longident::Longident;
use super::parsetree_viewer;

// ============================================================================
// Helper functions
// ============================================================================

/// Count the arity of nested Pexp_fun expressions
fn count_function_arity(expr: &Expression) -> usize {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun { rhs, .. } => 1 + count_function_arity(rhs),
        _ => 0,
    }
}

/// Check if pattern is a simple variable with given name (and no attributes)
fn pattern_is_simple_var(pat: &Pattern, name: &str) -> bool {
    if !pat.ppat_attributes.is_empty() {
        return false;
    }
    match &pat.ppat_desc {
        PatternDesc::Ppat_var(var) => var.txt == name,
        _ => false,
    }
}

/// Check if pattern needs parens when used as a function parameter
fn pattern_needs_parens_as_param(pat: &Pattern, arena: &ParseArena) -> bool {
    match &pat.ppat_desc {
        PatternDesc::Ppat_construct(lid, arg) => {
            match arena.get_longident(lid.txt) {
                // () and [] don't need parens, but :: with args does
                Longident::Lident(name_idx) => {
                    let name = arena.get_string(*name_idx);
                    if name == "()" || name == "[]" {
                        false
                    } else if name == "::" {
                        arg.is_some()
                    } else {
                        true
                    }
                }
                _ => true,
            }
        }
        PatternDesc::Ppat_variant(_, Some(_)) => true,
        PatternDesc::Ppat_alias(_, _) => true,
        PatternDesc::Ppat_or(_, _) => true,
        _ => false,
    }
}

/// Check if a pattern is "simple" according to OCaml's pprintast.
/// Simple patterns can appear as arguments to variants/constructors without parens.
/// Based on pprintast.ml's simple_pattern function.
fn pattern_is_simple(pat: &Pattern, arena: &ParseArena) -> bool {
    // If pattern has attributes, it needs to go through full pattern printing
    if !pat.ppat_attributes.is_empty() {
        return false;
    }
    match &pat.ppat_desc {
        // These are all handled directly by simple_pattern:
        PatternDesc::Ppat_any => true,
        PatternDesc::Ppat_var(_) => true,
        PatternDesc::Ppat_array(_) => true,
        PatternDesc::Ppat_unpack(_) => true,
        PatternDesc::Ppat_type(_) => true,
        PatternDesc::Ppat_record(_, _) => true,
        PatternDesc::Ppat_tuple(_) => true,
        PatternDesc::Ppat_constant(_) => true,
        PatternDesc::Ppat_interval(_, _) => true,
        PatternDesc::Ppat_variant(_, None) => true,  // Only variant without arg
        PatternDesc::Ppat_constraint(_, _) => true,
        PatternDesc::Ppat_exception(_) => true,
        PatternDesc::Ppat_extension(_) => true,
        PatternDesc::Ppat_open(_, _) => true,
        PatternDesc::Ppat_construct(lid, _) => {
            // () and [] are simple, others are not
            match arena.get_longident(lid.txt) {
                Longident::Lident(name_idx) => {
                    let name = arena.get_string(*name_idx);
                    name == "()" || name == "[]"
                }
                _ => false,
            }
        }
        // These are NOT simple - need parens:
        PatternDesc::Ppat_alias(_, _) => false,
        PatternDesc::Ppat_or(_, _) => false,
        PatternDesc::Ppat_variant(_, Some(_)) => false,
    }
}

/// Check if a payload is empty (empty PStr)
fn payload_is_empty(payload: &Payload) -> bool {
    matches!(payload, Payload::PStr(items) if items.is_empty())
}

/// Check if a constant is negative (starts with -)
fn constant_is_negative(c: &Constant) -> bool {
    match c {
        Constant::Integer(s, _) | Constant::Float(s, _) => s.starts_with('-'),
        _ => false,
    }
}

/// Check if a pattern is a cons pattern (::)
fn is_cons_pattern(p: &Pattern, arena: &ParseArena) -> bool {
    if let PatternDesc::Ppat_construct(lid, _) = &p.ppat_desc {
        if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
            let name = arena.get_string(*name_idx);
            return name == "::";
        }
    }
    false
}

/// Check if a string represents an infix operator
fn is_infix_operator(name: &str) -> bool {
    const INFIX_SPECIALS: &[&str] =
        &[":=", "!=", "::", "mod", "land", "lor", "lxor", "lsl", "lsr", "asr", "or"];
    if INFIX_SPECIALS.contains(&name) {
        return true;
    }
    const INFIX_SYMBOLS: &[char] = &[
        '=', '<', '>', '@', '^', '|', '&', '+', '-', '*', '/', '$', '%', '#',
    ];
    if let Some(c) = name.chars().next() {
        if INFIX_SYMBOLS.contains(&c) {
            return true;
        }
    }
    false
}

/// Check if a string represents a prefix operator
fn is_prefix_operator(name: &str) -> bool {
    const PREFIX_SYMBOLS: &[char] = &['!', '?', '~'];
    if let Some(c) = name.chars().next() {
        PREFIX_SYMBOLS.contains(&c)
    } else {
        false
    }
}

/// Check if an identifier needs parens when printed as a value
/// (OCaml wraps operators in parens when used as values)
fn ident_needs_parens(name: &str) -> bool {
    is_infix_operator(name) || is_prefix_operator(name)
}

/// Get operator name if expression is an identifier that is an operator
fn get_operator_name<'a>(expr: &Expression, arena: &'a ParseArena) -> Option<&'a str> {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => match arena.get_longident(lid.txt) {
            Longident::Lident(name_idx) => Some(arena.get_string(*name_idx)),
            _ => None,
        },
        _ => None,
    }
}

/// Check if expression needs parens in binary context
fn needs_parens_in_binary_context(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ifthenelse(_, _, _)
        | ExpressionDesc::Pexp_let(_, _, _)
        | ExpressionDesc::Pexp_fun { .. }
        | ExpressionDesc::Pexp_match(_, _)
        | ExpressionDesc::Pexp_try(_, _)
        | ExpressionDesc::Pexp_sequence(_, _)
        | ExpressionDesc::Pexp_await(_) => true,
        // Note: for/while have do...done delimiters so don't need extra parens
        ExpressionDesc::Pexp_apply { args, .. } if !args.is_empty() => true,
        _ => false,
    }
}

/// Check if expression is an infix operator application
fn is_infix_application(expr: &Expression, arena: &ParseArena) -> bool {
    if let ExpressionDesc::Pexp_apply { funct, args, .. } = &expr.pexp_desc {
        if args.len() == 2 {
            if let Some(op_name) = get_operator_name(funct, arena) {
                return is_infix_operator(op_name);
            }
        }
    }
    false
}

/// Check if expression is a prefix operator application
fn is_prefix_application(expr: &Expression, arena: &ParseArena) -> bool {
    if let ExpressionDesc::Pexp_apply { funct, args, .. } = &expr.pexp_desc {
        if args.len() == 1 {
            if let Some(op_name) = get_operator_name(funct, arena) {
                return is_prefix_operator(op_name);
            }
        }
    }
    false
}

/// Check if a function application has labeled arguments
fn application_has_labeled_args(expr: &Expression) -> bool {
    if let ExpressionDesc::Pexp_apply { args, .. } = &expr.pexp_desc {
        args.iter().any(|(label, _)| !matches!(label, ArgLabel::Nolabel))
    } else {
        false
    }
}

/// Check if expression needs parens when used as a labeled argument value
fn needs_parens_as_labeled_arg(expr: &Expression, arena: &ParseArena) -> bool {
    if is_prefix_application(expr, arena) {
        return false;
    }
    needs_parens_in_binary_context(expr) || is_infix_application(expr, arena)
}

/// Check if expression needs parens when used as an unlabeled function argument
fn needs_parens_as_function_arg(expr: &Expression, arena: &ParseArena) -> bool {
    if is_prefix_application(expr, arena) {
        return false;
    }
    needs_parens_in_binary_context(expr) || application_has_labeled_args(expr)
}

/// Check if expression already prints its own parens
fn expression_has_own_parens(expr: &Expression) -> bool {
    matches!(&expr.pexp_desc, ExpressionDesc::Pexp_assert(_))
}

/// Check if a Pexp_construct is a "simple construct" (like OCaml's is_simple_construct)
/// This includes: [], (), complete lists [a; b; c], and simple constructors without args
fn is_simple_construct(expr: &Expression, arena: &ParseArena) -> bool {
    if !expr.pexp_attributes.is_empty() {
        return false;
    }
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_construct(lid, None) => {
            // () and [] and simple constructors are simple
            true
        }
        ExpressionDesc::Pexp_construct(lid, Some(_)) => {
            // Check if it's a complete list (ends with [])
            if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                let name = arena.get_string(*name_idx);
                if name == "::" {
                    let (_, is_complete) = collect_list_elements(expr, arena);
                    return is_complete;
                }
            }
            false
        }
        _ => false,
    }
}

/// Check if expression is "simple" - doesn't need parens
/// Note: This version doesn't have arena access, so it can't check complete lists.
/// Use is_simple_expression_with_arena for full checking.
fn is_simple_expression(expr: &Expression) -> bool {
    if !expr.pexp_attributes.is_empty() {
        return false;
    }
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_ident(_)
            | ExpressionDesc::Pexp_constant(_)
            | ExpressionDesc::Pexp_tuple(_)
            | ExpressionDesc::Pexp_array(_)
            | ExpressionDesc::Pexp_record(_, _)
            | ExpressionDesc::Pexp_constraint(_, _)
            | ExpressionDesc::Pexp_coerce(_, _, _)
            | ExpressionDesc::Pexp_variant(_, None)
            | ExpressionDesc::Pexp_construct(_, None)
            | ExpressionDesc::Pexp_pack(_)
            | ExpressionDesc::Pexp_field(_, _)  // Field access is simple
            // OCaml's simple_expr handles these directly (no paren wrapping)
            | ExpressionDesc::Pexp_for(_, _, _, _, _)
            | ExpressionDesc::Pexp_while(_, _)
    )
}

/// Check if expression is "simple" - doesn't need parens (with arena for complete list checking)
fn is_simple_expression_with_arena(expr: &Expression, arena: &ParseArena) -> bool {
    if !expr.pexp_attributes.is_empty() {
        return false;
    }
    // Check for simple constructs first (handles complete lists)
    if is_simple_construct(expr, arena) {
        return true;
    }
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_ident(_)
            | ExpressionDesc::Pexp_constant(_)
            | ExpressionDesc::Pexp_tuple(_)
            | ExpressionDesc::Pexp_array(_)
            | ExpressionDesc::Pexp_record(_, _)
            | ExpressionDesc::Pexp_constraint(_, _)
            | ExpressionDesc::Pexp_coerce(_, _, _)
            | ExpressionDesc::Pexp_variant(_, None)
            | ExpressionDesc::Pexp_pack(_)
            | ExpressionDesc::Pexp_field(_, _)  // Field access is simple
            // OCaml's simple_expr handles these directly (no paren wrapping)
            | ExpressionDesc::Pexp_for(_, _, _, _, _)
            | ExpressionDesc::Pexp_while(_, _)
            | ExpressionDesc::Pexp_jsx_element(_)  // JSX elements are simple in OCaml
    )
}

/// Check if expression needs parens in "semi" context (array/list elements)
fn needs_parens_in_semi_context(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_fun { .. }
            | ExpressionDesc::Pexp_match(_, _)
            | ExpressionDesc::Pexp_try(_, _)
            | ExpressionDesc::Pexp_sequence(_, _)
    )
}

/// Check if an attribute is internal and should not be printed
fn is_internal_attribute(name: &str) -> bool {
    // Note: "res.await" IS printed (it's the AST marker for await modules)
    // "res.array.access" is internal and should not be printed
    matches!(name, "res.array.access")
}

/// Filter attributes to only include ones that should be printed
fn printable_attributes(attrs: &[(Located<String>, Payload)]) -> Vec<&(Located<String>, Payload)> {
    attrs
        .iter()
        .filter(|(name, _)| !is_internal_attribute(&name.txt))
        .collect()
}

/// Check if attributes contain a specific attribute name
fn has_attribute(attrs: &[(Located<String>, Payload)], name: &str) -> bool {
    attrs.iter().any(|(n, _)| n.txt == name)
}

/// Collect list elements from a cons pattern
fn collect_list_elements<'a>(expr: &'a Expression, arena: &ParseArena) -> (Vec<&'a Expression>, bool) {
    let mut elements = Vec::new();
    let mut current = expr;

    loop {
        if !current.pexp_attributes.is_empty() {
            elements.push(current);
            return (elements, false);
        }

        match &current.pexp_desc {
            ExpressionDesc::Pexp_construct(lid, None) => {
                if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                    let name = arena.get_string(*name_idx);
                    if name == "[]" {
                        return (elements, true);
                    }
                }
                elements.push(current);
                return (elements, false);
            }
            ExpressionDesc::Pexp_construct(lid, Some(arg)) => {
                if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                    let name = arena.get_string(*name_idx);
                    if name == "::" {
                        if let ExpressionDesc::Pexp_tuple(items) = &arg.pexp_desc {
                            if items.len() == 2 && arg.pexp_attributes.is_empty() {
                                elements.push(&items[0]);
                                current = &items[1];
                                continue;
                            }
                        }
                    }
                }
                elements.push(current);
                return (elements, false);
            }
            _ => {
                elements.push(current);
                return (elements, false);
            }
        }
    }
}

fn escape_string(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            c => result.push(c),
        }
    }
    result
}

// ============================================================================
// Public API
// ============================================================================

/// Print structure in ML format (like Pprintast in OCaml)
pub fn print_structure_ml(structure: &Structure, arena: &ParseArena, out: &mut impl Write) {
    let mut f = Formatter::new(out);
    for (i, item) in structure.iter().enumerate() {
        if i > 0 {
            f.newline();
        }
        print_structure_item(&mut f, arena, item);
    }
    f.flush();
}

/// Print signature in ML format
pub fn print_signature_ml(signature: &[SignatureItem], arena: &ParseArena, out: &mut impl Write) {
    let mut f = Formatter::new(out);
    for (i, item) in signature.iter().enumerate() {
        if i > 0 {
            f.newline();  // Top-level items on separate lines
        }
        print_signature_item(&mut f, arena, item);
    }
    f.flush();
}

// ============================================================================
// Structure items
// ============================================================================

fn print_structure_item<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, item: &StructureItem) {
    match &item.pstr_desc {
        StructureItemDesc::Pstr_eval(expr, attrs) => {
            f.string(";;");
            print_expression_no_outer_parens(f, arena, expr);
            print_item_attributes(f, arena, attrs);
        }
        StructureItemDesc::Pstr_value(rec_flag, bindings) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "let rec ",
                RecFlag::Nonrecursive => "let ",
            };
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    f.newline();
                }
                f.open_box(BoxKind::HOV, 2);
                if i > 0 {
                    f.string("and ");
                } else {
                    f.string(rec_str);
                }
                print_value_binding(f, arena, &binding.pvb_pat, &binding.pvb_expr);
                // Value binding attributes
                print_item_attributes(f, arena, &binding.pvb_attributes);
                f.close_box();
            }
        }
        StructureItemDesc::Pstr_primitive(vd) => {
            f.open_box(BoxKind::HOV, 2);
            f.string("external ");
            f.string(&vd.pval_name.txt);
            f.string(" : ");
            print_core_type(f, arena, &vd.pval_type);
            for prim in &vd.pval_prim {
                f.string(" = \"");
                f.string(prim);
                f.string("\"");
            }
            print_item_attributes(f, arena, &vd.pval_attributes);
            f.close_box();
        }
        StructureItemDesc::Pstr_type(rec_flag, decls) => {
            // OCaml: @[<2>type %a%a%s%s%a@]%a for first, @[<2>and %a@]%a for others
            // Each declaration is wrapped in a box with indent 2
            let rec_str = match rec_flag {
                RecFlag::Recursive => "",
                RecFlag::Nonrecursive => " nonrec",
            };
            for (i, decl) in decls.iter().enumerate() {
                if i > 0 {
                    f.newline();
                }
                // Open box with indent 2 for this declaration
                f.open_box(BoxKind::HOV, 2);
                if i == 0 {
                    f.string("type");
                    f.string(rec_str);
                    f.string(" ");
                } else {
                    f.string("and ");
                }
                print_type_declaration(f, arena, decl);
                f.close_box();
                // NOTE: print_type_declaration already prints ptype_attributes
            }
        }
        StructureItemDesc::Pstr_typext(ext) => {
            // OCaml: pp f "@[<2>type %a%a += %a@ %a@]%a"
            // Format: "type t +=  \n  | Foo \n  | Bar"
            f.open_box(BoxKind::HV, 2);
            f.string("type ");
            // Print type params if present
            if !ext.ptyext_params.is_empty() {
                f.string("(");
                for (i, (ty, _variance)) in ext.ptyext_params.iter().enumerate() {
                    if i > 0 {
                        f.string(",");
                    }
                    print_core_type(f, arena, ty);
                }
                f.string(") ");
            }
            print_longident_idx(f, arena, ext.ptyext_path.txt);
            f.string(" += ");
            if matches!(ext.ptyext_private, PrivateFlag::Private) {
                f.string("private ");
            }
            // Extra space before line break to match OCaml's "+=  \n"
            f.string(" ");
            // Each constructor starts with @\n|@; (newline, pipe, break)
            for ctor in ext.ptyext_constructors.iter() {
                f.newline();
                f.string("| ");
                print_extension_constructor(f, arena, ctor);
            }
            f.close_box();
            print_item_attributes(f, arena, &ext.ptyext_attributes);
        }
        StructureItemDesc::Pstr_exception(ext) => {
            // OCaml: pp f "@[<hov2>exception@ %a@]"
            // The HOV2 box means inline records get +2 indentation
            f.open_box(BoxKind::HOV, 2);
            f.string("exception ");
            print_extension_constructor(f, arena, ext);
            f.close_box();
        }
        StructureItemDesc::Pstr_module(mb) => {
            f.string("module ");
            f.string(&mb.pmb_name.txt);
            // OCaml: module_helper unwraps Pmod_functor with no attributes
            // and prints functor parameters inline: `module F(A:X) = body`
            let mut me = &mb.pmb_expr;
            while let ModuleExprDesc::Pmod_functor(name, mt, body) = &me.pmod_desc {
                if me.pmod_attributes.is_empty() {
                    if let Some(mt) = mt {
                        f.string("(");
                        f.string(&name.txt);
                        f.string(":");
                        print_module_type(f, arena, mt);
                        f.string(")");
                    } else {
                        f.string("()");
                    }
                    me = body;
                } else {
                    break;
                }
            }
            // OCaml: special case for Pmod_constraint with ident or signature type
            // prints "module X : mt = me'" instead of "module X = (me' : mt)"
            if let ModuleExprDesc::Pmod_constraint(inner_me, mt) = &me.pmod_desc {
                let is_sugar_type = matches!(
                    mt.pmty_desc,
                    ModuleTypeDesc::Pmty_ident(..) | ModuleTypeDesc::Pmty_signature(..)
                );
                if is_sugar_type && me.pmod_attributes.is_empty() {
                    f.string(" :");
                    f.space();
                    print_module_type(f, arena, mt);
                    f.space();
                    f.string("=");
                    f.space();
                    print_module_expr(f, arena, inner_me);
                    f.space();  // OCaml: trailing @; produces space
                } else {
                    f.string(" = ");
                    print_module_expr(f, arena, me);
                }
            } else {
                f.string(" = ");
                print_module_expr(f, arena, me);
            }
        }
        StructureItemDesc::Pstr_recmodule(mbs) => {
            for (i, mb) in mbs.iter().enumerate() {
                if i == 0 {
                    f.string("module rec ");
                } else {
                    f.string("  and ");  // Two spaces before "and"
                }
                f.string(&mb.pmb_name.txt);
                // Check for constraint sugar: `A:Mt = Me` instead of `A = (Me : Mt)`
                if let ModuleExprDesc::Pmod_constraint(me, mt) = &mb.pmb_expr.pmod_desc {
                    f.string(":");
                    print_module_type(f, arena, mt);
                    f.string(" = ");
                    print_module_expr(f, arena, me);
                } else {
                    f.string(" = ");
                    print_module_expr(f, arena, &mb.pmb_expr);
                }
                // Print module binding attributes
                print_item_attributes(f, arena, &mb.pmb_attributes);
            }
        }
        StructureItemDesc::Pstr_modtype(mtd) => {
            // OCaml: pp f "@[<hov2>module@ type@ %s%a@]%a"
            // where inner is: pp_print_space f (); pp f "@ =@ %a" (module_type ctxt) mt
            // This produces two spaces before = when not breaking: "Sig  ="
            f.open_box(BoxKind::HOV, 2);
            f.string("module");
            f.space();
            f.string("type");
            f.space();
            f.string(&mtd.pmtd_name.txt);
            if let Some(mt) = &mtd.pmtd_type {
                // pp_print_space + "@ =" gives two spaces before =
                f.string(" ");  // literal space from pp_print_space
                f.space();      // break hint before =
                f.string("=");
                f.space();
                print_module_type(f, arena, mt);
            }
            f.close_box();
            print_item_attributes(f, arena, &mtd.pmtd_attributes);
        }
        StructureItemDesc::Pstr_open(od) => {
            f.string("open");
            if matches!(od.popen_override, OverrideFlag::Override) {
                f.string("!");
            }
            f.string(" ");
            print_longident_idx(f, arena, od.popen_lid.txt);
            print_item_attributes(f, arena, &od.popen_attributes);
        }
        StructureItemDesc::Pstr_include(incl) => {
            f.string("include ");
            print_module_expr(f, arena, &incl.pincl_mod);
            print_item_attributes(f, arena, &incl.pincl_attributes);
        }
        StructureItemDesc::Pstr_attribute((name, payload)) => {
            // Standalone/floating attributes use [@@@...] (3 @)
            f.string("[@@@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            } else {
                f.string(" ");
            }
            f.string("]");
        }
        StructureItemDesc::Pstr_extension((name, payload), attrs) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            print_item_attributes(f, arena, attrs);
            f.close_box();
        }
    }
}

// ============================================================================
// Value bindings
// ============================================================================

fn print_value_binding<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, pat: &Pattern, expr: &Expression) {
    if !expr.pexp_attributes.is_empty() {
        print_pattern(f, arena, pat);
        f.string(" =");
        f.space();
        print_expression(f, arena, expr);
        return;
    }

    // Special case: Ppat_constraint - different handling based on type
    // OCaml pprintast.ml lines 1096-1103
    if let PatternDesc::Ppat_constraint(inner_pat, typ) = &pat.ppat_desc {
        if pat.ppat_attributes.is_empty() {
            if matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_poly(..)) && typ.ptyp_attributes.is_empty() {
                // Ptyp_poly: print without outer parens
                // e.g. "let t : 'a . t = x"
                print_simple_pattern(f, arena, inner_pat);
                f.string(" : ");
                print_core_type(f, arena, typ);
                f.string(" =");
                f.space();
                print_expression_no_outer_parens(f, arena, expr);
                return;
            } else {
                // Non-poly: print with outer parens using simple_pattern for inner
                // e.g. "let ((`Instance component) : React.t) = x"
                f.string("(");
                print_simple_pattern(f, arena, inner_pat);
                f.string(" : ");
                print_core_type(f, arena, typ);
                f.string(")");
                f.string(" =");
                f.space();
                print_expression_no_outer_parens(f, arena, expr);
                return;
            }
        }
    }

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            arity,
            is_async,
        } => {
            print_pattern(f, arena, pat);
            f.string(" ");

            if *is_async {
                f.string("async ");
            }

            // Print arity annotation
            if let Arity::Full(n) = arity {
                f.string(&format!("[arity:{}]", n));
            } else {
                let computed_arity = count_function_arity(expr);
                if computed_arity > 0 {
                    f.string(&format!("[arity:{}]", computed_arity));
                }
            }

            print_inline_param(f, arena, arg_label, default, lhs);
            let body = print_remaining_inline_params(f, arena, rhs);

            f.string("=");
            f.space();
            print_expression_no_outer_parens(f, arena, body);
        }
        ExpressionDesc::Pexp_newtype(name, inner) => {
            // Handle locally abstract types at the start: let f (type t) ...
            print_pattern(f, arena, pat);
            f.string(" (type ");
            f.string(&name.txt);
            f.string(") ");
            // Continue collecting newtypes and print arity when we hit a fun
            let body = print_remaining_inline_params_with_arity(f, arena, inner, true);
            f.string("=");
            f.space();
            print_expression_no_outer_parens(f, arena, body);
        }
        _ => {
            print_pattern(f, arena, pat);
            f.string(" =");
            f.space();
            print_expression_no_outer_parens(f, arena, expr);
        }
    }
}

fn print_inline_param<W: Write>(
    f: &mut Formatter<W>,
    arena: &ParseArena,
    label: &ArgLabel,
    default: &Option<Box<Expression>>,
    pat: &Pattern,
) {
    match label {
        ArgLabel::Nolabel => {
            if pattern_needs_parens_as_param(pat, arena) {
                f.string("(");
                print_pattern(f, arena, pat);
                f.string(") ");
            } else {
                print_pattern(f, arena, pat);
                f.string(" ");
            }
        }
        ArgLabel::Labelled(name) => {
            let name_str = arena.get_string(name.txt);
            if pattern_is_simple_var(pat, name_str) {
                f.string("~");
                f.string(name_str);
                f.string("  ");
            } else {
                f.string("~");
                f.string(name_str);
                f.string(":");
                print_pattern(f, arena, pat);
                f.string("  ");
            }
        }
        ArgLabel::Optional(name) => {
            let name_str = arena.get_string(name.txt);
            if let Some(def) = default {
                if pattern_is_simple_var(pat, name_str) {
                    f.string("?(");
                    f.string(name_str);
                    f.string("= ");
                    print_expression(f, arena, def);
                    f.string(")  ");
                } else {
                    f.string("?");
                    f.string(name_str);
                    f.string(":(");
                    print_pattern(f, arena, pat);
                    f.string("= ");
                    print_expression(f, arena, def);
                    f.string(")  ");
                }
            } else if pattern_is_simple_var(pat, name_str) {
                f.string("?");
                f.string(name_str);
                f.string("  ");
            } else {
                f.string("?");
                f.string(name_str);
                f.string(":");
                print_pattern(f, arena, pat);
                f.string("  ");
            }
        }
    }
}

fn print_remaining_inline_params<'a, W: Write>(
    f: &mut Formatter<W>,
    arena: &ParseArena,
    expr: &'a Expression,
) -> &'a Expression {
    print_remaining_inline_params_with_arity(f, arena, expr, false)
}

/// Helper to print remaining inline params, optionally printing arity on first Pexp_fun
fn print_remaining_inline_params_with_arity<'a, W: Write>(
    f: &mut Formatter<W>,
    arena: &ParseArena,
    expr: &'a Expression,
    need_arity: bool,
) -> &'a Expression {
    if !expr.pexp_attributes.is_empty() {
        return expr;
    }

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            arity,
            ..
        } => {
            // Print arity for each Pexp_fun that has an arity annotation
            // (In uncurried-by-default mode, there can be multiple arity markers)
            if let Arity::Full(n) = arity {
                f.string(&format!("[arity:{}]", n));
            } else if need_arity {
                // Only compute arity for the first function if no explicit arity
                let computed_arity = count_function_arity(expr);
                if computed_arity > 0 {
                    f.string(&format!("[arity:{}]", computed_arity));
                }
            }
            print_inline_param(f, arena, arg_label, default, lhs);
            print_remaining_inline_params_with_arity(f, arena, rhs, false)
        }
        ExpressionDesc::Pexp_newtype(name, inner) => {
            // Print (type t) inline, continue looking for arity
            f.string("(type ");
            f.string(&name.txt);
            f.string(") ");
            print_remaining_inline_params_with_arity(f, arena, inner, need_arity)
        }
        _ => expr,
    }
}

// ============================================================================
// Expressions
// ============================================================================

fn print_expression<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();

    if has_attrs {
        // OCaml Format: pp f "((%a)@,%a)" (expression ctxt) {x with pexp_attributes = []} (attributes ctxt) x.pexp_attributes
        // No explicit box - the cut allows breaking before attributes
        f.string("((");
        print_expression_inner(f, arena, expr, false);
        f.string(")");
        for (name, payload) in attrs {
            f.cut();
            f.string("[@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            }
            f.string(" ]");
        }
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false);
    }
}

fn print_expression_no_outer_parens<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();

    if has_attrs {
        // OCaml Format: pp f "((%a)@,%a)" - double parens with inner around expr
        f.string("((");
        print_expression_inner(f, arena, expr, false);
        f.string(")");
        for (name, payload) in attrs {
            f.cut(); // Allow break before attribute
            f.string("[@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            }
            f.string(" ]");
        }
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false);
    }
}

fn print_expression_simple<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    // OCaml's simple_expr wraps non-simple expressions in parens
    // BUT: attributed expressions already produce ((expr)[@attr]) structure
    // so we don't add extra parens for them
    let has_attrs = !printable_attributes(&expr.pexp_attributes).is_empty();

    if is_simple_expression_with_arena(expr, arena) {
        print_expression(f, arena, expr);
    } else if has_attrs {
        // Attributed expressions already produce their own parens structure
        print_expression(f, arena, expr);
    } else {
        f.string("(");
        print_expression(f, arena, expr);
        f.string(")");
    }
}

fn print_expression_semi_context<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();
    let needs_semi_parens = needs_parens_in_semi_context(expr);
    let is_simple = is_simple_expression(expr);

    if has_attrs {
        f.string("((");
        print_expression_inner(f, arena, expr, false);
        f.string(")");
        for (name, payload) in attrs {
            f.cut(); // Allow break before attribute
            f.string("[@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            }
            f.string(" ]");
        }
        f.string(")");
    } else if needs_semi_parens {
        f.string("((");
        print_expression_inner(f, arena, expr, false);
        f.string("))");
    } else if !is_simple {
        f.string("(");
        print_expression_inner(f, arena, expr, false);
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false);
    }
}

fn print_expression_list_context<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();
    let needs_semi_parens = needs_parens_in_semi_context(expr);

    if has_attrs {
        f.string("(");
        print_expression_inner(f, arena, expr, false);
        for (name, payload) in attrs {
            f.cut(); // Allow break before attribute
            f.string("[@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            }
            f.string(" ]");
        }
        f.string(")");
    } else if needs_semi_parens {
        f.string("(");
        print_expression_inner(f, arena, expr, false);
        f.string(")");
    } else {
        print_expression(f, arena, expr);
    }
}

fn print_expression_parens_if_complex<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    let attrs = printable_attributes(&expr.pexp_attributes);
    let has_attrs = !attrs.is_empty();
    let needs_parens = needs_parens_as_function_arg(expr, arena);
    let has_own_parens = expression_has_own_parens(expr);

    if has_attrs {
        // OCaml Format: pp f "((%a)@,%a)" - cut before attributes allows line break there
        if has_own_parens {
            f.string("(");
            print_expression_inner(f, arena, expr, false);
            for (name, payload) in attrs {
                f.cut(); // Allow break before attribute
                f.string("[@");
                f.string(&name.txt);
                if !payload_is_empty(payload) {
                    f.string(" ");
                    print_payload(f, arena, payload);
                }
                f.string(" ]");
            }
            f.string(")");
        } else {
            f.string("((");
            print_expression_inner(f, arena, expr, false);
            f.string(")");
            for (name, payload) in attrs {
                f.cut(); // Allow break before attribute
                f.string("[@");
                f.string(&name.txt);
                if !payload_is_empty(payload) {
                    f.string(" ");
                    print_payload(f, arena, payload);
                }
                f.string(" ]");
            }
            f.string(")");
        }
    } else if needs_parens && !has_own_parens {
        f.string("(");
        print_expression_inner(f, arena, expr, false);
        f.string(")");
    } else {
        print_expression_inner(f, arena, expr, false);
    }
}

fn print_fun_body<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            ..
        } if expr.pexp_attributes.is_empty() => {
            f.string("fun ");
            print_arg_label_expr(f, arena, arg_label);
            if let Some(def) = default {
                f.string("?(");
                print_pattern(f, arena, lhs);
                f.string(" = ");
                print_expression(f, arena, def);
                f.string(")");
            } else {
                print_pattern(f, arena, lhs);
            }
            f.string(" -> ");
            print_fun_body(f, arena, rhs);
        }
        _ => {
            print_expression_no_outer_parens(f, arena, expr);
        }
    }
}

fn print_expression_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, expr: &Expression, use_parens: bool) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            // OCaml wraps operators in parens when used as values
            if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                let name = arena.get_string(*name_idx);
                if ident_needs_parens(name) {
                    f.string("(");
                    f.string(name);
                    f.string(")");
                    return;
                }
            }
            print_longident_idx(f, arena, lid.txt);
        }
        ExpressionDesc::Pexp_constant(c) => {
            let is_negative = match c {
                Constant::Integer(s, _) | Constant::Float(s, _) => s.starts_with('-'),
                _ => false,
            };
            if is_negative {
                f.string("(");
            }
            print_constant(f, c);
            if is_negative {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => " rec",
                RecFlag::Nonrecursive => "",
            };
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            f.string("let");
            f.string(rec_str);
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    f.string(" and");
                }
                f.string(" ");
                print_pattern(f, arena, &binding.pvb_pat);
                f.string(" = ");
                print_expression(f, arena, &binding.pvb_expr);
            }
            f.string(" in");
            f.space();
            print_expression(f, arena, body);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            arity,
            is_async,
        } => {
            let arity_value = match arity {
                Arity::Full(n) => Some(*n),
                Arity::Unknown => {
                    let computed = count_function_arity(expr);
                    if computed > 0 { Some(computed) } else { None }
                }
            };
            if use_parens {
                f.string("(");
            }
            if *is_async {
                f.string("async ");
            }
            f.string("fun ");
            if let Some(n) = arity_value {
                f.string(&format!("[arity:{}]", n));
            }
            print_arg_label_expr(f, arena, arg_label);
            if let Some(def) = default {
                f.string("?(");
                print_pattern(f, arena, lhs);
                f.string(" = ");
                print_expression(f, arena, def);
                f.string(")");
            } else {
                print_pattern(f, arena, lhs);
            }
            f.string(" -> ");
            print_fun_body(f, arena, rhs);
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_apply { funct, args, partial, .. } => {
            // Array access: Array.get(arr, idx) -> arr.(idx)
            // Also handles @res.array.access attribute for backwards compatibility
            if (parsetree_viewer::is_array_access(arena, expr) || has_attribute(&expr.pexp_attributes, "res.array.access")) && args.len() == 2 {
                let (_, arr) = &args[0];
                let (_, idx) = &args[1];
                if use_parens {
                    f.string("(");
                }
                print_expression(f, arena, arr);
                f.string(".(");
                print_expression(f, arena, idx);
                f.string(")");
                if use_parens {
                    f.string(")");
                }
                return;
            }
            // Array set: Array.set(arr, idx, val) -> arr.(idx) <- val
            if parsetree_viewer::is_array_set(arena, expr) && args.len() == 3 {
                let (_, arr) = &args[0];
                let (_, idx) = &args[1];
                let (_, val) = &args[2];
                if use_parens {
                    f.string("(");
                }
                print_expression(f, arena, arr);
                f.string(".(");
                print_expression(f, arena, idx);
                f.string(") <- ");
                print_expression(f, arena, val);
                if use_parens {
                    f.string(")");
                }
                return;
            }

            // Infix/prefix operators
            if let Some(op_name) = get_operator_name(funct, arena) {
                if is_infix_operator(op_name) && args.len() == 2 {
                    let (_, lhs) = &args[0];
                    let (_, rhs) = &args[1];
                    if use_parens {
                        f.string("(");
                    }
                    f.open_box(BoxKind::HOV, 2);
                    // OCaml uses simple_expr which wraps non-simple exprs in parens
                    print_expression_simple(f, arena, lhs);
                    f.space();
                    f.string(op_name);
                    f.space();
                    print_expression_simple(f, arena, rhs);
                    if *partial {
                        f.string(" ...");
                    }
                    f.close_box();
                    if use_parens {
                        f.string(")");
                    }
                    return;
                } else if is_prefix_operator(op_name) && args.len() == 1 {
                    let (_, arg) = &args[0];
                    // OCaml converts ~-, ~+, ~-., ~+., ~~ to shorter forms unless arg is a constant
                    // (to avoid (- 1) being parsed as negative literal)
                    let shortened_op = match op_name {
                        "~-" | "~+" | "~-." | "~+." | "~~" => {
                            if !matches!(&arg.pexp_desc, ExpressionDesc::Pexp_constant(_)) {
                                &op_name[1..] // Remove leading ~
                            } else {
                                op_name
                            }
                        }
                        _ => op_name,
                    };
                    // OCaml: @[<2>%s@;%a@] - no outer parens
                    f.open_box(BoxKind::HOV, 2);
                    f.string(shortened_op);
                    f.space();
                    print_expression_simple(f, arena, arg);
                    if *partial {
                        f.string(" ...");
                    }
                    f.close_box();
                    return;
                }
            }

            // Default: prefix application
            // OCaml: pp f "@[<hov2>%a@ %a@]" with break hint between func and args
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            print_expression(f, arena, funct);
            for (label, arg) in args {
                f.space(); // Break hint between func and args
                print_arg_with_label(f, arena, label, arg);
            }
            if *partial {
                f.string(" ...");
            }
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_match(scrutinee, cases) => {
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::HOV, 2);
            f.string("match ");
            print_expression(f, arena, scrutinee);
            f.close_box();
            f.space();
            f.string("with");
            f.close_box();
            for case in cases {
                f.space();
                f.string("| ");
                // OCaml: @[<2>%a%a@;->@;%a@]
                f.open_box(BoxKind::HOV, 2);
                print_pattern(f, arena, &case.pc_lhs);
                if let Some(guard) = &case.pc_guard {
                    f.space();
                    f.string("when ");
                    print_expression(f, arena, guard);
                }
                f.space();
                f.string("->");
                f.space();
                print_expression(f, arena, &case.pc_rhs);
                f.close_box();
            }
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_try(body, cases) => {
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 0);
            f.open_box(BoxKind::HV, 2);
            f.string("try ");
            print_expression(f, arena, body);
            f.close_box();
            f.space();
            f.open_box(BoxKind::HOV, 0);
            f.string("with");
            for case in cases {
                f.space();
                f.string("| ");
                f.open_box(BoxKind::HOV, 2);
                print_pattern(f, arena, &case.pc_lhs);
                if let Some(guard) = &case.pc_guard {
                    f.string(" when ");
                    print_expression(f, arena, guard);
                }
                f.string(" -> ");
                print_expression(f, arena, &case.pc_rhs);
                f.close_box();
            }
            f.close_box();
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_tuple(exprs) => {
            f.string("(");
            for (i, e) in exprs.iter().enumerate() {
                if i > 0 {
                    f.string(", ");
                }
                print_expression(f, arena, e);
            }
            f.string(")");
        }
        ExpressionDesc::Pexp_construct(lid, arg) => {
            // Special handling for list syntax
            if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                let name = arena.get_string(*name_idx);
                if name == "::" {
                    let (elements, is_complete) = collect_list_elements(expr, arena);
                    if is_complete && !elements.is_empty() {
                        f.string("[");
                        for (i, elem) in elements.iter().enumerate() {
                            if i > 0 {
                                f.string("; ");
                            }
                            print_expression_list_context(f, arena, elem);
                        }
                        f.string("]");
                        return;
                    }
                    for (i, elem) in elements.iter().enumerate() {
                        if i > 0 {
                            f.string(" :: ");
                        }
                        print_expression(f, arena, elem);
                    }
                    return;
                } else if name == "[]" {
                    f.string("[]");
                    return;
                } else if name == "()" {
                    f.string("()");
                    return;
                }
            }
            print_longident_idx(f, arena, lid.txt);
            if let Some(a) = arg {
                f.string(" ");
                print_expression(f, arena, a);
            }
        }
        ExpressionDesc::Pexp_variant(label, arg) => {
            f.string("`");
            f.string(label);
            if let Some(a) = arg {
                f.string(" ");
                print_expression(f, arena, a);
            }
        }
        ExpressionDesc::Pexp_record(fields, base) => {
            f.string("{ ");
            if let Some(b) = base {
                print_expression(f, arena, b);
                f.string(" with ");
            }
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    f.string("; ");
                }
                // Check for punning: { a } is shorthand for { a = a }
                // OCaml only uses punning when field is Lident (not qualified)
                let lid = arena.get_longident(field.lid.txt);
                let is_punned = if let Longident::Lident(name_idx) = lid {
                    let fname = arena.get_string(*name_idx);
                    // Check if expression is Pexp_ident(Lident(fname))
                    if let ExpressionDesc::Pexp_ident(expr_lid) = &field.expr.pexp_desc {
                        match arena.get_longident(expr_lid.txt) {
                            Longident::Lident(var_idx) => arena.get_string(*var_idx) == fname,
                            _ => false,
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if is_punned {
                    print_longident_idx(f, arena, field.lid.txt);
                    if field.opt {
                        f.string("?");  // Optional punned field: name?
                    }
                } else {
                    print_longident_idx(f, arena, field.lid.txt);
                    f.string(" = ");
                    if field.opt {
                        f.string("?");  // Optional field: name = ?value
                    }
                    print_expression(f, arena, &field.expr);
                }
            }
            f.string(" }");
        }
        ExpressionDesc::Pexp_field(obj, field) => {
            print_expression_simple(f, arena, obj);
            f.string(".");
            print_longident_idx(f, arena, field.txt);
        }
        ExpressionDesc::Pexp_setfield(obj, field, value) => {
            print_expression(f, arena, obj);
            f.string(".");
            print_longident_idx(f, arena, field.txt);
            f.string(" <- ");
            print_expression(f, arena, value);
        }
        ExpressionDesc::Pexp_array(elems) => {
            f.open_box(BoxKind::HOV, 0);
            f.open_box(BoxKind::HOV, 2);
            f.string("[|");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    f.string(";");
                }
                print_expression_semi_context(f, arena, e);
            }
            f.string("|]");
            f.close_box();
            f.close_box();
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_expr, else_expr) => {
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::HOV, 2);
            f.string("if ");
            print_expression(f, arena, cond);
            f.close_box();
            f.space();
            f.open_box(BoxKind::HOV, 2);
            f.string("then ");
            print_expression(f, arena, then_expr);
            f.close_box();
            if let Some(e) = else_expr {
                f.space();
                f.open_box(BoxKind::HOV, 2);
                f.string("else ");
                print_expression(f, arena, e);
                f.close_box();
            }
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            if use_parens {
                f.string("(");
            }
            // First element - wrap if it's a let (but not sequence, which prints flat)
            if matches!(&e1.pexp_desc, ExpressionDesc::Pexp_let(_, _, _)) {
                f.string("(");
                print_expression(f, arena, e1);
                f.string(")");
            } else {
                print_expression(f, arena, e1);
            }
            f.string("; ");
            // Second element - wrap if it's a let (sequence nests without extra parens)
            if matches!(&e2.pexp_desc, ExpressionDesc::Pexp_let(_, _, _)) {
                f.string("(");
                print_expression(f, arena, e2);
                f.string(")");
            } else {
                print_expression(f, arena, e2);
            }
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            // OCaml: @[<2>while@;%a@;do@;%a@;done@]
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            f.string("while");
            f.space();
            print_expression(f, arena, cond);
            f.space();
            f.string("do");
            f.space();
            print_expression(f, arena, body);
            f.space();
            f.string("done");
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_for(pat, start, end, dir, body) => {
            if use_parens {
                f.string("(");
            }
            f.string("for ");
            print_pattern(f, arena, pat);
            f.string(" = ");
            print_expression(f, arena, start);
            let dir_str = match dir {
                DirectionFlag::Upto => " to ",
                DirectionFlag::Downto => " downto ",
            };
            f.string(dir_str);
            print_expression(f, arena, end);
            f.string(" do ");
            print_expression(f, arena, body);
            f.string(" done");
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_constraint(e, t) => {
            f.string("(");
            print_expression(f, arena, e);
            f.string(" : ");
            print_core_type(f, arena, t);
            f.string(")");
        }
        ExpressionDesc::Pexp_coerce(e, t1, t2) => {
            f.string("(");
            print_expression(f, arena, e);
            if let Some(t) = t1 {
                f.string(" : ");
                print_core_type(f, arena, t);
            }
            f.string(" :> ");
            print_core_type(f, arena, t2);
            f.string(")");
        }
        ExpressionDesc::Pexp_send(e, meth) => {
            print_expression(f, arena, e);
            f.string("#");
            f.string(&meth.txt);
        }
        ExpressionDesc::Pexp_letmodule(name, mexpr, body) => {
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            f.string("let module ");
            f.string(&name.txt);
            f.string(" = ");
            print_module_expr(f, arena, mexpr);
            f.string(" in");
            f.space();
            print_expression(f, arena, body);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_letexception(ext, body) => {
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            f.string("let exception ");
            print_extension_constructor(f, arena, ext);
            // OCaml has @;%a for attributes which produces trailing space even when no attrs
            f.string("  in"); // Double space to match OCaml's trailing break from constructor_declaration
            f.space();
            print_expression(f, arena, body);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_assert(e) => {
            // OCaml: @[<hov2>assert@ %a@] - no parens
            // Parens come from simple_expr when needed
            f.open_box(BoxKind::HOV, 2);
            f.string("assert ");
            print_expression_simple(f, arena, e);
            f.close_box();
        }
        ExpressionDesc::Pexp_newtype(name, body) => {
            f.string("(fun (type ");
            f.string(&name.txt);
            f.string(") -> ");
            print_expression(f, arena, body);
            f.string(")");
        }
        ExpressionDesc::Pexp_pack(mexpr) => {
            f.string("(module ");
            print_module_expr(f, arena, mexpr);
            f.string(")");
        }
        ExpressionDesc::Pexp_open(_override_flag, lid, body) => {
            if use_parens {
                f.string("(");
            }
            f.open_box(BoxKind::HOV, 2);
            f.string("let open ");
            print_longident_idx(f, arena, lid.txt);
            f.string(" in");
            f.space();
            print_expression(f, arena, body);
            f.close_box();
            if use_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_extension((name, payload)) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
        ExpressionDesc::Pexp_await(e) => {
            f.string("await ");
            // Check if expression is "simple" enough to not need parens
            // Note: attributed expressions will get their own parens from print_expression
            let has_attrs = !printable_attributes(&e.pexp_attributes).is_empty();
            let is_simple = matches!(
                &e.pexp_desc,
                ExpressionDesc::Pexp_ident(_)
                    | ExpressionDesc::Pexp_constant(_)
                    | ExpressionDesc::Pexp_construct(_, None)
                    | ExpressionDesc::Pexp_tuple(_)
                    | ExpressionDesc::Pexp_array(_)
                    | ExpressionDesc::Pexp_record(_, _)
            );
            // If has attributes, print_expression already produces ((expr)[@attr ])
            // so we don't need to add extra parens
            let needs_parens = !is_simple && !has_attrs;
            if needs_parens {
                f.string("(");
            }
            print_expression(f, arena, e);
            if needs_parens {
                f.string(")");
            }
        }
        ExpressionDesc::Pexp_jsx_element(jsx) => {
            print_jsx_element(f, arena, jsx);
        }
    }
}

// ============================================================================
// JSX Printing
// ============================================================================

fn jsx_tag_name_to_string(tag_name: &JsxTagName, arena: &ParseArena) -> String {
    match tag_name {
        JsxTagName::Lower(name) => name.clone(),
        JsxTagName::QualifiedLower { path, name } => {
            let path_str = print_longident_to_string(arena.get_longident(*path), arena);
            format!("{}.{}", path_str, name)
        }
        JsxTagName::Upper(lident) => {
            print_longident_to_string(arena.get_longident(*lident), arena)
        }
        JsxTagName::Invalid(name) => name.clone(),
    }
}

fn print_longident_to_string(lid: &Longident, arena: &ParseArena) -> String {
    match lid {
        Longident::Lident(name_idx) => arena.get_string(*name_idx).to_string(),
        Longident::Ldot(prefix, name_idx) => {
            let prefix_str = print_longident_to_string(prefix.as_ref(), arena);
            let name = arena.get_string(*name_idx);
            format!("{}.{}", prefix_str, name)
        }
        Longident::Lapply(left, right) => {
            let left_str = print_longident_to_string(left.as_ref(), arena);
            let right_str = print_longident_to_string(right.as_ref(), arena);
            format!("{}({})", left_str, right_str)
        }
    }
}

fn print_jsx_element<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, jsx: &JsxElement) {
    match jsx {
        JsxElement::Fragment(frag) => {
            f.string("<>");
            for (i, child) in frag.children.iter().enumerate() {
                if i > 0 {
                    f.space(); // OCaml uses "@ " (space with optional break) between children
                }
                print_expression_simple(f, arena, child);
            }
            f.string("</>");
        }
        JsxElement::Unary(elem) => {
            let name = jsx_tag_name_to_string(&elem.tag_name.txt, arena);
            if elem.props.is_empty() {
                f.string(&format!("<{} />", name));
            } else {
                f.string(&format!("<{} ", name));
                print_jsx_props(f, arena, &elem.props);
                f.string(" />");
            }
        }
        JsxElement::Container(elem) => {
            let name = jsx_tag_name_to_string(&elem.tag_name_start.txt, arena);
            let closing_name = match &elem.closing_tag {
                None => String::new(),
                Some(closing) => {
                    let close_name = jsx_tag_name_to_string(&closing.name.txt, arena);
                    format!("</{}>", close_name)
                }
            };
            if elem.props.is_empty() {
                f.string(&format!("<{}>", name));
                for (i, child) in elem.children.iter().enumerate() {
                    if i > 0 {
                        f.space(); // OCaml uses "@ " between children
                    }
                    print_expression_simple(f, arena, child);
                }
                f.string(&closing_name);
            } else {
                f.string(&format!("<{} ", name));
                print_jsx_props(f, arena, &elem.props);
                f.string(">");
                for (i, child) in elem.children.iter().enumerate() {
                    if i > 0 {
                        f.space(); // OCaml uses "@ " between children
                    }
                    print_expression_simple(f, arena, child);
                }
                f.string(&closing_name);
            }
        }
    }
}

fn print_jsx_props<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, props: &[JsxProp]) {
    for (i, prop) in props.iter().enumerate() {
        if i > 0 {
            f.string(" ");
        }
        print_jsx_prop(f, arena, prop);
    }
}

fn print_jsx_prop<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, prop: &JsxProp) {
    match prop {
        JsxProp::Punning { optional, name } => {
            if *optional {
                f.string("?");
            }
            f.string(&name.txt);
        }
        JsxProp::Value { name, optional, value } => {
            f.string(&name.txt);
            f.string("=");
            if *optional {
                f.string("?");
            }
            print_expression_simple(f, arena, value);
        }
        JsxProp::Spreading { expr, .. } => {
            f.string("{...");
            print_expression_simple(f, arena, expr);
            f.string("}");
        }
    }
}

// ============================================================================
// Patterns
// ============================================================================

fn print_pattern<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, pat: &Pattern) {
    let has_attrs = !pat.ppat_attributes.is_empty();
    if has_attrs {
        f.string("((");
    }
    print_pattern_inner(f, arena, pat);
    if has_attrs {
        f.string(")");
        for (name, payload) in &pat.ppat_attributes {
            f.cut(); // Allow break before attribute
            f.string("[@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            }
            f.string(" ]");
        }
        f.string(")");
    }
}

/// Print a pattern in "simple" context - non-simple patterns get wrapped in parens.
/// This matches OCaml's simple_pattern which wraps non-simple patterns via:
/// `| _ -> paren true (pattern ctxt) f x`
fn print_simple_pattern<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, pat: &Pattern) {
    if pattern_is_simple(pat, arena) {
        print_pattern(f, arena, pat);
    } else {
        f.string("(");
        print_pattern(f, arena, pat);
        f.string(")");
    }
}

fn print_pattern_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, pat: &Pattern) {
    match &pat.ppat_desc {
        PatternDesc::Ppat_any => {
            f.string("_");
        }
        PatternDesc::Ppat_var(name) => {
            f.string(&name.txt);
        }
        PatternDesc::Ppat_alias(p, name) => {
            // OCaml: @[<2>%a@;as@;%a@] - no outer parens
            print_pattern(f, arena, p);
            f.string(" as ");
            f.string(&name.txt);
        }
        PatternDesc::Ppat_constant(c) => {
            // Negative numbers need parentheses in patterns
            if constant_is_negative(c) {
                f.string("(");
                print_constant(f, c);
                f.string(")");
            } else {
                print_constant(f, c);
            }
        }
        PatternDesc::Ppat_interval(c1, c2) => {
            // Negative numbers in intervals need parentheses
            let c1_needs_parens = constant_is_negative(c1);
            let c2_needs_parens = constant_is_negative(c2);
            if c1_needs_parens {
                f.string("(");
            }
            print_constant(f, c1);
            if c1_needs_parens {
                f.string(")");
            }
            f.string("..");
            if c2_needs_parens {
                f.string("(");
            }
            print_constant(f, c2);
            if c2_needs_parens {
                f.string(")");
            }
        }
        PatternDesc::Ppat_tuple(pats) => {
            f.string("(");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    f.string(", ");
                }
                // Alias patterns inside tuples need parentheses
                let needs_parens = matches!(p.ppat_desc, PatternDesc::Ppat_alias(..));
                if needs_parens {
                    f.string("(");
                }
                print_pattern(f, arena, p);
                if needs_parens {
                    f.string(")");
                }
            }
            f.string(")");
        }
        PatternDesc::Ppat_construct(lid, arg) => {
            // Special case for cons pattern - no outer parens by default
            if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                let name = arena.get_string(*name_idx);
                if name == "::" {
                    if let Some(a) = arg {
                        if let PatternDesc::Ppat_tuple(elements) = &a.ppat_desc {
                            if elements.len() == 2 {
                                // Left element needs parens if it's an alias or constraint
                                let left_needs_parens = pattern_needs_parens_as_param(&elements[0], arena);
                                if left_needs_parens {
                                    f.string("(");
                                }
                                print_pattern(f, arena, &elements[0]);
                                if left_needs_parens {
                                    f.string(")");
                                }
                                f.string("::");
                                // Right element needs parens if it's an alias (so it doesn't
                                // look like `(x::xs) as tail`)
                                let right_needs_parens = matches!(
                                    &elements[1].ppat_desc,
                                    PatternDesc::Ppat_alias(..)
                                );
                                if right_needs_parens {
                                    f.string("(");
                                }
                                print_pattern(f, arena, &elements[1]);
                                if right_needs_parens {
                                    f.string(")");
                                }
                                return;
                            }
                        }
                    }
                }
            }
            print_longident_idx(f, arena, lid.txt);
            if let Some(a) = arg {
                f.string(" ");
                // Alias patterns in constructor arguments need parentheses
                let needs_parens = matches!(a.ppat_desc, PatternDesc::Ppat_alias(..));
                if needs_parens {
                    f.string("(");
                }
                print_pattern(f, arena, a);
                if needs_parens {
                    f.string(")");
                }
            }
        }
        PatternDesc::Ppat_variant(label, arg) => {
            f.string("`");
            f.string(label);
            if let Some(a) = arg {
                f.string(" ");
                // OCaml uses simple_pattern here - wrap alias/or patterns in parens
                let needs_parens = !pattern_is_simple(a, arena);
                if needs_parens {
                    f.string("(");
                }
                print_pattern(f, arena, a);
                if needs_parens {
                    f.string(")");
                }
            }
        }
        PatternDesc::Ppat_record(fields, closed) => {
            f.string("{ ");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    f.string("; ");
                }
                // OCaml only punns for simple Lident fields, not qualified Ldot fields
                // Pattern: { Lident s, Ppat_var { txt } } when s = txt -> punning
                let is_punned = match arena.get_longident(field.lid.txt) {
                    Longident::Lident(name_idx) => {
                        let fname = arena.get_string(*name_idx);
                        // Also require no attributes on the pattern
                        field.pat.ppat_attributes.is_empty() &&
                            matches!(&field.pat.ppat_desc, PatternDesc::Ppat_var(var) if var.txt == fname)
                    }
                    _ => false,
                };

                if is_punned {
                    print_longident_idx(f, arena, field.lid.txt);
                    if field.opt {
                        f.string("?");  // Optional punned field: name?
                    }
                } else {
                    print_longident_idx(f, arena, field.lid.txt);
                    if field.opt {
                        f.string("?");  // Optional field: name? = pattern
                    }
                    f.string(" = ");
                    // Print with parens for alias patterns (pattern1 context)
                    let needs_parens = matches!(&field.pat.ppat_desc,
                        PatternDesc::Ppat_alias(..) | PatternDesc::Ppat_or(..)
                    );
                    if needs_parens {
                        f.string("(");
                    }
                    print_pattern(f, arena, &field.pat);
                    if needs_parens {
                        f.string(")");
                    }
                }
            }
            if matches!(closed, ClosedFlag::Open) {
                f.string(";_}");
            } else {
                f.string(" }");
            }
        }
        PatternDesc::Ppat_array(pats) => {
            f.string("[|");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    f.string(";");
                }
                print_pattern(f, arena, p);
            }
            f.string("|]");
        }
        PatternDesc::Ppat_or(p1, p2) => {
            print_pattern(f, arena, p1);
            f.string("|");
            print_pattern(f, arena, p2);
        }
        PatternDesc::Ppat_constraint(p, t) => {
            f.string("(");
            // Alias, or, and cons patterns inside constraints need extra parens
            let needs_inner_parens = matches!(
                p.ppat_desc,
                PatternDesc::Ppat_alias(..) | PatternDesc::Ppat_or(..)
            ) || is_cons_pattern(p, arena);
            if needs_inner_parens {
                f.string("(");
            }
            print_pattern(f, arena, p);
            if needs_inner_parens {
                f.string(")");
            }
            f.string(" : ");
            print_core_type(f, arena, t);
            f.string(")");
        }
        PatternDesc::Ppat_type(lid) => {
            f.string("#");
            print_longident_idx(f, arena, lid.txt);
        }
        PatternDesc::Ppat_unpack(name) => {
            f.string("(module ");
            f.string(&name.txt);
            f.string(")");
        }
        PatternDesc::Ppat_exception(p) => {
            f.string("exception ");
            // Or-patterns and alias patterns inside exception need parentheses
            let needs_parens = matches!(
                p.ppat_desc,
                PatternDesc::Ppat_or(..) | PatternDesc::Ppat_alias(..)
            );
            if needs_parens {
                f.string("(");
            }
            print_pattern(f, arena, p);
            if needs_parens {
                f.string(")");
            }
        }
        PatternDesc::Ppat_extension((name, payload)) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
        PatternDesc::Ppat_open(lid, p) => {
            print_longident_idx(f, arena, lid.txt);
            f.string(".(");
            print_pattern(f, arena, p);
            f.string(")");
        }
    }
}

// ============================================================================
// Types
// ============================================================================

fn print_core_type<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, typ: &CoreType) {
    // Types with attributes are wrapped: ((type)[@attr ])
    let has_attrs = printable_attributes(&typ.ptyp_attributes).len() > 0;
    if has_attrs {
        f.string("((");
    }
    print_core_type_inner(f, arena, typ);
    if has_attrs {
        f.string(")");
        print_attributes(f, arena, &typ.ptyp_attributes);
        f.string(")");
    }
}

fn print_core_type_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, typ: &CoreType) {
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => {
            f.string("_");
        }
        CoreTypeDesc::Ptyp_var(name) => {
            f.string("'");
            f.string(name);
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, arity } => {
            print_arg_label(f, arena, &arg.lbl);
            // Arrow types as args need parentheses (OCaml's core_type1 wraps them)
            let arg_needs_parens = matches!(arg.typ.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. });
            if arg_needs_parens {
                f.string("(");
            }
            print_core_type(f, arena, &arg.typ);
            if arg_needs_parens {
                f.string(")");
            }
            // Print attributes on the argument (after the type)
            print_attributes(f, arena, &arg.attrs);
            f.string(" -> ");
            print_core_type(f, arena, ret);
            if let Arity::Full(n) = arity {
                f.string(&format!(" (a:{})", n));
            }
        }
        CoreTypeDesc::Ptyp_tuple(types) => {
            f.string("(");
            for (i, t) in types.iter().enumerate() {
                if i > 0 {
                    f.string(" * ");
                }
                // Alias types inside tuple need parens for precedence
                let needs_parens = matches!(t.ptyp_desc, CoreTypeDesc::Ptyp_alias(..));
                if needs_parens {
                    f.string("(");
                }
                print_core_type(f, arena, t);
                if needs_parens {
                    f.string(")");
                }
            }
            f.string(")");
        }
        CoreTypeDesc::Ptyp_constr(lid, args) => {
            // OCaml format: list ~first:"(" ~last:")@;" (core_type ctxt) ~sep:",@;" f l
            // On single line: (type , type ) constructor
            if !args.is_empty() {
                if args.len() == 1 {
                    print_core_type(f, arena, &args[0]);
                    // Single arg has @; after = space
                    f.string(" ");
                } else {
                    f.string("(");
                    for (i, t) in args.iter().enumerate() {
                        if i > 0 {
                            // Separator is ,@; = comma + break = ", "
                            f.string(", ");
                        }
                        print_core_type(f, arena, t);
                    }
                    // last is ")@;" = ) + break = ") "
                    f.string(") ");
                }
            }
            print_longident_idx(f, arena, lid.txt);
        }
        CoreTypeDesc::Ptyp_object(fields, closed) => {
            // OCaml format: @[<hov2><@ %a%a@ > @]
            // Field format: @[<hov2>%s: %a@ %a@ @]
            f.string("< ");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    f.string(";");
                }
                match field {
                    ObjectField::Otag(name, attrs, typ) => {
                        // OCaml format: @[<hov2>%s: %a@ %a@ @]
                        // = name: type (break) attrs (break) close_box
                        // On single line: name: type<space>attrs<space>
                        f.string(&name.txt);
                        f.string(": ");
                        print_core_type(f, arena, typ);
                        // After type: @ (break = space)
                        f.string(" ");
                        // Attributes: %a then @ (break)
                        let printable = printable_attributes(attrs);
                        if !printable.is_empty() {
                            print_attributes(f, arena, attrs);
                        }
                        // Final break from @ before @]
                        f.string(" ");
                    }
                    ObjectField::Oinherit(typ) => {
                        // OCaml format: @[<hov2>%a@ @]
                        print_core_type(f, arena, typ);
                        f.string(" ");
                    }
                }
            }
            // Open object suffix
            // OCaml field_var: if fields empty then ".." else " ;.."
            if matches!(closed, ClosedFlag::Open) {
                if fields.is_empty() {
                    f.string("..");
                } else {
                    // Note: leading space before ;.. (combines with field's trailing space)
                    f.string(" ;..");
                }
            }
            f.string(" > ");
        }
        CoreTypeDesc::Ptyp_alias(t, name) => {
            // Arrow types inside alias need parens for precedence
            let needs_parens = matches!(t.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. });
            if needs_parens {
                f.string("(");
            }
            print_core_type(f, arena, t);
            if needs_parens {
                f.string(")");
            }
            f.string(" as '");
            f.string(name);
        }
        CoreTypeDesc::Ptyp_variant(rows, closed, labels) => {
            // OCaml: @[<2>[%a%a]@] with complex formatting
            f.string("[");

            // Empty variant cases
            if rows.is_empty() {
                match closed {
                    ClosedFlag::Open => f.string(">"),
                    ClosedFlag::Closed => {}
                }
            } else {
                // Non-empty: prefix + list
                match (closed, &labels) {
                    (ClosedFlag::Closed, None) => f.string(" "), // Just break
                    (ClosedFlag::Closed, Some(_)) => f.string("< "), // < + break
                    (ClosedFlag::Open, _) => f.string("> "), // > + break
                }

                for (i, row) in rows.iter().enumerate() {
                    if i > 0 {
                        // Separator: @;<1 -2>| = break + " | " on same line
                        f.string(" | ");
                    }
                    match row {
                        RowField::Rtag(label, attrs, empty, types) => {
                            // OCaml: @[<2>%a%a@;%a@]
                            f.string("`");
                            f.string(&label.txt);
                            if !*empty || !types.is_empty() {
                                // of@; = " of "
                                f.string(" of ");
                                for (j, t) in types.iter().enumerate() {
                                    if j > 0 {
                                        f.string("&");
                                    }
                                    print_core_type(f, arena, t);
                                }
                            }
                            // Trailing break (@;) then attrs
                            f.string(" ");
                            print_attributes(f, arena, attrs);
                        }
                        RowField::Rinherit(typ) => {
                            print_core_type(f, arena, typ);
                        }
                    }
                }
            }

            // Low labels (present for some polymorphic variants)
            // OCaml: Some [] | None -> () (do nothing)
            //        Some xs -> print "> " then labels
            if let Some(lbls) = labels {
                if !lbls.is_empty() {
                    f.string("> ");
                    for (i, lbl) in lbls.iter().enumerate() {
                        if i > 0 {
                            f.string(" ");
                        }
                        f.string("`");
                        f.string(lbl);
                    }
                }
            }
            f.string("]");
        }
        CoreTypeDesc::Ptyp_poly(vars, t) => {
            if !vars.is_empty() {
                for var in vars {
                    f.string("'");
                    f.string(&var.txt);
                    f.string(" ");
                }
                f.string(". ");
            }
            print_core_type(f, arena, t);
        }
        CoreTypeDesc::Ptyp_package((lid, constraints)) => {
            f.string("(module ");
            print_longident_idx(f, arena, lid.txt);
            for (i, (path, typ)) in constraints.iter().enumerate() {
                if i == 0 {
                    f.string(" with type ");
                } else {
                    f.string(" and type ");
                }
                print_longident_idx(f, arena, path.txt);
                f.string(" = ");
                print_core_type(f, arena, typ);
            }
            f.string(")");
        }
        CoreTypeDesc::Ptyp_extension((name, payload)) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
    }
    // Note: attributes handled in print_core_type wrapper
}

// ============================================================================
// Labels and arguments
// ============================================================================

fn print_arg_label<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, label: &ArgLabel) {
    // In OCaml types, labeled arguments are printed as "label:type" without ~
    match label {
        ArgLabel::Nolabel => {}
        ArgLabel::Labelled(s) => {
            f.string(arena.get_string(s.txt));
            f.string(":");
        }
        ArgLabel::Optional(s) => {
            f.string("?");
            f.string(arena.get_string(s.txt));
            f.string(":");
        }
    }
}

/// Print arg label for expression context (uses ~)
fn print_arg_label_expr<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, label: &ArgLabel) {
    match label {
        ArgLabel::Nolabel => {}
        ArgLabel::Labelled(s) => {
            f.string("~");
            f.string(arena.get_string(s.txt));
            f.string(":");
        }
        ArgLabel::Optional(s) => {
            f.string("?");
            f.string(arena.get_string(s.txt));
            f.string(":");
        }
    }
}

fn print_arg_with_label<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, label: &ArgLabel, arg: &Expression) {
    match label {
        ArgLabel::Labelled(name) => {
            let name_str = arena.get_string(name.txt);
            if let ExpressionDesc::Pexp_ident(lid) = &arg.pexp_desc {
                if let Longident::Lident(arg_name_idx) = arena.get_longident(lid.txt) {
                    let arg_name = arena.get_string(*arg_name_idx);
                    if arg_name == name_str && arg.pexp_attributes.is_empty() {
                        f.string("~");
                        f.string(name_str);
                        return;
                    }
                }
            }
            f.string("~");
            f.string(name_str);
            f.string(":");
            // OCaml uses simple_expr which wraps non-simple exprs in parens
            print_expression_simple(f, arena, arg);
        }
        ArgLabel::Optional(name) => {
            let name_str = arena.get_string(name.txt);
            if let ExpressionDesc::Pexp_ident(lid) = &arg.pexp_desc {
                if let Longident::Lident(arg_name_idx) = arena.get_longident(lid.txt) {
                    let arg_name = arena.get_string(*arg_name_idx);
                    if arg_name == name_str && arg.pexp_attributes.is_empty() {
                        f.string("?");
                        f.string(name_str);
                        return;
                    }
                }
            }
            f.string("?");
            f.string(name_str);
            f.string(":");
            // OCaml uses simple_expr which wraps non-simple exprs in parens
            print_expression_simple(f, arena, arg);
        }
        ArgLabel::Nolabel => {
            // OCaml uses simple_expr which wraps non-simple exprs in parens
            print_expression_simple(f, arena, arg);
        }
    }
}

// ============================================================================
// Constants
// ============================================================================

fn print_constant<W: Write>(f: &mut Formatter<W>, c: &Constant) {
    match c {
        Constant::Integer(s, suffix) => {
            f.string(s);
            if let Some(c) = suffix {
                f.string(&c.to_string());
            }
        }
        Constant::Char(i) => {
            let escaped = match *i {
                0x5C => "\\\\".to_string(),
                0x27 => "\\'".to_string(),
                0x0A => "\\n".to_string(),
                0x09 => "\\t".to_string(),
                0x08 => "\\b".to_string(),
                0x0D => "\\r".to_string(),
                n if (0x20..=0x7E).contains(&n) => {
                    if let Some(c) = char::from_u32(n as u32) {
                        c.to_string()
                    } else {
                        format!("\\{}", n)
                    }
                }
                n => format!("\\{}", n),
            };
            f.string("'");
            f.string(&escaped);
            f.string("'");
        }
        Constant::String(s, delim) => {
            // Match OCaml's pprintast.ml:
            // - None -> %S (quoted string with escaping)
            // - Some(delim) -> {delim|...|delim} (delimited string)
            if let Some(d) = delim {
                f.string("{");
                f.string(d);
                f.string("|");
                f.string(s);
                f.string("|");
                f.string(d);
                f.string("}");
            } else {
                f.string("\"");
                f.string(&escape_string(s));
                f.string("\"");
            }
        }
        Constant::Float(s, suffix) => {
            f.string(s);
            if let Some(c) = suffix {
                f.string(&c.to_string());
            }
        }
    }
}

// ============================================================================
// Longident
// ============================================================================

fn print_longident<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, lid: &Longident) {
    match lid {
        Longident::Lident(name_idx) => {
            let name = arena.get_string(*name_idx);
            f.string(name);
        }
        Longident::Ldot(prefix, name_idx) => {
            print_longident(f, arena, prefix);
            f.string(".");
            let name = arena.get_string(*name_idx);
            f.string(name);
        }
        Longident::Lapply(m1, m2) => {
            print_longident(f, arena, m1);
            f.string("(");
            print_longident(f, arena, m2);
            f.string(")");
        }
    }
}

/// Print longident from index using arena
fn print_longident_idx<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, lid_idx: LidentIdx) {
    print_longident(f, arena, arena.get_longident(lid_idx));
}

// ============================================================================
// Payload
// ============================================================================

fn print_payload<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, payload: &Payload) {
    match payload {
        Payload::PStr(items) => {
            // Special case: single Pstr_eval prints just the expression
            if items.len() == 1 {
                if let StructureItemDesc::Pstr_eval(expr, attrs) = &items[0].pstr_desc {
                    print_expression(f, arena, expr);
                    print_item_attributes(f, arena, attrs);
                    return;
                }
            }
            for (i, item) in items.iter().enumerate() {
                print_structure_item(f, arena, item);
                if i < items.len() - 1 {
                    f.string("\n  ");
                }
            }
        }
        Payload::PSig(sig_items) => {
            // Print signature items with : prefix and newlines between items
            f.string(":");
            for (i, item) in sig_items.iter().enumerate() {
                print_signature_item(f, arena, item);
                if i < sig_items.len() - 1 {
                    f.string("\n    ");
                }
            }
        }
        Payload::PTyp(typ) => {
            f.string(":");
            print_core_type(f, arena, typ);
        }
        Payload::PPat(pat, guard) => {
            f.string("?");
            print_pattern(f, arena, pat);
            if let Some(g) = guard {
                f.string(" when ");
                print_expression(f, arena, g);
            }
        }
    }
}

// ============================================================================
// Type declarations
// ============================================================================

fn print_type_declaration<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, decl: &TypeDeclaration) {
    if !decl.ptype_params.is_empty() {
        if decl.ptype_params.len() == 1 {
            // Print variance prefix
            match decl.ptype_params[0].1 {
                Variance::Covariant => f.string("+"),
                Variance::Contravariant => f.string("-"),
                Variance::Invariant => {}
            }
            print_core_type(f, arena, &decl.ptype_params[0].0);
            f.string(" ");
        } else {
            f.string("(");
            for (i, (t, variance)) in decl.ptype_params.iter().enumerate() {
                if i > 0 {
                    f.string(", ");
                }
                // Print variance prefix
                match variance {
                    Variance::Covariant => f.string("+"),
                    Variance::Contravariant => f.string("-"),
                    Variance::Invariant => {}
                }
                print_core_type(f, arena, t);
            }
            f.string(") ");
        }
    }
    f.string(&decl.ptype_name.txt);

    if let Some(manifest) = &decl.ptype_manifest {
        f.string(" = ");
        if matches!(decl.ptype_private, PrivateFlag::Private) {
            f.string("private ");
        }
        print_core_type(f, arena, manifest);
    }

    match &decl.ptype_kind {
        TypeKind::Ptype_abstract => {}
        TypeKind::Ptype_variant(ctors) => {
            f.string(" =");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                f.string(" private");
            }
            // OCaml uses: pp f "%t%t@\n%a" intro priv (list ~sep:"@\n" constructor_declaration) xs
            // Where constructor_declaration is: pp f "|@;"; ...
            // This means each constructor gets @\n (force newline with indent) + |@; (break hint)
            for ctor in ctors.iter() {
                // Each constructor starts on a new line with | prefix
                f.newline();  // Force newline with current indentation
                f.string("| ");
                f.string(&ctor.pcd_name.txt);
                match &ctor.pcd_args {
                    ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                        f.string(" of ");
                        for (j, arg) in args.iter().enumerate() {
                            if j > 0 {
                                f.string(" * ");
                            }
                            print_core_type(f, arena, arg);
                        }
                    }
                    ConstructorArguments::Pcstr_record(fields) => {
                        f.string(" of {\n  ");
                        let mut last_field_has_attrs = false;
                        for (j, field) in fields.iter().enumerate() {
                            if j > 0 {
                                f.string(";\n  ");
                            }
                            if matches!(field.pld_mutable, MutableFlag::Mutable) {
                                f.string("mutable ");
                            }
                            f.string(&field.pld_name.txt);
                            if field.pld_optional {
                                f.string("?");
                            }
                            f.string(": ");
                            print_core_type(f, arena, &field.pld_type);
                            last_field_has_attrs = !field.pld_attributes.is_empty();
                            if last_field_has_attrs {
                                f.string(" ");
                                print_attributes(f, arena, &field.pld_attributes);
                            }
                        }
                        // If last field has attributes, no space before }
                        if last_field_has_attrs {
                            f.string("}");
                        } else {
                            f.string(" }");
                        }
                    }
                    _ => {}
                }
                if let Some(res) = &ctor.pcd_res {
                    f.string(": ");
                    print_core_type(f, arena, res);
                }
                // Print constructor attributes (like @as) with leading space
                // OCaml: pp f "%s%a@;%a" name args (attributes ctxt) attrs
                // The @; before attributes is a break hint that becomes a space
                // When attrs is empty, still outputs a trailing space
                f.string(" ");
                print_attributes(f, arena, &ctor.pcd_attributes);
            }
        }
        TypeKind::Ptype_record(fields) => {
            f.string(" =");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                f.string(" private");
            }
            if decl.ptype_params.len() > 1 {
                f.string("\n  {\n");
            } else {
                f.string(" {\n");
            }
            for (i, field) in fields.iter().enumerate() {
                f.string("  ");
                if matches!(field.pld_mutable, MutableFlag::Mutable) {
                    f.string("mutable ");
                }
                f.string(&field.pld_name.txt);
                if field.pld_optional {
                    f.string("?");
                }
                f.string(": ");
                print_core_type(f, arena, &field.pld_type);
                let has_attrs = !field.pld_attributes.is_empty();
                if has_attrs {
                    f.string(" ");
                    print_attributes(f, arena, &field.pld_attributes);
                }
                if i < fields.len() - 1 {
                    if has_attrs {
                        f.string(";\n");
                    } else {
                        f.string(" ;\n");
                    }
                } else {
                    // If last field has attributes, no space before }
                    // Otherwise, space before }
                    if has_attrs {
                        f.string("}");
                    } else {
                        f.string(" }");
                    }
                }
            }
        }
        TypeKind::Ptype_open => {
            f.string(" =");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                f.string(" private");
            }
            f.string(" ..");
        }
    }

    // Type constraints
    for (ct1, ct2, _) in &decl.ptype_cstrs {
        f.string(" constraint ");
        print_core_type(f, arena, ct1);
        f.string(" = ");
        print_core_type(f, arena, ct2);
    }

    // Type attributes
    print_item_attributes(f, arena, &decl.ptype_attributes);
}

fn print_extension_constructor<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, ext: &ExtensionConstructor) {
    f.string(&ext.pext_name.txt);
    match &ext.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            match args {
                ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                    f.string(" of ");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            f.string(" * ");
                        }
                        print_core_type(f, arena, arg);
                    }
                    f.string(" ");
                }
                ConstructorArguments::Pcstr_record(fields) => {
                    // OCaml: pp f "@;of@;%a" (record_declaration ctxt) l
                    // where record_declaration is: pp f "{@\n%a}" (list ... ~sep:";@\n") fields
                    // Fields print at current indentation level (not extra indented)
                    f.string(" of {");
                    f.newline();
                    let mut last_field_has_attrs = false;
                    for (i, field) in fields.iter().enumerate() {
                        if matches!(field.pld_mutable, MutableFlag::Mutable) {
                            f.string("mutable ");
                        }
                        f.string(&field.pld_name.txt);
                        if field.pld_optional {
                            f.string("?");
                        }
                        f.string(": ");
                        print_core_type(f, arena, &field.pld_type);
                        last_field_has_attrs = !field.pld_attributes.is_empty();
                        if last_field_has_attrs {
                            f.string(" ");
                            print_attributes(f, arena, &field.pld_attributes);
                        }
                        if i < fields.len() - 1 {
                            f.string(" ;");
                            f.newline();
                        } else {
                            // If last field has attributes, no space before }
                            if last_field_has_attrs {
                                f.string("} ");
                            } else {
                                f.string(" } ");
                            }
                        }
                    }
                }
                _ => {
                    // No args - add trailing space
                    f.string(" ");
                }
            }
            if let Some(r) = res {
                f.string(": ");
                print_core_type(f, arena, r);
                f.string(" ");
            }
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            // Print attributes before rebind
            print_attributes(f, arena, &ext.pext_attributes);
            f.string(" = ");
            print_longident_idx(f, arena, lid.txt);
            return; // Don't print attributes twice
        }
    }
    // Print attributes
    print_attributes(f, arena, &ext.pext_attributes);
}

// ============================================================================
// Modules
// ============================================================================

fn print_module_expr<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, mexpr: &ModuleExpr) {
    // Print attributes on module expressions: ((mexpr)[@attr ])
    let has_attrs = printable_attributes(&mexpr.pmod_attributes).len() > 0;
    if has_attrs {
        f.string("((");
    }
    print_module_expr_inner(f, arena, mexpr);
    if has_attrs {
        f.string(")");
        print_attributes(f, arena, &mexpr.pmod_attributes);
        f.string(")");
    }
}

fn print_module_expr_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, mexpr: &ModuleExpr) {
    match &mexpr.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => {
            print_longident_idx(f, arena, lid.txt);
        }
        ModuleExprDesc::Pmod_structure(items) => {
            // OCaml: pp f "@[<hv2>struct@;@[<0>%a@]@;<1 -2>end@]" with items separated by @\n
            f.open_box(BoxKind::HV, 2);
            f.string("struct");
            f.space();
            f.open_box(BoxKind::HV, 0);
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    f.newline();
                }
                print_structure_item(f, arena, item);
            }
            f.close_box();
            f.break_(1, -2);
            f.string("end");
            f.close_box();
        }
        ModuleExprDesc::Pmod_functor(name, mtype, body) => {
            // OCaml: functor () -> ... for unit, functor (Name : Type) -> ... otherwise
            if let Some(mt) = mtype {
                f.string("functor (");
                f.string(&name.txt);
                f.string(" : ");
                print_module_type(f, arena, mt);
                f.string(") -> ");
            } else {
                f.string("functor () -> ");
            }
            print_module_expr(f, arena, body);
        }
        ModuleExprDesc::Pmod_apply(m1, m2) => {
            // OCaml: pp f "(%a)(%a)" - both functor and argument get parens
            f.string("(");
            print_module_expr(f, arena, m1);
            f.string(")(");
            print_module_expr(f, arena, m2);
            f.string(")");
        }
        ModuleExprDesc::Pmod_constraint(m, mt) => {
            f.string("(");
            print_module_expr(f, arena, m);
            f.string(" : ");
            print_module_type(f, arena, mt);
            f.string(")");
        }
        ModuleExprDesc::Pmod_unpack(e) => {
            f.string("(val ");
            print_expression(f, arena, e);
            f.string(")");
        }
        ModuleExprDesc::Pmod_extension((name, payload)) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
    }
}

fn print_module_type<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, mtype: &ModuleType) {
    // Print attributes on module types: ((mtype)[@attr ])
    let has_attrs = printable_attributes(&mtype.pmty_attributes).len() > 0;
    if has_attrs {
        f.string("((");
    }
    print_module_type_inner(f, arena, mtype);
    if has_attrs {
        f.string(")");
        print_attributes(f, arena, &mtype.pmty_attributes);
        f.string(")");
    }
}

fn print_module_type_inner<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, mtype: &ModuleType) {
    match &mtype.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => {
            print_longident_idx(f, arena, lid.txt);
        }
        ModuleTypeDesc::Pmty_signature(items) => {
            // OCaml: pp f "@[<hv0>@[<hv2>sig@ %a@]@ end@]" with items separated by @\n
            // In HV box mode, break hints become either all spaces or all newlines
            // based on whether the content fits on one line
            f.open_box(BoxKind::HV, 0);
            f.open_box(BoxKind::HV, 2);
            f.string("sig");
            f.space();  // break hint after sig
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    f.space();  // break hint between items (becomes newline if broken)
                }
                print_signature_item(f, arena, item);
            }
            f.close_box();
            f.space();  // break hint before end
            f.string("end");
            f.close_box();
        }
        ModuleTypeDesc::Pmty_functor(name, arg_type, ret_type) => {
            // OCaml handling:
            // - Pmty_functor (_, None, mt2) -> "functor () -> mt2"
            // - name = "_" -> anonymous, print as "ArgType -> RetType"
            // - otherwise -> "functor (Name : ArgType) -> RetType"
            if arg_type.is_none() {
                // Empty functor: functor () -> ...
                f.string("functor () -> ");
                print_module_type(f, arena, ret_type);
            } else if name.txt == "_" {
                // Anonymous functor: ArgType -> RetType
                if let Some(mt) = arg_type {
                    print_module_type(f, arena, mt);
                }
                f.string(" -> ");
                print_module_type(f, arena, ret_type);
            } else {
                // Named functor: functor (Name : ArgType) -> RetType
                f.string("functor (");
                f.string(&name.txt);
                if let Some(mt) = arg_type {
                    f.string(" : ");
                    print_module_type(f, arena, mt);
                }
                f.string(") -> ");
                print_module_type(f, arena, ret_type);
            }
        }
        ModuleTypeDesc::Pmty_with(mt, constraints) => {
            // OCaml: pp f "@[<hov2>(%a@ with@ %a)@]" when there are constraints
            // or pp f "@[<hov2>%a@]" when empty
            if constraints.is_empty() {
                f.open_box(BoxKind::HOV, 2);
                print_module_type(f, arena, mt);
                f.close_box();
            } else {
                f.open_box(BoxKind::HOV, 2);
                f.string("(");
                print_module_type(f, arena, mt);
                f.space();
                f.string("with");
                for (i, constraint) in constraints.iter().enumerate() {
                    if i > 0 {
                        f.space();
                        f.string("and");
                    }
                    f.space();
                    match constraint {
                        WithConstraint::Pwith_type(lid, decl) => {
                            // OCaml: pp f "type@ %a %a =@ %a" params lid type_decl
                            f.string("type");
                            f.space();  // @ after type
                            // Print type params
                            if !decl.ptype_params.is_empty() {
                                if decl.ptype_params.len() == 1 {
                                    print_core_type(f, arena, &decl.ptype_params[0].0);
                                    f.string(" ");
                                } else {
                                    f.string("(");
                                    for (i, (t, _)) in decl.ptype_params.iter().enumerate() {
                                        if i > 0 {
                                            f.string(",");
                                        }
                                        print_core_type(f, arena, t);
                                    }
                                    f.string(") ");
                                }
                            } else {
                                f.string(" ");  // space before lid when params empty
                            }
                            print_longident_idx(f, arena, lid.txt);
                            f.string(" =");
                            f.space();  // @ after =
                            // type_declaration prints @; then private then manifest then constraints
                            f.space();  // @; in type_declaration
                            if decl.ptype_private == PrivateFlag::Private {
                                f.string("private ");
                            }
                            if let Some(manifest) = &decl.ptype_manifest {
                                print_core_type(f, arena, manifest);
                            }
                            // Print type constraints
                            for (ct1, ct2, _) in &decl.ptype_cstrs {
                                f.string(" constraint ");
                                print_core_type(f, arena, ct1);
                                f.string(" = ");
                                print_core_type(f, arena, ct2);
                            }
                        }
                        WithConstraint::Pwith_module(lid1, lid2) => {
                            f.string("module ");
                            print_longident_idx(f, arena, lid1.txt);
                            f.string(" = ");
                            print_longident_idx(f, arena, lid2.txt);
                        }
                        WithConstraint::Pwith_typesubst(lid, decl) => {
                            // Same pattern as Pwith_type but with :=
                            f.string("type");
                            f.space();
                            // Print type params
                            if !decl.ptype_params.is_empty() {
                                if decl.ptype_params.len() == 1 {
                                    print_core_type(f, arena, &decl.ptype_params[0].0);
                                    f.string(" ");
                                } else {
                                    f.string("(");
                                    for (i, (t, _)) in decl.ptype_params.iter().enumerate() {
                                        if i > 0 {
                                            f.string(",");
                                        }
                                        print_core_type(f, arena, t);
                                    }
                                    f.string(") ");
                                }
                            } else {
                                f.string(" ");
                            }
                            print_longident_idx(f, arena, lid.txt);
                            f.string(" :=");
                            f.space();
                            f.space();
                            if decl.ptype_private == PrivateFlag::Private {
                                f.string("private ");
                            }
                            if let Some(manifest) = &decl.ptype_manifest {
                                print_core_type(f, arena, manifest);
                            }
                            // Print type constraints
                            for (ct1, ct2, _) in &decl.ptype_cstrs {
                                f.string(" constraint ");
                                print_core_type(f, arena, ct1);
                                f.string(" = ");
                                print_core_type(f, arena, ct2);
                            }
                        }
                        WithConstraint::Pwith_modsubst(lid1, lid2) => {
                            f.string("module ");
                            print_longident_idx(f, arena, lid1.txt);
                            f.string(" := ");
                            print_longident_idx(f, arena, lid2.txt);
                        }
                    }
                }
                f.string(")");
                f.close_box();
            }
        }
        ModuleTypeDesc::Pmty_typeof(mexpr) => {
            f.string("module type of ");
            print_module_expr(f, arena, mexpr);
        }
        ModuleTypeDesc::Pmty_extension((name, payload)) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            f.close_box();
        }
        ModuleTypeDesc::Pmty_alias(lid) => {
            f.string("(module ");
            print_longident_idx(f, arena, lid.txt);
            f.string(")");
        }
    }
}

// ============================================================================
// Signature items
// ============================================================================

fn print_signature_item<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, item: &SignatureItem) {
    match &item.psig_desc {
        SignatureItemDesc::Psig_value(vd) => {
            // OCaml: pp f "@[<2>%s@ %a@ :@ %a@]%a"
            f.open_box(BoxKind::HOV, 2);
            if vd.pval_prim.is_empty() {
                f.string("val");
            } else {
                f.string("external");
            }
            f.space();
            f.string(&vd.pval_name.txt);
            f.space();
            f.string(":");
            f.space();
            print_core_type(f, arena, &vd.pval_type);
            for prim in &vd.pval_prim {
                f.string(" = \"");
                f.string(prim);
                f.string("\"");
            }
            f.close_box();
            print_item_attributes(f, arena, &vd.pval_attributes);
        }
        SignatureItemDesc::Psig_type(rec_flag, decls) => {
            // OCaml: @[<2>type %a%a%s%s%a@]%a for first, @[<2>and %a@]%a for others
            // Each declaration is wrapped in a box with indent 2
            let rec_str = match rec_flag {
                RecFlag::Recursive => "",
                RecFlag::Nonrecursive => " nonrec",
            };
            for (i, decl) in decls.iter().enumerate() {
                if i > 0 {
                    f.newline();
                }
                // Open box with indent 2 for this declaration
                f.open_box(BoxKind::HOV, 2);
                if i == 0 {
                    f.string("type");
                    f.string(rec_str);
                    f.string(" ");
                } else {
                    f.string("and ");
                }
                print_type_declaration(f, arena, decl);
                f.close_box();
                // NOTE: print_type_declaration already prints ptype_attributes
            }
        }
        SignatureItemDesc::Psig_typext(ext) => {
            // Same format as Pstr_typext
            f.open_box(BoxKind::HV, 2);
            f.string("type ");
            // Print type params if present
            if !ext.ptyext_params.is_empty() {
                f.string("(");
                for (i, (ty, _variance)) in ext.ptyext_params.iter().enumerate() {
                    if i > 0 {
                        f.string(",");
                    }
                    print_core_type(f, arena, ty);
                }
                f.string(") ");
            }
            print_longident_idx(f, arena, ext.ptyext_path.txt);
            f.string(" += ");
            if matches!(ext.ptyext_private, PrivateFlag::Private) {
                f.string("private ");
            }
            // Extra space before line break to match OCaml's "+=  \n"
            f.string(" ");
            for ctor in ext.ptyext_constructors.iter() {
                f.newline();
                f.string("| ");
                print_extension_constructor(f, arena, ctor);
            }
            f.close_box();
            print_item_attributes(f, arena, &ext.ptyext_attributes);
        }
        SignatureItemDesc::Psig_exception(ext) => {
            // OCaml: pp f "@[<hov2>exception@ %a@]"
            // The HOV2 box means inline records get +2 indentation
            f.open_box(BoxKind::HOV, 2);
            f.string("exception ");
            print_extension_constructor(f, arena, ext);
            f.close_box();
        }
        SignatureItemDesc::Psig_module(md) => {
            // OCaml has special case: if pmd_type is Pmty_alias with no attrs, use = syntax
            f.string("module ");
            f.string(&md.pmd_name.txt);
            if let ModuleTypeDesc::Pmty_alias(lid) = &md.pmd_type.pmty_desc {
                if md.pmd_type.pmty_attributes.is_empty() {
                    // module Name = Alias
                    f.string(" = ");
                    print_longident_idx(f, arena, lid.txt);
                    print_item_attributes(f, arena, &md.pmd_attributes);
                    return;
                }
            }
            // module Name : ModuleType
            f.string(" : ");
            print_module_type(f, arena, &md.pmd_type);
            print_item_attributes(f, arena, &md.pmd_attributes);
        }
        SignatureItemDesc::Psig_recmodule(mds) => {
            // OCaml: First decl is @[<hov2>module@ rec@ %s:@ %a@]%a
            //        Others are @ @[<hov2>and@ %s:@ %a@]%a
            for (i, md) in mds.iter().enumerate() {
                if i > 0 {
                    // Break before "and" for subsequent declarations
                    f.break_(1, 0);
                }
                f.open_box(BoxKind::HOV, 2);
                if i == 0 {
                    f.string("module");
                    f.space();
                    f.string("rec");
                    f.space();
                } else {
                    f.string("and");
                    f.space();
                }
                f.string(&md.pmd_name.txt);
                f.string(":");
                f.space();
                print_module_type(f, arena, &md.pmd_type);
                f.close_box();
                print_item_attributes(f, arena, &md.pmd_attributes);
            }
        }
        SignatureItemDesc::Psig_modtype(mtd) => {
            // OCaml: pp f "@[<hov2>module@ type@ %s%a@]%a"
            // where inner is: pp_print_space f (); pp f "@ =@ %a" (module_type ctxt) mt
            // This produces two spaces before = when not breaking: "Sig  ="
            f.open_box(BoxKind::HOV, 2);
            f.string("module");
            f.space();
            f.string("type");
            f.space();
            f.string(&mtd.pmtd_name.txt);
            if let Some(mt) = &mtd.pmtd_type {
                // pp_print_space + "@ =" gives two spaces before =
                f.string(" ");  // literal space from pp_print_space
                f.space();      // break hint before =
                f.string("=");
                f.space();
                print_module_type(f, arena, mt);
            }
            f.close_box();
            print_item_attributes(f, arena, &mtd.pmtd_attributes);
        }
        SignatureItemDesc::Psig_open(od) => {
            f.string("open");
            if matches!(od.popen_override, OverrideFlag::Override) {
                f.string("!");
            }
            f.string(" ");
            print_longident_idx(f, arena, od.popen_lid.txt);
            print_item_attributes(f, arena, &od.popen_attributes);
        }
        SignatureItemDesc::Psig_include(incl) => {
            f.string("include ");
            print_module_type(f, arena, &incl.pincl_mod);
            print_item_attributes(f, arena, &incl.pincl_attributes);
        }
        SignatureItemDesc::Psig_attribute((name, payload)) => {
            // Standalone/floating attributes use [@@@...] (3 @)
            f.string("[@@@");
            f.string(&name.txt);
            if !payload_is_empty(payload) {
                f.string(" ");
                print_payload(f, arena, payload);
            } else {
                f.string(" ");
            }
            f.string("]");
        }
        SignatureItemDesc::Psig_extension((name, payload), attrs) => {
            f.open_box(BoxKind::H, 2);
            f.string("[%%");
            f.string(&name.txt);
            f.space();
            print_payload(f, arena, payload);
            f.string("]");
            print_item_attributes(f, arena, attrs);
            f.close_box();
        }
    }
}

fn print_item_attributes<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, attrs: &[(Located<String>, Payload)]) {
    let attrs = printable_attributes(attrs);
    for (name, payload) in attrs {
        f.string("[@@");
        f.string(&name.txt);
        if !payload_is_empty(payload) {
            f.string(" ");
            print_payload(f, arena, payload);
            f.string("]");
        } else {
            f.string(" ]");
        }
    }
}

/// Print attributes with single @ (for expressions, types, patterns)
fn print_attributes<W: Write>(f: &mut Formatter<W>, arena: &ParseArena, attrs: &[(Located<String>, Payload)]) {
    let attrs = printable_attributes(attrs);
    for (name, payload) in attrs {
        f.string("[@");
        f.string(&name.txt);
        if !payload_is_empty(payload) {
            f.string(" ");
            print_payload(f, arena, payload);
            f.string("]");
        } else {
            f.string(" ]");
        }
    }
}
