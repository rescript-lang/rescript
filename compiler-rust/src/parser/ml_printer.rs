//! ML Printer (OCaml-compatible output)
//!
//! This module provides printing functions that output AST in a format
//! compatible with OCaml's Pprintast module. Used for testing and debugging.

use std::io::Write;

use super::ast::*;
use super::longident::Longident;

/// Print structure in ML format (like Pprintast in OCaml)
pub fn print_structure_ml(structure: &Structure, out: &mut impl Write) {
    for (i, item) in structure.iter().enumerate() {
        if i > 0 {
            let _ = out.write_all(b"\n");
        }
        print_structure_item_ml(item, out);
    }
    // No trailing newline - OCaml doesn't add one
}

/// Count the arity of nested Pexp_fun expressions
fn count_function_arity(expr: &Expression) -> usize {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun { rhs, .. } => 1 + count_function_arity(rhs),
        _ => 0,
    }
}

/// Check if pattern is a simple variable with given name
fn pattern_is_simple_var(pat: &Pattern, name: &str) -> bool {
    match &pat.ppat_desc {
        PatternDesc::Ppat_var(var) => var.txt == name,
        _ => false,
    }
}

/// Print a function parameter in inline format (for value binding)
fn print_inline_param_ml(
    label: &ArgLabel,
    default: &Option<Box<Expression>>,
    pat: &Pattern,
    out: &mut impl Write,
) {
    match label {
        ArgLabel::Nolabel => {
            print_pattern_ml(pat, out);
            let _ = write!(out, " ");
        }
        ArgLabel::Labelled(name) => {
            if pattern_is_simple_var(pat, name) {
                // ~name  (double space when pattern matches label)
                let _ = write!(out, "~{}  ", name);
            } else {
                // ~name:pattern
                let _ = write!(out, "~{}:", name);
                print_pattern_ml(pat, out);
                let _ = write!(out, "  ");
            }
        }
        ArgLabel::Optional(name) => {
            if let Some(def) = default {
                if pattern_is_simple_var(pat, name) {
                    // ?(name= default)  when pattern matches label
                    let _ = write!(out, "?({}= ", name);
                    print_expression_ml(def, out);
                    let _ = write!(out, ")  ");
                } else {
                    // ?name:(pattern = default)
                    let _ = write!(out, "?{}:(", name);
                    print_pattern_ml(pat, out);
                    let _ = write!(out, " = ");
                    print_expression_ml(def, out);
                    let _ = write!(out, ")  ");
                }
            } else if pattern_is_simple_var(pat, name) {
                // ?name  (double space when pattern matches label)
                let _ = write!(out, "?{}  ", name);
            } else {
                // ?name:pattern
                let _ = write!(out, "?{}:", name);
                print_pattern_ml(pat, out);
                let _ = write!(out, "  ");
            }
        }
    }
}

/// Print a value binding with inlined function parameters (if expr is a function)
/// Transforms: `let f = fun x -> fun y -> body` into `let f [arity:2]x y = body`
fn print_value_binding_ml(pat: &Pattern, expr: &Expression, out: &mut impl Write) {
    // If expression has attributes, don't inline
    if !expr.pexp_attributes.is_empty() {
        print_pattern_ml(pat, out);
        let _ = write!(out, " = ");
        print_expression_ml(expr, out);
        return;
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
            // Print the binding pattern
            print_pattern_ml(pat, out);
            let _ = write!(out, " ");

            // Print arity annotation if known
            if let Arity::Full(n) = arity {
                let _ = write!(out, "[arity:{}]", n);
            } else {
                // Compute arity by counting nested funs
                let computed_arity = count_function_arity(expr);
                if computed_arity > 0 {
                    let _ = write!(out, "[arity:{}]", computed_arity);
                }
            }

            // Print first parameter
            print_inline_param_ml(arg_label, default, lhs, out);

            // Print remaining parameters and get the body
            let body = print_remaining_inline_params_ml(rhs, out);

            // Print = body
            let _ = write!(out, "= ");
            print_expression_ml(body, out);
        }
        _ => {
            // Not a function, print normally
            print_pattern_ml(pat, out);
            let _ = write!(out, " = ");
            print_expression_ml(expr, out);
        }
    }
}

/// Print remaining inline parameters and return the body expression
fn print_remaining_inline_params_ml<'a>(
    expr: &'a Expression,
    out: &mut impl Write,
) -> &'a Expression {
    // Don't inline if this expression has attributes
    if !expr.pexp_attributes.is_empty() {
        return expr;
    }

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            ..
        } => {
            print_inline_param_ml(arg_label, default, lhs, out);
            print_remaining_inline_params_ml(rhs, out)
        }
        _ => expr,
    }
}

fn print_structure_item_ml(item: &StructureItem, out: &mut impl Write) {
    match &item.pstr_desc {
        StructureItemDesc::Pstr_eval(expr, _attrs) => {
            let _ = write!(out, ";;");
            print_expression_ml(expr, out);
        }
        StructureItemDesc::Pstr_value(rec_flag, bindings) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => " rec",
                RecFlag::Nonrecursive => "",
            };
            let _ = write!(out, "let{}", rec_str);
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " and");
                }
                let _ = write!(out, " ");
                print_value_binding_ml(&binding.pvb_pat, &binding.pvb_expr, out);
            }
        }
        StructureItemDesc::Pstr_primitive(vd) => {
            let _ = write!(out, "external {} : ", vd.pval_name.txt);
            print_core_type_ml(&vd.pval_type, out);
            for prim in &vd.pval_prim {
                let _ = write!(out, " = \"{}\"", prim);
            }
        }
        StructureItemDesc::Pstr_type(rec_flag, decls) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "",
                RecFlag::Nonrecursive => " nonrec",
            };
            for (i, decl) in decls.iter().enumerate() {
                if i == 0 {
                    let _ = write!(out, "type{} ", rec_str);
                } else {
                    let _ = write!(out, " and ");
                }
                print_type_declaration_ml(decl, out);
            }
        }
        StructureItemDesc::Pstr_typext(ext) => {
            let _ = write!(out, "type ");
            print_longident(&ext.ptyext_path.txt, out);
            let _ = write!(out, " += ");
            for (i, ctor) in ext.ptyext_constructors.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " | ");
                }
                print_extension_constructor_ml(ctor, out);
            }
        }
        StructureItemDesc::Pstr_exception(ext) => {
            let _ = write!(out, "exception ");
            print_extension_constructor_ml(ext, out);
        }
        StructureItemDesc::Pstr_module(mb) => {
            let _ = write!(out, "module {} = ", mb.pmb_name.txt);
            print_module_expr_ml(&mb.pmb_expr, out);
        }
        StructureItemDesc::Pstr_recmodule(mbs) => {
            for (i, mb) in mbs.iter().enumerate() {
                if i == 0 {
                    let _ = write!(out, "module rec {} = ", mb.pmb_name.txt);
                } else {
                    let _ = write!(out, " and {} = ", mb.pmb_name.txt);
                }
                print_module_expr_ml(&mb.pmb_expr, out);
            }
        }
        StructureItemDesc::Pstr_modtype(mtd) => {
            let _ = write!(out, "module type {}", mtd.pmtd_name.txt);
            if let Some(mt) = &mtd.pmtd_type {
                let _ = write!(out, " = ");
                print_module_type_ml(mt, out);
            }
        }
        StructureItemDesc::Pstr_open(od) => {
            let _ = write!(out, "open ");
            print_longident(&od.popen_lid.txt, out);
        }
        StructureItemDesc::Pstr_include(incl) => {
            let _ = write!(out, "include ");
            print_module_expr_ml(&incl.pincl_mod, out);
        }
        StructureItemDesc::Pstr_attribute((name, _payload)) => {
            let _ = write!(out, "[@@{}]", name.txt);
        }
        StructureItemDesc::Pstr_extension((name, _payload), _attrs) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
    }
}

/// Print a payload in ML format
fn print_payload_ml(payload: &Payload, out: &mut impl Write) {
    match payload {
        Payload::PStr(items) => {
            for item in items {
                print_structure_item_ml(item, out);
            }
        }
        Payload::PSig(_) => {
            let _ = write!(out, "...");
        }
        Payload::PTyp(typ) => {
            let _ = write!(out, ": ");
            print_core_type_ml(typ, out);
        }
        Payload::PPat(pat, guard) => {
            let _ = write!(out, "? ");
            print_pattern_ml(pat, out);
            if let Some(g) = guard {
                let _ = write!(out, " when ");
                print_expression_ml(g, out);
            }
        }
    }
}

/// Check if a payload is empty (empty PStr)
fn payload_is_empty(payload: &Payload) -> bool {
    matches!(payload, Payload::PStr(items) if items.is_empty())
}

/// Check if a string represents an infix operator
fn is_infix_operator(name: &str) -> bool {
    // Special infix operators (multi-char starting with special chars)
    const INFIX_SPECIALS: &[&str] =
        &[":=", "!=", "::", "mod", "land", "lor", "lxor", "lsl", "lsr", "asr", "or"];
    if INFIX_SPECIALS.contains(&name) {
        return true;
    }

    // Operators that start with these chars are infix
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

/// Get operator name if expression is an identifier that is an operator
fn get_operator_name(expr: &Expression) -> Option<&str> {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => match &lid.txt {
            Longident::Lident(name) => Some(name.as_str()),
            _ => None,
        },
        _ => None,
    }
}

fn print_expression_ml(expr: &Expression, out: &mut impl Write) {
    let has_attrs = !expr.pexp_attributes.is_empty();
    if has_attrs {
        let _ = write!(out, "(");
    }
    print_expression_ml_inner(expr, out);
    if has_attrs {
        for (name, payload) in &expr.pexp_attributes {
            let _ = write!(out, "[@{}", name.txt);
            if !payload_is_empty(payload) {
                let _ = write!(out, " ");
                print_payload_ml(payload, out);
            }
            let _ = write!(out, " ]");
        }
        let _ = write!(out, ")");
    }
}

fn print_expression_ml_inner(expr: &Expression, out: &mut impl Write) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            print_longident(&lid.txt, out);
        }
        ExpressionDesc::Pexp_constant(c) => {
            print_constant_ml(c, out);
        }
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => " rec",
                RecFlag::Nonrecursive => "",
            };
            let _ = write!(out, "(let{}", rec_str);
            for (i, binding) in bindings.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " and");
                }
                let _ = write!(out, " ");
                print_pattern_ml(&binding.pvb_pat, out);
                let _ = write!(out, " = ");
                print_expression_ml(&binding.pvb_expr, out);
            }
            let _ = write!(out, " in ");
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            ..
        } => {
            let _ = write!(out, "(fun ");
            print_arg_label_ml(arg_label, out);
            if let Some(def) = default {
                let _ = write!(out, "?(");
                print_pattern_ml(lhs, out);
                let _ = write!(out, " = ");
                print_expression_ml(def, out);
                let _ = write!(out, ")");
            } else {
                print_pattern_ml(lhs, out);
            }
            let _ = write!(out, " -> ");
            print_expression_ml(rhs, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            // Check if this is an infix or prefix operator application
            if let Some(op_name) = get_operator_name(funct) {
                if is_infix_operator(op_name) && args.len() == 2 {
                    // Binary infix: print as `lhs op rhs`
                    let (_, lhs) = &args[0];
                    let (_, rhs) = &args[1];
                    print_expression_ml(lhs, out);
                    let _ = write!(out, " {} ", op_name);
                    print_expression_ml(rhs, out);
                    return;
                } else if is_prefix_operator(op_name) && args.len() == 1 {
                    // Unary prefix: print as `(op arg)`
                    let (_, arg) = &args[0];
                    let _ = write!(out, "(");
                    let _ = write!(out, "{} ", op_name);
                    print_expression_ml(arg, out);
                    let _ = write!(out, ")");
                    return;
                }
            }
            // Default: prefix application
            let _ = write!(out, "(");
            print_expression_ml(funct, out);
            for (label, arg) in args {
                let _ = write!(out, " ");
                print_arg_label_ml(label, out);
                print_expression_ml(arg, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_match(scrutinee, cases) => {
            let _ = write!(out, "(match ");
            print_expression_ml(scrutinee, out);
            let _ = write!(out, " with");
            for case in cases {
                let _ = write!(out, " | ");
                print_pattern_ml(&case.pc_lhs, out);
                if let Some(guard) = &case.pc_guard {
                    let _ = write!(out, " when ");
                    print_expression_ml(guard, out);
                }
                let _ = write!(out, " -> ");
                print_expression_ml(&case.pc_rhs, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_try(body, cases) => {
            let _ = write!(out, "(try ");
            print_expression_ml(body, out);
            let _ = write!(out, " with");
            for case in cases {
                let _ = write!(out, " | ");
                print_pattern_ml(&case.pc_lhs, out);
                if let Some(guard) = &case.pc_guard {
                    let _ = write!(out, " when ");
                    print_expression_ml(guard, out);
                }
                let _ = write!(out, " -> ");
                print_expression_ml(&case.pc_rhs, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_tuple(exprs) => {
            let _ = write!(out, "(");
            for (i, e) in exprs.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                print_expression_ml(e, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_construct(lid, arg) => {
            print_longident(&lid.txt, out);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_expression_ml(a, out);
            }
        }
        ExpressionDesc::Pexp_variant(label, arg) => {
            let _ = write!(out, "`{}", label);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_expression_ml(a, out);
            }
        }
        ExpressionDesc::Pexp_record(fields, base) => {
            let _ = write!(out, "{{");
            if let Some(b) = base {
                print_expression_ml(b, out);
                let _ = write!(out, " with ");
            }
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                print_longident(&field.lid.txt, out);
                let _ = write!(out, " = ");
                print_expression_ml(&field.expr, out);
            }
            let _ = write!(out, "}}");
        }
        ExpressionDesc::Pexp_field(obj, field) => {
            print_expression_ml(obj, out);
            let _ = write!(out, ".");
            print_longident(&field.txt, out);
        }
        ExpressionDesc::Pexp_setfield(obj, field, value) => {
            print_expression_ml(obj, out);
            let _ = write!(out, ".");
            print_longident(&field.txt, out);
            let _ = write!(out, " <- ");
            print_expression_ml(value, out);
        }
        ExpressionDesc::Pexp_array(elems) => {
            let _ = write!(out, "[|");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                print_expression_ml(e, out);
            }
            let _ = write!(out, "|]");
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_expr, else_expr) => {
            let _ = write!(out, "(if ");
            print_expression_ml(cond, out);
            let _ = write!(out, " then ");
            print_expression_ml(then_expr, out);
            if let Some(e) = else_expr {
                let _ = write!(out, " else ");
                print_expression_ml(e, out);
            }
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            let _ = write!(out, "(");
            print_expression_ml(e1, out);
            let _ = write!(out, "; ");
            print_expression_ml(e2, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_while(cond, body) => {
            let _ = write!(out, "(while ");
            print_expression_ml(cond, out);
            let _ = write!(out, " do ");
            print_expression_ml(body, out);
            let _ = write!(out, " done)");
        }
        ExpressionDesc::Pexp_for(pat, start, end, dir, body) => {
            let _ = write!(out, "(for ");
            print_pattern_ml(pat, out);
            let _ = write!(out, " = ");
            print_expression_ml(start, out);
            let dir_str = match dir {
                DirectionFlag::Upto => " to ",
                DirectionFlag::Downto => " downto ",
            };
            let _ = write!(out, "{}", dir_str);
            print_expression_ml(end, out);
            let _ = write!(out, " do ");
            print_expression_ml(body, out);
            let _ = write!(out, " done)");
        }
        ExpressionDesc::Pexp_constraint(e, t) => {
            let _ = write!(out, "(");
            print_expression_ml(e, out);
            let _ = write!(out, " : ");
            print_core_type_ml(t, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_coerce(e, t1, t2) => {
            let _ = write!(out, "(");
            print_expression_ml(e, out);
            if let Some(t) = t1 {
                let _ = write!(out, " : ");
                print_core_type_ml(t, out);
            }
            let _ = write!(out, " :> ");
            print_core_type_ml(t2, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_send(e, meth) => {
            print_expression_ml(e, out);
            let _ = write!(out, "#{}", meth.txt);
        }
        ExpressionDesc::Pexp_letmodule(name, mexpr, body) => {
            let _ = write!(out, "(let module {} = ", name.txt);
            print_module_expr_ml(mexpr, out);
            let _ = write!(out, " in ");
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_letexception(ext, body) => {
            let _ = write!(out, "(let exception ");
            print_extension_constructor_ml(ext, out);
            let _ = write!(out, " in ");
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_assert(e) => {
            let _ = write!(out, "(assert ");
            print_expression_ml(e, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_newtype(name, body) => {
            let _ = write!(out, "(fun (type {}) -> ", name.txt);
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_pack(mexpr) => {
            let _ = write!(out, "(module ");
            print_module_expr_ml(mexpr, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_open(_override_flag, lid, body) => {
            let _ = write!(out, "(let open ");
            print_longident(&lid.txt, out);
            let _ = write!(out, " in ");
            print_expression_ml(body, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_extension((name, _payload)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
        ExpressionDesc::Pexp_await(e) => {
            let _ = write!(out, "(await ");
            print_expression_ml(e, out);
            let _ = write!(out, ")");
        }
        ExpressionDesc::Pexp_jsx_element(_jsx) => {
            let _ = write!(out, "<jsx>");
        }
    }
}

fn print_pattern_ml(pat: &Pattern, out: &mut impl Write) {
    match &pat.ppat_desc {
        PatternDesc::Ppat_any => {
            let _ = write!(out, "_");
        }
        PatternDesc::Ppat_var(name) => {
            let _ = write!(out, "{}", name.txt);
        }
        PatternDesc::Ppat_alias(p, name) => {
            let _ = write!(out, "(");
            print_pattern_ml(p, out);
            let _ = write!(out, " as {})", name.txt);
        }
        PatternDesc::Ppat_constant(c) => {
            print_constant_ml(c, out);
        }
        PatternDesc::Ppat_interval(c1, c2) => {
            print_constant_ml(c1, out);
            let _ = write!(out, " .. ");
            print_constant_ml(c2, out);
        }
        PatternDesc::Ppat_tuple(pats) => {
            let _ = write!(out, "(");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                print_pattern_ml(p, out);
            }
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_construct(lid, arg) => {
            print_longident(&lid.txt, out);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_pattern_ml(a, out);
            }
        }
        PatternDesc::Ppat_variant(label, arg) => {
            let _ = write!(out, "`{}", label);
            if let Some(a) = arg {
                let _ = write!(out, " ");
                print_pattern_ml(a, out);
            }
        }
        PatternDesc::Ppat_record(fields, closed) => {
            let _ = write!(out, "{{");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                print_longident(&field.lid.txt, out);
                let _ = write!(out, " = ");
                print_pattern_ml(&field.pat, out);
            }
            if matches!(closed, ClosedFlag::Open) {
                let _ = write!(out, "; _");
            }
            let _ = write!(out, "}}");
        }
        PatternDesc::Ppat_array(pats) => {
            let _ = write!(out, "[|");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                print_pattern_ml(p, out);
            }
            let _ = write!(out, "|]");
        }
        PatternDesc::Ppat_or(p1, p2) => {
            let _ = write!(out, "(");
            print_pattern_ml(p1, out);
            let _ = write!(out, " | ");
            print_pattern_ml(p2, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_constraint(p, t) => {
            let _ = write!(out, "(");
            print_pattern_ml(p, out);
            let _ = write!(out, " : ");
            print_core_type_ml(t, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_type(lid) => {
            let _ = write!(out, "#");
            print_longident(&lid.txt, out);
        }
        PatternDesc::Ppat_unpack(name) => {
            let _ = write!(out, "(module {})", name.txt);
        }
        PatternDesc::Ppat_exception(p) => {
            let _ = write!(out, "(exception ");
            print_pattern_ml(p, out);
            let _ = write!(out, ")");
        }
        PatternDesc::Ppat_extension((name, _)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
        PatternDesc::Ppat_open(lid, p) => {
            print_longident(&lid.txt, out);
            let _ = write!(out, ".(");
            print_pattern_ml(p, out);
            let _ = write!(out, ")");
        }
    }
}

fn print_core_type_ml(typ: &CoreType, out: &mut impl Write) {
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => {
            let _ = write!(out, "_");
        }
        CoreTypeDesc::Ptyp_var(name) => {
            let _ = write!(out, "'{}", name);
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, arity } => {
            print_arg_label_ml(&arg.lbl, out);
            print_core_type_ml(&arg.typ, out);
            let _ = write!(out, " -> ");
            print_core_type_ml(ret, out);
            // Print arity annotation if known
            if let Arity::Full(n) = arity {
                let _ = write!(out, " (a:{})", n);
            }
        }
        CoreTypeDesc::Ptyp_tuple(types) => {
            let _ = write!(out, "(");
            for (i, t) in types.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " * ");
                }
                print_core_type_ml(t, out);
            }
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_constr(lid, args) => {
            if !args.is_empty() {
                if args.len() == 1 {
                    print_core_type_ml(&args[0], out);
                    let _ = write!(out, " ");
                } else {
                    let _ = write!(out, "(");
                    for (i, t) in args.iter().enumerate() {
                        if i > 0 {
                            let _ = write!(out, ", ");
                        }
                        print_core_type_ml(t, out);
                    }
                    let _ = write!(out, ") ");
                }
            }
            print_longident(&lid.txt, out);
        }
        CoreTypeDesc::Ptyp_object(fields, closed) => {
            let _ = write!(out, "<");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                match field {
                    ObjectField::Otag(name, _attrs, typ) => {
                        let _ = write!(out, "{}: ", name.txt);
                        print_core_type_ml(typ, out);
                    }
                    ObjectField::Oinherit(typ) => {
                        print_core_type_ml(typ, out);
                    }
                }
            }
            if matches!(closed, ClosedFlag::Open) {
                let _ = write!(out, "; ..");
            }
            let _ = write!(out, ">");
        }
        CoreTypeDesc::Ptyp_alias(t, name) => {
            let _ = write!(out, "(");
            print_core_type_ml(t, out);
            let _ = write!(out, " as '{})", name);
        }
        CoreTypeDesc::Ptyp_variant(rows, closed, labels) => {
            let _ = write!(out, "[");
            match closed {
                ClosedFlag::Open => {
                    let _ = write!(out, "> ");
                }
                ClosedFlag::Closed => {
                    if labels.is_some() {
                        let _ = write!(out, "< ");
                    }
                }
            }
            for (i, row) in rows.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " | ");
                }
                match row {
                    RowField::Rtag(label, _attrs, empty, types) => {
                        let _ = write!(out, "`{}", label.txt);
                        if !*empty || !types.is_empty() {
                            let _ = write!(out, " of ");
                            for (j, t) in types.iter().enumerate() {
                                if j > 0 {
                                    let _ = write!(out, " & ");
                                }
                                print_core_type_ml(t, out);
                            }
                        }
                    }
                    RowField::Rinherit(typ) => {
                        print_core_type_ml(typ, out);
                    }
                }
            }
            if let Some(lbls) = labels {
                let _ = write!(out, " > ");
                for (i, lbl) in lbls.iter().enumerate() {
                    if i > 0 {
                        let _ = write!(out, " ");
                    }
                    let _ = write!(out, "`{}", lbl);
                }
            }
            let _ = write!(out, "]");
        }
        CoreTypeDesc::Ptyp_poly(vars, t) => {
            if !vars.is_empty() {
                for var in vars {
                    let _ = write!(out, "'{} ", var.txt);
                }
                let _ = write!(out, ". ");
            }
            print_core_type_ml(t, out);
        }
        CoreTypeDesc::Ptyp_package((lid, constraints)) => {
            let _ = write!(out, "(module ");
            print_longident(&lid.txt, out);
            for (path, typ) in constraints {
                let _ = write!(out, " with type ");
                print_longident(&path.txt, out);
                let _ = write!(out, " = ");
                print_core_type_ml(typ, out);
            }
            let _ = write!(out, ")");
        }
        CoreTypeDesc::Ptyp_extension((name, _)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
    }
}

fn print_arg_label_ml(label: &ArgLabel, out: &mut impl Write) {
    match label {
        ArgLabel::Nolabel => {}
        ArgLabel::Labelled(s) => {
            let _ = write!(out, "~{}:", s);
        }
        ArgLabel::Optional(s) => {
            let _ = write!(out, "?{}:", s);
        }
    }
}

fn print_constant_ml(c: &Constant, out: &mut impl Write) {
    match c {
        Constant::Integer(s, suffix) => {
            let _ = write!(out, "{}", s);
            if let Some(c) = suffix {
                let _ = write!(out, "{}", c);
            }
        }
        Constant::Char(i) => {
            if let Some(c) = char::from_u32(*i as u32) {
                let _ = write!(out, "'{}'", c);
            } else {
                let _ = write!(out, "'\\{}'", i);
            }
        }
        Constant::String(s, _) => {
            let _ = write!(out, "\"{}\"", escape_string(s));
        }
        Constant::Float(s, suffix) => {
            let _ = write!(out, "{}", s);
            if let Some(c) = suffix {
                let _ = write!(out, "{}", c);
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

fn print_longident(lid: &Longident, out: &mut impl Write) {
    match lid {
        Longident::Lident(name) => {
            let _ = write!(out, "{}", name);
        }
        Longident::Ldot(prefix, name) => {
            print_longident(prefix, out);
            let _ = write!(out, ".{}", name);
        }
        Longident::Lapply(m1, m2) => {
            print_longident(m1, out);
            let _ = write!(out, "(");
            print_longident(m2, out);
            let _ = write!(out, ")");
        }
    }
}

fn print_type_declaration_ml(decl: &TypeDeclaration, out: &mut impl Write) {
    // Print type parameters
    if !decl.ptype_params.is_empty() {
        if decl.ptype_params.len() == 1 {
            print_core_type_ml(&decl.ptype_params[0].0, out);
            let _ = write!(out, " ");
        } else {
            let _ = write!(out, "(");
            for (i, (t, _)) in decl.ptype_params.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ", ");
                }
                print_core_type_ml(t, out);
            }
            let _ = write!(out, ") ");
        }
    }
    let _ = write!(out, "{}", decl.ptype_name.txt);

    // Print manifest
    if let Some(manifest) = &decl.ptype_manifest {
        let _ = write!(out, " = ");
        if matches!(decl.ptype_private, PrivateFlag::Private) {
            let _ = write!(out, "private ");
        }
        print_core_type_ml(manifest, out);
    }

    // Print kind
    match &decl.ptype_kind {
        TypeKind::Ptype_abstract => {}
        TypeKind::Ptype_variant(ctors) => {
            let _ = write!(out, " = ");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                let _ = write!(out, "private ");
            }
            for (i, ctor) in ctors.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " | ");
                }
                let _ = write!(out, "{}", ctor.pcd_name.txt);
                match &ctor.pcd_args {
                    ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                        let _ = write!(out, " of ");
                        for (j, arg) in args.iter().enumerate() {
                            if j > 0 {
                                let _ = write!(out, " * ");
                            }
                            print_core_type_ml(arg, out);
                        }
                    }
                    ConstructorArguments::Pcstr_record(fields) => {
                        let _ = write!(out, " of {{");
                        for (j, field) in fields.iter().enumerate() {
                            if j > 0 {
                                let _ = write!(out, "; ");
                            }
                            if matches!(field.pld_mutable, MutableFlag::Mutable) {
                                let _ = write!(out, "mutable ");
                            }
                            let _ = write!(out, "{}: ", field.pld_name.txt);
                            print_core_type_ml(&field.pld_type, out);
                        }
                        let _ = write!(out, "}}");
                    }
                    _ => {}
                }
                if let Some(res) = &ctor.pcd_res {
                    let _ = write!(out, " : ");
                    print_core_type_ml(res, out);
                }
            }
        }
        TypeKind::Ptype_record(fields) => {
            let _ = write!(out, " =");
            if matches!(decl.ptype_private, PrivateFlag::Private) {
                let _ = write!(out, " private");
            }
            // OCaml breaks line before { when there are multiple type parameters
            if decl.ptype_params.len() > 1 {
                let _ = write!(out, "\n  {{\n");
            } else {
                let _ = write!(out, " {{\n");
            }
            for (i, field) in fields.iter().enumerate() {
                let _ = write!(out, "  "); // 2-space indent
                if matches!(field.pld_mutable, MutableFlag::Mutable) {
                    let _ = write!(out, "mutable ");
                }
                let _ = write!(out, "{}: ", field.pld_name.txt);
                print_core_type_ml(&field.pld_type, out);
                if i < fields.len() - 1 {
                    let _ = write!(out, " ;\n");
                } else {
                    let _ = write!(out, " }}");
                }
            }
        }
        TypeKind::Ptype_open => {
            let _ = write!(out, " = ..");
        }
    }
}

fn print_extension_constructor_ml(ext: &ExtensionConstructor, out: &mut impl Write) {
    let _ = write!(out, "{}", ext.pext_name.txt);
    match &ext.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            match args {
                ConstructorArguments::Pcstr_tuple(args) if !args.is_empty() => {
                    let _ = write!(out, " of ");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            let _ = write!(out, " * ");
                        }
                        print_core_type_ml(arg, out);
                    }
                }
                ConstructorArguments::Pcstr_record(fields) => {
                    let _ = write!(out, " of {{");
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            let _ = write!(out, "; ");
                        }
                        if matches!(field.pld_mutable, MutableFlag::Mutable) {
                            let _ = write!(out, "mutable ");
                        }
                        let _ = write!(out, "{}: ", field.pld_name.txt);
                        print_core_type_ml(&field.pld_type, out);
                    }
                    let _ = write!(out, "}}");
                }
                _ => {}
            }
            if let Some(r) = res {
                let _ = write!(out, " : ");
                print_core_type_ml(r, out);
            }
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            let _ = write!(out, " = ");
            print_longident(&lid.txt, out);
        }
    }
}

fn print_module_expr_ml(mexpr: &ModuleExpr, out: &mut impl Write) {
    match &mexpr.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => {
            print_longident(&lid.txt, out);
        }
        ModuleExprDesc::Pmod_structure(items) => {
            let _ = write!(out, "struct ");
            for item in items {
                print_structure_item_ml(item, out);
                let _ = write!(out, " ");
            }
            let _ = write!(out, "end");
        }
        ModuleExprDesc::Pmod_functor(name, mtype, body) => {
            let _ = write!(out, "functor (");
            let _ = write!(out, "{}", name.txt);
            if let Some(mt) = mtype {
                let _ = write!(out, " : ");
                print_module_type_ml(mt, out);
            }
            let _ = write!(out, ") -> ");
            print_module_expr_ml(body, out);
        }
        ModuleExprDesc::Pmod_apply(m1, m2) => {
            print_module_expr_ml(m1, out);
            let _ = write!(out, "(");
            print_module_expr_ml(m2, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_constraint(m, mt) => {
            let _ = write!(out, "(");
            print_module_expr_ml(m, out);
            let _ = write!(out, " : ");
            print_module_type_ml(mt, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_unpack(e) => {
            let _ = write!(out, "(val ");
            print_expression_ml(e, out);
            let _ = write!(out, ")");
        }
        ModuleExprDesc::Pmod_extension((name, _)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
    }
}

fn print_module_type_ml(mtype: &ModuleType, out: &mut impl Write) {
    match &mtype.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => {
            print_longident(&lid.txt, out);
        }
        ModuleTypeDesc::Pmty_signature(items) => {
            let _ = write!(out, "sig ");
            for item in items {
                print_signature_item_ml(item, out);
                let _ = write!(out, " ");
            }
            let _ = write!(out, "end");
        }
        ModuleTypeDesc::Pmty_functor(name, arg_type, ret_type) => {
            let _ = write!(out, "functor (");
            let _ = write!(out, "{}", name.txt);
            if let Some(mt) = arg_type {
                let _ = write!(out, " : ");
                print_module_type_ml(mt, out);
            }
            let _ = write!(out, ") -> ");
            print_module_type_ml(ret_type, out);
        }
        ModuleTypeDesc::Pmty_with(mt, constraints) => {
            print_module_type_ml(mt, out);
            for constraint in constraints {
                match constraint {
                    WithConstraint::Pwith_type(lid, decl) => {
                        let _ = write!(out, " with type ");
                        print_longident(&lid.txt, out);
                        let _ = write!(out, " = ");
                        if let Some(manifest) = &decl.ptype_manifest {
                            print_core_type_ml(manifest, out);
                        }
                    }
                    WithConstraint::Pwith_module(lid1, lid2) => {
                        let _ = write!(out, " with module ");
                        print_longident(&lid1.txt, out);
                        let _ = write!(out, " = ");
                        print_longident(&lid2.txt, out);
                    }
                    WithConstraint::Pwith_typesubst(lid, decl) => {
                        let _ = write!(out, " with type ");
                        print_longident(&lid.txt, out);
                        let _ = write!(out, " := ");
                        if let Some(manifest) = &decl.ptype_manifest {
                            print_core_type_ml(manifest, out);
                        }
                    }
                    WithConstraint::Pwith_modsubst(lid1, lid2) => {
                        let _ = write!(out, " with module ");
                        print_longident(&lid1.txt, out);
                        let _ = write!(out, " := ");
                        print_longident(&lid2.txt, out);
                    }
                }
            }
        }
        ModuleTypeDesc::Pmty_typeof(mexpr) => {
            let _ = write!(out, "module type of ");
            print_module_expr_ml(mexpr, out);
        }
        ModuleTypeDesc::Pmty_extension((name, _)) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
        ModuleTypeDesc::Pmty_alias(lid) => {
            let _ = write!(out, "(module ");
            print_longident(&lid.txt, out);
            let _ = write!(out, ")");
        }
    }
}

/// Print signature in ML format
pub fn print_signature_ml(signature: &[SignatureItem], out: &mut impl Write) {
    for (i, item) in signature.iter().enumerate() {
        if i > 0 {
            let _ = out.write_all(b" ");
        }
        print_signature_item_ml(item, out);
    }
    let _ = out.write_all(b"\n");
}

fn print_signature_item_ml(item: &SignatureItem, out: &mut impl Write) {
    match &item.psig_desc {
        SignatureItemDesc::Psig_value(vd) => {
            let _ = write!(out, "val {} : ", vd.pval_name.txt);
            print_core_type_ml(&vd.pval_type, out);
        }
        SignatureItemDesc::Psig_type(rec_flag, decls) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => "",
                RecFlag::Nonrecursive => " nonrec",
            };
            for (i, decl) in decls.iter().enumerate() {
                if i == 0 {
                    let _ = write!(out, "type{} ", rec_str);
                } else {
                    let _ = write!(out, " and ");
                }
                print_type_declaration_ml(decl, out);
            }
        }
        SignatureItemDesc::Psig_typext(ext) => {
            let _ = write!(out, "type ");
            print_longident(&ext.ptyext_path.txt, out);
            let _ = write!(out, " += ");
            for (i, ctor) in ext.ptyext_constructors.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, " | ");
                }
                print_extension_constructor_ml(ctor, out);
            }
        }
        SignatureItemDesc::Psig_exception(ext) => {
            let _ = write!(out, "exception ");
            print_extension_constructor_ml(ext, out);
        }
        SignatureItemDesc::Psig_module(md) => {
            let _ = write!(out, "module {} : ", md.pmd_name.txt);
            print_module_type_ml(&md.pmd_type, out);
        }
        SignatureItemDesc::Psig_recmodule(mds) => {
            for (i, md) in mds.iter().enumerate() {
                if i == 0 {
                    let _ = write!(out, "module rec {} : ", md.pmd_name.txt);
                } else {
                    let _ = write!(out, " and {} : ", md.pmd_name.txt);
                }
                print_module_type_ml(&md.pmd_type, out);
            }
        }
        SignatureItemDesc::Psig_modtype(mtd) => {
            let _ = write!(out, "module type {}", mtd.pmtd_name.txt);
            if let Some(mt) = &mtd.pmtd_type {
                let _ = write!(out, " = ");
                print_module_type_ml(mt, out);
            }
        }
        SignatureItemDesc::Psig_open(od) => {
            let _ = write!(out, "open ");
            print_longident(&od.popen_lid.txt, out);
        }
        SignatureItemDesc::Psig_include(incl) => {
            let _ = write!(out, "include ");
            print_module_type_ml(&incl.pincl_mod, out);
        }
        SignatureItemDesc::Psig_attribute((name, _)) => {
            let _ = write!(out, "[@@{}]", name.txt);
        }
        SignatureItemDesc::Psig_extension((name, _), _) => {
            let _ = write!(out, "[%%{}]", name.txt);
        }
    }
}
