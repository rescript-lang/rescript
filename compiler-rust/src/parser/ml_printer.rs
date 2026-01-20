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

/// Check if pattern is a simple variable with given name (and no attributes)
fn pattern_is_simple_var(pat: &Pattern, name: &str) -> bool {
    if !pat.ppat_attributes.is_empty() {
        return false; // Patterns with attributes can't use short form
    }
    match &pat.ppat_desc {
        PatternDesc::Ppat_var(var) => var.txt == name,
        _ => false,
    }
}

/// Check if pattern needs parens when used as a function parameter
/// Patterns like `Constructor` or `Constructor arg` need parens
fn pattern_needs_parens_as_param(pat: &Pattern) -> bool {
    match &pat.ppat_desc {
        // Construct patterns need parens: (None), (Some x)
        // But unit () and empty list [] don't need extra parens
        // Cons pattern :: already adds its own parens in print_pattern_ml
        PatternDesc::Ppat_construct(lid, _arg) => {
            match &lid.txt {
                Longident::Lident(name) if name == "()" || name == "[]" || name == "::" => false,
                _ => true, // All other constructors (with or without args) need parens
            }
        }
        // Variant with argument needs parens: (`Tag x)
        PatternDesc::Ppat_variant(_, Some(_)) => true,
        // Alias patterns need parens: (p as x)
        PatternDesc::Ppat_alias(_, _) => true,
        // Or patterns need parens: (A | B)
        PatternDesc::Ppat_or(_, _) => true,
        // Constraint patterns already add their own parens in print_pattern_ml
        // so don't add extra parens here
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
            // Wrap complex patterns in parens (like constructor patterns with args)
            if pattern_needs_parens_as_param(pat) {
                let _ = write!(out, "(");
                print_pattern_ml(pat, out);
                let _ = write!(out, ") ");
            } else {
                print_pattern_ml(pat, out);
                let _ = write!(out, " ");
            }
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
                    // ?name:(pattern= default)
                    let _ = write!(out, "?{}:(", name);
                    print_pattern_ml(pat, out);
                    let _ = write!(out, "= ");
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
            // Use no outer parens for simple applies
            print_expression_ml_no_outer_parens(body, out);
        }
        _ => {
            // Not a function, print normally
            print_pattern_ml(pat, out);
            let _ = write!(out, " = ");
            // Use no outer parens for simple applies
            print_expression_ml_no_outer_parens(expr, out);
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
            // Print without parens for top-level eval
            print_expression_ml_no_outer_parens(expr, out);
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

/// Check if expression needs parens in binary context (operators, function args, etc.)
fn needs_parens_in_binary_context(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ifthenelse(_, _, _)
        | ExpressionDesc::Pexp_let(_, _, _)
        | ExpressionDesc::Pexp_fun { .. }
        | ExpressionDesc::Pexp_match(_, _)
        | ExpressionDesc::Pexp_try(_, _)
        | ExpressionDesc::Pexp_sequence(_, _) => true,
        ExpressionDesc::Pexp_apply { args, .. } if !args.is_empty() => true,
        _ => false,
    }
}

/// Check if expression is an infix operator application
fn is_infix_application(expr: &Expression) -> bool {
    if let ExpressionDesc::Pexp_apply { funct, args, .. } = &expr.pexp_desc {
        if args.len() == 2 {
            if let Some(op_name) = get_operator_name(funct) {
                return is_infix_operator(op_name);
            }
        }
    }
    false
}

/// Check if expression is a prefix operator application (like `~~~ a`)
fn is_prefix_application(expr: &Expression) -> bool {
    if let ExpressionDesc::Pexp_apply { funct, args, .. } = &expr.pexp_desc {
        if args.len() == 1 {
            if let Some(op_name) = get_operator_name(funct) {
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
fn needs_parens_as_labeled_arg(expr: &Expression) -> bool {
    // Prefix applications (like `~~~ a`) already add their own parens in the printer,
    // so don't add extra outer parens for them
    if is_prefix_application(expr) {
        return false;
    }
    needs_parens_in_binary_context(expr) || is_infix_application(expr)
}

/// Check if expression needs parens when used as an unlabeled function argument
fn needs_parens_as_function_arg(expr: &Expression) -> bool {
    // Prefix applications (like `~~~ a`) already add their own parens in the printer,
    // so don't add extra outer parens for them
    if is_prefix_application(expr) {
        return false;
    }
    needs_parens_in_binary_context(expr) || application_has_labeled_args(expr)
}

/// Print expression with parens if needed as a function argument
fn print_expression_ml_parens_if_complex(expr: &Expression, out: &mut impl Write) {
    if needs_parens_as_function_arg(expr) {
        let _ = write!(out, "(");
        print_expression_ml_inner(expr, out, false);
        let _ = write!(out, ")");
    } else {
        print_expression_ml(expr, out);
    }
}

fn print_expression_ml(expr: &Expression, out: &mut impl Write) {
    let has_attrs = !expr.pexp_attributes.is_empty();
    if has_attrs {
        // OCaml format: ((expr)[@attr ])
        let _ = write!(out, "((");
        // Use use_parens = false inside the double parens to avoid triple nesting
        print_expression_ml_inner(expr, out, false);
        let _ = write!(out, ")");
        for (name, payload) in &expr.pexp_attributes {
            let _ = write!(out, "[@{}", name.txt);
            if !payload_is_empty(payload) {
                let _ = write!(out, " ");
                print_payload_ml(payload, out);
            }
            let _ = write!(out, " ]");
        }
        let _ = write!(out, ")");
    } else {
        // Don't add parens by default - let the specific expression types handle it
        print_expression_ml_inner(expr, out, false);
    }
}

/// Print expression without outer parentheses (for top-level eval statements)
fn print_expression_ml_no_outer_parens(expr: &Expression, out: &mut impl Write) {
    let has_attrs = !expr.pexp_attributes.is_empty();
    if has_attrs {
        let _ = write!(out, "(");
    }
    print_expression_ml_inner(expr, out, false);
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

/// Print fun body - strips the outer parens from nested funs
/// OCaml prints `fun [arity:2]acc -> fun curr -> body` not `fun acc -> (fun curr -> body)`
fn print_fun_body_ml(expr: &Expression, out: &mut impl Write) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            ..
        } if expr.pexp_attributes.is_empty() => {
            // Print nested fun without wrapping parens
            let _ = write!(out, "fun ");
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
            print_fun_body_ml(rhs, out);
        }
        _ => {
            // Not a nested fun, print without outer parens (infix ops don't need parens here)
            print_expression_ml_no_outer_parens(expr, out);
        }
    }
}

fn print_expression_ml_inner(expr: &Expression, out: &mut impl Write, use_parens: bool) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            print_longident(&lid.txt, out);
        }
        ExpressionDesc::Pexp_constant(c) => {
            // Wrap negative numbers in parens to match OCaml output
            let is_negative = match c {
                Constant::Integer(s, _) | Constant::Float(s, _) => s.starts_with('-'),
                _ => false,
            };
            if is_negative {
                let _ = write!(out, "(");
            }
            print_constant_ml(c, out);
            if is_negative {
                let _ = write!(out, ")");
            }
        }
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            let rec_str = match rec_flag {
                RecFlag::Recursive => " rec",
                RecFlag::Nonrecursive => "",
            };
            if use_parens {
                let _ = write!(out, "(");
            }
            let _ = write!(out, "let{}", rec_str);
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
            if use_parens {
                let _ = write!(out, ")");
            }
        }
        ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            arity,
            ..
        } => {
            // Print with arity annotation
            let arity_value = match arity {
                Arity::Full(n) => Some(*n),
                Arity::Unknown => {
                    // Compute arity by counting nested funs
                    let computed = count_function_arity(expr);
                    if computed > 0 {
                        Some(computed)
                    } else {
                        None
                    }
                }
            };
            if use_parens {
                let _ = write!(out, "(");
            }
            let _ = write!(out, "fun ");
            if let Some(n) = arity_value {
                let _ = write!(out, "[arity:{}]", n);
            }
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
            // Print nested funs without the outer parens
            print_fun_body_ml(rhs, out);
            if use_parens {
                let _ = write!(out, ")");
            }
        }
        ExpressionDesc::Pexp_apply { funct, args, partial, .. } => {
            // Check if this is an infix or prefix operator application
            if let Some(op_name) = get_operator_name(funct) {
                if is_infix_operator(op_name) && args.len() == 2 {
                    // Binary infix: print as `lhs op rhs`
                    // Add parens around complex expressions (if/let/fun/match/try/sequence)
                    let (_, lhs) = &args[0];
                    let (_, rhs) = &args[1];
                    // Use parens when we're inside something else (use_parens = true)
                    if use_parens {
                        let _ = write!(out, "(");
                    }
                    // Use print_expression_ml_parens_if_complex for operands
                    print_expression_ml_parens_if_complex(lhs, out);
                    let _ = write!(out, " {} ", op_name);
                    print_expression_ml_parens_if_complex(rhs, out);
                    // Print partial marker if applicable
                    if *partial {
                        let _ = write!(out, " ...");
                    }
                    if use_parens {
                        let _ = write!(out, ")");
                    }
                    return;
                } else if is_prefix_operator(op_name) && args.len() == 1 {
                    // Unary prefix: print as `(op arg)`
                    let (_, arg) = &args[0];
                    let _ = write!(out, "(");
                    let _ = write!(out, "{} ", op_name);
                    print_expression_ml_parens_if_complex(arg, out);
                    if *partial {
                        let _ = write!(out, " ...");
                    }
                    let _ = write!(out, ")");
                    return;
                }
            }
            // Default: prefix application
            // Only use parens if needed (determined by use_parens flag)
            if use_parens {
                let _ = write!(out, "(");
            }
            print_expression_ml(funct, out);
            for (label, arg) in args {
                let _ = write!(out, " ");
                // Check if labeled arg matches the form ~name where arg is just `name`
                print_arg_with_label_ml(label, arg, out);
            }
            // Print partial marker if applicable
            if *partial {
                let _ = write!(out, " ...");
            }
            if use_parens {
                let _ = write!(out, ")");
            }
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
                    let _ = write!(out, ";");
                }
                print_expression_ml(e, out);
            }
            let _ = write!(out, "|]");
        }
        ExpressionDesc::Pexp_ifthenelse(cond, then_expr, else_expr) => {
            if use_parens {
                let _ = write!(out, "(");
            }
            let _ = write!(out, "if ");
            print_expression_ml(cond, out);
            let _ = write!(out, " then ");
            print_expression_ml(then_expr, out);
            if let Some(e) = else_expr {
                let _ = write!(out, " else ");
                print_expression_ml(e, out);
            }
            if use_parens {
                let _ = write!(out, ")");
            }
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            if use_parens {
                let _ = write!(out, "(");
            }
            print_expression_ml(e1, out);
            let _ = write!(out, "; ");
            print_expression_ml(e2, out);
            if use_parens {
                let _ = write!(out, ")");
            }
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
    let has_attrs = !pat.ppat_attributes.is_empty();
    if has_attrs {
        let _ = write!(out, "((");
    }
    print_pattern_ml_inner(pat, out);
    if has_attrs {
        let _ = write!(out, ")");
        for (name, payload) in &pat.ppat_attributes {
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

fn print_pattern_ml_inner(pat: &Pattern, out: &mut impl Write) {
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
            // Special case for cons pattern: print as (x::xs) not (:: (x, xs))
            if let Longident::Lident(name) = &lid.txt {
                if name == "::" {
                    if let Some(a) = arg {
                        // The argument should be a tuple (head, tail)
                        if let PatternDesc::Ppat_tuple(elements) = &a.ppat_desc {
                            if elements.len() == 2 {
                                let _ = write!(out, "(");
                                print_pattern_ml(&elements[0], out);
                                let _ = write!(out, "::");
                                print_pattern_ml(&elements[1], out);
                                let _ = write!(out, ")");
                                return;
                            }
                        }
                    }
                }
            }
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
            let _ = write!(out, "{{ ");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, "; ");
                }
                // Check if we can use punning: { name } instead of { name = name }
                let field_name = match &field.lid.txt {
                    Longident::Lident(name) => Some(name.as_str()),
                    Longident::Ldot(_, name) => Some(name.as_str()),
                    _ => None,
                };
                let is_punned = if let Some(fname) = field_name {
                    matches!(&field.pat.ppat_desc, PatternDesc::Ppat_var(var) if var.txt == fname)
                } else {
                    false
                };

                if is_punned {
                    // Use punned form: just the field name
                    print_longident(&field.lid.txt, out);
                } else {
                    print_longident(&field.lid.txt, out);
                    let _ = write!(out, " = ");
                    print_pattern_ml(&field.pat, out);
                }
            }
            if matches!(closed, ClosedFlag::Open) {
                let _ = write!(out, "; _");
            }
            let _ = write!(out, " }}");
        }
        PatternDesc::Ppat_array(pats) => {
            let _ = write!(out, "[|");
            for (i, p) in pats.iter().enumerate() {
                if i > 0 {
                    let _ = write!(out, ";");
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
            let _ = write!(out, "exception ");
            print_pattern_ml(p, out);
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

/// Print a labeled argument, using short form ~name when value is just an identifier with same name
fn print_arg_with_label_ml(label: &ArgLabel, arg: &Expression, out: &mut impl Write) {
    // Check if we can use the short form: ~name instead of ~name:name
    match label {
        ArgLabel::Labelled(name) => {
            if let ExpressionDesc::Pexp_ident(lid) = &arg.pexp_desc {
                if let Longident::Lident(arg_name) = &lid.txt {
                    if arg_name == name && arg.pexp_attributes.is_empty() {
                        // Use short form: just ~name
                        let _ = write!(out, "~{}", name);
                        return;
                    }
                }
            }
            // Full form: ~name:value (with parens for complex expressions)
            let _ = write!(out, "~{}:", name);
            if needs_parens_as_labeled_arg(arg) {
                let _ = write!(out, "(");
                print_expression_ml(arg, out);
                let _ = write!(out, ")");
            } else {
                print_expression_ml(arg, out);
            }
        }
        ArgLabel::Optional(name) => {
            if let ExpressionDesc::Pexp_ident(lid) = &arg.pexp_desc {
                if let Longident::Lident(arg_name) = &lid.txt {
                    if arg_name == name && arg.pexp_attributes.is_empty() {
                        // Use short form: just ?name
                        let _ = write!(out, "?{}", name);
                        return;
                    }
                }
            }
            // Full form: ?name:value (with parens for complex expressions)
            let _ = write!(out, "?{}:", name);
            if needs_parens_as_labeled_arg(arg) {
                let _ = write!(out, "(");
                print_expression_ml(arg, out);
                let _ = write!(out, ")");
            } else {
                print_expression_ml(arg, out);
            }
        }
        ArgLabel::Nolabel => {
            // Use parens for complex expressions when used as function arguments
            print_expression_ml_parens_if_complex(arg, out);
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
            // Escape special characters in character literals
            // OCaml uses decimal escapes (\170 for char 170), not octal
            let escaped = match *i {
                0x5C => "\\\\".to_string(),      // backslash -> '\\'
                0x27 => "\\'".to_string(),       // single quote -> '\''
                0x0A => "\\n".to_string(),       // newline -> '\n'
                0x09 => "\\t".to_string(),       // tab -> '\t'
                0x08 => "\\b".to_string(),       // backspace -> '\b'
                0x0D => "\\r".to_string(),       // carriage return -> '\r'
                n if (0x20..=0x7E).contains(&n) => {
                    // Printable ASCII (excluding backslash and quote which are handled above)
                    if let Some(c) = char::from_u32(n as u32) {
                        c.to_string()
                    } else {
                        format!("\\{}", n) // Decimal escape
                    }
                }
                n => format!("\\{}", n),         // Decimal escape for other chars
            };
            let _ = write!(out, "'{}'", escaped);
        }
        Constant::String(s, delim) => {
            // Use the original delimiter if available (for JS strings, template literals, etc.)
            if let Some(d) = delim {
                if d.is_empty() {
                    // Empty delimiter means regular double-quoted string
                    let _ = write!(out, "\"{}\"", escape_string(s));
                } else {
                    // Use the original delimiter (e.g., "js", "j")
                    let _ = write!(out, "{{{}|{}|{}}}", d, s, d);
                }
            } else {
                // No delimiter, use js| format for plain strings
                let _ = write!(out, "{{js|{}|js}}", s);
            }
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
