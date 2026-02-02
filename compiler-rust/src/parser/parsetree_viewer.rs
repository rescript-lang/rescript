//! Parsetree viewer utilities.
//!
//! This module provides helper functions for analyzing and inspecting the AST.
//! It's a port of OCaml's `res_parsetree_viewer.ml`.

use crate::parse_arena::{LocIdx, ParseArena};
use crate::parser::ast::*;
use crate::parser::longident::Longident;

// ============================================================================
// Expression Analysis
// ============================================================================

/// Check if an expression is a "block" expression (let, sequence, etc.)
pub fn is_block_expr(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_letmodule(_, _, _)
            | ExpressionDesc::Pexp_letexception(_, _)
            | ExpressionDesc::Pexp_let(_, _, _)
            | ExpressionDesc::Pexp_open(_, _, _)
            | ExpressionDesc::Pexp_sequence(_, _)
    )
}

/// Check if an expression has braces attribute.
pub fn is_braced_expr(expr: &Expression) -> bool {
    process_braces_attr(expr).is_some()
}

/// Check if expression has the "res.braces" attribute and return it.
pub fn process_braces_attr(expr: &Expression) -> Option<&Attribute> {
    expr.pexp_attributes.iter().find(|attr| attr.0.txt == "res.braces")
}

/// Check if an expression is a template literal.
pub fn is_template_literal(expr: &Expression) -> bool {
    has_template_literal_attr(&expr.pexp_attributes)
}

/// Check if attributes contain template literal marker.
pub fn has_template_literal_attr(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| attr.0.txt == "res.template")
}

/// Check if an expression has the ternary attribute.
pub fn has_ternary_attribute(attrs: &Attributes) -> bool {
    attrs.iter().any(|attr| attr.0.txt == "res.ternary")
}

/// Check if an expression has the tagged template attribute.
pub fn has_tagged_template_attr(attrs: &Attributes) -> bool {
    attrs.iter().any(|attr| attr.0.txt == "res.taggedTemplate")
}

/// Check if an expression is a ternary expression.
/// This checks for the res.ternary attribute which is added by the parser
/// for expressions like `cond ? a : b` (as opposed to `if cond { a } else { b }`).
pub fn is_ternary_expr(expr: &Expression) -> bool {
    // Check for res.ternary attribute - this distinguishes ternary from if-then-else
    has_ternary_attribute(&expr.pexp_attributes)
}

/// Collect the parts of a ternary expression chain.
/// Returns a list of (condition, consequent) pairs and the final alternate expression.
pub fn collect_ternary_parts(
    expr: &Expression,
) -> (Vec<(&Expression, &Expression)>, &Expression) {
    let mut parts = Vec::new();
    let mut current = expr;

    loop {
        match &current.pexp_desc {
            ExpressionDesc::Pexp_ifthenelse(condition, consequent, Some(alternate))
                if has_ternary_attribute(&current.pexp_attributes) =>
            {
                parts.push((condition.as_ref(), consequent.as_ref()));
                current = alternate;
            }
            _ => break,
        }
    }

    (parts, current)
}

/// Filter out ternary attributes from a list of attributes.
pub fn filter_ternary_attributes(attrs: &Attributes) -> Vec<&Attribute> {
    attrs
        .iter()
        .filter(|attr| attr.0.txt != "res.ternary")
        .collect()
}

/// Check if an expression is a if-then-else (any form).
pub fn is_if_then_else_expr(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_ifthenelse(_, _, _)
    )
}

/// Check if attributes contain the "res.iflet" marker.
pub fn has_if_let_attribute(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| attr.0.txt == "res.iflet")
}

/// Check if an expression is an "if let" expression.
/// This is a `Pexp_match` with the `res.iflet` attribute.
pub fn is_if_let_expr(expr: &Expression) -> bool {
    matches!(&expr.pexp_desc, ExpressionDesc::Pexp_match(_, _))
        && has_if_let_attribute(&expr.pexp_attributes)
}

/// Represents the kind of condition in an if-chain.
#[derive(Debug)]
pub enum IfConditionKind<'a> {
    /// Regular if condition: `if expr { ... }`
    If(&'a Expression),
    /// If-let pattern matching: `if let pattern = expr { ... }`
    IfLet(&'a Pattern, &'a Expression),
}

/// Collect all the branches of an if-expression chain.
/// Returns a list of (location, condition_kind, then_expr) tuples and the final else expression.
pub fn collect_if_expressions<'a>(
    arena: &crate::parse_arena::ParseArena,
    expr: &'a Expression,
) -> (Vec<(&'a LocIdx, IfConditionKind<'a>, &'a Expression)>, Option<&'a Expression>) {
    let mut acc = Vec::new();
    let mut current = expr;

    loop {
        let expr_loc = &current.pexp_loc;
        match &current.pexp_desc {
            // Regular if-then-else with else branch
            ExpressionDesc::Pexp_ifthenelse(if_expr, then_expr, Some(else_expr)) => {
                acc.push((expr_loc, IfConditionKind::If(if_expr.as_ref()), then_expr.as_ref()));
                current = else_expr.as_ref();
            }
            // Regular if-then without else branch (terminal)
            ExpressionDesc::Pexp_ifthenelse(if_expr, then_expr, None) => {
                acc.push((expr_loc, IfConditionKind::If(if_expr.as_ref()), then_expr.as_ref()));
                return (acc, None);
            }
            // if let with no else (pattern matches unit constructor in else branch)
            ExpressionDesc::Pexp_match(condition, cases)
                if is_if_let_expr(current)
                    && cases.len() == 2
                    && cases[0].pc_guard.is_none()
                    && matches!(
                        &cases[1].pc_rhs.pexp_desc,
                        ExpressionDesc::Pexp_construct(lid, _) if arena.is_lident(lid.txt, "()")
                    ) =>
            {
                let pattern = &cases[0].pc_lhs;
                let then_expr = &cases[0].pc_rhs;
                acc.push((expr_loc, IfConditionKind::IfLet(pattern, condition.as_ref()), then_expr));
                return (acc, None);
            }
            // if let with else branch
            ExpressionDesc::Pexp_match(condition, cases)
                if is_if_let_expr(current)
                    && cases.len() == 2
                    && cases[0].pc_guard.is_none() =>
            {
                let pattern = &cases[0].pc_lhs;
                let then_expr = &cases[0].pc_rhs;
                let else_expr = &cases[1].pc_rhs;
                acc.push((expr_loc, IfConditionKind::IfLet(pattern, condition.as_ref()), then_expr));
                current = else_expr;
            }
            // End of chain
            _ => {
                return (acc, Some(current));
            }
        }
    }
}

/// Check if an expression is a simple "single expression" (not a block).
pub fn is_single_expression(expr: &Expression) -> bool {
    !is_block_expr(expr)
}

/// Check if expression is a binary expression.
pub fn is_binary_expression(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return false;
            }
            match &funct.pexp_desc {
                ExpressionDesc::Pexp_ident(ident) => {
                    if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                        let op = arena.get_string(*op_idx);
                        not_ghost_operator(op, ident.loc.is_none())
                    } else {
                        false
                    }
                }
                _ => false,
            }
        }
        _ => false,
    }
}

/// Get the operator from a binary expression, if it is one.
pub fn get_binary_operator(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> Option<String> {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return None;
            }
            match &funct.pexp_desc {
                ExpressionDesc::Pexp_ident(ident) => {
                    if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                        let op = arena.get_string(*op_idx);
                        if not_ghost_operator(op, ident.loc.is_none()) {
                            return Some(op.to_string());
                        }
                    }
                    None
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Check if expression is an await expression.
pub fn expr_is_await(expr: &Expression) -> bool {
    matches!(&expr.pexp_desc, ExpressionDesc::Pexp_await(_))
}

/// Check if expression is underscore apply sugar: `(__x) => f(...)`.
pub fn is_underscore_apply_sugar(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label: ArgLabel::Nolabel,
            default: None,
            lhs,
            rhs,
            ..
        } => {
            if let PatternDesc::Ppat_var(name) = &lhs.ppat_desc {
                if name.txt == "__x" {
                    return matches!(&rhs.pexp_desc, ExpressionDesc::Pexp_apply { .. });
                }
            }
            false
        }
        _ => false,
    }
}

/// Check if expression is a function or newtype.
pub fn is_fun_newtype(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_fun { .. } | ExpressionDesc::Pexp_newtype(_, _)
    )
}

/// Check if expression is an uncurried function (has explicit arity).
pub fn expr_is_uncurried_fun(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_fun { arity: Arity::Full(_), .. }
    )
}

/// Check if the last argument is a callback that needs special printing.
/// This returns true if the last argument is a function/newtype and
/// all other arguments are NOT functions/newtypes.
pub fn requires_special_callback_printing_last_arg(args: &[(ArgLabel, Expression)]) -> bool {
    let mut iter = args.iter().peekable();
    while let Some((_, expr)) = iter.next() {
        if is_fun_newtype(expr) {
            // If this is the last element, return true
            // Otherwise (function in non-last position), return false
            return iter.peek().is_none();
        }
    }
    false
}

/// Check if the first argument is a callback that needs special printing.
/// This returns true if:
/// - The first argument is a function/newtype
/// - AND there is at least one more argument after it
/// - AND all remaining arguments are NOT functions/newtypes
pub fn requires_special_callback_printing_first_arg(args: &[(ArgLabel, Expression)]) -> bool {
    match args {
        // Single argument callback - use normal printing
        [(_, expr)] if is_fun_newtype(expr) => false,
        // First is callback, need to check rest
        [(_, expr), rest @ ..] if is_fun_newtype(expr) => {
            // All remaining args must NOT be callbacks
            rest.iter().all(|(_, e)| !is_fun_newtype(e))
        }
        _ => false,
    }
}

/// Check if an expression is "huggable" (can be printed without line breaks).
pub fn is_huggable_expression(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_array(_)
        | ExpressionDesc::Pexp_tuple(_)
        | ExpressionDesc::Pexp_record(_, _) => true,
        ExpressionDesc::Pexp_constant(Constant::String(_, Some(_))) => true,
        ExpressionDesc::Pexp_construct(lid, _)
            if arena.is_lident(lid.txt, "::") || arena.is_lident(lid.txt, "[]") =>
        {
            true
        }
        ExpressionDesc::Pexp_extension(ext) if ext.0.txt == "obj" => true,
        _ if is_block_expr(expr) => true,
        _ if is_braced_expr(expr) => true,
        ExpressionDesc::Pexp_constant(Constant::String(txt, None))
            if is_multiline_text(txt) =>
        {
            true
        }
        _ => false,
    }
}

/// Check if text contains newlines (is multiline).
fn is_multiline_text(txt: &str) -> bool {
    let bytes = txt.as_bytes();
    let len = bytes.len();
    let mut i = 0;
    while i < len {
        match bytes[i] {
            b'\n' | b'\r' => return true,
            b'\\' => {
                if i + 2 >= len {
                    return false;
                }
                i += 2;
            }
            _ => i += 1,
        }
    }
    false
}

/// Check if an expression is "huggable" as a RHS (simpler than is_huggable_expression).
pub fn is_huggable_rhs(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_array(_)
        | ExpressionDesc::Pexp_tuple(_)
        | ExpressionDesc::Pexp_record(_, _) => true,
        ExpressionDesc::Pexp_extension(ext) if ext.0.txt == "obj" => true,
        _ if is_braced_expr(expr) => true,
        _ => false,
    }
}

/// Check if a longident represents a binary operator.
pub fn is_binary_operator(arena: &crate::parse_arena::ParseArena, lid: &Longident) -> bool {
    match lid {
        Longident::Lident(op_idx) => {
            let op = arena.get_string(*op_idx);
            matches!(
                op,
                "+" | "+."
                    | "-"
                    | "-."
                    | "*"
                    | "*."
                    | "/"
                    | "/."
                    | "**"
                    | "=="
                    | "==="
                    | "!="
                    | "!=="
                    | "<"
                    | "<="
                    | ">"
                    | ">="
                    | "|>"
                    | "||"
                    | "&&"
                    | "="
                    | ":="
                    | "++"
                    | "^"
                    | "|"
                    | "&"
                    | "mod"
                    | "land"
                    | "lor"
                    | "lxor"
                    | "lsl"
                    | "lsr"
                    | "asr"
            )
        }
        _ => false,
    }
}

/// Check if an expression is a unary expression.
pub fn is_unary_expression(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 1 {
                return false;
            }
            if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
                is_unary_operator_idx(arena, ident.txt)
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if a longident index represents a unary operator.
pub fn is_unary_operator_idx(arena: &crate::parse_arena::ParseArena, lid_idx: crate::parse_arena::LidentIdx) -> bool {
    if let Longident::Lident(op_idx) = arena.get_longident(lid_idx) {
        let op = arena.get_string(*op_idx);
        matches!(op, "~+" | "~+." | "~-" | "~-." | "~~~" | "!" | "not")
    } else {
        false
    }
}

/// Check if a longident represents a unary operator.
pub fn is_unary_operator(arena: &crate::parse_arena::ParseArena, lid: &Longident) -> bool {
    match lid {
        Longident::Lident(op_idx) => {
            let op = arena.get_string(*op_idx);
            matches!(op, "~+" | "~+." | "~-" | "~-." | "~~~" | "!" | "not")
        }
        _ => false,
    }
}

/// Check if the RHS of a binary expression should be inlined (not put on a new line).
/// This matches OCaml's should_inline_rhs_binary_expr from res_parsetree_viewer.ml:
/// - Pexp_constant: inline
/// - Pexp_let, Pexp_letmodule, Pexp_letexception: inline
/// - Pexp_sequence, Pexp_open, Pexp_ifthenelse: inline
/// - Pexp_for, Pexp_while, Pexp_try: inline
/// - Pexp_array, Pexp_record: inline
/// Note: Pexp_match (switch) is NOT in this list, so it will NOT inline
pub fn should_inline_rhs_binary_expr(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_constant(_)
            | ExpressionDesc::Pexp_let(_, _, _)
            | ExpressionDesc::Pexp_letmodule(_, _, _)
            | ExpressionDesc::Pexp_letexception(_, _)
            | ExpressionDesc::Pexp_sequence(_, _)
            | ExpressionDesc::Pexp_open(_, _, _)
            | ExpressionDesc::Pexp_ifthenelse(_, _, _)
            | ExpressionDesc::Pexp_for(_, _, _, _, _)
            | ExpressionDesc::Pexp_while(_, _)
            | ExpressionDesc::Pexp_try(_, _)
            | ExpressionDesc::Pexp_array(_)
            | ExpressionDesc::Pexp_record(_, _)
    )
}

// ============================================================================
// List Collection
// ============================================================================

/// Collect expressions from a list constructor (::).
/// Returns (list of expressions, optional spread expression).
pub fn collect_list_expressions<'a>(arena: &crate::parse_arena::ParseArena, expr: &'a Expression) -> (Vec<&'a Expression>, Option<&'a Expression>) {
    let mut acc = Vec::new();
    let mut current = expr;

    loop {
        match &current.pexp_desc {
            ExpressionDesc::Pexp_construct(lid, None) if arena.is_lident(lid.txt, "[]") => {
                return (acc, None);
            }
            ExpressionDesc::Pexp_construct(lid, Some(arg)) if arena.is_lident(lid.txt, "::") => {
                // The argument should be a tuple of (head, tail)
                if let ExpressionDesc::Pexp_tuple(exprs) = &arg.pexp_desc {
                    if exprs.len() == 2 {
                        acc.push(&exprs[0]);
                        current = &exprs[1];
                        continue;
                    }
                }
                // Malformed list, return what we have with spread
                return (acc, Some(current));
            }
            _ => {
                // Not a list constructor, this is a spread
                return (acc, Some(current));
            }
        }
    }
}

/// Collect patterns from a list constructor (::).
pub fn collect_list_patterns<'a>(arena: &crate::parse_arena::ParseArena, pat: &'a Pattern) -> (Vec<&'a Pattern>, Option<&'a Pattern>) {
    let mut acc = Vec::new();
    let mut current = pat;

    loop {
        match &current.ppat_desc {
            PatternDesc::Ppat_construct(lid, None) if arena.is_lident(lid.txt, "[]") => {
                return (acc, None);
            }
            PatternDesc::Ppat_construct(lid, Some(arg)) if arena.is_lident(lid.txt, "::") => {
                // The argument should be a tuple of (head, tail)
                if let PatternDesc::Ppat_tuple(pats) = &arg.ppat_desc {
                    if pats.len() == 2 {
                        acc.push(&pats[0]);
                        current = &pats[1];
                        continue;
                    }
                }
                // Malformed list, return what we have with spread
                return (acc, Some(current));
            }
            _ => {
                // Not a list constructor, this is a spread
                return (acc, Some(current));
            }
        }
    }
}

// ============================================================================
// Array Collection
// ============================================================================

/// Collect expressions from an array (or tuple).
pub fn collect_array_expressions(expr: &Expression) -> (Vec<&Expression>, Option<&Expression>) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_array(exprs) => {
            (exprs.iter().collect(), None)
        }
        _ => (vec![], Some(expr))
    }
}

// ============================================================================
// Function Analysis
// ============================================================================

/// Parameter for a function expression.
#[derive(Debug)]
pub enum FunParam<'a> {
    /// Regular parameter with pattern
    Parameter {
        attrs: &'a [Attribute],
        label: &'a ArgLabel,
        default_expr: Option<&'a Expression>,
        pat: &'a Pattern,
    },
    /// Newtype parameter(s): (type t) or (type t u v)
    /// Multiple consecutive newtypes are combined into a single NewTypes variant.
    NewTypes {
        attrs: &'a [Attribute],
        names: Vec<&'a StringLoc>,
    },
}

/// Extract function parameters from a potentially nested function expression.
/// Returns (is_async, parameters, body).
///
/// This matches OCaml's fun_expr which uses the `arity` field to determine
/// when to stop collecting parameters. For curried functions like
/// `(a, b, c) => (d, e, f) => 4`, the first function has an arity marker
/// that signals the end of its parameter list.
///
/// The rule from OCaml: collect params when `arity = None || n_fun = 0`
/// - Always collect from the first function (n_fun = 0)
/// - For nested functions, only collect if arity is Unknown (no marker)
///
/// IMPORTANT: OCaml's fun_expr only collects Pexp_newtype at the BEGINNING,
/// not during the collect_params loop. After encountering any Pexp_fun,
/// the loop stops at non-Pexp_fun nodes (including Pexp_newtype).
pub fn fun_expr(expr: &Expression) -> (bool, Vec<FunParam<'_>>, &Expression) {
    use crate::parser::ast::Arity;

    // Check for async attribute on the outermost expression
    let mut is_async = expr.pexp_attributes.iter().any(|attr| attr.0.txt == "res.async");

    let mut params = Vec::new();
    let mut current = expr;

    // First, collect leading Pexp_newtype (like OCaml's top-level match)
    // This combines consecutive newtypes like "type t, type u" into "type t u"
    if let ExpressionDesc::Pexp_newtype(name, body) = &current.pexp_desc {
        let mut names = vec![name];
        current = body;

        // Collect any additional consecutive newtypes
        while let ExpressionDesc::Pexp_newtype(next_name, next_body) = &current.pexp_desc {
            names.push(next_name);
            current = next_body;
        }

        params.push(FunParam::NewTypes { attrs: &[], names });
    }

    // Now collect_params: only handle Pexp_fun, stop at anything else
    let mut n_fun = 0;
    let mut found_first_pexp_fun = false;

    loop {
        match &current.pexp_desc {
            ExpressionDesc::Pexp_fun {
                arg_label,
                default,
                lhs,
                rhs,
                arity,
                is_async: expr_is_async,
            } => {
                // Check is_async on the FIRST Pexp_fun we encounter
                // (it may be nested inside Pexp_newtype)
                if !found_first_pexp_fun {
                    is_async = is_async || *expr_is_async;
                    found_first_pexp_fun = true;
                }

                // OCaml's condition: arity = None || n_fun = 0
                // In Rust: Arity::Unknown corresponds to None, Arity::Full(_) to Some(_)
                // Only collect if this is the first function OR there's no arity marker
                if n_fun == 0 || matches!(arity, Arity::Unknown) {
                    // OCaml's fun_expr passes {expr_ with pexp_attributes = []} to collect_params,
                    // which means the FIRST parameter always gets empty attrs. For nested functions,
                    // the inner expression keeps its attributes. We emulate this by using empty
                    // slice for n_fun == 0.
                    let attrs: &[Attribute] = if n_fun == 0 {
                        &[] // First parameter: OCaml strips top-level attrs
                    } else {
                        &current.pexp_attributes // Nested: preserve attrs
                    };
                    params.push(FunParam::Parameter {
                        attrs,
                        label: arg_label,
                        default_expr: default.as_ref().map(|e| e.as_ref()),
                        pat: lhs,
                    });
                    n_fun += 1;
                    current = rhs;
                } else {
                    // Stop collecting - arity marker indicates end of this function's params
                    break;
                }
            }
            // IMPORTANT: Don't collect Pexp_newtype here - OCaml's collect_params doesn't
            // This ensures `(type a, ()) => (type b c, x) => 3` keeps the two functions separate
            _ => break,
        }
    }

    (is_async, params, current)
}

// ============================================================================
// Attribute Processing
// ============================================================================

/// Filter out fragile match attributes (used internally).
pub fn filter_fragile_match_attributes(attrs: &Attributes) -> Vec<&Attribute> {
    attrs
        .iter()
        .filter(|attr| attr.0.txt != "res.braces" && attr.0.txt != "res.ternary")
        .collect()
}

/// Check if expression has spread attribute (...).
pub fn has_spread_attr(attrs: &Attributes) -> bool {
    attrs.iter().any(|attr| attr.0.txt == "res.spread")
}

/// Check if pattern has spread attribute (...).
pub fn pattern_has_spread_attr(attrs: &Attributes) -> bool {
    has_spread_attr(attrs)
}

/// Filter out parsing-only attributes (res.braces, res.ternary, res.await, etc.)
pub fn filter_parsing_attrs(attrs: &[Attribute]) -> Vec<&Attribute> {
    attrs
        .iter()
        .filter(|attr| {
            !matches!(
                attr.0.txt.as_str(),
                "res.braces"
                    | "ns.braces"
                    | "res.iflet"
                    | "res.ternary"
                    | "res.await"
                    | "res.template"
                    | "res.taggedTemplate"
                    | "res.patVariantSpread"
                    | "res.dictPattern"
                    | "res.inlineRecordDefinition"
            )
        })
        .collect()
}

/// Check if an attribute is printable (not an internal parsing attribute).
pub fn is_printable_attribute(attr: &Attribute) -> bool {
    !matches!(
        attr.0.txt.as_str(),
        "res.iflet"
            | "res.braces"
            | "ns.braces"
            | "JSX"
            | "res.await"
            | "res.template"
            | "res.taggedTemplate"
            | "res.ternary"
            | "res.inlineRecordDefinition"
    )
}

/// Check if attributes contain any printable attributes.
pub fn has_printable_attributes(attrs: &Attributes) -> bool {
    attrs.iter().any(is_printable_attribute)
}

/// Filter to only printable attributes.
pub fn filter_printable_attributes(attrs: &Attributes) -> Vec<&Attribute> {
    attrs.iter().filter(|a| is_printable_attribute(a)).collect()
}

/// Check if expression has any attributes (excluding internal parsing ones).
pub fn has_attributes(attrs: &Attributes) -> bool {
    attrs.iter().any(|attr| {
        !matches!(
            attr.0.txt.as_str(),
            "res.braces"
                | "ns.braces"
                | "res.iflet"
                | "res.ternary"
                | "res.await"
                | "res.template"
                | "res.inlineRecordDefinition"
        )
    })
}

// ============================================================================
// JSX Analysis
// ============================================================================

/// Check if an expression is a JSX expression.
pub fn is_jsx_expression(expr: &Expression) -> bool {
    matches!(&expr.pexp_desc, ExpressionDesc::Pexp_jsx_element(_))
}

// ============================================================================
// Operator Precedence
// ============================================================================

/// Get the precedence of a binary operator.
/// Higher number = higher precedence (binds tighter).
/// This matches the OCaml res_parsetree_viewer.ml exactly.
pub fn operator_precedence(op: &str) -> i32 {
    match op {
        ":=" => 1,
        "||" => 2,
        "&&" => 3,
        "|||" => 4,
        "^^^" => 5,
        "&&&" => 6,
        "==" | "===" | "<" | ">" | "!=" | "<>" | "!==" | "<=" | ">=" => 7,
        "<<" | ">>" | ">>>" => 8,
        "+" | "+." | "-" | "-." | "++" => 9,
        "*" | "*." | "/" | "/." | "%" => 10,
        "**" => 11,
        "#" | "##" | "->" => 12,
        _ => 0,
    }
}

/// Check if operator is right-associative.
pub fn is_right_associative(op: &str) -> bool {
    matches!(op, "**" | ":=" | "=")
}

/// Check if operator is an equality operator.
pub fn is_equality_operator(op: &str) -> bool {
    matches!(op, "==" | "===" | "!=" | "!==")
}

/// Check if parent and child operators can be flattened (same precedence, not both equality).
pub fn flattenable_operators(parent_op: &str, child_op: &str) -> bool {
    let prec_parent = operator_precedence(parent_op);
    let prec_child = operator_precedence(child_op);
    if prec_parent == prec_child {
        !(is_equality_operator(parent_op) && is_equality_operator(child_op))
    } else {
        false
    }
}

/// Check if a binary expression should have its RHS indented.
/// Returns true if:
/// - The operator is an equality operator (==, ===, !=, !==), OR
/// - The LHS is NOT a same-precedence sub-expression, OR
/// - The operator is `:=`
pub fn should_indent_binary_expr(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> bool {
    use crate::parser::longident::Longident;

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } if args.len() == 2 => {
            if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
                if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                    let op = arena.get_string(*op_idx);
                    if is_binary_operator_str(op) {
                        // Check the three conditions
                        if is_equality_operator(op) {
                            return true;
                        }
                        if op == ":=" {
                            return true;
                        }
                        // Check if LHS is NOT a same-precedence sub-expression
                        let (_, lhs) = &args[0];
                        if !same_precedence_sub_expression(arena, op, lhs) {
                            return true;
                        }
                    }
                }
            }
            false
        }
        _ => false,
    }
}

/// Check if sub_expression is a binary expression with same precedence as parent operator.
fn same_precedence_sub_expression(arena: &crate::parse_arena::ParseArena, parent_op: &str, sub_expr: &Expression) -> bool {
    use crate::parser::longident::Longident;

    match &sub_expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } if args.len() == 2 => {
            if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
                if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                    let sub_op = arena.get_string(*op_idx);
                    if is_binary_operator_str(sub_op) {
                        return flattenable_operators(parent_op, sub_op);
                    }
                }
            }
            // Not a binary expression, so not same precedence
            true
        }
        // Not a binary expression
        _ => true,
    }
}

/// Check if string is a binary operator.
pub fn is_binary_operator_str(op: &str) -> bool {
    matches!(
        op,
        ":=" | "||"
            | "&&"
            | "=="
            | "==="
            | "<"
            | ">"
            | "!="
            | "!=="
            | "<="
            | ">="
            | "+"
            | "+."
            | "-"
            | "-."
            | "++"
            | "*"
            | "*."
            | "/"
            | "/."
            | "**"
            | "->"
            | "<>"
            | "%"
            | "|||"
            | "^^^"
            | "&&&"
            | "<<"
            | ">>"
            | ">>>"
    )
}

/// Check if string is a unary operator.
pub fn is_unary_operator_str(op: &str) -> bool {
    matches!(op, "~+" | "~+." | "~-" | "~-." | "~~~" | "not")
}

/// Check if operator is not a ghost operator.
/// Ghost operators are internal and shouldn't be printed as binary ops.
pub fn not_ghost_operator(op: &str, is_ghost: bool) -> bool {
    is_binary_operator_str(op) && !(is_ghost && op == "++")
}

// ============================================================================
// Expression Rewriting
// ============================================================================

/// Check if an expression is the underscore apply sugar pattern: `(__x) => f(a, __x, c)`.
/// Returns a reference to the inner apply expression if it matches.
pub fn get_underscore_apply_inner<'a>(expr: &'a Expression) -> Option<&'a Expression> {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_fun {
            arg_label: ArgLabel::Nolabel,
            default: None,
            lhs,
            rhs,
            ..
        } => {
            // Check if pattern is `__x`
            if let PatternDesc::Ppat_var(name) = &lhs.ppat_desc {
                if name.txt == "__x" {
                    // Check if rhs is an apply
                    if matches!(&rhs.pexp_desc, ExpressionDesc::Pexp_apply { .. }) {
                        return Some(rhs);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Check if an expression is a `__x` identifier that should be printed as `_`.
/// Used when printing inside underscore apply sugar.
pub fn is_underscore_ident(expr: &Expression, arena: &ParseArena) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => arena.is_lident(lid.txt, "__x"),
        _ => false,
    }
}

// ============================================================================
// Type Analysis
// ============================================================================

/// A type parameter extracted from an arrow type.
#[derive(Debug)]
pub struct TypeParameter<'a> {
    /// Attributes on the parameter.
    pub attrs: &'a Attributes,
    /// Parameter label.
    pub lbl: &'a ArgLabel,
    /// Parameter type.
    pub typ: &'a CoreType,
}

/// Check if attributes contain the "@as" attribute.
fn has_as_attr(attrs: &Attributes) -> bool {
    attrs.iter().any(|attr| attr.0.txt == "as")
}

/// Extract arrow type components: (attrs_before, args, return_type)
///
/// This decomposes an arrow type like `(a, b) => c` into its argument list
/// and return type. The `max_arity` parameter limits how many arguments to extract.
pub fn arrow_type<'a>(
    typ: &'a CoreType,
    max_arity: Option<usize>,
) -> (&'a Attributes, Vec<TypeParameter<'a>>, &'a CoreType) {
    // Use isize to match OCaml's behavior where we stop when arity goes negative
    let max_arity: isize = max_arity.map(|n| n as isize).unwrap_or(isize::MAX);

    fn process<'a>(
        attrs_before: &'a Attributes,
        acc: Vec<TypeParameter<'a>>,
        typ: &'a CoreType,
        remaining_arity: isize,
    ) -> (&'a Attributes, Vec<TypeParameter<'a>>, &'a CoreType) {
        // OCaml: | _ when max_arity < 0 -> (attrs_before, List.rev acc, typ)
        // Stop when we've exhausted our arity budget (gone negative means we
        // tried to consume one more than allowed)
        if remaining_arity < 0 {
            return (attrs_before, acc, typ);
        }

        match &typ.ptyp_desc {
            CoreTypeDesc::Ptyp_arrow { arg, ret, arity } => {
                // Stop at explicitly-aritied arrow if we already have arguments
                if matches!(arity, Arity::Full(_)) && !acc.is_empty() && arg.attrs.is_empty() {
                    return (attrs_before, acc, typ);
                }

                // Unlabeled argument with no attrs - continue processing
                if matches!(arg.lbl, ArgLabel::Nolabel) && arg.attrs.is_empty() {
                    let mut new_acc = acc;
                    new_acc.push(TypeParameter {
                        attrs: &arg.attrs,
                        lbl: &arg.lbl,
                        typ: &arg.typ,
                    });
                    return process(attrs_before, new_acc, ret, remaining_arity - 1);
                }

                // Unlabeled argument with attrs - return as final type
                if matches!(arg.lbl, ArgLabel::Nolabel) {
                    return (attrs_before, acc, typ);
                }

                // Labeled argument - continue processing
                // Calculate adjusted arity for @as workaround
                // OCaml: When arg is Ptyp_any with @as attr, don't decrement arity
                let adjusted_arity = if matches!(
                    &arg.typ.ptyp_desc,
                    CoreTypeDesc::Ptyp_any
                ) && has_as_attr(&arg.typ.ptyp_attributes)
                {
                    remaining_arity
                } else {
                    remaining_arity - 1
                };

                let mut new_acc = acc;
                new_acc.push(TypeParameter {
                    attrs: &arg.attrs,
                    lbl: &arg.lbl,
                    typ: &arg.typ,
                });
                process(attrs_before, new_acc, ret, adjusted_arity)
            }

            _ => (attrs_before, acc, typ),
        }
    }

    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_arrow { .. } => {
            // Start processing with the type's attributes as attrs_before,
            // and process the type without those attributes
            process(&typ.ptyp_attributes, Vec::new(), typ, max_arity)
        }
        _ => process(&EMPTY_ATTRS, Vec::new(), typ, max_arity),
    }
}

/// Empty attributes constant for returning references.
static EMPTY_ATTRS: Attributes = Vec::new();

// ============================================================================
// Module Analysis
// ============================================================================

/// Extract functor applications from a module expression.
///
/// For `F(A)(B)(C)`, returns `([A, B, C], F)`.
pub fn mod_expr_apply(mod_expr: &ModuleExpr) -> (Vec<&ModuleExpr>, &ModuleExpr) {
    fn loop_apply<'a>(acc: Vec<&'a ModuleExpr>, mod_expr: &'a ModuleExpr) -> (Vec<&'a ModuleExpr>, &'a ModuleExpr) {
        match &mod_expr.pmod_desc {
            ModuleExprDesc::Pmod_apply(next, arg) => {
                let mut new_acc = vec![arg.as_ref()];
                new_acc.extend(acc);
                loop_apply(new_acc, next)
            }
            _ => (acc, mod_expr),
        }
    }
    loop_apply(Vec::new(), mod_expr)
}

/// A functor parameter.
pub struct FunctorParam<'a> {
    /// Attributes on the parameter.
    pub attrs: &'a Attributes,
    /// Parameter name.
    pub lbl: &'a StringLoc,
    /// Optional module type constraint.
    pub mod_type: Option<&'a ModuleType>,
}

/// Extract functor parameters from a module expression.
///
/// For `(A: X, B: Y) => body`, returns `([(attrs, A, Some(X)), (attrs, B, Some(Y))], body)`.
pub fn mod_expr_functor(mod_expr: &ModuleExpr) -> (Vec<FunctorParam<'_>>, &ModuleExpr) {
    fn loop_functor<'a>(acc: Vec<FunctorParam<'a>>, mod_expr: &'a ModuleExpr) -> (Vec<FunctorParam<'a>>, &'a ModuleExpr) {
        match &mod_expr.pmod_desc {
            ModuleExprDesc::Pmod_functor(lbl, mod_type, return_mod_expr) => {
                let param = FunctorParam {
                    attrs: &mod_expr.pmod_attributes,
                    lbl,
                    mod_type: mod_type.as_ref().map(|t| t.as_ref()),
                };
                let mut new_acc = acc;
                new_acc.push(param);
                loop_functor(new_acc, return_mod_expr)
            }
            _ => (acc, mod_expr),
        }
    }
    let (params, body) = loop_functor(Vec::new(), mod_expr);
    (params, body)
}

/// Check if attributes contain the await attribute.
pub fn has_await_attribute(attrs: &Attributes) -> bool {
    attrs.iter().any(|attr| attr.0.txt == "res.await")
}

// ============================================================================
// Array/String Access Detection
// ============================================================================

/// Check if an expression is `Array.get(arr, idx)`.
pub fn is_array_access(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return false;
            }
            if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                if let Longident::Ldot(base, method_idx) = arena.get_longident(lid.txt) {
                    if let Longident::Lident(module_idx) = base.as_ref() {
                        let module = arena.get_string(*module_idx);
                        let method = arena.get_string(*method_idx);
                        return module == "Array" && method == "get";
                    }
                }
            }
            false
        }
        _ => false,
    }
}

/// Check if an expression is `Array.set(arr, idx, value)`.
pub fn is_array_set(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 3 {
                return false;
            }
            if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                if let Longident::Ldot(base, method_idx) = arena.get_longident(lid.txt) {
                    if let Longident::Lident(module_idx) = base.as_ref() {
                        let module = arena.get_string(*module_idx);
                        let method = arena.get_string(*method_idx);
                        return module == "Array" && method == "set";
                    }
                }
            }
            false
        }
        _ => false,
    }
}

/// Check if an expression is `String.get(str, idx)`.
pub fn is_string_access(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return false;
            }
            if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                if let Longident::Ldot(base, method_idx) = arena.get_longident(lid.txt) {
                    if let Longident::Lident(module_idx) = base.as_ref() {
                        let module = arena.get_string(*module_idx);
                        let method = arena.get_string(*method_idx);
                        return module == "String" && method == "get";
                    }
                }
            }
            false
        }
        _ => false,
    }
}

/// Check if an expression is a rewritten underscore apply sugar pattern.
/// i.e., the pattern `_` that comes from `arr[_]` being rewritten to `(__x) => Array.get(__x, _)`.
pub fn is_rewritten_underscore_apply_sugar(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            arena.is_lident(lid.txt, "_")
        }
        _ => false,
    }
}

/// Partition attributes into doc comments and regular attributes.
pub fn partition_doc_comment_attributes(attrs: &Attributes) -> (Vec<&Attribute>, Vec<&Attribute>) {
    let mut doc_comments = Vec::new();
    let mut others = Vec::new();
    for attr in attrs {
        if attr.0.txt == "res.doc" || attr.0.txt == "ocaml.doc" {
            doc_comments.push(attr);
        } else {
            others.push(attr);
        }
    }
    (doc_comments, others)
}

// ============================================================================
// Pattern Analysis
// ============================================================================

/// Collect all patterns in an or-pattern chain.
/// For example, `Red | Blue | Green` becomes `[Red, Blue, Green]`.
/// For `Red | (Blue | Green)`, this becomes `[Red, Ppat_or(Blue, Green)]`,
/// preserving the nested or-pattern (which should be wrapped in parens when printed).
pub fn collect_or_pattern_chain(pat: &Pattern) -> Vec<&Pattern> {
    fn loop_collect<'a>(pattern: &'a Pattern, chain: &mut Vec<&'a Pattern>) {
        match &pattern.ppat_desc {
            PatternDesc::Ppat_or(left, right) => {
                loop_collect(left, chain);
                chain.push(right.as_ref());
            }
            _ => {
                chain.push(pattern);
            }
        }
    }

    let mut result = Vec::new();
    loop_collect(pat, &mut result);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_binary_operator() {
        assert!(is_binary_operator(&Longident::Lident("+".to_string())));
        assert!(is_binary_operator(&Longident::Lident("==".to_string())));
        assert!(is_binary_operator(&Longident::Lident("|>".to_string())));
        assert!(!is_binary_operator(&Longident::Lident("foo".to_string())));
    }

    #[test]
    fn test_is_unary_operator() {
        assert!(is_unary_operator(&Longident::Lident("!".to_string())));
        assert!(is_unary_operator(&Longident::Lident("not".to_string())));
        assert!(is_unary_operator(&Longident::Lident("~-".to_string())));
        assert!(!is_unary_operator(&Longident::Lident("+".to_string())));
    }

    #[test]
    fn test_operator_precedence() {
        assert!(operator_precedence("*") > operator_precedence("+"));
        assert!(operator_precedence("**") > operator_precedence("*"));
        assert!(operator_precedence("&&") > operator_precedence("||"));
    }
}
