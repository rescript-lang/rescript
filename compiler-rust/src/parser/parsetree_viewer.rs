//! Parsetree viewer utilities.
//!
//! This module provides helper functions for analyzing and inspecting the AST.
//! It's a port of OCaml's `res_parsetree_viewer.ml`.

use crate::location::Location;
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

/// Check if an expression is a simple "single expression" (not a block).
pub fn is_single_expression(expr: &Expression) -> bool {
    !is_block_expr(expr)
}

/// Check if expression is a binary expression.
pub fn is_binary_expression(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return false;
            }
            match &funct.pexp_desc {
                ExpressionDesc::Pexp_ident(ident) => {
                    if let Longident::Lident(op) = &ident.txt {
                        not_ghost_operator(op, &ident.loc)
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

/// Check if an expression is "huggable" (can be printed without line breaks).
pub fn is_huggable_expression(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_array(_)
        | ExpressionDesc::Pexp_tuple(_)
        | ExpressionDesc::Pexp_record(_, _) => true,
        ExpressionDesc::Pexp_constant(Constant::String(_, Some(_))) => true,
        ExpressionDesc::Pexp_construct(lid, _)
            if matches!(
                lid.txt,
                Longident::Lident(ref s) if s == "::" || s == "[]"
            ) =>
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

/// Check if a longident represents a binary operator.
pub fn is_binary_operator(lid: &Longident) -> bool {
    match lid {
        Longident::Lident(op) => {
            matches!(
                op.as_str(),
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
pub fn is_unary_expression(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 1 {
                return false;
            }
            if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
                is_unary_operator(&ident.txt)
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if a longident represents a unary operator.
pub fn is_unary_operator(lid: &Longident) -> bool {
    match lid {
        Longident::Lident(op) => {
            matches!(op.as_str(), "~+" | "~+." | "~-" | "~-." | "!" | "not")
        }
        _ => false,
    }
}

// ============================================================================
// List Collection
// ============================================================================

/// Collect expressions from a list constructor (::).
/// Returns (list of expressions, optional spread expression).
pub fn collect_list_expressions(expr: &Expression) -> (Vec<&Expression>, Option<&Expression>) {
    let mut acc = Vec::new();
    let mut current = expr;

    loop {
        match &current.pexp_desc {
            ExpressionDesc::Pexp_construct(lid, None) if lid.txt == Longident::Lident("[]".to_string()) => {
                return (acc, None);
            }
            ExpressionDesc::Pexp_construct(lid, Some(arg)) if lid.txt == Longident::Lident("::".to_string()) => {
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
pub fn collect_list_patterns(pat: &Pattern) -> (Vec<&Pattern>, Option<&Pattern>) {
    let mut acc = Vec::new();
    let mut current = pat;

    loop {
        match &current.ppat_desc {
            PatternDesc::Ppat_construct(lid, None) if lid.txt == Longident::Lident("[]".to_string()) => {
                return (acc, None);
            }
            PatternDesc::Ppat_construct(lid, Some(arg)) if lid.txt == Longident::Lident("::".to_string()) => {
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
        attrs: &'a Attributes,
        label: &'a ArgLabel,
        default_expr: Option<&'a Expression>,
        pat: &'a Pattern,
    },
    /// Newtype parameter: (type t)
    NewType {
        attrs: &'a Attributes,
        name: &'a StringLoc,
    },
}

/// Extract function parameters from a potentially nested function expression.
/// Returns (is_async, parameters, body).
pub fn fun_expr(expr: &Expression) -> (bool, Vec<FunParam<'_>>, &Expression) {
    // Check for async attribute OR is_async field in Pexp_fun
    let mut is_async = expr.pexp_attributes.iter().any(|attr| attr.0.txt == "res.async");

    // Also check is_async field on the first Pexp_fun
    if let ExpressionDesc::Pexp_fun {
        is_async: expr_is_async,
        ..
    } = &expr.pexp_desc
    {
        is_async = is_async || *expr_is_async;
    }

    let mut params = Vec::new();
    let mut current = expr;

    loop {
        match &current.pexp_desc {
            ExpressionDesc::Pexp_fun {
                arg_label,
                default,
                lhs,
                rhs,
                ..
            } => {
                params.push(FunParam::Parameter {
                    attrs: &current.pexp_attributes,
                    label: arg_label,
                    default_expr: default.as_ref().map(|e| e.as_ref()),
                    pat: lhs,
                });
                current = rhs;
            }
            ExpressionDesc::Pexp_newtype(name, body) => {
                params.push(FunParam::NewType {
                    attrs: &current.pexp_attributes,
                    name,
                });
                current = body;
            }
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
pub fn filter_parsing_attrs(attrs: &Attributes) -> Vec<&Attribute> {
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
pub fn not_ghost_operator(op: &str, loc: &Location) -> bool {
    is_binary_operator_str(op) && !(loc.loc_ghost && op == "++")
}

// ============================================================================
// Expression Rewriting
// ============================================================================

/// Rewrite underscore apply: `(__x) => f(a, __x, c)` becomes `f(a, _, c)`.
/// Returns the rewritten expression if applicable, otherwise None.
pub fn rewrite_underscore_apply(expr: &Expression) -> Option<Expression> {
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
                    if let ExpressionDesc::Pexp_apply { .. } = &rhs.pexp_desc {
                        // TODO: Actually rewrite the apply to use `_`
                        // For now, just return None to use the original printing
                        return None;
                    }
                }
            }
            None
        }
        _ => None,
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
    let max_arity = max_arity.unwrap_or(usize::MAX);

    fn process<'a>(
        attrs_before: &'a Attributes,
        acc: Vec<TypeParameter<'a>>,
        typ: &'a CoreType,
        remaining_arity: usize,
    ) -> (&'a Attributes, Vec<TypeParameter<'a>>, &'a CoreType) {
        if remaining_arity == 0 {
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
pub fn is_array_access(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return false;
            }
            if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                if let Longident::Ldot(base, method) = &lid.txt {
                    if let Longident::Lident(module) = base.as_ref() {
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
pub fn is_array_set(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 3 {
                return false;
            }
            if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                if let Longident::Ldot(base, method) = &lid.txt {
                    if let Longident::Lident(module) = base.as_ref() {
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
pub fn is_string_access(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return false;
            }
            if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                if let Longident::Ldot(base, method) = &lid.txt {
                    if let Longident::Lident(module) = base.as_ref() {
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
pub fn is_rewritten_underscore_apply_sugar(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            matches!(&lid.txt, Longident::Lident(name) if name == "_")
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
