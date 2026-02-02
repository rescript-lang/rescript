//! Parenthesization logic for the printer.
//!
//! This module provides functions to determine when parentheses or braces
//! are needed in printed output. It's a port of OCaml's `res_parens.ml`.

use crate::parse_arena::LocIdx;
use crate::parser::ast::*;
use crate::parser::longident::Longident;
use crate::parser::parsetree_viewer;

/// Result of checking if parentheses/braces are needed.
#[derive(Debug, Clone, PartialEq)]
pub enum ParenKind {
    /// Parentheses are needed
    Parenthesized,
    /// Braces are needed at the given location
    Braced(LocIdx),
    /// No wrapping needed
    Nothing,
}

/// Check if an expression needs parentheses/braces.
pub fn expr(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => match &expr.pexp_desc {
            // module(M: S) doesn't need parens
            ExpressionDesc::Pexp_constraint(inner, typ)
                if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                    && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
            {
                ParenKind::Nothing
            }
            ExpressionDesc::Pexp_constraint(_, _) => ParenKind::Parenthesized,
            _ => ParenKind::Nothing,
        },
    }
}

/// Check if expression record row RHS needs parentheses.
pub fn expr_record_row_rhs(arena: &crate::parse_arena::ParseArena, optional: bool, e: &Expression) -> ParenKind {
    let kind = expr(arena, e);
    match kind {
        ParenKind::Nothing if optional => match &e.pexp_desc {
            ExpressionDesc::Pexp_ifthenelse(_, _, _) | ExpressionDesc::Pexp_fun { .. } => {
                ParenKind::Parenthesized
            }
            _ if parsetree_viewer::is_binary_expression(arena, e) => ParenKind::Parenthesized,
            _ => kind,
        },
        _ => kind,
    }
}

/// Check if a call expression needs parentheses/braces.
pub fn call_expr(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => {
            // Check for parsing attrs
            let filtered = parsetree_viewer::filter_parsing_attrs(&expr.pexp_attributes);
            if !filtered.is_empty() {
                return ParenKind::Parenthesized;
            }

            if parsetree_viewer::is_unary_expression(arena, expr)
                || parsetree_viewer::is_binary_expression(arena, expr)
            {
                return ParenKind::Parenthesized;
            }

            match &expr.pexp_desc {
                // module(M: S) doesn't need parens
                ExpressionDesc::Pexp_constraint(inner, typ)
                    if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                        && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
                {
                    ParenKind::Nothing
                }
                ExpressionDesc::Pexp_fun { .. }
                    if parsetree_viewer::is_underscore_apply_sugar(expr) =>
                {
                    ParenKind::Nothing
                }
                ExpressionDesc::Pexp_assert(_)
                | ExpressionDesc::Pexp_fun { .. }
                | ExpressionDesc::Pexp_newtype(_, _)
                | ExpressionDesc::Pexp_constraint(_, _)
                | ExpressionDesc::Pexp_setfield(_, _, _)
                | ExpressionDesc::Pexp_match(_, _)
                | ExpressionDesc::Pexp_try(_, _)
                | ExpressionDesc::Pexp_while(_, _)
                | ExpressionDesc::Pexp_for(_, _, _, _, _)
                | ExpressionDesc::Pexp_ifthenelse(_, _, _) => ParenKind::Parenthesized,
                _ if parsetree_viewer::expr_is_uncurried_fun(expr) => ParenKind::Parenthesized,
                _ if parsetree_viewer::expr_is_await(expr) => ParenKind::Parenthesized,
                _ => ParenKind::Nothing,
            }
        }
    }
}

/// Check if a structure expression needs parentheses/braces.
pub fn structure_expr(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => match &expr.pexp_desc {
            ExpressionDesc::Pexp_jsx_element(_) => ParenKind::Nothing,
            _ if parsetree_viewer::has_attributes(&expr.pexp_attributes) => {
                ParenKind::Parenthesized
            }
            // module(M: S) doesn't need parens
            ExpressionDesc::Pexp_constraint(inner, typ)
                if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                    && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
            {
                ParenKind::Nothing
            }
            ExpressionDesc::Pexp_constraint(_, _) => ParenKind::Parenthesized,
            _ => ParenKind::Nothing,
        },
    }
}

/// Check if a unary expression operand needs parentheses/braces.
pub fn unary_expr_operand(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => {
            let filtered = parsetree_viewer::filter_parsing_attrs(&expr.pexp_attributes);
            if !filtered.is_empty() {
                return ParenKind::Parenthesized;
            }

            if parsetree_viewer::is_unary_expression(arena, expr)
                || parsetree_viewer::is_binary_expression(arena, expr)
            {
                return ParenKind::Parenthesized;
            }

            match &expr.pexp_desc {
                // module(M: S) doesn't need parens
                ExpressionDesc::Pexp_constraint(inner, typ)
                    if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                        && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
                {
                    ParenKind::Nothing
                }
                ExpressionDesc::Pexp_fun { .. }
                    if parsetree_viewer::is_underscore_apply_sugar(expr) =>
                {
                    ParenKind::Nothing
                }
                ExpressionDesc::Pexp_assert(_)
                | ExpressionDesc::Pexp_fun { .. }
                | ExpressionDesc::Pexp_newtype(_, _)
                | ExpressionDesc::Pexp_constraint(_, _)
                | ExpressionDesc::Pexp_setfield(_, _, _)
                | ExpressionDesc::Pexp_extension(_)
                | ExpressionDesc::Pexp_match(_, _)
                | ExpressionDesc::Pexp_try(_, _)
                | ExpressionDesc::Pexp_while(_, _)
                | ExpressionDesc::Pexp_for(_, _, _, _, _)
                | ExpressionDesc::Pexp_ifthenelse(_, _, _) => ParenKind::Parenthesized,
                _ if parsetree_viewer::expr_is_await(expr) => ParenKind::Parenthesized,
                _ => ParenKind::Nothing,
            }
        }
    }
}

/// Check if a binary expression operand needs parentheses/braces.
pub fn binary_expr_operand(arena: &crate::parse_arena::ParseArena, is_lhs: bool, expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => {
            match &expr.pexp_desc {
                // module(M: S) doesn't need parens
                ExpressionDesc::Pexp_constraint(inner, typ)
                    if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                        && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
                {
                    ParenKind::Nothing
                }
                ExpressionDesc::Pexp_fun { .. }
                    if parsetree_viewer::is_underscore_apply_sugar(expr) =>
                {
                    ParenKind::Nothing
                }
                ExpressionDesc::Pexp_constraint(_, _)
                | ExpressionDesc::Pexp_fun { .. }
                | ExpressionDesc::Pexp_newtype(_, _) => ParenKind::Parenthesized,
                _ if parsetree_viewer::expr_is_uncurried_fun(expr) => ParenKind::Parenthesized,
                _ if parsetree_viewer::is_binary_expression(arena, expr) => ParenKind::Parenthesized,
                _ if parsetree_viewer::is_ternary_expr(expr) => ParenKind::Parenthesized,
                ExpressionDesc::Pexp_assert(_) if is_lhs => ParenKind::Parenthesized,
                _ if parsetree_viewer::expr_is_await(expr) => ParenKind::Parenthesized,
                _ => {
                    if parsetree_viewer::has_printable_attributes(&expr.pexp_attributes) {
                        ParenKind::Parenthesized
                    } else {
                        ParenKind::Nothing
                    }
                }
            }
        }
    }
}

/// Check if sub-binary expression operand needs parens based on precedence.
pub fn sub_binary_expr_operand(parent_operator: &str, child_operator: &str) -> bool {
    let prec_parent = parsetree_viewer::operator_precedence(parent_operator);
    let prec_child = parsetree_viewer::operator_precedence(child_operator);

    prec_parent > prec_child
        || (parsetree_viewer::is_equality_operator(parent_operator)
            && parsetree_viewer::is_equality_operator(child_operator))
        // a && b || c, add parens to (a && b) for readability
        || (parent_operator == "||" && child_operator == "&&")
}

/// Check if RHS binary expression operand needs parentheses.
pub fn rhs_binary_expr_operand(arena: &crate::parse_arena::ParseArena, parent_operator: &str, rhs: &Expression) -> bool {
    match &rhs.pexp_desc {
        ExpressionDesc::Pexp_apply {
            funct,
            args,
            ..
        } => {
            if args.len() != 2 {
                return false;
            }
            match &funct.pexp_desc {
                ExpressionDesc::Pexp_ident(ident) if funct.pexp_attributes.is_empty() => {
                    if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                        let operator = arena.get_string(*op_idx);
                        if parsetree_viewer::not_ghost_operator(operator, ident.loc.is_none()) {
                            let prec_parent =
                                parsetree_viewer::operator_precedence(parent_operator);
                            let prec_child = parsetree_viewer::operator_precedence(operator);
                            return prec_parent == prec_child;
                        }
                    }
                    false
                }
                _ => false,
            }
        }
        _ => false,
    }
}

/// Check if flatten operand RHS needs parentheses.
pub fn flatten_operand_rhs(arena: &crate::parse_arena::ParseArena, parent_operator: &str, rhs: &Expression) -> bool {
    match &rhs.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return false;
            }
            match &funct.pexp_desc {
                ExpressionDesc::Pexp_ident(ident) => {
                    if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                        let operator = arena.get_string(*op_idx);
                        if parsetree_viewer::not_ghost_operator(operator, ident.loc.is_none()) {
                            let prec_parent =
                                parsetree_viewer::operator_precedence(parent_operator);
                            let prec_child = parsetree_viewer::operator_precedence(operator);
                            return prec_parent >= prec_child || !rhs.pexp_attributes.is_empty();
                        }
                    }
                    false
                }
                _ => false,
            }
        }
        ExpressionDesc::Pexp_constraint(inner, typ)
            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
        {
            false
        }
        ExpressionDesc::Pexp_fun { lhs, .. } => {
            if let PatternDesc::Ppat_var(name) = &lhs.ppat_desc {
                if name.txt == "__x" {
                    return false;
                }
            }
            true
        }
        ExpressionDesc::Pexp_newtype(_, _)
        | ExpressionDesc::Pexp_setfield(_, _, _)
        | ExpressionDesc::Pexp_constraint(_, _) => true,
        _ if parsetree_viewer::is_ternary_expr(rhs) => true,
        _ => false,
    }
}

/// Check if flatten operand RHS needs parentheses, ignoring the attribute check.
/// This is used when we've already partitioned printable attrs and will handle them separately.
pub fn flatten_operand_rhs_without_attrs(arena: &crate::parse_arena::ParseArena, parent_operator: &str, rhs: &Expression) -> bool {
    match &rhs.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            if args.len() != 2 {
                return false;
            }
            match &funct.pexp_desc {
                ExpressionDesc::Pexp_ident(ident) => {
                    if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                        let operator = arena.get_string(*op_idx);
                        if parsetree_viewer::not_ghost_operator(operator, ident.loc.is_none()) {
                            let prec_parent =
                                parsetree_viewer::operator_precedence(parent_operator);
                            let prec_child = parsetree_viewer::operator_precedence(operator);
                            // NOTE: Unlike flatten_operand_rhs, we don't check rhs.pexp_attributes here
                            return prec_parent >= prec_child;
                        }
                    }
                    false
                }
                _ => false,
            }
        }
        ExpressionDesc::Pexp_constraint(inner, typ)
            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
        {
            false
        }
        ExpressionDesc::Pexp_fun { lhs, .. } => {
            if let PatternDesc::Ppat_var(name) = &lhs.ppat_desc {
                if name.txt == "__x" {
                    return false;
                }
            }
            true
        }
        ExpressionDesc::Pexp_newtype(_, _)
        | ExpressionDesc::Pexp_setfield(_, _, _)
        | ExpressionDesc::Pexp_constraint(_, _) => true,
        _ if parsetree_viewer::is_ternary_expr(rhs) => true,
        _ => false,
    }
}

/// Check if binary operator inside await needs parentheses.
pub fn binary_operator_inside_await_needs_parens(operator: &str) -> bool {
    parsetree_viewer::operator_precedence(operator) < parsetree_viewer::operator_precedence("->")
}

/// Check if assert or await expression RHS needs parentheses/braces.
pub fn assert_or_await_expr_rhs(arena: &crate::parse_arena::ParseArena, in_await: bool, expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => assert_or_await_expr_rhs_inner(arena, in_await, expr),
    }
}

/// Check if await expression RHS needs parens, but IGNORE the res.braces attribute.
/// OCaml's await printing filters out @res.braces before checking, so `await {x}` prints as `await x`.
pub fn assert_or_await_expr_rhs_ignore_braces(arena: &crate::parse_arena::ParseArena, in_await: bool, expr: &Expression) -> ParenKind {
    // Ignore the braces attribute - just check the expression structure
    assert_or_await_expr_rhs_inner(arena, in_await, expr)
}

fn assert_or_await_expr_rhs_inner(arena: &crate::parse_arena::ParseArena, in_await: bool, expr: &Expression) -> ParenKind {
    let filtered = parsetree_viewer::filter_parsing_attrs(&expr.pexp_attributes);
    if !filtered.is_empty() {
        return ParenKind::Parenthesized;
    }

    match &expr.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, .. }
            if parsetree_viewer::is_binary_expression(arena, expr) =>
        {
            if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
                if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                    let operator = arena.get_string(*op_idx);
                    if in_await && !binary_operator_inside_await_needs_parens(operator) {
                        return ParenKind::Nothing;
                    }
                }
            }
            ParenKind::Parenthesized
        }
        // module(M: S) doesn't need parens
        ExpressionDesc::Pexp_constraint(inner, typ)
            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
        {
            ParenKind::Nothing
        }
        ExpressionDesc::Pexp_fun { .. }
            if parsetree_viewer::is_underscore_apply_sugar(expr) =>
        {
            ParenKind::Nothing
        }
        ExpressionDesc::Pexp_assert(_)
        | ExpressionDesc::Pexp_fun { .. }
        | ExpressionDesc::Pexp_newtype(_, _)
        | ExpressionDesc::Pexp_constraint(_, _)
        | ExpressionDesc::Pexp_setfield(_, _, _)
        | ExpressionDesc::Pexp_match(_, _)
        | ExpressionDesc::Pexp_try(_, _)
        | ExpressionDesc::Pexp_while(_, _)
        | ExpressionDesc::Pexp_for(_, _, _, _, _)
        | ExpressionDesc::Pexp_ifthenelse(_, _, _) => ParenKind::Parenthesized,
        _ if !in_await && parsetree_viewer::expr_is_await(expr) => {
            ParenKind::Parenthesized
        }
        _ => ParenKind::Nothing,
    }
}

/// Check if a constant is negative.
pub fn is_negative_constant(constant: &Constant) -> bool {
    fn is_neg(txt: &str) -> bool {
        txt.starts_with('-')
    }
    match constant {
        Constant::Integer(i, _) | Constant::Float(i, _) => is_neg(i),
        _ => false,
    }
}

/// Check if field expression needs parentheses/braces.
pub fn field_expr(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => {
            let filtered = parsetree_viewer::filter_parsing_attrs(&expr.pexp_attributes);
            if !filtered.is_empty() {
                return ParenKind::Parenthesized;
            }

            if parsetree_viewer::is_binary_expression(arena, expr)
                || parsetree_viewer::is_unary_expression(arena, expr)
            {
                return ParenKind::Parenthesized;
            }

            match &expr.pexp_desc {
                // module(M: S) doesn't need parens
                ExpressionDesc::Pexp_constraint(inner, typ)
                    if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                        && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
                {
                    ParenKind::Nothing
                }
                ExpressionDesc::Pexp_constant(c) if is_negative_constant(c) => {
                    ParenKind::Parenthesized
                }
                ExpressionDesc::Pexp_fun { .. }
                    if parsetree_viewer::is_underscore_apply_sugar(expr) =>
                {
                    ParenKind::Nothing
                }
                ExpressionDesc::Pexp_assert(_)
                | ExpressionDesc::Pexp_extension(_)
                | ExpressionDesc::Pexp_fun { .. }
                | ExpressionDesc::Pexp_newtype(_, _)
                | ExpressionDesc::Pexp_constraint(_, _)
                | ExpressionDesc::Pexp_setfield(_, _, _)
                | ExpressionDesc::Pexp_match(_, _)
                | ExpressionDesc::Pexp_try(_, _)
                | ExpressionDesc::Pexp_while(_, _)
                | ExpressionDesc::Pexp_for(_, _, _, _, _)
                | ExpressionDesc::Pexp_ifthenelse(_, _, _) => ParenKind::Parenthesized,
                _ if parsetree_viewer::expr_is_await(expr) => ParenKind::Parenthesized,
                _ => ParenKind::Nothing,
            }
        }
    }
}

/// Check if set field expression RHS needs parentheses/braces.
pub fn set_field_expr_rhs(expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => match &expr.pexp_desc {
            // module(M: S) doesn't need parens
            ExpressionDesc::Pexp_constraint(inner, typ)
                if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                    && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
            {
                ParenKind::Nothing
            }
            ExpressionDesc::Pexp_constraint(_, _) => ParenKind::Parenthesized,
            _ => ParenKind::Nothing,
        },
    }
}

/// Check if ternary operand needs parentheses/braces.
pub fn ternary_operand(expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => match &expr.pexp_desc {
            // module(M: S) doesn't need parens
            ExpressionDesc::Pexp_constraint(inner, typ)
                if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                    && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
            {
                ParenKind::Nothing
            }
            ExpressionDesc::Pexp_constraint(_, _) => ParenKind::Parenthesized,
            _ if parsetree_viewer::is_fun_newtype(expr) => {
                let (_, _, return_expr) = parsetree_viewer::fun_expr(expr);
                match &return_expr.pexp_desc {
                    ExpressionDesc::Pexp_constraint(_, _) => ParenKind::Parenthesized,
                    _ => ParenKind::Nothing,
                }
            }
            _ => ParenKind::Nothing,
        },
    }
}

/// Check if a string starts with minus.
pub fn starts_with_minus(txt: &str) -> bool {
    txt.starts_with('-')
}

/// Check if JSX prop expression needs parentheses/braces.
pub fn jsx_prop_expr(expr: &Expression) -> ParenKind {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_let(_, _, _)
        | ExpressionDesc::Pexp_sequence(_, _)
        | ExpressionDesc::Pexp_letexception(_, _)
        | ExpressionDesc::Pexp_letmodule(_, _, _)
        | ExpressionDesc::Pexp_open(_, _, _) => ParenKind::Nothing,
        _ => {
            let opt_braces = parsetree_viewer::process_braces_attr(expr);
            match opt_braces {
                Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
                None => {
                    match &expr.pexp_desc {
                        ExpressionDesc::Pexp_constant(
                            Constant::Integer(x, _) | Constant::Float(x, _),
                        ) if expr.pexp_attributes.is_empty() && starts_with_minus(x) => {
                            ParenKind::Parenthesized
                        }
                        _ if parsetree_viewer::expr_is_await(expr) => ParenKind::Parenthesized,
                        ExpressionDesc::Pexp_ident(_)
                        | ExpressionDesc::Pexp_constant(_)
                        | ExpressionDesc::Pexp_field(_, _)
                        | ExpressionDesc::Pexp_construct(_, _)
                        | ExpressionDesc::Pexp_variant(_, _)
                        | ExpressionDesc::Pexp_array(_)
                        | ExpressionDesc::Pexp_pack(_)
                        | ExpressionDesc::Pexp_record { .. }
                        | ExpressionDesc::Pexp_extension(_)
                        | ExpressionDesc::Pexp_letmodule(_, _, _)
                        | ExpressionDesc::Pexp_letexception(_, _)
                        | ExpressionDesc::Pexp_open(_, _, _)
                        | ExpressionDesc::Pexp_sequence(_, _)
                        | ExpressionDesc::Pexp_let(_, _, _)
                        | ExpressionDesc::Pexp_tuple(_)
                            if expr.pexp_attributes.is_empty() =>
                        {
                            ParenKind::Nothing
                        }
                        // module(M: S) doesn't need parens
                        ExpressionDesc::Pexp_constraint(inner, typ)
                            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_))
                                && expr.pexp_attributes.is_empty() =>
                        {
                            ParenKind::Nothing
                        }
                        _ => ParenKind::Parenthesized,
                    }
                }
            }
        }
    }
}

/// Check if JSX child expression needs parentheses/braces.
pub fn jsx_child_expr(expr: &Expression) -> ParenKind {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_let(_, _, _)
        | ExpressionDesc::Pexp_sequence(_, _)
        | ExpressionDesc::Pexp_letexception(_, _)
        | ExpressionDesc::Pexp_letmodule(_, _, _)
        | ExpressionDesc::Pexp_open(_, _, _) => ParenKind::Nothing,
        _ => {
            let opt_braces = parsetree_viewer::process_braces_attr(expr);
            match opt_braces {
                Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
                None => {
                    match &expr.pexp_desc {
                        ExpressionDesc::Pexp_constant(
                            Constant::Integer(x, _) | Constant::Float(x, _),
                        ) if expr.pexp_attributes.is_empty() && starts_with_minus(x) => {
                            ParenKind::Parenthesized
                        }
                        _ if parsetree_viewer::expr_is_await(expr) => ParenKind::Parenthesized,
                        ExpressionDesc::Pexp_ident(_)
                        | ExpressionDesc::Pexp_constant(_)
                        | ExpressionDesc::Pexp_field(_, _)
                        | ExpressionDesc::Pexp_construct(_, _)
                        | ExpressionDesc::Pexp_variant(_, _)
                        | ExpressionDesc::Pexp_array(_)
                        | ExpressionDesc::Pexp_pack(_)
                        | ExpressionDesc::Pexp_record { .. }
                        | ExpressionDesc::Pexp_extension(_)
                        | ExpressionDesc::Pexp_letmodule(_, _, _)
                        | ExpressionDesc::Pexp_letexception(_, _)
                        | ExpressionDesc::Pexp_open(_, _, _)
                        | ExpressionDesc::Pexp_sequence(_, _)
                        | ExpressionDesc::Pexp_let(_, _, _)
                        | ExpressionDesc::Pexp_jsx_element(_)
                            if expr.pexp_attributes.is_empty() =>
                        {
                            ParenKind::Nothing
                        }
                        // module(M: S) doesn't need parens
                        ExpressionDesc::Pexp_constraint(inner, typ)
                            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_))
                                && expr.pexp_attributes.is_empty() =>
                        {
                            ParenKind::Nothing
                        }
                        ExpressionDesc::Pexp_jsx_element(_) => ParenKind::Nothing,
                        _ => ParenKind::Parenthesized,
                    }
                }
            }
        }
    }
}

/// Check if binary expression needs parentheses/braces.
pub fn binary_expr(arena: &crate::parse_arena::ParseArena, expr: &Expression) -> ParenKind {
    let opt_braces = parsetree_viewer::process_braces_attr(expr);
    match opt_braces {
        Some(attr) => ParenKind::Braced(attr.0.loc.clone()),
        None => {
            if !expr.pexp_attributes.is_empty() && parsetree_viewer::is_binary_expression(arena, expr) {
                ParenKind::Parenthesized
            } else {
                ParenKind::Nothing
            }
        }
    }
}

/// Check if module type functor return needs parentheses.
pub fn mod_type_functor_return(mod_type: &ModuleType) -> bool {
    matches!(&mod_type.pmty_desc, ModuleTypeDesc::Pmty_with(_, _))
}

/// Check if module type with operand needs parentheses.
pub fn mod_type_with_operand(mod_type: &ModuleType) -> bool {
    matches!(
        &mod_type.pmty_desc,
        ModuleTypeDesc::Pmty_functor(_, _, _) | ModuleTypeDesc::Pmty_with(_, _)
    )
}

/// Check if module expression functor constraint needs parentheses.
pub fn mod_expr_functor_constraint(mod_type: &ModuleType) -> bool {
    matches!(
        &mod_type.pmty_desc,
        ModuleTypeDesc::Pmty_functor(_, _, _) | ModuleTypeDesc::Pmty_with(_, _)
    )
}

/// Check if braced expression needs explicit braces.
pub fn braced_expr(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_constraint(inner, typ)
            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
        {
            false
        }
        ExpressionDesc::Pexp_constraint(_, _) => true,
        _ => false,
    }
}

/// Check if include module expression needs parentheses.
pub fn include_mod_expr(mod_expr: &ModuleExpr) -> bool {
    matches!(&mod_expr.pmod_desc, ModuleExprDesc::Pmod_constraint(_, _))
}

/// Check if module expression needs parentheses.
/// OCaml logic:
/// - Pmod_constraint(Pmod_structure, Pmty_signature([Psig_module])) -> false
/// - Pmod_constraint(_, Pmty_signature([Psig_module])) -> true
/// - Otherwise -> false
pub fn mod_expr_parens(mod_expr: &ModuleExpr) -> bool {
    match &mod_expr.pmod_desc {
        ModuleExprDesc::Pmod_constraint(inner, typ) => {
            // Check if typ is Pmty_signature with single Psig_module
            if let ModuleTypeDesc::Pmty_signature(items) = &typ.pmty_desc {
                if items.len() == 1 {
                    if let SignatureItemDesc::Psig_module(_) = &items[0].psig_desc {
                        // Sig is [Psig_module] - return false if inner is Pmod_structure, true otherwise
                        return !matches!(&inner.pmod_desc, ModuleExprDesc::Pmod_structure(_));
                    }
                }
            }
            // Not the special [Psig_module] case -> false
            false
        }
        _ => false,
    }
}

/// Check if arrow return type expression needs parentheses.
pub fn arrow_return_typ_expr(typ_expr: &CoreType) -> bool {
    matches!(&typ_expr.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. })
}

/// Check if pattern record row RHS needs parentheses.
pub fn pattern_record_row_rhs(pattern: &Pattern) -> bool {
    match &pattern.ppat_desc {
        PatternDesc::Ppat_constraint(inner, typ)
            if matches!(&inner.ppat_desc, PatternDesc::Ppat_unpack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
        {
            false
        }
        PatternDesc::Ppat_constraint(_, _) => true,
        _ => false,
    }
}
