//! Expression parsing for ReScript.
//!
//! This module contains the expression parsing logic, converting tokens
//! into expression AST nodes.

use crate::location::{Located, Location, Position};

use super::ast::*;
use super::core::{ExprContext, ast_helper, make_braces_attr, mknoloc, recover, with_loc};
use super::diagnostics::DiagnosticCategory;
use super::grammar;
use super::grammar::Grammar;
use super::longident::Longident;
use super::state::{Parser, ParserMode};
use super::token::Token;

// ============================================================================
// Constants
// ============================================================================

/// Get the string tag based on parser mode.
/// In typechecker mode, strings are tagged with "js" to indicate JS string literals.
/// In printer mode, strings have no tag.
fn get_string_tag(p: &Parser<'_>) -> Option<String> {
    if p.mode == ParserMode::ParseForTypeChecker {
        Some("js".to_string())
    } else {
        None
    }
}

/// Parse a constant value (number, string, char, etc).
pub fn parse_constant(p: &mut Parser<'_>) -> Constant {
    match &p.token {
        Token::Int { i, suffix } => {
            let value = i.clone();
            let s = *suffix;
            p.next();
            Constant::Integer(value, s)
        }
        Token::Float { f, suffix } => {
            let value = f.clone();
            let s = *suffix;
            p.next();
            Constant::Float(value, s)
        }
        Token::String(s) => {
            let value = s.clone();
            let tag = get_string_tag(p);
            p.next();
            Constant::String(value, tag)
        }
        Token::Codepoint { c, original } => {
            let value = *c;
            let _orig = original.clone();
            p.next();
            Constant::Char(value)
        }
        _ => {
            p.err(DiagnosticCategory::Message("Expected constant".to_string()));
            Constant::Integer("0".to_string(), None)
        }
    }
}

// ============================================================================
// Placeholder Transformation
// ============================================================================

/// Check if an expression is the underscore placeholder `_`
fn is_underscore_placeholder(expr: &Expression) -> bool {
    matches!(
        &expr.pexp_desc,
        ExpressionDesc::Pexp_ident(loc) if matches!(&loc.txt, Longident::Lident(s) if s == "_")
    )
}

/// Replace underscore placeholders with a reference to the given variable name
fn replace_underscores_in_expr(expr: Expression, var_name: &str) -> Expression {
    if is_underscore_placeholder(&expr) {
        let loc = expr.pexp_loc.clone();
        Expression {
            pexp_desc: ExpressionDesc::Pexp_ident(with_loc(
                Longident::Lident(var_name.to_string()),
                loc.clone(),
            )),
            pexp_loc: loc,
            pexp_attributes: vec![],
        }
    } else {
        expr
    }
}

/// Transform function application with underscore placeholders.
/// f(a, _, b) becomes: fun __x -> f(a, __x, b)
fn transform_placeholder_application(
    funct: Box<Expression>,
    args: Vec<(ArgLabel, Expression)>,
    partial: bool,
    loc: Location,
) -> Expression {
    // Check if any argument is an underscore placeholder
    let has_placeholder = args.iter().any(|(_, expr)| is_underscore_placeholder(expr));

    if !has_placeholder {
        // No placeholders, return normal Pexp_apply
        return Expression {
            pexp_desc: ExpressionDesc::Pexp_apply {
                funct,
                args,
                partial,
                transformed_jsx: false,
            },
            pexp_loc: loc,
            pexp_attributes: vec![],
        };
    }

    // Replace all underscores with __x
    let var_name = "__x";
    let transformed_args: Vec<(ArgLabel, Expression)> = args
        .into_iter()
        .map(|(label, expr)| (label, replace_underscores_in_expr(expr, var_name)))
        .collect();

    // Create the inner Pexp_apply
    let inner_apply = Expression {
        pexp_desc: ExpressionDesc::Pexp_apply {
            funct,
            args: transformed_args,
            partial,
            transformed_jsx: false,
        },
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    };

    // Create Pexp_fun wrapping the application
    let pattern = Pattern {
        ppat_desc: PatternDesc::Ppat_var(mknoloc(var_name.to_string())),
        ppat_loc: Location::none(),
        ppat_attributes: vec![],
    };

    Expression {
        pexp_desc: ExpressionDesc::Pexp_fun {
            arg_label: ArgLabel::Nolabel,
            default: None,
            lhs: pattern,
            rhs: Box::new(inner_apply),
            arity: Arity::Unknown,
            is_async: false,
        },
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

// ============================================================================
// Value and Constructor Parsing
// ============================================================================

/// Parse a value path after a dot (for field access).
/// This parses things like `Lexing.pos_fname` into a Longident.
fn parse_value_path_after_dot(p: &mut Parser<'_>) -> Located<Longident> {
    let start_pos = p.start_pos.clone();

    match &p.token {
        Token::Uident(name) => {
            let mut path = Longident::Lident(name.clone());
            p.next();

            // Continue parsing the path: Module.Module.field
            while p.token == Token::Dot {
                p.next();
                match &p.token {
                    Token::Uident(name) => {
                        path = Longident::Ldot(Box::new(path), name.clone());
                        p.next();
                    }
                    Token::Lident(name) => {
                        path = Longident::Ldot(Box::new(path), name.clone());
                        p.next();
                        break;
                    }
                    Token::String(name) => {
                        // Quoted field: Module."switch"
                        path = Longident::Ldot(Box::new(path), name.clone());
                        p.next();
                        break;
                    }
                    _ => break,
                }
            }

            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            with_loc(path, loc)
        }
        Token::Lident(name) => {
            let path = Longident::Lident(name.clone());
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            with_loc(path, loc)
        }
        Token::String(name) => {
            // Quoted field: ."switch"
            let path = Longident::Lident(name.clone());
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            with_loc(path, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected identifier after dot".to_string(),
            ));
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            with_loc(Longident::Lident("_".to_string()), loc)
        }
    }
}

/// Parse a value or constructor expression.
pub fn parse_value_or_constructor(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();

    match &p.token {
        Token::Uident(name) => {
            // Module path or constructor
            let mut path_parts = vec![name.clone()];
            p.next();

            // Collect the full path
            while p.token == Token::Dot {
                p.next();
                match &p.token {
                    Token::Uident(name) => {
                        path_parts.push(name.clone());
                        p.next();
                    }
                    Token::Lident(name) => {
                        path_parts.push(name.clone());
                        p.next();
                        break;
                    }
                    Token::String(name) => {
                        // Quoted identifiers (used for keywords): `Module.\"switch\"`
                        path_parts.push(name.clone());
                        p.next();
                        break;
                    }
                    _ => break,
                }
            }

            let lid = super::core::build_longident(&path_parts);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

            // Check if it's a constructor (all uppercase components)
            if path_parts
                .last()
                .map(|s| s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false))
                .unwrap_or(false)
            {
                // Constructor - check for argument
                let arg = parse_constructor_arg(p);
                let has_arg = arg.is_some();
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_construct(
                        with_loc(lid, loc.clone()),
                        arg.map(Box::new),
                    ),
                    pexp_loc: if has_arg {
                        p.mk_loc(&start_pos, &p.prev_end_pos)
                    } else {
                        loc
                    },
                    pexp_attributes: vec![],
                }
            } else {
                // Regular identifier
                ast_helper::make_ident(lid, loc)
            }
        }
        Token::Lident(name) => {
            let name = name.clone();
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            ast_helper::make_ident(Longident::Lident(name), loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected identifier".to_string(),
            ));
            recover::default_expr()
        }
    }
}

/// Parse an optional constructor argument.
fn parse_constructor_arg(p: &mut Parser<'_>) -> Option<Expression> {
    match &p.token {
        // If there's a newline before the `(`, it's not a constructor argument
        // but the start of a new statement
        Token::Lparen if !p.has_newline_before() => {
            let start_pos = p.start_pos.clone();
            p.next();
            if p.token == Token::Rparen {
                // Empty constructor args: Rgb()
                p.next();
                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                let lid = Longident::Lident("()".to_string());
                Some(Expression {
                    pexp_desc: ExpressionDesc::Pexp_construct(with_loc(lid, loc.clone()), None),
                    pexp_loc: loc,
                    pexp_attributes: vec![],
                })
            } else {
                let first_expr = parse_constrained_or_coerced_expr(p);
                if p.token == Token::Comma {
                    // Multiple arguments: Rgb(r, g, b) -> tuple
                    // OCaml includes the ( in the tuple location
                    let tuple_start = start_pos.clone();
                    let mut exprs = vec![first_expr];
                    while p.token == Token::Comma {
                        p.next();
                        if p.token == Token::Rparen {
                            break; // Trailing comma
                        }
                        exprs.push(parse_constrained_or_coerced_expr(p));
                    }
                    p.expect(Token::Rparen);
                    // Only create a tuple if there's more than one expression
                    // A single expression with trailing comma is NOT a tuple
                    if exprs.len() == 1 {
                        Some(exprs.into_iter().next().unwrap())
                    } else {
                        let loc = p.mk_loc(&tuple_start, &p.prev_end_pos);
                        Some(Expression {
                            pexp_desc: ExpressionDesc::Pexp_tuple(exprs),
                            pexp_loc: loc,
                            pexp_attributes: vec![],
                        })
                    }
                } else {
                    p.expect(Token::Rparen);
                    // If the single argument is already a tuple, wrap it in another
                    // single-element tuple. This distinguishes C((1,2)) from C(1,2).
                    // C((1,2)) = single tuple argument = Pexp_tuple([Pexp_tuple([1,2])])
                    // C(1,2) = multiple arguments = Pexp_tuple([1, 2])
                    if matches!(first_expr.pexp_desc, ExpressionDesc::Pexp_tuple(_)) {
                        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                        Some(Expression {
                            pexp_desc: ExpressionDesc::Pexp_tuple(vec![first_expr]),
                            pexp_loc: loc,
                            pexp_attributes: vec![],
                        })
                    } else {
                        Some(first_expr)
                    }
                }
            }
        }
        _ => None,
    }
}

// ============================================================================
// Main Expression Parsing
// ============================================================================

/// Parse an expression.
pub fn parse_expr(p: &mut Parser<'_>) -> Expression {
    parse_expr_with_context(p, ExprContext::Ordinary)
}

/// Parse an expression starting from an already-parsed operand.
/// Used when the caller has parsed an atomic expression and wants to continue.
pub fn parse_expr_with_operand(p: &mut Parser<'_>, operand: Expression) -> Expression {
    // Continue with primary parsing (function calls, field access, etc.)
    let expr = parse_primary_expr(p, operand, false);
    // Then binary and ternary
    let expr = parse_binary_expr(p, expr, 1, ExprContext::Ordinary);
    let expr = parse_ternary_expr(p, expr);
    // Handle coercion
    if p.token == Token::ColonGreaterThan {
        p.next();
        let typ = super::typ::parse_typ_expr(p);
        let loc = p.mk_loc(&expr.pexp_loc.loc_start, &typ.ptyp_loc.loc_end);
        Expression {
            pexp_desc: ExpressionDesc::Pexp_coerce(Box::new(expr), None, typ),
            pexp_loc: loc,
            pexp_attributes: vec![],
        }
    } else {
        expr
    }
}

/// Parse an expression with a specific context.
pub fn parse_expr_with_context(p: &mut Parser<'_>, context: ExprContext) -> Expression {
    let expr = parse_operand_expr(p, context);
    let expr = parse_binary_expr(p, expr, 1, context);
    let expr = parse_ternary_expr(p, expr);
    // Handle coercion: expr :> type
    if p.token == Token::ColonGreaterThan {
        p.next();
        let typ = super::typ::parse_typ_expr(p);
        let loc = p.mk_loc(&expr.pexp_loc.loc_start, &typ.ptyp_loc.loc_end);
        Expression {
            pexp_desc: ExpressionDesc::Pexp_coerce(Box::new(expr), None, typ),
            pexp_loc: loc,
            pexp_attributes: vec![],
        }
    } else {
        expr
    }
}

/// Parse a ternary expression (condition ? true_branch : false_branch).
pub fn parse_ternary_expr(p: &mut Parser<'_>, left_operand: Expression) -> Expression {
    if p.token == Token::Question {
        p.leave_breadcrumb(Grammar::Ternary);
        p.next();

        let true_branch = parse_expr_with_context(p, ExprContext::TernaryTrueBranch);
        p.expect(Token::Colon);
        let false_branch = parse_expr(p);

        p.eat_breadcrumb();

        let loc = p.mk_loc(
            &left_operand.pexp_loc.loc_start,
            &false_branch.pexp_loc.loc_end,
        );

        Expression {
            pexp_desc: ExpressionDesc::Pexp_ifthenelse(
                Box::new(left_operand),
                Box::new(true_branch),
                Some(Box::new(false_branch)),
            ),
            pexp_loc: loc,
            pexp_attributes: vec![super::core::ternary_attr()],
        }
    } else {
        left_operand
    }
}

/// Parse an operand expression (handles unary operators and primary expressions).
pub fn parse_operand_expr(p: &mut Parser<'_>, context: ExprContext) -> Expression {
    let start_pos = p.start_pos.clone();

    // Parse leading attributes first (e.g., @attr expr)
    let attrs = parse_attributes(p);

    // Handle unary operators
    // Track whether attrs were passed to arrow expression (so we don't attach them twice)
    let (expr, attrs_consumed) = match &p.token {
        Token::Plus
        | Token::PlusDot
        | Token::Minus
        | Token::MinusDot
        | Token::Bang
        | Token::Bnot
        | Token::Question => {
            let token = p.token.clone();
            p.next();
            let token_end = p.prev_end_pos.clone();
            let operand = parse_unary_expr(p);
            (
                super::core::make_unary_expr(p, start_pos.clone(), token_end, token, operand),
                false,
            )
        }
        Token::Lident(s) if s == "async" => {
            // async arrow function
            p.next();
            if super::core::is_es6_arrow_expression(p, context == ExprContext::TernaryTrueBranch) {
                // Pass attrs to arrow expression - they're consumed
                // OCaml: async function location starts AFTER async, not including it
                (
                    parse_es6_arrow_expression(
                        p,
                        true,
                        None, // Don't include "async" in the function location
                        attrs.clone(),
                        context,
                    ),
                    true,
                )
            } else {
                // Just a regular "async" identifier - continue with primary expr parsing
                // to handle function calls like async(x)
                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                let ident = ast_helper::make_ident(Longident::Lident("async".to_string()), loc);
                (parse_primary_expr(p, ident, false), false)
            }
        }
        Token::Await => (parse_await_expression(p), false),
        Token::Assert => {
            // Handle assert directly to preserve attribute location in expr location
            p.next();
            let arg = parse_expr(p);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            (
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_assert(Box::new(arg)),
                    pexp_loc: loc,
                    pexp_attributes: vec![],
                },
                false,
            )
        }
        _ => {
            let is_arrow = context != ExprContext::When
                && super::core::is_es6_arrow_expression(
                    p,
                    context == ExprContext::TernaryTrueBranch,
                );
            if is_arrow {
                // Pass attrs to arrow expression - they're consumed
                (
                    parse_es6_arrow_expression(p, false, None, attrs.clone(), context),
                    true,
                )
            } else {
                (parse_unary_expr(p), false)
            }
        }
    };

    // Attach attributes to the expression if not already consumed by arrow expression
    if attrs.is_empty() || attrs_consumed {
        expr
    } else {
        // Combine existing attributes with parsed attributes
        let mut combined_attrs = expr.pexp_attributes.clone();
        combined_attrs.extend(attrs);
        Expression {
            pexp_attributes: combined_attrs,
            ..expr
        }
    }
}

/// Parse a unary expression.
pub fn parse_unary_expr(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();

    // Parse any leading attributes: @attr expr
    let attrs = parse_attributes(p);

    let mut expr = match &p.token {
        Token::Plus
        | Token::PlusDot
        | Token::Minus
        | Token::MinusDot
        | Token::Bang
        | Token::Bnot
        | Token::Question => {
            let token = p.token.clone();
            p.next();
            let token_end = p.prev_end_pos.clone();
            let operand = parse_unary_expr(p);
            super::core::make_unary_expr(p, start_pos, token_end, token, operand)
        }
        _ => {
            let atomic = parse_atomic_expr(p);
            parse_primary_expr(p, atomic, false)
        }
    };

    // Attach attributes to the expression
    if !attrs.is_empty() {
        expr.pexp_attributes.extend(attrs);
    }

    expr
}

/// Parse an await expression.
pub fn parse_await_expression(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Await);
    // Parse with -> precedence to match OCaml behavior
    // This allows: await x->Promise.resolve to parse as await (x->Promise.resolve)
    let token_prec = Token::MinusGreater.precedence();
    let operand = parse_unary_expr(p);
    let expr = parse_binary_expr(p, operand, token_prec, ExprContext::Ordinary);
    let loc = p.mk_loc(&start_pos, &expr.pexp_loc.loc_end);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_await(Box::new(expr)),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse an ES6 arrow function expression.
pub fn parse_es6_arrow_expression(
    p: &mut Parser<'_>,
    is_async: bool,
    arrow_start_pos: Option<Position>,
    arrow_attrs: Attributes,
    context: ExprContext,
) -> Expression {
    let start_pos = arrow_start_pos.unwrap_or_else(|| p.start_pos.clone());
    p.leave_breadcrumb(Grammar::Es6ArrowExpr);

    // Parse parameters
    let parameters = parse_parameters(p);

    // Parse optional return type
    // We use parse_typ_expr_no_arrow to avoid confusing the return type
    // with the arrow body (e.g., (x): int => body shouldn't parse as int => body)
    let return_type = if p.token == Token::Colon {
        p.next();
        Some(super::typ::parse_typ_expr_no_arrow(p))
    } else {
        None
    };

    // Expect =>
    p.expect(Token::EqualGreater);

    // Parse body
    let body = parse_expr_with_context(p, context);
    let body = match return_type {
        Some(typ) => {
            let loc = p.mk_loc(&body.pexp_loc.loc_start, &typ.ptyp_loc.loc_end);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(body), typ),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        None => body,
    };

    p.eat_breadcrumb();
    let end_pos = p.prev_end_pos.clone();

    // Build the function expression from parameters
    let (type_params, term_params) = super::core::extract_fundef_params(parameters);

    // Fold the term parameters into nested functions
    // If there are type params but no term params, add an implicit unit param
    let arrow_expr = if term_params.is_empty() && type_params.is_some() {
        // Implicit unit parameter for type-only arrow
        let unit_loc = type_params.as_ref().map(|tp| p.mk_loc(&tp.start_pos, &end_pos)).unwrap();
        let unit_pat = Pattern {
            ppat_desc: PatternDesc::Ppat_construct(
                with_loc(Longident::Lident("()".to_string()), unit_loc.clone()),
                None,
            ),
            ppat_loc: unit_loc.clone(),
            ppat_attributes: vec![],
        };
        Expression {
            pexp_desc: ExpressionDesc::Pexp_fun {
                arg_label: ArgLabel::Nolabel,
                default: None,
                lhs: unit_pat,
                rhs: Box::new(body),
                arity: Arity::Unknown,
                is_async,
            },
            pexp_loc: unit_loc,
            pexp_attributes: vec![],
        }
    } else {
        // Compute arity for the outermost function
        let total_arity = term_params.len();
        term_params
            .into_iter()
            .rev()
            .enumerate()
            .fold(body, |acc, (i, param)| {
                let loc = p.mk_loc(&param.start_pos, &end_pos);
                // Apply parameter attributes to the pattern's ppat_attributes
                // This is where attributes like @as("open") on parameters like (@as("open") ~_open)
                // should end up, so the JSX PPX can access them via pattern.ppat_attributes
                let mut pat = param.pat;
                if !param.attrs.is_empty() {
                    let mut attrs = param.attrs;
                    attrs.extend(pat.ppat_attributes);
                    pat.ppat_attributes = attrs;
                }
                // Only the outermost function (last in fold, first in original list) gets arity
                // After reversing, index 0 in fold corresponds to the innermost function
                // The last iteration (i == total_arity - 1) is the outermost function
                let is_outermost = i == total_arity - 1;
                let arity = if is_outermost {
                    Arity::Full(total_arity)
                } else {
                    Arity::Unknown
                };
                // Only the outermost function gets is_async: true (OCaml behavior)
                let fn_is_async = is_outermost && is_async;
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_fun {
                        arg_label: param.label,
                        default: param.expr.map(Box::new),
                        lhs: pat,
                        rhs: Box::new(acc),
                        arity,
                        is_async: fn_is_async,
                    },
                    pexp_loc: loc,
                    pexp_attributes: vec![],
                }
            })
    };

    // Handle newtype parameters
    let arrow_expr = match type_params {
        Some(tp) => {
            let loc = p.mk_loc(&tp.start_pos, &end_pos);
            tp.locs
                .into_iter()
                .rev()
                .fold(arrow_expr, |acc, name| Expression {
                    pexp_desc: ExpressionDesc::Pexp_newtype(name, Box::new(acc)),
                    pexp_loc: loc.clone(),
                    pexp_attributes: tp.attrs.clone(),
                })
        }
        None => arrow_expr,
    };

    Expression {
        pexp_loc: p.mk_loc(&start_pos, &arrow_expr.pexp_loc.loc_end),
        pexp_attributes: arrow_attrs,
        ..arrow_expr
    }
}

/// Parse function parameters.
fn parse_parameters(p: &mut Parser<'_>) -> Vec<super::core::FundefParameter> {
    let mut params = vec![];

    match &p.token {
        Token::Lparen => {
            let start_pos = p.start_pos.clone();
            p.next();

            // Skip optional dot (uncurried marker)
            let had_dot = p.optional(&Token::Dot);

            if p.token == Token::Rparen {
                // Unit parameter: () or (.)
                let end_pos = p.end_pos.clone();
                p.next();
                let loc = p.mk_loc(&start_pos, &end_pos);
                let unit_pat = Pattern {
                    ppat_desc: PatternDesc::Ppat_construct(
                        with_loc(Longident::Lident("()".to_string()), loc.clone()),
                        None,
                    ),
                    ppat_loc: loc.clone(),
                    ppat_attributes: vec![],
                };
                params.push(super::core::FundefParameter::Term(
                    super::core::FundefTermParam {
                        attrs: vec![],
                        label: ArgLabel::Nolabel,
                        expr: None,
                        pat: unit_pat,
                        start_pos,
                    },
                ));
            } else if had_dot {
                // We had a dot but there are more tokens - parse parameters
                while p.token != Token::Rparen && p.token != Token::Eof {
                    if let Some(param) = parse_parameter(p) {
                        params.push(param);
                    }
                    if !p.optional(&Token::Comma) {
                        break;
                    }
                }
                p.expect(Token::Rparen);
            } else {
                while p.token != Token::Rparen && p.token != Token::Eof {
                    if let Some(param) = parse_parameter(p) {
                        params.push(param);
                    }
                    if !p.optional(&Token::Comma) {
                        break;
                    }
                }
                p.expect(Token::Rparen);
            }
        }
        Token::Lident(_) | Token::Underscore => {
            // Single parameter without parens
            if let Some(param) = parse_parameter(p) {
                params.push(param);
            }
        }
        _ => {}
    }

    params
}

/// Parse a single function parameter.
fn parse_parameter(p: &mut Parser<'_>) -> Option<super::core::FundefParameter> {
    let start_pos = p.start_pos.clone();

    // Skip optional dot (uncurried marker, ignored now)
    p.optional(&Token::Dot);

    // Parse attributes
    let attrs = parse_attributes(p);

    // Check for type parameter
    if p.token == Token::Typ {
        p.next();
        let lidents = parse_lident_list(p);
        return Some(super::core::FundefParameter::Type(
            super::core::FundefTypeParam {
                attrs,
                locs: lidents,
                start_pos,
            },
        ));
    }

    // Check for labeled parameter
    if p.token == Token::Tilde {
        // Capture tilde position - OCaml includes ~ in the pattern var location but NOT label location
        let tilde_pos = p.start_pos.clone();
        p.next();
        let (lbl_name, lbl_loc) = parse_lident(p);
        // OCaml: label location is just the name, not including ~
        let lbl_located = Located::new(lbl_name.clone(), lbl_loc);
        // OCaml: pattern var location includes ~ (from tilde_pos to end of name)
        let name_loc_with_tilde = p.mk_loc(&tilde_pos, &p.prev_end_pos);
        let lbl = match &p.token {
            Token::Comma | Token::Equal | Token::Rparen | Token::EqualGreater => {
                // Just ~name
                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                let pat = ast_helper::make_var_pat(lbl_name.clone(), loc);
                (ArgLabel::Labelled(lbl_located), pat, None)
            }
            Token::Colon => {
                // ~name: type
                // OCaml: var_pat location includes ~ (from tilde to end of name)
                p.next();
                let typ = super::typ::parse_typ_expr(p);
                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                let var_pat = ast_helper::make_var_pat(lbl_name.clone(), name_loc_with_tilde.clone());
                let pat = Pattern {
                    ppat_desc: PatternDesc::Ppat_constraint(Box::new(var_pat), typ),
                    ppat_loc: loc,
                    ppat_attributes: vec![],
                };
                (ArgLabel::Labelled(lbl_located), pat, None)
            }
            Token::As => {
                // ~name as pattern
                p.next();
                let pat = super::pattern::parse_constrained_pattern(p);
                (ArgLabel::Labelled(lbl_located), pat, None)
            }
            _ => {
                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                let pat = ast_helper::make_var_pat(lbl_name.clone(), loc);
                (ArgLabel::Labelled(lbl_located), pat, None)
            }
        };

        let (label, pat, default) = lbl;

        // Check for default value
        let (label, pat, default) = if p.token == Token::Equal {
            p.next();
            let label = match label {
                ArgLabel::Labelled(name) => ArgLabel::Optional(name),
                other => other,
            };
            if p.token == Token::Question {
                p.next();
                (label, pat, None)
            } else {
                let expr = parse_expr(p);
                // Check for type annotation after default: ~name = default : type
                // The type constraint goes on the expression, not the pattern
                let expr = if p.token == Token::Colon {
                    p.next();
                    let typ = super::typ::parse_typ_expr(p);
                    let loc = p.mk_loc(&expr.pexp_loc.loc_start, &typ.ptyp_loc.loc_end);
                    Expression {
                        pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(expr), typ),
                        pexp_loc: loc,
                        pexp_attributes: vec![],
                    }
                } else {
                    expr
                };
                (label, pat, Some(expr))
            }
        } else {
            (label, pat, default)
        };

        return Some(super::core::FundefParameter::Term(
            super::core::FundefTermParam {
                attrs,
                label,
                expr: default,
                pat,
                start_pos,
            },
        ));
    }

    // Regular pattern parameter
    if grammar::is_pattern_start(&p.token) {
        let pat = super::pattern::parse_pattern(p);
        // Handle constraint inline to allow full arrow types (unlike switch cases)
        // For function parameters like `(x: int => int)`, we want to parse
        // the full arrow type, but `parse_constrained_pattern` uses
        // `parse_typ_expr_no_arrow` which is correct for switch cases but not here.
        let pat = if p.token == Token::Colon {
            p.next();
            let typ = super::typ::parse_typ_expr(p);
            let loc = p.mk_loc(&pat.ppat_loc.loc_start, &typ.ptyp_loc.loc_end);
            Pattern {
                ppat_desc: PatternDesc::Ppat_constraint(Box::new(pat), typ),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        } else {
            pat
        };
        return Some(super::core::FundefParameter::Term(
            super::core::FundefTermParam {
                attrs,
                label: ArgLabel::Nolabel,
                expr: None,
                pat,
                start_pos,
            },
        ));
    }

    None
}

/// Parse a list of lowercase identifiers.
fn parse_lident_list(p: &mut Parser<'_>) -> Vec<Located<String>> {
    let mut lidents = vec![];
    while let Token::Lident(name) = &p.token {
        let name = name.clone();
        let loc = p.mk_loc(&p.start_pos, &p.end_pos);
        p.next();
        lidents.push(with_loc(name, loc));
    }
    lidents
}

/// Parse a lowercase identifier.
fn parse_lident(p: &mut Parser<'_>) -> (String, Location) {
    match &p.token {
        Token::Lident(name) => {
            let name = name.clone();
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            (name, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected lowercase identifier".to_string(),
            ));
            ("_".to_string(), Location::none())
        }
    }
}

/// Parse attributes (including doc comments which become res.doc attributes).
fn parse_attributes(p: &mut Parser<'_>) -> Attributes {
    let mut attrs = vec![];
    loop {
        match &p.token {
            Token::At => {
                if let Some(attr) = parse_attribute(p) {
                    attrs.push(attr);
                }
            }
            Token::DocComment { loc, content } => {
                let loc = loc.clone();
                let content = content.clone();
                p.next();
                attrs.push(super::core::doc_comment_to_attribute(loc, content));
            }
            _ => break,
        }
    }
    attrs
}

/// Parse a single attribute.
fn parse_attribute(p: &mut Parser<'_>) -> Option<Attribute> {
    if p.token != Token::At {
        return None;
    }
    // Capture position before @ so attribute location includes it
    let start_pos = p.start_pos.clone();
    p.next();

    let attr_id = parse_attribute_id(p, start_pos);

    // Only parse Lparen as payload if it's immediately adjacent to the attribute ID
    // (no whitespace between them). e.g., @attr(payload) vs @attr (expression)
    let is_adjacent = p.start_pos.cnum == p.prev_end_pos.cnum;
    let payload = if p.token == Token::Lparen && is_adjacent {
        p.next();
        let payload = parse_payload(p);
        p.expect(Token::Rparen);
        payload
    } else {
        Payload::PStr(vec![])
    };

    Some((attr_id, payload))
}

/// Parse an attribute identifier.
/// Attribute identifiers can be regular identifiers or certain keywords like `as`, `type`, etc.
/// The `start_pos` parameter should be the position BEFORE the `@` was consumed.
fn parse_attribute_id(p: &mut Parser<'_>, start_pos: Position) -> Located<String> {
    let mut parts = vec![];

    loop {
        let name = match &p.token {
            Token::Lident(name) | Token::Uident(name) => Some(name.clone()),
            // Keywords that can be used as attribute names
            Token::As => Some("as".to_string()),
            Token::Typ => Some("type".to_string()),
            Token::Module => Some("module".to_string()),
            Token::Open => Some("open".to_string()),
            Token::Private => Some("private".to_string()),
            Token::External => Some("external".to_string()),
            Token::Exception => Some("exception".to_string()),
            Token::Include => Some("include".to_string()),
            Token::Constraint => Some("constraint".to_string()),
            Token::Let { .. } => Some("let".to_string()),
            Token::Rec => Some("rec".to_string()),
            Token::And => Some("and".to_string()),
            Token::If => Some("if".to_string()),
            Token::Else => Some("else".to_string()),
            Token::Switch => Some("switch".to_string()),
            Token::Try => Some("try".to_string()),
            Token::For => Some("for".to_string()),
            Token::In => Some("in".to_string()),
            Token::While => Some("while".to_string()),
            Token::True => Some("true".to_string()),
            Token::False => Some("false".to_string()),
            Token::Assert => Some("assert".to_string()),
            Token::Mutable => Some("mutable".to_string()),
            Token::Await => Some("await".to_string()),
            _ => None,
        };
        match name {
            Some(n) => {
                parts.push(n);
                p.next();
                if p.token == Token::Dot {
                    p.next();
                } else {
                    break;
                }
            }
            None => break,
        }
    }

    let name = parts.join(".");
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
    with_loc(name, loc)
}

/// Parse an attribute payload.
/// Handles simple payloads like `("string")` or `(expression)`.
fn parse_payload(p: &mut Parser<'_>) -> Payload {
    // Simple case: just a string literal like @as("foo")
    if let Token::String(s) = &p.token {
        let s = s.clone();
        let tag = get_string_tag(p);
        let start_pos = p.start_pos.clone();
        p.next();
        // Create a structure item with the string expression
        let str_expr = Expression {
            pexp_desc: ExpressionDesc::Pexp_constant(Constant::String(s, tag)),
            pexp_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
            pexp_attributes: vec![],
        };
        return Payload::PStr(vec![StructureItem {
            pstr_desc: StructureItemDesc::Pstr_eval(str_expr, vec![]),
            pstr_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
        }]);
    }

    // For other payloads, skip them for now while preserving nesting
    let mut depth = 1;
    while depth > 0 && p.token != Token::Eof {
        match &p.token {
            Token::Lparen | Token::Lbrace | Token::Lbracket => depth += 1,
            Token::Rparen | Token::Rbrace | Token::Rbracket => depth -= 1,
            _ => {}
        }
        if depth > 0 {
            p.next();
        }
    }
    Payload::PStr(vec![])
}

// ============================================================================
// Binary Expression Parsing
// ============================================================================

/// Parse a binary expression with precedence climbing.
pub fn parse_binary_expr(
    p: &mut Parser<'_>,
    left: Expression,
    min_prec: i32,
    context: ExprContext,
) -> Expression {
    let mut left = left;

    loop {
        // Skip `|` as binary operator in switch case RHS - it's the case delimiter
        if context == ExprContext::SwitchCaseRhs && p.token == Token::Bar {
            break;
        }

        // Disambiguate `<` between binary operator and JSX start.
        // If `<` is immediately followed by JSX tokens (`<div`, `</div`, `<>`),
        // treat it as the start of a new JSX expression, not as a binary operator.
        if p.token == Token::LessThan && is_jsx_start_after_less_than(p) {
            break;
        }

        // Disambiguate `-`, `-.`, `<`, and `%` between binary and unary/new expression.
        // When on a new line AND not surrounded by whitespace on both sides,
        // these tokens start a new expression rather than being binary operators.
        // Example:
        //   x
        //   -10  <- unary minus on new expression
        // vs:
        //   x - 10  <- binary subtraction
        if matches!(
            p.token,
            Token::Minus | Token::MinusDot | Token::LessThan | Token::Percent
        ) && !p.is_binary_op()
            && p.has_newline_before()
        {
            break;
        }

        // If a line starts with an extension (`%ext`) treat it as a new expression
        // instead of a binary operator continuation.
        if p.token == Token::Percent && p.prev_end_pos.line < p.start_pos.line {
            let looks_like_extension = p.lookahead(|state| {
                let percent_end = state.end_pos.clone();
                state.next();
                let adjacent = state.start_pos.cnum == percent_end.cnum;
                let is_ident = matches!(state.token, Token::Lident(_) | Token::Uident(_))
                    || state.token.is_keyword();
                adjacent && is_ident
            });
            if looks_like_extension {
                break;
            }
        }

        let (token, prec) = match get_operator_precedence(&p.token) {
            Some((t, prec)) if prec >= min_prec => (t, prec),
            _ => break,
        };

        let op_start = p.start_pos.clone();
        p.next();
        let op_end = p.prev_end_pos.clone();

        // Handle special case for assignment
        if token == Token::ColonEqual {
            let right = parse_expr_with_context(p, context);
            let loc = p.mk_loc(&left.pexp_loc.loc_start, &right.pexp_loc.loc_end);
            left = ast_helper::make_apply(
                super::core::make_infix_operator(p, Token::ColonEqual, op_start, op_end),
                vec![(ArgLabel::Nolabel, left), (ArgLabel::Nolabel, right)],
                loc,
            );
            continue;
        }

        let right = parse_operand_expr(p, context);

        // Right-associative handling for ** and =>
        let next_prec = if is_right_associative(&token) {
            prec
        } else {
            prec + 1
        };
        let right = parse_binary_expr(p, right, next_prec, context);

        let loc = p.mk_loc(&left.pexp_loc.loc_start, &right.pexp_loc.loc_end);
        let op_loc = p.mk_loc(&op_start, &op_end);

        // Build application expression
        let op_expr = ast_helper::make_ident(Longident::Lident(token.to_string()), op_loc);
        left = ast_helper::make_apply(
            op_expr,
            vec![(ArgLabel::Nolabel, left), (ArgLabel::Nolabel, right)],
            loc,
        );
    }

    left
}

fn is_jsx_start_after_less_than(p: &mut Parser<'_>) -> bool {
    p.lookahead(|state| {
        state.next();
        let is_adjacent = state.start_pos.cnum == state.prev_end_pos.cnum;
        is_adjacent
            && matches!(
                state.token,
                Token::Lident(_)
                    | Token::Uident(_)
                    | Token::Forwardslash
                    | Token::GreaterThan
            )
    })
}

/// Get the precedence of an operator token.
fn get_operator_precedence(token: &Token) -> Option<(Token, i32)> {
    let prec = match token {
        Token::HashEqual | Token::ColonEqual => 1,
        Token::Lor => 2,
        Token::Land => 3,
        Token::Bor => 4,
        Token::Bxor => 5,
        Token::Band => 6,
        Token::Equal
        | Token::EqualEqual
        | Token::EqualEqualEqual
        | Token::LessThan
        | Token::GreaterThan
        | Token::BangEqual
        | Token::BangEqualEqual
        | Token::LessEqual
        | Token::GreaterEqual => 7,
        Token::LeftShift | Token::RightShift | Token::RightShiftUnsigned => 8,
        Token::Plus | Token::PlusDot | Token::Minus | Token::MinusDot | Token::PlusPlus => 9,
        Token::Asterisk
        | Token::AsteriskDot
        | Token::Forwardslash
        | Token::ForwardslashDot
        | Token::Percent => 10,
        Token::Exponentiation => 11,
        Token::MinusGreater => 12, // Pipe operator - kept as infix, not desugared
        _ => return None,
    };
    Some((token.clone(), prec))
}

/// Check if an operator is right-associative.
fn is_right_associative(token: &Token) -> bool {
    matches!(token, Token::Exponentiation)
}

// ============================================================================
// Atomic Expressions
// ============================================================================

/// Parse an atomic expression.
pub fn parse_atomic_expr(p: &mut Parser<'_>) -> Expression {
    p.leave_breadcrumb(Grammar::ExprOperand);
    let start_pos = p.start_pos.clone();

    let expr = match &p.token {
        Token::True | Token::False => {
            let is_true = p.token == Token::True;
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            let lid = Longident::Lident(if is_true { "true" } else { "false" }.to_string());
            Expression {
                pexp_desc: ExpressionDesc::Pexp_construct(with_loc(lid, loc.clone()), None),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        Token::Int { .. } | Token::String(_) | Token::Float { .. } | Token::Codepoint { .. } => {
            let c = parse_constant(p);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            ast_helper::make_constant(c, loc)
        }
        Token::Regex { pattern, flags } => {
            let pattern = pattern.clone();
            let flags = flags.clone();
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

            // OCaml format: single string with slashes: "/" + pattern + "/" + flags
            let regex_str = format!("/{}/{}", pattern, flags);
            let tag = get_string_tag(p);
            let str_expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_constant(Constant::String(regex_str, tag)),
                pexp_loc: loc.clone(),
                pexp_attributes: vec![],
            };
            let str_item = StructureItem {
                pstr_desc: StructureItemDesc::Pstr_eval(str_expr, vec![]),
                pstr_loc: loc.clone(),
            };

            Expression {
                pexp_desc: ExpressionDesc::Pexp_extension((
                    with_loc("re".to_string(), loc.clone()),
                    Payload::PStr(vec![str_item]),
                )),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        Token::Uident(_) | Token::Lident(_) => parse_value_or_constructor(p),
        Token::Lparen => {
            p.next();
            if p.token == Token::Rparen {
                // Unit: ()
                p.next();
                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                let lid = Longident::Lident("()".to_string());
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_construct(with_loc(lid, loc.clone()), None),
                    pexp_loc: loc,
                    pexp_attributes: vec![],
                }
            } else {
                // Parenthesized expression or tuple
                let expr = parse_constrained_or_coerced_expr(p);
                if p.token == Token::Comma {
                    // Tuple
                    p.next();
                    parse_tuple_expr(p, start_pos, expr)
                } else {
                    p.expect(Token::Rparen);
                    expr
                }
            }
        }
        Token::Lbracket => parse_array_expr(p),
        Token::Lbrace => parse_braced_or_record_expr(p),
        Token::List => {
            p.next();
            parse_list_expr(p, start_pos)
        }
        Token::Dict => {
            p.next();
            parse_dict_expr(p, start_pos)
        }
        Token::If => parse_if_expr(p),
        Token::Switch => parse_switch_expr(p),
        Token::While => parse_while_expr(p),
        Token::For => parse_for_expr(p),
        Token::Try => parse_try_expr(p),
        Token::Assert => {
            p.next();
            // Assert takes a full expression, not just an operand
            let arg = parse_expr(p);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_assert(Box::new(arg)),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        Token::Percent => {
            // Extension expression: %ext, %ext.with.dots, %ext(payload)
            // OCaml includes the % in the extension name location
            let id_start = p.start_pos.clone();
            p.next();
            let mut parts: Vec<String> = vec![];

            loop {
                match &p.token {
                    Token::Lident(name) | Token::Uident(name) => {
                        parts.push(name.clone());
                        p.next();
                    }
                    _ if p.token.is_keyword() => {
                        parts.push(p.token.to_string());
                        p.next();
                    }
                    _ => break,
                }

                if p.token == Token::Dot {
                    p.next();
                    continue;
                }
                break;
            }

            let name = if parts.is_empty() {
                p.err(DiagnosticCategory::Message(
                    "Expected extension name after %".to_string(),
                ));
                with_loc("error".to_string(), Location::none())
            } else {
                let id = parts.join(".");
                with_loc(id, p.mk_loc(&id_start, &p.prev_end_pos))
            };

            let payload = if p.token == Token::Lparen {
                p.next();
                let payload = super::module::parse_payload(p);
                p.expect(Token::Rparen);
                payload
            } else {
                Payload::PStr(vec![])
            };

            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_extension((name, payload)),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        Token::LessThan => parse_jsx(p),
        Token::Backtick => parse_template_literal(p),
        Token::TemplateTail { .. } => parse_template_literal(p),
        Token::TemplatePart { .. } => parse_template_literal(p),
        Token::Hash => {
            // Polyvariant: #tag or #tag(arg)
            p.next();
            let tag = match &p.token {
                Token::Lident(name) | Token::Uident(name) => {
                    let name = name.clone();
                    p.next();
                    name
                }
                Token::Int { i, .. } => {
                    let tag = i.clone();
                    p.next();
                    tag
                }
                Token::String(s) => {
                    let tag = s.clone();
                    p.next();
                    tag
                }
                _ => {
                    p.err(DiagnosticCategory::Message(
                        "Expected variant tag after #".to_string(),
                    ));
                    "error".to_string()
                }
            };

            // Check for argument
            let arg = if p.token == Token::Lparen && !p.has_newline_before() {
                let lparen_pos = p.start_pos.clone();
                p.next();
                // Handle empty parens: #tag() is a variant with unit argument
                if p.token == Token::Rparen {
                    let unit_loc = p.mk_loc(&p.start_pos, &p.start_pos);
                    p.next();
                    Some(Box::new(Expression {
                        pexp_desc: ExpressionDesc::Pexp_construct(
                            mknoloc(Longident::Lident("()".to_string())),
                            None,
                        ),
                        pexp_loc: unit_loc,
                        pexp_attributes: vec![],
                    }))
                } else {
                    // Parse first expression
                    let first_expr = parse_expr(p);
                    // Check for more comma-separated expressions (tuple)
                    if p.token == Token::Comma {
                        // OCaml includes the ( in the tuple location
                        let tuple_start = lparen_pos.clone();
                        let mut exprs = vec![first_expr];
                        while p.token == Token::Comma {
                            p.next();
                            if p.token == Token::Rparen {
                                // Trailing comma
                                break;
                            }
                            exprs.push(parse_expr(p));
                        }
                        p.expect(Token::Rparen);
                        let rparen_pos = p.prev_end_pos.clone();
                        let paren_loc = p.mk_loc(&lparen_pos, &rparen_pos);

                        // OCaml wraps polymorphic variant args differently based on mode:
                        // - For multiple args #a(1, 2), both modes create Pexp_tuple([1, 2])
                        // - For single tuple arg #a((1, 2)), printer mode wraps in extra tuple
                        if exprs.len() == 1 {
                            // Single expression (possibly with trailing comma)
                            let single = exprs.into_iter().next().unwrap();
                            // Check if it's a tuple that needs extra wrapping in printer mode
                            // This handles #a((1, 2)) case where inner parens create a tuple
                            if p.mode != ParserMode::ParseForTypeChecker {
                                if matches!(single.pexp_desc, ExpressionDesc::Pexp_tuple(_)) {
                                    // Wrap the tuple in another tuple for printer mode
                                    Some(Box::new(Expression {
                                        pexp_desc: ExpressionDesc::Pexp_tuple(vec![single]),
                                        pexp_loc: paren_loc,
                                        pexp_attributes: vec![],
                                    }))
                                } else {
                                    Some(Box::new(single))
                                }
                            } else {
                                Some(Box::new(single))
                            }
                        } else {
                            // Multiple expressions - just wrap in a single tuple (no extra wrapping)
                            let tuple_loc = p.mk_loc(&tuple_start, &rparen_pos);
                            Some(Box::new(Expression {
                                pexp_desc: ExpressionDesc::Pexp_tuple(exprs),
                                pexp_loc: tuple_loc,
                                pexp_attributes: vec![],
                            }))
                        }
                    } else {
                        p.expect(Token::Rparen);
                        let rparen_pos = p.prev_end_pos.clone();
                        // Single expression without comma - check if it's a tuple that needs wrapping
                        // This handles #a((1, 2)) case - inner parens create tuple, wrap for printer
                        if p.mode != ParserMode::ParseForTypeChecker {
                            if matches!(first_expr.pexp_desc, ExpressionDesc::Pexp_tuple(_)) {
                                let paren_loc = p.mk_loc(&lparen_pos, &rparen_pos);
                                Some(Box::new(Expression {
                                    pexp_desc: ExpressionDesc::Pexp_tuple(vec![first_expr]),
                                    pexp_loc: paren_loc,
                                    pexp_attributes: vec![],
                                }))
                            } else {
                                Some(Box::new(first_expr))
                            }
                        } else {
                            Some(Box::new(first_expr))
                        }
                    }
                }
            } else {
                None
            };

            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_variant(tag, arg),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        Token::Module => {
            // First-class module: module(Expr) or module(Expr: Type)
            p.next();
            parse_first_class_module_expr(p, start_pos)
        }
        Token::Underscore => {
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            ast_helper::make_ident(Longident::Lident("_".to_string()), loc)
        }
        Token::Eof => {
            p.err(DiagnosticCategory::Message(
                "Unexpected end of file".to_string(),
            ));
            recover::default_expr()
        }
        _ => {
            p.err(DiagnosticCategory::Message(format!(
                "Unexpected token: {:?}",
                p.token
            )));
            // Advance past the unexpected token to prevent infinite loops
            p.next();
            recover::default_expr()
        }
    };

    p.eat_breadcrumb();
    expr
}

/// Parse a constrained or coerced expression.
pub fn parse_constrained_or_coerced_expr(p: &mut Parser<'_>) -> Expression {
    let expr = parse_expr(p);

    if p.token == Token::Colon {
        p.next();
        let typ = super::typ::parse_typ_expr(p);
        // Check for coercion: (expr : type :> type)
        if p.token == Token::ColonGreaterThan {
            p.next();
            let target_typ = super::typ::parse_typ_expr(p);
            let loc = p.mk_loc(&expr.pexp_loc.loc_start, &target_typ.ptyp_loc.loc_end);
            // OCaml represents (x : t :> int) as Pexp_coerce(Pexp_constraint(x, t), int)
            // The constraint is nested inside the expression, not as the optional middle type
            let constraint_expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(expr), typ),
                pexp_loc: loc.clone(),
                pexp_attributes: vec![],
            };
            Expression {
                pexp_desc: ExpressionDesc::Pexp_coerce(Box::new(constraint_expr), None, target_typ),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        } else {
            let loc = p.mk_loc(&expr.pexp_loc.loc_start, &typ.ptyp_loc.loc_end);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(expr), typ),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
    } else if p.token == Token::ColonGreaterThan {
        // Direct coercion: expr :> type
        p.next();
        let typ = super::typ::parse_typ_expr(p);
        let loc = p.mk_loc(&expr.pexp_loc.loc_start, &typ.ptyp_loc.loc_end);
        Expression {
            pexp_desc: ExpressionDesc::Pexp_coerce(Box::new(expr), None, typ),
            pexp_loc: loc,
            pexp_attributes: vec![],
        }
    } else {
        expr
    }
}

/// Parse an expression with optional type constraint for function arguments.
/// Used in labeled argument contexts like `~compare=?intCompare: (int, int) => int`.
fn parse_constrained_expr_in_arg(p: &mut Parser<'_>) -> Expression {
    parse_constrained_or_coerced_expr(p)
}

// ============================================================================
// Primary Expression Parsing
// ============================================================================

/// Parse a primary expression (handles field access, method calls, array indexing).
pub fn parse_primary_expr(p: &mut Parser<'_>, operand: Expression, no_call: bool) -> Expression {
    let mut expr = operand;

    loop {
        match &p.token {
            Token::Dot => {
                p.next();
                match &p.token {
                    Token::Lident(name) => {
                        let name = name.clone();
                        let name_loc = p.mk_loc(&p.start_pos, &p.end_pos);
                        p.next();

                        // Check for field assignment: expr.field = value
                        if p.token == Token::Equal {
                            p.next();
                            let value = parse_expr(p);
                            let loc = p.mk_loc(&expr.pexp_loc.loc_start, &p.prev_end_pos);
                            expr = Expression {
                                pexp_desc: ExpressionDesc::Pexp_setfield(
                                    Box::new(expr),
                                    with_loc(Longident::Lident(name), name_loc),
                                    Box::new(value),
                                ),
                                pexp_loc: loc,
                                pexp_attributes: vec![],
                            };
                        } else {
                            let loc = p.mk_loc(&expr.pexp_loc.loc_start, &p.prev_end_pos);
                            expr = Expression {
                                pexp_desc: ExpressionDesc::Pexp_field(
                                    Box::new(expr),
                                    with_loc(Longident::Lident(name), name_loc),
                                ),
                                pexp_loc: loc,
                                pexp_attributes: vec![],
                            };
                        }
                    }
                    Token::String(name) => {
                        // Quoted field access: expr."switch"
                        let name = name.clone();
                        let name_loc = p.mk_loc(&p.start_pos, &p.end_pos);
                        p.next();

                        // Check for field assignment: expr."field" = value
                        if p.token == Token::Equal {
                            p.next();
                            let value = parse_expr(p);
                            let loc = p.mk_loc(&expr.pexp_loc.loc_start, &p.prev_end_pos);
                            expr = Expression {
                                pexp_desc: ExpressionDesc::Pexp_setfield(
                                    Box::new(expr),
                                    with_loc(Longident::Lident(name), name_loc),
                                    Box::new(value),
                                ),
                                pexp_loc: loc,
                                pexp_attributes: vec![],
                            };
                        } else {
                            let loc = p.mk_loc(&expr.pexp_loc.loc_start, &p.prev_end_pos);
                            expr = Expression {
                                pexp_desc: ExpressionDesc::Pexp_field(
                                    Box::new(expr),
                                    with_loc(Longident::Lident(name), name_loc),
                                ),
                                pexp_loc: loc,
                                pexp_attributes: vec![],
                            };
                        }
                    }
                    Token::Uident(_) => {
                        // Module-qualified field access: expr.Module.field
                        // Parse the path (e.g., Lexing.pos_fname) and create Pexp_field
                        let field_path = parse_value_path_after_dot(p);
                        let loc = p.mk_loc(&expr.pexp_loc.loc_start, &field_path.loc.loc_end);
                        expr = Expression {
                            pexp_desc: ExpressionDesc::Pexp_field(Box::new(expr), field_path),
                            pexp_loc: loc,
                            pexp_attributes: vec![],
                        };
                    }
                    _ => break,
                }
            }
            Token::Lparen if !no_call => {
                // Function call - but only if on the same line
                // A ( on a new line should not be treated as a function call
                if p.prev_end_pos.line < p.start_pos.line {
                    break;
                }
                let (args, partial) = parse_call_args(p);
                let loc = p.mk_loc(&expr.pexp_loc.loc_start, &p.prev_end_pos);
                // Apply placeholder transformation: f(a, _, b) => fun __x -> f(a, __x, b)
                expr = transform_placeholder_application(Box::new(expr), args, partial, loc);
            }
            Token::Lbracket => {
                // If [ is on a new line, it's an array literal (new expression), not indexing.
                // This is needed for automatic semicolon insertion (ASI) in sequences like:
                //   let x = foo
                //   [1, 2]  // This is a new array expression, not foo[1, 2]
                if p.has_newline_before() {
                    break;
                }
                // Array indexing or assignment
                let start = expr.pexp_loc.loc_start.clone();
                // Capture the bracket location for Array.set/get identifiers
                // OCaml uses the location from [ to ] for synthesized Array.set/get
                let lbracket_start = p.start_pos.clone();
                p.next();
                let index = parse_constrained_or_coerced_expr(p);
                p.expect(Token::Rbracket);
                let rbracket_end = p.prev_end_pos.clone();
                let bracket_loc = p.mk_loc(&lbracket_start, &rbracket_end);

                if p.token == Token::Equal {
                    let equal_start = p.start_pos.clone();
                    p.next();
                    let equal_end = p.prev_end_pos.clone();
                    let value = parse_expr(p);
                    let loc = p.mk_loc(&start, &p.prev_end_pos);

                    // Check if index is a string constant for object property assignment
                    if let ExpressionDesc::Pexp_constant(Constant::String(s, _)) =
                        &index.pexp_desc
                    {
                        // String index: obj["prop"] = value -> Pexp_apply("#=", [Pexp_send(obj, "prop"), value])
                        // First create the Pexp_send for property access
                        // Use rbracket_end for location (OCaml includes the ] in the location)
                        let send_expr = Expression {
                            pexp_desc: ExpressionDesc::Pexp_send(
                                Box::new(expr),
                                Loc {
                                    txt: s.clone(),
                                    loc: index.pexp_loc.clone(),
                                },
                            ),
                            pexp_loc: p.mk_loc(&start, &rbracket_end),
                            pexp_attributes: vec![],
                        };
                        // Create the "#=" operator
                        let operator_loc = p.mk_loc(&equal_start, &equal_end);
                        let operator = ast_helper::make_ident(
                            Longident::Lident("#=".to_string()),
                            operator_loc,
                        );
                        // Apply "#=" to [send_expr, value]
                        expr = ast_helper::make_apply(
                            operator,
                            vec![
                                (ArgLabel::Nolabel, send_expr),
                                (ArgLabel::Nolabel, value),
                            ],
                            loc,
                        );
                    } else {
                        // Non-string index: arr[index] = value -> Array.set(arr, index, value)
                        // Use the bracket location [index] for the synthesized Array.set identifier
                        // (matching OCaml's array_loc = mk_loc lbracket rbracket)
                        let array_set = ast_helper::make_ident(
                            Longident::Ldot(
                                Box::new(Longident::Lident("Array".to_string())),
                                "set".to_string(),
                            ),
                            bracket_loc.clone(),
                        );
                        let mut apply_expr = ast_helper::make_apply(
                            array_set,
                            vec![
                                (ArgLabel::Nolabel, expr),
                                (ArgLabel::Nolabel, index),
                                (ArgLabel::Nolabel, value),
                            ],
                            loc,
                        );
                        expr = apply_expr;
                    }
                } else {
                    // Check if index is a string constant for Pexp_send (JS object access)
                    // or use Array.get for other index types
                    // Use rbracket_end for the location end (OCaml includes the ] in the location)
                    let loc = p.mk_loc(&start, &rbracket_end);

                    if let ExpressionDesc::Pexp_constant(Constant::String(s, _)) =
                        &index.pexp_desc
                    {
                        // String index: obj["prop"] -> Pexp_send(obj, "prop")
                        expr = Expression {
                            pexp_desc: ExpressionDesc::Pexp_send(
                                Box::new(expr),
                                Loc {
                                    txt: s.clone(),
                                    loc: index.pexp_loc.clone(),
                                },
                            ),
                            pexp_loc: loc,
                            pexp_attributes: vec![],
                        };
                    } else {
                        // Non-string index: arr[index] -> Array.get(arr, index)
                        // Use the bracket location [index] for the synthesized Array.get identifier
                        // (matching OCaml's array_loc = mk_loc lbracket rbracket)
                        let array_get = ast_helper::make_ident(
                            Longident::Ldot(
                                Box::new(Longident::Lident("Array".to_string())),
                                "get".to_string(),
                            ),
                            bracket_loc,
                        );
                        // OCaml doesn't add res.array.access attribute in the parser
                        // The printer can detect array access from the AST structure
                        expr = ast_helper::make_apply(
                            array_get,
                            vec![(ArgLabel::Nolabel, expr), (ArgLabel::Nolabel, index)],
                            loc,
                        );
                    }
                }
            }
            // Note: MinusGreater (->) is handled by parse_binary_expr as a regular
            // binary operator, not here. It is NOT desugared - a->f(b) stays as (->)(a, f(b))
            Token::Backtick if !no_call => {
                // Tagged template literal: tag`strings${values}`
                // Only if on same line
                if p.prev_end_pos.line < p.start_pos.line {
                    break;
                }
                // Tagged templates can be any expression: (foo ? bar : baz)`...`
                match &expr.pexp_desc {
                    ExpressionDesc::Pexp_ident(lid) => {
                        let prefix = (lid.txt.clone(), lid.loc.clone());
                        expr = parse_template_literal_with_prefix(p, Some(prefix));
                    }
                    _ => {
                        expr = parse_tagged_template_literal_with_tag_expr(p, expr);
                    }
                }
            }
            _ => break,
        }
    }

    expr
}

/// Parse function call arguments.
fn parse_call_args(p: &mut Parser<'_>) -> (Vec<(ArgLabel, Expression)>, bool) {
    let mut args = vec![];
    let mut partial = false;

    p.expect(Token::Lparen);
    // Capture start position AFTER consuming '(' (to match OCaml behavior)
    // OCaml's start_pos for empty args is the position after '(' is consumed
    let start_pos = p.start_pos.clone();

    // Check for uncurried call syntax: f(. x, y)
    // The dot indicates an uncurried function call (deprecated in newer ReScript)
    let uncurried = p.token == Token::Dot;
    if uncurried {
        p.next();
        // Check for apply(.) - uncurried unit call
        // This produces a single unit argument, not an empty list
        // OCaml uses Location.mknoloc here, so no location
        if p.token == Token::Rparen {
            let unit_expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_construct(
                    Loc {
                        txt: Longident::Lident("()".to_string()),
                        loc: Location::none(),
                    },
                    None,
                ),
                pexp_loc: Location::none(),
                pexp_attributes: vec![],
            };
            args.push((ArgLabel::Nolabel, unit_expr));
        }
    }

    while p.token != Token::Rparen && p.token != Token::Eof {
        // Check for partial application marker: f(a, ...)
        if p.token == Token::DotDotDot {
            p.next();
            partial = true;
            break;
        }

        let (label, expr) = parse_argument(p);
        args.push((label, expr));

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rparen);
    // Get prev_end_pos AFTER consuming ')' to match OCaml behavior
    let rparen_end = p.prev_end_pos.clone();

    // If no arguments were parsed, treat f() as f(())
    // OCaml treats empty parens as a unit argument
    if args.is_empty() && !partial {
        let unit_loc = p.mk_loc(&start_pos, &rparen_end);
        let unit_expr = Expression {
            pexp_desc: ExpressionDesc::Pexp_construct(
                Loc {
                    txt: Longident::Lident("()".to_string()),
                    loc: unit_loc.clone(),
                },
                None,
            ),
            pexp_loc: unit_loc,
            pexp_attributes: vec![],
        };
        args.push((ArgLabel::Nolabel, unit_expr));
    }

    (args, partial)
}

/// Parse a single argument (possibly labeled).
fn parse_argument(p: &mut Parser<'_>) -> (ArgLabel, Expression) {
    if p.token == Token::Tilde {
        p.next();
        let (name, name_loc) = parse_lident(p);
        let name_located = Located::new(name.clone(), name_loc.clone());

        if p.token == Token::Equal {
            p.next();
            if p.token == Token::Question {
                p.next();
                // ~label=?expr or ~label=?expr: type
                (
                    ArgLabel::Optional(name_located),
                    parse_constrained_expr_in_arg(p),
                )
            } else {
                // ~label=expr or ~label=expr: type
                (ArgLabel::Labelled(name_located), parse_constrained_expr_in_arg(p))
            }
        } else if p.token == Token::Question {
            // ~label? (optional punning)
            p.next();
            // Use the label's location for the punned expression
            let expr = ast_helper::make_ident(Longident::Lident(name.clone()), name_loc.clone());
            (ArgLabel::Optional(name_located), expr)
        } else if p.token == Token::Colon {
            // ~label: type (labeled with type annotation - punning with constraint)
            p.next();
            let typ = super::typ::parse_typ_expr(p);
            // Use the label's location for the punned expression
            let ident_expr = ast_helper::make_ident(Longident::Lident(name.clone()), name_loc.clone());
            // Constraint location spans from label name to end of type
            let constraint_loc = p.mk_loc(&name_loc.loc_start, &p.prev_end_pos);
            let expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(ident_expr), typ),
                pexp_loc: constraint_loc,
                pexp_attributes: vec![],
            };
            (ArgLabel::Labelled(name_located), expr)
        } else {
            // ~label (punning)
            // Use the label's location for the punned expression
            let expr = ast_helper::make_ident(Longident::Lident(name.clone()), name_loc.clone());
            (ArgLabel::Labelled(name_located), expr)
        }
    } else {
        // Unlabeled argument - may have type constraint like f({}:t)
        (ArgLabel::Nolabel, parse_constrained_or_coerced_expr(p))
    }
}

// ============================================================================
// Tuple Expression
// ============================================================================

/// Parse a tuple expression.
fn parse_tuple_expr(p: &mut Parser<'_>, start_pos: Position, first: Expression) -> Expression {
    let mut elements = vec![first];

    while p.token != Token::Rparen && p.token != Token::Eof {
        elements.push(parse_constrained_or_coerced_expr(p));
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rparen);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    if elements.len() < 2 {
        p.err(DiagnosticCategory::Message(
            super::core::error_messages::TUPLE_SINGLE_ELEMENT.to_string(),
        ));
    }

    Expression {
        pexp_desc: ExpressionDesc::Pexp_tuple(elements),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

// ============================================================================
// Collection Expressions
// ============================================================================

/// Parse an array expression [e1, e2, ...].
/// - `[a, b, c]` becomes `Pexp_array([a, b, c])`
/// - `[...xs, a, b]` becomes `Belt.Array.concatMany([xs, [a, b]])`
fn parse_array_expr(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lbracket);

    // Track items with spread marker: (is_spread, expr)
    let mut items: Vec<(bool, Expression)> = vec![];
    while p.token != Token::Rbracket && p.token != Token::Eof {
        let is_spread = p.token == Token::DotDotDot;
        if is_spread {
            p.next();
        }
        let expr = parse_constrained_or_coerced_expr(p);
        items.push((is_spread, expr));
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbracket);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Check if there are any spreads
    let has_spread = items.iter().any(|(is_spread, _)| *is_spread);

    if !has_spread {
        // Simple case: no spread, just return array
        let elements: Vec<Expression> = items.into_iter().map(|(_, e)| e).collect();
        return Expression {
            pexp_desc: ExpressionDesc::Pexp_array(elements),
            pexp_loc: loc,
            pexp_attributes: vec![],
        };
    }

    // With spread: [...xs, a, b, ...ys, c] becomes Belt.Array.concatMany([xs, [a, b], ys, [c]])
    // Split into segments: each spread item becomes its own array element,
    // consecutive non-spread items become a single array
    let mut array_items: Vec<Expression> = vec![];
    let mut current_non_spread: Vec<Expression> = vec![];

    for (is_spread, expr) in items {
        if is_spread {
            // Flush any accumulated non-spread items as an array
            if !current_non_spread.is_empty() {
                array_items.push(Expression {
                    pexp_desc: ExpressionDesc::Pexp_array(std::mem::take(&mut current_non_spread)),
                    pexp_loc: loc.clone(),
                    pexp_attributes: vec![],
                });
            }
            // Add the spread item directly
            array_items.push(expr);
        } else {
            current_non_spread.push(expr);
        }
    }

    // Flush remaining non-spread items
    if !current_non_spread.is_empty() {
        array_items.push(Expression {
            pexp_desc: ExpressionDesc::Pexp_array(current_non_spread),
            pexp_loc: loc.clone(),
            pexp_attributes: vec![],
        });
    }

    // Create Belt.Array.concatMany([...])
    let outer_array = Expression {
        pexp_desc: ExpressionDesc::Pexp_array(array_items),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    };

    let concat_many = Expression {
        pexp_desc: ExpressionDesc::Pexp_ident(with_loc(
            Longident::Ldot(
                Box::new(Longident::Ldot(
                    Box::new(Longident::Lident("Belt".to_string())),
                    "Array".to_string(),
                )),
                "concatMany".to_string(),
            ),
            loc.clone(),
        )),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![(mknoloc("res.spread".to_string()), Payload::PStr(vec![]))],
    };

    Expression {
        pexp_desc: ExpressionDesc::Pexp_apply {
            funct: Box::new(concat_many),
            args: vec![(ArgLabel::Nolabel, outer_array)],
            partial: false,
            transformed_jsx: false,
        },
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse a list expression list{e1, e2, ...} and lower to nested :: constructors.
/// - `list{1, 2, 3}` becomes `1 :: (2 :: (3 :: []))`
/// - `list{1, 2, ...rest}` becomes `1 :: (2 :: rest)`
/// - `list{...xs, ...ys}` becomes `Belt.List.concatMany([xs, ys])`
/// - `list{...xs, 1, ...ys}` becomes `Belt.List.concatMany([xs, 1 :: ys])`
fn parse_list_expr(p: &mut Parser<'_>, start_pos: Position) -> Expression {
    // Token::List already consumed the opening '{'
    // Each item is (is_spread, expr)
    let mut items: Vec<(bool, Expression)> = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        let is_spread = p.token == Token::DotDotDot;
        if is_spread {
            p.next();
        }
        let expr = parse_constrained_or_coerced_expr(p);
        items.push((is_spread, expr));

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Split items into segments by spread boundaries
    // Each segment is (non_spread_items, optional_spread)
    let segments = split_list_by_spread(items);

    match segments.len() {
        0 => {
            // Empty list: list{} → []
            make_empty_list(&loc)
        }
        1 => {
            // Single segment: just build with :: constructors
            let (exprs, spread) = segments.into_iter().next().unwrap();
            let mut result = make_list_expression(&loc, exprs, spread);
            // The outermost expression should have the full list location
            result.pexp_loc = loc;
            result
        }
        _ => {
            // Multiple segments: use Belt.List.concatMany([...])
            let sub_exprs: Vec<Expression> = segments
                .into_iter()
                .map(|(exprs, spread)| make_list_expression(&loc, exprs, spread))
                .collect();

            let array_expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_array(sub_exprs),
                pexp_loc: loc.clone(),
                pexp_attributes: vec![],
            };

            // Build Belt.List.concatMany
            let concat_many_ident = Expression {
                pexp_desc: ExpressionDesc::Pexp_ident(Loc {
                    txt: Longident::Ldot(
                        Box::new(Longident::Ldot(
                            Box::new(Longident::Lident("Belt".to_string())),
                            "List".to_string(),
                        )),
                        "concatMany".to_string(),
                    ),
                    loc: loc.clone(),
                }),
                pexp_loc: loc.clone(),
                // Add the res.spread attribute to match OCaml
                pexp_attributes: vec![(
                    mknoloc("res.spread".to_string()),
                    Payload::PStr(vec![]),
                )],
            };

            Expression {
                pexp_desc: ExpressionDesc::Pexp_apply {
                    funct: Box::new(concat_many_ident),
                    args: vec![(ArgLabel::Nolabel, array_expr)],
                    partial: false,
                    transformed_jsx: false,
                },
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
    }
}

/// Split list items by spread into segments.
/// Each segment contains (non_spread_items, optional_spread_at_end).
/// A spread at the beginning of items starts a new segment.
fn split_list_by_spread(
    items: Vec<(bool, Expression)>,
) -> Vec<(Vec<Expression>, Option<Expression>)> {
    let mut segments: Vec<(Vec<Expression>, Option<Expression>)> = vec![];
    let mut current_exprs: Vec<Expression> = vec![];

    for (is_spread, expr) in items {
        if is_spread {
            // Spread starts a new segment (with current items) and this spread becomes the tail
            if !current_exprs.is_empty() || !segments.is_empty() {
                // If we have accumulated exprs, they go into a segment with this spread as tail
                segments.push((std::mem::take(&mut current_exprs), Some(expr)));
            } else {
                // First item is spread - just set it as the spread of first segment
                segments.push((vec![], Some(expr)));
            }
        } else {
            current_exprs.push(expr);
        }
    }

    // Handle remaining non-spread items
    if !current_exprs.is_empty() {
        segments.push((current_exprs, None));
    }

    // Handle edge case: if we have no segments but also no items
    if segments.is_empty() {
        // Will be handled by the empty case
        return vec![];
    }

    segments
}

/// Build an empty list expression `[]`
fn make_empty_list(loc: &Location) -> Expression {
    Expression {
        pexp_desc: ExpressionDesc::Pexp_construct(
            Loc {
                txt: Longident::Lident("[]".to_string()),
                loc: loc.clone(),
            },
            None,
        ),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    }
}

/// Build a list expression from items and optional spread tail
/// `[e1, e2]` → `e1 :: (e2 :: [])`
/// `[e1, e2, ...rest]` → `e1 :: (e2 :: rest)`
fn make_list_expression(
    loc: &Location,
    exprs: Vec<Expression>,
    spread: Option<Expression>,
) -> Expression {
    // For lists with spread, the inner cons cells end at the spread's end, not at }
    // For lists without spread, they end at the empty list's end (which is loc.loc_end)
    let list_end = spread
        .as_ref()
        .map(|s| s.pexp_loc.loc_end.clone())
        .unwrap_or_else(|| loc.loc_end.clone());
    // Start with either the spread tail or empty list
    let tail = spread.unwrap_or_else(|| make_empty_list(loc));

    // Fold from right: 3 :: [] -> 2 :: (3 :: []) -> 1 :: (2 :: (3 :: []))
    // Each cons cell location starts at its element and ends at the list end
    exprs.into_iter().rev().fold(tail, |acc, item| {
        // Location for this cons cell: from item's start to list end
        let cons_loc = Location::from_positions(item.pexp_loc.loc_start.clone(), list_end.clone());
        let tuple = Expression {
            pexp_desc: ExpressionDesc::Pexp_tuple(vec![item, acc]),
            pexp_loc: cons_loc.clone(),
            pexp_attributes: vec![],
        };
        Expression {
            pexp_desc: ExpressionDesc::Pexp_construct(
                Loc {
                    txt: Longident::Lident("::".to_string()),
                    loc: cons_loc.clone(),
                },
                Some(Box::new(tuple)),
            ),
            pexp_loc: cons_loc,
            pexp_attributes: vec![],
        }
    })
}

/// Parse a dict expression dict{"key": value, ...}.
/// Note: The Token::Dict already includes the '{', so we don't expect Lbrace.
///
/// Desugars to: Primitive_dict.make([("key1", val1), ("key2", val2), ...])
fn parse_dict_expr(p: &mut Parser<'_>, start_pos: Position) -> Expression {
    // Token::Dict already consumed the opening '{'

    let mut key_value_pairs: Vec<Expression> = vec![];
    while p.token != Token::Rbrace && p.token != Token::Eof {
        // Parse key (string)
        match &p.token {
            Token::String(s) => {
                let key = s.clone();
                // Dict keys are always None (printed as "key"), not Some("js")
                // This matches OCaml's behavior where dict keys stay as object property names
                let key_start = p.start_pos.clone();
                p.next();
                let key_end = p.prev_end_pos.clone();
                let key_loc = p.mk_loc(&key_start, &key_end);

                p.expect(Token::Colon);
                let value = parse_expr(p);
                let value_end = value.pexp_loc.loc_end.clone();

                // Create tuple (key_string, value)
                // Key uses None delimiter to be printed as "key" (object property style)
                let key_expr = Expression {
                    pexp_desc: ExpressionDesc::Pexp_constant(Constant::String(key, None)),
                    pexp_loc: key_loc.clone(),
                    pexp_attributes: vec![],
                };
                let tuple_loc = p.mk_loc(&key_loc.loc_start, &value_end);
                let tuple = Expression {
                    pexp_desc: ExpressionDesc::Pexp_tuple(vec![key_expr, value]),
                    pexp_loc: tuple_loc,
                    pexp_attributes: vec![],
                };
                key_value_pairs.push(tuple);
            }
            _ => {
                p.err(DiagnosticCategory::Message(
                    "Dict keys must be strings".to_string(),
                ));
                break;
            }
        }
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Create array of key-value pairs
    let array_expr = Expression {
        pexp_desc: ExpressionDesc::Pexp_array(key_value_pairs),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    };

    // Create Primitive_dict.make identifier
    let make_ident = Expression {
        pexp_desc: ExpressionDesc::Pexp_ident(Loc {
            txt: Longident::Ldot(
                Box::new(Longident::Lident("Primitive_dict".to_string())),
                "make".to_string(),
            ),
            loc: loc.clone(),
        }),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    };

    // Create application: Primitive_dict.make(array)
    Expression {
        pexp_desc: ExpressionDesc::Pexp_apply {
            funct: Box::new(make_ident),
            args: vec![(ArgLabel::Nolabel, array_expr)],
            partial: false,
            transformed_jsx: false,
        },
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse a braced expression or record.
fn parse_braced_or_record_expr(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lbrace);

    // Check for empty braces
    if p.token == Token::Rbrace {
        p.next();
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
        return Expression {
            pexp_desc: ExpressionDesc::Pexp_record(vec![], None),
            pexp_loc: loc,
            pexp_attributes: vec![],
        };
    }

    // Check for spread - definitely a record
    if p.token == Token::DotDotDot {
        p.next();
        let spread_start = p.start_pos.clone();
        let mut spread = parse_unary_expr(p);
        // Check for type constraint on spread: {...make() : myRecord, foo: bar}
        if p.token == Token::Colon {
            p.next();
            let typ = super::typ::parse_typ_expr(p);
            let loc = p.mk_loc(&spread_start, &p.prev_end_pos);
            spread = Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(spread), typ),
                pexp_loc: loc,
                pexp_attributes: vec![],
            };
        }
        p.optional(&Token::Comma);
        let fields = parse_record_fields(p);
        p.expect(Token::Rbrace);
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
        return Expression {
            pexp_desc: ExpressionDesc::Pexp_record(fields, Some(Box::new(spread))),
            pexp_loc: loc,
            pexp_attributes: vec![],
        };
    }

    // Check if this looks like a block expression (module-level constructs or let/expressions)
    if is_block_expression_start(&p.token) {
        return parse_block_expr(p, start_pos);
    }

    // Check if this looks like a record (name followed by : or ,)
    // or a JS object (string key followed by :)
    // Also check for optional field syntax: { ? name, ... }
    let is_record = p.lookahead(|state| {
        // Optional field punning: { ? name, ... }
        if state.token == Token::Question {
            state.next();
            // Should be followed by an identifier
            return matches!(state.token, Token::Lident(_));
        }

        let started_with_uident = matches!(state.token, Token::Uident(_));

        let mut parts = 0;
        while let Token::Lident(_) | Token::Uident(_) = &state.token {
            parts += 1;
            state.next();
            if state.token == Token::Dot {
                state.next();
            } else {
                break;
            }
        }

        if parts == 0 {
            return false;
        }

        // If the record field starts with a module path (Uident or dotted),
        // require a `:`/`?` to avoid misclassifying `{Foo.bar}` blocks as records.
        if started_with_uident || parts > 1 {
            matches!(state.token, Token::Colon | Token::Comma)
        } else {
            matches!(state.token, Token::Colon | Token::Comma)
        }
    });

    // Check if this looks like a JS object literal: { "key": value }
    let is_js_object = p.lookahead(|state| {
        if let Token::String(_) = &state.token {
            state.next();
            matches!(state.token, Token::Colon)
        } else {
            false
        }
    });

    if is_record {
        let fields = parse_record_fields(p);
        p.expect(Token::Rbrace);
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
        return Expression {
            pexp_desc: ExpressionDesc::Pexp_record(fields, None),
            pexp_loc: loc,
            pexp_attributes: vec![],
        };
    }

    if is_js_object {
        return parse_js_object_expr(p, start_pos);
    }

    // Fall back to block expression
    parse_block_expr(p, start_pos)
}

/// Check if a token starts a block expression (vs record).
fn is_block_expression_start(token: &Token) -> bool {
    matches!(
        token,
        Token::Open
            | Token::Module
            | Token::Exception
            | Token::Typ
            | Token::Let { .. }
            | Token::If
            | Token::Switch
            | Token::While
            | Token::For
            | Token::Try
            | Token::At // attribute
    )
}

/// Parse an expression block body WITHOUT adding res.braces.
/// This is used for if-then-else branches, for loops, while loops, etc.
/// where the block is structurally required and res.braces is not needed.
/// Note: This does NOT consume the closing Rbrace - the caller must do that.
fn parse_expr_block(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    // Parse the block body - this handles building the proper nesting for let/module/open/exception
    let body = parse_block_body(p);

    // Don't expect Rbrace here - caller will do that
    // Use current position for empty block, or body's end position
    let end_pos = body
        .as_ref()
        .map(|e| e.pexp_loc.loc_end.clone())
        .unwrap_or_else(|| p.prev_end_pos.clone());
    let loc = p.mk_loc(&start_pos, &end_pos);

    body.unwrap_or_else(|| ast_helper::make_unit(loc))
}

/// Parse a block expression { stmt1; stmt2; expr } WITH res.braces attribute.
/// This is used for standalone braced expressions where res.braces is needed
/// to preserve the bracing information.
fn parse_block_expr(p: &mut Parser<'_>, start_pos: Position) -> Expression {
    // Parse the block body - this handles building the proper nesting for let/module/open/exception
    let body = parse_block_body(p);

    p.expect(Token::Rbrace);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    let mut expr = body.unwrap_or_else(|| ast_helper::make_unit(loc.clone()));

    // OCaml includes the opening `{` in the pattern's location for braced arrow functions.
    // It also shifts the Pexp_fun's location to start at `=>`.
    // This preserves information about the bracing context.
    if let ExpressionDesc::Pexp_fun { ref mut lhs, .. } = expr.pexp_desc {
        // Extend the pattern's location to start at the opening brace
        lhs.ppat_loc.loc_start = start_pos.clone();

        // Also extend the inner location for Ppat_var (OCaml includes `{` in the variable location too)
        if let super::ast::PatternDesc::Ppat_var(ref mut var_loc) = lhs.ppat_desc {
            var_loc.loc.loc_start = start_pos.clone();
        }

        // Adjust the Pexp_fun's location to start just after the pattern ends
        // (at the `=>` position, which is pattern_end + 1 for space)
        // OCaml starts at the `=` of `=>`, which is pattern_end + 1 character
        expr.pexp_loc.loc_start = lhs.ppat_loc.loc_end.clone();
        expr.pexp_loc.loc_start.cnum += 1; // Skip space after pattern
    }

    // Add res.braces attribute to mark this as a braced expression
    let braces_attr = make_braces_attr(loc);
    expr.pexp_attributes.insert(0, braces_attr);

    expr
}

/// Parse the body of a block expression.
/// Returns None if the block is empty, Some(expr) otherwise.
/// This properly nests let/module/open/exception with their continuation as the body.
fn parse_block_body(p: &mut Parser<'_>) -> Option<Expression> {
    // Skip leading semicolons
    while p.token == Token::Semicolon {
        p.next();
    }

    if p.token == Token::Rbrace || p.token == Token::Eof {
        return None;
    }

    // Parse any leading attributes
    let attrs = parse_attributes(p);

    // Parse the first expression/statement
    let mut expr = match &p.token {
        Token::Open => {
            let expr_start = p.start_pos.clone();
            p.next();
            // Check for override flag (open!)
            let override_flag = if p.token == Token::Bang {
                p.next();
                OverrideFlag::Override
            } else {
                OverrideFlag::Fresh
            };
            let lid = parse_module_longident(p);

            // Consume optional semicolon/newline
            p.optional(&Token::Semicolon);

            // Parse the rest of the block as the body
            let rest = parse_block_body(p);
            let body = rest.unwrap_or_else(|| {
                // OCaml: when block ends without trailing expression, implicit () is at the } position
                let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                ast_helper::make_unit(loc)
            });

            let loc = p.mk_loc(&expr_start, &body.pexp_loc.loc_end);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_open(override_flag, lid, Box::new(body)),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        Token::Let { .. } => {
            // For let bindings, attributes go on the binding, not the expression
            // Return early to avoid adding attrs to the expression at the end
            return Some(parse_let_in_block_with_continuation_and_attrs(p, attrs));
        }
        Token::Module => {
            let expr_start = p.start_pos.clone();
            p.next();
            // Check for first-class module: module(Expr) or module(Expr: Type)
            if p.token == Token::Lparen {
                // Parse the first-class module and continue with full expression
                let pack_expr = parse_first_class_module_expr(p, expr_start);
                let expr = parse_expr_with_operand(p, pack_expr);

                // Consume optional semicolon
                p.optional(&Token::Semicolon);

                // Check for continuation
                if let Some(rest) = parse_block_body(p) {
                    let new_loc = p.mk_loc(&expr.pexp_loc.loc_start, &rest.pexp_loc.loc_end);
                    Expression {
                        pexp_desc: ExpressionDesc::Pexp_sequence(Box::new(expr), Box::new(rest)),
                        pexp_loc: new_loc,
                        pexp_attributes: vec![],
                    }
                } else {
                    expr
                }
            } else {
                // Parse module definition: module M = expr or module M: T = expr
                let module_name = parse_module_name(p);

                // Check for optional type constraint: module M: T = expr
                let mod_type = if p.token == Token::Colon {
                    p.next();
                    Some(super::module::parse_module_type(p))
                } else {
                    None
                };

                p.expect(Token::Equal);
                let mut module_expr = super::module::parse_module_expr(p);

                // If there's a type constraint, wrap the expression
                if let Some(mod_type) = mod_type {
                    let loc = p.mk_loc(&module_expr.pmod_loc.loc_start, &mod_type.pmty_loc.loc_end);
                    module_expr = ModuleExpr {
                        pmod_desc: ModuleExprDesc::Pmod_constraint(
                            Box::new(module_expr),
                            Box::new(mod_type),
                        ),
                        pmod_loc: loc,
                        pmod_attributes: vec![],
                    };
                }

                // Consume optional semicolon/newline
                p.optional(&Token::Semicolon);

                // Parse the rest of the block as the body
                let rest = parse_block_body(p);
                let body = rest.unwrap_or_else(|| {
                    // OCaml: when block ends without trailing expression, implicit () is at the } position
                    let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                    ast_helper::make_unit(loc)
                });

                let loc = p.mk_loc(&expr_start, &body.pexp_loc.loc_end);
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_letmodule(
                        module_name,
                        module_expr,
                        Box::new(body),
                    ),
                    pexp_loc: loc,
                    pexp_attributes: vec![],
                }
            }
        }
        Token::Exception => {
            // Capture start position BEFORE consuming 'exception' for accurate location
            let expr_start = p.start_pos.clone();
            p.next();
            let ext =
                super::module::parse_extension_constructor(p, vec![], Some(expr_start.clone()));

            // Consume optional semicolon/newline
            p.optional(&Token::Semicolon);

            // Parse the rest of the block as the body
            let rest = parse_block_body(p);
            let body = rest.unwrap_or_else(|| {
                // OCaml: when block ends without trailing expression, implicit () is at the } position
                let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                ast_helper::make_unit(loc)
            });

            let loc = p.mk_loc(&expr_start, &body.pexp_loc.loc_end);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_letexception(ext, Box::new(body)),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        _ => {
            let mut expr = parse_expr(p);

            // Attach any leading attributes to this expression (before any sequence wrapping)
            if !attrs.is_empty() {
                expr.pexp_attributes.extend(attrs.clone());
            }

            // Consume optional semicolon
            p.optional(&Token::Semicolon);

            // Check for continuation
            if let Some(rest) = parse_block_body(p) {
                let new_loc = p.mk_loc(&expr.pexp_loc.loc_start, &rest.pexp_loc.loc_end);
                return Some(Expression {
                    pexp_desc: ExpressionDesc::Pexp_sequence(Box::new(expr), Box::new(rest)),
                    pexp_loc: new_loc,
                    pexp_attributes: vec![],
                });
            } else {
                return Some(expr);
            }
        }
    };

    // Attach any leading attributes to the expression (for let/open/module/exception cases)
    if !attrs.is_empty() {
        expr.pexp_attributes.extend(attrs);
    }

    Some(expr)
}

/// Parse a let expression inside a block, with proper continuation handling.
fn parse_let_in_block_with_continuation(p: &mut Parser<'_>) -> Expression {
    parse_let_in_block_with_continuation_and_attrs(p, vec![])
}

fn parse_let_in_block_with_continuation_and_attrs(
    p: &mut Parser<'_>,
    leading_attrs: Attributes,
) -> Expression {
    let start_pos = p.start_pos.clone();

    // Consume let token
    let unwrap = match &p.token {
        Token::Let { unwrap } => *unwrap,
        _ => false,
    };
    p.next();
    // Track let? position for let.unwrap attribute location
    let let_end_pos = p.prev_end_pos.clone();

    // Check for rec
    let rec_flag = if p.token == Token::Rec {
        p.next();
        RecFlag::Recursive
    } else {
        RecFlag::Nonrecursive
    };

    // Parse first binding
    let mut bindings = vec![];
    let first_pat = super::pattern::parse_pattern(p);

    // Handle optional type annotation: let x: int = ...
    // Also track locally abstract types for Pexp_newtype wrapping
    let (first_pat, newtype_info) = if p.token == Token::Colon {
        p.next();

        // Check if this is `type a.` syntax (locally abstract types)
        let is_locally_abstract = p.token == Token::Typ;

        let typ = super::typ::parse_typ_expr(p);

        // Extract type variables and inner type if this is a Ptyp_poly from `type a.` syntax
        let (final_typ, newtype_info) = match typ.ptyp_desc {
            CoreTypeDesc::Ptyp_poly(ref vars, ref inner) if is_locally_abstract => {
                // Build the set of variable names for substitution
                let var_set: std::collections::HashSet<&str> =
                    vars.iter().map(|v| v.txt.as_str()).collect();

                // The inner type (with Ptyp_constr) is used for Pexp_constraint
                let inner_for_constraint = (**inner).clone();

                // Substitute Ptyp_constr -> Ptyp_var for the Ptyp_poly body
                let substituted_inner = super::typ::substitute_type_vars((**inner).clone(), &var_set);

                // Rebuild the Ptyp_poly with the substituted body
                let substituted_typ = CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_poly(vars.clone(), Box::new(substituted_inner)),
                    ptyp_loc: typ.ptyp_loc.clone(),
                    ptyp_attributes: typ.ptyp_attributes.clone(),
                };

                (substituted_typ, Some((vars.clone(), inner_for_constraint)))
            }
            _ => (typ, None),
        };

        // Create a constraint pattern
        let loc = p.mk_loc(&first_pat.ppat_loc.loc_start, &final_typ.ptyp_loc.loc_end);
        let pat = Pattern {
            ppat_desc: PatternDesc::Ppat_constraint(Box::new(first_pat), final_typ),
            ppat_loc: loc,
            ppat_attributes: vec![],
        };
        (pat, newtype_info)
    } else {
        (first_pat, None)
    };

    p.expect(Token::Equal);
    let mut first_value = parse_expr(p);

    // Wrap expression in Pexp_newtype for locally abstract types
    if let Some((newtype_vars, inner_type)) = newtype_info {
        // First wrap the expression in Pexp_constraint with the inner type
        let loc = first_value.pexp_loc.clone();
        first_value = Expression {
            pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(first_value), inner_type),
            pexp_loc: loc,
            pexp_attributes: vec![],
        };

        // Then wrap in Pexp_newtype for each type variable
        // Fold in reverse order so that the outermost newtype is the first variable
        for var in newtype_vars.into_iter().rev() {
            let loc = first_value.pexp_loc.clone();
            first_value = Expression {
                pexp_desc: ExpressionDesc::Pexp_newtype(var, Box::new(first_value)),
                pexp_loc: loc,
                pexp_attributes: vec![],
            };
        }
    }

    let first_loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Combine leading_attrs with let.unwrap attribute if this is let?
    let first_attrs = if unwrap {
        let mut attrs = leading_attrs;
        let unwrap_loc = p.mk_loc(&start_pos, &let_end_pos);
        attrs.push((
            with_loc("let.unwrap".to_string(), unwrap_loc),
            Payload::PStr(vec![]),
        ));
        attrs
    } else {
        leading_attrs
    };

    bindings.push(ValueBinding {
        pvb_pat: first_pat,
        pvb_expr: first_value,
        pvb_attributes: first_attrs,
        pvb_loc: first_loc,
    });

    // Parse additional bindings with 'and'
    while p.token == Token::And {
        // Capture start position BEFORE consuming `and`
        let binding_start = p.start_pos.clone();
        p.next();
        let pat = super::pattern::parse_pattern(p);

        // Handle optional type annotation
        let pat = if p.token == Token::Colon {
            p.next();
            let typ = super::typ::parse_typ_expr(p);
            let loc = p.mk_loc(&pat.ppat_loc.loc_start, &typ.ptyp_loc.loc_end);
            Pattern {
                ppat_desc: PatternDesc::Ppat_constraint(Box::new(pat), typ),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        } else {
            pat
        };

        p.expect(Token::Equal);
        let value = parse_expr(p);
        let binding_loc = p.mk_loc(&binding_start, &p.prev_end_pos);
        bindings.push(ValueBinding {
            pvb_pat: pat,
            pvb_expr: value,
            pvb_attributes: vec![],
            pvb_loc: binding_loc,
        });
    }

    // Consume optional semicolon/newline
    p.optional(&Token::Semicolon);

    // Parse the rest of the block as the body
    let rest = parse_block_body(p);
    let is_implicit_body = rest.is_none();
    let body = rest.unwrap_or_else(|| {
        // OCaml: when block ends without trailing expression, implicit () is at the } position
        let loc = p.mk_loc(&p.start_pos, &p.end_pos);
        ast_helper::make_unit(loc)
    });

    // OCaml: Pexp_let location extends to cover body if body is not implicit ()
    // But if body IS implicit (), use the binding end position
    let let_end = if is_implicit_body {
        // If body is implicit unit at }, use the binding end (before semicolon)
        bindings.last().map(|b| b.pvb_loc.loc_end.clone()).unwrap_or(p.prev_end_pos.clone())
    } else {
        // Otherwise use body's end (this includes Pexp_let and explicit expressions)
        body.pexp_loc.loc_end.clone()
    };

    let loc = p.mk_loc(&start_pos, &let_end);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_let(rec_flag, bindings, Box::new(body)),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse a module name.
fn parse_module_name(p: &mut Parser<'_>) -> Located<String> {
    match &p.token {
        Token::Uident(name) => {
            let name = name.clone();
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(name, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected module name".to_string(),
            ));
            mknoloc("Error".to_string())
        }
    }
}

/// Parse a module long identifier.
fn parse_module_longident(p: &mut Parser<'_>) -> Located<Longident> {
    let start_pos = p.start_pos.clone();
    let mut parts = vec![];

    loop {
        match &p.token {
            Token::Uident(name) => {
                parts.push(name.clone());
                p.next();
                if p.token == Token::Dot {
                    p.next();
                } else {
                    break;
                }
            }
            Token::Lident(name) => {
                parts.push(name.clone());
                p.next();
                break;
            }
            _ => break,
        }
    }

    if parts.is_empty() {
        p.err(DiagnosticCategory::Message(
            "Expected module path".to_string(),
        ));
        parts.push("Error".to_string());
    }

    let lid = super::core::build_longident(&parts);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
    with_loc(lid, loc)
}

/// Parse record fields.
fn parse_record_fields(p: &mut Parser<'_>) -> Vec<ExpressionRecordField> {
    let mut fields = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        let field_start = p.start_pos.clone();

        // Check for optional field punning: { ? name, ... }
        let opt_punning = p.token == Token::Question;
        if opt_punning {
            p.next();
        }

        // Support qualified record labels: {Module.label: value}
        let mut parts: Vec<String> = vec![];
        loop {
            match &p.token {
                Token::Lident(name) | Token::Uident(name) => {
                    parts.push(name.clone());
                    p.next();
                }
                _ if p.token.is_keyword() => {
                    parts.push(p.token.to_string());
                    p.next();
                }
                _ => break,
            }
            if p.token == Token::Dot {
                p.next();
            } else {
                break;
            }
        }

        if parts.is_empty() {
            break;
        }

        let lid_unloc = super::core::build_longident(&parts);
        let pun_name = parts.last().cloned().unwrap_or("_".to_string());

        let (lid, expr, opt) = if p.token == Token::Colon {
            // field: value or field: ? value
            // Capture lid location - just the identifier, not including the colon
            let lid_end = p.prev_end_pos.clone();
            p.next();
            let lid_loc = p.mk_loc(&field_start, &lid_end);
            // Check for optional marker after colon: name: ? value
            let opt = p.token == Token::Question;
            if opt {
                p.next();
            }
            let expr = parse_expr(p);
            (with_loc(lid_unloc, lid_loc), expr, opt)
        } else {
            // Punning: field is same as variable
            let loc = p.mk_loc(&field_start, &p.prev_end_pos);
            let expr = ast_helper::make_ident(Longident::Lident(pun_name), loc.clone());
            (with_loc(lid_unloc, loc), expr, opt_punning)
        };

        fields.push(ExpressionRecordField { lid, expr, opt });

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    fields
}

/// Parse a JS object expression: { "key": value, ... }.
/// Returns a %obj extension wrapping a record.
fn parse_js_object_expr(p: &mut Parser<'_>, start_pos: Position) -> Expression {
    let mut fields = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        if let Token::String(key) = &p.token {
            let key = key.clone();
            let field_start = p.start_pos.clone();
            let key_end = p.end_pos.clone();
            p.next();

            // The lid location should be just the key, not including the value
            let key_loc = p.mk_loc(&field_start, &key_end);

            if p.token == Token::Colon {
                p.next();
                let expr = parse_expr(p);
                fields.push(ExpressionRecordField {
                    lid: with_loc(Longident::Lident(key), key_loc),
                    expr,
                    opt: false,
                });
            } else {
                // Punning for string keys (less common but valid)
                let expr = ast_helper::make_ident(Longident::Lident(key.clone()), key_loc.clone());
                fields.push(ExpressionRecordField {
                    lid: with_loc(Longident::Lident(key), key_loc),
                    expr,
                    opt: false,
                });
            }
        } else {
            break;
        }

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Create the inner record expression
    let record_expr = Expression {
        pexp_desc: ExpressionDesc::Pexp_record(fields, None),
        pexp_loc: loc.clone(),
        pexp_attributes: vec![],
    };

    // Wrap in %obj extension
    let record_item = StructureItem {
        pstr_desc: StructureItemDesc::Pstr_eval(record_expr, vec![]),
        pstr_loc: loc.clone(),
    };

    Expression {
        pexp_desc: ExpressionDesc::Pexp_extension((
            with_loc("obj".to_string(), loc.clone()),
            Payload::PStr(vec![record_item]),
        )),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

// ============================================================================
// Control Flow Expressions
// ============================================================================

/// Parse an if expression.
fn parse_if_expr(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::If);

    let condition = parse_expr_with_context(p, ExprContext::When);
    p.expect(Token::Lbrace);
    // Use parse_expr_block (no res.braces) for if branches, matching OCaml behavior
    let then_branch = parse_expr_block(p);
    p.expect(Token::Rbrace);

    let else_branch = if p.token == Token::Else {
        p.next();
        if p.token == Token::If {
            Some(Box::new(parse_if_expr(p)))
        } else {
            p.expect(Token::Lbrace);
            // Use parse_expr_block (no res.braces) for else branches, matching OCaml behavior
            let expr = parse_expr_block(p);
            p.expect(Token::Rbrace);
            Some(Box::new(expr))
        }
    } else {
        None
    };

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_ifthenelse(
            Box::new(condition),
            Box::new(then_branch),
            else_branch,
        ),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse a block body for switch case RHS (no braces).
/// Similar to parse_block_body but respects ExprContext and stops at | or }.
fn parse_switch_case_body(p: &mut Parser<'_>) -> Option<Expression> {
    // Skip leading semicolons
    while p.token == Token::Semicolon {
        p.next();
    }

    // Stop at | (next case), } (end of switch), or EOF
    if p.token == Token::Bar || p.token == Token::Rbrace || p.token == Token::Eof {
        return None;
    }

    // Check if this can start a block expression
    if !grammar::is_block_expr_start(&p.token) {
        return None;
    }

    let expr = match &p.token {
        Token::Open => {
            let expr_start = p.start_pos.clone();
            p.next();
            let lid = parse_module_longident(p);

            // Consume optional semicolon/newline
            p.optional(&Token::Semicolon);

            // Parse the rest as the body
            let rest = parse_switch_case_body(p);
            let body = rest.unwrap_or_else(|| {
                let loc = p.mk_loc(&expr_start, &p.prev_end_pos);
                ast_helper::make_unit(loc)
            });

            let loc = p.mk_loc(&expr_start, &body.pexp_loc.loc_end);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_open(OverrideFlag::Fresh, lid, Box::new(body)),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        Token::Let { .. } => parse_let_in_switch_case(p),
        Token::Module => {
            let expr_start = p.start_pos.clone();
            p.next();
            // Check for first-class module: module(Expr) or module(Expr: Type)
            if p.token == Token::Lparen {
                // Parse first-class module and continue with expression parsing
                let pack_expr = parse_first_class_module_expr(p, expr_start);
                let expr = parse_primary_expr(p, pack_expr, false);
                let expr = parse_binary_expr(p, expr, 1, ExprContext::SwitchCaseRhs);
                let expr = parse_ternary_expr(p, expr);

                // Consume optional semicolon
                p.optional(&Token::Semicolon);

                // Check for continuation
                if let Some(rest) = parse_switch_case_body(p) {
                    let new_loc = p.mk_loc(&expr.pexp_loc.loc_start, &rest.pexp_loc.loc_end);
                    Expression {
                        pexp_desc: ExpressionDesc::Pexp_sequence(Box::new(expr), Box::new(rest)),
                        pexp_loc: new_loc,
                        pexp_attributes: vec![],
                    }
                } else {
                    expr
                }
            } else {
                // Regular local module definition
                let module_name = parse_module_name(p);
                p.expect(Token::Equal);
                let module_expr = super::module::parse_module_expr(p);

                // Consume optional semicolon/newline
                p.optional(&Token::Semicolon);

                // Parse the rest as the body
                let rest = parse_switch_case_body(p);
                let body = rest.unwrap_or_else(|| {
                    let loc = p.mk_loc(&expr_start, &p.prev_end_pos);
                    ast_helper::make_unit(loc)
                });

                let loc = p.mk_loc(&expr_start, &body.pexp_loc.loc_end);
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_letmodule(
                        module_name,
                        module_expr,
                        Box::new(body),
                    ),
                    pexp_loc: loc,
                    pexp_attributes: vec![],
                }
            }
        }
        Token::Exception => {
            // Capture start position BEFORE consuming 'exception' for accurate location
            let expr_start = p.start_pos.clone();
            p.next();
            let ext =
                super::module::parse_extension_constructor(p, vec![], Some(expr_start.clone()));

            // Consume optional semicolon/newline
            p.optional(&Token::Semicolon);

            // Parse the rest as the body
            let rest = parse_switch_case_body(p);
            let body = rest.unwrap_or_else(|| {
                let loc = p.mk_loc(&expr_start, &p.prev_end_pos);
                ast_helper::make_unit(loc)
            });

            let loc = p.mk_loc(&expr_start, &body.pexp_loc.loc_end);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_letexception(ext, Box::new(body)),
                pexp_loc: loc,
                pexp_attributes: vec![],
            }
        }
        _ => {
            // Regular expression - use SwitchCaseRhs context
            let expr = parse_expr_with_context(p, ExprContext::SwitchCaseRhs);

            // Consume optional semicolon
            p.optional(&Token::Semicolon);

            // Check for continuation
            if let Some(rest) = parse_switch_case_body(p) {
                let new_loc = p.mk_loc(&expr.pexp_loc.loc_start, &rest.pexp_loc.loc_end);
                Expression {
                    pexp_desc: ExpressionDesc::Pexp_sequence(Box::new(expr), Box::new(rest)),
                    pexp_loc: new_loc,
                    pexp_attributes: vec![],
                }
            } else {
                expr
            }
        }
    };

    Some(expr)
}

/// Parse a let expression inside a switch case, with proper continuation handling.
fn parse_let_in_switch_case(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();

    // Consume let token
    let unwrap = match &p.token {
        Token::Let { unwrap } => *unwrap,
        _ => false,
    };
    p.next();
    // Track let? position for let.unwrap attribute location
    let let_end_pos = p.prev_end_pos.clone();

    // Check for rec
    let rec_flag = if p.token == Token::Rec {
        p.next();
        RecFlag::Recursive
    } else {
        RecFlag::Nonrecursive
    };

    // Parse first binding
    let mut bindings = vec![];
    let first_pat = super::pattern::parse_pattern(p);

    // Handle optional type annotation: let x: int = ...
    let first_pat = if p.token == Token::Colon {
        p.next();
        let typ = super::typ::parse_typ_expr(p);
        // Create a constraint pattern
        let loc = p.mk_loc(&first_pat.ppat_loc.loc_start, &typ.ptyp_loc.loc_end);
        Pattern {
            ppat_desc: PatternDesc::Ppat_constraint(Box::new(first_pat), typ),
            ppat_loc: loc,
            ppat_attributes: vec![],
        }
    } else {
        first_pat
    };

    p.expect(Token::Equal);
    let first_value = parse_expr(p);
    let first_loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Add let.unwrap attribute if this is let?
    let first_attrs = if unwrap {
        let unwrap_loc = p.mk_loc(&start_pos, &let_end_pos);
        vec![(
            with_loc("let.unwrap".to_string(), unwrap_loc),
            Payload::PStr(vec![]),
        )]
    } else {
        vec![]
    };

    bindings.push(ValueBinding {
        pvb_pat: first_pat,
        pvb_expr: first_value,
        pvb_attributes: first_attrs,
        pvb_loc: first_loc,
    });

    // Parse additional bindings with 'and'
    while p.token == Token::And {
        // Capture start position BEFORE consuming `and`
        let binding_start = p.start_pos.clone();
        p.next();
        let pat = super::pattern::parse_pattern(p);

        // Handle optional type annotation
        let pat = if p.token == Token::Colon {
            p.next();
            let typ = super::typ::parse_typ_expr(p);
            let loc = p.mk_loc(&pat.ppat_loc.loc_start, &typ.ptyp_loc.loc_end);
            Pattern {
                ppat_desc: PatternDesc::Ppat_constraint(Box::new(pat), typ),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        } else {
            pat
        };

        p.expect(Token::Equal);
        let value = parse_expr(p);
        let binding_loc = p.mk_loc(&binding_start, &p.prev_end_pos);
        bindings.push(ValueBinding {
            pvb_pat: pat,
            pvb_expr: value,
            pvb_attributes: vec![],
            pvb_loc: binding_loc,
        });
    }

    // Consume optional semicolon/newline
    p.optional(&Token::Semicolon);

    // Parse the rest as the body
    let rest = parse_switch_case_body(p);
    let is_implicit_body = rest.is_none();
    let body = rest.unwrap_or_else(|| {
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
        ast_helper::make_unit(loc)
    });

    // If body is implicit (), use binding end; otherwise use body's end
    let let_end = if is_implicit_body {
        bindings.last().map(|b| b.pvb_loc.loc_end.clone()).unwrap_or(p.prev_end_pos.clone())
    } else {
        body.pexp_loc.loc_end.clone()
    };
    let loc = p.mk_loc(&start_pos, &let_end);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_let(rec_flag, bindings, Box::new(body)),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse a first-class module expression: module(Expr) or module(Expr: ModType)
/// `module` keyword already consumed, expects `(` next
fn parse_first_class_module_expr(p: &mut Parser<'_>, start_pos: Position) -> Expression {
    p.expect(Token::Lparen);
    parse_first_class_module_expr_inner(p, start_pos)
}

/// Parse a first-class module expression after `module(` has been matched.
/// Used when the caller has already consumed `module` and checked for `(`.
/// Public for use from module.rs when parsing structure items.
pub fn parse_first_class_module_expr_from_paren(
    p: &mut Parser<'_>,
    start_pos: Position,
) -> Expression {
    p.expect(Token::Lparen);
    parse_first_class_module_expr_inner(p, start_pos)
}

/// Inner implementation for first-class module expression parsing.
/// Expects the `(` to already be consumed.
fn parse_first_class_module_expr_inner(p: &mut Parser<'_>, start_pos: Position) -> Expression {
    // Use parse_module_expr_without_constraint so the `:` is available for package type.
    // For `module(Foo: Bar)`, we want Pexp_constraint(Pexp_pack(Foo), Ptyp_package(Bar)),
    // not Pexp_pack(Pmod_constraint(Foo, Bar)).
    let mod_expr = super::module::parse_module_expr_without_constraint(p);
    // Capture position after module expr for pack_expr location (before parsing type)
    let mod_expr_end = p.prev_end_pos.clone();

    // Check for type annotation: module(Expr: ModType)
    // This becomes Pexp_constraint(Pexp_pack(ME), Ptyp_package(...))
    let package_type = if p.token == Token::Colon {
        // OCaml includes the : in the package type location
        let colon_pos = p.start_pos.clone();
        p.next();
        let mut pkg_type = super::typ::parse_package_type(p);
        pkg_type.ptyp_loc.loc_start = colon_pos;
        Some(pkg_type)
    } else {
        None
    };

    p.expect(Token::Rparen);
    let full_loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    if let Some(pkg_type) = package_type {
        // OCaml: when constrained, pack_expr location spans only start to end of module expr (not including type)
        let pack_loc = p.mk_loc(&start_pos, &mod_expr_end);
        let pack_expr = Expression {
            pexp_desc: ExpressionDesc::Pexp_pack(mod_expr),
            pexp_loc: pack_loc,
            pexp_attributes: vec![],
        };
        Expression {
            pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(pack_expr), pkg_type),
            pexp_loc: full_loc,
            pexp_attributes: vec![],
        }
    } else {
        // Without constraint, pack_expr uses full location
        Expression {
            pexp_desc: ExpressionDesc::Pexp_pack(mod_expr),
            pexp_loc: full_loc,
            pexp_attributes: vec![],
        }
    }
}

/// Parse a block expression sequence for switch case RHS (no braces).
/// Parses one or more expressions separated by newlines/semicolons,
/// stopping at `|` (next case) or `}` (end of switch).
fn parse_switch_case_block(p: &mut Parser<'_>) -> Expression {
    // Use the new recursive parser that properly nests let/module/open/exception
    parse_switch_case_body(p).unwrap_or_else(|| {
        let loc = p.mk_loc(&p.start_pos, &p.prev_end_pos);
        ast_helper::make_unit(loc)
    })
}

/// Parse a switch expression.
fn parse_switch_expr(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Switch);

    let scrutinee = parse_expr_with_context(p, ExprContext::When);
    p.expect(Token::Lbrace);

    let mut cases = vec![];
    while p.token != Token::Rbrace && p.token != Token::Eof {
        if p.token == Token::Bar {
            let bar_pos = Some(p.start_pos.clone());
            p.next();
            let lhs = super::pattern::parse_constrained_pattern(p);
            let guard = if p.token == Token::When || (p.token == Token::If) {
                p.next();
                Some(parse_expr_with_context(p, ExprContext::When))
            } else {
                None
            };
            p.expect(Token::EqualGreater);
            // Parse block of expressions for case RHS
            let rhs = parse_switch_case_block(p);
            cases.push(Case {
                pc_bar: bar_pos,
                pc_lhs: lhs,
                pc_guard: guard,
                pc_rhs: rhs,
            });
        } else {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_match(Box::new(scrutinee), cases),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse a while expression.
fn parse_while_expr(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::While);

    let condition = parse_expr_with_context(p, ExprContext::When);
    p.expect(Token::Lbrace);
    // Use parse_expr_block (no res.braces) for while body, matching OCaml behavior
    let body = parse_expr_block(p);
    p.expect(Token::Rbrace);

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_while(Box::new(condition), Box::new(body)),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse a for expression.
///
/// For loop patterns have complex parsing due to optional parentheses:
/// - `for () in 0 to 10 {...}` - unit pattern without outer parens
/// - `for (pattern in 0 to 10) {...}` - pattern with optional outer parens wrapping entire head
/// - `for (a, b) in 0 to 10 {...}` - tuple pattern (no outer paren)
fn parse_for_expr(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::For);
    p.leave_breadcrumb(Grammar::ExprFor);
    p.begin_region();

    let (pat, has_opening_paren) = if p.token == Token::Lparen {
        let lparen_pos = p.start_pos.clone();
        p.next();

        if p.token == Token::Rparen {
            // for () - unit pattern, then check for alias
            p.next();
            let loc = p.mk_loc(&lparen_pos, &p.prev_end_pos);
            let unit_pat = super::pattern::make_unit_construct_pattern(loc);
            let pat = super::pattern::parse_alias_pattern(p, unit_pat, vec![]);
            (pat, false)
        } else {
            // Could be:
            // 1. for (pattern in start to end) - outer paren wraps for head
            // 2. for (a, b) in start to end - tuple pattern
            p.leave_breadcrumb(Grammar::Pattern);
            let pat = super::pattern::parse_pattern(p);
            p.eat_breadcrumb();

            if p.token == Token::Comma {
                // Tuple pattern: for (a, b, ...) in ...
                p.next();
                let tuple_pat =
                    super::pattern::parse_tuple_pattern(p, lparen_pos, pat, vec![]);
                let aliased = super::pattern::parse_alias_pattern(p, tuple_pat, vec![]);
                (aliased, false)
            } else {
                // Pattern with outer paren: for (pattern in ...)
                // The ) will be expected after the end expression
                (pat, true)
            }
        }
    } else {
        // No leading paren: for pattern in ...
        p.leave_breadcrumb(Grammar::Pattern);
        let pat = super::pattern::parse_pattern(p);
        p.eat_breadcrumb();
        (pat, false)
    };

    p.expect(Token::In);
    let start_expr = parse_expr(p);

    let direction = if matches!(&p.token, Token::Lident(s) if s == "to") {
        p.next();
        DirectionFlag::Upto
    } else if matches!(&p.token, Token::Lident(s) if s == "downto") {
        p.next();
        DirectionFlag::Downto
    } else {
        p.err(DiagnosticCategory::Message(
            "Expected 'to' or 'downto'".to_string(),
        ));
        DirectionFlag::Upto
    };

    let end_expr = parse_expr_with_context(p, ExprContext::When);

    if has_opening_paren {
        p.expect(Token::Rparen);
    }

    p.expect(Token::Lbrace);
    // Use parse_expr_block (no res.braces) for for loop body, matching OCaml behavior
    let body = parse_expr_block(p);
    p.expect(Token::Rbrace);

    p.eat_breadcrumb();
    p.end_region();

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_for(
            pat,
            Box::new(start_expr),
            Box::new(end_expr),
            direction,
            Box::new(body),
        ),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Parse a try expression.
fn parse_try_expr(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Try);

    let body = parse_expr_with_context(p, ExprContext::When);
    // "catch" is not a keyword, so we check for Lident("catch")
    if !matches!(&p.token, Token::Lident(s) if s == "catch") {
        p.err(DiagnosticCategory::Message("Expected 'catch'".to_string()));
    } else {
        p.next();
    }
    p.expect(Token::Lbrace);

    let mut cases = vec![];
    while p.token != Token::Rbrace && p.token != Token::Eof {
        if p.token == Token::Bar {
            let bar_pos = Some(p.start_pos.clone());
            p.next();
            let lhs = super::pattern::parse_constrained_pattern(p);
            let guard = if p.token == Token::When {
                p.next();
                Some(parse_expr_with_context(p, ExprContext::When))
            } else {
                None
            };
            p.expect(Token::EqualGreater);
            // Parse a block of expressions for catch case RHS (no braces),
            // stopping at `|` (next case) or `}` (end of catch).
            let rhs = parse_switch_case_block(p);
            cases.push(Case {
                pc_bar: bar_pos,
                pc_lhs: lhs,
                pc_guard: guard,
                pc_rhs: rhs,
            });
        } else {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_try(Box::new(body), cases),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

// ============================================================================
// JSX Parsing
// ============================================================================

/// Parse a JSX element.
fn parse_jsx(p: &mut Parser<'_>) -> Expression {
    let start_pos = p.start_pos.clone();
    p.expect(Token::LessThan);

    // Check for fragment <>
    if p.token == Token::GreaterThan {
        return parse_jsx_fragment(p, start_pos);
    }

    // Parse tag name - the location starts at the identifier, not the '<'
    let tag_name_start = p.start_pos.clone();
    let tag_name = parse_jsx_tag_name(p);
    let tag_name_loc = p.mk_loc(&tag_name_start, &p.prev_end_pos);

    // Parse props
    let props = parse_jsx_props(p);

    // Self-closing or container?
    if p.token == Token::Forwardslash {
        p.next();
        p.expect(Token::GreaterThan);
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

        Expression {
            pexp_desc: ExpressionDesc::Pexp_jsx_element(JsxElement::Unary(JsxUnaryElement {
                tag_name: with_loc(tag_name, tag_name_loc),
                props,
            })),
            pexp_loc: loc,
            pexp_attributes: vec![],
        }
    } else {
        p.expect(Token::GreaterThan);
        let opening_end = p.prev_end_pos.clone();

        // Parse children
        let children = parse_jsx_children(p);

        // Parse closing tag
        p.expect(Token::LessThan);
        p.expect(Token::Forwardslash);
        let _closing_tag_name = parse_jsx_tag_name(p);
        p.expect(Token::GreaterThan);

        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

        Expression {
            pexp_desc: ExpressionDesc::Pexp_jsx_element(JsxElement::Container(
                JsxContainerElement {
                    tag_name_start: with_loc(tag_name, tag_name_loc),
                    opening_end,
                    props,
                    children,
                    closing_tag: None, // Simplified
                },
            )),
            pexp_loc: loc,
            pexp_attributes: vec![],
        }
    }
}

/// Parse a JSX fragment.
fn parse_jsx_fragment(p: &mut Parser<'_>, start_pos: Position) -> Expression {
    let opening = p.prev_end_pos.clone();
    p.expect(Token::GreaterThan);

    let children = parse_jsx_children(p);

    p.expect(Token::LessThan);
    p.expect(Token::Forwardslash);
    let closing = p.start_pos.clone();
    p.expect(Token::GreaterThan);

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Expression {
        pexp_desc: ExpressionDesc::Pexp_jsx_element(JsxElement::Fragment(JsxFragment {
            opening,
            children,
            closing,
        })),
        pexp_loc: loc,
        pexp_attributes: vec![],
    }
}

/// Read hyphen chain: consumes `-ident` sequences and appends them to the buffer.
/// Returns the final name with hyphens concatenated.
fn read_hyphen_chain(p: &mut Parser<'_>, initial: String) -> String {
    let mut result = initial;
    while p.token == Token::Minus {
        // Look ahead to see if there's an identifier after the hyphen
        let has_ident_after = p.lookahead(|state| {
            state.next(); // consume -
            matches!(&state.token, Token::Lident(_) | Token::Uident(_))
        });
        if !has_ident_after {
            break;
        }
        p.next(); // consume -
        match &p.token {
            Token::Lident(name) | Token::Uident(name) => {
                result.push('-');
                result.push_str(name);
                p.next();
            }
            _ => break,
        }
    }
    result
}

/// Parse a JSX tag name.
fn parse_jsx_tag_name(p: &mut Parser<'_>) -> JsxTagName {
    match &p.token {
        Token::Lident(name) => {
            let name = name.clone();
            p.next();
            // Handle hyphenated tag names like `custom-tag`
            let full_name = read_hyphen_chain(p, name);
            JsxTagName::Lower(full_name)
        }
        Token::Uident(name) => {
            let mut parts = vec![name.clone()];
            p.next();
            while p.token == Token::Dot {
                p.next();
                match &p.token {
                    Token::Uident(name) => {
                        parts.push(name.clone());
                        p.next();
                    }
                    Token::Lident(name) => {
                        // Last segment is lowercase, may have hyphens
                        let lname = name.clone();
                        p.next();
                        let full_name = read_hyphen_chain(p, lname);
                        // This is a qualified lowercase tag: Foo.custom-tag
                        let path = super::core::build_longident(&parts);
                        return JsxTagName::QualifiedLower { path, name: full_name };
                    }
                    _ => break,
                }
            }
            let lid = super::core::build_longident(&parts);
            JsxTagName::Upper(lid)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected JSX tag name".to_string(),
            ));
            JsxTagName::Invalid("error".to_string())
        }
    }
}

/// Parse JSX props.
fn parse_jsx_props(p: &mut Parser<'_>) -> Vec<JsxProp> {
    let mut props = vec![];

    loop {
        match &p.token {
            Token::Lbrace => {
                // Spreading: `{...props}`
                if p.lookahead(|state| {
                    state.next();
                    state.token == Token::DotDotDot
                }) {
                    let spread_start = p.start_pos.clone();
                    p.next(); // {
                    p.expect(Token::DotDotDot);
                    let expr = parse_expr(p);
                    p.expect(Token::Rbrace);
                    let loc = p.mk_loc(&spread_start, &p.prev_end_pos);
                    props.push(JsxProp::Spreading { loc, expr });
                } else {
                    break;
                }
            }
            Token::Lident(name) => {
                let name = name.clone();
                let name_loc = p.mk_loc(&p.start_pos, &p.end_pos);
                p.next();

                if p.token == Token::Equal {
                    p.next();
                    let optional = p.optional(&Token::Question);
                    // JSX prop values can be braced expressions (`={expr}`) or simple literals/idents (`="str"`, `=x`).
                    let value = if p.token == Token::Lbrace {
                        // Parse the full braced expression (record/object/block) without
                        // consuming beyond the closing `}`.
                        let atomic = parse_atomic_expr(p);
                        parse_primary_expr(p, atomic, false)
                    } else {
                        // Parse a single operand (not a full binary expression), so we don't consume the tag's `>`/`/>`.
                        parse_operand_expr(p, ExprContext::Ordinary)
                    };
                    props.push(JsxProp::Value {
                        name: with_loc(name, name_loc),
                        optional,
                        value,
                    });
                } else {
                    // Punning
                    let optional = name.starts_with('?');
                    props.push(JsxProp::Punning {
                        optional,
                        name: with_loc(name, name_loc),
                    });
                }
            }
            Token::Question => {
                p.next();
                if let Token::Lident(name) = &p.token {
                    let name = name.clone();
                    let name_loc = p.mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    props.push(JsxProp::Punning {
                        optional: true,
                        name: with_loc(name, name_loc),
                    });
                }
            }
            token if token.is_keyword() => {
                // JSX props may use keyword-like identifiers (rare but valid).
                let name = token.to_string();
                let name_loc = p.mk_loc(&p.start_pos, &p.end_pos);
                p.next();

                if p.token == Token::Equal {
                    p.next();
                    let optional = p.optional(&Token::Question);
                    let value = if p.token == Token::Lbrace {
                        let atomic = parse_atomic_expr(p);
                        parse_primary_expr(p, atomic, false)
                    } else {
                        parse_operand_expr(p, ExprContext::Ordinary)
                    };
                    props.push(JsxProp::Value {
                        name: with_loc(name, name_loc),
                        optional,
                        value,
                    });
                } else {
                    props.push(JsxProp::Punning {
                        optional: false,
                        name: with_loc(name, name_loc),
                    });
                }
            }
            _ => break,
        }
    }

    props
}

/// Parse JSX children.
fn parse_jsx_children(p: &mut Parser<'_>) -> Vec<Expression> {
    let mut children = vec![];

    loop {
        // Check for closing tag
        if p.token == Token::LessThan {
            // Look ahead for closing
            if p.lookahead(|state| {
                state.next();
                state.token == Token::Forwardslash
            }) {
                break;
            }
            // Otherwise it's a child element
            children.push(parse_jsx(p));
        } else if p.token == Token::Lbrace {
            let atomic = parse_atomic_expr(p);
            let expr = parse_primary_expr(p, atomic, true);
            children.push(expr);
        } else if p.token == Token::Eof {
            break;
        } else if matches!(p.token, Token::Lident(_) | Token::Uident(_)) {
            // Handle identifiers and module paths as JSX children (e.g., React.null)
            // Only parse field access (.) after identifiers in JSX children.
            // Do NOT parse [] as subscript or () as function call - these should be
            // separate children (arrays wrapped in [] or calls wrapped in {}).
            let mut expr = parse_value_or_constructor(p);
            // Only parse field access chains like x.y.z
            while p.token == Token::Dot {
                p.next();
                match &p.token {
                    Token::Lident(name) => {
                        let name = name.clone();
                        let name_loc = p.mk_loc(&p.start_pos, &p.end_pos);
                        p.next();
                        let loc = p.mk_loc(&expr.pexp_loc.loc_start, &p.prev_end_pos);
                        expr = Expression {
                            pexp_desc: ExpressionDesc::Pexp_field(
                                Box::new(expr),
                                with_loc(Longident::Lident(name), name_loc),
                            ),
                            pexp_loc: loc,
                            pexp_attributes: vec![],
                        };
                    }
                    Token::Uident(_) => {
                        let field_path = parse_value_path_after_dot(p);
                        let loc = p.mk_loc(&expr.pexp_loc.loc_start, &field_path.loc.loc_end);
                        expr = Expression {
                            pexp_desc: ExpressionDesc::Pexp_field(Box::new(expr), field_path),
                            pexp_loc: loc,
                            pexp_attributes: vec![],
                        };
                    }
                    _ => break,
                }
            }
            children.push(expr);
        } else if let Token::String(s) = &p.token {
            // Handle string literals in JSX content
            let start_pos = p.start_pos.clone();
            let s = s.clone();
            let tag = get_string_tag(p);
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            children.push(ast_helper::make_constant(
                Constant::String(s, tag),
                loc,
            ));
        } else if p.token == Token::List {
            // Handle list{...} expressions in JSX children
            let start_pos = p.start_pos.clone();
            p.next(); // consume 'list'
            let expr = parse_list_expr(p, start_pos);
            children.push(expr);
        } else if p.token == Token::Lbracket {
            // Handle array literals [a, b, c] in JSX children
            let expr = parse_array_expr(p);
            children.push(expr);
        } else if p.token == Token::Lparen {
            // Handle parenthesized expressions including first-class modules
            let expr = parse_atomic_expr(p);
            let expr = parse_primary_expr(p, expr, true);
            children.push(expr);
        } else if p.token == Token::Module {
            // Handle first-class modules: module(Foo), module(Foo: Bar)
            let expr = parse_atomic_expr(p);
            let expr = parse_primary_expr(p, expr, true);
            children.push(expr);
        } else if p.token == Token::Percent {
            // Handle extension expressions: %raw("...")
            let expr = parse_atomic_expr(p);
            let expr = parse_primary_expr(p, expr, true);
            children.push(expr);
        } else if matches!(&p.token, Token::Int { .. } | Token::Float { .. } | Token::Codepoint { .. }) {
            // Handle numeric literals as JSX children
            let expr = parse_atomic_expr(p);
            children.push(expr);
        } else {
            // Skip whitespace and other tokens
            p.next();
        }
    }

    children
}

/// Parse a JSX `{ ... }` child/prop expression.
/// JSX braces can contain block-style expressions (let/open/etc) without extra braces.
fn parse_jsx_braced_expr(p: &mut Parser<'_>, lbrace_start: Position) -> Expression {
    match parse_block_body(p) {
        Some(expr) => expr,
        None => {
            let loc = p.mk_loc(&lbrace_start, &p.prev_end_pos);
            ast_helper::make_unit(loc)
        }
    }
}

// ============================================================================
// Template Literal Parsing
// ============================================================================

/// Parse a template literal with optional tag/prefix.
/// If prefix is None, it's a regular template literal.
/// If prefix is Some(ident), it's a tagged template literal like `sql`...``.
fn parse_template_literal_with_prefix(
    p: &mut Parser<'_>,
    prefix: Option<(Longident, Location)>,
) -> Expression {
    let start_pos = p.start_pos.clone();
    let template_literal_attr = (mknoloc("res.template".to_string()), Payload::PStr(vec![]));

    // Determine the string delimiter based on prefix (json -> "json", otherwise "js")
    let part_prefix = match &prefix {
        Some((Longident::Lident(name), _)) if name == "json" => Some("json".to_string()),
        _ => Some("js".to_string()),
    };

    // Parse template parts
    let mut parts: Vec<(Expression, Option<Expression>)> = vec![];

    // Handle the case where we start with a backtick
    // Track the backtick position for OCaml-compatible locations
    let backtick_pos = p.start_pos.clone();
    if p.token == Token::Backtick {
        // After backtick, switch to template literal scanning mode
        p.next_template_literal_token();
    }

    let mut is_first_part = true;
    loop {
        // For OCaml compatibility, the first string part's location includes the opening backtick
        let text_start_pos = if is_first_part {
            backtick_pos.clone()
        } else {
            p.start_pos.clone()
        };
        is_first_part = false;
        match &p.token {
            Token::TemplatePart { text, .. } => {
                // Part before an interpolation ${
                let text = text.clone();
                let text_loc = p.mk_loc(&text_start_pos, &p.end_pos);
                let mut str_expr = ast_helper::make_constant(
                    Constant::String(text, part_prefix.clone()),
                    text_loc,
                );
                str_expr.pexp_attributes.push(template_literal_attr.clone());
                p.next();
                // Parse the interpolation expression
                let expr = parse_expr(p);
                parts.push((str_expr, Some(expr)));
                // After the interpolation, switch back to template literal scanning
                p.next_template_literal_token();
            }
            Token::TemplateTail { text, .. } => {
                // Final part or simple template string
                let text = text.clone();
                let text_loc = p.mk_loc(&text_start_pos, &p.end_pos);
                let mut str_expr = ast_helper::make_constant(
                    Constant::String(text, part_prefix.clone()),
                    text_loc,
                );
                str_expr.pexp_attributes.push(template_literal_attr.clone());
                parts.push((str_expr, None));
                p.next();
                break;
            }
            Token::Backtick => {
                // Empty template string
                let text_loc = p.mk_loc(&text_start_pos, &p.end_pos);
                let mut str_expr = ast_helper::make_constant(
                    Constant::String("".to_string(), part_prefix.clone()),
                    text_loc,
                );
                str_expr.pexp_attributes.push(template_literal_attr.clone());
                parts.push((str_expr, None));
                p.next();
                break;
            }
            Token::Eof => {
                p.err(DiagnosticCategory::Message(
                    "Unterminated template literal".to_string(),
                ));
                break;
            }
            _ => {
                // This shouldn't happen in well-formed template literals
                p.err(DiagnosticCategory::Message(format!(
                    "Unexpected token in template literal: {:?}",
                    p.token
                )));
                p.next();
                break;
            }
        }
    }

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Check if this is a tagged template (non-json prefix)
    let is_tagged = match &prefix {
        Some((Longident::Lident(name), _)) if name == "json" => false,
        Some(_) => true,
        None => false,
    };

    if is_tagged {
        // Tagged template literal: tag`strings${values}`
        // Creates: tag([strings], [values]) with res.taggedTemplate attribute
        let (prefix_ident, prefix_loc) = prefix.unwrap();

        // OCaml excludes the closing backtick from the last string part's location
        let strings: Vec<Expression> = parts
            .iter()
            .enumerate()
            .map(|(i, (s, _))| {
                let mut s = s.clone();
                // Only adjust the last string part (the one with no interpolation following)
                if i == parts.len() - 1 {
                    s.pexp_loc.loc_end.cnum -= 1;
                }
                s
            })
            .collect();
        let values: Vec<Expression> = parts.iter().filter_map(|(_, v)| v.clone()).collect();

        let strings_array = Expression {
            pexp_desc: ExpressionDesc::Pexp_array(strings),
            pexp_loc: Location::none(),
            pexp_attributes: vec![],
        };
        let values_array = Expression {
            pexp_desc: ExpressionDesc::Pexp_array(values),
            pexp_loc: Location::none(),
            pexp_attributes: vec![],
        };

        let tag_expr = Expression {
            pexp_desc: ExpressionDesc::Pexp_ident(with_loc(prefix_ident, prefix_loc.clone())),
            pexp_loc: prefix_loc.clone(),
            pexp_attributes: vec![],
        };

        let tagged_template_attr = (
            mknoloc("res.taggedTemplate".to_string()),
            Payload::PStr(vec![]),
        );

        // OCaml sets the Pexp_apply location to just the tag location
        Expression {
            pexp_desc: ExpressionDesc::Pexp_apply {
                funct: Box::new(tag_expr),
                args: vec![
                    (ArgLabel::Nolabel, strings_array),
                    (ArgLabel::Nolabel, values_array),
                ],
                partial: false,
                transformed_jsx: false,
            },
            pexp_loc: prefix_loc,
            pexp_attributes: vec![tagged_template_attr],
        }
    } else {
        // Regular (untagged) template literal: `strings${values}`
        // Creates: str1 ++ val1 ++ str2 ++ val2 ... with res.template attribute

        // If there's only one part with no interpolation, just return the string
        // OCaml includes the backticks in the expression location
        if parts.len() == 1 && parts[0].1.is_none() {
            let (mut str_expr, _) = parts.into_iter().next().unwrap();
            str_expr.pexp_loc.loc_start = start_pos;
            return str_expr;
        }

        // Build the flattened list: [str1, val1, str2, val2, str3, ...]
        let mut subparts: Vec<Expression> = vec![];
        for (str_expr, val_opt) in parts {
            subparts.push(str_expr);
            if let Some(val) = val_opt {
                subparts.push(val);
            }
        }

        // Fold with ++ operator
        let hidden_operator = Expression {
            pexp_desc: ExpressionDesc::Pexp_ident(mknoloc(Longident::Lident("++".to_string()))),
            pexp_loc: Location::none(),
            pexp_attributes: vec![],
        };

        let concat = |e1: Expression, e2: Expression| -> Expression {
            let concat_loc = p.mk_loc(&e1.pexp_loc.loc_start, &e2.pexp_loc.loc_end);
            Expression {
                pexp_desc: ExpressionDesc::Pexp_apply {
                    funct: Box::new(hidden_operator.clone()),
                    args: vec![(ArgLabel::Nolabel, e1), (ArgLabel::Nolabel, e2)],
                    partial: false,
                    transformed_jsx: false,
                },
                pexp_loc: concat_loc,
                pexp_attributes: vec![template_literal_attr.clone()],
            }
        };

        let mut iter = subparts.into_iter();
        let first = iter.next().unwrap_or_else(|| {
            ast_helper::make_constant(Constant::String("".to_string(), part_prefix), loc.clone())
        });

        iter.fold(first, concat)
    }
}

/// Parse a tagged template literal where the tag is an arbitrary expression (not just an identifier).
fn parse_tagged_template_literal_with_tag_expr(p: &mut Parser<'_>, tag_expr: Expression) -> Expression {
    let tag_start = tag_expr.pexp_loc.loc_start.clone();
    let template_start = p.start_pos.clone();
    let template_literal_attr = (mknoloc("res.template".to_string()), Payload::PStr(vec![]));

    // Parse template parts
    let mut parts: Vec<(Expression, Option<Expression>)> = vec![];

    if p.token == Token::Backtick {
        p.next_template_literal_token();
    }

    loop {
        let text_start_pos = p.start_pos.clone();
        match &p.token {
            Token::TemplatePart { text, .. } => {
                let text = text.clone();
                let text_loc = p.mk_loc(&text_start_pos, &p.end_pos);
                let mut str_expr =
                    ast_helper::make_constant(Constant::String(text, Some("js".to_string())), text_loc);
                str_expr.pexp_attributes.push(template_literal_attr.clone());
                p.next();
                let expr = parse_expr(p);
                parts.push((str_expr, Some(expr)));
                p.next_template_literal_token();
            }
            Token::TemplateTail { text, .. } => {
                let text = text.clone();
                let text_loc = p.mk_loc(&text_start_pos, &p.end_pos);
                let mut str_expr =
                    ast_helper::make_constant(Constant::String(text, Some("js".to_string())), text_loc);
                str_expr.pexp_attributes.push(template_literal_attr.clone());
                parts.push((str_expr, None));
                p.next();
                break;
            }
            Token::Backtick => {
                // Empty template string
                let text_loc = p.mk_loc(&text_start_pos, &p.end_pos);
                let mut str_expr = ast_helper::make_constant(
                    Constant::String("".to_string(), Some("js".to_string())),
                    text_loc,
                );
                str_expr.pexp_attributes.push(template_literal_attr.clone());
                parts.push((str_expr, None));
                p.next();
                break;
            }
            Token::Eof => {
                p.err(DiagnosticCategory::Message(
                    "Unterminated template literal".to_string(),
                ));
                break;
            }
            _ => {
                p.err(DiagnosticCategory::Message(format!(
                    "Unexpected token in template literal: {:?}",
                    p.token
                )));
                p.next();
                break;
            }
        }
    }

    let strings: Vec<Expression> = parts.iter().map(|(s, _)| s.clone()).collect();
    let values: Vec<Expression> = parts.iter().filter_map(|(_, v)| v.clone()).collect();

    let strings_array = Expression {
        pexp_desc: ExpressionDesc::Pexp_array(strings),
        pexp_loc: Location::none(),
        pexp_attributes: vec![],
    };
    let values_array = Expression {
        pexp_desc: ExpressionDesc::Pexp_array(values),
        pexp_loc: Location::none(),
        pexp_attributes: vec![],
    };

    let tagged_template_attr = (
        mknoloc("res.taggedTemplate".to_string()),
        Payload::PStr(vec![]),
    );

    let loc = p.mk_loc(&tag_start, &p.prev_end_pos);

    // If we failed to advance (EOF / malformed), make sure the location at least starts at the tag.
    let loc = if loc.loc_end.cnum == 0 {
        p.mk_loc(&tag_start, &template_start)
    } else {
        loc
    };

    Expression {
        pexp_desc: ExpressionDesc::Pexp_apply {
            funct: Box::new(tag_expr),
            args: vec![
                (ArgLabel::Nolabel, strings_array),
                (ArgLabel::Nolabel, values_array),
            ],
            partial: false,
            transformed_jsx: false,
        },
        pexp_loc: loc,
        pexp_attributes: vec![tagged_template_attr],
    }
}

/// Parse a template literal (untagged).
fn parse_template_literal(p: &mut Parser<'_>) -> Expression {
    parse_template_literal_with_prefix(p, None)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;

    /// Default timeout for parsing operations (5 seconds - very generous for ms-scale ops)
    const PARSE_TIMEOUT: Duration = Duration::from_secs(5);

    /// Parse source code into an expression with a timeout.
    /// Panics if parsing takes longer than PARSE_TIMEOUT.
    fn parse_expr_with_timeout(source: &str) -> Expression {
        let source_owned = source.to_string();
        let source_for_error = source_owned.clone();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut parser = Parser::new("test.res", &source_owned);
            let expr = parse_expr(&mut parser);
            let _ = tx.send(expr);
        });

        match rx.recv_timeout(PARSE_TIMEOUT) {
            Ok(result) => result,
            Err(mpsc::RecvTimeoutError::Timeout) => {
                panic!(
                    "Parser timed out after {:?} for input: {}",
                    PARSE_TIMEOUT, source_for_error
                )
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                panic!("Parser thread panicked for input: {}", source_for_error)
            }
        }
    }

    /// Parse source code into a constant with a timeout.
    /// Panics if parsing takes longer than PARSE_TIMEOUT.
    fn parse_constant_with_timeout(source: &str) -> Constant {
        let source_owned = source.to_string();
        let source_for_error = source_owned.clone();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut parser = Parser::new("test.res", &source_owned);
            let c = parse_constant(&mut parser);
            let _ = tx.send(c);
        });

        match rx.recv_timeout(PARSE_TIMEOUT) {
            Ok(result) => result,
            Err(mpsc::RecvTimeoutError::Timeout) => {
                panic!(
                    "Parser timed out after {:?} for input: {}",
                    PARSE_TIMEOUT, source_for_error
                )
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                panic!("Parser thread panicked for input: {}", source_for_error)
            }
        }
    }

    #[test]
    fn test_parse_constant_int() {
        let c = parse_constant_with_timeout("42");
        assert!(matches!(c, Constant::Integer(n, None) if n == "42"));
    }

    #[test]
    fn test_parse_constant_float() {
        let c = parse_constant_with_timeout("3.14");
        assert!(matches!(c, Constant::Float(n, None) if n == "3.14"));
    }

    #[test]
    fn test_parse_constant_string() {
        let c = parse_constant_with_timeout("\"hello\"");
        assert!(matches!(c, Constant::String(s, None) if s == "hello"));
    }

    #[test]
    fn test_parse_expr_int() {
        let expr = parse_expr_with_timeout("42");
        assert!(matches!(
            expr.pexp_desc,
            ExpressionDesc::Pexp_constant(Constant::Integer(..))
        ));
    }

    #[test]
    fn test_parse_expr_ident() {
        let expr = parse_expr_with_timeout("foo");
        assert!(matches!(expr.pexp_desc, ExpressionDesc::Pexp_ident(_)));
    }

    #[test]
    fn test_parse_expr_unit() {
        let expr = parse_expr_with_timeout("()");
        assert!(matches!(expr.pexp_desc, ExpressionDesc::Pexp_construct(..)));
    }

    #[test]
    fn test_parse_expr_true() {
        let expr = parse_expr_with_timeout("true");
        assert!(matches!(expr.pexp_desc, ExpressionDesc::Pexp_construct(..)));
    }

    #[test]
    fn test_parse_array() {
        let expr = parse_expr_with_timeout("[1, 2, 3]");
        assert!(matches!(expr.pexp_desc, ExpressionDesc::Pexp_array(_)));
    }

    #[test]
    fn test_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Constant>();
    }
}
