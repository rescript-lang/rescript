//! Type expression parsing for ReScript.
//!
//! This module contains the type expression parsing logic, converting tokens
//! into type AST nodes.

use std::collections::HashSet;

use crate::location::Position;
use crate::parse_arena::{Located, LocIdx};
use super::ast::*;
use super::core::{ast_helper, mknoloc, recover, with_loc};
use super::diagnostics::DiagnosticCategory;
use super::grammar;
use super::grammar::Grammar;
use super::longident::Longident;
use super::state::{Parser, ParserMode};
use super::token::Token;

/// Get the string tag based on parser mode.
/// In typechecker mode, strings are tagged with "js" to indicate JS string literals.
fn get_string_tag(p: &Parser<'_>) -> Option<String> {
    if p.mode == ParserMode::ParseForTypeChecker {
        Some("js".to_string())
    } else {
        None
    }
}

// ============================================================================
// Type Variable Substitution
// ============================================================================

/// Transform a type by converting Ptyp_constr with a single Lident that matches
/// one of the poly type variables into Ptyp_var.
/// This is needed for locally abstract types: `type a b c. list<a, b, c>` should
/// have `a`, `b`, `c` as Ptyp_var, not Ptyp_constr.
pub fn substitute_type_vars(arena: &crate::parse_arena::ParseArena, typ: CoreType, vars: &HashSet<&str>) -> CoreType {
    let new_desc = match typ.ptyp_desc {
        CoreTypeDesc::Ptyp_constr(lid, args) => {
            // Check if this is a simple Lident that matches a type variable
            if args.is_empty() {
                if let Longident::Lident(name_idx) = arena.get_longident(lid.txt) {
                    let name = arena.get_string(*name_idx);
                    if vars.contains(name) {
                        // Convert to Ptyp_var
                        return CoreType {
                            ptyp_desc: CoreTypeDesc::Ptyp_var(name.to_string()),
                            ptyp_loc: typ.ptyp_loc,
                            ptyp_attributes: typ.ptyp_attributes,
                        };
                    }
                }
            }
            // Recursively process type arguments
            let new_args = args
                .into_iter()
                .map(|arg| substitute_type_vars(arena, arg, vars))
                .collect();
            CoreTypeDesc::Ptyp_constr(lid, new_args)
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, arity } => CoreTypeDesc::Ptyp_arrow {
            arg: Box::new(TypeArg {
                attrs: arg.attrs,
                lbl: arg.lbl,
                typ: substitute_type_vars(arena, arg.typ, vars),
            }),
            ret: Box::new(substitute_type_vars(arena, *ret, vars)),
            arity,
        },
        CoreTypeDesc::Ptyp_tuple(elements) => CoreTypeDesc::Ptyp_tuple(
            elements
                .into_iter()
                .map(|e| substitute_type_vars(arena, e, vars))
                .collect(),
        ),
        CoreTypeDesc::Ptyp_poly(poly_vars, body) => {
            CoreTypeDesc::Ptyp_poly(poly_vars, Box::new(substitute_type_vars(arena, *body, vars)))
        }
        CoreTypeDesc::Ptyp_alias(inner, alias) => {
            CoreTypeDesc::Ptyp_alias(Box::new(substitute_type_vars(arena, *inner, vars)), alias)
        }
        CoreTypeDesc::Ptyp_object(fields, closed) => {
            let new_fields = fields
                .into_iter()
                .map(|field| match field {
                    ObjectField::Otag(name, attrs, inner_typ) => {
                        ObjectField::Otag(name, attrs, substitute_type_vars(arena, inner_typ, vars))
                    }
                    ObjectField::Oinherit(inner_typ) => {
                        ObjectField::Oinherit(substitute_type_vars(arena, inner_typ, vars))
                    }
                })
                .collect();
            CoreTypeDesc::Ptyp_object(new_fields, closed)
        }
        CoreTypeDesc::Ptyp_variant(rows, closed, labels) => {
            let new_rows = rows
                .into_iter()
                .map(|row| match row {
                    RowField::Rtag(name, attrs, empty, types) => {
                        let new_types = types
                            .into_iter()
                            .map(|t| substitute_type_vars(arena, t, vars))
                            .collect();
                        RowField::Rtag(name, attrs, empty, new_types)
                    }
                    RowField::Rinherit(inner_typ) => {
                        RowField::Rinherit(substitute_type_vars(arena, inner_typ, vars))
                    }
                })
                .collect();
            CoreTypeDesc::Ptyp_variant(new_rows, closed, labels)
        }
        CoreTypeDesc::Ptyp_package((lid, constraints)) => {
            let new_constraints = constraints
                .into_iter()
                .map(|(c_lid, c_typ)| (c_lid, substitute_type_vars(arena, c_typ, vars)))
                .collect();
            CoreTypeDesc::Ptyp_package((lid, new_constraints))
        }
        // These don't contain nested types, return as-is
        other => other,
    };
    CoreType {
        ptyp_desc: new_desc,
        ptyp_loc: typ.ptyp_loc,
        ptyp_attributes: typ.ptyp_attributes,
    }
}

// ============================================================================
// Main Type Parsing
// ============================================================================

/// Parse a type expression.
pub fn parse_typ_expr(p: &mut Parser<'_>) -> CoreType {
    // Check for excessive parse depth to prevent stack overflow
    if p.exceeded_parse_depth() {
        p.err(DiagnosticCategory::Message(
            "Maximum parse depth exceeded".to_string(),
        ));
        return recover::default_type();
    }
    p.inc_parse_depth();

    let typ = parse_typ_expr_inner(p, true);
    let result = parse_type_alias(p, typ);

    p.dec_parse_depth();
    result
}

/// Parse a polymorphic type expression.
/// Like parse_typ_expr but with special handling for `'a => T` patterns.
/// OCaml's parse_poly_type_expr has a special case where single type vars
/// followed by `=>` use the type var's location for the arrow, not start_pos.
/// This is used for record field types where `'a => T` should NOT include
/// the `'` in the arrow type's location.
pub fn parse_poly_type_expr(p: &mut Parser<'_>) -> CoreType {
    // Special case: single type var followed by =>
    if p.token == Token::SingleQuote {
        // Try to parse as single type var
        let result = p.lookahead(|state| {
            state.next(); // consume '
            match &state.token {
                Token::Lident(_) | Token::Uident(_) => {
                    state.next(); // consume name
                    // Check if followed by => (not another type var or .)
                    state.token == Token::EqualGreater
                }
                _ => false,
            }
        });

        if result {
            // It's a single type var followed by =>
            p.next(); // consume '
            let var_start = p.start_pos.clone();
            let name = match &p.token {
                Token::Lident(n) | Token::Uident(n) => {
                    let n = n.clone();
                    p.next();
                    n
                }
                _ => "a".to_string(),
            };
            let var_loc = p.mk_loc_to_prev_end(&var_start);
            let typ = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                ptyp_loc: var_loc.clone(),
                ptyp_attributes: vec![],
            };
            p.next(); // consume =>
            let return_type = parse_typ_expr_inner(p, true);
            // Use typ.ptyp_loc.loc_start (the var's location, excluding ')
            let end_pos = p.prev_end_pos.clone();
            let loc = p.mk_loc_from_start_of(typ.ptyp_loc, &end_pos);
            let arg = TypeArg {
                attrs: vec![],
                lbl: ArgLabel::Nolabel,
                typ,
            };
            return CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_arrow {
                    arg: Box::new(arg),
                    ret: Box::new(return_type),
                    arity: Arity::Full(1),
                },
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            };
        }
    }

    // Fall through to regular type expression parsing
    let typ = parse_typ_expr_inner(p, true);
    parse_type_alias(p, typ)
}

/// Parse a type expression without allowing arrow types at the top level.
/// Used for return type annotations like `(x): int => body`.
pub fn parse_typ_expr_no_arrow(p: &mut Parser<'_>) -> CoreType {
    let typ = parse_typ_expr_inner(p, false);
    parse_type_alias(p, typ)
}

/// Parse a type expression with pre-parsed attributes.
fn parse_typ_expr_with_attrs(p: &mut Parser<'_>, attrs: Attributes) -> CoreType {
    let start_pos = p.start_pos.clone();
    let mut typ = parse_atomic_typ_expr(p, attrs, true);
    // Match OCaml: handle both EqualGreater and MinusGreater (error recovery)
    if matches!(p.token, Token::EqualGreater | Token::MinusGreater) {
        typ = parse_arrow_type_rest(p, typ, start_pos);
    }
    parse_type_alias(p, typ)
}

fn parse_type_alias(p: &mut Parser<'_>, typ: CoreType) -> CoreType {
    // Handle type aliases (type as 'name)
    if p.token == Token::As {
        p.next();
        p.expect(Token::SingleQuote);
        let var_name = match &p.token {
            Token::Lident(name) | Token::Uident(name) => {
                let name = name.clone();
                p.next();
                name
            }
            _ => {
                p.err(DiagnosticCategory::Message(
                    "Expected type variable after 'as'".to_string(),
                ));
                "a".to_string()
            }
        };
        let end_pos = p.prev_end_pos.clone();
        let loc = p.mk_loc_from_start_of(typ.ptyp_loc, &end_pos);
        return CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_alias(Box::new(typ), var_name),
            ptyp_loc: loc,
            ptyp_attributes: vec![],
        };
    }

    typ
}

/// Parse a type expression with optional ES6 arrow support.
fn parse_typ_expr_inner(p: &mut Parser<'_>, es6_arrow: bool) -> CoreType {
    let start_pos = p.start_pos.clone(); // Capture before parsing anything (including attributes)
    let attrs = parse_type_attributes(p);

    if es6_arrow && super::core::is_es6_arrow_type(p) {
        parse_es6_arrow_type(p, attrs)
    } else {
        let typ = parse_atomic_typ_expr(p, attrs.clone(), es6_arrow);
        // Match OCaml: handle both EqualGreater and MinusGreater (error recovery)
        // When MinusGreater is found where EqualGreater is expected, generate error and continue
        if es6_arrow && matches!(p.token, Token::EqualGreater | Token::MinusGreater) {
            parse_arrow_type_rest(p, typ, start_pos)
        } else {
            typ
        }
    }
}

/// Parse the rest of an arrow type.
/// `start_pos` is the position before parsing the first argument.
/// OCaml's parse_arrow_type_rest uses start_pos (before the argument) for location.
fn parse_arrow_type_rest(
    p: &mut Parser<'_>,
    param_type: CoreType,
    start_pos: crate::location::Position,
) -> CoreType {
    // Error recovery: if user wrote -> instead of =>, report error but continue
    // This matches OCaml's: if token = MinusGreater then Parser.expect EqualGreater p;
    if p.token == Token::MinusGreater {
        p.expect(Token::EqualGreater); // generates "Did you forget a `=>` here?"
    }
    p.next(); // consume => or ->
    // Parse return type without alias - `as` binds looser than `=>`
    // So `int => unit as 'a` is `(int => unit) as 'a`, not `int => (unit as 'a)`
    let return_type = parse_typ_expr_inner(p, true);
    // OCaml's parse_arrow_type_rest uses start_pos (before the argument),
    // NOT param_type.ptyp_loc.loc_start. This is different from the special case
    // in parse_poly_type_expr which uses typ.ptyp_loc.loc_start.
    let loc = p.mk_loc_to_prev_end(&start_pos);

    let arg = TypeArg {
        attrs: vec![],
        lbl: ArgLabel::Nolabel,
        typ: param_type,
    };

    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_arrow {
            arg: Box::new(arg),
            ret: Box::new(return_type),
            // Simple unlabeled arrow has arity 1
            arity: Arity::Full(1),
        },
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    }
}

/// Parse an ES6 arrow type starting at either `~` or a parenthesized parameter list.
fn parse_es6_arrow_type(p: &mut Parser<'_>, attrs: Attributes) -> CoreType {
    let start_pos = p.start_pos.clone();

    if p.token == Token::Tilde {
        p.next();
        let (name, name_loc) = parse_lident(p);
        // Pass grammar context for error message: "Did you forget a `:` here? It signals the start of a type"
        p.expect_with_grammar(Token::Colon, Some(Grammar::TypeExpression));

        let typ = parse_typ_expr_inner(p, false);
        let name_idx = p.arena_mut().intern_string(&name);
        let mut lbl = ArgLabel::Labelled(Located::new(name_idx, name_loc));

        // Optional labeled args: `~x: t=?`
        if p.token == Token::Equal {
            p.next();
            if p.token == Token::Question {
                p.next();
                if let ArgLabel::Labelled(name) = lbl {
                    lbl = ArgLabel::Optional(name);
                }
            } else {
                p.err(DiagnosticCategory::Message(
                    "Expected '?' after '=' in optional argument type".to_string(),
                ));
            }
        }

        p.expect(Token::EqualGreater);
        // Parse return type without alias - `as` binds looser than `=>`
        let return_type = parse_typ_expr_inner(p, true);

        // OCaml uses prev_end_pos (actual end after parsing return type), not return_type's ptyp_loc.loc_end
        let loc = p.mk_loc_to_prev_end(&start_pos);
        let arg = TypeArg {
            attrs,
            lbl,
            typ,
        };

        CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_arrow {
                arg: Box::new(arg),
                ret: Box::new(return_type),
                // OCaml uses ~arity:None for parsed type expressions
                arity: Arity::Unknown,
            },
            ptyp_loc: loc,
            ptyp_attributes: vec![],
        }
    } else {
        p.expect(Token::Lparen);
        let mut typ = parse_function_type(p, start_pos);
        if !attrs.is_empty() {
            typ.ptyp_attributes.extend(attrs);
        }
        typ
    }
}

// ============================================================================
// Atomic Type Parsing
// ============================================================================

/// Parse attributes that can appear before a type (including doc comments).
fn parse_type_attributes(p: &mut Parser<'_>) -> Attributes {
    let mut attrs = vec![];
    loop {
        match &p.token {
            Token::At => {
                // Capture start_pos BEFORE consuming @ so attribute location includes the @
                let start_pos = p.start_pos.clone();
                p.next();
                // Parse attribute id (possibly with path)
                let id = parse_attribute_id(p);
                // Capture end position for attribute id BEFORE parsing payload
                // OCaml: Location.mkloc id (mk_loc start_pos end_pos) before parse_payload
                let id_end_pos = p.prev_end_pos.clone();
                // Parse optional payload
                let payload = if p.token == Token::Lparen && p.start_pos.cnum == p.prev_end_pos.cnum {
                    parse_payload(p)
                } else {
                    Payload::PStr(vec![])
                };
                let loc = p.mk_loc(&start_pos, &id_end_pos);
                attrs.push((with_loc(id, loc), payload));
            }
            Token::DocComment { loc, content } => {
                let loc_cloned = loc.clone();
                let content = content.clone();
                p.next();
                let loc = p.from_location(&loc_cloned);
                attrs.push(super::core::doc_comment_to_attribute(loc, content));
            }
            _ => break,
        }
    }
    attrs
}

/// Parse an attribute id (name with optional path).
fn parse_attribute_id(p: &mut Parser<'_>) -> String {
    let mut id = String::new();
    loop {
        let name = match &p.token {
            Token::Lident(name) | Token::Uident(name) => Some(name.clone()),
            // Keywords can also be used as attribute names
            Token::As => Some("as".to_string()),
            Token::String(s) => Some(s.clone()),
            _ => None,
        };
        match name {
            Some(name) => {
                if !id.is_empty() {
                    id.push('.');
                }
                id.push_str(&name);
                p.next();
            }
            None => break,
        }
        if p.token == Token::Dot {
            p.next();
        } else {
            break;
        }
    }
    if id.is_empty() {
        p.err(DiagnosticCategory::Message(
            "Expected attribute name".to_string(),
        ));
        id = "error".to_string();
    }
    id
}

/// Parse an attribute payload (in type context).
fn parse_payload(p: &mut Parser<'_>) -> Payload {
    p.expect(Token::Lparen);
    let payload = super::module::parse_payload(p);
    p.expect(Token::Rparen);
    payload
}

/// Parse an atomic type expression.
fn parse_atomic_typ_expr(p: &mut Parser<'_>, attrs: Attributes, es6_arrow: bool) -> CoreType {
    p.leave_breadcrumb(Grammar::AtomicTypExpr);
    let start_pos = p.start_pos.clone();

    let mut typ = match &p.token {
        Token::Typ => {
            // Locally abstract types: type a b c. SomeType
            // This creates a Ptyp_poly with the type names as string variables
            // NOTE: We don't substitute Ptyp_constr -> Ptyp_var here. That's done
            // in module.rs where we have the full context of how the type is used.
            p.next();
            let mut vars: Vec<StringLoc> = vec![];

            // Collect all type variable names until we see a dot
            while let Token::Lident(name) = &p.token {
                let var_start = p.start_pos.clone();
                let name = name.clone();
                p.next();
                let var_loc = p.mk_loc_to_prev_end(&var_start);
                vars.push(with_loc(name, var_loc));
            }

            p.expect(Token::Dot);
            let body = parse_typ_expr(p);
            let loc = p.mk_loc_to_end_of(&start_pos, body.ptyp_loc);
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_poly(vars, Box::new(body)),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            }
        }
        Token::SingleQuote => {
            // Type variable: 'a
            // Polytype: 'a 'b. T
            let mut vars: Vec<StringLoc> = vec![];

            while p.token == Token::SingleQuote {
                p.next(); // consume the single quote first
                let var_start = p.start_pos.clone(); // capture start position AFTER the quote
                // OCaml: special case for EOF - emit unexpected token error
                let (name, var_loc) = if p.token == Token::Eof {
                    p.err(DiagnosticCategory::Unexpected {
                        token: p.token.clone(),
                        context: vec![],
                    });
                    (String::new(), p.mk_loc_to_prev_end(&var_start))
                } else {
                    // OCaml: parse_ident checks for keywords first
                    super::core::parse_ident(p, super::core::error_messages::TYPE_VAR, var_start.clone())
                };
                vars.push(with_loc(name, var_loc));
            }

            if p.token == Token::Dot && !vars.is_empty() {
                p.next();
                let body = parse_typ_expr(p);
                let loc = p.mk_loc_to_end_of(&start_pos, body.ptyp_loc);
                CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_poly(vars, Box::new(body)),
                    ptyp_loc: loc,
                    ptyp_attributes: vec![],
                }
            } else if let Some(first) = vars.first() {
                CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_var(first.txt.clone()),
                    ptyp_loc: first.loc.clone(), // Use the variable's location (excludes the ')
                    ptyp_attributes: vec![],
                }
            } else {
                p.err(DiagnosticCategory::Message(
                    super::core::error_messages::TYPE_VAR.to_string(),
                ));
                recover::default_type()
            }
        }
        Token::Underscore => {
            // Any type: _
            p.next();
            let loc = p.mk_loc_to_prev_end(&start_pos);
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_any,
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            }
        }
        Token::Lident(_) | Token::Uident(_) => parse_type_constr(p),
        Token::Lparen => {
            p.next();
            // Disambiguate `( ... ) => ...` function types from parenthesized/tuple types.
            //
            // ReScript function types with multiple or labeled arguments use a parenthesized
            // parameter list, e.g. `(~x: int, ~y: int) => int`.
            //
            // Note that `(t1 => t2) => t3` is *not* a parameter list, it's a normal arrow type
            // taking a function. So we only treat `( ... ) => ...` as a parameter-list function
            // type when the parens look like a parameter list (start with `~`/`.`/`@` or contain
            // a top-level comma).
            let is_function_type = es6_arrow && p.lookahead(|state| {
                let starts_like_params = matches!(state.token, Token::Tilde | Token::Dot | Token::At);
                let is_empty = state.token == Token::Rparen;

                let mut depth = 0;
                let mut saw_top_level_comma = false;

                while state.token != Token::Eof {
                    match state.token {
                        Token::Lparen => depth += 1,
                        Token::Rparen => {
                            if depth == 0 {
                                state.next();
                                let followed_by_arrow = state.token == Token::EqualGreater;
                                return followed_by_arrow
                                    && !is_empty
                                    && (starts_like_params || saw_top_level_comma);
                            }
                            depth -= 1;
                        }
                        Token::Comma if depth == 0 => {
                            saw_top_level_comma = true;
                        }
                        _ => {}
                    }
                    state.next();
                }
                false
            });

            if is_function_type {
                parse_function_type(p, start_pos)
            } else if p.token == Token::Rparen {
                // Unit type: ()
                // OCaml: Ast_helper.Typ.constr ~attrs unit_constr []
                // No ~loc passed, so ptyp_loc defaults to Location.none (ghost)
                // But lid_loc uses the real location
                p.next();
                let lid_loc = p.mk_loc_to_prev_end(&start_pos);
                let lid_idx = p.push_lident_static("unit");
                ast_helper::make_type_constr(lid_idx, vec![], lid_loc, LocIdx::none())
            } else {
                // Parenthesized type or tuple
                let typ = parse_typ_expr(p);
                if p.token == Token::Comma {
                    // Tuple type
                    parse_tuple_type(p, start_pos, typ)
                } else {
                    p.expect(Token::Rparen);
                    // OCaml includes the surrounding parentheses in the type's location
                    CoreType {
                        ptyp_loc: p.mk_loc_to_prev_end(&start_pos),
                        ..typ
                    }
                }
            }
        }
        Token::Lbrace => parse_record_or_object_type(p),
        Token::Lbracket => parse_poly_variant_type(p),
        Token::Hash => parse_poly_variant_type_simple(p),
        Token::Module => {
            p.next();
            parse_package_type_with_parens(p, start_pos)
        }
        Token::Percent => {
            // Extension: %ext
            let ext = parse_extension(p);
            let loc = p.mk_loc_to_prev_end(&start_pos);
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_extension(ext),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            }
        }
        _ => {
            p.err_unexpected();
            // Try to skip tokens and retry parsing if we find a valid start token
            // This matches OCaml's skip_tokens_and_maybe_retry behavior
            if recover::skip_tokens_and_maybe_retry(p, grammar::is_atomic_typ_expr_start) {
                // Eat our breadcrumb first, then recurse
                // OCaml returns from match, then eat_breadcrumb runs
                p.eat_breadcrumb();
                return parse_atomic_typ_expr(p, attrs, es6_arrow);
            }
            recover::default_type()
        }
    };

    // Attach any leading attributes to the type
    if !attrs.is_empty() {
        typ.ptyp_attributes.extend(attrs);
    }

    p.eat_breadcrumb();
    typ
}

// ============================================================================
// Type Constructor
// ============================================================================

/// Parse a type constructor (path with optional type arguments).
fn parse_type_constr(p: &mut Parser<'_>) -> CoreType {
    let start_pos = p.start_pos.clone();

    // Parse the path
    let mut path_parts = vec![];
    loop {
        match &p.token {
            Token::Lident(name) | Token::Uident(name) => {
                path_parts.push(name.clone());
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

    let lid = super::core::build_longident(p.arena_mut(), &path_parts);
    let lid_idx = p.push_longident(lid);
    // Capture location of just the identifier (to match OCaml's lid.loc)
    let lid_end_pos = p.prev_end_pos.clone();
    let lid_loc = p.mk_loc(&start_pos, &lid_end_pos);

    // Parse optional type arguments
    let args = if p.token == Token::LessThan {
        p.set_diamond_mode();
        p.next();
        let args = parse_type_args(p);
        p.expect(Token::GreaterThan);
        p.pop_diamond_mode();
        args
    } else {
        vec![]
    };

    // ptyp_loc includes the full extent (identifier + type args)
    // Note: For Ldot types in certain contexts (type manifests, arrow parameters),
    // the location is adjusted later to exclude type args.
    let ptyp_loc = p.mk_loc_to_prev_end(&start_pos);
    ast_helper::make_type_constr(lid_idx, args, lid_loc, ptyp_loc)
}

/// Parse type arguments.
fn parse_type_args(p: &mut Parser<'_>) -> Vec<CoreType> {
    let mut args = vec![];

    while p.token != Token::GreaterThan && p.token != Token::Eof {
        args.push(parse_typ_expr(p));
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    args
}

// ============================================================================
// Function Type
// ============================================================================

/// Parse a function type with labeled arguments.
fn parse_function_type(p: &mut Parser<'_>, start_pos: crate::location::Position) -> CoreType {
    // Capture position before the first uncurried marker - OCaml includes . in argument type location
    let first_uncurried_start = p.start_pos.clone();
    // Skip uncurried marker
    let first_has_dot = p.optional(&Token::Dot);

    // Track both the TypeArg and its start position (for arrow location)
    let mut params: Vec<(TypeArg, crate::location::Position)> = vec![];
    let mut is_first_param = true;

    while p.token != Token::Rparen && p.token != Token::Eof {
        // Capture position before the uncurried marker - OCaml includes . in argument type location
        let uncurried_start = if is_first_param {
            first_uncurried_start.clone()
        } else {
            p.start_pos.clone()
        };
        // Allow dotted parameters for uncurried segments (ignored for now).
        let has_dot = if is_first_param {
            is_first_param = false;
            first_has_dot
        } else {
            p.optional(&Token::Dot)
        };
        // OCaml handles doc comments separately from regular attributes for location purposes.
        // In OCaml parse_type_parameter:
        // 1. DocComment is consumed first (doc_attr)
        // 2. start_pos is captured AFTER doc comment
        // 3. Regular @-attributes are parsed after start_pos
        //
        // So doc comments should NOT be included in arrow location, but @-attrs SHOULD be.
        let mut doc_attrs = vec![];
        while let Token::DocComment { loc, content } = &p.token {
            let loc = loc.clone();
            let content = content.clone();
            p.next();
            let loc = p.from_location(&loc);
            doc_attrs.push(super::core::doc_comment_to_attribute(loc, content));
        }

        // Now capture the start position for the arrow - AFTER doc comments but BEFORE @-attrs
        // This matches OCaml: let start_pos = p.Parser.start_pos in
        let post_doc_start = p.start_pos.clone();

        // Parse regular @-style attributes
        let regular_attrs = parse_regular_attributes(p);

        // Combine: doc_attrs @ regular_attrs (OCaml: doc_attr @ parse_attributes p)
        let param_attrs: Attributes = doc_attrs.into_iter().chain(regular_attrs.into_iter()).collect();

        // Capture start position before label - OCaml includes the ~ in arrow location
        // But if there's an uncurried marker (.), the arrow should start at the .
        // The uncurried marker takes precedence over @-attributes for the start position
        // Doc comments do NOT affect the arrow start position
        let has_regular_attrs = param_attrs.iter().any(|(name, _)| name.txt != "res.doc");
        let param_start = if has_dot {
            // For uncurried parameters, OCaml includes the . in arrow location
            uncurried_start.clone()
        } else if has_regular_attrs {
            // If there are @-attributes (but no uncurried marker), start at the attribute
            post_doc_start.clone()
        } else {
            p.start_pos.clone()
        };

        let (mut label, mut typ) = if p.token == Token::Tilde {
            p.next();
            let (name, name_loc) = parse_lident(p);
            // Pass grammar context for error message: "Did you forget a `:` here? It signals the start of a type"
            p.expect_with_grammar(Token::Colon, Some(Grammar::TypeExpression));
            let typ = parse_typ_expr(p);
            let name_idx = p.arena_mut().intern_string(&name);
            (ArgLabel::Labelled(Located::new(name_idx, name_loc)), typ)
        } else if matches!(p.token, Token::Lident(_)) {
            // Check if this is `name: type` (missing tilde)
            // OCaml: parse Lident first, then check for Colon
            let lookahead_is_labeled = p.lookahead(|state| {
                if matches!(state.token, Token::Lident(_)) {
                    state.next();
                    state.token == Token::Colon
                } else {
                    false
                }
            });
            if lookahead_is_labeled {
                // This is `name: type` but missing the tilde
                let (name, name_loc) = parse_lident(p);
                // Emit "missing tilde" error at the identifier location
                let error_msg = super::core::error_messages::missing_tilde_labeled_parameter(&name);
                let start_pos = p.loc_start(name_loc);
                let end_pos = p.loc_end(name_loc);
                p.err_at(
                    start_pos,
                    end_pos,
                    DiagnosticCategory::Message(error_msg),
                );
                // Consume the colon
                p.next();
                let typ = parse_typ_expr(p);
                let name_idx = p.arena_mut().intern_string(&name);
                // Check for `=?` (optional without default)
                if p.token == Token::Equal {
                    p.next();
                    p.expect(Token::Question);
                    (ArgLabel::Optional(Located::new(name_idx, name_loc)), typ)
                } else {
                    (ArgLabel::Labelled(Located::new(name_idx, name_loc)), typ)
                }
            } else {
                // Regular type expression
                let typ = parse_typ_expr(p);
                (ArgLabel::Nolabel, typ)
            }
        } else {
            let typ = parse_typ_expr(p);
            (ArgLabel::Nolabel, typ)
        };

        // OCaml includes the uncurried marker (.) in the argument type's location,
        // but only for UNLABELED simple Ptyp_constr types (Lident), not for labeled arguments
        // or qualified types (Ldot) or other types like Ptyp_var.
        // Also, don't modify ghost locations (synthesized unit types from empty parens)
        // Don't modify parenthesized types with inner attributes - if param_attrs is empty
        // but typ already has attributes, the attrs came from inside parens like (@attr float)
        if has_dot && matches!(label, ArgLabel::Nolabel) && !typ.ptyp_loc.is_none() {
            let is_simple_constr = matches!(
                &typ.ptyp_desc,
                CoreTypeDesc::Ptyp_constr(lid, _) if p.arena().is_simple_lident(lid.txt)
            );
            // If param_attrs is empty but the type already has attributes, it was parsed as
            // a parenthesized expression with inner attributes like `(@attr float)`.
            // In that case, OCaml doesn't extend the type location to include the `.`
            let was_not_parenthesized_with_inner_attrs =
                !param_attrs.is_empty() || typ.ptyp_attributes.is_empty();
            if is_simple_constr && was_not_parenthesized_with_inner_attrs {
                let end_pos = p.loc_end(typ.ptyp_loc).clone();
                typ = CoreType {
                    ptyp_loc: p.mk_loc_from_positions(&uncurried_start, &end_pos),
                    ..typ
                };
            }

            // For direct arrow types like `. unit => unit`, OCaml includes the `.` in both
            // the arrow's location AND the first argument's location.
            // But for nested/parenthesized arrows like `. (. string) => unit`, don't extend
            // because the inner arrow has its own `.` handling.
            // We detect direct arrows by checking if the arrow's first arg starts exactly
            // at post_doc_start. For nested arrows, the inner `.` extension moves the
            // first arg location before post_doc_start.
            if let CoreTypeDesc::Ptyp_arrow { arg, ret, arity } = typ.ptyp_desc {
                let first_arg_start = p.loc_start(arg.typ.ptyp_loc);
                // If first arg location starts exactly at post_doc_start, it's a direct arrow
                // If it starts elsewhere (before due to inner `.` or after due to parens), don't extend
                let is_direct_arrow = first_arg_start.cnum == post_doc_start.cnum;
                if is_direct_arrow {
                    // Extend arrow location to include `.`
                    let end_pos = p.loc_end(typ.ptyp_loc).clone();
                    let new_arrow_loc = p.mk_loc_from_positions(&uncurried_start, &end_pos);

                    // Also extend first argument's location if it's a simple Lident
                    let new_arg = if matches!(
                        &arg.typ.ptyp_desc,
                        CoreTypeDesc::Ptyp_constr(lid, _) if p.arena().is_simple_lident(lid.txt)
                    ) && !arg.typ.ptyp_loc.is_none() {
                        let arg_end = p.loc_end(arg.typ.ptyp_loc).clone();
                        let new_arg_loc = p.mk_loc_from_positions(&uncurried_start, &arg_end);
                        TypeArg {
                            typ: CoreType {
                                ptyp_loc: new_arg_loc,
                                ..arg.typ
                            },
                            ..(*arg)
                        }
                    } else {
                        *arg
                    };

                    typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_arrow {
                            arg: Box::new(new_arg),
                            ret,
                            arity,
                        },
                        ptyp_loc: new_arrow_loc,
                        ptyp_attributes: typ.ptyp_attributes,
                    };
                } else {
                    // Restore the type without modification
                    typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_arrow { arg, ret, arity },
                        ..typ
                    };
                }
            }
        }

        // Optional labeled args: `~x: t=?`
        if p.token == Token::Equal {
            p.next();
            if p.token == Token::Question {
                p.next();
                if let ArgLabel::Labelled(name) = label {
                    label = ArgLabel::Optional(name);
                }
            } else {
                p.err(DiagnosticCategory::Message(
                    "Expected '?' after '=' in optional argument type".to_string(),
                ));
            }
        }

        // For unlabeled arguments with attributes, OCaml behavior depends on the type:
        // - For simple Lident types (like `@ignore unit`): type location includes attrs, attrs on type
        // - For other types: type location unchanged, attrs prepended to ptyp_attributes
        // In both cases, the arrow's attrs are empty for unlabeled args with outer attrs
        // Don't modify ghost locations (synthesized unit types from empty parens)
        let arrow_attrs = if matches!(label, ArgLabel::Nolabel) && !param_attrs.is_empty() {
            let is_simple_lident = matches!(
                &typ.ptyp_desc,
                CoreTypeDesc::Ptyp_constr(lid, _) if p.arena().is_simple_lident(lid.txt)
            );
            if is_simple_lident && !typ.ptyp_loc.is_none() {
                // For simple Lident types, OCaml creates the type with location starting at attrs
                // and attrs passed directly to Ast_helper.Typ.constr
                // If there's an uncurried marker (.), the location should start at the .
                // Doc comments don't affect type location, only @-attributes do
                let end_pos = p.loc_end(typ.ptyp_loc).clone();
                typ.ptyp_loc = if has_dot {
                    p.mk_loc_from_positions(&uncurried_start, &end_pos)
                } else if has_regular_attrs {
                    p.mk_loc_from_positions(&post_doc_start, &end_pos)
                } else {
                    typ.ptyp_loc
                };
                typ.ptyp_attributes = [param_attrs, typ.ptyp_attributes].concat();
            } else {
                // For other types (or ghost locations), attrs are prepended but location unchanged
                typ.ptyp_attributes = [param_attrs, typ.ptyp_attributes].concat();
            }
            // For unlabeled args, attributes go on type, not on arrow
            vec![]
        } else {
            param_attrs
        };

        params.push((TypeArg {
            attrs: arrow_attrs,
            lbl: label,
            typ,
        }, param_start));

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rparen);
    let rparen_end_pos = p.prev_end_pos.clone();
    p.expect(Token::EqualGreater);
    // Parse return type without alias - `as` binds looser than `=>`
    let return_type = parse_typ_expr_inner(p, true);
    // OCaml uses prev_end_pos (actual end after parsing return type) for arrow locations,
    // not return_type's ptyp_loc.loc_end. This matters for Ptyp_variant return types
    // whose ptyp_loc doesn't include the closing ]
    let arrow_end_pos = p.prev_end_pos.clone();

    if params.is_empty() {
        // OCaml: Ast_helper.Typ.constr unit_constr []
        // No ~loc passed, so ptyp_loc defaults to Location.none (ghost)
        // But lid_loc uses the real location
        let lid_loc = p.mk_loc(&start_pos, &rparen_end_pos);
        let lid_idx = p.push_lident_static("unit");
        params.push((TypeArg {
            attrs: vec![],
            lbl: ArgLabel::Nolabel,
            typ: ast_helper::make_type_constr(lid_idx, vec![], lid_loc, LocIdx::none()),
        }, start_pos.clone()));
    }

    // The total arity is the number of parameters
    // OCaml: When in external, decrement arity for labeled @as(_) parameters
    // These are parameters where: label is Labelled, type is Ptyp_any, and has @as attribute
    let as_underscore_count = if p.in_external {
        params.iter().filter(|(param, _)| {
            matches!(param.lbl, ArgLabel::Labelled(_))
                && matches!(param.typ.ptyp_desc, CoreTypeDesc::Ptyp_any)
                && param.typ.ptyp_attributes.iter().any(|(attr, _)| attr.txt == "as")
        }).count()
    } else {
        0
    };
    let total_arity = params.len() - as_underscore_count;

    // Build the arrow type from right to left, tracking the end position
    // The first iteration uses arrow_end_pos, subsequent iterations use the arrow's end
    let mut acc = return_type;
    let mut end_pos = arrow_end_pos;
    for (param, param_start) in params.into_iter().rev() {
        // Each arrow's location spans from the label (if any) to the end of return
        // OCaml includes the ~ in the location for labeled arguments
        let arrow_loc = p.mk_loc_from_positions(&param_start, &end_pos);
        let arrow = CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_arrow {
                arg: Box::new(param),
                ret: Box::new(acc),
                // Inner arrows don't have arity annotation
                arity: Arity::Unknown,
            },
            ptyp_loc: arrow_loc,
            ptyp_attributes: vec![],
        };
        // Next iteration should use this arrow's end position
        end_pos = p.loc_end(arrow.ptyp_loc).clone();
        acc = arrow;
    }
    let result = acc;

    // Update the outermost arrow to have the full arity
    let result = match result.ptyp_desc {
        CoreTypeDesc::Ptyp_arrow { arg, ret, .. } => CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_arrow {
                arg,
                ret,
                arity: Arity::Full(total_arity),
            },
            ptyp_loc: result.ptyp_loc,
            ptyp_attributes: result.ptyp_attributes,
        },
        _ => result,
    };

    // The outermost arrow should start at start_pos (the opening paren) not at first arg
    let end_pos = p.loc_end(result.ptyp_loc).clone();
    CoreType {
        ptyp_loc: p.mk_loc_from_positions(&start_pos, &end_pos),
        ..result
    }
}

/// Parse a lowercase identifier.
fn parse_lident(p: &mut Parser<'_>) -> (String, Location) {
    let start_pos = p.start_pos.clone();
    match &p.token {
        Token::Lident(name) => {
            let name = name.clone();
            p.next();
            let loc = p.mk_loc_to_prev_end(&start_pos);
            (name, loc)
        }
        _ => {
            // OCaml uses Lident diagnostic here which produces context-sensitive error
            p.err(DiagnosticCategory::Lident(p.token.clone()));
            p.next(); // Consume the invalid token
            let loc = p.mk_loc_to_prev_end(&start_pos);
            ("_".to_string(), loc)
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
                let loc_cloned = loc.clone();
                let content = content.clone();
                p.next();
                let loc = p.from_location(&loc_cloned);
                attrs.push(super::core::doc_comment_to_attribute(loc, content));
            }
            _ => break,
        }
    }
    attrs
}

/// Parse only @-style attributes (NOT doc comments).
/// Used when doc comments need to be handled separately for location purposes.
fn parse_regular_attributes(p: &mut Parser<'_>) -> Attributes {
    let mut attrs = vec![];
    while p.token == Token::At {
        if let Some(attr) = parse_attribute(p) {
            attrs.push(attr);
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

    let attr_id_str = parse_attribute_id(p);
    let attr_id = with_loc(attr_id_str, p.mk_loc_to_prev_end(&start_pos));

    // Only parse Lparen as payload if it's immediately adjacent to the attribute ID
    let is_adjacent = p.start_pos.cnum == p.prev_end_pos.cnum;
    let payload = if p.token == Token::Lparen && is_adjacent {
        p.next();
        let payload = parse_attribute_payload(p);
        p.expect(Token::Rparen);
        payload
    } else {
        Payload::PStr(vec![])
    };

    Some((attr_id, payload))
}

/// Parse an attribute payload.
fn parse_attribute_payload(p: &mut Parser<'_>) -> Payload {
    match &p.token {
        Token::String(s) => {
            let value = s.clone();
            let tag = get_string_tag(p);
            // IMPORTANT: Create separate locations for expression and structure_item
            // to match OCaml's allocation pattern (each gets its own Location object)
            let pexp_loc = p.mk_loc_current();
            let pstr_loc = p.mk_loc_current();
            p.next();
            // Create Pstr_eval(Pexp_constant(Pconst_string(s, tag)))
            let expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_constant(Constant::String(value, tag)),
                pexp_loc,
                pexp_attributes: vec![],
            };
            let item = StructureItem {
                pstr_desc: StructureItemDesc::Pstr_eval(expr, vec![]),
                pstr_loc,
            };
            Payload::PStr(vec![item])
        }
        Token::Rparen => {
            // Empty payload
            Payload::PStr(vec![])
        }
        _ => {
            // Try to parse as an expression
            // Capture start_pos BEFORE parsing, like OCaml does in parse_payload
            let start_pos = p.start_pos.clone();
            let expr = super::expr::parse_expr(p);
            // OCaml structure_item location spans from before expression to after
            let loc = p.mk_loc_to_prev_end(&start_pos);
            let item = StructureItem {
                pstr_desc: StructureItemDesc::Pstr_eval(expr, vec![]),
                pstr_loc: loc,
            };
            Payload::PStr(vec![item])
        }
    }
}

// ============================================================================
// Tuple Type
// ============================================================================

/// Parse a tuple type.
fn parse_tuple_type(
    p: &mut Parser<'_>,
    start_pos: crate::location::Position,
    first: CoreType,
) -> CoreType {
    let mut types = vec![first];

    while p.token == Token::Comma {
        p.next();
        // Handle trailing comma
        if p.token == Token::Rparen {
            break;
        }
        // Allow arrow types and type aliases in tuple elements
        let elem_typ = parse_typ_expr_inner(p, true);
        let elem_typ = parse_type_alias(p, elem_typ);
        types.push(elem_typ);
    }

    p.expect(Token::Rparen);

    // OCaml: A tuple needs at least two elements
    if types.len() == 1 {
        p.err_at(
            start_pos.clone(),
            p.prev_end_pos.clone(),
            DiagnosticCategory::Message("A tuple needs at least two elements".to_string()),
        );
    }

    let loc = p.mk_loc_to_prev_end(&start_pos);

    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_tuple(types),
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    }
}

// ============================================================================
// Record/Object Type
// ============================================================================

/// Parse a record or object type.
fn parse_record_or_object_type(p: &mut Parser<'_>) -> CoreType {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lbrace);

    // Check for object type: {. ... } or {.. ... }
    let is_object = p.token == Token::Dot || p.token == Token::DotDot;
    let closed = if p.token == Token::DotDot {
        p.next();
        ClosedFlag::Open
    } else if p.token == Token::Dot {
        p.next();
        ClosedFlag::Closed
    } else {
        ClosedFlag::Closed
    };

    let fields = parse_object_fields(p);
    p.expect(Token::Rbrace);
    let loc = p.mk_loc_to_prev_end(&start_pos);
    let typ = CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_object(fields, closed),
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    };
    // Handle type alias: {...} as 'name
    let typ = parse_type_alias(p, typ);
    // Note: Don't parse arrow rest here - let the caller (parse_typ_expr_inner) handle it.
    // This is important for correct precedence of arrows in function return types.
    typ
}

/// Parse object type body after `{` has been consumed.
/// This is used by constructor argument parsing where OCaml captures start_pos AFTER consuming `{`.
/// The caller should pass the position AFTER consuming `{` as `start_pos`.
pub fn parse_object_type_body(p: &mut Parser<'_>, start_pos: Position) -> CoreType {
    // Check for open/closed markers: {. ... } or {.. ... }
    let closed = if p.token == Token::DotDot {
        p.next();
        ClosedFlag::Open
    } else if p.token == Token::Dot {
        p.next();
        ClosedFlag::Closed
    } else {
        ClosedFlag::Closed
    };

    let fields = parse_object_fields(p);
    p.expect(Token::Rbrace);
    let loc = p.mk_loc_to_prev_end(&start_pos);
    let typ = CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_object(fields, closed),
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    };
    // Handle type alias: {...} as 'name
    let typ = parse_type_alias(p, typ);
    // Handle arrow rest if present
    // Match OCaml: handle both EqualGreater and MinusGreater (error recovery)
    if matches!(p.token, Token::EqualGreater | Token::MinusGreater) {
        parse_arrow_type_rest(p, typ, start_pos)
    } else {
        typ
    }
}

/// Parse object fields.
fn parse_object_fields(p: &mut Parser<'_>) -> Vec<ObjectField> {
    let mut fields = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        // Optional attributes on the field.
        let field_attrs = parse_attributes(p);

        // Handle spread: ...typ
        if p.token == Token::DotDotDot {
            p.next();
            let typ = parse_typ_expr(p);
            fields.push(ObjectField::Oinherit(typ));
            if !p.optional(&Token::Comma) {
                break;
            }
            continue;
        }

        // Handle optional field: name?: typ
        let name = match &p.token {
            Token::Lident(n) => Some(n.clone()),
            Token::String(s) => Some(s.clone()),
            _ => None,
        };
        match name {
            Some(name) => {
                let field_start = p.start_pos.clone();
                p.next();
                // OCaml's Otag location only includes the field name, not the type
                let name_loc = p.mk_loc_to_prev_end(&field_start);
                // Check for optional field marker
                let _is_optional = p.optional(&Token::Question);
                // Pass grammar context for error message: "Did you forget a `:` here? It signals the start of a type"
                p.expect_with_grammar(Token::Colon, Some(Grammar::TypeExpression));
                // OCaml uses parse_poly_type_expr for object field types
                let typ = parse_poly_type_expr(p);
                fields.push(ObjectField::Otag(with_loc(name, name_loc), field_attrs, typ));
            }
            None => {
                p.err(DiagnosticCategory::Message(
                    "Expected field name".to_string(),
                ));
                break;
            }
        }

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    fields
}

// ============================================================================
// Polymorphic Variant Type
// ============================================================================

/// Parse polymorphic variant type args: `(typ1, typ2, ...)` for intersection types.
/// OCaml's `parse_polymorphic_variant_type_args` behavior:
/// - For a single non-tuple type, returns it without updating location (no parens in loc)
/// - For multiple types or a single tuple, creates a Ptyp_tuple with full parenthesis location
fn parse_polymorphic_variant_type_args(p: &mut Parser<'_>) -> CoreType {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lparen);

    let mut args = vec![];
    while p.token != Token::Rparen && p.token != Token::Eof {
        args.push(parse_typ_expr(p));
        if !p.optional(&Token::Comma) {
            break;
        }
    }
    p.expect(Token::Rparen);
    let loc = p.mk_loc_to_prev_end(&start_pos);

    match args.as_slice() {
        // Single tuple type - wrap it in another tuple (OCaml's type checker mode)
        // Actually OCaml does: `if p.mode = ParseForTypeChecker then typ else Ast_helper.Typ.tuple`
        // For parity, we follow the else branch (default mode)
        [typ] if matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_tuple(_)) => {
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_tuple(args),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            }
        }
        // Single non-tuple type - return as-is (without parenthesis location!)
        [typ] => typ.clone(),
        // Multiple types - create tuple with full parenthesis location
        _ => CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_tuple(args),
            ptyp_loc: loc,
            ptyp_attributes: vec![],
        },
    }
}

/// Parse a polymorphic variant type.
fn parse_poly_variant_type(p: &mut Parser<'_>) -> CoreType {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lbracket);

    // Check for closed/open markers
    let (closed, mut low_tags) = match &p.token {
        Token::GreaterThan => {
            p.next();
            (ClosedFlag::Open, None)
        }
        Token::LessThan => {
            p.next();
            (ClosedFlag::Closed, Some(vec![]))
        }
        _ => (ClosedFlag::Closed, None),
    };

    // Parsing logic depends on the variant type:
    // - [> ...] (Open): can be empty, uses parse_row_fields which allows empty
    // - [< ...] (Closed with low_tags): requires at least one tag_spec_full
    // - [...] (Closed without marker): requires tag_spec_first then more tag_specs
    // Only [> ...] can be empty. For the other two, we need at least one row field.
    let fields = if closed == ClosedFlag::Open {
        // [> ...] can be empty
        parse_row_fields(p)
    } else if low_tags.is_some() {
        // [< ...] requires at least one tag_spec_full after optional Bar
        p.optional(&Token::Bar);
        parse_row_fields_with_required_first(p)
    } else {
        // [...] requires tag_spec_first | { | tag_spec }+
        // OCaml grammar: '[ tag_spec ]' where tag_spec_first is first | then more | tag_spec
        // When empty [], OCaml generates TWO type holes to indicate invalid variant
        parse_row_fields_no_marker(p)
    };

    // For [< ... > #X #Y ], parse the lower bound tag names
    if low_tags.is_some() && p.token == Token::GreaterThan {
        p.next();
        let mut tags = vec![];
        while p.token == Token::Hash {
            p.next();
            if let Token::Lident(name) | Token::Uident(name) = &p.token {
                tags.push(name.clone());
                p.next();
            }
        }
        low_tags = Some(tags);
    }

    // OCaml's Ptyp_variant location ends at prev_end_pos (end of last row field), BEFORE the closing ]
    let loc = p.mk_loc_to_prev_end(&start_pos);
    p.expect(Token::Rbracket);

    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_variant(fields, closed, low_tags),
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    }
}

/// Parse a simple polymorphic variant type starting with #.
fn parse_poly_variant_type_simple(p: &mut Parser<'_>) -> CoreType {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Hash);

    let tag = match &p.token {
        Token::Lident(name) | Token::Uident(name) => {
            let name = name.clone();
            p.next();
            name
        }
        Token::Int { i, suffix } => {
            // Clone values before borrowing p mutably
            let tag = i.clone();
            let has_suffix = suffix.is_some();
            // OCaml: emit error if numeric poly variant has a suffix
            if has_suffix {
                p.err(DiagnosticCategory::Message(format!(
                    "A numeric polymorphic variant cannot be followed by a letter. Did you mean `#{}`?",
                    tag
                )));
            }
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

    // Check for type arguments
    let args = if p.token == Token::Lparen {
        p.next();
        let mut args = vec![];
        while p.token != Token::Rparen && p.token != Token::Eof {
            // Allow arrow types in polymorphic variant arguments (e.g., #Foo(string => unit))
            args.push(parse_typ_expr_inner(p, true));
            if !p.optional(&Token::Comma) {
                break;
            }
        }
        p.expect(Token::Rparen);
        args
    } else {
        vec![]
    };

    let loc = p.mk_loc_to_prev_end(&start_pos);
    let tag_loc = loc.clone();

    let fields = vec![RowField::Rtag(
        with_loc(tag, tag_loc),
        vec![],
        args.is_empty(),
        args,
    )];

    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_variant(fields, ClosedFlag::Closed, None),
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    }
}

/// Check if the current token is `|` or is a doc comment followed by `|`.
/// This matches OCaml's is_bar_or_doc_comment_then_bar function.
fn is_bar_or_doc_comment_then_bar(p: &mut Parser<'_>) -> bool {
    match &p.token {
        Token::Bar => true,
        Token::DocComment { .. } => {
            // Look ahead to see if there's a Bar after the doc comment
            p.lookahead(|state| {
                state.next(); // skip doc comment
                matches!(state.token, Token::Bar)
            })
        }
        _ => false,
    }
}

/// Parse row fields for `[...]` syntax (no `<` or `>` marker).
/// OCaml grammar: '[ tag_spec ]' where tag_spec_first must be first, then | tag_spec...
/// When empty [], OCaml generates TWO type holes (not one) to represent an invalid variant.
fn parse_row_fields_no_marker(p: &mut Parser<'_>) -> Vec<RowField> {
    let mut fields = vec![];

    // Handle optional leading `|` with possible doc comment (e.g., [| #A] or [/** doc */ | #A])
    if is_bar_or_doc_comment_then_bar(p) {
        // Consume doc comment if present (it belongs to the tag after |)
        let doc_attrs = if let Token::DocComment { loc, content } = &p.token {
            let loc = loc.clone();
            let content = content.clone();
            p.next();
            let loc = p.from_location(&loc);
            vec![super::core::doc_comment_to_attribute(loc, content)]
        } else {
            vec![]
        };
        // Now consume the |
        p.expect(Token::Bar);
        // Parse the first tag spec with doc attrs
        let mut attrs = doc_attrs;
        attrs.extend(parse_attributes(p));
        if let Some(field) = parse_single_row_field_with_attrs(p, attrs) {
            fields.push(field);
        }
        // Parse remaining fields
        while p.token != Token::Rbracket && p.token != Token::GreaterThan && p.token != Token::Eof {
            if !is_bar_or_doc_comment_then_bar(p) {
                break;
            }
            // Consume doc comment if present
            let doc_attrs = if let Token::DocComment { loc, content } = &p.token {
                let loc = loc.clone();
                let content = content.clone();
                p.next();
                let loc = p.from_location(&loc);
                vec![super::core::doc_comment_to_attribute(loc, content)]
            } else {
                vec![]
            };
            p.optional(&Token::Bar);
            let mut attrs = doc_attrs;
            attrs.extend(parse_attributes(p));
            if let Some(field) = parse_single_row_field_with_attrs(p, attrs) {
                fields.push(field);
            }
        }
        return fields;
    }

    // First field is required - must be either #tag, doc comment, @attr, or a type expression
    let attrs = parse_attributes(p);
    if p.token == Token::Hash {
        // Parse tagged row field
        if let Some(field) = parse_single_row_field_with_attrs(p, attrs) {
            fields.push(field);
        }
    } else if super::grammar::is_typ_expr_start(&p.token) {
        // Parse inherited type (Rinherit)
        let typ = parse_typ_expr_with_attrs(p, attrs);
        fields.push(RowField::Rinherit(typ));
    } else {
        // Not a valid start - generate error and recover
        // For [...] syntax, OCaml generates TWO type holes (variant needs at least 2 options)
        p.err(DiagnosticCategory::Unexpected {
            token: p.token.clone(),
            context: vec![],
        });
        // Create two typeholes as recovery
        fields.push(RowField::Rinherit(recover::default_type()));
        fields.push(RowField::Rinherit(recover::default_type()));
        return fields;
    }

    // Parse remaining fields (same logic as parse_row_fields)
    while p.token != Token::Rbracket && p.token != Token::GreaterThan && p.token != Token::Eof {
        if !p.optional(&Token::Bar) {
            break;
        }
        let attrs = parse_attributes(p);
        if let Some(field) = parse_single_row_field_with_attrs(p, attrs) {
            fields.push(field);
        }
    }

    fields
}

/// Parse row fields with a required first field.
/// This matches OCaml's behavior for [< ... ] where at least one tag_spec_full is expected.
/// If the token is not a valid start of a row field, an error is generated.
fn parse_row_fields_with_required_first(p: &mut Parser<'_>) -> Vec<RowField> {
    let mut fields = vec![];

    // Handle optional leading `|` with possible doc comment (e.g., [| #A] or [/** doc */ | #A])
    // OCaml's parse_tag_spec_first handles this: (Bar | DocComment _) when is_bar_or_doc_comment_then_bar p
    if is_bar_or_doc_comment_then_bar(p) {
        // Consume doc comment if present (it belongs to the tag after |)
        let doc_attrs = if let Token::DocComment { loc, content } = &p.token {
            let loc = loc.clone();
            let content = content.clone();
            p.next();
            let loc = p.from_location(&loc);
            vec![super::core::doc_comment_to_attribute(loc, content)]
        } else {
            vec![]
        };
        // Now consume the |
        p.expect(Token::Bar);
        // Parse the first tag spec with doc attrs
        let mut attrs = doc_attrs;
        attrs.extend(parse_attributes(p));
        if let Some(field) = parse_single_row_field_with_attrs(p, attrs) {
            fields.push(field);
        }
        // Parse remaining fields
        while p.token != Token::Rbracket && p.token != Token::GreaterThan && p.token != Token::Eof {
            if !is_bar_or_doc_comment_then_bar(p) {
                break;
            }
            // Consume doc comment if present
            let doc_attrs = if let Token::DocComment { loc, content } = &p.token {
                let loc = loc.clone();
                let content = content.clone();
                p.next();
                let loc = p.from_location(&loc);
                vec![super::core::doc_comment_to_attribute(loc, content)]
            } else {
                vec![]
            };
            p.optional(&Token::Bar);
            let mut attrs = doc_attrs;
            attrs.extend(parse_attributes(p));
            if let Some(field) = parse_single_row_field_with_attrs(p, attrs) {
                fields.push(field);
            }
        }
        return fields;
    }

    // First field is required - must be either #tag, doc comment, @attr, or a type expression
    let attrs = parse_attributes(p);
    if p.token == Token::Hash {
        // Parse tagged row field
        if let Some(field) = parse_single_row_field_with_attrs(p, attrs) {
            fields.push(field);
        }
    } else if super::grammar::is_typ_expr_start(&p.token) {
        // Parse inherited type (Rinherit)
        let typ = parse_typ_expr_with_attrs(p, attrs);
        fields.push(RowField::Rinherit(typ));
    } else {
        // Not a valid start - generate error and recover
        // This includes the case where token is ] (empty [< ] is invalid)
        p.err(DiagnosticCategory::Unexpected {
            token: p.token.clone(),
            context: vec![],
        });
        // Create a typehole as recovery
        fields.push(RowField::Rinherit(recover::default_type()));
    }

    // Parse remaining fields (same logic as parse_row_fields)
    while p.token != Token::Rbracket && p.token != Token::GreaterThan && p.token != Token::Eof {
        if !p.optional(&Token::Bar) {
            break;
        }
        let attrs = parse_attributes(p);
        if let Some(field) = parse_single_row_field_with_attrs(p, attrs) {
            fields.push(field);
        }
    }

    fields
}

/// Parse a single row field with pre-parsed attributes.
fn parse_single_row_field_with_attrs(p: &mut Parser<'_>, attrs: Attributes) -> Option<RowField> {
    if p.token == Token::Hash {
        // Tagged row field: #tag or #tag(args)
        let hash_start = p.start_pos.clone();
        p.next();
        let tag = match &p.token {
            Token::Lident(name) | Token::Uident(name) => {
                let name = name.clone();
                let loc = p.mk_loc_to_end(&hash_start);
                p.next();
                with_loc(name, loc)
            }
            Token::Int { i, suffix } => {
                // Clone values before borrowing p mutably
                let tag = i.clone();
                let has_suffix = suffix.is_some();
                let loc = p.mk_loc_to_end(&hash_start);
                // OCaml: emit error if numeric poly variant has a suffix
                if has_suffix {
                    p.err(DiagnosticCategory::Message(format!(
                        "A numeric polymorphic variant cannot be followed by a letter. Did you mean `#{}`?",
                        tag
                    )));
                }
                p.next();
                with_loc(tag, loc)
            }
            Token::String(s) => {
                let tag = s.clone();
                let loc = p.mk_loc_to_end(&hash_start);
                p.next();
                with_loc(tag, loc)
            }
            Token::True => {
                let loc = p.mk_loc_to_end(&hash_start);
                p.next();
                with_loc("true".to_string(), loc)
            }
            Token::False => {
                let loc = p.mk_loc_to_end(&hash_start);
                p.next();
                with_loc("false".to_string(), loc)
            }
            _ => {
                p.err(DiagnosticCategory::Message(
                    "Expected variant tag after #".to_string(),
                ));
                mknoloc("error".to_string())
            }
        };

        // Parse arguments inside parentheses
        let (args, comma_separated, had_parens, lparen_pos, rparen_end_pos) = if p.token == Token::Lparen {
            let lparen_pos = p.start_pos.clone();
            p.next();
            let mut args = vec![];
            let mut has_comma = false;
            while p.token != Token::Rparen && p.token != Token::Eof {
                args.push(parse_typ_expr_inner(p, true));
                if p.optional(&Token::Comma) {
                    has_comma = true;
                } else if !p.optional(&Token::Ampersand) {
                    break;
                }
            }
            p.expect(Token::Rparen);
            let rparen_end_pos = p.prev_end_pos.clone();
            (args, has_comma, true, Some(lparen_pos), Some(rparen_end_pos))
        } else {
            (vec![], false, false, None, None)
        };

        let is_constant = !had_parens || args.is_empty();

        // Handle tuple wrapping for comma-separated args
        let first_type = if comma_separated && args.len() >= 2 {
            let loc = p.mk_loc(
                lparen_pos.as_ref().unwrap(),
                rparen_end_pos.as_ref().unwrap(),
            );
            Some(CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_tuple(args),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            })
        } else if args.len() == 1 {
            Some(args.into_iter().next().unwrap())
        } else {
            None
        };

        // Parse additional & constraints
        // OCaml expects parentheses after & and uses parse_polymorphic_variant_type_args
        // which returns the inner type without paren location for single types
        let mut all_types = first_type.into_iter().collect::<Vec<_>>();
        while p.optional(&Token::Ampersand) {
            if p.token == Token::Lparen {
                all_types.push(parse_polymorphic_variant_type_args(p));
            } else {
                all_types.push(parse_typ_expr_inner(p, true));
            }
        }

        Some(RowField::Rtag(tag, attrs, is_constant, all_types))
    } else if super::grammar::is_typ_expr_start(&p.token) {
        // Inherited type (Rinherit)
        let typ = parse_typ_expr_with_attrs(p, attrs);
        Some(RowField::Rinherit(typ))
    } else {
        None
    }
}

/// Parse row fields.
fn parse_row_fields(p: &mut Parser<'_>) -> Vec<RowField> {
    let mut fields = vec![];

    // Handle optional leading `|`
    p.optional(&Token::Bar);

    while p.token != Token::Rbracket && p.token != Token::Eof {
        // Parse attributes (including doc comments) before the tag
        let attrs = parse_attributes(p);

        // Doc comments may be followed by `|`, which we need to skip
        p.optional(&Token::Bar);

        if p.token == Token::Hash {
            // Tagged row field: #tag or #tag(args)
            // Capture the # position for location - OCaml includes # in the tag location
            let hash_start = p.start_pos.clone();
            p.next();
            let tag = match &p.token {
                Token::Lident(name) | Token::Uident(name) => {
                    let name = name.clone();
                    let loc = p.mk_loc_to_end(&hash_start);
                    p.next();
                    with_loc(name, loc)
                }
                Token::Int { i, suffix } => {
                    // Clone values before borrowing p mutably
                    let tag = i.clone();
                    let has_suffix = suffix.is_some();
                    let loc = p.mk_loc_to_end(&hash_start);
                    // OCaml: emit error if numeric poly variant has a suffix
                    if has_suffix {
                        p.err(DiagnosticCategory::Message(format!(
                            "A numeric polymorphic variant cannot be followed by a letter. Did you mean `#{}`?",
                            tag
                        )));
                    }
                    p.next();
                    with_loc(tag, loc)
                }
                Token::String(s) => {
                    let tag = s.clone();
                    let loc = p.mk_loc_to_end(&hash_start);
                    p.next();
                    with_loc(tag, loc)
                }
                Token::True => {
                    let loc = p.mk_loc_to_end(&hash_start);
                    p.next();
                    with_loc("true".to_string(), loc)
                }
                Token::False => {
                    let loc = p.mk_loc_to_end(&hash_start);
                    p.next();
                    with_loc("false".to_string(), loc)
                }
                _ => {
                    p.err(DiagnosticCategory::Message(
                        "Expected variant tag after #".to_string(),
                    ));
                    mknoloc("error".to_string())
                }
            };

            // Parse arguments inside parentheses
            // Track if they're comma-separated (tuple wrap) vs ampersand-separated (no wrap)
            let (args, comma_separated, had_parens, lparen_pos, rparen_end_pos) = if p.token == Token::Lparen {
                let lparen_pos = p.start_pos.clone();
                p.next();
                let mut args = vec![];
                let mut has_comma = false;
                while p.token != Token::Rparen && p.token != Token::Eof {
                    // Allow arrow types in polymorphic variant arguments (e.g., #Foo(string => unit))
                    args.push(parse_typ_expr_inner(p, true));
                    if p.optional(&Token::Comma) {
                        has_comma = true;
                    } else if !p.optional(&Token::Ampersand) {
                        break;
                    }
                }
                p.expect(Token::Rparen);
                let rparen_end_pos = p.prev_end_pos.clone();
                (args, has_comma, true, Some(lparen_pos), Some(rparen_end_pos))
            } else {
                (vec![], false, false, None, None)
            };

            // Track if the tag itself is constant (no args in parens)
            // #A is constant, #A(int) is not, #A&(...) is constant (& are constraints, not args)
            let is_constant = !had_parens || args.is_empty();

            // First, handle the initial arguments - wrap in tuple if comma-separated
            // For #E(int, int) & (...), we first need to make (int, int) a tuple type
            let first_type = if comma_separated && args.len() >= 2 {
                // OCaml includes the parentheses in the tuple location
                let loc = p.mk_loc(
                    lparen_pos.as_ref().unwrap(),
                    rparen_end_pos.as_ref().unwrap(),
                );
                Some(CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_tuple(args),
                    ptyp_loc: loc,
                    ptyp_attributes: vec![],
                })
            } else if args.len() == 1 {
                Some(args.into_iter().next().unwrap())
            } else {
                None
            };

            // Parse intersection constraints: #tag & typ1 & typ2
            // OCaml calls parse_polymorphic_variant_type_args after &, which expects parens
            // and handles single types specially (no paren location in the type)
            let mut constraints: Vec<CoreType> = first_type.into_iter().collect();
            while p.token == Token::Ampersand {
                p.next();
                // OCaml expects parentheses after & and uses parse_polymorphic_variant_type_args
                if p.token == Token::Lparen {
                    constraints.push(parse_polymorphic_variant_type_args(p));
                } else {
                    constraints.push(parse_typ_expr_inner(p, false));
                }
            }

            fields.push(RowField::Rtag(tag, attrs, is_constant, constraints));
        } else if matches!(p.token, Token::Lident(_) | Token::Uident(_)) {
            // Inherited row type: Foo.t or just t
            // OCaml: doc comment attributes go on the type's ptyp_attributes
            let mut typ = parse_type_constr(p);
            if !attrs.is_empty() {
                // Prepend attrs to the type's existing attributes
                let mut new_attrs = attrs;
                new_attrs.extend(typ.ptyp_attributes);
                typ.ptyp_attributes = new_attrs;
            }
            fields.push(RowField::Rinherit(typ));
        } else {
            break;
        }

        // Handle separator: `|` between fields
        // Note: Doc comments or attributes may appear before the next `|`, so we also
        // continue if we see those (the bar will be consumed after parsing them)
        if !p.optional(&Token::Bar) && !matches!(p.token, Token::At | Token::DocComment { .. }) {
            break;
        }
    }

    fields
}

// ============================================================================
// Package Type
// ============================================================================

/// Parse a package type without leading paren: S [with type constraints].
/// Used for first-class module constraints: module(Expr: S)
pub fn parse_package_type(p: &mut Parser<'_>) -> CoreType {
    let start_pos = p.start_pos.clone();
    let lid = parse_module_long_ident(p);

    // Parse optional constraints ("with" is not a keyword)
    let constraints = if matches!(&p.token, Token::Lident(s) if s == "with") {
        p.next();
        parse_package_constraints(p)
    } else {
        vec![]
    };

    let loc = p.mk_loc_to_prev_end(&start_pos);

    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_package((lid, constraints)),
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    }
}

/// Parse a package type: module(S).
fn parse_package_type_with_parens(
    p: &mut Parser<'_>,
    start_pos: crate::location::Position,
) -> CoreType {
    p.expect(Token::Lparen);

    let lid = parse_module_long_ident(p);

    // Parse optional constraints ("with" is not a keyword)
    let constraints = if matches!(&p.token, Token::Lident(s) if s == "with") {
        p.next();
        parse_package_constraints(p)
    } else {
        vec![]
    };

    p.expect(Token::Rparen);
    let loc = p.mk_loc_to_prev_end(&start_pos);

    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_package((lid, constraints)),
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    }
}

/// Parse a module long identifier.
fn parse_module_long_ident(p: &mut Parser<'_>) -> super::ast::Loc<crate::parse_arena::LidentIdx> {
    let start_pos = p.start_pos.clone();
    let mut path_parts = vec![];

    while let Token::Uident(name) = &p.token {
        path_parts.push(name.clone());
        p.next();
        if p.token == Token::Dot {
            p.next();
        } else {
            break;
        }
    }

    if path_parts.is_empty() {
        // Allow lowercase module identifiers in some contexts (e.g. module type names).
        if let Token::Lident(name) = &p.token {
            path_parts.push(name.clone());
            p.next();
        } else {
            p.err(DiagnosticCategory::Message(
                "Expected module identifier".to_string(),
            ));
            path_parts.push("Error".to_string());
        }
    }

    let lid = super::core::build_longident(p.arena_mut(), &path_parts);
    let lid_idx = p.push_longident(lid);
    let loc = p.mk_loc_to_prev_end(&start_pos);
    with_loc(lid_idx, loc)
}

/// Parse a type long identifier (module path ending in a type name).
fn parse_type_long_ident(p: &mut Parser<'_>) -> super::ast::Loc<crate::parse_arena::LidentIdx> {
    let start_pos = p.start_pos.clone();
    let mut path_parts = vec![];

    loop {
        match &p.token {
            Token::Uident(name) => {
                path_parts.push(name.clone());
                p.next();
                if p.token == Token::Dot {
                    p.next();
                } else {
                    break;
                }
            }
            Token::Lident(name) => {
                path_parts.push(name.clone());
                p.next();
                break;
            }
            _ => break,
        }
    }

    if path_parts.is_empty() {
        p.err(DiagnosticCategory::Message(
            "Expected type identifier".to_string(),
        ));
        path_parts.push("error".to_string());
    }

    let lid = super::core::build_longident(p.arena_mut(), &path_parts);
    let lid_idx = p.push_longident(lid);
    let loc = p.mk_loc_to_prev_end(&start_pos);
    with_loc(lid_idx, loc)
}

/// Parse package constraints.
fn parse_package_constraints(p: &mut Parser<'_>) -> Vec<(super::ast::Loc<crate::parse_arena::LidentIdx>, CoreType)> {
    let mut constraints = vec![];

    while p.token == Token::Typ || p.token == Token::And {
        if p.token == Token::And {
            p.next();
        }
        p.expect(Token::Typ);

        let lid = parse_type_long_ident(p);
        p.expect(Token::Equal);
        let typ = parse_typ_expr_inner(p, false);

        constraints.push((lid, typ));
    }

    constraints
}

// ============================================================================
// Extension
// ============================================================================

/// Parse an extension.
fn parse_extension(p: &mut Parser<'_>) -> Extension {
    // OCaml includes the % in the extension name location
    let id_start = p.start_pos.clone();
    p.expect(Token::Percent);
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

    let id = if parts.is_empty() {
        p.err(DiagnosticCategory::Message(
            "Expected extension identifier".to_string(),
        ));
        mknoloc("error".to_string())
    } else {
        let id = parts.join(".");
        with_loc(id, p.mk_loc_to_prev_end(&id_start))
    };

    // Parse optional payload: %ext(payload)
    let payload = if p.token == Token::Lparen {
        parse_payload(p)
    } else {
        Payload::PStr(vec![])
    };

    (id, payload)
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

    /// Parse source code into a type expression with a timeout.
    /// Panics if parsing takes longer than PARSE_TIMEOUT.
    fn parse_type_with_timeout(source: &str) -> CoreType {
        let source_owned = source.to_string();
        let source_for_error = source_owned.clone();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut parser = Parser::new("test.res", &source_owned);
            let typ = parse_typ_expr(&mut parser);
            let _ = tx.send(typ);
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
    fn test_parse_type_var() {
        let typ = parse_type_with_timeout("'a");
        assert!(matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_var(_)));
    }

    #[test]
    fn test_parse_type_any() {
        let typ = parse_type_with_timeout("_");
        assert!(matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_any));
    }

    #[test]
    fn test_parse_type_constr_simple() {
        let typ = parse_type_with_timeout("int");
        assert!(matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_constr(..)));
    }

    #[test]
    fn test_parse_type_constr_with_args() {
        let typ = parse_type_with_timeout("array<int>");
        assert!(matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_constr(..)));
    }

    #[test]
    fn test_parse_type_tuple() {
        let typ = parse_type_with_timeout("(int, string)");
        assert!(matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_tuple(_)));
    }

    #[test]
    fn test_parse_type_arrow() {
        let typ = parse_type_with_timeout("int => string");
        assert!(matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. }));
    }

    #[test]
    fn test_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<CoreType>();
    }
}
