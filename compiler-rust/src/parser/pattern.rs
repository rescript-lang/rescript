//! Pattern parsing for ReScript.
//!
//! This module contains the pattern parsing logic, converting tokens
//! into pattern AST nodes.

use super::ast::*;
use super::core::{ast_helper, mknoloc, recover, with_loc};
use super::diagnostics::DiagnosticCategory;
use super::longident::Longident;
use super::state::Parser;
use super::token::Token;
use crate::location::Located;

// ============================================================================
// Attribute Parsing for Patterns
// ============================================================================

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
    p.next();

    let attr_id = parse_attribute_id(p);

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
fn parse_attribute_id(p: &mut Parser<'_>) -> Located<String> {
    let start_pos = p.start_pos.clone();
    let mut parts = vec![];

    loop {
        let name = match &p.token {
            Token::Lident(name) | Token::Uident(name) => {
                let name = name.clone();
                p.next();
                name
            }
            _ if p.token.is_keyword() => {
                let name = p.token.to_string();
                p.next();
                name
            }
            _ => break,
        };
        parts.push(name);
        if p.token == Token::Dot {
            p.next();
        } else {
            break;
        }
    }

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
    with_loc(parts.join("."), loc)
}

/// Parse an attribute payload.
fn parse_payload(p: &mut Parser<'_>) -> Payload {
    // For patterns, we mainly need to handle string payloads like @attr("value")
    // and simple structure payloads
    let mut items = vec![];

    while p.token != Token::Rparen && p.token != Token::Eof {
        // Try to parse an expression for the payload
        let expr = super::expr::parse_expr(p);
        items.push(StructureItem {
            pstr_desc: StructureItemDesc::Pstr_eval(expr, vec![]),
            pstr_loc: crate::location::Location::none(),
        });

        if !p.optional(&Token::Comma) && !p.optional(&Token::Semicolon) {
            break;
        }
    }

    Payload::PStr(items)
}

/// Parse a type longident (for #...type pattern).
/// Type paths can be like: typevar, Module.typevar, etc.
fn parse_type_longident(p: &mut Parser<'_>) -> Longident {
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
        Longident::Lident("_".to_string())
    } else {
        super::core::build_longident(&parts)
    }
}

// ============================================================================
// Main Pattern Parsing
// ============================================================================

/// Parse a single pattern (atomic + interval), used by or-pattern parsing.
/// This parses one "branch" of an or-pattern without consuming further `|`.
fn parse_single_pattern(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();

    // Parse leading attributes
    let attrs = parse_attributes(p);

    let mut pat = parse_atomic_pattern(p);

    // Handle interval patterns: 1 .. 2, 'a' .. 'z', -1 .. -1., etc.
    if p.token == Token::DotDot {
        if let PatternDesc::Ppat_constant(from) = &pat.ppat_desc {
            match from {
                Constant::Integer(..) | Constant::Char(..) | Constant::Float(..) => {
                    let from = from.clone();
                    p.next();
                    let to = match &p.token {
                        Token::Int { .. } | Token::Codepoint { .. } | Token::Float { .. } => {
                            super::expr::parse_constant(p)
                        }
                        Token::Minus => {
                            // Handle negative numbers: -1, -1.0
                            p.next();
                            match &p.token {
                                Token::Int { i, suffix } => {
                                    let value = format!("-{}", i);
                                    let suffix = suffix.clone();
                                    p.next();
                                    Constant::Integer(value, suffix)
                                }
                                Token::Float { f, suffix } => {
                                    let value = format!("-{}", f);
                                    let suffix = suffix.clone();
                                    p.next();
                                    Constant::Float(value, suffix)
                                }
                                _ => {
                                    p.err(DiagnosticCategory::Message(
                                        "Expected number after '-' in interval pattern".to_string(),
                                    ));
                                    Constant::Integer("0".to_string(), None)
                                }
                            }
                        }
                        _ => {
                            p.err(DiagnosticCategory::Message(
                                "Expected constant after '..' in pattern".to_string(),
                            ));
                            Constant::Integer("0".to_string(), None)
                        }
                    };
                    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                    pat = Pattern {
                        ppat_desc: PatternDesc::Ppat_interval(from, to),
                        ppat_loc: loc,
                        ppat_attributes: vec![],
                    };
                }
                _ => {}
            }
        }
    }

    // Apply leading attributes to the pattern
    if !attrs.is_empty() {
        let loc = p.mk_loc(&start_pos, &pat.ppat_loc.loc_end);
        pat = Pattern {
            ppat_desc: pat.ppat_desc,
            ppat_loc: loc,
            ppat_attributes: [attrs, pat.ppat_attributes].concat(),
        };
    }

    pat
}

/// Parse a single pattern with optional alias (atomic + interval + as name).
/// Used by or-pattern parsing so each branch can have its own alias.
fn parse_single_pattern_with_alias(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();
    let mut pat = parse_single_pattern(p);

    // Handle pattern aliases (pat as name) for this single branch
    while p.token == Token::As {
        p.next();
        let name = match &p.token {
            Token::Lident(name) => {
                let name = name.clone();
                let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                p.next();
                with_loc(name, loc)
            }
            _ => {
                p.err(DiagnosticCategory::Message(
                    "Expected identifier after 'as'".to_string(),
                ));
                with_loc("_".to_string(), p.mk_loc(&p.start_pos, &p.end_pos))
            }
        };
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
        pat = Pattern {
            ppat_desc: PatternDesc::Ppat_alias(Box::new(pat), name),
            ppat_loc: loc,
            ppat_attributes: vec![],
        };
    }

    pat
}

/// Parse a pattern.
pub fn parse_pattern(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();

    // Parse first pattern with its alias (if any)
    let mut pat = parse_single_pattern_with_alias(p);

    // Handle or-patterns - each branch can have its own alias
    if p.token == Token::Bar {
        pat = parse_or_pattern(p, pat, start_pos.clone());
    }

    // Note: Type constraints (pat : type) are NOT handled here.
    // Callers that need constraints should use parse_constrained_pattern.
    // This matches OCaml's design where parse_pattern doesn't consume `:`.

    pat
}

/// Parse an or-pattern (pat1 | pat2).
fn parse_or_pattern(
    p: &mut Parser<'_>,
    first: Pattern,
    _start_pos: crate::location::Position,
) -> Pattern {
    let mut patterns = vec![first];

    while p.token == Token::Bar {
        p.next();
        // Use parse_single_pattern_with_alias so each branch can have its own alias
        // e.g., `_ as _y | _ as _x` parses as `(_ as _y) | (_ as _x)`
        patterns.push(parse_single_pattern_with_alias(p));
    }

    // Build left-associative or-pattern chain: A | B | C => ((A | B) | C)
    // This matches OCaml's behavior
    patterns
        .into_iter()
        .reduce(|acc, pat| {
            let loc = p.mk_loc(&acc.ppat_loc.loc_start, &pat.ppat_loc.loc_end);
            Pattern {
                ppat_desc: PatternDesc::Ppat_or(Box::new(acc), Box::new(pat)),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        })
        .unwrap_or_else(recover::default_pattern)
}

/// Parse a constrained pattern (pattern : type).
pub fn parse_constrained_pattern(p: &mut Parser<'_>) -> Pattern {
    let pat = parse_pattern(p);

    if p.token == Token::Colon {
        p.next();
        // Use no-arrow parsing so the `=>` token isn't consumed as part of a type
        // when we're in contexts like switch cases: `| pat: t => expr`.
        let typ = super::typ::parse_typ_expr_no_arrow(p);
        let loc = p.mk_loc(&pat.ppat_loc.loc_start, &typ.ptyp_loc.loc_end);
        Pattern {
            ppat_desc: PatternDesc::Ppat_constraint(Box::new(pat), typ),
            ppat_loc: loc,
            ppat_attributes: vec![],
        }
    } else {
        pat
    }
}

/// Create a unit construct pattern `()`.
pub fn make_unit_construct_pattern(loc: crate::location::Location) -> Pattern {
    ast_helper::make_construct_pat(Longident::Lident("()".to_string()), None, loc.clone(), loc)
}

/// Parse an optional alias `as name` after a pattern.
///
/// This is used in for loops where we need to parse aliases separately
/// from the main pattern parsing.
pub fn parse_alias_pattern(
    p: &mut Parser<'_>,
    mut pat: Pattern,
    _attrs: Vec<Attribute>,
) -> Pattern {
    let start_pos = pat.ppat_loc.loc_start.clone();
    while p.token == Token::As {
        p.next();
        let name = match &p.token {
            Token::Lident(name) => {
                let name = name.clone();
                let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                p.next();
                with_loc(name, loc)
            }
            _ => {
                p.err(DiagnosticCategory::Message(
                    "Expected identifier after 'as'".to_string(),
                ));
                with_loc("_".to_string(), p.mk_loc(&p.start_pos, &p.end_pos))
            }
        };
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
        pat = Pattern {
            ppat_desc: PatternDesc::Ppat_alias(Box::new(pat), name),
            ppat_loc: loc,
            ppat_attributes: vec![],
        };
    }
    pat
}

/// Parse a tuple pattern from a starting position and first pattern.
///
/// The caller must have already consumed the first comma between the first
/// pattern and the second. This function continues parsing subsequent patterns.
///
/// Example: for `(a, b, c)`, after parsing `a` and consuming the comma,
/// this function parses `b, c)`.
pub fn parse_tuple_pattern(
    p: &mut Parser<'_>,
    start_pos: crate::location::Position,
    first: Pattern,
    _attrs: Vec<Attribute>,
) -> Pattern {
    let mut patterns = vec![first];

    // Parse remaining patterns (first comma already consumed by caller)
    loop {
        if p.token == Token::Rparen {
            break;
        }
        patterns.push(parse_constrained_pattern(p));
        if p.token == Token::Comma {
            p.next();
        } else {
            break;
        }
    }

    p.expect(Token::Rparen);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Pattern {
        ppat_desc: PatternDesc::Ppat_tuple(patterns),
        ppat_loc: loc,
        ppat_attributes: vec![],
    }
}

// ============================================================================
// Atomic Pattern Parsing
// ============================================================================

/// Parse an atomic pattern.
fn parse_atomic_pattern(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();

    match &p.token {
        Token::Underscore => {
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            Pattern {
                ppat_desc: PatternDesc::Ppat_any,
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        }
        Token::Lident(name) => {
            let name = name.clone();
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

            // Check for alias (as)
            if p.token == Token::As {
                p.next();
                // Capture alias name position BEFORE parsing it
                let alias_start = p.start_pos.clone();
                let alias = parse_lident(p);
                // OCaml: alias name location is just the name, not the whole pattern
                let alias_name_loc = p.mk_loc(&alias_start, &p.prev_end_pos);
                let overall_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                let inner = ast_helper::make_var_pat(name, loc);
                Pattern {
                    ppat_desc: PatternDesc::Ppat_alias(
                        Box::new(inner),
                        with_loc(alias, alias_name_loc),
                    ),
                    ppat_loc: overall_loc,
                    ppat_attributes: vec![],
                }
            } else {
                ast_helper::make_var_pat(name, loc)
            }
        }
        Token::Uident(_) => parse_constructor_pattern(p),
        Token::Int { .. }
        | Token::String { .. }
        | Token::Float { .. }
        | Token::Codepoint { .. } => {
            let c = super::expr::parse_constant(p);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            Pattern {
                ppat_desc: PatternDesc::Ppat_constant(c),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        }
        Token::True | Token::False => {
            let is_true = p.token == Token::True;
            p.next();
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            let lid = Longident::Lident(if is_true { "true" } else { "false" }.to_string());
            ast_helper::make_construct_pat(lid, None, loc.clone(), loc)
        }
        Token::Lparen => {
            p.next();
            if p.token == Token::Rparen {
                // Unit pattern: ()
                p.next();
                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                ast_helper::make_construct_pat(Longident::Lident("()".to_string()), None, loc.clone(), loc)
            } else {
                // Parenthesized pattern or tuple
                let mut pat = parse_constrained_pattern(p);
                if p.token == Token::Comma {
                    // Tuple pattern - consume first comma, then parse rest
                    p.next();
                    parse_tuple_pattern(p, start_pos, pat, vec![])
                } else {
                    p.expect(Token::Rparen);
                    // Extend the pattern's location to include the parentheses
                    pat.ppat_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                    pat
                }
            }
        }
        Token::Lbracket => parse_array_pattern(p),
        Token::Lbrace => parse_record_pattern(p),
        Token::List => {
            p.next();
            parse_list_pattern(p, start_pos)
        }
        Token::Dict => {
            p.next();
            parse_dict_pattern(p, start_pos)
        }
        Token::Exception => {
            p.next();
            let pat = parse_atomic_pattern(p);
            let loc = p.mk_loc(&start_pos, &pat.ppat_loc.loc_end);
            Pattern {
                ppat_desc: PatternDesc::Ppat_exception(Box::new(pat)),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        }
        Token::Module => {
            // Unpack pattern: module(M) or module(M: S)
            p.next();
            p.expect(Token::Lparen);

            let name_loc = match &p.token {
                Token::Uident(name) | Token::Lident(name) => {
                    let name = name.clone();
                    let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    with_loc(name, loc)
                }
                _ => {
                    p.err(DiagnosticCategory::Message(
                        "Expected module name".to_string(),
                    ));
                    with_loc("Error".to_string(), p.mk_loc(&p.start_pos, &p.end_pos))
                }
            };

            let pkg_type = if p.token == Token::Colon {
                // OCaml: core_type location starts from after the colon
                let colon_pos = p.start_pos.clone();
                p.next();
                let mut typ = super::typ::parse_package_type(p);
                typ.ptyp_loc.loc_start = colon_pos;
                Some(typ)
            } else {
                None
            };

            p.expect(Token::Rparen);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

            // OCaml: Ppat_unpack location is just the name, not the whole module(...)
            let unpack = Pattern {
                ppat_desc: PatternDesc::Ppat_unpack(name_loc.clone()),
                ppat_loc: name_loc.loc,
                ppat_attributes: vec![],
            };

            if let Some(typ) = pkg_type {
                Pattern {
                    ppat_desc: PatternDesc::Ppat_constraint(Box::new(unpack), typ),
                    ppat_loc: loc,
                    ppat_attributes: vec![],
                }
            } else {
                unpack
            }
        }
        Token::Hash => parse_poly_variant_pattern(p),
        Token::Percent => parse_extension_pattern(p, start_pos),
        Token::Backtick | Token::TemplateTail { .. } | Token::TemplatePart { .. } => {
            parse_template_literal_pattern(p, start_pos)
        }
        Token::Colon => {
            // List cons constructor: :: (printed by the current list pattern printer)
            p.next();
            if p.token != Token::Colon {
                p.err(DiagnosticCategory::Message(
                    "Unexpected token in pattern: Colon".to_string(),
                ));
                recover::default_pattern()
            } else {
                p.next();

                let lid_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                let lid = mknoloc(Longident::Lident("::".to_string()));

                let arg = if p.token == Token::Lparen {
                    // Capture position of ( for tuple location (OCaml includes parens)
                    let lparen_pos = p.start_pos.clone();
                    p.next();
                    if p.token == Token::Rparen {
                        p.next();
                        let unit_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                        Some(ast_helper::make_construct_pat(
                            Longident::Lident("()".to_string()),
                            None,
                            unit_loc.clone(),
                            unit_loc,
                        ))
                    } else {
                        let first = parse_pattern(p);
                        if p.token == Token::Comma {
                            // OCaml includes the ( in the tuple location
                            let tuple_start = lparen_pos.clone();
                            let mut patterns = vec![first];
                            while p.token == Token::Comma {
                                p.next();
                                if p.token == Token::Rparen {
                                    break; // Trailing comma
                                }
                                patterns.push(parse_pattern(p));
                            }
                            p.expect(Token::Rparen);

                            // If we only have one pattern (trailing comma case), treat as single argument
                            if patterns.len() == 1 {
                                let single = patterns.into_iter().next().unwrap();
                                if matches!(single.ppat_desc, PatternDesc::Ppat_tuple(_)) {
                                    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                                    Some(Pattern {
                                        ppat_desc: PatternDesc::Ppat_tuple(vec![single]),
                                        ppat_loc: loc,
                                        ppat_attributes: vec![],
                                    })
                                } else {
                                    Some(single)
                                }
                            } else {
                                let tuple_loc = p.mk_loc(&tuple_start, &p.prev_end_pos);
                                Some(Pattern {
                                    ppat_desc: PatternDesc::Ppat_tuple(patterns),
                                    ppat_loc: tuple_loc,
                                    ppat_attributes: vec![],
                                })
                            }
                        } else {
                            p.expect(Token::Rparen);
                            // If the single argument is itself a tuple, wrap it in another
                            // single-element tuple. This distinguishes C((a,b)) from C(a,b).
                            // C((a,b)) = single tuple argument = Ppat_tuple([Ppat_tuple([a,b])])
                            // C(a,b) = multiple arguments = Ppat_tuple([a, b])
                            if matches!(first.ppat_desc, PatternDesc::Ppat_tuple(_)) {
                                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                                Some(Pattern {
                                    ppat_desc: PatternDesc::Ppat_tuple(vec![first]),
                                    ppat_loc: loc,
                                    ppat_attributes: vec![],
                                })
                            } else {
                                Some(first)
                            }
                        }
                    }
                } else {
                    None
                };

                // OCaml uses just the constructor name location, not including the argument
                Pattern {
                    ppat_desc: PatternDesc::Ppat_construct(lid, arg.map(Box::new)),
                    ppat_loc: lid_loc,
                    ppat_attributes: vec![],
                }
            }
        }
        Token::Minus | Token::Plus => {
            // Negative or positive number pattern: -1 or +1
            let is_negative = p.token == Token::Minus;
            p.next();
            match &p.token {
                Token::Int { i, suffix } => {
                    let value = if is_negative {
                        format!("-{}", i)
                    } else {
                        i.clone()
                    };
                    let suffix = suffix.clone();
                    p.next();
                    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                    Pattern {
                        ppat_desc: PatternDesc::Ppat_constant(Constant::Integer(value, suffix)),
                        ppat_loc: loc,
                        ppat_attributes: vec![],
                    }
                }
                Token::Float { f, suffix } => {
                    let value = if is_negative {
                        format!("-{}", f)
                    } else {
                        f.clone()
                    };
                    let suffix = suffix.clone();
                    p.next();
                    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                    Pattern {
                        ppat_desc: PatternDesc::Ppat_constant(Constant::Float(value, suffix)),
                        ppat_loc: loc,
                        ppat_attributes: vec![],
                    }
                }
                _ => {
                    p.err(DiagnosticCategory::Message(
                        "Expected number after sign in pattern".to_string(),
                    ));
                    recover::default_pattern()
                }
            }
        }
        _ => {
            p.err(DiagnosticCategory::Message(format!(
                "Unexpected token in pattern: {:?}",
                p.token
            )));
            // Advance to prevent infinite loops
            p.next();
            recover::default_pattern()
        }
    }
}

/// Parse a lowercase identifier.
fn parse_lident(p: &mut Parser<'_>) -> String {
    match &p.token {
        Token::Lident(name) => {
            let name = name.clone();
            p.next();
            name
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected lowercase identifier".to_string(),
            ));
            "_".to_string()
        }
    }
}

// ============================================================================
// Constructor Pattern
// ============================================================================

/// Parse a constructor pattern (Foo or Foo(pat)).
fn parse_constructor_pattern(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();

    // Parse the constructor path
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

    // Handle lowercase at the end
    if let Token::Lident(name) = &p.token {
        path_parts.push(name.clone());
        p.next();
    }

    let lid = super::core::build_longident(&path_parts);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Check for constructor argument
    let arg = if p.token == Token::Lparen {
        // Capture position of ( for tuple location (OCaml includes parens)
        let lparen_pos = p.start_pos.clone();
        p.next();
        if p.token == Token::Rparen {
            p.next();
            // Empty parentheses - unit pattern
            // OCaml: unit pattern location starts at ( not at constructor name
            let unit_loc = p.mk_loc(&lparen_pos, &p.prev_end_pos);
            Some(ast_helper::make_construct_pat(
                Longident::Lident("()".to_string()),
                None,
                unit_loc.clone(),
                unit_loc,
            ))
        } else {
            // Use parse_constrained_pattern to support (pat: type) inside constructors
            let first = parse_constrained_pattern(p);
            if p.token == Token::Comma {
                // Possible multiple arguments - parse remaining
                // OCaml includes the ( in the tuple location
                let tuple_start = lparen_pos.clone();
                let mut patterns = vec![first];
                while p.token == Token::Comma {
                    p.next();
                    if p.token == Token::Rparen {
                        break; // Trailing comma
                    }
                    patterns.push(parse_constrained_pattern(p));
                }
                p.expect(Token::Rparen);

                // If we only have one pattern (trailing comma case), treat as single argument
                if patterns.len() == 1 {
                    let single = patterns.into_iter().next().unwrap();
                    // If the single argument is itself a tuple, wrap it
                    if matches!(single.ppat_desc, PatternDesc::Ppat_tuple(_)) {
                        let wrap_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                        Some(Pattern {
                            ppat_desc: PatternDesc::Ppat_tuple(vec![single]),
                            ppat_loc: wrap_loc,
                            ppat_attributes: vec![],
                        })
                    } else {
                        Some(single)
                    }
                } else {
                    let tuple_loc = p.mk_loc(&tuple_start, &p.prev_end_pos);
                    Some(Pattern {
                        ppat_desc: PatternDesc::Ppat_tuple(patterns),
                        ppat_loc: tuple_loc,
                        ppat_attributes: vec![],
                    })
                }
            } else {
                p.expect(Token::Rparen);
                // If the single argument is itself a tuple, wrap it in another
                // single-element tuple. This distinguishes C((a,b)) from C(a,b).
                if matches!(first.ppat_desc, PatternDesc::Ppat_tuple(_)) {
                    let wrap_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                    Some(Pattern {
                        ppat_desc: PatternDesc::Ppat_tuple(vec![first]),
                        ppat_loc: wrap_loc,
                        ppat_attributes: vec![],
                    })
                } else {
                    Some(first)
                }
            }
        }
    } else {
        None
    };

    let full_loc = if arg.is_some() {
        p.mk_loc(&start_pos, &p.prev_end_pos)
    } else {
        loc.clone()
    };

    ast_helper::make_construct_pat(lid, arg, loc, full_loc)
}

// ============================================================================
// Collection Patterns
// ============================================================================

/// Parse an array pattern [p1, p2, ...].
fn parse_array_pattern(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lbracket);

    let mut patterns = vec![];
    while p.token != Token::Rbracket && p.token != Token::Eof {
        // Use parse_constrained_pattern to support [1 : int, 2 : int] syntax
        patterns.push(parse_constrained_pattern(p));
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbracket);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Pattern {
        ppat_desc: PatternDesc::Ppat_array(patterns),
        ppat_loc: loc,
        ppat_attributes: vec![],
    }
}

/// Parse a list pattern list{p1, p2, ...spread}.
fn parse_list_pattern(p: &mut Parser<'_>, start_pos: crate::location::Position) -> Pattern {
    // Token::List already includes the opening '{'

    let mut patterns = vec![];
    let mut spread = None;

    while p.token != Token::Rbrace && p.token != Token::Eof {
        if p.token == Token::DotDotDot {
            // OCaml includes ... in the spread pattern location
            let spread_start = p.start_pos.clone();
            p.next();
            let mut spread_pat = parse_constrained_pattern(p);
            spread_pat.ppat_loc.loc_start = spread_start;
            spread = Some(spread_pat);
            // Allow trailing comma after spread: list{a, ...rest,}
            p.optional(&Token::Comma);
            break;
        }
        // Use parse_constrained_pattern to support (pat: type) inside list patterns
        patterns.push(parse_constrained_pattern(p));
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    ast_helper::make_list_pattern(p, loc, patterns, spread)
}

/// Parse a record pattern {field: pat, ...}.
fn parse_record_pattern(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lbrace);

    let mut fields = vec![];
    let mut closed = ClosedFlag::Closed;

    while p.token != Token::Rbrace && p.token != Token::Eof {
        if p.token == Token::Underscore {
            p.next();
            closed = ClosedFlag::Open;
            // Allow trailing comma after underscore: {a, _,}
            p.optional(&Token::Comma);
            break;
        }

        let field_start = p.start_pos.clone();

        // Check for optional field punning: { ? name, ... }
        let opt_punning = p.token == Token::Question;
        if opt_punning {
            p.next();
        }

        // Support qualified record labels in patterns: {Module.label: pat}
        // (also needed for ambiguity disambiguation, mirroring OCaml syntax).
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

        let (lid, pat, opt) = if p.token == Token::Colon {
            // field: pattern or field: ? pattern
            p.next();
            // Check for optional marker after colon: name: ? pat
            let opt = p.token == Token::Question;
            if opt {
                p.next();
            }
            let pat = parse_pattern(p);
            let loc = p.mk_loc(&field_start, &p.prev_end_pos);
            (with_loc(lid_unloc, loc), pat, opt)
        } else if p.token == Token::Question {
            // field? (optional punning after field name - less common)
            p.next();
            let loc = p.mk_loc(&field_start, &p.prev_end_pos);
            let pat = ast_helper::make_var_pat(pun_name, loc.clone());
            (with_loc(lid_unloc, loc), pat, true)
        } else {
            // Punning: field is same as variable (use last identifier in the path)
            let loc = p.mk_loc(&field_start, &p.prev_end_pos);
            let pat = ast_helper::make_var_pat(pun_name, loc.clone());
            (with_loc(lid_unloc, loc), pat, opt_punning)
        };

        fields.push(PatternRecordField { lid, pat, opt });

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Pattern {
        ppat_desc: PatternDesc::Ppat_record(fields, closed),
        ppat_loc: loc,
        ppat_attributes: vec![],
    }
}

/// Parse a dict pattern: dict{"key": pat, ...}.
/// The `dict{` token is already consumed.
fn parse_dict_pattern(p: &mut Parser<'_>, start_pos: crate::location::Position) -> Pattern {
    let mut fields = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        match &p.token {
            Token::String(key) => {
                let key = key.clone();
                let key_start = p.start_pos.clone();
                p.next();

                p.expect(Token::Colon);

                // Optional value marker: "key": ?x
                let opt = p.optional(&Token::Question);

                let pat = parse_pattern(p);
                let loc = p.mk_loc(&key_start, &p.prev_end_pos);
                fields.push(PatternRecordField {
                    lid: with_loc(Longident::Lident(key), loc),
                    pat,
                    opt,
                });
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

    // Dict patterns are parsed directly as open record patterns (not wrapped in extension)
    Pattern {
        ppat_desc: PatternDesc::Ppat_record(fields, ClosedFlag::Open),
        ppat_loc: loc,
        ppat_attributes: vec![],
    }
}

/// Parse an extension pattern: %ext or %ext.with.dots(payload)
fn parse_extension_pattern(p: &mut Parser<'_>, start_pos: crate::location::Position) -> Pattern {
    // OCaml includes the % in the extension name location
    let id_start = start_pos.clone();
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
        with_loc(parts.join("."), p.mk_loc(&id_start, &p.prev_end_pos))
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

    Pattern {
        ppat_desc: PatternDesc::Ppat_extension((id, payload)),
        ppat_loc: loc,
        ppat_attributes: vec![],
    }
}

/// Parse a template literal pattern: `foo` (and consumes interpolations if present).
fn parse_template_literal_pattern(p: &mut Parser<'_>, start_pos: crate::location::Position) -> Pattern {
    let mut text = String::new();

    if p.token == Token::Backtick {
        p.next_template_literal_token();
    }

    loop {
        match &p.token {
            Token::TemplatePart { text: chunk, .. } => {
                text.push_str(chunk);
                p.next();
                // Consume interpolation expression.
                let _ = super::expr::parse_expr(p);
                p.next_template_literal_token();
            }
            Token::TemplateTail { text: chunk, .. } => {
                text.push_str(chunk);
                p.next();
                break;
            }
            Token::Backtick => {
                // Empty template string
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
                    "Unexpected token in template literal pattern: {:?}",
                    p.token
                )));
                p.next();
                break;
            }
        }
    }

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Pattern {
        ppat_desc: PatternDesc::Ppat_constant(Constant::String(text, Some("js".to_string()))),
        ppat_loc: loc,
        ppat_attributes: vec![],
    }
}

// ============================================================================
// Poly Variant Pattern
// ============================================================================

/// Parse a polymorphic variant pattern (#Tag or #Tag(pat) or #...type).
fn parse_poly_variant_pattern(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Hash);

    // Handle #...type - type constraint pattern
    if p.token == Token::DotDotDot {
        p.next();
        // Parse a type identifier - can be lident or Uident.Uident.lident
        let lid = parse_type_longident(p);
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
        return Pattern {
            ppat_desc: PatternDesc::Ppat_type(mknoloc(lid)),
            ppat_loc: loc,
            ppat_attributes: vec![],
        };
    }

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
    let arg = if p.token == Token::Lparen {
        let tuple_start = p.start_pos.clone();
        p.next();
        if p.token == Token::Rparen {
            // Empty: #tag()
            p.next();
            let unit_loc = p.mk_loc(&tuple_start, &p.prev_end_pos);
            Some(Box::new(ast_helper::make_construct_pat(
                Longident::Lident("()".to_string()),
                None,
                unit_loc.clone(),
                unit_loc,
            )))
        } else {
            let first = parse_constrained_pattern(p);
            if p.token == Token::Comma {
                // Multiple arguments: #tag(a, b) -> tuple pattern
                let mut patterns = vec![first];
                while p.token == Token::Comma {
                    p.next();
                    if p.token == Token::Rparen {
                        break; // Trailing comma
                    }
                    patterns.push(parse_constrained_pattern(p));
                }
                p.expect(Token::Rparen);
                let tuple_loc = p.mk_loc(&tuple_start, &p.prev_end_pos);
                Some(Box::new(Pattern {
                    ppat_desc: PatternDesc::Ppat_tuple(patterns),
                    ppat_loc: tuple_loc,
                    ppat_attributes: vec![],
                }))
            } else {
                p.expect(Token::Rparen);
                Some(Box::new(first))
            }
        }
    } else {
        None
    };

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    Pattern {
        ppat_desc: PatternDesc::Ppat_variant(tag, arg),
        ppat_loc: loc,
        ppat_attributes: vec![],
    }
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

    /// Parse source code into a pattern with a timeout.
    /// Panics if parsing takes longer than PARSE_TIMEOUT.
    fn parse_pattern_with_timeout(source: &str) -> Pattern {
        let source_owned = source.to_string();
        let source_for_error = source_owned.clone();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut parser = Parser::new("test.res", &source_owned);
            let pat = parse_pattern(&mut parser);
            let _ = tx.send(pat);
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
    fn test_parse_pattern_underscore() {
        let pat = parse_pattern_with_timeout("_");
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_any));
    }

    #[test]
    fn test_parse_pattern_var() {
        let pat = parse_pattern_with_timeout("foo");
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_var(_)));
    }

    #[test]
    fn test_parse_pattern_unit() {
        let pat = parse_pattern_with_timeout("()");
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_construct(..)));
    }

    #[test]
    fn test_parse_pattern_constant() {
        let pat = parse_pattern_with_timeout("42");
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_constant(_)));
    }

    #[test]
    fn test_parse_pattern_tuple() {
        let pat = parse_pattern_with_timeout("(a, b, c)");
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_tuple(_)));
    }

    #[test]
    fn test_parse_pattern_array() {
        let pat = parse_pattern_with_timeout("[a, b]");
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_array(_)));
    }

    #[test]
    fn test_parse_pattern_constructor() {
        let pat = parse_pattern_with_timeout("Some(x)");
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_construct(..)));
    }

    #[test]
    fn test_parse_pattern_record() {
        let pat = parse_pattern_with_timeout("{a, b: x}");
        assert!(matches!(pat.ppat_desc, PatternDesc::Ppat_record(..)));
    }

    #[test]
    fn test_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Pattern>();
    }
}
