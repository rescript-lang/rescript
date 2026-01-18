//! Pattern parsing for ReScript.
//!
//! This module contains the pattern parsing logic, converting tokens
//! into pattern AST nodes.

use super::ast::*;
use super::core::{ast_helper, mk_loc, mknoloc, recover, with_loc};
use super::diagnostics::DiagnosticCategory;
use super::longident::Longident;
use super::state::Parser;
use super::token::Token;

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

/// Parse a pattern.
pub fn parse_pattern(p: &mut Parser<'_>) -> Pattern {
    let start_pos = p.start_pos.clone();
    let mut pat = parse_atomic_pattern(p);

    // Handle or-patterns
    if p.token == Token::Bar {
        pat = parse_or_pattern(p, pat, start_pos.clone());
    }

    // Handle pattern aliases (pat as name)
    while p.token == Token::As {
        p.next();
        let name = match &p.token {
            Token::Lident(name) => {
                let name = name.clone();
                let loc = mk_loc(&p.start_pos, &p.end_pos);
                p.next();
                with_loc(name, loc)
            }
            _ => {
                p.err(DiagnosticCategory::Message(
                    "Expected identifier after 'as'".to_string(),
                ));
                with_loc("_".to_string(), mk_loc(&p.start_pos, &p.end_pos))
            }
        };
        let loc = mk_loc(&start_pos, &p.prev_end_pos);
        pat = Pattern {
            ppat_desc: PatternDesc::Ppat_alias(Box::new(pat), name),
            ppat_loc: loc,
            ppat_attributes: vec![],
        };
    }

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
        patterns.push(parse_atomic_pattern(p));
    }

    // Build right-associative or-pattern chain
    patterns
        .into_iter()
        .rev()
        .reduce(|acc, pat| {
            let loc = mk_loc(&pat.ppat_loc.loc_start, &acc.ppat_loc.loc_end);
            Pattern {
                ppat_desc: PatternDesc::Ppat_or(Box::new(pat), Box::new(acc)),
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
        let typ = super::typ::parse_typ_expr(p);
        let loc = mk_loc(&pat.ppat_loc.loc_start, &typ.ptyp_loc.loc_end);
        Pattern {
            ppat_desc: PatternDesc::Ppat_constraint(Box::new(pat), typ),
            ppat_loc: loc,
            ppat_attributes: vec![],
        }
    } else {
        pat
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
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            Pattern {
                ppat_desc: PatternDesc::Ppat_any,
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        }
        Token::Lident(name) => {
            let name = name.clone();
            p.next();
            let loc = mk_loc(&start_pos, &p.prev_end_pos);

            // Check for alias (as)
            if p.token == Token::As {
                p.next();
                let alias = parse_lident(p);
                let alias_loc = mk_loc(&start_pos, &p.prev_end_pos);
                let inner = ast_helper::make_var_pat(name, loc);
                Pattern {
                    ppat_desc: PatternDesc::Ppat_alias(
                        Box::new(inner),
                        with_loc(alias, alias_loc.clone()),
                    ),
                    ppat_loc: alias_loc,
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
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            Pattern {
                ppat_desc: PatternDesc::Ppat_constant(c),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        }
        Token::True | Token::False => {
            let is_true = p.token == Token::True;
            p.next();
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            let lid = Longident::Lident(if is_true { "true" } else { "false" }.to_string());
            ast_helper::make_construct_pat(lid, None, loc)
        }
        Token::Lparen => {
            p.next();
            if p.token == Token::Rparen {
                // Unit pattern: ()
                p.next();
                let loc = mk_loc(&start_pos, &p.prev_end_pos);
                ast_helper::make_construct_pat(Longident::Lident("()".to_string()), None, loc)
            } else {
                // Parenthesized pattern or tuple
                let pat = parse_constrained_pattern(p);
                if p.token == Token::Comma {
                    // Tuple pattern
                    parse_tuple_pattern(p, start_pos, pat)
                } else {
                    p.expect(Token::Rparen);
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
        Token::Exception => {
            p.next();
            let pat = parse_atomic_pattern(p);
            let loc = mk_loc(&start_pos, &pat.ppat_loc.loc_end);
            Pattern {
                ppat_desc: PatternDesc::Ppat_exception(Box::new(pat)),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        }
        Token::Hash => parse_poly_variant_pattern(p),
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
                    let loc = mk_loc(&start_pos, &p.prev_end_pos);
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
                    let loc = mk_loc(&start_pos, &p.prev_end_pos);
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
    let loc = mk_loc(&start_pos, &p.prev_end_pos);

    // Check for constructor argument
    let arg = if p.token == Token::Lparen {
        p.next();
        if p.token == Token::Rparen {
            p.next();
            // Empty parentheses - unit pattern
            let unit_loc = mk_loc(&start_pos, &p.prev_end_pos);
            Some(ast_helper::make_construct_pat(
                Longident::Lident("()".to_string()),
                None,
                unit_loc,
            ))
        } else {
            let first = parse_pattern(p);
            if p.token == Token::Comma {
                // Multiple arguments - create a tuple pattern
                let tuple_start = first.ppat_loc.loc_start.clone();
                let mut patterns = vec![first];
                while p.token == Token::Comma {
                    p.next();
                    patterns.push(parse_pattern(p));
                }
                p.expect(Token::Rparen);
                let tuple_loc = mk_loc(&tuple_start, &p.prev_end_pos);
                Some(Pattern {
                    ppat_desc: PatternDesc::Ppat_tuple(patterns),
                    ppat_loc: tuple_loc,
                    ppat_attributes: vec![],
                })
            } else {
                p.expect(Token::Rparen);
                Some(first)
            }
        }
    } else {
        None
    };

    let full_loc = if arg.is_some() {
        mk_loc(&start_pos, &p.prev_end_pos)
    } else {
        loc.clone()
    };

    ast_helper::make_construct_pat(lid, arg, full_loc)
}

// ============================================================================
// Tuple Pattern
// ============================================================================

/// Parse a tuple pattern.
fn parse_tuple_pattern(
    p: &mut Parser<'_>,
    start_pos: crate::location::Position,
    first: Pattern,
) -> Pattern {
    let mut patterns = vec![first];

    while p.token == Token::Comma {
        p.next();
        patterns.push(parse_constrained_pattern(p));
    }

    p.expect(Token::Rparen);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);

    Pattern {
        ppat_desc: PatternDesc::Ppat_tuple(patterns),
        ppat_loc: loc,
        ppat_attributes: vec![],
    }
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
        patterns.push(parse_pattern(p));
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbracket);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);

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
            p.next();
            spread = Some(parse_pattern(p));
            break;
        }
        patterns.push(parse_pattern(p));
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);

    ast_helper::make_list_pattern(loc, patterns, spread)
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
            break;
        }

        match &p.token {
            Token::Lident(name) => {
                let name = name.clone();
                let field_start = p.start_pos.clone();
                p.next();

                let (lid, pat, opt) = if p.token == Token::Colon {
                    // field: pattern
                    p.next();
                    let pat = parse_pattern(p);
                    let loc = mk_loc(&field_start, &p.prev_end_pos);
                    (with_loc(Longident::Lident(name), loc), pat, false)
                } else if p.token == Token::Question {
                    // field? (optional punning)
                    p.next();
                    let loc = mk_loc(&field_start, &p.prev_end_pos);
                    let pat = ast_helper::make_var_pat(name.clone(), loc.clone());
                    (with_loc(Longident::Lident(name), loc), pat, true)
                } else {
                    // Punning: field is same as variable
                    let loc = mk_loc(&field_start, &p.prev_end_pos);
                    let pat = ast_helper::make_var_pat(name.clone(), loc.clone());
                    (with_loc(Longident::Lident(name), loc), pat, false)
                };

                fields.push(PatternRecordField { lid, pat, opt });
            }
            _ => break,
        }

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rbrace);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);

    Pattern {
        ppat_desc: PatternDesc::Ppat_record(fields, closed),
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
        let loc = mk_loc(&start_pos, &p.prev_end_pos);
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
            let unit_loc = mk_loc(&tuple_start, &p.prev_end_pos);
            Some(Box::new(ast_helper::make_construct_pat(
                Longident::Lident("()".to_string()),
                None,
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
                let tuple_loc = mk_loc(&tuple_start, &p.prev_end_pos);
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

    let loc = mk_loc(&start_pos, &p.prev_end_pos);

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
