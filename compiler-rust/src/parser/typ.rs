//! Type expression parsing for ReScript.
//!
//! This module contains the type expression parsing logic, converting tokens
//! into type AST nodes.

use super::ast::*;
use super::core::{ast_helper, mk_loc, mknoloc, recover, with_loc};
use super::diagnostics::DiagnosticCategory;
use super::longident::Longident;
use super::state::Parser;
use super::token::Token;

// ============================================================================
// Main Type Parsing
// ============================================================================

/// Parse a type expression.
pub fn parse_typ_expr(p: &mut Parser<'_>) -> CoreType {
    let typ = parse_typ_expr_inner(p, true);

    // Handle type aliases (type as 'name)
    parse_type_alias(p, typ)
}

/// Parse a type expression without allowing arrow types at the top level.
/// Used for return type annotations like `(x): int => body`.
pub fn parse_typ_expr_no_arrow(p: &mut Parser<'_>) -> CoreType {
    let typ = parse_typ_expr_inner(p, false);
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
        let loc = mk_loc(&typ.ptyp_loc.loc_start, &p.prev_end_pos);
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
    let typ = parse_atomic_typ_expr(p);

    // Handle arrow types
    if es6_arrow && p.token == Token::EqualGreater {
        parse_arrow_type_rest(p, typ)
    } else {
        typ
    }
}

/// Parse the rest of an arrow type.
fn parse_arrow_type_rest(p: &mut Parser<'_>, param_type: CoreType) -> CoreType {
    p.expect(Token::EqualGreater);
    let return_type = parse_typ_expr(p);
    let loc = mk_loc(
        &param_type.ptyp_loc.loc_start,
        &return_type.ptyp_loc.loc_end,
    );

    let arg = TypeArg {
        attrs: vec![],
        lbl: ArgLabel::Nolabel,
        typ: param_type,
    };

    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_arrow {
            arg: Box::new(arg),
            ret: Box::new(return_type),
            arity: Arity::Unknown,
        },
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    }
}

// ============================================================================
// Atomic Type Parsing
// ============================================================================

/// Parse attributes that can appear before a type.
fn parse_type_attributes(p: &mut Parser<'_>) -> Attributes {
    let mut attrs = vec![];
    while p.token == Token::At {
        p.next();
        let start_pos = p.start_pos.clone();
        // Parse attribute id (possibly with path)
        let id = parse_attribute_id(p);
        // Parse optional payload
        let payload = if p.token == Token::Lparen && p.start_pos.cnum == p.prev_end_pos.cnum {
            parse_payload(p)
        } else {
            Payload::PStr(vec![])
        };
        let loc = mk_loc(&start_pos, &p.prev_end_pos);
        attrs.push((with_loc(id, loc), payload));
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

/// Parse an attribute payload.
fn parse_payload(p: &mut Parser<'_>) -> Payload {
    p.expect(Token::Lparen);
    // For now, just skip to matching )
    let mut depth = 1;
    while depth > 0 && p.token != Token::Eof {
        match p.token {
            Token::Lparen => depth += 1,
            Token::Rparen => depth -= 1,
            _ => {}
        }
        p.next();
    }
    Payload::PStr(vec![])
}

/// Parse an atomic type expression.
fn parse_atomic_typ_expr(p: &mut Parser<'_>) -> CoreType {
    let start_pos = p.start_pos.clone();

    // Parse attributes first
    let attrs = parse_type_attributes(p);

    let mut typ = match &p.token {
        Token::SingleQuote => {
            // Type variable: 'a
            p.next();
            let name = match &p.token {
                Token::Lident(name) | Token::Uident(name) => {
                    let name = name.clone();
                    p.next();
                    name
                }
                _ => {
                    p.err(DiagnosticCategory::Message(
                        super::core::error_messages::TYPE_VAR.to_string(),
                    ));
                    "a".to_string()
                }
            };
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            }
        }
        Token::Underscore => {
            // Any type: _
            p.next();
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_any,
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            }
        }
        Token::Lident(_) | Token::Uident(_) => parse_type_constr(p),
        Token::Lparen => {
            p.next();
            if p.token == Token::Rparen {
                // Unit type: ()
                p.next();
                let loc = mk_loc(&start_pos, &p.prev_end_pos);
                ast_helper::make_type_constr(Longident::Lident("unit".to_string()), vec![], loc)
            } else if p.token == Token::Dot || p.token == Token::Tilde {
                // Function type with labeled args
                parse_function_type(p, start_pos)
            } else {
                // Parenthesized type or tuple
                let typ = parse_typ_expr(p);
                if p.token == Token::Comma {
                    // Tuple type
                    parse_tuple_type(p, start_pos, typ)
                } else {
                    p.expect(Token::Rparen);
                    typ
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
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_extension(ext),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            }
        }
        _ => {
            p.err(DiagnosticCategory::Message(format!(
                "Unexpected token in type: {:?}",
                p.token
            )));
            recover::default_type()
        }
    };

    // Attach any leading attributes to the type
    if !attrs.is_empty() {
        typ.ptyp_attributes.extend(attrs);
    }

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

    let lid = super::core::build_longident(&path_parts);

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

    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    ast_helper::make_type_constr(lid, args, loc)
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
    // Skip uncurried marker
    p.optional(&Token::Dot);

    let mut params = vec![];

    while p.token != Token::Rparen && p.token != Token::Eof {
        let param_attrs = parse_attributes(p);

        let (label, typ) = if p.token == Token::Tilde {
            p.next();
            let name = parse_lident(p);
            p.expect(Token::Colon);
            let typ = parse_typ_expr_inner(p, false);
            (ArgLabel::Labelled(name), typ)
        } else {
            let typ = parse_typ_expr_inner(p, false);
            (ArgLabel::Nolabel, typ)
        };

        params.push(TypeArg {
            attrs: param_attrs,
            lbl: label,
            typ,
        });

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::Rparen);
    p.expect(Token::EqualGreater);
    let return_type = parse_typ_expr(p);

    // Build the arrow type from params
    let loc = mk_loc(&start_pos, &return_type.ptyp_loc.loc_end);

    params
        .into_iter()
        .rev()
        .fold(return_type, |acc, param| CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_arrow {
                arg: Box::new(param),
                ret: Box::new(acc),
                arity: Arity::Unknown,
            },
            ptyp_loc: loc.clone(),
            ptyp_attributes: vec![],
        })
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

/// Parse attributes.
fn parse_attributes(p: &mut Parser<'_>) -> Attributes {
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
    p.next();

    let start_pos = p.start_pos.clone();
    let attr_id_str = parse_attribute_id(p);
    let attr_id = with_loc(attr_id_str, mk_loc(&start_pos, &p.prev_end_pos));

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
    // For now, just parse a simple string payload
    match &p.token {
        Token::String(_s) => {
            p.next();
            Payload::PStr(vec![])
        }
        _ => {
            // Skip until closing paren
            let mut depth = 1;
            while depth > 0 && p.token != Token::Eof {
                match &p.token {
                    Token::Lparen => depth += 1,
                    Token::Rparen => depth -= 1,
                    _ => {}
                }
                if depth > 0 {
                    p.next();
                }
            }
            Payload::PStr(vec![])
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
        // Allow arrow types in tuple elements
        types.push(parse_typ_expr_inner(p, true));
    }

    p.expect(Token::Rparen);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);

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

    if is_object {
        // Object type
        let fields = parse_object_fields(p);
        p.expect(Token::Rbrace);
        let loc = mk_loc(&start_pos, &p.prev_end_pos);
        CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_object(fields, closed),
            ptyp_loc: loc,
            ptyp_attributes: vec![],
        }
    } else {
        // Record type - convert to object type with special marker
        // For now, just parse as object
        let fields = parse_object_fields(p);
        p.expect(Token::Rbrace);
        let loc = mk_loc(&start_pos, &p.prev_end_pos);
        CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_object(fields, closed),
            ptyp_loc: loc,
            ptyp_attributes: vec![],
        }
    }
}

/// Parse object fields.
fn parse_object_fields(p: &mut Parser<'_>) -> Vec<ObjectField> {
    let mut fields = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        // Handle spread: ...typ
        if p.token == Token::DotDotDot {
            p.next();
            let typ = parse_typ_expr_inner(p, false);
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
                // Check for optional field marker
                let _is_optional = p.optional(&Token::Question);
                p.expect(Token::Colon);
                let typ = parse_typ_expr_inner(p, false);
                let loc = mk_loc(&field_start, &p.prev_end_pos);
                fields.push(ObjectField::Otag(with_loc(name, loc), vec![], typ));
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

/// Parse a polymorphic variant type.
fn parse_poly_variant_type(p: &mut Parser<'_>) -> CoreType {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lbracket);

    // Check for closed/open markers
    let (closed, low_tags) = match &p.token {
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

    let fields = parse_row_fields(p);

    p.expect(Token::Rbracket);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);

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

    // Check for type arguments
    let args = if p.token == Token::Lparen {
        p.next();
        let mut args = vec![];
        while p.token != Token::Rparen && p.token != Token::Eof {
            args.push(parse_typ_expr_inner(p, false));
            if !p.optional(&Token::Comma) {
                break;
            }
        }
        p.expect(Token::Rparen);
        args
    } else {
        vec![]
    };

    let loc = mk_loc(&start_pos, &p.prev_end_pos);
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

/// Parse row fields.
fn parse_row_fields(p: &mut Parser<'_>) -> Vec<RowField> {
    let mut fields = vec![];

    while p.token != Token::Rbracket && p.token != Token::Eof {
        if p.token == Token::Bar {
            p.next();
        }

        // Parse attributes before the tag
        let attrs = parse_attributes(p);

        if p.token == Token::Hash {
            p.next();
            let tag = match &p.token {
                Token::Lident(name) | Token::Uident(name) => {
                    let name = name.clone();
                    let loc = mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    with_loc(name, loc)
                }
                Token::Int { i, .. } => {
                    let tag = i.clone();
                    let loc = mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    with_loc(tag, loc)
                }
                Token::String(s) => {
                    let tag = s.clone();
                    let loc = mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    with_loc(tag, loc)
                }
                _ => {
                    p.err(DiagnosticCategory::Message(
                        "Expected variant tag after #".to_string(),
                    ));
                    mknoloc("error".to_string())
                }
            };

            let args = if p.token == Token::Lparen {
                p.next();
                let mut args = vec![];
                while p.token != Token::Rparen && p.token != Token::Eof {
                    args.push(parse_typ_expr_inner(p, false));
                    if !p.optional(&Token::Ampersand) && !p.optional(&Token::Comma) {
                        break;
                    }
                }
                p.expect(Token::Rparen);
                args
            } else {
                vec![]
            };

            fields.push(RowField::Rtag(tag, attrs, args.is_empty(), args));
        } else {
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

    let loc = mk_loc(&start_pos, &p.prev_end_pos);

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
    let loc = mk_loc(&start_pos, &p.prev_end_pos);

    CoreType {
        ptyp_desc: CoreTypeDesc::Ptyp_package((lid, constraints)),
        ptyp_loc: loc,
        ptyp_attributes: vec![],
    }
}

/// Parse a module long identifier.
fn parse_module_long_ident(p: &mut Parser<'_>) -> super::ast::Loc<Longident> {
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
        p.err(DiagnosticCategory::Message(
            "Expected module identifier".to_string(),
        ));
        path_parts.push("Error".to_string());
    }

    let lid = super::core::build_longident(&path_parts);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    with_loc(lid, loc)
}

/// Parse package constraints.
fn parse_package_constraints(p: &mut Parser<'_>) -> Vec<(super::ast::Loc<Longident>, CoreType)> {
    let mut constraints = vec![];

    while p.token == Token::Typ || p.token == Token::And {
        if p.token == Token::And {
            p.next();
        }
        p.expect(Token::Typ);

        let lid = parse_module_long_ident(p);
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
    let _start_pos = p.start_pos.clone();
    p.expect(Token::Percent);

    let id = match &p.token {
        Token::Lident(name) | Token::Uident(name) => {
            let name = name.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(name, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected extension identifier".to_string(),
            ));
            mknoloc("error".to_string())
        }
    };

    (id, Payload::PStr(vec![]))
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
