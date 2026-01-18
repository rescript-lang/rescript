//! Module parsing for ReScript.
//!
//! This module contains the module parsing logic, converting tokens
//! into module AST nodes (structures, signatures, module expressions, etc.).

use crate::location::Position;

use super::ast::*;
use super::core::{is_es6_arrow_functor, mk_loc, mknoloc, recover, with_loc};
use super::diagnostics::DiagnosticCategory;
use super::expr;
use super::longident::Longident;
use super::pattern;
use super::state::Parser;
use super::token::Token;
use super::typ;

// ============================================================================
// Structure (Implementation) Parsing
// ============================================================================

/// Parse a structure (list of structure items).
pub fn parse_structure(p: &mut Parser<'_>) -> Structure {
    let mut items = vec![];

    while p.token != Token::Eof {
        // Skip semicolons
        while p.token == Token::Semicolon {
            p.next();
        }

        if p.token == Token::Eof {
            break;
        }

        if let Some(item) = parse_structure_item(p) {
            items.push(item);
        }
    }

    items
}

/// Parse a single structure item.
pub fn parse_structure_item(p: &mut Parser<'_>) -> Option<StructureItem> {
    let start_pos = p.start_pos.clone();

    // Parse attributes
    let attrs = parse_attributes(p);

    let desc = match &p.token {
        Token::Open => {
            p.next();
            // Check for open! override flag
            let override_flag = if p.token == Token::Bang {
                p.next();
                OverrideFlag::Override
            } else {
                OverrideFlag::Fresh
            };
            let lid = parse_module_long_ident(p);
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            Some(StructureItemDesc::Pstr_open(OpenDescription {
                popen_lid: lid,
                popen_override: override_flag,
                popen_loc: loc,
                popen_attributes: attrs.clone(),
            }))
        }
        Token::Let { unwrap } => {
            let _unwrap = *unwrap;
            p.next();
            let rec_flag = if p.token == Token::Rec {
                p.next();
                RecFlag::Recursive
            } else {
                RecFlag::Nonrecursive
            };
            let bindings = parse_let_bindings(p, rec_flag);
            Some(StructureItemDesc::Pstr_value(rec_flag, bindings))
        }
        Token::Typ => {
            p.next();
            let rec_flag = if p.token == Token::Rec {
                p.next();
                RecFlag::Recursive
            } else {
                RecFlag::Nonrecursive
            };
            let decls = parse_type_declarations(p);
            Some(StructureItemDesc::Pstr_type(rec_flag, decls))
        }
        Token::External => {
            p.next();
            let vd = parse_value_description(p);
            Some(StructureItemDesc::Pstr_primitive(vd))
        }
        Token::Exception => {
            p.next();
            let ext = parse_extension_constructor(p);
            Some(StructureItemDesc::Pstr_exception(ext))
        }
        Token::Module => {
            p.next();
            // Check for first-class module expression: module(...)
            if p.token == Token::Lparen {
                // Parse the first-class module, then continue with primary/binary parsing
                let pack_expr =
                    super::expr::parse_first_class_module_expr_from_paren(p, start_pos.clone());
                // Continue parsing the expression (handles ->, [], etc.)
                let expr = super::expr::parse_expr_with_operand(p, pack_expr);
                Some(StructureItemDesc::Pstr_eval(expr, attrs.clone()))
            } else {
                parse_module_definition(p)
            }
        }
        Token::Include => {
            p.next();
            let mod_expr = parse_module_expr(p);
            Some(StructureItemDesc::Pstr_include(IncludeDeclaration {
                pincl_mod: mod_expr,
                pincl_loc: mk_loc(&start_pos, &p.prev_end_pos),
                pincl_attributes: attrs.clone(),
            }))
        }
        Token::At | Token::AtAt => {
            // Floating attribute or extension
            if p.token == Token::AtAt {
                p.next();
                let attr = parse_attribute_body(p);
                Some(StructureItemDesc::Pstr_attribute(attr))
            } else {
                // @ might be a doc comment or similar
                p.next();
                let attr = parse_attribute_body(p);
                Some(StructureItemDesc::Pstr_attribute(attr))
            }
        }
        Token::Percent => {
            // Extension
            let ext = parse_extension(p);
            Some(StructureItemDesc::Pstr_extension(ext, attrs.clone()))
        }
        Token::PercentPercent => {
            // Structure-level extension (%%raw(...), etc.)
            p.next();
            // Parse extension name
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
            // Parse optional payload
            let payload = if p.token == Token::Lparen {
                p.next();
                let payload = parse_payload(p);
                p.expect(Token::Rparen);
                payload
            } else {
                Payload::PStr(vec![])
            };
            Some(StructureItemDesc::Pstr_extension(
                (id, payload),
                attrs.clone(),
            ))
        }
        _ => {
            // Could be an expression
            if is_expr_start(&p.token) {
                let expr = expr::parse_expr(p);
                Some(StructureItemDesc::Pstr_eval(expr, attrs.clone()))
            } else if p.token != Token::Eof {
                p.err(DiagnosticCategory::Message(format!(
                    "Unexpected token: {:?}",
                    p.token
                )));
                p.next();
                None
            } else {
                None
            }
        }
    };

    desc.map(|d| StructureItem {
        pstr_desc: d,
        pstr_loc: mk_loc(&start_pos, &p.prev_end_pos),
    })
}

// ============================================================================
// Signature Parsing
// ============================================================================

/// Parse a signature (list of signature items).
pub fn parse_signature(p: &mut Parser<'_>) -> Signature {
    let mut items = vec![];

    while p.token != Token::Eof && p.token != Token::Rbrace {
        // Skip semicolons
        while p.token == Token::Semicolon {
            p.next();
        }

        if p.token == Token::Eof || p.token == Token::Rbrace {
            break;
        }

        if let Some(item) = parse_signature_item(p) {
            items.push(item);
        }
    }

    items
}

/// Parse a single signature item.
pub fn parse_signature_item(p: &mut Parser<'_>) -> Option<SignatureItem> {
    let start_pos = p.start_pos.clone();

    // Parse attributes
    let attrs = parse_attributes(p);

    let desc = match &p.token {
        Token::Open => {
            p.next();
            let lid = parse_module_long_ident(p);
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            Some(SignatureItemDesc::Psig_open(OpenDescription {
                popen_lid: lid,
                popen_override: OverrideFlag::Fresh,
                popen_loc: loc,
                popen_attributes: attrs.clone(),
            }))
        }
        Token::Let { .. } => {
            p.next();
            let vd = parse_value_description(p);
            Some(SignatureItemDesc::Psig_value(vd))
        }
        Token::Typ => {
            p.next();
            let rec_flag = if p.token == Token::Rec {
                p.next();
                RecFlag::Recursive
            } else {
                RecFlag::Nonrecursive
            };
            let decls = parse_type_declarations(p);
            Some(SignatureItemDesc::Psig_type(rec_flag, decls))
        }
        Token::External => {
            p.next();
            let vd = parse_value_description(p);
            Some(SignatureItemDesc::Psig_value(vd))
        }
        Token::Exception => {
            p.next();
            let ext = parse_extension_constructor(p);
            Some(SignatureItemDesc::Psig_exception(ext))
        }
        Token::Module => {
            p.next();
            if p.token == Token::Typ {
                p.next();
                let mtd = parse_module_type_declaration(p);
                Some(SignatureItemDesc::Psig_modtype(mtd))
            } else {
                let md = parse_module_declaration(p);
                Some(SignatureItemDesc::Psig_module(md))
            }
        }
        Token::Include => {
            p.next();
            let mod_type = parse_module_type(p);
            Some(SignatureItemDesc::Psig_include(IncludeDescription {
                pincl_mod: mod_type,
                pincl_loc: mk_loc(&start_pos, &p.prev_end_pos),
                pincl_attributes: attrs.clone(),
            }))
        }
        Token::At | Token::AtAt => {
            if p.token == Token::AtAt {
                p.next();
                let attr = parse_attribute_body(p);
                Some(SignatureItemDesc::Psig_attribute(attr))
            } else {
                p.next();
                let attr = parse_attribute_body(p);
                Some(SignatureItemDesc::Psig_attribute(attr))
            }
        }
        Token::Percent => {
            let ext = parse_extension(p);
            Some(SignatureItemDesc::Psig_extension(ext, attrs.clone()))
        }
        _ => {
            if p.token != Token::Eof && p.token != Token::Rbrace {
                p.err(DiagnosticCategory::Message(format!(
                    "Unexpected token in signature: {:?}",
                    p.token
                )));
                p.next();
            }
            None
        }
    };

    desc.map(|d| SignatureItem {
        psig_desc: d,
        psig_loc: mk_loc(&start_pos, &p.prev_end_pos),
    })
}

// ============================================================================
// Module Expression Parsing
// ============================================================================

/// Parse a module expression.
pub fn parse_module_expr(p: &mut Parser<'_>) -> ModuleExpr {
    let start_pos = p.start_pos.clone();

    // Handle await keyword
    let has_await = if p.token == Token::Await {
        p.next();
        true
    } else {
        false
    };

    // Parse attributes (e.g., @functorAttr)
    let mut attrs = parse_attributes(p);

    // Add await attribute if present
    if has_await {
        attrs.insert(
            0,
            (
                mknoloc("res.await".to_string()),
                Payload::PStr(vec![]),
            ),
        );
    }

    // Check if this is a functor: (args) => body or (args) : type => body
    let expr = if is_es6_arrow_functor(p) {
        parse_functor_module_expr(p)
    } else {
        parse_primary_module_expr(p)
    };

    // Handle module application: F(X)
    let expr = parse_module_apply(p, expr);

    // Handle module constraint: M : S
    let expr = if p.token == Token::Colon {
        p.next();
        let mod_type = parse_module_type(p);
        let loc = mk_loc(&start_pos, &p.prev_end_pos);
        ModuleExpr {
            pmod_desc: ModuleExprDesc::Pmod_constraint(Box::new(expr), Box::new(mod_type)),
            pmod_loc: loc,
            pmod_attributes: vec![],
        }
    } else {
        expr
    };

    // Attach attributes to the module expression
    if attrs.is_empty() {
        expr
    } else {
        let mut combined_attrs = expr.pmod_attributes.clone();
        combined_attrs.extend(attrs);
        ModuleExpr {
            pmod_attributes: combined_attrs,
            ..expr
        }
    }
}

/// Parse a primary module expression.
fn parse_primary_module_expr(p: &mut Parser<'_>) -> ModuleExpr {
    let start_pos = p.start_pos.clone();

    match &p.token {
        Token::Uident(_) => {
            // Module path
            let lid = parse_module_long_ident(p);
            ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_ident(lid),
                pmod_loc: mk_loc(&start_pos, &p.prev_end_pos),
                pmod_attributes: vec![],
            }
        }
        Token::Lbrace => {
            // Module structure: { ... }
            p.next();
            let items = parse_structure_in_braces(p);
            p.expect(Token::Rbrace);
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_structure(items),
                pmod_loc: loc,
                pmod_attributes: vec![],
            }
        }
        Token::Lparen => {
            // Check if this is a functor: (args) => body or (args) : type => body
            if is_es6_arrow_functor(p) {
                parse_functor_module_expr(p)
            } else {
                // Parenthesized module expression
                p.next();
                let inner = parse_module_expr(p);
                p.expect(Token::Rparen);
                inner
            }
        }
        Token::Percent => {
            // Module extension
            let ext = parse_extension(p);
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_extension(ext),
                pmod_loc: loc,
                pmod_attributes: vec![],
            }
        }
        Token::At => {
            // Attributes on module expression - should be handled at parse_module_expr level
            // But we might get here for nested calls, so parse and attach
            let attrs = parse_attributes(p);
            let inner = parse_primary_module_expr(p);
            let mut combined_attrs = inner.pmod_attributes.clone();
            combined_attrs.extend(attrs);
            ModuleExpr {
                pmod_attributes: combined_attrs,
                ..inner
            }
        }
        _ => {
            // Try to parse as unpack: (val expr : pkg_type)
            if matches!(&p.token, Token::Lident(s) if s == "unpack") {
                p.next();
                p.expect(Token::Lparen);
                let expr = expr::parse_expr(p);
                p.expect(Token::Rparen);
                let loc = mk_loc(&start_pos, &p.prev_end_pos);
                ModuleExpr {
                    pmod_desc: ModuleExprDesc::Pmod_unpack(Box::new(expr)),
                    pmod_loc: loc,
                    pmod_attributes: vec![],
                }
            } else {
                p.err(DiagnosticCategory::Message(
                    "Expected module expression".to_string(),
                ));
                recover::default_module_expr()
            }
        }
    }
}

/// Parse module application.
fn parse_module_apply(p: &mut Parser<'_>, func: ModuleExpr) -> ModuleExpr {
    let mut result = func;

    // Module application only happens on the same line
    // A ( on a new line should not be treated as module application
    while p.token == Token::Lparen && p.prev_end_pos.line >= p.start_pos.line {
        let start = result.pmod_loc.loc_start.clone();
        p.next();

        let arg = if p.token == Token::Rparen {
            // Empty argument: F()
            p.next();
            let loc = mk_loc(&p.prev_end_pos, &p.prev_end_pos);
            ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_structure(vec![]),
                pmod_loc: loc,
                pmod_attributes: vec![],
            }
        } else {
            let arg = parse_module_expr(p);
            p.expect(Token::Rparen);
            arg
        };

        let loc = mk_loc(&start, &p.prev_end_pos);
        result = ModuleExpr {
            pmod_desc: ModuleExprDesc::Pmod_apply(Box::new(result), Box::new(arg)),
            pmod_loc: loc,
            pmod_attributes: vec![],
        };
    }

    result
}

// ============================================================================
// Functor Parsing
// ============================================================================

/// A functor argument parsed from source.
#[derive(Debug, Clone)]
struct FunctorArg {
    attrs: Attributes,
    name: Loc<String>,
    mod_type: Option<ModuleType>,
    start_pos: Position,
}

/// Parse a single functor argument.
/// functor-arg ::= Uident : modtype | _ : modtype | modtype (punning for _ : modtype)
fn parse_functor_arg(p: &mut Parser<'_>) -> Option<FunctorArg> {
    let start_pos = p.start_pos.clone();
    let attrs = parse_attributes(p);

    match &p.token {
        Token::Uident(ident) => {
            let ident = ident.clone();
            let ident_end_pos = p.end_pos.clone();
            p.next();

            match &p.token {
                Token::Colon => {
                    // Named parameter: Uident : modtype
                    p.next();
                    let mod_type = parse_module_type(p);
                    let loc = mk_loc(&start_pos, &ident_end_pos);
                    Some(FunctorArg {
                        attrs,
                        name: with_loc(ident, loc),
                        mod_type: Some(mod_type),
                        start_pos,
                    })
                }
                Token::Dot => {
                    // Module path as type: Foo.Bar (punning for _ : Foo.Bar)
                    // Continue parsing the module path
                    let mut lid = Longident::Lident(ident);
                    while p.token == Token::Dot {
                        p.next();
                        match &p.token {
                            Token::Uident(name) => {
                                lid = Longident::Ldot(Box::new(lid), name.clone());
                                p.next();
                            }
                            _ => break,
                        }
                    }
                    let loc = mk_loc(&start_pos, &p.prev_end_pos);
                    let mod_type = ModuleType {
                        pmty_desc: ModuleTypeDesc::Pmty_ident(with_loc(lid, loc.clone())),
                        pmty_loc: loc,
                        pmty_attributes: vec![],
                    };
                    Some(FunctorArg {
                        attrs,
                        name: mknoloc("_".to_string()),
                        mod_type: Some(mod_type),
                        start_pos,
                    })
                }
                _ => {
                    // Just Uident (punning for _ : Uident)
                    let loc = mk_loc(&start_pos, &ident_end_pos);
                    let mod_ident = with_loc(Longident::Lident(ident), loc.clone());
                    let mod_type = ModuleType {
                        pmty_desc: ModuleTypeDesc::Pmty_ident(mod_ident),
                        pmty_loc: loc,
                        pmty_attributes: vec![],
                    };
                    Some(FunctorArg {
                        attrs,
                        name: mknoloc("_".to_string()),
                        mod_type: Some(mod_type),
                        start_pos,
                    })
                }
            }
        }
        Token::Underscore => {
            // Anonymous parameter: _ : modtype
            p.next();
            let name_loc = mk_loc(&start_pos, &p.prev_end_pos);
            p.expect(Token::Colon);
            let mod_type = parse_module_type(p);
            Some(FunctorArg {
                attrs,
                name: with_loc("_".to_string(), name_loc),
                mod_type: Some(mod_type),
                start_pos,
            })
        }
        Token::Lparen => {
            // Unit parameter: ()
            p.next();
            p.expect(Token::Rparen);
            let name_loc = mk_loc(&start_pos, &p.prev_end_pos);
            Some(FunctorArg {
                attrs,
                name: with_loc("*".to_string(), name_loc),
                mod_type: None,
                start_pos,
            })
        }
        Token::Lbrace => {
            // Anonymous module signature: { ... } (punning for _ : { ... })
            let mod_type = parse_module_type(p);
            Some(FunctorArg {
                attrs,
                name: mknoloc("_".to_string()),
                mod_type: Some(mod_type),
                start_pos,
            })
        }
        _ => None,
    }
}

/// Parse functor arguments: ( arg1, arg2, ... )
fn parse_functor_args(p: &mut Parser<'_>) -> Vec<FunctorArg> {
    let start_pos = p.start_pos.clone();
    p.expect(Token::Lparen);

    let mut args = vec![];
    while p.token != Token::Rparen && p.token != Token::Eof {
        if let Some(arg) = parse_functor_arg(p) {
            args.push(arg);
        } else {
            break;
        }

        // Handle comma
        if p.token == Token::Comma {
            p.next();
        } else {
            break;
        }
    }

    p.expect(Token::Rparen);

    // If no args were parsed, treat as unit functor: ()
    if args.is_empty() {
        vec![FunctorArg {
            attrs: vec![],
            name: with_loc("*".to_string(), mk_loc(&start_pos, &p.prev_end_pos)),
            mod_type: None,
            start_pos,
        }]
    } else {
        args
    }
}

/// Parse a functor module expression: (args) => body
fn parse_functor_module_expr(p: &mut Parser<'_>) -> ModuleExpr {
    let start_pos = p.start_pos.clone();
    let args = parse_functor_args(p);

    // Optional return type constraint: (args) : RetType => body
    let return_type = if p.token == Token::Colon {
        p.next();
        Some(parse_module_type(p))
    } else {
        None
    };

    p.expect(Token::EqualGreater);

    // Parse the body
    let rhs_module_expr = parse_module_expr(p);

    // Apply return type constraint if present
    let rhs_module_expr = match return_type {
        Some(mod_type) => {
            let loc = mk_loc(
                &rhs_module_expr.pmod_loc.loc_start,
                &mod_type.pmty_loc.loc_end,
            );
            ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_constraint(
                    Box::new(rhs_module_expr),
                    Box::new(mod_type),
                ),
                pmod_loc: loc,
                pmod_attributes: vec![],
            }
        }
        None => rhs_module_expr,
    };

    let end_pos = p.prev_end_pos.clone();

    // Fold arguments into nested functors (right to left)
    args.into_iter().rev().fold(rhs_module_expr, |acc, arg| {
        let loc = mk_loc(&arg.start_pos, &end_pos);
        ModuleExpr {
            pmod_desc: ModuleExprDesc::Pmod_functor(
                arg.name,
                arg.mod_type.map(Box::new),
                Box::new(acc),
            ),
            pmod_loc: loc,
            pmod_attributes: arg.attrs,
        }
    })
}

/// Parse structure items within braces.
fn parse_structure_in_braces(p: &mut Parser<'_>) -> Vec<StructureItem> {
    let mut items = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        // Skip semicolons
        while p.token == Token::Semicolon {
            p.next();
        }

        if p.token == Token::Rbrace || p.token == Token::Eof {
            break;
        }

        if let Some(item) = parse_structure_item(p) {
            items.push(item);
        }
    }

    items
}

// ============================================================================
// Module Type Parsing
// ============================================================================

/// Parse a module type.
pub fn parse_module_type(p: &mut Parser<'_>) -> ModuleType {
    let start_pos = p.start_pos.clone();

    let typ = parse_primary_module_type(p);

    // Handle with constraint: S with type t = ...
    if matches!(&p.token, Token::Lident(s) if s == "with") {
        p.next();
        let constraints = parse_with_constraints(p);
        let loc = mk_loc(&start_pos, &p.prev_end_pos);
        ModuleType {
            pmty_desc: ModuleTypeDesc::Pmty_with(Box::new(typ), constraints),
            pmty_loc: loc,
            pmty_attributes: vec![],
        }
    } else {
        typ
    }
}

/// Parse a primary module type.
fn parse_primary_module_type(p: &mut Parser<'_>) -> ModuleType {
    let start_pos = p.start_pos.clone();

    match &p.token {
        Token::Uident(_) | Token::Lident(_) => {
            // Module type path
            let lid = parse_module_long_ident(p);
            ModuleType {
                pmty_desc: ModuleTypeDesc::Pmty_ident(lid),
                pmty_loc: mk_loc(&start_pos, &p.prev_end_pos),
                pmty_attributes: vec![],
            }
        }
        Token::Lbrace => {
            // Signature: { ... }
            p.next();
            let items = parse_signature_in_braces(p);
            p.expect(Token::Rbrace);
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            ModuleType {
                pmty_desc: ModuleTypeDesc::Pmty_signature(items),
                pmty_loc: loc,
                pmty_attributes: vec![],
            }
        }
        Token::Lparen => {
            p.next();
            let inner = parse_module_type(p);
            p.expect(Token::Rparen);
            inner
        }
        Token::Module => {
            // module type of M
            p.next();
            if matches!(&p.token, Token::Lident(s) if s == "type") {
                p.next();
                if matches!(&p.token, Token::Lident(s) if s == "of") {
                    p.next();
                    let mod_expr = parse_module_expr(p);
                    let loc = mk_loc(&start_pos, &p.prev_end_pos);
                    return ModuleType {
                        pmty_desc: ModuleTypeDesc::Pmty_typeof(Box::new(mod_expr)),
                        pmty_loc: loc,
                        pmty_attributes: vec![],
                    };
                }
            }
            p.err(DiagnosticCategory::Message(
                "Expected 'type of'".to_string(),
            ));
            recover::default_module_type()
        }
        Token::Percent => {
            // Module type extension
            let ext = parse_extension(p);
            let loc = mk_loc(&start_pos, &p.prev_end_pos);
            ModuleType {
                pmty_desc: ModuleTypeDesc::Pmty_extension(ext),
                pmty_loc: loc,
                pmty_attributes: vec![],
            }
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected module type".to_string(),
            ));
            recover::default_module_type()
        }
    }
}

/// Parse signature items within braces.
fn parse_signature_in_braces(p: &mut Parser<'_>) -> Vec<SignatureItem> {
    let mut items = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        // Skip semicolons
        while p.token == Token::Semicolon {
            p.next();
        }

        if p.token == Token::Rbrace || p.token == Token::Eof {
            break;
        }

        if let Some(item) = parse_signature_item(p) {
            items.push(item);
        }
    }

    items
}

/// Parse with constraints.
fn parse_with_constraints(p: &mut Parser<'_>) -> Vec<WithConstraint> {
    let mut constraints = vec![];

    loop {
        if let Some(c) = parse_with_constraint(p) {
            constraints.push(c);
        }

        if p.token == Token::And {
            p.next();
        } else {
            break;
        }
    }

    constraints
}

/// Parse a single with constraint.
fn parse_with_constraint(p: &mut Parser<'_>) -> Option<WithConstraint> {
    match &p.token {
        Token::Typ => {
            p.next();
            let lid = parse_type_long_ident(p);
            let type_name = lid.txt.last().to_string();
            let type_loc = lid.loc.clone();

            // Parse type parameters if present
            let params = if p.token == Token::LessThan {
                parse_type_params(p)
            } else {
                vec![]
            };

            p.expect(Token::Equal);
            let typ = typ::parse_typ_expr(p);

            Some(WithConstraint::Pwith_type(
                lid,
                TypeDeclaration {
                    ptype_name: mknoloc(type_name),
                    ptype_params: params,
                    ptype_cstrs: vec![],
                    ptype_kind: TypeKind::Ptype_abstract,
                    ptype_private: PrivateFlag::Public,
                    ptype_manifest: Some(typ),
                    ptype_attributes: vec![],
                    ptype_loc: type_loc,
                },
            ))
        }
        Token::Module => {
            p.next();
            let lid = parse_module_long_ident(p);
            p.expect(Token::Equal);
            let lid2 = parse_module_long_ident(p);
            Some(WithConstraint::Pwith_module(lid, lid2))
        }
        _ => None,
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if a token can start an expression.
fn is_expr_start(token: &Token) -> bool {
    matches!(
        token,
        Token::True
            | Token::False
            | Token::Int { .. }
            | Token::Float { .. }
            | Token::String(_)
            | Token::Codepoint { .. }
            | Token::Lident(_)
            | Token::Uident(_)
            | Token::Lparen
            | Token::Lbracket
            | Token::Lbrace
            | Token::If
            | Token::Switch
            | Token::While
            | Token::For
            | Token::Try
            | Token::LessThan
            | Token::Minus
            | Token::Plus
            | Token::Bang
            | Token::Await
            | Token::Assert
            | Token::List
            | Token::Dict
            | Token::Hash
            | Token::Backtick
    )
}

/// Parse a module long identifier.
fn parse_module_long_ident(p: &mut Parser<'_>) -> Loc<Longident> {
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
        // Try lowercase for module type names
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

    let lid = super::core::build_longident(&path_parts);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    with_loc(lid, loc)
}

/// Parse a type long identifier.
fn parse_type_long_ident(p: &mut Parser<'_>) -> Loc<Longident> {
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

    let lid = super::core::build_longident(&path_parts);
    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    with_loc(lid, loc)
}

/// Parse let bindings.
fn parse_let_bindings(p: &mut Parser<'_>, _rec_flag: RecFlag) -> Vec<ValueBinding> {
    let mut bindings = vec![];

    loop {
        let start_pos = p.start_pos.clone();
        let attrs = parse_attributes(p);
        let pat = pattern::parse_pattern(p);

        // Handle optional type annotation: let x: int = ...
        let pat = if p.token == Token::Colon {
            p.next();
            let typ = typ::parse_typ_expr(p);
            // Create a constraint pattern
            let loc = mk_loc(&pat.ppat_loc.loc_start, &typ.ptyp_loc.loc_end);
            Pattern {
                ppat_desc: PatternDesc::Ppat_constraint(Box::new(pat), typ),
                ppat_loc: loc,
                ppat_attributes: vec![],
            }
        } else {
            pat
        };

        p.expect(Token::Equal);
        let expr = expr::parse_expr(p);

        let loc = mk_loc(&start_pos, &p.prev_end_pos);
        bindings.push(ValueBinding {
            pvb_pat: pat,
            pvb_expr: expr,
            pvb_attributes: attrs,
            pvb_loc: loc,
        });

        if p.token == Token::And {
            p.next();
        } else {
            break;
        }
    }

    bindings
}

/// Parse type declarations.
fn parse_type_declarations(p: &mut Parser<'_>) -> Vec<TypeDeclaration> {
    let mut decls = vec![];

    loop {
        if let Some(decl) = parse_type_declaration(p) {
            decls.push(decl);
        }

        if p.token == Token::And {
            p.next();
        } else {
            break;
        }
    }

    decls
}

/// Parse a single type declaration.
fn parse_type_declaration(p: &mut Parser<'_>) -> Option<TypeDeclaration> {
    let start_pos = p.start_pos.clone();
    let attrs = parse_attributes(p);

    // Parse type name first (ReScript uses t<'a> not 'a t syntax)
    let name = match &p.token {
        Token::Lident(n) => {
            let n = n.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(n, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected type name".to_string(),
            ));
            return None;
        }
    };

    // Parse type parameters after name (e.g., t<'a, 'b> or t<+'a, -'b>)
    let params = if p.token == Token::LessThan {
        parse_type_params_angle(p)
    } else {
        vec![]
    };

    // Parse optional manifest and/or kind
    let (manifest, kind, private) = if p.token == Token::Equal {
        p.next();
        let private = if p.token == Token::Private {
            p.next();
            PrivateFlag::Private
        } else {
            PrivateFlag::Public
        };

        // Check for variant or record
        match &p.token {
            Token::Bar => {
                // Variant type starting with | (e.g., type t = | A | B)
                let kind = parse_type_kind(p);
                (None, kind, private)
            }
            Token::Lbrace => {
                // Record type
                let kind = parse_type_kind(p);
                (None, kind, private)
            }
            Token::Uident(_) => {
                // Could be a variant type starting with constructor (type t = A | B)
                // or a type alias (type t = SomeType)
                // We need to look ahead to see if there's a | after the constructor
                let is_variant = p.lookahead(|state| {
                    // Skip the constructor name
                    state.next();
                    // Check if followed by | or ( ... ) |
                    state.token == Token::Bar
                        || (state.token == Token::Lparen && {
                            // Skip the argument list
                            let mut depth = 1;
                            state.next();
                            while depth > 0 && state.token != Token::Eof {
                                match state.token {
                                    Token::Lparen => depth += 1,
                                    Token::Rparen => depth -= 1,
                                    _ => {}
                                }
                                state.next();
                            }
                            state.token == Token::Bar
                        })
                        || (state.token == Token::Lbrace && {
                            // Skip the inline record
                            let mut depth = 1;
                            state.next();
                            while depth > 0 && state.token != Token::Eof {
                                match state.token {
                                    Token::Lbrace => depth += 1,
                                    Token::Rbrace => depth -= 1,
                                    _ => {}
                                }
                                state.next();
                            }
                            state.token == Token::Bar
                        })
                });

                if is_variant {
                    // Parse as variant type without leading |
                    let constructors = parse_constructors_without_leading_bar(p);
                    (None, TypeKind::Ptype_variant(constructors), private)
                } else {
                    // Parse as manifest type
                    let typ = typ::parse_typ_expr(p);
                    (Some(typ), TypeKind::Ptype_abstract, private)
                }
            }
            Token::DotDot => {
                // Extensible variant: type t = ..
                p.next();
                (None, TypeKind::Ptype_open, private)
            }
            _ => {
                // Manifest type
                let typ = typ::parse_typ_expr(p);
                (Some(typ), TypeKind::Ptype_abstract, private)
            }
        }
    } else {
        (None, TypeKind::Ptype_abstract, PrivateFlag::Public)
    };

    // Parse constraints (constraint 't = type)
    let mut cstrs = vec![];
    while p.token == Token::Constraint {
        p.next();
        // Parse the type variable
        p.expect(Token::SingleQuote);
        let var = if let Token::Lident(name) = &p.token {
            let name = name.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            }
        } else {
            p.err(DiagnosticCategory::Message(
                "Expected type variable in constraint".to_string(),
            ));
            break;
        };
        p.expect(Token::Equal);
        let typ = typ::parse_typ_expr(p);
        cstrs.push((var, typ, mk_loc(&start_pos, &p.prev_end_pos)));
    }

    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    Some(TypeDeclaration {
        ptype_name: name,
        ptype_params: params,
        ptype_cstrs: cstrs,
        ptype_kind: kind,
        ptype_private: private,
        ptype_manifest: manifest,
        ptype_attributes: attrs,
        ptype_loc: loc,
    })
}

/// Parse type parameters.
fn parse_type_params(p: &mut Parser<'_>) -> Vec<(CoreType, Variance)> {
    let mut params = vec![];

    if p.token == Token::Lparen {
        p.next();
        while p.token != Token::Rparen && p.token != Token::Eof {
            let variance = Variance::Invariant;
            if p.token == Token::SingleQuote {
                p.next();
                if let Token::Lident(name) = &p.token {
                    let name = name.clone();
                    let loc = mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    let typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                        ptyp_loc: loc,
                        ptyp_attributes: vec![],
                    };
                    params.push((typ, variance));
                }
            }
            if !p.optional(&Token::Comma) {
                break;
            }
        }
        p.expect(Token::Rparen);
    } else if p.token == Token::SingleQuote {
        p.next();
        if let Token::Lident(name) = &p.token {
            let name = name.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            let typ = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            };
            params.push((typ, Variance::Invariant));
        }
    }

    params
}

/// Parse type parameters in angle bracket syntax: <'a, +'b, -'c>
fn parse_type_params_angle(p: &mut Parser<'_>) -> Vec<(CoreType, Variance)> {
    let mut params = vec![];

    if p.token != Token::LessThan {
        return params;
    }
    p.next(); // consume <

    while p.token != Token::GreaterThan && p.token != Token::Eof {
        // Parse optional variance: +, -, or nothing
        let variance = match &p.token {
            Token::Plus => {
                p.next();
                Variance::Covariant
            }
            Token::Minus => {
                p.next();
                Variance::Contravariant
            }
            _ => Variance::Invariant,
        };

        // Parse type variable: 'name
        if p.token == Token::SingleQuote {
            p.next();
            if let Token::Lident(name) = &p.token {
                let name = name.clone();
                let loc = mk_loc(&p.start_pos, &p.end_pos);
                p.next();
                let typ = CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                    ptyp_loc: loc,
                    ptyp_attributes: vec![],
                };
                params.push((typ, variance));
            }
        }

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::GreaterThan);
    params
}

/// Parse type kind (variant or record).
fn parse_type_kind(p: &mut Parser<'_>) -> TypeKind {
    match &p.token {
        Token::Bar => {
            // Variant type
            let constructors = parse_constructors(p);
            TypeKind::Ptype_variant(constructors)
        }
        Token::Lbrace => {
            // Record type
            p.next();
            let labels = parse_label_declarations(p);
            p.expect(Token::Rbrace);
            TypeKind::Ptype_record(labels)
        }
        _ => TypeKind::Ptype_abstract,
    }
}

/// Parse variant constructors (starting with |).
fn parse_constructors(p: &mut Parser<'_>) -> Vec<ConstructorDeclaration> {
    let mut constructors = vec![];

    while p.token == Token::Bar {
        p.next();
        if let Some(c) = parse_constructor(p) {
            constructors.push(c);
        }
    }

    constructors
}

/// Parse variant constructors where the first doesn't have a leading |.
fn parse_constructors_without_leading_bar(p: &mut Parser<'_>) -> Vec<ConstructorDeclaration> {
    let mut constructors = vec![];

    // Parse the first constructor (no leading |)
    if let Some(c) = parse_constructor(p) {
        constructors.push(c);
    }

    // Parse remaining constructors (with leading |)
    while p.token == Token::Bar {
        p.next();
        if let Some(c) = parse_constructor(p) {
            constructors.push(c);
        }
    }

    constructors
}

/// Parse a single constructor.
fn parse_constructor(p: &mut Parser<'_>) -> Option<ConstructorDeclaration> {
    let start_pos = p.start_pos.clone();

    let name = match &p.token {
        Token::Uident(n) => {
            let n = n.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(n, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected constructor name".to_string(),
            ));
            return None;
        }
    };

    // Parse constructor arguments
    let args = if p.token == Token::Lparen {
        p.next();
        let mut args = vec![];
        while p.token != Token::Rparen && p.token != Token::Eof {
            args.push(typ::parse_typ_expr(p));
            if !p.optional(&Token::Comma) {
                break;
            }
        }
        p.expect(Token::Rparen);
        ConstructorArguments::Pcstr_tuple(args)
    } else if p.token == Token::Lbrace {
        p.next();
        let labels = parse_label_declarations(p);
        p.expect(Token::Rbrace);
        ConstructorArguments::Pcstr_record(labels)
    } else {
        ConstructorArguments::Pcstr_tuple(vec![])
    };

    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    Some(ConstructorDeclaration {
        pcd_name: name,
        pcd_args: args,
        pcd_res: None,
        pcd_loc: loc,
        pcd_attributes: vec![],
    })
}

/// Parse record label declarations.
/// Handles both regular fields and spread syntax (...typ).
fn parse_label_declarations(p: &mut Parser<'_>) -> Vec<LabelDeclaration> {
    let mut labels = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        // Handle spread: ...typ (skip for now, not represented in AST)
        if p.token == Token::DotDotDot {
            p.next();
            let _spread_type = typ::parse_typ_expr(p);
            // TODO: Handle spread in record type definitions properly
            // For now, skip it
            if !p.optional(&Token::Comma) {
                break;
            }
            continue;
        }

        if let Some(l) = parse_label_declaration(p) {
            labels.push(l);
        }
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    labels
}

/// Parse a single label declaration.
fn parse_label_declaration(p: &mut Parser<'_>) -> Option<LabelDeclaration> {
    let start_pos = p.start_pos.clone();

    let mutable = if p.token == Token::Mutable {
        p.next();
        MutableFlag::Mutable
    } else {
        MutableFlag::Immutable
    };

    let name = match &p.token {
        Token::Lident(n) => {
            let n = n.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(n, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected field name".to_string(),
            ));
            return None;
        }
    };

    // Check for optional field marker
    let is_optional = p.optional(&Token::Question);

    p.expect(Token::Colon);
    let typ = typ::parse_typ_expr(p);

    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    Some(LabelDeclaration {
        pld_name: name,
        pld_mutable: mutable,
        pld_type: typ,
        pld_loc: loc,
        pld_attributes: vec![],
        pld_optional: is_optional,
    })
}

/// Parse a value description (for external).
fn parse_value_description(p: &mut Parser<'_>) -> ValueDescription {
    let start_pos = p.start_pos.clone();
    let attrs = parse_attributes(p);

    let name = match &p.token {
        Token::Lident(n) => {
            let n = n.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(n, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected value name".to_string(),
            ));
            mknoloc("error".to_string())
        }
    };

    p.expect(Token::Colon);
    let typ = typ::parse_typ_expr(p);

    // Parse optional primitive
    let prim = if p.token == Token::Equal {
        p.next();
        let mut prims = vec![];
        while let Token::String(s) = &p.token {
            prims.push(s.clone());
            p.next();
        }
        prims
    } else {
        vec![]
    };

    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    ValueDescription {
        pval_name: name,
        pval_type: typ,
        pval_prim: prim,
        pval_attributes: attrs,
        pval_loc: loc,
    }
}

/// Parse an extension constructor (for exception).
pub fn parse_extension_constructor(p: &mut Parser<'_>) -> ExtensionConstructor {
    let start_pos = p.start_pos.clone();
    let attrs = parse_attributes(p);

    let constructor = parse_constructor(p).unwrap_or_else(|| ConstructorDeclaration {
        pcd_name: mknoloc("Error".to_string()),
        pcd_args: ConstructorArguments::Pcstr_tuple(vec![]),
        pcd_res: None,
        pcd_loc: mk_loc(&start_pos, &p.prev_end_pos),
        pcd_attributes: vec![],
    });

    ExtensionConstructor {
        pext_name: constructor.pcd_name,
        pext_kind: ExtensionConstructorKind::Pext_decl(constructor.pcd_args, None),
        pext_loc: constructor.pcd_loc,
        pext_attributes: attrs,
    }
}

/// Parse a module definition.
fn parse_module_definition(p: &mut Parser<'_>) -> Option<StructureItemDesc> {
    let _start_pos = p.start_pos.clone();

    // Check for module type
    if p.token == Token::Typ {
        p.next();
        let mtd = parse_module_type_declaration(p);
        return Some(StructureItemDesc::Pstr_modtype(mtd));
    }

    // Check for rec
    let rec_flag = if p.token == Token::Rec {
        p.next();
        RecFlag::Recursive
    } else {
        RecFlag::Nonrecursive
    };

    let name = match &p.token {
        Token::Uident(n) => {
            let n = n.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(n, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected module name".to_string(),
            ));
            return None;
        }
    };

    // Parse optional module type annotation
    // TODO: Use mod_type for constraint
    let _mod_type = if p.token == Token::Colon {
        p.next();
        Some(parse_module_type(p))
    } else {
        None
    };

    p.expect(Token::Equal);
    let mod_expr = parse_module_expr(p);

    let binding = ModuleBinding {
        pmb_name: name,
        pmb_expr: mod_expr,
        pmb_attributes: vec![],
        pmb_loc: mk_loc(&_start_pos, &p.prev_end_pos),
    };

    if rec_flag == RecFlag::Recursive {
        Some(StructureItemDesc::Pstr_recmodule(vec![binding]))
    } else {
        Some(StructureItemDesc::Pstr_module(binding))
    }
}

/// Parse a module declaration (for signature).
fn parse_module_declaration(p: &mut Parser<'_>) -> ModuleDeclaration {
    let start_pos = p.start_pos.clone();
    let attrs = parse_attributes(p);

    let name = match &p.token {
        Token::Uident(n) => {
            let n = n.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(n, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected module name".to_string(),
            ));
            mknoloc("Error".to_string())
        }
    };

    p.expect(Token::Colon);
    let mod_type = parse_module_type(p);

    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    ModuleDeclaration {
        pmd_name: name,
        pmd_type: mod_type,
        pmd_attributes: attrs,
        pmd_loc: loc,
    }
}

/// Parse a module type declaration.
fn parse_module_type_declaration(p: &mut Parser<'_>) -> ModuleTypeDeclaration {
    let start_pos = p.start_pos.clone();
    let attrs = parse_attributes(p);

    let name = match &p.token {
        Token::Uident(n) | Token::Lident(n) => {
            let n = n.clone();
            let loc = mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(n, loc)
        }
        _ => {
            p.err(DiagnosticCategory::Message(
                "Expected module type name".to_string(),
            ));
            mknoloc("error".to_string())
        }
    };

    let typ = if p.token == Token::Equal {
        p.next();
        Some(parse_module_type(p))
    } else {
        None
    };

    let loc = mk_loc(&start_pos, &p.prev_end_pos);
    ModuleTypeDeclaration {
        pmtd_name: name,
        pmtd_type: typ,
        pmtd_attributes: attrs,
        pmtd_loc: loc,
    }
}

/// Parse attributes.
fn parse_attributes(p: &mut Parser<'_>) -> Attributes {
    let mut attrs = vec![];
    while p.token == Token::At {
        p.next();
        attrs.push(parse_attribute_body(p));
    }
    attrs
}

/// Parse an attribute body.
fn parse_attribute_body(p: &mut Parser<'_>) -> Attribute {
    let start_pos = p.start_pos.clone();
    let mut parts = vec![];

    // Parse attribute identifier
    loop {
        match &p.token {
            Token::Lident(name) | Token::Uident(name) => {
                parts.push(name.clone());
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

    let name = parts.join(".");
    let name_loc = mk_loc(&start_pos, &p.prev_end_pos);

    // Parse optional payload
    let payload = if p.token == Token::Lparen {
        p.next();
        let payload = parse_payload(p);
        p.expect(Token::Rparen);
        payload
    } else {
        Payload::PStr(vec![])
    };

    (with_loc(name, name_loc), payload)
}

/// Parse an attribute payload.
pub fn parse_payload(p: &mut Parser<'_>) -> Payload {
    // Simplified: skip payload content for now
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

    // Parse optional payload
    let payload = if p.token == Token::Lparen {
        p.next();
        let payload = parse_payload(p);
        p.expect(Token::Rparen);
        payload
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

    /// Parse source code into a structure with a timeout.
    /// Panics if parsing takes longer than PARSE_TIMEOUT.
    fn parse_with_timeout(source: &str) -> Structure {
        let source_owned = source.to_string();
        let source_for_error = source_owned.clone();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut parser = Parser::new("test.res", &source_owned);
            let structure = parse_structure(&mut parser);
            let _ = tx.send(structure);
        });

        match rx.recv_timeout(PARSE_TIMEOUT) {
            Ok(result) => result,
            Err(mpsc::RecvTimeoutError::Timeout) => {
                panic!(
                    "Parser timed out after {:?} for input: {}",
                    PARSE_TIMEOUT,
                    if source_for_error.len() > 100 {
                        format!("{}...", &source_for_error[..100])
                    } else {
                        source_for_error
                    }
                )
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                panic!("Parser thread panicked for input: {}", source_for_error)
            }
        }
    }

    #[test]
    fn test_parse_empty_structure() {
        let structure = parse_with_timeout("");
        assert!(structure.is_empty());
    }

    #[test]
    fn test_parse_let_binding() {
        let structure = parse_with_timeout("let x = 42");
        assert_eq!(structure.len(), 1);
        assert!(matches!(
            structure[0].pstr_desc,
            StructureItemDesc::Pstr_value { .. }
        ));
    }

    #[test]
    fn test_parse_open() {
        let structure = parse_with_timeout("open Belt");
        assert_eq!(structure.len(), 1);
        assert!(matches!(
            structure[0].pstr_desc,
            StructureItemDesc::Pstr_open(_)
        ));
    }

    #[test]
    fn test_parse_type_declaration() {
        let structure = parse_with_timeout("type t = int");
        assert_eq!(structure.len(), 1);
        assert!(matches!(
            structure[0].pstr_desc,
            StructureItemDesc::Pstr_type(..)
        ));
    }

    #[test]
    fn test_parse_variant_type() {
        let structure = parse_with_timeout("type color = | Red | Green | Blue");
        assert_eq!(structure.len(), 1);
        if let StructureItemDesc::Pstr_type(_, type_decls) = &structure[0].pstr_desc {
            assert_eq!(type_decls.len(), 1);
            if let TypeKind::Ptype_variant(constructors) = &type_decls[0].ptype_kind {
                assert_eq!(constructors.len(), 3);
            } else {
                panic!("Expected variant type");
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn test_parse_record_type() {
        let structure = parse_with_timeout("type person = { name: string, age: int }");
        assert_eq!(structure.len(), 1);
        if let StructureItemDesc::Pstr_type(_, type_decls) = &structure[0].pstr_desc {
            assert_eq!(type_decls.len(), 1);
            if let TypeKind::Ptype_record(labels) = &type_decls[0].ptype_kind {
                assert_eq!(labels.len(), 2);
            } else {
                panic!("Expected record type");
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn test_parse_module_definition() {
        let structure = parse_with_timeout("module M = { let x = 1 }");
        assert_eq!(structure.len(), 1);
        assert!(matches!(
            structure[0].pstr_desc,
            StructureItemDesc::Pstr_module(_)
        ));
    }

    #[test]
    fn test_parse_external() {
        let structure = parse_with_timeout("external log: string => unit = \"console.log\"");
        assert_eq!(structure.len(), 1);
        assert!(matches!(
            structure[0].pstr_desc,
            StructureItemDesc::Pstr_primitive(_)
        ));
    }

    #[test]
    fn test_parse_exception() {
        let structure = parse_with_timeout("exception MyError(string)");
        assert_eq!(structure.len(), 1);
        assert!(matches!(
            structure[0].pstr_desc,
            StructureItemDesc::Pstr_exception(_)
        ));
    }

    #[test]
    fn test_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Structure>();
        assert_send_sync::<Signature>();
    }
}
