//! Module parsing for ReScript.
//!
//! This module contains the module parsing logic, converting tokens
//! into module AST nodes (structures, signatures, module expressions, etc.).

use std::cell::RefCell;

use crate::location::{Location, Position};

use super::ast::*;
use super::core::{is_es6_arrow_functor, mknoloc, recover, with_loc};
use super::diagnostics::DiagnosticCategory;
use super::expr;
use super::longident::Longident;
use super::pattern;
use super::state::Parser;
use super::token::Token;
use super::typ;

// ============================================================================
// Inline Record Desugaring Context
// ============================================================================

/// Context for collecting inline record types during type declaration parsing.
/// Inline records like `type t = {permissions: {all: {stuff: bool}}}` get
/// desugared into separate type declarations with dotted names like `t.permissions.all`.
#[derive(Debug)]
pub struct InlineTypesContext {
    /// Collected inline types: (dotted_name, location, type_kind)
    pub found_inline_types: Vec<(String, Location, TypeKind)>,
    /// Type parameters from the outer type declaration
    pub params: Vec<(CoreType, Variance)>,
}

impl InlineTypesContext {
    fn new(params: Vec<(CoreType, Variance)>) -> Self {
        Self {
            found_inline_types: vec![],
            params,
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Convert a module type to a package type (for Ptyp_package).
/// For a simple module type identifier like `T`, creates `(T, [])`.
/// For `T with type t = int`, creates `(T, [(t, int)])`.
fn module_type_to_package(mt: &ModuleType) -> PackageType {
    match &mt.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => (lid.clone(), vec![]),
        ModuleTypeDesc::Pmty_with(base, constraints) => {
            // For "T with type t = int", extract base identifier and constraints
            let lid = if let ModuleTypeDesc::Pmty_ident(lid) = &base.pmty_desc {
                lid.clone()
            } else {
                with_loc(Longident::Lident("_".to_string()), mt.pmty_loc.clone())
            };

            // Convert WithConstraint to package type constraints
            let pkg_constraints: Vec<(Loc<Longident>, CoreType)> = constraints
                .iter()
                .filter_map(|c| match c {
                    WithConstraint::Pwith_type(type_lid, type_decl) => {
                        // Extract the manifest type from the type declaration
                        type_decl.ptype_manifest.as_ref().map(|manifest| {
                            (type_lid.clone(), manifest.clone())
                        })
                    }
                    WithConstraint::Pwith_typesubst(type_lid, type_decl) => {
                        // Type substitution also has a manifest
                        type_decl.ptype_manifest.as_ref().map(|manifest| {
                            (type_lid.clone(), manifest.clone())
                        })
                    }
                    // Module constraints are not relevant for package types
                    _ => None,
                })
                .collect();

            (lid, pkg_constraints)
        }
        _ => (with_loc(Longident::Lident("_".to_string()), mt.pmty_loc.clone()), vec![]),
    }
}

// ============================================================================
// Structure (Implementation) Parsing
// ============================================================================

/// Parse a structure (list of structure items).
pub fn parse_structure(p: &mut Parser<'_>) -> Structure {
    let mut items = vec![];

    while p.token != Token::Eof {
        // Skip leading semicolons
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
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            Some(StructureItemDesc::Pstr_open(OpenDescription {
                popen_lid: lid,
                popen_override: override_flag,
                popen_loc: loc,
                popen_attributes: attrs.clone(),
            }))
        }
        Token::Let { unwrap } => {
            let unwrap = *unwrap;
            p.next();
            let rec_flag = if p.token == Token::Rec {
                p.next();
                RecFlag::Recursive
            } else {
                RecFlag::Nonrecursive
            };
            let bindings = parse_let_bindings(p, start_pos.clone(), rec_flag, attrs.clone(), unwrap);
            Some(StructureItemDesc::Pstr_value(rec_flag, bindings))
        }
        Token::Typ => {
            let type_start = p.start_pos.clone(); // Capture 'type' keyword position
            p.next();
            // `type t += ...` (type extension) vs `type t = ...` (type declaration)
            let is_type_extension = p.token != Token::Rec
                && p.lookahead(|state| {
                    // Parse the (possibly dotted) type path.
                    loop {
                        match &state.token {
                            Token::Lident(_) | Token::Uident(_) => {
                                state.next();
                                if state.token == Token::Dot {
                                    state.next();
                                    continue;
                                }
                            }
                            _ => {}
                        }
                        break;
                    }

                    // Optional type params: <...>
                    if state.token == Token::LessThan {
                        let mut depth = 1;
                        state.next();
                        while depth > 0 && state.token != Token::Eof {
                            match state.token {
                                Token::LessThan => depth += 1,
                                Token::GreaterThan => depth -= 1,
                                _ => {}
                            }
                            state.next();
                        }
                    }

                    state.token == Token::PlusEqual
                });

            if is_type_extension {
                let ext = parse_type_extension(p, attrs.clone());
                Some(StructureItemDesc::Pstr_typext(ext))
            } else {
                let mut rec_flag = if p.token == Token::Rec {
                    p.next();
                    RecFlag::Recursive
                } else {
                    RecFlag::Nonrecursive
                };
                let (decls, has_inline_types) = parse_type_declarations(p, attrs.clone(), type_start);
                // Inline record types require the type to be recursive
                if has_inline_types {
                    rec_flag = RecFlag::Recursive;
                }
                Some(StructureItemDesc::Pstr_type(rec_flag, decls))
            }
        }
        Token::External => {
            p.next();
            let vd = parse_value_description(p, attrs.clone(), start_pos.clone());
            Some(StructureItemDesc::Pstr_primitive(vd))
        }
        Token::Exception => {
            // Capture start position BEFORE consuming 'exception' for accurate location
            let exception_start = p.start_pos.clone();
            p.next();
            let ext = parse_extension_constructor(p, attrs.clone(), Some(exception_start));
            Some(StructureItemDesc::Pstr_exception(ext))
        }
        Token::Module => {
            // Capture module_start BEFORE consuming 'module' for accurate locations
            let module_start = p.start_pos.clone();
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
                parse_module_definition(p, module_start, attrs.clone())
            }
        }
        Token::Include => {
            p.next();
            let mod_expr = parse_module_expr(p);
            Some(StructureItemDesc::Pstr_include(IncludeDeclaration {
                pincl_mod: mod_expr,
                pincl_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
                pincl_attributes: attrs.clone(),
            }))
        }
        Token::At | Token::AtAt => {
            // Floating attribute or extension
            // Capture start_pos before consuming @ or @@ so attribute location includes it
            let attr_start = p.start_pos.clone();
            if p.token == Token::AtAt {
                p.next();
                let attr = parse_attribute_body(p, attr_start);
                Some(StructureItemDesc::Pstr_attribute(attr))
            } else {
                // @ might be a doc comment or similar
                p.next();
                let attr = parse_attribute_body(p, attr_start);
                Some(StructureItemDesc::Pstr_attribute(attr))
            }
        }
        Token::ModuleComment { loc, content } => {
            // Module-level doc comment (triple star /*** ... */) becomes a standalone res.doc attribute
            let loc = loc.clone();
            let content = content.clone();
            p.next();
            let attr = super::core::doc_comment_to_attribute(loc, content);
            Some(StructureItemDesc::Pstr_attribute(attr))
        }
        Token::PercentPercent => {
            // Structure-level extension (%%raw(...), %%private(...), etc.)
            // Capture start position BEFORE consuming %% so extension location includes it
            let ext_start = p.start_pos.clone();
            p.next();
            // Parse extension name - can be dotted like %%item.extension
            let id_start = ext_start.clone();
            let mut parts = vec![];
            loop {
                match &p.token {
                    Token::Lident(name) | Token::Uident(name) => {
                        parts.push(name.clone());
                        p.next();
                    }
                    // Keywords can be used as extension names (e.g., %%private)
                    token if token.is_keyword() => {
                        parts.push(token.to_string());
                        p.next();
                    }
                    _ if parts.is_empty() => {
                        p.err(DiagnosticCategory::Message(
                            "Expected extension identifier".to_string(),
                        ));
                        parts.push("error".to_string());
                        break;
                    }
                    _ => break,
                }
                // Check for dot to continue dotted extension name
                if p.token == Token::Dot {
                    p.next();
                } else {
                    break;
                }
            }
            let id_name = parts.join(".");
            let id = with_loc(id_name, p.mk_loc(&id_start, &p.prev_end_pos));
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

    // Consume trailing semicolon if present (OCaml includes this in the structure item location)
    if p.token == Token::Semicolon {
        p.next();
    }

    desc.map(|d| StructureItem {
        pstr_desc: d,
        pstr_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
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
            // Check for override flag: open! M
            let override_flag = if p.token == Token::Bang {
                p.next();
                OverrideFlag::Override
            } else {
                OverrideFlag::Fresh
            };
            let lid = parse_module_long_ident(p);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            Some(SignatureItemDesc::Psig_open(OpenDescription {
                popen_lid: lid,
                popen_override: override_flag,
                popen_loc: loc,
                popen_attributes: attrs.clone(),
            }))
        }
        Token::Let { .. } => {
            // For signature let declarations, OCaml's value_description location
            // starts at 'let', not at the attributes (different from external in structure)
            let let_pos = p.start_pos.clone();
            p.next();
            let vd = parse_value_description(p, attrs.clone(), let_pos);
            Some(SignatureItemDesc::Psig_value(vd))
        }
        Token::Typ => {
            let type_start = p.start_pos.clone(); // Capture 'type' keyword position
            p.next();
            let is_type_extension = p.token != Token::Rec
                && p.lookahead(|state| {
                    loop {
                        match &state.token {
                            Token::Lident(_) | Token::Uident(_) => {
                                state.next();
                                if state.token == Token::Dot {
                                    state.next();
                                    continue;
                                }
                            }
                            _ => {}
                        }
                        break;
                    }

                    if state.token == Token::LessThan {
                        let mut depth = 1;
                        state.next();
                        while depth > 0 && state.token != Token::Eof {
                            match state.token {
                                Token::LessThan => depth += 1,
                                Token::GreaterThan => depth -= 1,
                                _ => {}
                            }
                            state.next();
                        }
                    }

                    state.token == Token::PlusEqual
                });

            if is_type_extension {
                let ext = parse_type_extension(p, attrs.clone());
                Some(SignatureItemDesc::Psig_typext(ext))
            } else {
                let mut rec_flag = if p.token == Token::Rec {
                    p.next();
                    RecFlag::Recursive
                } else {
                    RecFlag::Nonrecursive
                };
                let (decls, has_inline_types) = parse_type_declarations(p, attrs.clone(), type_start);
                // Inline record types require the type to be recursive
                if has_inline_types {
                    rec_flag = RecFlag::Recursive;
                }
                Some(SignatureItemDesc::Psig_type(rec_flag, decls))
            }
        }
        Token::External => {
            p.next();
            let vd = parse_value_description(p, attrs.clone(), start_pos.clone());
            Some(SignatureItemDesc::Psig_value(vd))
        }
        Token::Exception => {
            // Capture start position BEFORE consuming 'exception' for accurate location
            let exception_start = p.start_pos.clone();
            p.next();
            let ext = parse_extension_constructor(p, attrs.clone(), Some(exception_start));
            Some(SignatureItemDesc::Psig_exception(ext))
        }
        Token::Module => {
            p.next();
            if p.token == Token::Typ {
                p.next();
                let mtd = parse_module_type_declaration(p, attrs.clone());
                Some(SignatureItemDesc::Psig_modtype(mtd))
            } else if p.token == Token::Rec {
                p.next();
                let mds = parse_rec_module_declarations(p, attrs.clone());
                Some(SignatureItemDesc::Psig_recmodule(mds))
            } else {
                let md = parse_module_declaration(p, attrs.clone());
                Some(SignatureItemDesc::Psig_module(md))
            }
        }
        Token::Include => {
            p.next();
            let mod_type = parse_module_type(p);
            Some(SignatureItemDesc::Psig_include(IncludeDescription {
                pincl_mod: mod_type,
                pincl_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
                pincl_attributes: attrs.clone(),
            }))
        }
        Token::At | Token::AtAt => {
            // Capture start_pos before consuming @ or @@ so attribute location includes it
            let attr_start = p.start_pos.clone();
            if p.token == Token::AtAt {
                p.next();
                let attr = parse_attribute_body(p, attr_start);
                Some(SignatureItemDesc::Psig_attribute(attr))
            } else {
                p.next();
                let attr = parse_attribute_body(p, attr_start);
                Some(SignatureItemDesc::Psig_attribute(attr))
            }
        }
        Token::ModuleComment { loc, content } => {
            // Module-level doc comment (triple star /*** ... */) becomes a standalone res.doc attribute
            let loc = loc.clone();
            let content = content.clone();
            p.next();
            let attr = super::core::doc_comment_to_attribute(loc, content);
            Some(SignatureItemDesc::Psig_attribute(attr))
        }
        Token::Percent | Token::PercentPercent => {
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
        psig_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
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
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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

/// Parse a module expression without consuming a trailing `: type` constraint.
/// Used for first-class modules where the constraint becomes Pexp_constraint + Ptyp_package.
pub fn parse_module_expr_without_constraint(p: &mut Parser<'_>) -> ModuleExpr {
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

    // NOTE: We intentionally DON'T handle `: type` constraint here

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
                pmod_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
                pmod_attributes: vec![],
            }
        }
        Token::Lbrace => {
            // Module structure: { ... }
            p.next();
            let items = parse_structure_in_braces(p);
            p.expect(Token::Rbrace);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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
                if p.token == Token::Rparen {
                    // Unit module expression: ()
                    p.next();
                    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                    ModuleExpr {
                        pmod_desc: ModuleExprDesc::Pmod_structure(vec![]),
                        pmod_loc: loc,
                        pmod_attributes: vec![],
                    }
                } else {
                let inner = parse_module_expr(p);
                p.expect(Token::Rparen);
                inner
                }
            }
        }
        Token::Percent => {
            // Module extension
            let ext = parse_extension(p);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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
            // Try to parse as unpack: unpack(expr : pkg_type)
            if matches!(&p.token, Token::Lident(s) if s == "unpack") {
                p.next();
                p.expect(Token::Lparen);
                let expr = expr::parse_expr(p);
                let final_expr = if p.token == Token::Colon {
                    p.next();
                    // Parse module type and convert to Ptyp_package
                    let mod_type = parse_module_type(p);
                    // Create Ptyp_package from module type
                    let pkg_type = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_package(
                            module_type_to_package(&mod_type),
                        ),
                        ptyp_loc: mod_type.pmty_loc.clone(),
                        ptyp_attributes: vec![],
                    };
                    // Wrap expression in Pexp_constraint
                    Expression {
                        pexp_desc: ExpressionDesc::Pexp_constraint(
                            Box::new(expr),
                            pkg_type,
                        ),
                        pexp_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
                        pexp_attributes: vec![],
                    }
                } else {
                    expr
                };
                p.expect(Token::Rparen);
                let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
                ModuleExpr {
                    pmod_desc: ModuleExprDesc::Pmod_unpack(Box::new(final_expr)),
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

        let args = if p.token == Token::Rparen {
            // Empty argument: F()
            p.next();
            let loc = p.mk_loc(&p.prev_end_pos, &p.prev_end_pos);
            vec![ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_structure(vec![]),
                pmod_loc: loc,
                pmod_attributes: vec![],
            }]
        } else {
            let mut args = vec![];
            loop {
                args.push(parse_module_expr(p));
                if p.token == Token::Comma {
                    p.next();
                    // Trailing comma.
                    if p.token == Token::Rparen {
                        break;
                    }
                    continue;
                }
                break;
            }
            p.expect(Token::Rparen);
            args
        };

        let loc = p.mk_loc(&start, &p.prev_end_pos);
        for arg in args {
            result = ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_apply(Box::new(result), Box::new(arg)),
                pmod_loc: loc.clone(),
                pmod_attributes: vec![],
            };
        }
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
                    let loc = p.mk_loc(&start_pos, &ident_end_pos);
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
                    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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
                    let loc = p.mk_loc(&start_pos, &ident_end_pos);
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
            let name_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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
            let name_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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
            name: with_loc("*".to_string(), p.mk_loc(&start_pos, &p.prev_end_pos)),
            mod_type: None,
            start_pos,
        }]
    } else {
        args
    }
}

/// Parse a functor module expression: (args) => body
fn parse_functor_module_expr(p: &mut Parser<'_>) -> ModuleExpr {
    let _start_pos = p.start_pos.clone();
    let args = parse_functor_args(p);

    // Optional return type constraint: (args) : RetType => body
    // Use parse_module_type_no_arrow to avoid interpreting `Set => A` as a functor type
    let return_type = if p.token == Token::Colon {
        p.next();
        Some(parse_module_type_no_arrow(p))
    } else {
        None
    };

    p.expect(Token::EqualGreater);

    // Parse the body
    let rhs_module_expr = parse_module_expr(p);

    // Apply return type constraint if present
    // Pmod_constraint(expr, type) represents "expr : type" and the location
    // should span from the start of the module type to the end of the module expr
    let rhs_module_expr = match return_type {
        Some(mod_type) => {
            let loc = p.mk_loc(
                &mod_type.pmty_loc.loc_start,
                &rhs_module_expr.pmod_loc.loc_end,
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
        let loc = p.mk_loc(&arg.start_pos, &end_pos);
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
    parse_module_type_impl(p, true, true)
}

/// Parse a module type without allowing ES6 arrow functor types.
/// Used when parsing return type constraints in functor expressions.
pub fn parse_module_type_no_arrow(p: &mut Parser<'_>) -> ModuleType {
    parse_module_type_impl(p, false, true)
}

/// Parse a module type without `with` constraint support.
/// Used for functor body types where `with` should apply to the outer functor.
fn parse_module_type_no_with(p: &mut Parser<'_>) -> ModuleType {
    parse_module_type_impl(p, true, false)
}

fn parse_module_type_impl(p: &mut Parser<'_>, es6_arrow: bool, parse_with: bool) -> ModuleType {
    let start_pos = p.start_pos.clone();

    // Parse attributes (e.g. `@attr module type of M`)
    let attrs = parse_attributes(p);

    // Parse functor types first (if allowed), then fall back to primary module types.
    let typ = if es6_arrow {
        parse_functor_module_type(p)
    } else {
        parse_primary_module_type(p)
    };

    // Handle with constraint: S with type t = ...
    // Only if parse_with is true - functor bodies don't parse `with` so it applies to the outer functor
    if parse_with && matches!(&p.token, Token::Lident(s) if s == "with") {
        p.next();
        let constraints = parse_with_constraints(p);
        let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
        let mut result = ModuleType {
            pmty_desc: ModuleTypeDesc::Pmty_with(Box::new(typ), constraints),
            pmty_loc: loc,
            pmty_attributes: vec![],
        };
        if !attrs.is_empty() {
            result.pmty_attributes.extend(attrs);
        }
        result
    } else {
        let mut result = typ;
        if !attrs.is_empty() {
            result.pmty_attributes.extend(attrs);
        }
        result
    }
}

fn parse_functor_module_type(p: &mut Parser<'_>) -> ModuleType {
    // Parenthesized functor params: `() => MT` or `(_ : S, X: T) => MT`
    if p.token == Token::Lparen && is_paren_functor_module_type(p) {
        return parse_paren_functor_module_type(p);
    }

    // Sugar: `S => MT` (unnamed parameter)
    let start_pos = p.start_pos.clone();
    let param_type = parse_primary_module_type(p);
    if p.token == Token::EqualGreater {
        p.next();
        // Use parse_module_type_no_with so `with` constraints apply to the outer functor
        let body = parse_module_type_no_with(p);
        let loc = p.mk_loc(&start_pos, &body.pmty_loc.loc_end);
        return ModuleType {
            pmty_desc: ModuleTypeDesc::Pmty_functor(
                mknoloc("_".to_string()),
                Some(Box::new(param_type)),
                Box::new(body),
            ),
            pmty_loc: loc,
            pmty_attributes: vec![],
        };
    }

    param_type
}

fn is_paren_functor_module_type(p: &mut Parser<'_>) -> bool {
    p.lookahead(|state| {
        let mut depth = 0;
        while state.token != Token::Eof {
            match state.token {
                Token::Lparen => depth += 1,
                Token::Rparen => {
                    depth -= 1;
                    if depth == 0 {
                        state.next();
                        break;
                    }
                }
                _ => {}
            }
            state.next();
        }

        state.token == Token::EqualGreater
    })
}

fn parse_paren_functor_module_type(p: &mut Parser<'_>) -> ModuleType {
    let functor_start = p.start_pos.clone();
    p.expect(Token::Lparen);

    #[derive(Debug)]
    struct FunctorParam {
        name: StringLoc,
        param: Option<ModuleType>,
        attrs: Attributes,
        start_pos: Position,
    }

    let mut params = vec![];

    if p.token == Token::Rparen {
        // Unit parameter: `() => MT` - OCaml uses "*" as the name for generative functors
        let start_pos = functor_start.clone();
        p.next();
        params.push(FunctorParam {
            name: mknoloc("*".to_string()),
            param: None,
            attrs: vec![],
            start_pos,
        });
    } else {
        while p.token != Token::Rparen && p.token != Token::Eof {
            let start_pos = p.start_pos.clone();
            // Parse parameter attributes like `@attr _: S` - attach to the functor type, not param
            let param_attrs = parse_attributes(p);

            match &p.token {
                Token::Underscore => {
                    let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    let name = with_loc("_".to_string(), loc);
                    let param = if p.token == Token::Colon {
                        p.next();
                        Some(parse_module_type(p))
                    } else {
                        None
                    };
                    params.push(FunctorParam {
                        name,
                        param,
                        attrs: param_attrs,
                        start_pos,
                    });
                }
                Token::Lident(_) | Token::Uident(_) => {
                    // `X: S` (named) or `S` (shorthand parameter type)
                    let is_named = p.lookahead(|state| {
                        state.next();
                        state.token == Token::Colon
                    });

                    if is_named {
                        let name = match &p.token {
                            Token::Lident(n) | Token::Uident(n) => {
                                let n = n.clone();
                                let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                                p.next();
                                with_loc(n, loc)
                            }
                            _ => mknoloc("_".to_string()),
                        };
                        p.expect(Token::Colon);
                        let mty = parse_module_type(p);
                        params.push(FunctorParam {
                            name,
                            param: Some(mty),
                            attrs: param_attrs,
                            start_pos,
                        });
                    } else {
                        // Shorthand: `(S, T) => U` (unnamed parameter types)
                        let param_ty = parse_module_type(p);
                        params.push(FunctorParam {
                            name: mknoloc("_".to_string()),
                            param: Some(param_ty),
                            attrs: param_attrs,
                            start_pos,
                        });
                    }
                }
                Token::Lparen => {
                    // Allow `()` as a unit parameter in multi-arg functors, and `(S)` as shorthand.
                    let is_unit = p.lookahead(|state| {
                        state.next();
                        state.token == Token::Rparen
                    });
                    if is_unit {
                        p.next();
                        p.expect(Token::Rparen);
                        params.push(FunctorParam {
                            name: mknoloc("*".to_string()),
                            param: None,
                            attrs: param_attrs,
                            start_pos,
                        });
                    } else {
                        let param_ty = parse_module_type(p);
                        params.push(FunctorParam {
                            name: mknoloc("_".to_string()),
                            param: Some(param_ty),
                            attrs: param_attrs,
                            start_pos,
                        });
                    }
                }
                _ => {
                    p.err(DiagnosticCategory::Message(
                        "Expected module type parameter".to_string(),
                    ));
                    break;
                }
            }

            if !p.optional(&Token::Comma) {
                break;
            }
        }
        p.expect(Token::Rparen);
    }

    p.expect(Token::EqualGreater);
    // Use parse_module_type_no_with so `with` constraints apply to the outer functor
    let body = parse_module_type_no_with(p);
    let end_pos = body.pmty_loc.loc_end.clone();

    params.into_iter().rev().fold(body, |acc, param| {
        let loc = p.mk_loc(&param.start_pos, &end_pos);
        ModuleType {
            pmty_desc: ModuleTypeDesc::Pmty_functor(
                param.name,
                param.param.map(Box::new),
                Box::new(acc),
            ),
            pmty_loc: loc,
            pmty_attributes: param.attrs,
        }
    })
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
                pmty_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
                pmty_attributes: vec![],
            }
        }
        Token::Lbrace => {
            // Signature: { ... }
            p.next();
            let items = parse_signature_in_braces(p);
            p.expect(Token::Rbrace);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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
            if p.token == Token::Typ {
                p.next();
                if p.token == Token::Of {
                    p.next();
                    let mod_expr = parse_module_expr(p);
                    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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

            // Parse type parameters if present (angle bracket syntax: <'a, 'b>)
            let params = if p.token == Token::LessThan {
                parse_type_params_angle(p)
            } else {
                vec![]
            };

            // Check for destructive substitution (:=) vs regular type equation (=)
            let is_substitution = p.token == Token::ColonEqual;
            if is_substitution {
                p.next();
            } else {
                p.expect(Token::Equal);
            }
            // Check for private keyword: type t = private string
            let private = if p.token == Token::Private {
                p.next();
                PrivateFlag::Private
            } else {
                PrivateFlag::Public
            };
            let typ = typ::parse_typ_expr(p);

            // Parse type constraints: constraint 'a = int constraint 'b = string
            let mut cstrs = vec![];
            while p.token == Token::Constraint {
                let cstr_start = p.start_pos.clone();
                p.next();
                p.expect(Token::SingleQuote);
                let var = if let Token::Lident(name) = &p.token {
                    let name = name.clone();
                    let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                        ptyp_loc: loc,
                        ptyp_attributes: vec![],
                    }
                } else {
                    break;
                };
                p.expect(Token::Equal);
                let cstr_typ = typ::parse_typ_expr(p);
                cstrs.push((var, cstr_typ, p.mk_loc(&cstr_start, &p.prev_end_pos)));
            }

            let decl = TypeDeclaration {
                ptype_name: mknoloc(type_name),
                ptype_params: params,
                ptype_cstrs: cstrs,
                ptype_kind: TypeKind::Ptype_abstract,
                ptype_private: private,
                ptype_manifest: Some(typ),
                ptype_attributes: vec![],
                ptype_loc: type_loc,
            };

            if is_substitution {
                Some(WithConstraint::Pwith_typesubst(lid, decl))
            } else {
                Some(WithConstraint::Pwith_type(lid, decl))
            }
        }
        Token::Module => {
            p.next();
            let lid = parse_module_long_ident(p);
            // Check for destructive substitution (:=) vs regular module equation (=)
            let is_substitution = p.token == Token::ColonEqual;
            if is_substitution {
                p.next();
            } else {
                p.expect(Token::Equal);
            }
            let lid2 = parse_module_long_ident(p);
            if is_substitution {
                Some(WithConstraint::Pwith_modsubst(lid, lid2))
            } else {
                Some(WithConstraint::Pwith_module(lid, lid2))
            }
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
            | Token::Percent
            | Token::Underscore
    )
}

/// Parse a module long identifier.
/// Handles both module paths (Foo.Bar.Baz) and module type paths (Foo.Bar.t).
/// Module type names are typically lowercase (e.g., RGLEvents.t).
fn parse_module_long_ident(p: &mut Parser<'_>) -> Loc<Longident> {
    let start_pos = p.start_pos.clone();
    let mut path_parts = vec![];

    loop {
        match &p.token {
            Token::Uident(name) => {
                path_parts.push(name.clone());
                p.next();
                if p.token == Token::Dot {
                    p.next();
                    continue;
                }
                break;
            }
            Token::Lident(name) => {
                // Allow lowercase as final component (for module type names like `t`)
                // This handles paths like RGLEvents.t where `t` is a module type name
                path_parts.push(name.clone());
                p.next();
                break;
            }
            _ => break,
        }
    }

    if path_parts.is_empty() {
        p.err(DiagnosticCategory::Message(
            "Expected module identifier".to_string(),
        ));
        path_parts.push("Error".to_string());
    }

    let lid = super::core::build_longident(&path_parts);
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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
    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
    with_loc(lid, loc)
}

/// Parse let bindings.
/// `start_pos` should be the position BEFORE the `let` keyword for the first binding.
fn parse_let_bindings(
    p: &mut Parser<'_>,
    start_pos: Position,
    _rec_flag: RecFlag,
    outer_attrs: Attributes,
    unwrap: bool,
) -> Vec<ValueBinding> {
    let mut bindings = vec![];
    let mut binding_start = start_pos;

    loop {
        let mut attrs = parse_attributes(p);
        // For the first binding, prepend the outer attributes
        if bindings.is_empty() {
            attrs = [outer_attrs.clone(), attrs].concat();
        }
        // Add let.unwrap attribute if this is let? (for all bindings, including `and` bindings)
        if unwrap {
            attrs.push((
                mknoloc("let.unwrap".to_string()),
                Payload::PStr(vec![]),
            ));
        }

        // Handle `and` after attributes: `@attr and foo = ...`
        // The `and` keyword is consumed here if present.
        if p.token == Token::And {
            // If this is the first binding, `and` is unexpected
            if bindings.is_empty() {
                p.err(DiagnosticCategory::Message(
                    "Unexpected token: And".to_string(),
                ));
            }
            p.next(); // consume `and`
        } else if !bindings.is_empty() {
            // Not `and` and not the first binding - we're done
            // But wait, we already consumed attributes. This shouldn't happen
            // if we properly check for continuation before starting a new iteration.
            // Since we already parsed attributes, we need to continue.
            // However, if we're here, it means we didn't have `and` after the previous binding.
            // This is a logic error - let's not hit this case.
            // Actually, we get here on the first iteration where bindings is empty.
        }

        let pat = pattern::parse_pattern(p);

        // Handle optional type annotation: let x: int = ...
        // Also track locally abstract types for Pexp_newtype wrapping
        let (pat, newtype_info) = if p.token == Token::Colon {
            p.next();

            // Check if this is `type a.` syntax (locally abstract types)
            // Only `type a.` syntax generates Pexp_newtype, not `'a.` syntax
            let is_locally_abstract = p.token == Token::Typ;

            let typ = typ::parse_typ_expr(p);

            // Extract type variables and inner type if this is a Ptyp_poly from `type a.` syntax
            // The inner type is used directly for Pexp_constraint (with Ptyp_constr for type vars)
            // But for the Ptyp_poly in the pattern, we need to substitute Ptyp_constr -> Ptyp_var
            let (final_typ, newtype_info) = match typ.ptyp_desc {
                CoreTypeDesc::Ptyp_poly(vars, inner) if is_locally_abstract => {
                    // Build the set of variable names for substitution
                    let var_set: std::collections::HashSet<&str> =
                        vars.iter().map(|v| v.txt.as_str()).collect();

                    // The inner type (with Ptyp_constr) is used for Pexp_constraint
                    let inner_for_constraint = (*inner).clone();

                    // Substitute Ptyp_constr -> Ptyp_var for the Ptyp_poly body
                    let substituted_inner = typ::substitute_type_vars((*inner).clone(), &var_set);

                    // Rebuild the Ptyp_poly with the substituted body
                    let substituted_typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_poly(vars.clone(), Box::new(substituted_inner)),
                        ptyp_loc: typ.ptyp_loc,
                        ptyp_attributes: typ.ptyp_attributes,
                    };

                    (substituted_typ, Some((vars, inner_for_constraint)))
                }
                _ => (typ, None),
            };

            // Create a constraint pattern
            let loc = p.mk_loc(&pat.ppat_loc.loc_start, &final_typ.ptyp_loc.loc_end);
            let pat = Pattern {
                ppat_desc: PatternDesc::Ppat_constraint(Box::new(pat), final_typ),
                ppat_loc: loc,
                ppat_attributes: vec![],
            };
            (pat, newtype_info)
        } else {
            (pat, None)
        };

        p.expect(Token::Equal);
        let mut expr = expr::parse_expr(p);

        // Wrap expression in Pexp_newtype for locally abstract types
        // Also add Pexp_constraint with the inner type (from Ptyp_poly)
        if let Some((newtype_vars, inner_type)) = newtype_info {
            // First wrap the expression in Pexp_constraint with the inner type
            let loc = expr.pexp_loc.clone();
            expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(expr), inner_type),
                pexp_loc: loc,
                pexp_attributes: vec![],
            };

            // Then wrap in Pexp_newtype for each type variable
            // Fold in reverse order so that the outermost newtype is the first variable
            for var in newtype_vars.into_iter().rev() {
                let loc = expr.pexp_loc.clone();
                expr = Expression {
                    pexp_desc: ExpressionDesc::Pexp_newtype(var, Box::new(expr)),
                    pexp_loc: loc,
                    pexp_attributes: vec![],
                };
            }
        }

        let loc = p.mk_loc(&binding_start, &p.prev_end_pos);
        bindings.push(ValueBinding {
            pvb_pat: pat,
            pvb_expr: expr,
            pvb_attributes: attrs,
            pvb_loc: loc,
        });

        // Check for `and` to continue with more bindings.
        // Also handle attributes/doc comments before `and`: `@attr and ...` or `/** doc */ and ...`
        if p.token == Token::And {
            // Capture start position BEFORE consuming `and` for next binding
            binding_start = p.start_pos.clone();
            p.next();
        } else if matches!(p.token, Token::At | Token::AtAt | Token::DocComment { .. }) {
            // Look ahead to see if attributes/doc comments are followed by And
            let has_and_after = p.lookahead(|state| {
                // Skip attributes and doc comments
                while matches!(state.token, Token::At | Token::AtAt | Token::DocComment { .. }) {
                    if matches!(state.token, Token::DocComment { .. }) {
                        state.next();
                    } else {
                        state.next(); // @
                        // Skip the attribute identifier and any payload
                        while !matches!(state.token, Token::At | Token::AtAt | Token::DocComment { .. } | Token::And | Token::Eof | Token::Let { .. } | Token::Typ | Token::Module | Token::External | Token::Open | Token::Include) {
                            state.next();
                        }
                    }
                }
                state.token == Token::And
            });
            if !has_and_after {
                break;
            }
            // Capture start position BEFORE the attributes for next binding
            binding_start = p.start_pos.clone();
            // Continue - attributes will be parsed in the next iteration, and `and` will be consumed there
        } else {
            break;
        }
    }

    bindings
}

/// Parse type declarations. Returns the declarations and a flag indicating if inline types were found.
/// `type_keyword_start` is the position of the `type` keyword for accurate location tracking.
fn parse_type_declarations(
    p: &mut Parser<'_>,
    outer_attrs: Attributes,
    type_keyword_start: Position,
) -> (Vec<TypeDeclaration>, bool) {
    let mut decls = vec![];
    let mut next_attrs = outer_attrs;
    let mut has_inline_types = false;

    // Create shared context for inline record desugaring
    // Note: For simplicity, we create a new context for each type declaration
    // The OCaml parser shares the context across `and` chains, but the inline types
    // are processed per-declaration anyway.

    // Track start position for type declarations
    // First declaration uses type_keyword_start, subsequent `and` declarations use the `and` position
    let mut decl_start = type_keyword_start;

    loop {
        // Create context for this declaration
        let inline_ctx = RefCell::new(InlineTypesContext::new(vec![]));
        if let Some(decl) = parse_type_declaration_with_context(p, next_attrs, &inline_ctx, decl_start.clone()) {
            // Collect inline types from the context
            let ctx = inline_ctx.borrow();
            if !ctx.found_inline_types.is_empty() {
                has_inline_types = true;
                // Create type declarations for inline types with @res.inlineRecordDefinition attribute
                let inline_attr: Attribute = (
                    mknoloc("res.inlineRecordDefinition".to_string()),
                    Payload::PStr(vec![]),
                );
                for (inline_name, loc, type_kind) in ctx.found_inline_types.iter() {
                    let inline_decl = TypeDeclaration {
                        ptype_name: with_loc(inline_name.clone(), loc.clone()),
                        ptype_params: ctx.params.clone(),
                        ptype_cstrs: vec![],
                        ptype_kind: type_kind.clone(),
                        ptype_private: PrivateFlag::Public,
                        ptype_manifest: None,
                        ptype_attributes: vec![inline_attr.clone()],
                        ptype_loc: loc.clone(),
                    };
                    decls.push(inline_decl);
                }
            }
            decls.push(decl);
        }
        next_attrs = vec![];

        // Check for `and` to continue with more type declarations.
        // Also handle attributes/doc comments before `and`: `/** doc */ and type t = ...`
        if p.token == Token::And {
            decl_start = p.start_pos.clone(); // Capture 'and' position
            p.next();
        } else if matches!(p.token, Token::At | Token::AtAt | Token::DocComment { .. }) {
            // Look ahead to see if attributes/doc comments are followed by And
            let has_and_after = p.lookahead(|state| {
                // Skip attributes and doc comments
                while matches!(state.token, Token::At | Token::AtAt | Token::DocComment { .. }) {
                    if matches!(state.token, Token::DocComment { .. }) {
                        state.next();
                    } else {
                        state.next(); // @
                        // Skip the attribute identifier and any payload
                        while !matches!(state.token, Token::At | Token::AtAt | Token::DocComment { .. } | Token::And | Token::Eof | Token::Let { .. } | Token::Typ | Token::Module | Token::External | Token::Open | Token::Include) {
                            state.next();
                        }
                    }
                }
                state.token == Token::And
            });
            if !has_and_after {
                break;
            }
            // Parse the attributes/doc comments, then consume `and`
            next_attrs = parse_attributes(p);
            if p.token == Token::And {
                decl_start = p.start_pos.clone(); // Capture 'and' position
                p.next();
            }
        } else {
            break;
        }
    }

    (decls, has_inline_types)
}

/// Parse a single type declaration (wrapper for backward compatibility).
/// Note: This captures start_pos at the current position, which should be the type name.
/// For accurate ptype_loc, use parse_type_declaration_with_context with the correct start position.
fn parse_type_declaration(p: &mut Parser<'_>, outer_attrs: Attributes) -> Option<TypeDeclaration> {
    let inline_ctx = RefCell::new(InlineTypesContext::new(vec![]));
    let start_pos = p.start_pos.clone();
    parse_type_declaration_with_context(p, outer_attrs, &inline_ctx, start_pos)
}

/// Parse a type extension: `type t += ...`.
fn parse_type_extension(p: &mut Parser<'_>, attrs: Attributes) -> TypeExtension {
    let start_pos = p.start_pos.clone();

    // Parse the extended type path (e.g. t or M.t).
    let mut parts: Vec<String> = vec![];
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
            continue;
        }
        break;
    }

    if parts.is_empty() {
        p.err(DiagnosticCategory::Message(
            "Expected type name for type extension".to_string(),
        ));
        parts.push("Error".to_string());
    }

    let path = with_loc(super::core::build_longident(&parts), p.mk_loc(&start_pos, &p.prev_end_pos));

    // Optional type params: <'a, +'b, -'c>
    let params = if p.token == Token::LessThan {
        parse_type_params_angle(p)
    } else {
        vec![]
    };

    p.expect(Token::PlusEqual);

    let private = if p.token == Token::Private {
        p.next();
        PrivateFlag::Private
    } else {
        PrivateFlag::Public
    };

    // Constructors (with optional leading `|`).
    if p.token == Token::Bar {
        p.next();
    }

    let mut constructors = vec![];
    while p.token != Token::Eof {
        constructors.push(parse_extension_constructor(p, vec![], None));
        if p.token == Token::Bar {
            p.next();
            continue;
        }
        break;
    }

    TypeExtension {
        ptyext_path: path,
        ptyext_params: params,
        ptyext_constructors: constructors,
        ptyext_private: private,
        ptyext_attributes: attrs,
    }
}

/// Parse a single type declaration with inline record desugaring context.
/// `decl_start` is the position of the `type` or `and` keyword that starts this declaration.
fn parse_type_declaration_with_context(
    p: &mut Parser<'_>,
    outer_attrs: Attributes,
    inline_ctx: &RefCell<InlineTypesContext>,
    decl_start: Position,
) -> Option<TypeDeclaration> {
    let start_pos = decl_start; // Use the passed start position (type/and keyword)
    let inner_attrs = parse_attributes(p);
    // Prepend outer attributes to inner ones
    let attrs = [outer_attrs, inner_attrs].concat();

    // Parse type name first (ReScript uses t<'a> not 'a t syntax)
    let name = match &p.token {
        Token::Lident(n) => {
            let n = n.clone();
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
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
    } else if p.token == Token::Lparen {
        // Detect old OCaml-style type parameters: type t('a) = ...
        // Give a helpful error message about using angle brackets instead
        let paren_start = p.start_pos.clone();
        let params = parse_type_params_old_style(p, &name.txt);
        let paren_end = p.prev_end_pos.clone();

        // Emit helpful error about angle bracket syntax
        let suggested = format_type_with_angle_brackets(&name.txt, &params);
        p.err_multiple(
            paren_start,
            paren_end,
            DiagnosticCategory::Message(format!(
                "Type parameters require angle brackets:\n  {}",
                suggested
            )),
        );
        params
    } else {
        vec![]
    };

    // Store params in context for inline type declarations
    inline_ctx.borrow_mut().params = params.clone();

    // The type name path for inline record desugaring
    let current_type_name_path = vec![name.txt.clone()];

    // Parse optional manifest and/or kind (and ReScript's `type t = T = ...` equation form).
    let (manifest, kind, private) = if p.token == Token::Equal {
        // Heuristic: `{...}` after `=` can be a record type *or* a bs-object/object type.
        // Disambiguate by looking at the first significant token inside the braces.
        let looks_like_object_type_after_lbrace = |p: &mut Parser<'_>| -> bool {
            p.lookahead(|state| {
                // Current token is `{`
                state.next();

                // Skip leading attributes: @attr ...
                while state.token == Token::At {
                    state.next();
                    // Skip attribute id segments (possibly dotted)
                    if matches!(state.token, Token::Lident(_) | Token::Uident(_)) || state.token.is_keyword()
                    {
                        state.next();
                        while state.token == Token::Dot {
                            state.next();
                            if matches!(state.token, Token::Lident(_) | Token::Uident(_))
                                || state.token.is_keyword()
                            {
                                state.next();
                            } else {
                                break;
                            }
                        }
                    }
                    // Skip optional payload in parentheses
                    if state.token == Token::Lparen {
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
                    }
                }

                // It's an object type if we see a string field name (like {"x": int})
                // Spread (...) alone doesn't make it an object - records can have spread too
                if matches!(state.token, Token::String(_)) {
                    return true;
                }

                // If we see spread, look for a string field anywhere after it
                // If all fields are spreads or idents, it's a record
                if state.token == Token::DotDotDot {
                    // Scan through the rest to see if there's any string field
                    let mut depth = 0;
                    loop {
                        match state.token {
                            Token::String(_) if depth == 0 => return true, // Found a string field - it's an object
                            Token::Rbrace if depth == 0 => return false, // End of braces, no string found - it's a record
                            Token::Eof => return false,
                            Token::Lparen | Token::Lbrace | Token::Lbracket => depth += 1,
                            Token::Rparen | Token::Rbracket => {
                                if depth > 0 {
                                    depth -= 1;
                                }
                            }
                            Token::Rbrace => {
                                if depth > 0 {
                                    depth -= 1;
                                } else {
                                    return false;
                                }
                            }
                            _ => {}
                        }
                        state.next();
                    }
                }

                // Dot or DotDot at start also indicates object type
                matches!(state.token, Token::Dot | Token::DotDot)
            })
        };

        // Better variant-vs-manifest disambiguation: a leading Uident without a following `.`
        // is almost always a constructor in a variant type, even if there's only a single
        // constructor (no `|`).
        let is_variant_type_starting_with_uident = |p: &mut Parser<'_>| -> bool {
            p.lookahead(|state| {
                state.next(); // consume Uident

                // `Foo.bar` is a type path, not a constructor.
                if state.token == Token::Dot {
                    return false;
                }

                // Any of these mean we're in constructor territory.
                // Includes tokens that end the type declaration (like keywords starting new decls).
                matches!(
                    state.token,
                    Token::Bar
                        | Token::Lparen
                        | Token::Lbrace
                        | Token::Colon // GADT syntax: `type t = Foo: t`
                        | Token::Equal // `type t = C = ...`
                        | Token::And
                        | Token::Constraint
                        | Token::Semicolon
                        | Token::Rbrace
                        | Token::Eof
                        // Keywords that start new top-level declarations
                        | Token::Let { .. }
                        | Token::Typ
                        | Token::Module
                        | Token::Open
                        | Token::Include
                        | Token::External
                        | Token::Exception
                )
            })
        };

        p.next(); // consume first '='
        let mut private = if p.token == Token::Private {
            p.next();
            PrivateFlag::Private
        } else {
            PrivateFlag::Public
        };

        let mut manifest = None;
        let mut kind = TypeKind::Ptype_abstract;

        match &p.token {
            Token::Bar => {
                // Variant type starting with | (e.g., type t = | A | B)
                kind = parse_type_kind(p, None, None);
            }
            Token::Lbrace => {
                // Record type OR object type ({"x": int})
                if looks_like_object_type_after_lbrace(p) {
                    manifest = Some(typ::parse_typ_expr(p));
                } else {
                    kind = parse_type_kind(p, Some(inline_ctx), Some(&current_type_name_path));
                }
            }
            Token::Uident(_) => {
                if is_variant_type_starting_with_uident(p) {
                    let constructors = parse_constructors_without_leading_bar(p);
                    kind = TypeKind::Ptype_variant(constructors);
                } else {
                    manifest = Some(typ::parse_typ_expr(p));
                }
            }
            Token::At | Token::DocComment { .. } => {
                // Check if this is a variant constructor with attribute: @attr Constr
                // vs a manifest type with attribute: @attr Type.t
                // vs an open type with attribute: @attr ..
                // vs a record type with attribute: @attr {x: int}
                // Lookahead to see if there's a Uident after the attribute that's NOT followed by `.`
                let what_follows = p.lookahead(|state| {
                    // Skip attributes and doc comments
                    while matches!(state.token, Token::At | Token::DocComment { .. }) {
                        if matches!(state.token, Token::DocComment { .. }) {
                            state.next();
                        } else {
                            state.next(); // @
                            // Skip attribute name (may be dotted like res.doc)
                            while matches!(state.token, Token::Lident(_) | Token::Uident(_)) || state.token.is_keyword() {
                                state.next();
                                if state.token == Token::Dot {
                                    state.next();
                                } else {
                                    break;
                                }
                            }
                            if state.token == Token::Lparen {
                                let mut depth = 1;
                                state.next();
                                while depth > 0 && state.token != Token::Eof {
                                    if state.token == Token::Lparen {
                                        depth += 1;
                                    } else if state.token == Token::Rparen {
                                        depth -= 1;
                                    }
                                    state.next();
                                }
                            }
                        }
                    }
                    // Check what comes after attributes
                    if state.token == Token::DotDot {
                        "open"
                    } else if state.token == Token::Lbrace {
                        "record_or_object"
                    } else if matches!(state.token, Token::Uident(_)) {
                        state.next();
                        if state.token != Token::Dot {
                            "variant"
                        } else {
                            "manifest"
                        }
                    } else {
                        "manifest"
                    }
                });
                match what_follows {
                    "variant" => {
                        let constructors = parse_constructors_without_leading_bar(p);
                        kind = TypeKind::Ptype_variant(constructors);
                    }
                    "open" => {
                        // Parse attributes and then ..
                        let _attrs = parse_attributes(p);
                        if p.token == Token::DotDot {
                            p.next();
                            kind = TypeKind::Ptype_open;
                        }
                    }
                    "record_or_object" => {
                        // Parse attributes then check if record or object
                        let attrs = parse_attributes(p);
                        if p.token == Token::Lbrace {
                            if looks_like_object_type_after_lbrace(p) {
                                let mut typ = typ::parse_typ_expr(p);
                                typ.ptyp_attributes = attrs;
                                manifest = Some(typ);
                            } else {
                                kind = parse_type_kind(p, Some(inline_ctx), Some(&current_type_name_path));
                            }
                        }
                    }
                    _ => {
                        manifest = Some(typ::parse_typ_expr(p));
                    }
                }
            }
            Token::DotDot => {
                // Extensible variant: type t = ..
                p.next();
                kind = TypeKind::Ptype_open;
            }
            _ => {
                // Manifest type
                manifest = Some(typ::parse_typ_expr(p));
            }
        }

        // ReScript type equation: `type t = T = ...`
        if manifest.is_some() && p.token == Token::Equal {
            p.next(); // consume second '='
            private = if p.token == Token::Private {
                p.next();
                PrivateFlag::Private
            } else {
                PrivateFlag::Public
            };

            kind = match &p.token {
                Token::Bar => parse_type_kind(p, None, None),
                Token::Lbrace => parse_type_kind(p, Some(inline_ctx), Some(&current_type_name_path)),
                Token::Uident(_) => TypeKind::Ptype_variant(parse_constructors_without_leading_bar(p)),
                Token::DotDot => {
                    p.next();
                    TypeKind::Ptype_open
                }
                _ => {
                    p.err(DiagnosticCategory::Message(
                        "Expected variant or record type after '='".to_string(),
                    ));
                    TypeKind::Ptype_abstract
                }
            };
        }

        (manifest, kind, private)
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
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
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
        cstrs.push((var, typ, p.mk_loc(&start_pos, &p.prev_end_pos)));
    }

    // In type declarations with qualified type constructors (Ldot),
    // OCaml uses the same location for both the CoreType and the longident
    // (just the constructor name, not including type arguments).
    // For unqualified (Lident), the CoreType uses full extent.
    let manifest = manifest.map(fix_type_manifest_location);

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
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

/// Fix the location of a Ptyp_constr in a type manifest context.
/// For qualified identifiers (Ldot), OCaml uses the same location for both
/// the CoreType and the longident. For unqualified (Lident), it uses full extent.
fn fix_type_manifest_location(typ: CoreType) -> CoreType {
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_constr(lid, _) => {
            // Only adjust for qualified identifiers (Ldot)
            if matches!(lid.txt, Longident::Ldot(..)) {
                CoreType {
                    ptyp_loc: lid.loc.clone(),
                    ..typ
                }
            } else {
                typ
            }
        }
        _ => typ,
    }
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
                match &p.token {
                    Token::Lident(name) | Token::Uident(name) => {
                        let name = name.clone();
                        let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                        p.next();
                        let typ = CoreType {
                            ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                            ptyp_loc: loc,
                            ptyp_attributes: vec![],
                        };
                        params.push((typ, variance));
                    }
                    _ => {}
                }
            }
            if !p.optional(&Token::Comma) {
                break;
            }
        }
        p.expect(Token::Rparen);
    } else if p.token == Token::SingleQuote {
        p.next();
        match &p.token {
            Token::Lident(name) | Token::Uident(name) => {
                let name = name.clone();
                let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                p.next();
                let typ = CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                    ptyp_loc: loc,
                    ptyp_attributes: vec![],
                };
                params.push((typ, Variance::Invariant));
            }
            _ => {}
        }
    }

    params
}

/// Parse type parameters in angle bracket syntax: <'a, +'b, -'c, _>
fn parse_type_params_angle(p: &mut Parser<'_>) -> Vec<(CoreType, Variance)> {
    let mut params = vec![];

    if p.token != Token::LessThan {
        return params;
    }
    // Enter diamond mode to prevent >= from being tokenized as a single token
    p.set_diamond_mode();
    p.next(); // consume <

    while p.token != Token::GreaterThan && p.token != Token::Eof {
        let param_start = p.start_pos.clone();

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

        // Parse type variable: 'name or underscore _
        if p.token == Token::SingleQuote {
            let quote_pos = p.start_pos.clone();
            p.next();
            match &p.token {
                Token::Lident(name) | Token::Uident(name) => {
                    let name = name.clone();
                    let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    let typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                        ptyp_loc: loc,
                        ptyp_attributes: vec![],
                    };
                    params.push((typ, variance));
                }
                t if t.is_keyword() => {
                    // Reserved keyword after singlequote: 'for, 'let, etc.
                    let keyword = format!("{}", t);
                    let keyword_start = p.start_pos.clone();
                    let keyword_end = p.end_pos.clone();
                    let loc = p.mk_loc(&keyword_start, &keyword_end);
                    p.err_multiple(
                        keyword_start,
                        keyword_end,
                        DiagnosticCategory::Message(format!(
                            "`{}` is a reserved keyword. Keywords need to be escaped: \\\"{}\"",
                            keyword, keyword
                        )),
                    );
                    p.next();
                    // Still add a param for recovery
                    let typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_var(keyword),
                        ptyp_loc: loc,
                        ptyp_attributes: vec![],
                    };
                    params.push((typ, variance));
                }
                Token::Underscore => {
                    // '_ is not valid - type param needs a name
                    // Report position at the underscore, not the quote
                    p.err_multiple(
                        p.start_pos.clone(),
                        p.start_pos.clone(),
                        DiagnosticCategory::Message(
                            "A type param consists of a singlequote followed by a name like `'a` or `'A`".to_string()
                        ),
                    );
                    p.next();
                    // Add placeholder for recovery - use empty string for type var
                    let typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_var(String::new()),
                        ptyp_loc: p.mk_loc(&param_start, &p.prev_end_pos),
                        ptyp_attributes: vec![],
                    };
                    params.push((typ, variance));
                }
                _ => {
                    // Something else after singlequote (like '+ )
                    // Report position at the invalid token, not the quote
                    p.err_multiple(
                        p.start_pos.clone(),
                        p.start_pos.clone(),
                        DiagnosticCategory::Message(
                            "A type param consists of a singlequote followed by a name like `'a` or `'A`".to_string()
                        ),
                    );
                    // Skip to next comma or closing >
                    while p.token != Token::Comma
                        && p.token != Token::GreaterThan
                        && p.token != Token::Eof
                    {
                        p.next();
                    }
                    // Add placeholder for recovery - use empty string for type var
                    let typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_var(String::new()),
                        ptyp_loc: p.mk_loc(&param_start, &p.prev_end_pos),
                        ptyp_attributes: vec![],
                    };
                    params.push((typ, variance));
                }
            }
        } else if p.token == Token::Underscore {
            // Anonymous/wildcard type parameter: _
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            let typ = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_any,
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            };
            params.push((typ, variance));
        } else if let Token::Lident(name) = &p.token {
            // Missing singlequote: foo instead of 'foo
            let name = name.clone();
            p.err_multiple(
                p.start_pos.clone(),
                p.end_pos.clone(),
                DiagnosticCategory::Message(format!(
                    "Type params start with a singlequote: '{}",
                    name
                )),
            );
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            // Add for recovery
            let typ = CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                ptyp_loc: loc,
                ptyp_attributes: vec![],
            };
            params.push((typ, variance));
        }

        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.expect(Token::GreaterThan);
    p.pop_diamond_mode();
    params
}

/// Parse old OCaml-style type parameters: ('a, 'b)
/// This is for error recovery - ReScript requires angle brackets.
fn parse_type_params_old_style(p: &mut Parser<'_>, _type_name: &str) -> Vec<(CoreType, Variance)> {
    let mut params = vec![];

    if p.token != Token::Lparen {
        return params;
    }
    p.next(); // consume (

    while p.token != Token::Rparen && p.token != Token::Eof {
        if p.token == Token::SingleQuote {
            p.next();
            match &p.token {
                Token::Lident(name) | Token::Uident(name) => {
                    let name = name.clone();
                    let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    let typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                        ptyp_loc: loc,
                        ptyp_attributes: vec![],
                    };
                    params.push((typ, Variance::Invariant));
                }
                t if t.is_keyword() => {
                    // Reserved keyword as type param (e.g., 'for)
                    let name = format!("{}", t);
                    let loc = p.mk_loc(&p.start_pos, &p.end_pos);
                    p.next();
                    let typ = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                        ptyp_loc: loc,
                        ptyp_attributes: vec![],
                    };
                    params.push((typ, Variance::Invariant));
                }
                _ => {
                    // Skip unknown token
                    p.next();
                }
            }
        } else {
            // Skip non-quote token
            p.next();
        }
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    if p.token == Token::Rparen {
        p.next();
    }
    params
}

/// Format a type with angle bracket syntax for error suggestions.
fn format_type_with_angle_brackets(name: &str, params: &[(CoreType, Variance)]) -> String {
    if params.is_empty() {
        name.to_string()
    } else {
        let param_strs: Vec<String> = params
            .iter()
            .map(|(t, _)| match &t.ptyp_desc {
                CoreTypeDesc::Ptyp_var(v) => format!("'{}", v),
                _ => "_".to_string(),
            })
            .collect();
        format!("{}<{}>", name, param_strs.join(", "))
    }
}

/// Parse type kind (variant or record).
fn parse_type_kind(
    p: &mut Parser<'_>,
    inline_ctx: Option<&RefCell<InlineTypesContext>>,
    current_type_name_path: Option<&Vec<String>>,
) -> TypeKind {
    match &p.token {
        Token::Bar => {
            // Variant type
            let constructors = parse_constructors(p);
            TypeKind::Ptype_variant(constructors)
        }
        Token::Lbrace => {
            // Record type
            p.next();
            let labels = parse_label_declarations(p, inline_ctx, current_type_name_path);
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
        // Capture start position BEFORE consuming the bar (like OCaml)
        let bar_start = p.start_pos.clone();
        p.next();
        if let Some(c) = parse_constructor_with_start(p, bar_start) {
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
        // Capture start position BEFORE consuming the bar (like OCaml)
        let bar_start = p.start_pos.clone();
        p.next();
        if let Some(c) = parse_constructor_with_start(p, bar_start) {
            constructors.push(c);
        }
    }

    constructors
}

/// Parse a single constructor with optional explicit start position.
fn parse_constructor_with_start(
    p: &mut Parser<'_>,
    start_pos: Position,
) -> Option<ConstructorDeclaration> {
    parse_constructor_impl(p, start_pos)
}

/// Parse a single constructor.
fn parse_constructor(p: &mut Parser<'_>) -> Option<ConstructorDeclaration> {
    let start_pos = p.start_pos.clone();
    parse_constructor_impl(p, start_pos)
}

/// Implementation of constructor parsing with a given start position.
fn parse_constructor_impl(p: &mut Parser<'_>, start_pos: Position) -> Option<ConstructorDeclaration> {
    let attrs = parse_attributes(p);

    // Check for spread syntax: ...typeName
    if p.token == Token::DotDotDot {
        let spread_start = p.start_pos.clone();
        p.next();
        // Name location is just the "..." part
        let name_loc = p.mk_loc(&spread_start, &p.prev_end_pos);
        let spread_type = typ::parse_typ_expr(p);
        let loc = p.mk_loc(&spread_start, &p.prev_end_pos);
        return Some(ConstructorDeclaration {
            pcd_name: with_loc("...".to_string(), name_loc),
            pcd_args: ConstructorArguments::Pcstr_tuple(vec![spread_type]),
            pcd_res: None,
            pcd_loc: loc,
            pcd_attributes: attrs,
        });
    }

    let name = match &p.token {
        Token::Uident(n) => {
            let n = n.clone();
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
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
        // Check for inline record: Foo({...}) with braces inside parens
        // But NOT object types which use string keys or . markers
        let is_inline_record = p.token == Token::Lbrace
            && p.lookahead(|state| {
                state.next(); // consume {
                // Skip any leading attributes
                while state.token == Token::At {
                    state.next();
                    // Skip attribute id and any payload
                    if matches!(
                        state.token,
                        Token::Lident(_) | Token::Uident(_) | Token::Module
                    ) {
                        state.next();
                    }
                    // Skip attribute payload if present
                    if state.token == Token::Lparen {
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
                    }
                }
                // If we see string, ".", "..", or "}" it's an object type, not inline record
                !matches!(
                    state.token,
                    Token::String(_)
                        | Token::Dot
                        | Token::DotDot
                        | Token::DotDotDot
                        | Token::Rbrace
                )
            });
        if is_inline_record {
            p.next(); // consume {
            let labels = parse_label_declarations(p, None, None);
            p.expect(Token::Rbrace);
            // Allow trailing comma after inline record: Foo({...},)
            p.optional(&Token::Comma);
            p.expect(Token::Rparen);
            ConstructorArguments::Pcstr_record(labels)
        } else {
            let mut args = vec![];
            while p.token != Token::Rparen && p.token != Token::Eof {
                args.push(typ::parse_typ_expr(p));
                if !p.optional(&Token::Comma) {
                    break;
                }
            }
            p.expect(Token::Rparen);
            ConstructorArguments::Pcstr_tuple(args)
        }
    } else if p.token == Token::Lbrace {
        p.next();
        let labels = parse_label_declarations(p, None, None);
        p.expect(Token::Rbrace);
        ConstructorArguments::Pcstr_record(labels)
    } else {
        ConstructorArguments::Pcstr_tuple(vec![])
    };

    // Optional GADT result type: Constructor: type
    let res = if p.token == Token::Colon {
        p.next();
        Some(typ::parse_typ_expr(p))
    } else {
        None
    };

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
    Some(ConstructorDeclaration {
        pcd_name: name,
        pcd_args: args,
        pcd_res: res,
        pcd_loc: loc,
        pcd_attributes: attrs,
    })
}

/// Parse record label declarations.
/// Handles both regular fields and spread syntax (...typ).
fn parse_label_declarations(
    p: &mut Parser<'_>,
    inline_ctx: Option<&RefCell<InlineTypesContext>>,
    current_type_name_path: Option<&Vec<String>>,
) -> Vec<LabelDeclaration> {
    let mut labels = vec![];

    while p.token != Token::Rbrace && p.token != Token::Eof {
        // Handle spread: ...typ - represented as a label with name "..."
        if p.token == Token::DotDotDot {
            let start_pos = p.start_pos.clone();
            p.next();
            // Name location is just the "..." part
            let name_loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            let spread_type = typ::parse_typ_expr(p);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
            labels.push(LabelDeclaration {
                pld_name: with_loc("...".to_string(), name_loc),
                pld_mutable: MutableFlag::Immutable,
                pld_type: spread_type,
                pld_loc: loc,
                pld_attributes: vec![],
                pld_optional: false,
            });
            if !p.optional(&Token::Comma) {
                break;
            }
            continue;
        }

        if let Some(l) = parse_label_declaration(p, inline_ctx, current_type_name_path) {
            labels.push(l);
        }
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    labels
}

/// Parse a single label declaration.
fn parse_label_declaration(
    p: &mut Parser<'_>,
    inline_ctx: Option<&RefCell<InlineTypesContext>>,
    current_type_name_path: Option<&Vec<String>>,
) -> Option<LabelDeclaration> {
    let start_pos = p.start_pos.clone();
    let attrs = parse_attributes(p);

    let mutable = if p.token == Token::Mutable {
        p.next();
        MutableFlag::Mutable
    } else {
        MutableFlag::Immutable
    };

    let name = match &p.token {
        Token::Lident(n) => {
            let n = n.clone();
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
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

    // Extend current type name path with this field name for nested inline records
    let extended_path = current_type_name_path.map(|path| {
        let mut new_path = path.clone();
        new_path.push(name.txt.clone());
        new_path
    });

    // Check for optional field marker
    let is_optional = p.optional(&Token::Question);

    // Check for field punning: {form} is shorthand for {form: form}
    // If no colon, the type name is the same as the field name
    let is_punning = p.token != Token::Colon;

    if !is_punning {
        p.expect(Token::Colon);
    }

    // Check if the field type is an inline record
    let typ = if is_punning {
        // Field punning: {form} becomes {form: form}
        // The type is a simple type constructor with the field name
        CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_constr(
                with_loc(Longident::Lident(name.txt.clone()), name.loc.clone()),
                vec![],
            ),
            ptyp_loc: name.loc.clone(),
            ptyp_attributes: vec![],
        }
    } else if p.token == Token::Lbrace && inline_ctx.is_some() && extended_path.is_some() {
        // Check if this looks like a record (starts with Lident, Mutable, At, or DotDotDot)
        let looks_like_record = p.lookahead(|state| {
            state.next(); // Skip {
            // Skip leading attributes
            while state.token == Token::At {
                state.next();
                // Skip attribute identifier
                if matches!(state.token, Token::Lident(_) | Token::Uident(_)) || state.token.is_keyword() {
                    state.next();
                    while state.token == Token::Dot {
                        state.next();
                        if matches!(state.token, Token::Lident(_) | Token::Uident(_)) || state.token.is_keyword() {
                            state.next();
                        } else {
                            break;
                        }
                    }
                }
                // Skip optional payload
                if state.token == Token::Lparen {
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
                }
            }
            // Check if first significant token indicates a record declaration
            matches!(
                state.token,
                Token::Lident(_) | Token::Mutable | Token::At | Token::DotDotDot
            )
        });

        if looks_like_record {
            // Parse as inline record and register it
            let type_start_pos = p.start_pos.clone();
            p.next(); // consume {
            let inline_ctx = inline_ctx.unwrap();
            let extended_path = extended_path.as_ref().unwrap();
            let labels = parse_label_declarations(p, Some(inline_ctx), Some(extended_path));
            p.expect(Token::Rbrace);
            let type_loc = p.mk_loc(&type_start_pos, &p.prev_end_pos);

            // Create the inline type name (e.g., "options.permissions.all")
            let inline_type_name = extended_path.join(".");

            // Register this inline type (insert at front for correct order)
            inline_ctx.borrow_mut().found_inline_types.insert(0, (
                inline_type_name.clone(),
                type_loc.clone(),
                TypeKind::Ptype_record(labels),
            ));

            // Return a Ptyp_constr pointing to the inline type
            let params = inline_ctx.borrow().params.clone();
            let type_args: Vec<CoreType> = params.into_iter().map(|(t, _)| t).collect();
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_constr(
                    with_loc(Longident::Lident(inline_type_name), type_loc.clone()),
                    type_args,
                ),
                ptyp_loc: type_loc,
                ptyp_attributes: vec![],
            }
        } else {
            // Not a record - parse as regular type (object type)
            typ::parse_typ_expr(p)
        }
    } else {
        typ::parse_typ_expr(p)
    };

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
    Some(LabelDeclaration {
        pld_name: name,
        pld_mutable: mutable,
        pld_type: typ,
        pld_loc: loc,
        pld_attributes: attrs,
        pld_optional: is_optional,
    })
}

/// Parse a value description (for external).
/// `loc_start` should be the position before any outer attributes (to match OCaml's location).
fn parse_value_description(
    p: &mut Parser<'_>,
    outer_attrs: Attributes,
    loc_start: crate::location::Position,
) -> ValueDescription {
    let inner_attrs = parse_attributes(p);
    // Merge outer (structure-level) attrs with inner (name-level) attrs
    let attrs = [outer_attrs, inner_attrs].concat();

    let name = match &p.token {
        Token::Lident(n) => {
            let n = n.clone();
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
            p.next();
            with_loc(n, loc)
        }
        Token::String(s) => {
            // Escaped identifier like "export" - store with quotes for printer to recognize
            let n = format!("\"{}\"", s);
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
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

    let loc = p.mk_loc(&loc_start, &p.prev_end_pos);
    ValueDescription {
        pval_name: name,
        pval_type: typ,
        pval_prim: prim,
        pval_attributes: attrs,
        pval_loc: loc,
    }
}

/// Parse an extension constructor (for exception).
/// `exception_start` should be the position BEFORE 'exception' was consumed (if applicable).
pub fn parse_extension_constructor(
    p: &mut Parser<'_>,
    attrs: Attributes,
    exception_start: Option<Position>,
) -> ExtensionConstructor {
    // Use provided start position (before 'exception') or current position
    let start_pos = exception_start.unwrap_or_else(|| p.start_pos.clone());
    let constructor = parse_constructor(p).unwrap_or_else(|| ConstructorDeclaration {
        pcd_name: mknoloc("Error".to_string()),
        pcd_args: ConstructorArguments::Pcstr_tuple(vec![]),
        pcd_res: None,
        pcd_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
        pcd_attributes: vec![],
    });

    let mut kind = ExtensionConstructorKind::Pext_decl(constructor.pcd_args, constructor.pcd_res);

    // Rebind: Constructor = Other
    if p.token == Token::Equal {
        p.next();
        let lid = parse_module_long_ident(p);
        kind = ExtensionConstructorKind::Pext_rebind(lid);
    }

    // Combine outer attributes with constructor attributes
    let mut all_attrs = attrs;
    all_attrs.extend(constructor.pcd_attributes);

    ExtensionConstructor {
        pext_name: constructor.pcd_name,
        pext_kind: kind,
        pext_loc: p.mk_loc(&start_pos, &p.prev_end_pos),
        pext_attributes: all_attrs,
    }
}

/// Parse a module definition.
/// `module_start` should be the position BEFORE 'module' was consumed.
fn parse_module_definition(p: &mut Parser<'_>, module_start: Position, attrs: Attributes) -> Option<StructureItemDesc> {
    // Check for module type
    if p.token == Token::Typ {
        p.next();
        let mtd = parse_module_type_declaration(p, attrs);
        return Some(StructureItemDesc::Pstr_modtype(mtd));
    }

    // Check for rec
    let rec_flag = if p.token == Token::Rec {
        p.next();
        RecFlag::Recursive
    } else {
        RecFlag::Nonrecursive
    };

    // For recursive modules (module rec X = ...), use position before 'module'.
    // For non-recursive modules (module X = ...), use current position (after 'module').
    // This matches OCaml's parse_maybe_rec_module_binding behavior.
    let start_pos = if rec_flag == RecFlag::Recursive {
        module_start
    } else {
        p.start_pos.clone()
    };

    let mut bindings = vec![];

    let parse_binding = |p: &mut Parser<'_>, binding_start: Position, attrs: Attributes| -> Option<ModuleBinding> {
        let name = match &p.token {
            Token::Uident(n) => {
                let n = n.clone();
                let loc = p.mk_loc(&p.start_pos, &p.end_pos);
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

        // Optional module type annotation (constraint)
        let mod_type = if p.token == Token::Colon {
            p.next();
            Some(parse_module_type(p))
        } else {
            None
        };

        p.expect(Token::Equal);
        let mut mod_expr = parse_module_expr(p);

        if let Some(mod_type) = mod_type {
            // Location spans from module type start to module expr end
            let loc = p.mk_loc(&mod_type.pmty_loc.loc_start, &mod_expr.pmod_loc.loc_end);
            mod_expr = ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_constraint(
                    Box::new(mod_expr),
                    Box::new(mod_type),
                ),
                pmod_loc: loc,
                pmod_attributes: vec![],
            };
        }

        Some(ModuleBinding {
            pmb_name: name,
            pmb_expr: mod_expr,
            pmb_attributes: attrs,
            pmb_loc: p.mk_loc(&binding_start, &p.prev_end_pos),
        })
    };

    if let Some(binding) = parse_binding(p, start_pos.clone(), attrs) {
        bindings.push(binding);
    } else {
        return None;
    }

    // Parse additional module bindings with `and`
    // Also handle attributes/doc comments before `and`: `@attr and M = ...` or `/** doc */ and M = ...`
    loop {
        if p.token == Token::And {
            // Capture start position BEFORE consuming `and`
            let binding_start = p.start_pos.clone();
            p.next();
            let binding_attrs = parse_attributes(p);
            if let Some(binding) = parse_binding(p, binding_start, binding_attrs) {
                bindings.push(binding);
            }
        } else if matches!(p.token, Token::At | Token::AtAt | Token::DocComment { .. }) {
            // Look ahead to see if attributes/doc comments are followed by And
            let has_and_after = p.lookahead(|state| {
                // Skip attributes and doc comments
                while matches!(state.token, Token::At | Token::AtAt | Token::DocComment { .. }) {
                    if matches!(state.token, Token::DocComment { .. }) {
                        state.next();
                    } else {
                        state.next(); // @
                        // Skip the attribute identifier and any payload
                        while !matches!(state.token, Token::At | Token::AtAt | Token::DocComment { .. } | Token::And | Token::Eof | Token::Let { .. } | Token::Typ | Token::Module | Token::External | Token::Open | Token::Include) {
                            state.next();
                        }
                    }
                }
                state.token == Token::And
            });
            if !has_and_after {
                break;
            }
            // Parse the attributes, then the `and` keyword
            let binding_start = p.start_pos.clone();
            let binding_attrs = parse_attributes(p);
            if p.token == Token::And {
                p.next();
            }
            if let Some(binding) = parse_binding(p, binding_start, binding_attrs) {
                bindings.push(binding);
            }
        } else {
            break;
        }
    }

    if rec_flag == RecFlag::Recursive {
        Some(StructureItemDesc::Pstr_recmodule(bindings))
    } else if bindings.len() == 1 {
        Some(StructureItemDesc::Pstr_module(bindings.remove(0)))
    } else {
        Some(StructureItemDesc::Pstr_recmodule(bindings))
    }
}

/// Parse a module declaration (for signature).
fn parse_module_declaration(p: &mut Parser<'_>, outer_attrs: Attributes) -> ModuleDeclaration {
    let start_pos = p.start_pos.clone();
    let inner_attrs = parse_attributes(p);
    // Merge outer (signature item level) attrs with inner (name level) attrs
    let attrs = [outer_attrs, inner_attrs].concat();

    let name = match &p.token {
        Token::Uident(n) => {
            let n = n.clone();
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
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

    // Handle both `module X: T` (type annotation) and `module X = M` (alias)
    // In signatures, `=` can be followed by a module type or a module alias
    let mod_type = if p.token == Token::Equal {
        p.next();
        // Check if this is `= { ... }` (module type) or `= ModuleName` (alias)
        if p.token == Token::Lbrace {
            // Module type: module X = { ... }
            parse_module_type(p)
        } else {
            // Module alias: module X = M
            let lid = parse_module_long_ident(p);
            let loc = p.mk_loc(&lid.loc.loc_start, &p.prev_end_pos);
            ModuleType {
                pmty_desc: ModuleTypeDesc::Pmty_alias(lid),
                pmty_loc: loc,
                pmty_attributes: vec![],
            }
        }
    } else {
        p.expect(Token::Colon);
        parse_module_type(p)
    };

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
    ModuleDeclaration {
        pmd_name: name,
        pmd_type: mod_type,
        pmd_attributes: attrs,
        pmd_loc: loc,
    }
}

/// Parse attributes only if followed by `and` keyword.
/// This uses speculative parsing with backtracking like OCaml's parse_attributes_and_binding.
/// If attrs are present but not followed by `and`, parser state is restored and empty attrs returned.
fn parse_attributes_and_binding(p: &mut Parser<'_>) -> Attributes {
    match &p.token {
        Token::At | Token::DocComment { .. } => {
            // Save parser state for potential backtrack
            let snapshot = p.snapshot();

            // Parse attributes
            let attrs = parse_attributes(p);

            // Check if `and` follows
            if p.token == Token::And {
                // Keep the attrs - they're for the `and` clause
                attrs
            } else {
                // Restore parser state - attrs belong to next item
                p.restore(snapshot);
                vec![]
            }
        }
        _ => vec![],
    }
}

/// Parse recursive module declarations (for signature).
fn parse_rec_module_declarations(p: &mut Parser<'_>, outer_attrs: Attributes) -> Vec<ModuleDeclaration> {
    let mut decls = vec![];

    // Parse first declaration (gets the outer attrs)
    decls.push(parse_module_declaration(p, outer_attrs));

    // Parse additional declarations with `and`
    // Uses parse_attributes_and_binding which backtracks if attrs aren't followed by `and`
    loop {
        let attrs = parse_attributes_and_binding(p);
        if p.token == Token::And {
            p.next();
            decls.push(parse_module_declaration(p, attrs));
        } else {
            break;
        }
    }

    decls
}

/// Parse a module type declaration.
fn parse_module_type_declaration(p: &mut Parser<'_>, outer_attrs: Attributes) -> ModuleTypeDeclaration {
    let start_pos = p.start_pos.clone();
    let inner_attrs = parse_attributes(p);
    // Merge outer (signature item level) attrs with inner (name level) attrs
    let attrs = [outer_attrs, inner_attrs].concat();

    let name = match &p.token {
        Token::Uident(n) | Token::Lident(n) => {
            let n = n.clone();
            let loc = p.mk_loc(&p.start_pos, &p.end_pos);
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

    let loc = p.mk_loc(&start_pos, &p.prev_end_pos);
    ModuleTypeDeclaration {
        pmtd_name: name,
        pmtd_type: typ,
        pmtd_attributes: attrs,
        pmtd_loc: loc,
    }
}

/// Parse attributes (including doc comments which become res.doc attributes).
fn parse_attributes(p: &mut Parser<'_>) -> Attributes {
    let mut attrs = vec![];
    loop {
        match &p.token {
            Token::At => {
                // Capture start_pos before consuming @ so attribute location includes it
                let attr_start = p.start_pos.clone();
                p.next();
                attrs.push(parse_attribute_body(p, attr_start));
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

/// Parse an attribute body.
/// `start_pos` should be the position BEFORE the `@` or `@@` token was consumed.
fn parse_attribute_body(p: &mut Parser<'_>, start_pos: Position) -> Attribute {
    let mut parts = vec![];

    // Parse attribute identifier
    loop {
        match &p.token {
            Token::Lident(name) | Token::Uident(name) => {
                parts.push(name.clone());
                p.next();
            }
            // Attributes can use keyword-like identifiers (e.g. `@module`, `@as`).
            // Treat keywords as identifiers in this context.
            token if token.is_keyword() => {
                parts.push(token.to_string());
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
    let name_loc = p.mk_loc(&start_pos, &p.prev_end_pos);

    // Parse optional payload
    let is_adjacent = p.start_pos.cnum == p.prev_end_pos.cnum;
    let payload = if p.token == Token::Lparen && is_adjacent {
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
/// Payloads can be:
/// - Structure items: `(expr1 expr2 ...)` - the default
/// - Type: `(: type)` - indicated by leading colon followed by type
/// - Signature: `(: signature_items)` - indicated by leading colon followed by signature items
/// - Pattern: `(? pattern)` or `(? pattern when guard)` - indicated by leading question mark
pub fn parse_payload(p: &mut Parser<'_>) -> Payload {
    // Check for type or signature payload: (: ...)
    if p.token == Token::Colon {
        p.next();
        // Check if it's a signature payload (starts with let, type, etc.) or a type
        if matches!(
            p.token,
            Token::Let { .. }
                | Token::Typ
                | Token::Module
                | Token::External
                | Token::Open
                | Token::Include
                | Token::Exception
        ) {
            // Parse as signature
            let mut sig_items = vec![];
            while p.token != Token::Rparen && p.token != Token::Eof {
                if let Some(item) = parse_signature_item(p) {
                    sig_items.push(item);
                }
                p.optional(&Token::Semicolon);
            }
            return Payload::PSig(sig_items);
        } else {
            // Parse as type
            let typ = super::typ::parse_typ_expr(p);
            return Payload::PTyp(Box::new(typ));
        }
    }

    // Check for pattern payload: (? pattern) or (? pattern when guard)
    if p.token == Token::Question {
        p.next();
        let pat = super::pattern::parse_pattern(p);
        let guard = if p.token == Token::When {
            p.next();
            Some(Box::new(super::expr::parse_expr(p)))
        } else {
            None
        };
        return Payload::PPat(Box::new(pat), guard);
    }

    // Otherwise, parse as structure items
    let mut items = vec![];

    // If the payload is empty, return early
    if p.token == Token::Rparen || p.token == Token::Eof {
        return Payload::PStr(items);
    }

    // Parse structure items
    loop {
        let start_pos = p.start_pos.clone();

        // Check if this looks like a structure item (let, type, module, etc.)
        let is_structure_item = matches!(
            p.token,
            Token::Let { .. }
                | Token::Typ
                | Token::Module
                | Token::External
                | Token::Open
                | Token::Include
                | Token::Exception
                | Token::At
                | Token::AtAt
                | Token::Percent
                | Token::PercentPercent
                | Token::ModuleComment { .. }
        );

        if is_structure_item {
            // Parse as structure item
            if let Some(item) = parse_structure_item(p) {
                items.push(item);
            }
        } else {
            // Parse as expression
            let expr = super::expr::parse_expr(p);
            let loc = p.mk_loc(&start_pos, &p.prev_end_pos);

            items.push(StructureItem {
                pstr_desc: StructureItemDesc::Pstr_eval(expr, vec![]),
                pstr_loc: loc,
            });
        }

        // Check for more items (separated by semicolons or just by whitespace)
        p.optional(&Token::Semicolon);

        // Stop if we hit the closing paren or EOF
        if p.token == Token::Rparen || p.token == Token::Eof {
            break;
        }
    }

    Payload::PStr(items)
}

/// Parse an extension.
fn parse_extension(p: &mut Parser<'_>) -> Extension {
    // Capture start position BEFORE consuming % or %% so extension location includes it
    let id_start = p.start_pos.clone();

    // Handle both %ext and %%ext (single and double percent)
    if p.token == Token::PercentPercent {
        p.next();
    } else {
        p.expect(Token::Percent);
    }
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
        with_loc(id, p.mk_loc(&id_start, &p.prev_end_pos))
    };

    // Parse optional payload only if it's immediately adjacent.
    let is_adjacent = p.start_pos.cnum == p.prev_end_pos.cnum;
    let payload = if p.token == Token::Lparen && is_adjacent {
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
