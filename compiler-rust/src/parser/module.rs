//! Module parsing for ReScript.
//!
//! This module contains the module parsing logic, converting tokens
//! into module AST nodes (structures, signatures, module expressions, etc.).

use std::cell::RefCell;

use crate::location::Position;
use crate::parse_arena::{LidentIdx, Located, LocIdx, PosIdx};

// Import Location as alias to LocIdx from ast
use super::ast::*;
use super::ast::Location;
use super::core::{error_messages, is_es6_arrow_functor, mknoloc, recover, with_loc};
use super::diagnostics::DiagnosticCategory;
use super::expr;
use super::grammar;
use super::grammar::Grammar;
use super::longident::Longident;
use super::pattern;
use super::state::Parser;
use super::token::Token;
use super::typ;

// ============================================================================
// Consecutive Statement/Expression Checking
// ============================================================================

/// Check and emit "consecutive statements" error if statements on same line without semicolon.
///
/// Matches OCaml's `parse_newline_or_semicolon_structure`:
/// - If token is Semicolon, consume it
/// - If token can start a structure item AND on same line as previous, emit error
/// - Otherwise, do nothing
fn parse_newline_or_semicolon_structure(p: &mut Parser<'_>) {
    if p.token == Token::Semicolon {
        p.next();
        return;
    }

    if grammar::is_structure_item_start(&p.token) {
        // Check if we're on the same line as the previous token
        // OCaml: if p.prev_end_pos.pos_lnum < p.start_pos.pos_lnum then () else err
        if p.prev_end_pos.line >= p.start_pos.line {
            // Same line: emit error
            // Use err_at which respects regions (like OCaml's Parser.err)
            p.err_at(
                p.prev_end_pos.clone(),
                p.end_pos.clone(),
                DiagnosticCategory::message(
                    "consecutive statements on a line must be separated by ';' or a newline",
                ),
            );
        }
    }
}

/// Check and emit "consecutive statements" error for signature items.
///
/// Matches OCaml's `parse_newline_or_semicolon_signature`:
/// - If token is Semicolon, consume it
/// - If token can start a signature item AND on same line as previous, emit error
/// - Otherwise, do nothing
fn parse_newline_or_semicolon_signature(p: &mut Parser<'_>) {
    if p.token == Token::Semicolon {
        p.next();
        return;
    }

    if grammar::is_signature_item_start(&p.token) {
        // Check if we're on the same line as the previous token
        // OCaml: if p.prev_end_pos.pos_lnum < p.start_pos.pos_lnum then () else err
        if p.prev_end_pos.line >= p.start_pos.line {
            // Same line: emit error
            // Use err_at which respects regions (like OCaml's Parser.err)
            p.err_at(
                p.prev_end_pos.clone(),
                p.end_pos.clone(),
                DiagnosticCategory::message(
                    "consecutive statements on a line must be separated by ';' or a newline",
                ),
            );
        }
    }
}

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
    /// Location of the outer type's name (used for inline type declarations)
    pub outer_name_loc: Location,
}

impl InlineTypesContext {
    fn new(params: Vec<(CoreType, Variance)>, outer_name_loc: Location) -> Self {
        Self {
            found_inline_types: vec![],
            params,
            outer_name_loc,
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Convert a module type to a package type (for Ptyp_package).
/// For a simple module type identifier like `T`, creates `(T, [])`.
/// For `T with type t = int`, creates `(T, [(t, int)])`.
fn module_type_to_package(p: &mut Parser<'_>, mt: &ModuleType) -> PackageType {
    match &mt.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => (lid.clone(), vec![]),
        ModuleTypeDesc::Pmty_with(base, constraints) => {
            // For "T with type t = int", extract base identifier and constraints
            let lid = if let ModuleTypeDesc::Pmty_ident(lid) = &base.pmty_desc {
                lid.clone()
            } else {
                let lid_idx = p.push_lident_static("_");
                with_loc(lid_idx, mt.pmty_loc.clone())
            };

            // Convert WithConstraint to package type constraints
            let pkg_constraints: Vec<(Loc<LidentIdx>, CoreType)> = constraints
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
        _ => {
            let lid_idx = p.push_lident_static("_");
            (with_loc(lid_idx, mt.pmty_loc.clone()), vec![])
        }
    }
}

// ============================================================================
// Structure (Implementation) Parsing
// ============================================================================

/// Parse a structure (list of structure items).
/// Matches OCaml's parse_implementation which uses parse_region.
/// Note: OCaml does NOT create regions per structure item - the region is shared
/// so that after one error, subsequent errors in the same region are silenced.
pub fn parse_structure(p: &mut Parser<'_>) -> Structure {
    // OCaml uses parse_region with Grammar::Implementation
    p.leave_breadcrumb(Grammar::Implementation);

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

    p.eat_breadcrumb();
    items
}

/// Parse a single structure item.
pub fn parse_structure_item(p: &mut Parser<'_>) -> Option<StructureItem> {
    // Save start position as PosIdx to enable sharing with inner locations
    let start_pos_idx = p.push_start_pos();
    let start_pos = p.start_pos;

    // Parse attributes
    let attrs = parse_attributes(p);

    // Track the PosIdx used for this structure item's location
    // For Pstr_value, this will be the same as the first binding's start
    let mut item_start_idx = start_pos_idx;
    // Track shared LocIdx for when we want to share the entire location (e.g., with value_binding)
    let mut item_loc_idx: Option<LocIdx> = None;

    let desc = match &p.token {
        Token::Open => {
            // OCaml's popen_loc starts at 'open', not at outer attributes
            let open_start = p.start_pos;
            p.next();
            // Check for open! override flag
            let override_flag = if p.token == Token::Bang {
                p.next();
                OverrideFlag::Override
            } else {
                OverrideFlag::Fresh
            };
            let lid = parse_module_long_ident(p);
            parse_newline_or_semicolon_structure(p);
            let loc = p.mk_loc_to_prev_end(&open_start);
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
            // Position after let/let? for let.unwrap attribute location
            // OCaml uses p.Parser.start_pos which is the START of the NEXT token (after whitespace)
            let let_end_pos = p.start_pos;
            let rec_flag = if p.token == Token::Rec {
                p.next();
                RecFlag::Recursive
            } else {
                RecFlag::Nonrecursive
            };
            // Pass start_pos_idx to enable position sharing with structure_item
            let (bindings, _binding_loc_idx) = parse_let_bindings(p, start_pos_idx, let_end_pos, rec_flag, attrs.clone(), unwrap);
            parse_newline_or_semicolon_structure(p);
            // Don't share LocIdx - OCaml creates separate Location objects for pstr_loc and pvb_loc.
            // The internal positions (PosIdx) are shared, but the Location records themselves are separate.
            Some(StructureItemDesc::Pstr_value(rec_flag, bindings))
        }
        Token::Typ => {
            // OCaml uses begin_region/end_region around type parsing
            // This allows errors within one type definition to be de-duplicated,
            // but errors from different type definitions can both be reported.
            p.begin_region();

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

            let result = if is_type_extension {
                let ext = parse_type_extension(p, attrs.clone());
                parse_newline_or_semicolon_structure(p);
                Some(StructureItemDesc::Pstr_typext(ext))
            } else {
                let mut rec_flag = if p.token == Token::Rec {
                    p.next();
                    RecFlag::Recursive
                } else if matches!(&p.token, Token::Lident(s) if s == "nonrec") {
                    // Handle `type nonrec t = ...` - nonrec is a keyword in this context
                    p.next();
                    RecFlag::Nonrecursive
                } else {
                    RecFlag::Nonrecursive
                };
                let (decls, has_inline_types) = parse_type_declarations(p, attrs.clone(), type_start);
                // Inline record types require the type to be recursive
                if has_inline_types {
                    rec_flag = RecFlag::Recursive;
                }
                parse_newline_or_semicolon_structure(p);
                Some(StructureItemDesc::Pstr_type(rec_flag, decls))
            };

            p.end_region();
            result
        }
        Token::External => {
            p.next();
            let vd = parse_value_description(p, attrs.clone(), start_pos.clone());
            parse_newline_or_semicolon_structure(p);
            Some(StructureItemDesc::Pstr_primitive(vd))
        }
        Token::Exception => {
            // Capture start position BEFORE consuming 'exception' for accurate location
            let exception_start = p.start_pos.clone();
            p.next();
            let ext = parse_extension_constructor(p, attrs.clone(), Some(exception_start));
            parse_newline_or_semicolon_structure(p);
            Some(StructureItemDesc::Pstr_exception(ext))
        }
        Token::Module => {
            // OCaml uses begin_region/end_region around module parsing
            p.begin_region();

            // Capture module_start BEFORE consuming 'module' for accurate locations
            let module_start = p.start_pos.clone();
            p.next();
            // Check for first-class module expression: module(...)
            let result = if p.token == Token::Lparen {
                // Parse the first-class module, then continue with primary/binary parsing
                let pack_expr =
                    super::expr::parse_first_class_module_expr_from_paren(p, start_pos.clone());
                // Continue parsing the expression (handles ->, [], etc.)
                let expr = super::expr::parse_expr_with_operand(p, pack_expr);
                parse_newline_or_semicolon_structure(p);
                Some(StructureItemDesc::Pstr_eval(expr, attrs.clone()))
            } else {
                let def = parse_module_definition(p, module_start, attrs.clone());
                parse_newline_or_semicolon_structure(p);
                def
            };

            p.end_region();
            result
        }
        Token::Include => {
            // OCaml: parse_include_statement captures start_pos before consuming Include
            let include_start = p.start_pos.clone();
            p.next();
            let mod_expr = parse_module_expr(p);
            parse_newline_or_semicolon_structure(p);
            Some(StructureItemDesc::Pstr_include(IncludeDeclaration {
                pincl_mod: mod_expr,
                pincl_loc: p.mk_loc_to_prev_end(&include_start),
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
                parse_newline_or_semicolon_structure(p);
                Some(StructureItemDesc::Pstr_attribute(attr))
            } else {
                // @ might be a doc comment or similar
                p.next();
                let attr = parse_attribute_body(p, attr_start);
                parse_newline_or_semicolon_structure(p);
                Some(StructureItemDesc::Pstr_attribute(attr))
            }
        }
        Token::ModuleComment { loc, content } => {
            // Module-level doc comment (triple star /*** ... */) becomes a standalone res.doc attribute
            let loc_cloned = loc.clone();
            let content = content.clone();
            p.next();
            let loc = p.from_location(&loc_cloned);
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
            let id = with_loc(id_name, p.mk_loc_to_prev_end(&id_start));
            // Parse optional payload
            let payload = if p.token == Token::Lparen {
                p.next();
                let payload = parse_payload(p);
                p.expect(Token::Rparen);
                payload
            } else {
                Payload::PStr(vec![])
            };
            parse_newline_or_semicolon_structure(p);
            Some(StructureItemDesc::Pstr_extension(
                (id, payload),
                attrs.clone(),
            ))
        }
        _ => {
            // Could be an expression
            if is_expr_start(&p.token) {
                let expr = expr::parse_expr(p);
                parse_newline_or_semicolon_structure(p);
                Some(StructureItemDesc::Pstr_eval(expr, attrs.clone()))
            } else if p.token != Token::Eof {
                // Use err_unexpected which creates a proper Unexpected diagnostic
                // with breadcrumbs, matching OCaml's parse_region behavior
                p.err_unexpected();
                p.next();
                None
            } else {
                None
            }
        }
    };

    // Consume trailing semicolon if present (OCaml includes this in the structure item location)
    // If a semicolon is consumed, we can't share the LocIdx with value_binding because
    // the structure_item location now extends past the binding's end
    if p.token == Token::Semicolon {
        p.next();
        // Clear any shared LocIdx - the semicolon extends the structure_item location
        item_loc_idx = None;
    }

    desc.map(|d| StructureItem {
        pstr_desc: d,
        pstr_loc: match item_loc_idx {
            Some(loc_idx) => loc_idx,
            None => p.mk_loc_idx_to_prev_end(item_start_idx),
        },
    })
}

// ============================================================================
// Signature Parsing
// ============================================================================

/// Parse a signature (list of signature items).
///
/// This matches OCaml's `parse_region p ~grammar:Grammar.Specification ~f:parse_signature_item_region`.
/// When a signature item cannot be parsed (returns None), we emit an "unexpected" error and
/// advance the token to avoid infinite loops.
pub fn parse_signature(p: &mut Parser<'_>) -> Signature {
    use super::core::recover::should_abort_list_parse;
    use super::grammar::Grammar;

    // OCaml: Parser.leave_breadcrumb p Grammar.Specification
    p.leave_breadcrumb(Grammar::Specification);

    let mut items = vec![];

    loop {
        // Skip semicolons
        while p.token == Token::Semicolon {
            p.next();
        }

        match parse_signature_item(p) {
            Some(item) => items.push(item),
            None => {
                // OCaml's parse_region: when f returns None, check if we should abort
                if p.token == Token::Eof || should_abort_list_parse(p) {
                    break;
                }
                // Emit unexpected token error and advance to avoid infinite loop
                p.err_unexpected();
                p.next();
            }
        }
    }

    // OCaml: Parser.eat_breadcrumb p
    p.eat_breadcrumb();

    items
}

/// Parse a single signature item.
pub fn parse_signature_item(p: &mut Parser<'_>) -> Option<SignatureItem> {
    let start_pos = p.start_pos.clone();

    // Parse attributes
    let attrs = parse_attributes(p);

    let desc = match &p.token {
        Token::Open => {
            // OCaml's popen_loc starts at 'open', not at outer attributes
            let open_start = p.start_pos.clone();
            p.next();
            // Check for override flag: open! M
            let override_flag = if p.token == Token::Bang {
                p.next();
                OverrideFlag::Override
            } else {
                OverrideFlag::Fresh
            };
            let lid = parse_module_long_ident(p);
            parse_newline_or_semicolon_signature(p);
            let loc = p.mk_loc_to_prev_end(&open_start);
            Some(SignatureItemDesc::Psig_open(OpenDescription {
                popen_lid: lid,
                popen_override: override_flag,
                popen_loc: loc,
                popen_attributes: attrs.clone(),
            }))
        }
        Token::Let { .. } => {
            // For signature let declarations, OCaml uses regions
            p.begin_region();
            // OCaml's value_description location starts at 'let', not at the attributes
            let let_pos = p.start_pos.clone();
            p.next();
            let vd = parse_value_description(p, attrs.clone(), let_pos);
            parse_newline_or_semicolon_signature(p);
            p.end_region();
            Some(SignatureItemDesc::Psig_value(vd))
        }
        Token::Typ => {
            // OCaml uses regions around type parsing in signatures
            p.begin_region();
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

            let result = if is_type_extension {
                let ext = parse_type_extension(p, attrs.clone());
                parse_newline_or_semicolon_signature(p);
                Some(SignatureItemDesc::Psig_typext(ext))
            } else {
                let mut rec_flag = if p.token == Token::Rec {
                    p.next();
                    RecFlag::Recursive
                } else if matches!(&p.token, Token::Lident(s) if s == "nonrec") {
                    // Handle `type nonrec t = ...` - nonrec is a keyword in this context
                    p.next();
                    RecFlag::Nonrecursive
                } else {
                    RecFlag::Nonrecursive
                };
                let (decls, has_inline_types) = parse_type_declarations(p, attrs.clone(), type_start);
                // Inline record types require the type to be recursive
                if has_inline_types {
                    rec_flag = RecFlag::Recursive;
                }
                parse_newline_or_semicolon_signature(p);
                Some(SignatureItemDesc::Psig_type(rec_flag, decls))
            };
            p.end_region();
            result
        }
        Token::External => {
            p.next();
            let vd = parse_value_description(p, attrs.clone(), start_pos.clone());
            parse_newline_or_semicolon_signature(p);
            Some(SignatureItemDesc::Psig_value(vd))
        }
        Token::Exception => {
            // Capture start position BEFORE consuming 'exception' for accurate location
            let exception_start = p.start_pos.clone();
            p.next();
            let ext = parse_extension_constructor(p, attrs.clone(), Some(exception_start));
            parse_newline_or_semicolon_signature(p);
            Some(SignatureItemDesc::Psig_exception(ext))
        }
        Token::Module => {
            // OCaml uses regions around module parsing in signatures
            p.begin_region();
            let module_start = p.start_pos.clone();
            p.next();
            let result = if p.token == Token::Typ {
                p.next();
                let mtd = parse_module_type_declaration(p, attrs.clone(), false);
                // No parse_newline_or_semicolon_signature for module type - matches OCaml
                Some(SignatureItemDesc::Psig_modtype(mtd))
            } else if p.token == Token::Rec {
                p.next();
                // For recursive modules, OCaml's pmd_loc includes attributes BEFORE 'module rec'
                // OCaml passes start_pos (captured BEFORE attributes) to parse_rec_module_spec
                let mds = parse_rec_module_declarations(p, attrs.clone(), start_pos.clone());
                parse_newline_or_semicolon_signature(p);
                Some(SignatureItemDesc::Psig_recmodule(mds))
            } else {
                // For non-recursive modules, location starts at name (not 'module')
                let md = parse_module_declaration(p, attrs.clone(), None);
                parse_newline_or_semicolon_signature(p);
                Some(SignatureItemDesc::Psig_module(md))
            };
            p.end_region();
            result
        }
        Token::Include => {
            p.next();
            let mod_type = parse_module_type(p);
            parse_newline_or_semicolon_signature(p);
            Some(SignatureItemDesc::Psig_include(IncludeDescription {
                pincl_mod: mod_type,
                pincl_loc: p.mk_loc_to_prev_end(&start_pos),
                pincl_attributes: attrs.clone(),
            }))
        }
        Token::At | Token::AtAt => {
            // Capture start_pos before consuming @ or @@ so attribute location includes it
            let attr_start = p.start_pos.clone();
            if p.token == Token::AtAt {
                p.next();
                let attr = parse_attribute_body(p, attr_start);
                parse_newline_or_semicolon_signature(p);
                Some(SignatureItemDesc::Psig_attribute(attr))
            } else {
                p.next();
                let attr = parse_attribute_body(p, attr_start);
                parse_newline_or_semicolon_signature(p);
                Some(SignatureItemDesc::Psig_attribute(attr))
            }
        }
        Token::ModuleComment { loc, content } => {
            // Module-level doc comment (triple star /*** ... */) becomes a standalone res.doc attribute
            // Note: OCaml does NOT call parse_newline_or_semicolon_signature for ModuleComment
            let loc_cloned = loc.clone();
            let content = content.clone();
            p.next();
            let loc = p.from_location(&loc_cloned);
            let attr = super::core::doc_comment_to_attribute(loc, content);
            Some(SignatureItemDesc::Psig_attribute(attr))
        }
        Token::Percent | Token::PercentPercent => {
            let ext = parse_extension(p);
            parse_newline_or_semicolon_signature(p);
            Some(SignatureItemDesc::Psig_extension(ext, attrs.clone()))
        }
        _ => {
            // OCaml: when attrs are present but no valid item follows, report the error
            // and return a sigitemhole placeholder
            if let Some(attr) = attrs.first() {
                let attr_name = &attr.0.txt;
                let attr_loc = p.to_location(attr.0.loc);
                p.err_at(
                    attr_loc.loc_start.clone(),
                    attr_loc.loc_end.clone(),
                    DiagnosticCategory::message(&error_messages::attribute_without_node(attr_name)),
                );
                // Return sigitemhole as recovery
                return Some(super::core::recover::default_signature_item());
            }
            None
        }
    };

    desc.map(|d| {
        // OCaml includes optional trailing semicolon in signature_item location
        let end_pos = if p.token == Token::Semicolon {
            p.end_pos.clone()
        } else {
            p.prev_end_pos.clone()
        };
        SignatureItem {
            psig_desc: d,
            psig_loc: p.mk_loc(&start_pos, &end_pos),
        }
    })
}

// ============================================================================
// Module Expression Parsing
// ============================================================================

/// Parse a module expression.
pub fn parse_module_expr(p: &mut Parser<'_>) -> ModuleExpr {
    let start_pos = p.start_pos.clone();

    // Handle await keyword
    // OCaml lines 6622-6629: capture await start_pos BEFORE consuming, end_pos AFTER consuming
    // NOTE: OCaml uses p.end_pos after consuming await, which is the end of the NEXT token
    let await_loc = if p.token == Token::Await {
        let await_start = p.start_pos.clone();
        p.next();
        // OCaml: let end_pos = p.end_pos in - this is the end of the token AFTER await
        let await_end = p.end_pos.clone();
        Some(p.mk_loc(&await_start, &await_end))
    } else {
        None
    };

    // Parse attributes (e.g., @functorAttr)
    let mut attrs = parse_attributes(p);

    // Add await attribute if present
    // OCaml line 6632: make_await_attr loc_await
    if let Some(loc) = await_loc {
        attrs.insert(
            0,
            (
                Located {
                    txt: "res.await".to_string(),
                    loc,
                },
                Payload::PStr(vec![]),
            ),
        );
    }

    // Check if this is a functor: (args) => body or (args) : type => body
    // For primary expressions, capture start_pos for location override (OCaml line 6503)
    let (expr, primary_start_pos) = if is_es6_arrow_functor(p) {
        (parse_functor_module_expr(p), None)
    } else {
        let primary_start = p.start_pos.clone();
        (parse_primary_module_expr(p), Some(primary_start))
    };

    // Handle module application: F(X)
    let mut expr = parse_module_apply(p, expr);

    // OCaml line 6512: {mod_expr with pmod_loc = mk_loc start_pos p.prev_end_pos}
    // Override the outermost location after module application processing
    if let Some(primary_start) = primary_start_pos {
        expr.pmod_loc = p.mk_loc_to_prev_end(&primary_start);
    }

    // Handle module constraint: M : S
    let expr = if p.token == Token::Colon {
        p.next();
        let mod_type = parse_module_type(p);
        let loc = p.mk_loc_to_prev_end(&start_pos);
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
    // OCaml lines 6622-6629: capture await start_pos BEFORE consuming, end_pos AFTER consuming
    // NOTE: OCaml uses p.end_pos after consuming await, which is the end of the NEXT token
    let await_loc = if p.token == Token::Await {
        let await_start = p.start_pos.clone();
        p.next();
        // OCaml: let end_pos = p.end_pos in - this is the end of the token AFTER await
        let await_end = p.end_pos.clone();
        Some(p.mk_loc(&await_start, &await_end))
    } else {
        None
    };

    // Parse attributes (e.g., @functorAttr)
    let mut attrs = parse_attributes(p);

    // Add await attribute if present
    // OCaml line 6632: make_await_attr loc_await
    if let Some(loc) = await_loc {
        attrs.insert(
            0,
            (
                Located {
                    txt: "res.await".to_string(),
                    loc,
                },
                Payload::PStr(vec![]),
            ),
        );
    }

    // Check if this is a functor: (args) => body or (args) : type => body
    // For primary expressions, capture start_pos for location override (OCaml line 6503)
    let (expr, primary_start_pos) = if is_es6_arrow_functor(p) {
        (parse_functor_module_expr(p), None)
    } else {
        let primary_start = p.start_pos.clone();
        (parse_primary_module_expr(p), Some(primary_start))
    };

    // Handle module application: F(X)
    let mut expr = parse_module_apply(p, expr);

    // OCaml line 6512: {mod_expr with pmod_loc = mk_loc start_pos p.prev_end_pos}
    // Override the outermost location after module application processing
    if let Some(primary_start) = primary_start_pos {
        expr.pmod_loc = p.mk_loc_to_prev_end(&primary_start);
    }

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
                pmod_loc: p.mk_loc_to_prev_end(&start_pos),
                pmod_attributes: vec![],
            }
        }
        Token::Lbrace => {
            // Module structure: { ... }
            p.next();
            let items = parse_structure_in_braces(p);
            p.expect(Token::Rbrace);
            let loc = p.mk_loc_to_prev_end(&start_pos);
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
                    let loc = p.mk_loc_to_prev_end(&start_pos);
                    ModuleExpr {
                        pmod_desc: ModuleExprDesc::Pmod_structure(vec![]),
                        pmod_loc: loc,
                        pmod_attributes: vec![],
                    }
                } else {
                    let inner = parse_module_expr(p);
                    p.expect(Token::Rparen);
                    // OCaml: parse_atomic_module_expr does NOT update the location to include parens.
                    // It just returns mod_expr as-is, keeping the inner expression's original location.
                    inner
                }
            }
        }
        Token::Percent => {
            // Module extension
            let ext = parse_extension(p);
            let loc = p.mk_loc_to_prev_end(&start_pos);
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
                    // OCaml captures colon position for package type location start
                    let colon_pos = p.start_pos.clone();
                    p.next();
                    // Parse module type and convert to Ptyp_package
                    let mod_type = parse_module_type(p);
                    // Create Ptyp_package from module type
                    // OCaml uses colon_start for the package type location start
                    let pkg_loc = p.mk_loc(&colon_pos, &p.loc_end(mod_type.pmty_loc));
                    let pkg_type = CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_package(
                            module_type_to_package(p, &mod_type),
                        ),
                        ptyp_loc: pkg_loc,
                        ptyp_attributes: vec![],
                    };
                    // OCaml: expect Rparen first, then create constraint with loc including Rparen
                    p.expect(Token::Rparen);
                    let constraint_loc = p.mk_loc_to_prev_end(&start_pos);
                    Expression {
                        pexp_desc: ExpressionDesc::Pexp_constraint(
                            Box::new(expr),
                            pkg_type,
                        ),
                        pexp_loc: constraint_loc,
                        pexp_attributes: vec![],
                    }
                } else {
                    p.expect(Token::Rparen);
                    expr
                };
                let loc = p.mk_loc_to_prev_end(&start_pos);
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
        // OCaml: let start_pos = p.Parser.start_pos in
        // Capture lparen position before consuming it
        let lparen_pos = p.start_pos.clone();
        p.next();

        let args = if p.token == Token::Rparen {
            // Empty argument: F()
            p.next();
            // OCaml: let loc = mk_loc start_pos p.prev_end_pos in
            // For empty args, loc spans from lparen to after rparen
            let loc = p.mk_loc_to_prev_end(&lparen_pos);
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

        // OCaml: List.fold_left (fun mod_expr arg ->
        //   Ast_helper.Mod.apply
        //     ~loc:(mk_loc mod_expr.pmod_loc.loc_start arg.pmod_loc.loc_end)
        //     mod_expr arg)
        //   mod_expr args
        // Each nested apply gets loc from current result start to current arg end
        for arg in args {
            let loc = p.mk_loc(&p.loc_start(result.pmod_loc), &p.loc_end(arg.pmod_loc));
            result = ModuleExpr {
                pmod_desc: ModuleExprDesc::Pmod_apply(Box::new(result), Box::new(arg)),
                pmod_loc: loc,
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
                    let ident_str_idx = p.arena_mut().push_string(ident);
                    let mut lid = Longident::Lident(ident_str_idx);
                    while p.token == Token::Dot {
                        p.next();
                        match &p.token {
                            Token::Uident(name) => {
                                let name_clone = name.clone();
                                let name_str_idx = p.arena_mut().push_string(name_clone);
                                lid = Longident::Ldot(Box::new(lid), name_str_idx);
                                p.next();
                            }
                            _ => break,
                        }
                    }
                    let lid_idx = p.push_longident(lid);
                    let loc = p.mk_loc_to_prev_end(&start_pos);
                    let mod_type = ModuleType {
                        pmty_desc: ModuleTypeDesc::Pmty_ident(with_loc(lid_idx, loc.clone())),
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
                    let lid_idx = p.push_lident(&ident);
                    let mod_ident = with_loc(lid_idx, loc.clone());
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
            let name_loc = p.mk_loc_to_prev_end(&start_pos);
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
            let name_loc = p.mk_loc_to_prev_end(&start_pos);
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
    // Capture position before ( - OCaml includes ( in the first arg's location
    let paren_start_pos = p.start_pos.clone();
    p.expect(Token::Lparen);

    let mut args = vec![];
    let mut is_first = true;
    while p.token != Token::Rparen && p.token != Token::Eof {
        if let Some(mut arg) = parse_functor_arg(p) {
            // For the first argument, OCaml includes the opening ( in the functor location
            if is_first {
                arg.start_pos = paren_start_pos.clone();
                is_first = false;
            }
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
            name: with_loc("*".to_string(), p.mk_loc_to_prev_end(&paren_start_pos)),
            mod_type: None,
            start_pos: paren_start_pos,
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
    // Pmod_constraint(expr, type) represents "expr : type"
    // OCaml creates a backwards location from mod_expr start to mod_type end,
    // which acts as a ghost location since the body comes after the type constraint
    let rhs_module_expr = match return_type {
        Some(mod_type) => {
            let loc = p.mk_loc(
                &p.loc_start(rhs_module_expr.pmod_loc),
                &p.loc_end(mod_type.pmty_loc),
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
    // Parse attributes (e.g. `@attr module type of M`)
    let attrs = parse_attributes(p);

    // Capture start position AFTER attributes - OCaml's module type locations
    // don't include leading attributes
    let start_pos = p.start_pos.clone();

    // Parse functor types first (if allowed), then fall back to primary module types.
    let typ = if es6_arrow {
        parse_functor_module_type(p)
    } else {
        parse_primary_module_type(p)
    };

    // OCaml attaches attributes to the inner module type BEFORE parsing `with` constraints.
    // This means `@attr Foo with type t = int` becomes `Pmty_with(Foo[@attr], constraints)`,
    // not `Pmty_with(Foo, constraints)[@attr]`.
    let mut typ = typ;
    if !attrs.is_empty() {
        typ.pmty_attributes.extend(attrs);
    }

    // Handle with constraint: S with type t = ...
    // Only if parse_with is true - functor bodies don't parse `with` so it applies to the outer functor
    if parse_with && matches!(&p.token, Token::Lident(s) if s == "with") {
        p.next();
        let constraints = parse_with_constraints(p);
        let loc = p.mk_loc_to_prev_end(&start_pos);
        ModuleType {
            pmty_desc: ModuleTypeDesc::Pmty_with(Box::new(typ), constraints),
            pmty_loc: loc,
            pmty_attributes: vec![],
        }
    } else {
        typ
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
        let loc = p.mk_loc_to_end_of(&start_pos, body.pmty_loc);
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
        // OCaml creates a location spanning the `()` for the `*` param name
        let name_loc = p.mk_loc_to_prev_end(&start_pos);
        params.push(FunctorParam {
            name: with_loc("*".to_string(), name_loc),
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
                    p.next();
                    // OCaml's arg_name location spans from start_pos (before attrs) to end of `_`
                    let name_loc = p.mk_loc_to_prev_end(&start_pos);
                    let name = with_loc("_".to_string(), name_loc);
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
                                let loc = p.mk_loc_current();
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
                        // OCaml creates a location spanning the `()` for the `*` param name
                        let name_loc = p.mk_loc_to_prev_end(&start_pos);
                        params.push(FunctorParam {
                            name: with_loc("*".to_string(), name_loc),
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
    let end_pos = p.loc_end(body.pmty_loc);

    // OCaml includes the opening `(` in the outermost Pmty_functor location
    // For inner functors (in multi-param), use the param's start position
    // After enumerate().rev(), idx==0 is the first param in original order
    params
        .into_iter()
        .enumerate()
        .rev()
        .fold(body, |acc, (idx, param)| {
            // First param in original order (idx==0) uses functor_start (opening paren)
            let start = if idx == 0 {
                functor_start.clone()
            } else {
                param.start_pos
            };
            let loc = p.mk_loc(&start, &end_pos);
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
                pmty_loc: p.mk_loc_to_prev_end(&start_pos),
                pmty_attributes: vec![],
            }
        }
        Token::Lbrace => {
            // Signature: { ... }
            p.next();
            let items = parse_signature_in_braces(p);
            p.expect(Token::Rbrace);
            let loc = p.mk_loc_to_prev_end(&start_pos);
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
            // OCaml updates the location to include the parentheses
            ModuleType {
                pmty_loc: p.mk_loc_to_prev_end(&start_pos),
                ..inner
            }
        }
        Token::Module => {
            // module type of M
            p.next();
            if p.token == Token::Typ {
                p.next();
                if p.token == Token::Of {
                    p.next();
                    let mod_expr = parse_module_expr(p);
                    let loc = p.mk_loc_to_prev_end(&start_pos);
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
            let loc = p.mk_loc_to_prev_end(&start_pos);
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
///
/// Like parse_signature, this matches OCaml's parse_region behavior.
/// When a signature item cannot be parsed, we emit an error and advance.
///
/// Note: Uses Grammar::Signature (not Specification) because Signature terminates on Rbrace.
fn parse_signature_in_braces(p: &mut Parser<'_>) -> Vec<SignatureItem> {
    use super::core::recover::should_abort_list_parse;
    use super::grammar::Grammar;

    // OCaml: Parser.leave_breadcrumb p Grammar.Signature (not Specification!)
    // Grammar.Signature terminates on Rbrace, while Specification doesn't.
    p.leave_breadcrumb(Grammar::Signature);

    let mut items = vec![];

    loop {
        // Skip semicolons
        while p.token == Token::Semicolon {
            p.next();
        }

        match parse_signature_item(p) {
            Some(item) => items.push(item),
            None => {
                // OCaml's parse_region: when f returns None, check if we should abort
                if p.token == Token::Eof || should_abort_list_parse(p) {
                    break;
                }
                // Emit unexpected token error and advance to avoid infinite loop
                p.err_unexpected();
                p.next();
            }
        }
    }

    // OCaml: Parser.eat_breadcrumb p
    p.eat_breadcrumb();

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
            let type_name = p.arena().lident_last(lid.txt).to_string();
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
            // Note: type variables can be uppercase ('X) or lowercase ('a)
            let mut cstrs = vec![];
            while p.token == Token::Constraint {
                let cstr_start = p.start_pos.clone();
                p.next();
                p.expect(Token::SingleQuote);
                // Accept both Lident ('a) and Uident ('X) as type variable names
                let var = match &p.token {
                    Token::Lident(name) | Token::Uident(name) => {
                        let name = name.clone();
                        // OCaml: ident_loc spans from 'constraint' keyword to end of identifier
                        let loc = p.mk_loc_to_end(&cstr_start);
                        p.next();
                        CoreType {
                            ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                            ptyp_loc: loc,
                            ptyp_attributes: vec![],
                        }
                    }
                    _ => break,
                };
                p.expect(Token::Equal);
                let cstr_typ = typ::parse_typ_expr(p);
                cstrs.push((var, cstr_typ, p.mk_loc_to_prev_end(&cstr_start)));
            }

            let decl = TypeDeclaration {
                // OCaml uses the full longident location for the type name location
                ptype_name: with_loc(type_name, type_loc.clone()),
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
fn parse_module_long_ident(p: &mut Parser<'_>) -> Loc<LidentIdx> {
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

    let lid_idx = super::core::build_longident_idx(p, &path_parts);
    let loc = p.mk_loc_to_prev_end(&start_pos);
    with_loc(lid_idx, loc)
}

/// Parse a type long identifier.
fn parse_type_long_ident(p: &mut Parser<'_>) -> Loc<LidentIdx> {
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

    let lid_idx = super::core::build_longident_idx(p, &path_parts);
    let loc = p.mk_loc_to_prev_end(&start_pos);
    with_loc(lid_idx, loc)
}

/// Parse let bindings.
/// `start_pos_idx` should be the PosIdx BEFORE the `let` keyword for the first binding.
/// `let_end_pos` should be the position AFTER the `let`/`let?` keyword (for let.unwrap attribute location).
/// Using PosIdx for start_pos enables position sharing with the structure_item location.
/// Returns (bindings, Option<LocIdx>) where LocIdx is Some only when there's exactly one binding
/// (to enable sharing with structure_item). For multiple bindings, returns None and the caller
/// should create a fresh location spanning all bindings.
fn parse_let_bindings(
    p: &mut Parser<'_>,
    start_pos_idx: PosIdx,
    let_end_pos: Position,
    _rec_flag: RecFlag,
    outer_attrs: Attributes,
    unwrap: bool,
) -> (Vec<ValueBinding>, Option<LocIdx>) {
    let mut bindings = vec![];
    // For the first binding, use the passed-in PosIdx to enable sharing
    let mut binding_start_idx = start_pos_idx;
    // Track the first binding's LocIdx for sharing with structure_item
    let mut first_binding_loc: Option<LocIdx> = None;
    // Track the location for let.unwrap attribute
    // For first binding: from start_pos to let_end_pos
    // For and bindings: captured at end of previous iteration when we see 'and'
    let start_pos = *p.arena().get_position(start_pos_idx);
    let mut unwrap_attr_start = start_pos;
    let mut unwrap_attr_end = let_end_pos;

    loop {
        // OCaml's parse_let_binding_body uses begin_region and leave_breadcrumb(LetBinding)
        // for each binding. This allows proper error messages and error de-duplication.
        p.begin_region();
        p.leave_breadcrumb(Grammar::LetBinding);

        let mut attrs = parse_attributes(p);

        // Handle `and` after attributes: `@attr and foo = ...`
        // We need to check for And here and capture its location for let.unwrap
        if p.token == Token::And {
            // Capture `and` keyword location for let.unwrap attribute
            unwrap_attr_start = p.start_pos.clone();
            unwrap_attr_end = p.end_pos.clone();
            p.next(); // consume `and`
        }

        // For the first binding, prepend the outer attributes
        if bindings.is_empty() {
            attrs = [outer_attrs.clone(), attrs].concat();
        }
        // Add let.unwrap attribute if this is let? (for all bindings, including `and` bindings)
        // OCaml uses: mk_loc start_pos end_pos (not ghost!)
        if unwrap {
            let unwrap_loc = p.mk_loc(&unwrap_attr_start, &unwrap_attr_end);
            attrs.push((
                with_loc("let.unwrap".to_string(), unwrap_loc),
                Payload::PStr(vec![]),
            ));
        }

        // OCaml sets Pattern breadcrumb before parsing the pattern
        p.leave_breadcrumb(Grammar::Pattern);
        let pat = pattern::parse_pattern(p);
        p.eat_breadcrumb();

        // Handle optional type annotation: let x: int = ...
        // Also track locally abstract types for Pexp_newtype wrapping
        let (pat, newtype_info, is_locally_abstract) = if p.token == Token::Colon {
            p.next();

            // Check if this is `type a.` syntax (locally abstract types)
            // Only `type a.` syntax generates Pexp_newtype, not `'a.` syntax
            let is_locally_abstract = p.token == Token::Typ;

            // OCaml uses parse_poly_type_expr here (not parse_typ_expr)
            // This has special handling for `'a => T` to exclude the `'` from arrow location
            let typ = typ::parse_poly_type_expr(p);

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
                    let substituted_inner = typ::substitute_type_vars(p.arena(), (*inner).clone(), &var_set);

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
            let loc = p.mk_loc_spanning(pat.ppat_loc, final_typ.ptyp_loc);
            let pat = Pattern {
                ppat_desc: PatternDesc::Ppat_constraint(Box::new(pat), final_typ),
                ppat_loc: loc,
                ppat_attributes: vec![],
            };
            (pat, newtype_info, is_locally_abstract)
        } else {
            (pat, None, false)
        };

        p.expect(Token::Equal);
        let mut expr = expr::parse_expr(p);

        // Compute binding location BEFORE wrapping (OCaml uses this for locally abstract type locations)
        // Use PosIdx-based location to enable sharing with structure_item
        let binding_loc = p.mk_loc_idx_to_prev_end(binding_start_idx);
        // Track the first binding's LocIdx for sharing with structure_item
        if first_binding_loc.is_none() {
            first_binding_loc = Some(binding_loc);
        }

        // For locally abstract types, OCaml uses the binding location for the pattern constraint and Ptyp_poly
        let pat = if is_locally_abstract {
            // Update Ppat_constraint and inner Ptyp_poly locations to use the binding location
            if let PatternDesc::Ppat_constraint(inner_pat, typ) = pat.ppat_desc {
                // Update the Ptyp_poly location to use binding location
                let updated_typ = match typ.ptyp_desc {
                    CoreTypeDesc::Ptyp_poly(vars, inner) => CoreType {
                        ptyp_desc: CoreTypeDesc::Ptyp_poly(vars, inner),
                        ptyp_loc: binding_loc.clone(),
                        ptyp_attributes: typ.ptyp_attributes,
                    },
                    _ => typ,
                };
                Pattern {
                    ppat_desc: PatternDesc::Ppat_constraint(inner_pat, updated_typ),
                    ppat_loc: binding_loc.clone(),
                    ppat_attributes: pat.ppat_attributes,
                }
            } else {
                pat
            }
        } else {
            pat
        };

        // Wrap expression in Pexp_newtype for locally abstract types
        // Also add Pexp_constraint with the inner type (from Ptyp_poly)
        // OCaml uses the binding location for all these wrappers
        if let Some((newtype_vars, inner_type)) = newtype_info {
            // First wrap the expression in Pexp_constraint with the inner type
            // OCaml uses the binding location for this Pexp_constraint
            expr = Expression {
                pexp_desc: ExpressionDesc::Pexp_constraint(Box::new(expr), inner_type),
                pexp_loc: binding_loc.clone(),
                pexp_attributes: vec![],
            };

            // Then wrap in Pexp_newtype for each type variable
            // Fold in reverse order so that the outermost newtype is the first variable
            // OCaml uses the binding location for all Pexp_newtype wrappers
            for var in newtype_vars.into_iter().rev() {
                expr = Expression {
                    pexp_desc: ExpressionDesc::Pexp_newtype(var, Box::new(expr)),
                    pexp_loc: binding_loc.clone(),
                    pexp_attributes: vec![],
                };
            }
        }

        let loc = binding_loc;
        bindings.push(ValueBinding {
            pvb_pat: pat,
            pvb_expr: expr,
            pvb_attributes: attrs,
            pvb_loc: loc,
        });

        // End the region and breadcrumb for this binding
        p.eat_breadcrumb();
        p.end_region();

        // Check for `and` to continue with more bindings.
        // Also handle attributes/doc comments before `and`: `@attr and ...` or `/** doc */ and ...`
        if p.token == Token::And {
            // Capture `and` keyword location for let.unwrap attribute in next binding
            // OCaml uses p.Parser.start_pos and p.Parser.end_pos (before consuming 'and')
            unwrap_attr_start = p.start_pos;
            unwrap_attr_end = p.end_pos;
            // Capture start position BEFORE consuming `and` for next binding
            // For `and` bindings, we push a fresh position (no sharing needed with structure_item)
            binding_start_idx = p.push_start_pos();
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
            binding_start_idx = p.push_start_pos();
            // Continue - attributes will be parsed in the next iteration, and `and` will be consumed there
        } else {
            break;
        }
    }

    // Return bindings and optionally the LocIdx for sharing with structure_item.
    // Only share when there's exactly one binding - for multiple bindings, the structure_item
    // should span all bindings which is different from the first binding's location.
    if bindings.len() == 1 {
        (bindings, first_binding_loc)
    } else {
        (bindings, None)
    }
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
        let inline_ctx = RefCell::new(InlineTypesContext::new(vec![], Location::none()));
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
                    // OCaml uses the outer type's name location for inline type ptype_name,
                    // not the inline type's location
                    let inline_decl = TypeDeclaration {
                        ptype_name: with_loc(inline_name.clone(), ctx.outer_name_loc.clone()),
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
            // Capture start position BEFORE parsing attributes/doc comments
            // OCaml does: let start_pos = p.Parser.start_pos in let attrs = parse_attributes_and_binding p in
            // So the location should include the doc comment
            decl_start = p.start_pos.clone();
            // Parse the attributes/doc comments, then consume `and`
            next_attrs = parse_attributes(p);
            if p.token == Token::And {
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
    let inline_ctx = RefCell::new(InlineTypesContext::new(vec![], Location::none()));
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

    let lid_idx = super::core::build_longident_idx(p, &parts);
    let path = with_loc(lid_idx, p.mk_loc_to_prev_end(&start_pos));

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
    // OCaml includes the `|` in the constructor location
    let mut bar_start = if p.token == Token::Bar {
        let pos = p.start_pos.clone();
        p.next();
        Some(pos)
    } else {
        None
    };

    let mut constructors = vec![];
    while p.token != Token::Eof {
        constructors.push(parse_extension_constructor(p, vec![], bar_start.clone()));
        if p.token == Token::Bar {
            bar_start = Some(p.start_pos.clone());
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
            let loc = p.mk_loc_current();
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

    // Store params and outer name location in context for inline type declarations
    {
        let mut ctx = inline_ctx.borrow_mut();
        ctx.params = params.clone();
        ctx.outer_name_loc = name.loc.clone();
    }

    // The type name path for inline record desugaring
    let current_type_name_path = vec![name.txt.clone()];

    // Parse optional manifest and/or kind (and ReScript's `type t = T = ...` equation form).
    // OCaml: if token is Bar, call expect Equal to generate "Did you forget a `=` here?" error
    let (manifest, kind, private) = if p.token == Token::Equal || p.token == Token::Bar {
        if p.token == Token::Bar {
            // Generate error about missing `=` before variant constructors
            p.expect(Token::Equal);
        }
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
        // is a constructor in a variant type.
        // OCaml parser logic: if Uident is followed by `.`, it's a type path (Module.Type),
        // otherwise it's a constructor declaration.
        let is_variant_type_starting_with_uident = |p: &mut Parser<'_>| -> bool {
            p.lookahead(|state| {
                state.next(); // consume Uident

                // `Foo.bar` is a type path, not a constructor.
                // Anything else means it's a constructor.
                state.token != Token::Dot
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
                    // Detect if this is a spread-first CLOSED object: `{...spread, fields}`
                    // (not `{.. ...spread}` or `{. ...spread}`)
                    // OCaml uses ghost location for this specific case
                    let is_spread_first = p.lookahead(|state| {
                        state.next(); // consume {
                        // Check if first token is DotDotDot (not Dot or DotDot)
                        state.token == Token::DotDotDot
                    });
                    let typ = typ::parse_typ_expr(p);
                    // Apply ghost location fix for spread-first closed objects
                    let typ = fix_spread_first_object_manifest(p, typ, is_spread_first).0;
                    manifest = Some(typ);
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
        // OCaml includes "constraint" keyword in the type variable and constraint locations
        let cstr_start = p.start_pos.clone();
        p.next();
        // Parse the type variable
        p.expect(Token::SingleQuote);
        let var = if let Token::Lident(name) = &p.token {
            let name = name.clone();
            let var_end = p.end_pos.clone();
            p.next();
            // OCaml: type variable location starts at "constraint" keyword
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_var(name),
                ptyp_loc: p.mk_loc(&cstr_start, &var_end),
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
        // OCaml: constraint tuple location also starts at "constraint" keyword
        cstrs.push((var, typ, p.mk_loc_to_prev_end(&cstr_start)));
    }

    // In type declarations with qualified type constructors (Ldot),
    // OCaml uses the same location for both the CoreType and the longident
    // (just the constructor name, not including type arguments).
    // For unqualified (Lident), the CoreType uses full extent.
    let manifest = manifest.map(|t| fix_type_manifest_location(p.arena(), t));

    let loc = p.mk_loc_to_prev_end(&start_pos);
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

/// Fix the location of a direct spread-first object type manifest.
/// ONLY applies to the outermost level when the type starts with `{...spread`
/// (not `{.. ...spread}` or `{. ...spread}` - those use real locations).
/// Also propagates ghost loc_start to alias wrappers.
///
/// Returns the fixed type and whether the fix was applied.
fn fix_spread_first_object_manifest(p: &mut Parser<'_>, typ: CoreType, is_spread_first: bool) -> (CoreType, bool) {
    if !is_spread_first {
        return (typ, false);
    }

    match typ.ptyp_desc {
        CoreTypeDesc::Ptyp_object(ref fields, closed) => {
            // Only for CLOSED objects where first field is Oinherit (spread)
            // Open objects (from {.. ...spread}) should NOT use ghost location
            let first_is_spread = fields.first().map_or(false, |f| matches!(f, ObjectField::Oinherit(_)));
            if first_is_spread && closed == ClosedFlag::Closed {
                (CoreType {
                    ptyp_loc: Location::none(),
                    ..typ
                }, true)
            } else {
                (typ, false)
            }
        }
        CoreTypeDesc::Ptyp_alias(inner, alias) => {
            // Recurse into inner type
            let (fixed_inner, applied) = fix_spread_first_object_manifest(p, *inner, true);
            if applied {
                // OCaml uses inner type's loc_start for alias, so if inner is ghost, alias start is ghost too
                // Create a spanning location from fixed_inner start to typ end
                let new_loc = p.mk_loc_spanning(fixed_inner.ptyp_loc, typ.ptyp_loc);
                (CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_alias(Box::new(fixed_inner), alias),
                    ptyp_loc: new_loc,
                    ptyp_attributes: typ.ptyp_attributes,
                }, true)
            } else {
                (CoreType {
                    ptyp_desc: CoreTypeDesc::Ptyp_alias(Box::new(fixed_inner), alias),
                    ..typ
                }, false)
            }
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, arity } => {
            // Recurse into arg ONLY if this is the direct manifest level
            let (fixed_arg_typ, _) = fix_spread_first_object_manifest(p, arg.typ, true);
            let fixed_arg = TypeArg {
                typ: fixed_arg_typ,
                ..*arg
            };
            (CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_arrow {
                    arg: Box::new(fixed_arg),
                    ret,
                    arity,
                },
                ..typ
            }, false)
        }
        _ => (typ, false),
    }
}

/// Fix the location of a Ptyp_constr in a type manifest context.
/// For qualified identifiers (Ldot), OCaml uses the same location for both
/// the CoreType and the longident. For unqualified (Lident), it uses full extent.
/// EXCEPTION: When the type has attributes, OCaml keeps the full extent including
/// type arguments (this allows the location to cover the entire attributed type).
fn fix_type_manifest_location(arena: &crate::parse_arena::ParseArena, typ: CoreType) -> CoreType {
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_constr(lid, _) => {
            // Only adjust for qualified identifiers (Ldot) WITHOUT attributes
            // When there are attributes, OCaml keeps the full extent including type args
            if arena.is_ldot(lid.txt) && typ.ptyp_attributes.is_empty() {
                CoreType {
                    ptyp_loc: lid.loc.clone(),
                    ..typ
                }
            } else {
                typ
            }
        }
        CoreTypeDesc::Ptyp_arrow { arg, ret, arity } => {
            // Also adjust Ldot types in arrow parameter positions
            // But NOT in return types - OCaml keeps type args in return type ptyp_loc
            let fixed_arg = TypeArg {
                typ: fix_type_manifest_location(arena, arg.typ.clone()),
                ..*arg.clone()
            };
            // Return type is NOT fixed - OCaml includes type args for return types
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_arrow {
                    arg: Box::new(fixed_arg),
                    ret: ret.clone(),
                    arity: *arity,
                },
                ..typ
            }
        }
        CoreTypeDesc::Ptyp_alias(inner, alias_name) => {
            // Recurse into the aliased type (e.g., `Color.t<...> as 'rgb`)
            let fixed_inner = fix_type_manifest_location(arena, inner.as_ref().clone());
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_alias(Box::new(fixed_inner), alias_name.clone()),
                ..typ
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
                        let loc = p.mk_loc_current();
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
                let loc = p.mk_loc_current();
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
                    let loc = p.mk_loc_current();
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
                        ptyp_loc: p.mk_loc_to_prev_end(&param_start),
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
                        ptyp_loc: p.mk_loc_to_prev_end(&param_start),
                        ptyp_attributes: vec![],
                    };
                    params.push((typ, variance));
                }
            }
        } else if p.token == Token::Underscore {
            // Anonymous/wildcard type parameter: _
            let loc = p.mk_loc_current();
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
            let loc = p.mk_loc_current();
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
                    let loc = p.mk_loc_current();
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
                    let loc = p.mk_loc_current();
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
            // Record type - capture { position for first field's location (OCaml extends first spread field's loc to {)
            let lbrace_pos = p.start_pos.clone();
            p.next();
            let labels = parse_label_declarations(p, inline_ctx, current_type_name_path, Some(lbrace_pos), false);
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
        p.next();
        // OCaml uses start_pos (which may include |) for name location as well
        let name_loc = p.mk_loc_to_prev_end(&start_pos);
        let spread_type = typ::parse_typ_expr(p);
        // Declaration location includes type
        let loc = p.mk_loc_to_prev_end(&start_pos);
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
            let loc = p.mk_loc_current();
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
            let lbrace_pos = p.start_pos.clone();
            p.next(); // consume {
            let labels = parse_label_declarations(p, None, None, Some(lbrace_pos), true);
            p.expect(Token::Rbrace);
            // Allow trailing comma after inline record: Foo({...},)
            p.optional(&Token::Comma);
            p.expect(Token::Rparen);
            ConstructorArguments::Pcstr_record(labels)
        } else {
            let mut args = vec![];
            // OCaml's parse_constr_decl_args has special handling for object types
            // ONLY for the first argument: it captures start_pos AFTER consuming `{`,
            // so the object type location excludes the opening brace.
            // Subsequent arguments use parse_comma_delimited_region with normal parse_typ_expr.
            let mut is_first = true;
            while p.token != Token::Rparen && p.token != Token::Eof {
                let arg = if is_first && p.token == Token::Lbrace {
                    // Check if this is an object type (has ., .., ..., or string keys)
                    let is_object = p.lookahead(|state| {
                        state.next(); // consume {
                        // Skip any leading attributes
                        while state.token == Token::At {
                            state.next();
                            if matches!(state.token, Token::Lident(_) | Token::Uident(_) | Token::Module) {
                                state.next();
                            }
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
                        matches!(
                            state.token,
                            Token::String(_) | Token::Dot | Token::DotDot | Token::DotDotDot | Token::Rbrace
                        )
                    });
                    if is_object {
                        // Consume `{` and capture start_pos AFTER (like OCaml)
                        p.next();
                        let start_pos = p.start_pos.clone();
                        typ::parse_object_type_body(p, start_pos)
                    } else {
                        typ::parse_typ_expr(p)
                    }
                } else {
                    typ::parse_typ_expr(p)
                };
                args.push(arg);
                is_first = false;
                if !p.optional(&Token::Comma) {
                    break;
                }
            }
            p.expect(Token::Rparen);
            ConstructorArguments::Pcstr_tuple(args)
        }
    } else if p.token == Token::Lbrace {
        let lbrace_pos = p.start_pos.clone();
        p.next();
        let labels = parse_label_declarations(p, None, None, Some(lbrace_pos), true);
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

    let loc = p.mk_loc_to_prev_end(&start_pos);
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
/// `lbrace_pos` is the position of the opening `{` - OCaml extends the first spread field's
/// location back to this position.
/// `for_constructor_record`: When true, exclude attributes from label_declaration locations
/// (for Pcstr_record in constructors). When false, include them (for Ptype_record).
fn parse_label_declarations(
    p: &mut Parser<'_>,
    inline_ctx: Option<&RefCell<InlineTypesContext>>,
    current_type_name_path: Option<&Vec<String>>,
    lbrace_pos: Option<Position>,
    for_constructor_record: bool,
) -> Vec<LabelDeclaration> {
    p.leave_breadcrumb(grammar::Grammar::FieldDeclarations);
    let mut labels = vec![];
    let mut is_first = true;

    while p.token != Token::Rbrace && p.token != Token::Eof {
        // Handle spread: ...typ - represented as a label with name "..."
        if p.token == Token::DotDotDot {
            let start_pos = p.start_pos.clone();
            p.next();
            // Name location is just the "..." part
            let name_loc = p.mk_loc_to_prev_end(&start_pos);
            let spread_type = typ::parse_typ_expr(p);
            // OCaml extends the first spread field's location back to the { position
            let loc_start = if is_first {
                lbrace_pos.as_ref().unwrap_or(&start_pos)
            } else {
                &start_pos
            };
            // OCaml has different end position handling for first vs non-first spread fields:
            // - First spread field: location extends to p.end_pos (includes comma/brace)
            // - Non-first spread fields: location uses typ.ptyp_loc.loc_end
            let loc_end = if is_first {
                p.end_pos.clone()
            } else {
                p.loc_end(spread_type.ptyp_loc)
            };
            let loc = p.mk_loc(loc_start, &loc_end);
            labels.push(LabelDeclaration {
                pld_name: with_loc("...".to_string(), name_loc),
                pld_mutable: MutableFlag::Immutable,
                pld_type: spread_type,
                pld_loc: loc,
                pld_attributes: vec![],
                pld_optional: false,
            });
            is_first = false;
            if !p.optional(&Token::Comma) {
                break;
            }
            continue;
        }

        if let Some(l) = parse_label_declaration(p, inline_ctx, current_type_name_path, for_constructor_record) {
            labels.push(l);
        }
        is_first = false;
        if !p.optional(&Token::Comma) {
            break;
        }
    }

    p.eat_breadcrumb();
    labels
}

/// Parse a single label declaration.
///
/// `for_constructor_record`: When true, capture location start AFTER attributes
/// (for Pcstr_record in constructors). When false, capture BEFORE (for Ptype_record).
/// This matches OCaml's behavior where constructor inline records parse outer attrs
/// before calling the field parser.
fn parse_label_declaration(
    p: &mut Parser<'_>,
    inline_ctx: Option<&RefCell<InlineTypesContext>>,
    current_type_name_path: Option<&Vec<String>>,
    for_constructor_record: bool,
) -> Option<LabelDeclaration> {
    // For regular records, capture start_pos BEFORE attrs (include attrs in location).
    // For constructor records, capture AFTER attrs (exclude attrs from location).
    // This matches OCaml's behavior where constructor records parse outer attrs
    // at a higher level before calling the field parser.
    let start_pos_before_attrs = p.start_pos.clone();
    let attrs = parse_attributes(p);
    let start_pos = if for_constructor_record {
        p.start_pos.clone()
    } else {
        start_pos_before_attrs
    };

    let mutable = if p.token == Token::Mutable {
        p.next();
        MutableFlag::Mutable
    } else {
        MutableFlag::Immutable
    };

    let name = match &p.token {
        Token::Lident(n) => {
            let n = n.clone();
            let loc = p.mk_loc_current();
            p.next();
            with_loc(n, loc)
        }
        _ => {
            // Match OCaml's error handling: give specific messages for dangling attrs/mutable
            // OCaml uses p.end_pos as the end position (not p.start_pos)
            if !attrs.is_empty() {
                p.err_at(start_pos.clone(), p.end_pos.clone(), DiagnosticCategory::Message(
                    "Attributes and doc comments can only be used at the beginning of a field declaration".to_string(),
                ));
            } else if mutable == MutableFlag::Mutable {
                p.err_at(start_pos.clone(), p.end_pos.clone(), DiagnosticCategory::Message(
                    "The `mutable` qualifier can only be used at the beginning of a field declaration".to_string(),
                ));
            } else {
                p.err(DiagnosticCategory::Message(
                    "Expected field name".to_string(),
                ));
            }
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

    // Check for colon, equal (error recovery), or field punning
    // OCaml: if Equal instead of Colon, emit specific error and continue
    let is_punning = match &p.token {
        Token::Colon => {
            p.next();
            false
        }
        Token::Equal => {
            // Common mistake: using = instead of : in record type declarations
            p.err(DiagnosticCategory::Message(
                "Record fields in type declarations use `:`. Example: `{field: string}`".to_string(),
            ));
            p.next();
            false
        }
        _ => {
            // Field punning: {form} is shorthand for {form: form}
            true
        }
    };

    // Check if the field type is an inline record
    let typ = if is_punning {
        // Field punning: {form} becomes {form: form}
        // The type is a simple type constructor with the field name
        let lid_idx = p.push_lident(&name.txt);
        CoreType {
            ptyp_desc: CoreTypeDesc::Ptyp_constr(
                with_loc(lid_idx, name.loc.clone()),
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
            let labels = parse_label_declarations(p, Some(inline_ctx), Some(extended_path), Some(type_start_pos.clone()), false);
            p.expect(Token::Rbrace);
            let type_loc = p.mk_loc_to_prev_end(&type_start_pos);

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
            let lid_idx = p.push_lident(&inline_type_name);
            CoreType {
                ptyp_desc: CoreTypeDesc::Ptyp_constr(
                    with_loc(lid_idx, type_loc.clone()),
                    type_args,
                ),
                ptyp_loc: type_loc,
                ptyp_attributes: vec![],
            }
        } else {
            // Not a record - parse as regular type (object type)
            typ::parse_poly_type_expr(p)
        }
    } else {
        typ::parse_poly_type_expr(p)
    };

    // OCaml uses typ.ptyp_loc.loc_end for label declaration's end, not p.prev_end_pos
    let loc = p.mk_loc_to_end_of(&start_pos, typ.ptyp_loc);
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
            let loc = p.mk_loc_current();
            p.next();
            with_loc(n, loc)
        }
        Token::String(s) => {
            // Escaped identifier like "export" - store with quotes for printer to recognize
            let n = format!("\"{}\"", s);
            let loc = p.mk_loc_current();
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
    // OCaml: InExternal.status := true before parsing type
    let was_in_external = p.in_external;
    p.in_external = true;
    let typ = typ::parse_typ_expr(p);
    p.in_external = was_in_external;

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

    let loc = p.mk_loc_to_prev_end(&loc_start);
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
        pcd_loc: p.mk_loc_to_prev_end(&start_pos),
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
        pext_loc: p.mk_loc_to_prev_end(&start_pos),
        pext_attributes: all_attrs,
    }
}

/// Parse a module definition.
/// `module_start` should be the position BEFORE 'module' was consumed.
fn parse_module_definition(p: &mut Parser<'_>, module_start: Position, attrs: Attributes) -> Option<StructureItemDesc> {
    // Check for module type
    if p.token == Token::Typ {
        p.next();
        let mtd = parse_module_type_declaration(p, attrs, true);
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
                let loc = p.mk_loc_current();
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
            let loc = p.mk_loc_spanning(mod_type.pmty_loc, mod_expr.pmod_loc);
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
            pmb_loc: p.mk_loc_to_prev_end(&binding_start),
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
/// Parse a module declaration.
/// If `start_pos` is Some, use it for the location (for recursive modules where it includes 'module rec' or 'and').
/// If `start_pos` is None, capture it at the current position (for non-recursive modules where loc starts at name).
fn parse_module_declaration(p: &mut Parser<'_>, outer_attrs: Attributes, start_pos: Option<Position>) -> ModuleDeclaration {
    let start_pos = start_pos.unwrap_or_else(|| p.start_pos.clone());
    let inner_attrs = parse_attributes(p);
    // Merge outer (signature item level) attrs with inner (name level) attrs
    let attrs = [outer_attrs, inner_attrs].concat();

    let name = match &p.token {
        Token::Uident(n) => {
            let n = n.clone();
            let loc = p.mk_loc_current();
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
            // OCaml uses a ghost location for Pmty_alias in module declarations
            let lid = parse_module_long_ident(p);
            ModuleType {
                pmty_desc: ModuleTypeDesc::Pmty_alias(lid),
                pmty_loc: Location::none(),
                pmty_attributes: vec![],
            }
        }
    } else {
        p.expect(Token::Colon);
        parse_module_type(p)
    };

    let loc = p.mk_loc_to_prev_end(&start_pos);
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
/// Parse recursive module declarations (for signature).
/// `start_pos` is the position before `module rec`, captured by the caller.
fn parse_rec_module_declarations(p: &mut Parser<'_>, outer_attrs: Attributes, start_pos: Position) -> Vec<ModuleDeclaration> {
    let mut decls = vec![];

    // Parse first declaration (gets the outer attrs and start_pos from caller)
    // For recursive modules, location includes 'module rec'
    decls.push(parse_module_declaration(p, outer_attrs, Some(start_pos)));

    // Parse additional declarations with `and`
    // Uses parse_attributes_and_binding which backtracks if attrs aren't followed by `and`
    loop {
        // Capture start_pos BEFORE consuming `and` (like OCaml)
        let and_start_pos = p.start_pos.clone();
        let attrs = parse_attributes_and_binding(p);
        if p.token == Token::And {
            p.next();
            // For 'and' declarations, location includes 'and'
            decls.push(parse_module_declaration(p, attrs, Some(and_start_pos)));
        } else {
            break;
        }
    }

    decls
}

/// Parse a module type declaration.
/// `in_structure` indicates whether this is in a structure (.res) or signature (.resi).
/// In structure context, OCaml passes ~loc, in signature context it doesn't (uses Location.none).
fn parse_module_type_declaration(p: &mut Parser<'_>, outer_attrs: Attributes, in_structure: bool) -> ModuleTypeDeclaration {
    let name_start = p.start_pos.clone();
    let inner_attrs = parse_attributes(p);
    // Merge outer (signature item level) attrs with inner (name level) attrs
    let attrs = [outer_attrs, inner_attrs].concat();

    let name = match &p.token {
        Token::Uident(n) | Token::Lident(n) => {
            let n = n.clone();
            let loc = p.mk_loc_current();
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

    // In structure context (parse_module_type_impl), OCaml passes ~loc:(mk_loc name_start p.prev_end_pos)
    // In signature context (parse_module_type_declaration), OCaml doesn't pass ~loc, uses Location.none
    let loc = if in_structure {
        p.mk_loc_to_prev_end(&name_start)
    } else {
        Location::none()
    };

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
    let name_loc = p.mk_loc_to_prev_end(&start_pos);

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

            // OCaml: parse_newline_or_semicolon_structure consumes the semicolon
            // BEFORE computing the location, so structure item location includes the semicolon
            p.optional(&Token::Semicolon);
            let loc = p.mk_loc_to_prev_end(&start_pos);

            items.push(StructureItem {
                pstr_desc: StructureItemDesc::Pstr_eval(expr, vec![]),
                pstr_loc: loc,
            });
        }

        // For non-expression items, check for more items
        // (Expression items already consumed the semicolon above)

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
        with_loc(id, p.mk_loc_to_prev_end(&id_start))
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
