//! ReScript AST printer - Doc-based implementation.
//!
//! This is a 1:1 port of OCaml's `res_printer.ml`, using Doc structures
//! for pretty-printing instead of direct string output.

use crate::location::Location as FullLocation;
use crate::parse_arena::{LocIdx, ParseArena};
use crate::parser::ast::*;
use crate::parser::comment::Comment;
use crate::parser::comment_table::{walk_structure, CommentTable};
use crate::parser::doc::Doc;
use crate::parser::longident::Longident;
use crate::parser::token::Token;
use std::collections::HashMap;

// ============================================================================
// Printer State
// ============================================================================

/// Printer state for tracking custom layout depth.
///
/// This is used to limit the depth of nested custom layouts to avoid
/// exponential blowup in layout computation.
#[derive(Debug, Clone)]
pub struct PrinterState {
    /// Current depth of nested custom layouts.
    custom_layout: i32,
}

impl PrinterState {
    /// Maximum depth of nested custom layouts before forcing breaks.
    const CUSTOM_LAYOUT_THRESHOLD: i32 = 2;

    /// Create a new printer state.
    pub fn init() -> Self {
        Self { custom_layout: 0 }
    }

    /// Create a new state with incremented custom layout depth.
    pub fn next_custom_layout(&self) -> Self {
        Self {
            custom_layout: self.custom_layout + 1,
        }
    }

    /// Check if we should break callbacks due to custom layout depth.
    pub fn should_break_callback(&self) -> bool {
        self.custom_layout > Self::CUSTOM_LAYOUT_THRESHOLD
    }
}

impl Default for PrinterState {
    fn default() -> Self {
        Self::init()
    }
}

// ============================================================================
// Identifier Classification
// ============================================================================

/// Classification of identifier style for printing.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IdentifierStyle {
    /// Uppercase exotic identifier (starts with backslash)
    UppercaseExoticIdent,
    /// Exotic identifier (needs escaping)
    ExoticIdent,
    /// Normal identifier (can be printed as-is)
    NormalIdent,
}

/// Classify an identifier to determine if it needs escaping.
///
/// # Arguments
/// * `txt` - The identifier text
/// * `allow_uident` - Whether to allow uppercase identifiers
/// * `allow_hyphen` - Whether to allow hyphens (for JSX tags)
pub fn classify_ident_content(txt: &str, allow_uident: bool, allow_hyphen: bool) -> IdentifierStyle {
    // Keywords always need escaping
    if Token::is_keyword_txt(txt) {
        return IdentifierStyle::ExoticIdent;
    }

    let bytes = txt.as_bytes();
    let len = bytes.len();

    if len == 0 {
        return IdentifierStyle::ExoticIdent;
    }

    // Check first character
    match bytes[0] {
        b'\\' => return IdentifierStyle::UppercaseExoticIdent,
        b'A'..=b'Z' if !allow_uident => return IdentifierStyle::ExoticIdent,
        b'a'..=b'z' | b'_' | b'A'..=b'Z' => {}
        _ => return IdentifierStyle::ExoticIdent,
    }

    // Check remaining characters
    for &c in &bytes[1..] {
        match c {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'\'' | b'_' => {}
            b'-' if allow_hyphen => {}
            _ => return IdentifierStyle::ExoticIdent,
        }
    }

    IdentifierStyle::NormalIdent
}

/// Print an identifier, escaping if necessary.
///
/// # Arguments
/// * `txt` - The identifier text
/// * `allow_uident` - Whether to allow uppercase identifiers
/// * `allow_hyphen` - Whether to allow hyphens
pub fn print_ident_like(txt: &str, allow_uident: bool, allow_hyphen: bool) -> Doc {
    match classify_ident_content(txt, allow_uident, allow_hyphen) {
        IdentifierStyle::ExoticIdent => {
            Doc::concat(vec![Doc::text("\\\""), Doc::text(txt), Doc::text("\"")])
        }
        IdentifierStyle::UppercaseExoticIdent | IdentifierStyle::NormalIdent => Doc::text(txt),
    }
}

/// Print an identifier with default settings (no uppercase, no hyphen).
pub fn print_ident(txt: &str) -> Doc {
    print_ident_like(txt, false, false)
}

// ============================================================================
// Longident Printing
// ============================================================================

/// Helper to flatten a longident into a list of docs.
fn print_longident_aux(arena: &crate::parse_arena::ParseArena, lid: &Longident, accu: &mut Vec<Doc>) {
    match lid {
        Longident::Lident(s_idx) => {
            let s = arena.get_string(*s_idx);
            accu.push(Doc::text(s.to_string()));
        }
        Longident::Ldot(inner, s_idx) => {
            print_longident_aux(arena, inner, accu);
            let s = arena.get_string(*s_idx);
            accu.push(Doc::text(s.to_string()));
        }
        Longident::Lapply(lid1, lid2) => {
            let mut docs1 = Vec::new();
            print_longident_aux(arena, lid1, &mut docs1);
            let d1 = Doc::join(Doc::dot(), docs1);

            let mut docs2 = Vec::new();
            print_longident_aux(arena, lid2, &mut docs2);
            let d2 = Doc::join(Doc::dot(), docs2);

            accu.push(Doc::concat(vec![d1, Doc::lparen(), d2, Doc::rparen()]));
        }
    }
}

/// Print a longident without escaping.
pub fn print_longident(arena: &crate::parse_arena::ParseArena, lid: &Longident) -> Doc {
    match lid {
        Longident::Lident(txt_idx) => {
            let txt = arena.get_string(*txt_idx);
            Doc::text(txt.to_string())
        }
        _ => {
            let mut docs = Vec::new();
            print_longident_aux(arena, lid, &mut docs);
            Doc::join(Doc::dot(), docs)
        }
    }
}

/// Print a longident with escaping for exotic identifiers.
pub fn print_lident(arena: &crate::parse_arena::ParseArena, lid: &Longident) -> Doc {
    match lid {
        Longident::Lident(txt_idx) => {
            let txt = arena.get_string(*txt_idx);
            print_ident(txt)
        }
        Longident::Ldot(path, txt_idx) => {
            // Flatten the path
            fn flat_lid(arena: &crate::parse_arena::ParseArena, lid: &Longident) -> Option<Vec<String>> {
                match lid {
                    Longident::Lident(txt_idx) => {
                        let txt = arena.get_string(*txt_idx);
                        Some(vec![txt.to_string()])
                    }
                    Longident::Ldot(inner, txt_idx) => {
                        let mut result = flat_lid(arena, inner)?;
                        let txt = arena.get_string(*txt_idx);
                        result.push(txt.to_string());
                        Some(result)
                    }
                    Longident::Lapply(_, _) => None,
                }
            }

            match flat_lid(arena, path) {
                Some(txts) => {
                    let mut docs: Vec<Doc> = txts.into_iter().map(|t| Doc::text(t)).collect();
                    let txt = arena.get_string(*txt_idx);
                    docs.push(print_ident(txt));
                    Doc::join(Doc::dot(), docs)
                }
                None => Doc::text("printLident: Longident.Lapply is not supported"),
            }
        }
        Longident::Lapply(_, _) => Doc::text("printLident: Longident.Lapply is not supported"),
    }
}

// ============================================================================
// Comment Printing
// ============================================================================

/// Trim leading/trailing spaces from comment text.
fn trim_spaces(s: &str) -> &str {
    s.trim()
}

/// Print multiline comment content with proper formatting.
fn print_multiline_comment_content(txt: &str) -> Doc {
    let lines: Vec<&str> = txt.split('\n').collect();

    match lines.len() {
        0 => Doc::text("/* */"),
        1 => {
            let trimmed = trim_spaces(lines[0]);
            Doc::concat(vec![
                Doc::text("/* "),
                Doc::text(trimmed),
                Doc::text(" */"),
            ])
        }
        _ => {
            let first_line = trim_spaces(lines[0]);
            let mut docs = vec![Doc::text("/*")];

            // Add first line if not empty
            if !first_line.is_empty() && first_line != "*" {
                docs.push(Doc::space());
            }

            docs.push(Doc::hard_line());
            docs.push(Doc::text(first_line));

            // Process remaining lines, indenting lines that start with *
            for line in &lines[1..] {
                let trimmed = line.trim();
                docs.push(Doc::hard_line());
                if !trimmed.is_empty() && trimmed.starts_with('*') {
                    docs.push(Doc::text(format!(" {}", trimmed)));
                } else {
                    docs.push(Doc::text(trimmed));
                }
            }

            // Handle trailing space
            let len = txt.len();
            if len > 0 && txt.ends_with(' ') {
                docs.push(Doc::space());
            }

            docs.push(Doc::text("*/"));
            Doc::concat(docs)
        }
    }
}

/// Print a leading comment, with optional reference to next comment for spacing.
fn print_leading_comment(comment: &Comment, next_comment: Option<&Comment>) -> Doc {
    let single_line = comment.is_single_line();
    let content = if single_line {
        Doc::text(format!("//{}", comment.txt()))
    } else {
        print_multiline_comment_content(comment.txt())
    };

    let separator = {
        let mut parts = Vec::new();

        if single_line {
            parts.push(Doc::hard_line());
            parts.push(Doc::break_parent());
        }

        if let Some(next) = next_comment {
            let next_loc = next.loc();
            let curr_loc = comment.loc();
            let diff = next_loc.loc_start.line as i32 - curr_loc.loc_end.line as i32;
            let next_single_line = next.is_single_line();

            if single_line && next_single_line {
                if diff > 1 {
                    parts.push(Doc::hard_line());
                }
            } else if single_line && !next_single_line {
                if diff > 1 {
                    parts.push(Doc::hard_line());
                }
            } else if diff > 1 {
                parts.push(Doc::hard_line());
                parts.push(Doc::hard_line());
            } else if diff == 1 {
                parts.push(Doc::hard_line());
            } else {
                parts.push(Doc::space());
            }
        }

        Doc::concat(parts)
    };

    Doc::concat(vec![content, separator])
}

/// Print a trailing comment.
fn print_trailing_comment(prev_loc: &FullLocation, node_loc: &FullLocation, comment: &Comment) -> Doc {
    let single_line = comment.is_single_line();
    let content = if single_line {
        Doc::text(format!("//{}", comment.txt()))
    } else {
        print_multiline_comment_content(comment.txt())
    };

    let cmt_start = &comment.loc().loc_start;
    let diff = cmt_start.line as i32 - prev_loc.loc_end.line as i32;
    let is_below = comment.loc().loc_start.line > node_loc.loc_end.line;

    if diff > 0 || is_below {
        Doc::concat(vec![
            Doc::break_parent(),
            Doc::line_suffix(Doc::concat(vec![
                Doc::hard_line(),
                if diff > 1 { Doc::hard_line() } else { Doc::nil() },
                content,
            ])),
        ])
    } else if !single_line {
        Doc::concat(vec![Doc::space(), content])
    } else {
        Doc::line_suffix(Doc::concat(vec![Doc::space(), content]))
    }
}

/// Print leading comments for a node.
fn print_leading_comments(
    node: Doc,
    leading: &mut HashMap<LocIdx, Vec<Comment>>,
    loc: LocIdx,
    full_loc: &FullLocation,
) -> Doc {
    match leading.remove(&loc) {
        None => node,
        Some(comments) if comments.is_empty() => node,
        Some(comments) => {
            let mut acc = Vec::new();
            let len = comments.len();

            for (i, comment) in comments.iter().enumerate() {
                let next_comment = if i + 1 < len {
                    Some(&comments[i + 1])
                } else {
                    None
                };
                acc.push(print_leading_comment(comment, next_comment));
            }

            // Calculate separator based on last comment
            if let Some(last_comment) = comments.last() {
                let diff =
                    full_loc.loc_start.line as i32 - last_comment.loc().loc_end.line as i32;
                let separator = if last_comment.is_single_line() {
                    if diff > 1 {
                        Doc::hard_line()
                    } else {
                        Doc::nil()
                    }
                } else if diff == 0 {
                    Doc::space()
                } else if diff > 1 {
                    Doc::concat(vec![Doc::hard_line(), Doc::hard_line()])
                } else {
                    Doc::hard_line()
                };

                acc.push(separator);
            }

            acc.push(node);
            Doc::group(Doc::concat(acc))
        }
    }
}

/// Print trailing comments for a node.
fn print_trailing_comments(
    node: Doc,
    trailing: &mut HashMap<LocIdx, Vec<Comment>>,
    loc: LocIdx,
    full_loc: &FullLocation,
) -> Doc {
    match trailing.remove(&loc) {
        None => node,
        Some(comments) if comments.is_empty() => node,
        Some(comments) => {
            let mut acc = Vec::new();
            let mut prev_loc = full_loc.clone();

            for comment in &comments {
                acc.push(print_trailing_comment(&prev_loc, full_loc, comment));
                prev_loc = comment.loc().clone();
            }

            Doc::concat(vec![node, Doc::concat(acc)])
        }
    }
}

/// Print a node with its leading and trailing comments.
pub fn print_comments(doc: Doc, cmt_tbl: &mut CommentTable, loc: LocIdx, arena: &ParseArena) -> Doc {
    let full_loc = arena.to_location(loc);
    let doc_with_leading = print_leading_comments(doc, &mut cmt_tbl.leading, loc, &full_loc);
    print_trailing_comments(doc_with_leading, &mut cmt_tbl.trailing, loc, &full_loc)
}

/// Get the first leading comment for a location, if any.
pub fn get_first_leading_comment<'a>(cmt_tbl: &'a CommentTable, loc: LocIdx) -> Option<&'a Comment> {
    cmt_tbl.leading.get(&loc).and_then(|comments| comments.first())
}

// ============================================================================
// List Printing
// ============================================================================

/// Print a list of nodes with proper comment handling and line breaks.
///
/// This function handles:
/// - Printing each node with its comments
/// - Adding appropriate separators (single or double hard lines)
/// - Creating a breakable group with optional forced breaks
pub fn print_list<'a, T, F, G>(
    get_loc: F,
    nodes: &'a [T],
    print: G,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
    force_break: bool,
) -> Doc
where
    F: Fn(&'a T) -> LocIdx,
    G: Fn(&'a T, &mut CommentTable) -> Doc,
{
    if nodes.is_empty() {
        return Doc::nil();
    }

    let mut docs = Vec::with_capacity(nodes.len() * 2);
    let first = &nodes[0];
    let first_loc = get_loc(first);
    let first_doc = {
        let doc = print(first, cmt_tbl);
        print_comments(doc, cmt_tbl, first_loc, arena)
    };
    docs.push(first_doc);

    let mut prev_loc = first_loc;

    for node in &nodes[1..] {
        let loc = get_loc(node);

        // Determine separator based on line distance
        let start_pos = match get_first_leading_comment(cmt_tbl, loc) {
            Some(comment) => comment.loc().loc_start.clone(),
            None => arena.loc_start(loc).clone(),
        };

        let prev_end = arena.loc_end(prev_loc);
        let sep = if start_pos.line as i32 - prev_end.line as i32 > 1 {
            Doc::concat(vec![Doc::hard_line(), Doc::hard_line()])
        } else {
            Doc::hard_line()
        };

        docs.push(sep);

        let doc = print(node, cmt_tbl);
        let doc = print_comments(doc, cmt_tbl, loc, arena);
        docs.push(doc);

        prev_loc = loc;
    }

    // Determine if we need to force break based on span
    let last_loc = get_loc(&nodes[nodes.len() - 1]);
    let should_break = force_break || arena.loc_start(first_loc).line != arena.loc_end(last_loc).line;

    Doc::breakable_group(Doc::concat(docs), should_break)
}

/// Print a list of nodes with index, with proper comment handling.
///
/// Similar to print_list but the print function also receives the index.
pub fn print_listi<'a, T, F, G>(
    get_loc: F,
    nodes: &'a [T],
    print: G,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
    ignore_empty_lines: bool,
    force_break: bool,
) -> Doc
where
    F: Fn(&'a T) -> LocIdx,
    G: Fn(&'a T, &mut CommentTable, usize) -> Doc,
{
    if nodes.is_empty() {
        return Doc::nil();
    }

    let mut docs = Vec::with_capacity(nodes.len() * 2);
    let first = &nodes[0];
    let first_loc = get_loc(first);
    let first_doc = {
        let doc = print(first, cmt_tbl, 0);
        print_comments(doc, cmt_tbl, first_loc, arena)
    };
    docs.push(first_doc);

    let mut prev_loc = first_loc;

    for (i, node) in nodes[1..].iter().enumerate() {
        let loc = get_loc(node);

        // Determine separator based on line distance
        let start_pos = match get_first_leading_comment(cmt_tbl, loc) {
            Some(comment) => comment.loc().loc_start.clone(),
            None => arena.loc_start(loc).clone(),
        };

        let prev_end = arena.loc_end(prev_loc);
        let sep = if start_pos.line as i32 - prev_end.line as i32 > 1 && !ignore_empty_lines
        {
            Doc::concat(vec![Doc::hard_line(), Doc::hard_line()])
        } else {
            Doc::line()
        };

        docs.push(sep);

        let doc = print(node, cmt_tbl, i + 1);
        let doc = print_comments(doc, cmt_tbl, loc, arena);
        docs.push(doc);

        prev_loc = loc;
    }

    // Determine if we need to force break based on span
    let last_loc = get_loc(&nodes[nodes.len() - 1]);
    let first_start = arena.loc_start(first_loc);
    let last_end = arena.loc_end(last_loc);
    let should_break = force_break || first_start.line != last_end.line;

    Doc::breakable_group(Doc::concat(docs), should_break)
}

// ============================================================================
// Comments Inside Empty Constructs
// ============================================================================

/// Print comments inside an empty construct (like empty block {}).
pub fn print_comments_inside(cmt_tbl: &mut CommentTable, loc: LocIdx, arena: &ParseArena) -> Doc {
    let force_break = arena.loc_start(loc).line != arena.loc_end(loc).line;

    match cmt_tbl.inside.remove(&loc) {
        None => Doc::nil(),
        Some(comments) if comments.is_empty() => Doc::nil(),
        Some(comments) => {
            let mut acc = Vec::new();

            for (i, comment) in comments.iter().enumerate() {
                let single_line = comment.is_single_line();
                let txt = comment.txt();
                let cmt_doc = if single_line {
                    Doc::text(format!("//{}", txt))
                } else {
                    print_multiline_comment_content(txt)
                };

                if i < comments.len() - 1 {
                    acc.push(cmt_doc);
                    acc.push(Doc::line());
                } else {
                    // Last comment
                    acc.push(Doc::soft_line());
                    acc.push(cmt_doc);
                }
            }

            let cmts_doc = Doc::concat(acc);
            let indented = Doc::indent(cmts_doc.clone());

            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::if_breaks(indented, cmts_doc),
                    Doc::soft_line(),
                ]),
                force_break,
            )
        }
    }
}

/// Print comments inside an empty file.
pub fn print_comments_inside_file(cmt_tbl: &mut CommentTable, arena: &ParseArena) -> Doc {
    match cmt_tbl.inside.remove(&Location::none()) {
        None => Doc::nil(),
        Some(comments) if comments.is_empty() => Doc::nil(),
        Some(comments) => {
            let mut acc = Vec::new();
            let len = comments.len();

            for (i, comment) in comments.iter().enumerate() {
                let next_comment = if i + 1 < len {
                    Some(&comments[i + 1])
                } else {
                    None
                };
                acc.push(print_leading_comment(comment, next_comment));
            }

            Doc::group(Doc::concat(acc))
        }
    }
}

// ============================================================================
// Polyvariant Printing
// ============================================================================

/// Check if a string is a valid numeric polyvariant number.
fn is_valid_numeric_polyvar_number(s: &str) -> bool {
    let bytes = s.as_bytes();
    let len = bytes.len();

    if len == 0 {
        return false;
    }

    let first = bytes[0];
    if first > b'9' {
        return false;
    }

    if len > 1 {
        // First digit must be > '0' (no leading zeros)
        if first <= b'0' {
            return false;
        }
        // Rest must be digits
        bytes[1..].iter().all(|&c| c >= b'0' && c <= b'9')
    } else {
        first >= b'0'
    }
}

/// Print a polyvariant identifier with proper escaping.
/// This does NOT include the leading `#` - call sites should add it.
pub fn print_poly_var_ident(txt: &str) -> Doc {
    // Numeric poly-vars don't need quotes
    if is_valid_numeric_polyvar_number(txt) {
        return Doc::text(txt);
    }

    match classify_ident_content(txt, true, false) {
        IdentifierStyle::UppercaseExoticIdent => {
            // UppercaseExoticIdent follows the \"..." format
            // but the first char is \ which we need to skip
            if txt.starts_with('\\') && txt.len() > 1 {
                Doc::concat(vec![Doc::text("\""), Doc::text(&txt[1..]), Doc::text("\"")])
            } else {
                Doc::concat(vec![Doc::text("\""), Doc::text(txt), Doc::text("\"")])
            }
        }
        IdentifierStyle::ExoticIdent => {
            Doc::concat(vec![Doc::text("\""), Doc::text(txt), Doc::text("\"")])
        }
        IdentifierStyle::NormalIdent => Doc::text(txt),
    }
}

// ============================================================================
// String Printing
// ============================================================================

/// Print a string location (identifier with location).
pub fn print_string_loc(sloc: &StringLoc, cmt_tbl: &mut CommentTable, arena: &ParseArena) -> Doc {
    let doc = print_ident(&sloc.txt);
    print_comments(doc, cmt_tbl, sloc.loc, arena)
}

/// Print a longident location.
/// Print a longident location with escaping (for value identifiers).
pub fn print_longident_loc(lid: &crate::parse_arena::Located<crate::parse_arena::LidentIdx>, cmt_tbl: &mut CommentTable, arena: &ParseArena) -> Doc {
    let doc = print_lident(arena, arena.get_longident(lid.txt));
    print_comments(doc, cmt_tbl, lid.loc, arena)
}

/// Print a longident location without escaping (for module paths).
pub fn print_longident_location(lid: &crate::parse_arena::Located<crate::parse_arena::LidentIdx>, cmt_tbl: &mut CommentTable, arena: &ParseArena) -> Doc {
    let doc = print_longident(arena, arena.get_longident(lid.txt));
    print_comments(doc, cmt_tbl, lid.loc, arena)
}

// ============================================================================
// Parentheses and Braces Helpers
// ============================================================================

use crate::parser::parens::{self, ParenKind};
use crate::parser::parsetree_viewer;

/// Add parentheses around a Doc.
pub fn add_parens(doc: Doc) -> Doc {
    Doc::group(Doc::concat(vec![
        Doc::lparen(),
        Doc::indent(Doc::concat(vec![Doc::soft_line(), doc])),
        Doc::soft_line(),
        Doc::rparen(),
    ]))
}

/// Print an expression with braces at the given location.
pub fn print_braces(doc: Doc, expr: &Expression, braces_loc: LocIdx, arena: &ParseArena) -> Doc {
    let over_multiple_lines =
        arena.loc_start(braces_loc).line != arena.loc_end(braces_loc).line;

    // These expression types already have braces when printed, so just return doc as-is
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_letmodule(_, _, _)
        | ExpressionDesc::Pexp_letexception(_, _)
        | ExpressionDesc::Pexp_let(_, _, _)
        | ExpressionDesc::Pexp_open(_, _, _)
        | ExpressionDesc::Pexp_sequence(_, _) => {
            return doc;
        }
        _ => {}
    }

    // For other expressions, wrap in braces
    let inner_doc = if parens::braced_expr(expr) {
        add_parens(doc)
    } else {
        doc
    };

    Doc::breakable_group(
        Doc::concat(vec![
            Doc::lbrace(),
            Doc::indent(Doc::concat(vec![Doc::soft_line(), inner_doc])),
            Doc::soft_line(),
            Doc::rbrace(),
        ]),
        over_multiple_lines,
    )
}

// ============================================================================
// Constant Printing
// ============================================================================

/// Print string contents with proper escaping.
fn print_string_contents(txt: &str) -> Doc {
    // For now, just return the text. A complete implementation would
    // handle escape sequences properly.
    Doc::text(txt)
}

/// Print a constant value.
pub fn print_constant(template_literal: bool, c: &Constant) -> Doc {
    match c {
        Constant::Integer(s, suffix) => match suffix {
            Some(c) => Doc::text(format!("{}{}", s, c)),
            None => Doc::text(s.clone()),
        },
        Constant::String(txt, None) => Doc::concat(vec![
            Doc::text("\""),
            print_string_contents(txt),
            Doc::text("\""),
        ]),
        Constant::String(txt, Some(prefix)) => {
            if prefix == "INTERNAL_RES_CHAR_CONTENTS" {
                Doc::concat(vec![Doc::text("'"), Doc::text(txt.clone()), Doc::text("'")])
            } else {
                let (lquote, rquote) = if template_literal {
                    ("`", "`")
                } else {
                    ("\"", "\"")
                };
                Doc::concat(vec![
                    if prefix == "js" {
                        Doc::nil()
                    } else {
                        Doc::text(prefix.clone())
                    },
                    Doc::text(lquote),
                    print_string_contents(txt),
                    Doc::text(rquote),
                ])
            }
        }
        Constant::Float(s, _) => Doc::text(s.clone()),
        Constant::Char(c) => {
            let ch = char::from_u32(*c as u32).unwrap_or('?');
            let s = match ch {
                '\'' => "\\'".to_string(),
                '\\' => "\\\\".to_string(),
                '\n' => "\\n".to_string(),
                '\t' => "\\t".to_string(),
                '\r' => "\\r".to_string(),
                '\x08' => "\\b".to_string(),
                ' '..='~' => ch.to_string(),
                _ => format!("\\u{{{:x}}}", c),
            };
            Doc::text(format!("'{}'", s))
        }
    }
}

// ============================================================================
// Expression Printing
// ============================================================================

/// Print an expression with its comments.
pub fn print_expression_with_comments(
    state: &PrinterState,
    expr: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let doc = print_expression(state, expr, cmt_tbl, arena);
    print_comments(doc, cmt_tbl, expr.pexp_loc, arena)
}

/// Print an expression.
pub fn print_expression(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let printed_expression = match &e.pexp_desc {
        // (__x) => f(a, __x, c) -----> f(a, _, c)
        ExpressionDesc::Pexp_fun {
            arg_label: ArgLabel::Nolabel,
            default: None,
            lhs,
            rhs,
            ..
        } if matches!(&lhs.ppat_desc, PatternDesc::Ppat_var(name) if name.txt == "__x")
            && matches!(&rhs.pexp_desc, ExpressionDesc::Pexp_apply { .. }) =>
        {
            match parsetree_viewer::rewrite_underscore_apply(e) {
                Some(rewritten) => print_expression_with_comments(state, &rewritten, cmt_tbl, arena),
                None => print_expression_with_comments(state, e, cmt_tbl, arena),
            }
        }
        // Function expressions
        ExpressionDesc::Pexp_fun { .. } | ExpressionDesc::Pexp_newtype(_, _) => {
            print_arrow_expression(state, e, cmt_tbl, arena)
        }
        // Constants
        ExpressionDesc::Pexp_constant(c) => {
            print_constant(parsetree_viewer::is_template_literal(e), c)
        }
        // Unit constructor: ()
        ExpressionDesc::Pexp_construct(lid, None)
            if arena.is_lident(lid.txt, "()") =>
        {
            Doc::text("()")
        }
        // Empty list: []
        ExpressionDesc::Pexp_construct(lid, None)
            if arena.is_lident(lid.txt, "[]") =>
        {
            Doc::concat(vec![
                Doc::text("list{"),
                print_comments_inside(cmt_tbl, e.pexp_loc, arena),
                Doc::rbrace(),
            ])
        }
        // List constructor: ::
        ExpressionDesc::Pexp_construct(lid, _)
            if arena.is_lident(lid.txt, "::") =>
        {
            print_list_expression(state, e, cmt_tbl, arena)
        }
        // Other constructors
        ExpressionDesc::Pexp_construct(longident_loc, args) => {
            let constr = print_longident_location(longident_loc, cmt_tbl, arena);
            let args_doc = match args {
                None => Doc::nil(),
                Some(arg) => match &arg.pexp_desc {
                    ExpressionDesc::Pexp_construct(lid, None)
                        if arena.is_lident(lid.txt, "()") =>
                    {
                        Doc::text("()")
                    }
                    // Some((1, 2))
                    ExpressionDesc::Pexp_tuple(exprs) if exprs.len() == 1 => {
                        if let ExpressionDesc::Pexp_tuple(_) = &exprs[0].pexp_desc {
                            let doc =
                                print_expression_with_comments(state, &exprs[0], cmt_tbl, arena);
                            let doc = match parens::expr(arena,&exprs[0]) {
                                ParenKind::Parenthesized => add_parens(doc),
                                ParenKind::Braced(loc) => print_braces(doc, &exprs[0], loc, arena),
                                ParenKind::Nothing => doc,
                            };
                            Doc::concat(vec![Doc::lparen(), doc, Doc::rparen()])
                        } else {
                            print_constructor_args(state, args.as_deref(), cmt_tbl, arena)
                        }
                    }
                    ExpressionDesc::Pexp_tuple(exprs) => {
                        print_tuple_args(state, exprs, cmt_tbl, arena)
                    }
                    _ => {
                        let arg_doc = print_expression_with_comments(state, arg, cmt_tbl, arena);
                        let arg_doc = match parens::expr(arena,arg) {
                            ParenKind::Parenthesized => add_parens(arg_doc),
                            ParenKind::Braced(loc) => print_braces(arg_doc, arg, loc, arena),
                            ParenKind::Nothing => arg_doc,
                        };
                        let should_hug = parsetree_viewer::is_huggable_expression(arena,arg);
                        Doc::concat(vec![
                            Doc::lparen(),
                            if should_hug {
                                arg_doc
                            } else {
                                Doc::concat(vec![
                                    Doc::indent(Doc::concat(vec![Doc::soft_line(), arg_doc])),
                                    Doc::trailing_comma(),
                                    Doc::soft_line(),
                                ])
                            },
                            Doc::rparen(),
                        ])
                    }
                },
            };
            Doc::group(Doc::concat(vec![constr, args_doc]))
        }
        // Identifier
        ExpressionDesc::Pexp_ident(path) => print_lident_path(path, cmt_tbl, arena),
        // Tuple
        ExpressionDesc::Pexp_tuple(exprs) => {
            Doc::group(Doc::concat(vec![
                Doc::lparen(),
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(
                        Doc::concat(vec![Doc::text(","), Doc::line()]),
                        exprs
                            .iter()
                            .map(|expr| {
                                let doc =
                                    print_expression_with_comments(state, expr, cmt_tbl, arena);
                                match parens::expr(arena,expr) {
                                    ParenKind::Parenthesized => add_parens(doc),
                                    ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                                    ParenKind::Nothing => doc,
                                }
                            })
                            .collect(),
                    ),
                ])),
                Doc::if_breaks(Doc::text(","), Doc::nil()),
                Doc::soft_line(),
                Doc::rparen(),
            ]))
        }
        // Empty array
        ExpressionDesc::Pexp_array(exprs) if exprs.is_empty() => {
            Doc::concat(vec![
                Doc::lbracket(),
                print_comments_inside(cmt_tbl, e.pexp_loc, arena),
                Doc::rbracket(),
            ])
        }
        // Array
        ExpressionDesc::Pexp_array(exprs) => {
            Doc::group(Doc::concat(vec![
                Doc::lbracket(),
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(
                        Doc::concat(vec![Doc::text(","), Doc::line()]),
                        exprs
                            .iter()
                            .map(|expr| {
                                let doc =
                                    print_expression_with_comments(state, expr, cmt_tbl, arena);
                                match parens::expr(arena,expr) {
                                    ParenKind::Parenthesized => add_parens(doc),
                                    ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                                    ParenKind::Nothing => doc,
                                }
                            })
                            .collect(),
                    ),
                ])),
                Doc::trailing_comma(),
                Doc::soft_line(),
                Doc::rbracket(),
            ]))
        }
        // Variant
        ExpressionDesc::Pexp_variant(label, args) => {
            let variant_name = Doc::concat(vec![
                Doc::text("#"),
                print_poly_var_ident(label),
            ]);
            let args_doc = match args {
                None => Doc::nil(),
                Some(arg) => match &arg.pexp_desc {
                    ExpressionDesc::Pexp_construct(lid, None)
                        if arena.is_lident(lid.txt, "()") =>
                    {
                        Doc::text("()")
                    }
                    ExpressionDesc::Pexp_tuple(exprs) => {
                        print_tuple_args(state, exprs, cmt_tbl, arena)
                    }
                    _ => {
                        let arg_doc = print_expression_with_comments(state, arg, cmt_tbl, arena);
                        let arg_doc = match parens::expr(arena,arg) {
                            ParenKind::Parenthesized => add_parens(arg_doc),
                            ParenKind::Braced(loc) => print_braces(arg_doc, arg, loc, arena),
                            ParenKind::Nothing => arg_doc,
                        };
                        let should_hug = parsetree_viewer::is_huggable_expression(arena,arg);
                        Doc::concat(vec![
                            Doc::lparen(),
                            if should_hug {
                                arg_doc
                            } else {
                                Doc::concat(vec![
                                    Doc::indent(Doc::concat(vec![Doc::soft_line(), arg_doc])),
                                    Doc::trailing_comma(),
                                    Doc::soft_line(),
                                ])
                            },
                            Doc::rparen(),
                        ])
                    }
                },
            };
            Doc::group(Doc::concat(vec![variant_name, args_doc]))
        }
        // Record expression
        ExpressionDesc::Pexp_record(fields, spread) => {
            print_record_expression(state, e.pexp_loc, spread.as_ref().map(|e| e.as_ref()), fields, cmt_tbl, arena)
        }
        // Field access: expr.field
        ExpressionDesc::Pexp_field(expr, longident_loc) => {
            let lhs = print_expression_with_comments(state, expr, cmt_tbl, arena);
            let lhs = match parens::field_expr(arena,expr) {
                ParenKind::Parenthesized => add_parens(lhs),
                ParenKind::Braced(loc) => print_braces(lhs, expr, loc, arena),
                ParenKind::Nothing => lhs,
            };
            let field = print_lident(arena, arena.get_longident(longident_loc.txt));
            Doc::concat(vec![lhs, Doc::dot(), field])
        }
        // Field set: expr.field = value
        ExpressionDesc::Pexp_setfield(expr, longident_loc, value) => {
            let lhs = {
                let expr_doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                let expr_doc = match parens::field_expr(arena,expr) {
                    ParenKind::Parenthesized => add_parens(expr_doc),
                    ParenKind::Braced(loc) => print_braces(expr_doc, expr, loc, arena),
                    ParenKind::Nothing => expr_doc,
                };
                let field = print_lident(arena, arena.get_longident(longident_loc.txt));
                Doc::concat(vec![expr_doc, Doc::dot(), field])
            };
            let rhs = {
                let doc = print_expression_with_comments(state, value, cmt_tbl, arena);
                match parens::set_field_expr_rhs(value) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, value, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };
            Doc::group(Doc::concat(vec![
                lhs,
                Doc::text(" ="),
                Doc::indent(Doc::concat(vec![Doc::line(), rhs])),
            ]))
        }
        // Ternary or if-then-else
        ExpressionDesc::Pexp_ifthenelse(_, _, _) if parsetree_viewer::is_ternary_expr(e) => {
            print_ternary_expression(state, e, cmt_tbl, arena)
        }
        ExpressionDesc::Pexp_ifthenelse(_, _, _) => {
            print_if_expression(state, e, cmt_tbl, arena)
        }
        // Sequence: expr1; expr2
        ExpressionDesc::Pexp_sequence(_, _) => {
            print_expression_block(state, true, e, cmt_tbl, arena)
        }
        // Let expression
        ExpressionDesc::Pexp_let(_, _, _) => {
            print_expression_block(state, true, e, cmt_tbl, arena)
        }
        // Array access: Array.get(arr, idx) -> arr[idx]
        ExpressionDesc::Pexp_apply { funct, args, .. }
            if parsetree_viewer::is_array_access(arena,e)
                && !parsetree_viewer::is_rewritten_underscore_apply_sugar(arena,&args[0].1) =>
        {
            let parent_expr = &args[0].1;
            let member_expr = &args[1].1;
            let parent_doc = print_expression_with_comments(state, parent_expr, cmt_tbl, arena);
            let member_doc = print_expression_with_comments(state, member_expr, cmt_tbl, arena);
            // Note: attributes are handled at the end of print_expression
            Doc::group(Doc::concat(vec![
                parent_doc,
                Doc::lbracket(),
                member_doc,
                Doc::rbracket(),
            ]))
        }
        // Array set: Array.set(arr, idx, value) -> arr[idx] = value
        ExpressionDesc::Pexp_apply { args, .. }
            if parsetree_viewer::is_array_set(arena,e) =>
        {
            let parent_expr = &args[0].1;
            let member_expr = &args[1].1;
            let target_expr = &args[2].1;
            let parent_doc = print_expression_with_comments(state, parent_expr, cmt_tbl, arena);
            let member_doc = print_expression_with_comments(state, member_expr, cmt_tbl, arena);
            let target_doc = print_expression_with_comments(state, target_expr, cmt_tbl, arena);
            let should_indent = parsetree_viewer::is_binary_expression(arena,target_expr);
            let rhs_doc = if should_indent {
                Doc::group(Doc::indent(Doc::concat(vec![Doc::line(), target_doc])))
            } else {
                Doc::concat(vec![Doc::space(), target_doc])
            };
            // Note: attributes are handled at the end of print_expression
            Doc::group(Doc::concat(vec![
                parent_doc,
                Doc::lbracket(),
                member_doc,
                Doc::rbracket(),
                Doc::text(" ="),
                rhs_doc,
            ]))
        }
        // Send-set: obj#prop = value -> obj["prop"] = value
        ExpressionDesc::Pexp_apply { funct, args, .. }
            if is_send_set_expr(arena, funct, args) =>
        {
            let lhs = &args[0].1;
            let rhs = &args[1].1;
            let rhs_doc = print_expression_with_comments(state, rhs, cmt_tbl, arena);
            let should_indent = !parsetree_viewer::is_braced_expr(rhs)
                && parsetree_viewer::is_binary_expression(arena,rhs);
            let rhs_doc = if should_indent {
                Doc::group(Doc::indent(Doc::concat(vec![Doc::line(), rhs_doc])))
            } else {
                Doc::concat(vec![Doc::space(), rhs_doc])
            };
            // Note: attributes are handled at the end of print_expression
            Doc::group(Doc::concat(vec![
                print_expression_with_comments(state, lhs, cmt_tbl, arena),
                Doc::text(" ="),
                rhs_doc,
            ]))
        }
        // Function application
        ExpressionDesc::Pexp_apply { funct, args, partial, .. } => {
            print_pexp_apply(state, e, funct, args, *partial, cmt_tbl, arena)
        }
        // Constraint with pack: module(M: S)
        ExpressionDesc::Pexp_constraint(inner, typ)
            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
        {
            if let ExpressionDesc::Pexp_pack(mod_expr) = &inner.pexp_desc {
                if let CoreTypeDesc::Ptyp_package(package_type) = &typ.ptyp_desc {
                    let mod_doc = print_mod_expr(state, mod_expr, cmt_tbl, arena);
                    // Don't print "module(...)" wrapper - we're already inside module()
                    let type_doc = print_comments(
                        print_package_type(state, package_type, false, cmt_tbl, arena),
                        cmt_tbl,
                        typ.ptyp_loc,
                        arena,
                    );
                    return Doc::group(Doc::concat(vec![
                        Doc::text("module("),
                        Doc::indent(Doc::concat(vec![Doc::soft_line(), mod_doc])),
                        Doc::text(": "),
                        type_doc,
                        Doc::soft_line(),
                        Doc::rparen(),
                    ]));
                }
            }
            Doc::nil()
        }
        // Constraint: expr: typ (parens added by caller when needed)
        ExpressionDesc::Pexp_constraint(expr, typ) => {
            let expr_doc = {
                let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                match parens::expr(arena,expr) {
                    parens::ParenKind::Parenthesized => add_parens(doc),
                    parens::ParenKind::Braced(braces) => print_braces(doc, expr, braces, arena),
                    parens::ParenKind::Nothing => doc,
                }
            };
            let typ_doc = print_typ_expr(state, typ, cmt_tbl, arena);
            Doc::concat(vec![expr_doc, Doc::text(": "), typ_doc])
        }
        // Coerce: (expr :> typ)
        ExpressionDesc::Pexp_coerce(expr, _, typ) => {
            let expr_doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
            let typ_doc = print_typ_expr(state, typ, cmt_tbl, arena);
            Doc::concat(vec![
                Doc::lparen(),
                expr_doc,
                Doc::text(" :> "),
                typ_doc,
                Doc::rparen(),
            ])
        }
        // Extension
        ExpressionDesc::Pexp_extension(ext) => {
            // Special case for %obj extension - print as JS object literal
            if let Some(obj_doc) = try_print_obj_extension(state, ext, e.pexp_loc, cmt_tbl, arena) {
                obj_doc
            } else if let Some(re_doc) = try_print_re_extension(ext) {
                re_doc
            } else {
                print_extension(state, ext, false, cmt_tbl, arena)
            }
        }
        // Match expression
        ExpressionDesc::Pexp_match(expr, cases) => {
            print_match_expression(state, expr, cases, cmt_tbl, arena)
        }
        // Try expression
        ExpressionDesc::Pexp_try(expr, cases) => {
            print_try_expression(state, expr, cases, cmt_tbl, arena)
        }
        // Assert
        ExpressionDesc::Pexp_assert(expr) => {
            let rhs = print_expression_with_comments(state, expr, cmt_tbl, arena);
            Doc::concat(vec![Doc::text("assert("), rhs, Doc::text(")")])
        }
        // Await
        ExpressionDesc::Pexp_await(expr) => {
            let rhs = print_expression_with_comments(state, expr, cmt_tbl, arena);
            let rhs = match parens::assert_or_await_expr_rhs(arena,true, expr) {
                ParenKind::Parenthesized => add_parens(rhs),
                ParenKind::Braced(loc) => print_braces(rhs, expr, loc, arena),
                ParenKind::Nothing => rhs,
            };
            Doc::concat(vec![Doc::text("await "), rhs])
        }
        // Open expression
        ExpressionDesc::Pexp_open(_, _, _) => {
            print_expression_block(state, true, e, cmt_tbl, arena)
        }
        // Let module
        ExpressionDesc::Pexp_letmodule(_, _, _) => {
            print_expression_block(state, true, e, cmt_tbl, arena)
        }
        // Let exception
        ExpressionDesc::Pexp_letexception(_, _) => {
            print_expression_block(state, true, e, cmt_tbl, arena)
        }
        // While loop
        ExpressionDesc::Pexp_while(cond, body) => {
            let cond_doc = print_expression_with_comments(state, cond, cmt_tbl, arena);
            // Check if condition is a block expression
            let cond_doc = if parsetree_viewer::is_block_expr(cond) {
                cond_doc
            } else {
                Doc::group(Doc::if_breaks(add_parens(cond_doc.clone()), cond_doc))
            };
            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::text("while "),
                    cond_doc,
                    Doc::space(),
                    print_expression_block(state, true, body, cmt_tbl, arena),
                ]),
                true, // force_break
            )
        }
        // For loop
        ExpressionDesc::Pexp_for(pat, start, finish, direction, body) => {
            let dir = match direction {
                DirectionFlag::Upto => " to ",
                DirectionFlag::Downto => " downto ",
            };
            // Handle parens for from_expr
            let start_doc = {
                let doc = print_expression_with_comments(state, start, cmt_tbl, arena);
                match parens::expr(arena, start) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, start, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };
            // Handle parens for to_expr
            let finish_doc = {
                let doc = print_expression_with_comments(state, finish, cmt_tbl, arena);
                match parens::expr(arena, finish) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, finish, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };
            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::text("for "),
                    print_pattern(state, pat, cmt_tbl, arena),
                    Doc::text(" in "),
                    start_doc,
                    Doc::text(dir),
                    finish_doc,
                    Doc::space(),
                    print_expression_block(state, true, body, cmt_tbl, arena),
                ]),
                true, // force_break
            )
        }
        // Pack: module(M)
        ExpressionDesc::Pexp_pack(mod_expr) => {
            Doc::concat(vec![
                Doc::text("module("),
                print_mod_expr(state, mod_expr, cmt_tbl, arena),
                Doc::text(")"),
            ])
        }
        // JSX element (placeholder - full implementation needed)
        ExpressionDesc::Pexp_jsx_element(_) => {
            Doc::text("<jsx />")  // Simplified placeholder
        }
        // Send (method call): expr#method -> expr["method"]
        ExpressionDesc::Pexp_send(parent_expr, label) => {
            let parent_doc = print_expression_with_comments(state, parent_expr, cmt_tbl, arena);
            // Apply parenthesization if needed (like unary_expr_operand)
            let parent_doc = match parens::unary_expr_operand(arena,parent_expr) {
                parens::ParenKind::Parenthesized => add_parens(parent_doc),
                parens::ParenKind::Braced(braces) => print_braces(parent_doc, parent_expr, braces, arena),
                parens::ParenKind::Nothing => parent_doc,
            };
            let member_doc = print_comments(Doc::text(&label.txt), cmt_tbl, label.loc, arena);
            let member = Doc::concat(vec![Doc::text("\""), member_doc, Doc::text("\"")]);
            Doc::group(Doc::concat(vec![
                parent_doc,
                Doc::lbracket(),
                member,
                Doc::rbracket(),
            ]))
        }
    };

    // Handle attributes on the expression
    let attrs = &e.pexp_attributes;
    if attrs.is_empty() {
        printed_expression
    } else {
        // Use Doc::line() separator so it can break when the expression breaks
        // Wrap in Doc::group to control breaking behavior
        let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
        Doc::group(Doc::concat(vec![attrs_doc, printed_expression]))
    }
}

// ============================================================================
// Expression Helper Functions
// ============================================================================

/// Print a longident path.
fn print_lident_path(path: &crate::parse_arena::Located<crate::parse_arena::LidentIdx>, cmt_tbl: &mut CommentTable, arena: &ParseArena) -> Doc {
    let doc = print_lident(arena, arena.get_longident(path.txt));
    print_comments(doc, cmt_tbl, path.loc, arena)
}

/// Print tuple arguments.
fn print_tuple_args(
    state: &PrinterState,
    exprs: &[Expression],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    Doc::concat(vec![
        Doc::lparen(),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(
                Doc::concat(vec![Doc::comma(), Doc::line()]),
                exprs
                    .iter()
                    .map(|expr| {
                        let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                        match parens::expr(arena,expr) {
                            ParenKind::Parenthesized => add_parens(doc),
                            ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                            ParenKind::Nothing => doc,
                        }
                    })
                    .collect(),
            ),
        ])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::rparen(),
    ])
}

/// Print constructor arguments (for pattern).
fn print_constructor_args(
    state: &PrinterState,
    arg: Option<&Expression>,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match arg {
        None => Doc::nil(),
        Some(arg) => {
            let arg_doc = print_expression_with_comments(state, arg, cmt_tbl, arena);
            let arg_doc = match parens::expr(arena,arg) {
                ParenKind::Parenthesized => add_parens(arg_doc),
                ParenKind::Braced(loc) => print_braces(arg_doc, arg, loc, arena),
                ParenKind::Nothing => arg_doc,
            };
            let should_hug = parsetree_viewer::is_huggable_expression(arena,arg);
            Doc::concat(vec![
                Doc::lparen(),
                if should_hug {
                    arg_doc
                } else {
                    Doc::concat(vec![
                        Doc::indent(Doc::concat(vec![Doc::soft_line(), arg_doc])),
                        Doc::trailing_comma(),
                        Doc::soft_line(),
                    ])
                },
                Doc::rparen(),
            ])
        }
    }
}

/// Print list expression (cons cells).
fn print_list_expression(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (expressions, spread) = parsetree_viewer::collect_list_expressions(arena,e);
    let spread_doc = match spread {
        Some(expr) => Doc::concat(vec![
            Doc::text(","),
            Doc::line(),
            Doc::dotdotdot(),
            {
                let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                match parens::expr(arena,expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                    ParenKind::Nothing => doc,
                }
            },
        ]),
        None => Doc::nil(),
    };
    Doc::group(Doc::concat(vec![
        Doc::text("list{"),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(
                Doc::concat(vec![Doc::text(","), Doc::line()]),
                expressions
                    .iter()
                    .map(|expr| {
                        let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                        match parens::expr(arena,expr) {
                            ParenKind::Parenthesized => add_parens(doc),
                            ParenKind::Braced(loc) => print_braces(doc, *expr, loc, arena),
                            ParenKind::Nothing => doc,
                        }
                    })
                    .collect(),
            ),
            spread_doc,
        ])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::rbrace(),
    ]))
}

/// Callback printing mode.
#[derive(Debug, Clone, Copy, PartialEq)]
enum InCallback {
    NoCallback,
    FitsOnOneLine,
    ArgumentsFitOnOneLine,
}

/// Print arrow expression (function).
fn print_arrow_expression(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (is_async, parameters, return_expr) = parsetree_viewer::fun_expr(e);
    let attrs_on_arrow = &e.pexp_attributes;

    // Extract type constraint if present
    let (return_expr, typ_constraint) = match &return_expr.pexp_desc {
        ExpressionDesc::Pexp_constraint(expr, typ) => (expr.as_ref(), Some(typ)),
        _ => (return_expr, None),
    };

    let has_constraint = typ_constraint.is_some();

    // Print parameters
    let parameters_doc = print_expr_fun_parameters(
        state,
        InCallback::NoCallback,
        is_async,
        has_constraint,
        &parameters,
        cmt_tbl,
        arena,
    );

    // Print return expression
    let return_expr_doc = {
        let opt_braces = parsetree_viewer::process_braces_attr(return_expr);
        let should_inline = match (&return_expr.pexp_desc, opt_braces) {
            (_, Some(_)) => true,
            (
                ExpressionDesc::Pexp_array(_)
                | ExpressionDesc::Pexp_tuple(_)
                | ExpressionDesc::Pexp_construct(_, Some(_))
                | ExpressionDesc::Pexp_record(_, _),
                _,
            ) => true,
            _ => false,
        };
        let should_indent = !matches!(
            &return_expr.pexp_desc,
            ExpressionDesc::Pexp_sequence(_, _)
                | ExpressionDesc::Pexp_let(_, _, _)
                | ExpressionDesc::Pexp_letmodule(_, _, _)
                | ExpressionDesc::Pexp_letexception(_, _)
                | ExpressionDesc::Pexp_open(_, _, _)
        );

        let return_doc = {
            let doc = print_expression_with_comments(state, return_expr, cmt_tbl, arena);
            match parens::expr(arena,return_expr) {
                ParenKind::Parenthesized => add_parens(doc),
                ParenKind::Braced(loc) => print_braces(doc, return_expr, loc, arena),
                ParenKind::Nothing => doc,
            }
        };

        if should_inline {
            Doc::concat(vec![Doc::space(), return_doc])
        } else {
            Doc::group(if should_indent {
                Doc::indent(Doc::concat(vec![Doc::line(), return_doc]))
            } else {
                Doc::concat(vec![Doc::space(), return_doc])
            })
        }
    };

    // Print type constraint
    let typ_constraint_doc = match typ_constraint {
        Some(typ) => {
            let typ_doc = print_typ_expr(state, typ, cmt_tbl, arena);
            let typ_doc = if parens::arrow_return_typ_expr(typ) {
                add_parens(typ_doc)
            } else {
                typ_doc
            };
            Doc::concat(vec![Doc::text(": "), typ_doc])
        }
        None => Doc::nil(),
    };

    let attrs = print_attributes(state, attrs_on_arrow, cmt_tbl, arena);

    Doc::group(Doc::concat(vec![
        attrs,
        parameters_doc,
        typ_constraint_doc,
        Doc::text(" =>"),
        return_expr_doc,
    ]))
}

/// Print function parameters.
fn print_expr_fun_parameters(
    state: &PrinterState,
    in_callback: InCallback,
    is_async: bool,
    has_constraint: bool,
    parameters: &[parsetree_viewer::FunParam<'_>],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    use parsetree_viewer::FunParam;

    // Handle special single parameter cases
    match parameters {
        // let f = _ => ()
        [FunParam::Parameter {
            attrs,
            label: ArgLabel::Nolabel,
            default_expr: None,
            pat,
        }] if attrs.is_empty() && matches!(&pat.ppat_desc, PatternDesc::Ppat_any) => {
            let doc = if has_constraint {
                Doc::text("(_)")
            } else {
                Doc::text("_")
            };
            let doc = print_comments(doc, cmt_tbl, pat.ppat_loc, arena);
            return if is_async { add_async(doc) } else { doc };
        }
        // let f = a => ()
        [FunParam::Parameter {
            attrs,
            label: ArgLabel::Nolabel,
            default_expr: None,
            pat,
        }] if attrs.is_empty()
            && matches!(&pat.ppat_desc, PatternDesc::Ppat_var(_))
            && pat.ppat_attributes.is_empty() =>
        {
            if let PatternDesc::Ppat_var(string_loc) = &pat.ppat_desc {
                let var = print_ident_like(&string_loc.txt, false, false);
                let var = if has_constraint { add_parens(var) } else { var };
                let doc = if is_async { add_async(var) } else { var };
                return print_comments(doc, cmt_tbl, string_loc.loc, arena);
            }
        }
        // let f = () => ()
        [FunParam::Parameter {
            attrs,
            label: ArgLabel::Nolabel,
            default_expr: None,
            pat,
        }] if attrs.is_empty()
            && matches!(
                &pat.ppat_desc,
                PatternDesc::Ppat_construct(lid, None)
                    if arena.is_lident(lid.txt, "()")
            ) =>
        {
            let doc = Doc::text("()");
            let doc = if is_async { add_async(doc) } else { doc };
            return print_comments(doc, cmt_tbl, pat.ppat_loc, arena);
        }
        _ => {}
    }

    // General case: multiple parameters or complex single parameter
    let fits_on_one_line = matches!(in_callback, InCallback::FitsOnOneLine);
    let should_hug = parameters_should_hug(parameters);

    let lparen = if is_async { add_async(Doc::lparen()) } else { Doc::lparen() };

    let printed_params = Doc::concat(vec![
        if should_hug || fits_on_one_line {
            Doc::nil()
        } else {
            Doc::soft_line()
        },
        Doc::join(
            Doc::concat(vec![Doc::comma(), Doc::line()]),
            parameters
                .iter()
                .map(|p| print_exp_fun_parameter(state, p, cmt_tbl, arena))
                .collect(),
        ),
    ]);

    Doc::group(Doc::concat(vec![
        lparen,
        if should_hug || fits_on_one_line {
            printed_params
        } else {
            Doc::concat(vec![
                Doc::indent(printed_params),
                Doc::trailing_comma(),
                Doc::soft_line(),
            ])
        },
        Doc::rparen(),
    ]))
}

/// Check if parameters should be hugged (printed without line breaks).
fn parameters_should_hug(parameters: &[parsetree_viewer::FunParam<'_>]) -> bool {
    use parsetree_viewer::FunParam;
    match parameters {
        [FunParam::Parameter {
            attrs,
            label: ArgLabel::Nolabel,
            default_expr: None,
            pat,
        }] if attrs.is_empty() => is_huggable_pattern(pat),
        _ => false,
    }
}

/// Print a single function parameter.
fn print_exp_fun_parameter(
    state: &PrinterState,
    param: &parsetree_viewer::FunParam<'_>,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    use parsetree_viewer::FunParam;
    match param {
        FunParam::NewType { attrs, name } => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            Doc::group(Doc::concat(vec![
                attrs_doc,
                Doc::text("type "),
                print_comments(print_ident_like(&name.txt, false, false), cmt_tbl, name.loc, arena),
            ]))
        }
        FunParam::Parameter {
            attrs,
            label,
            default_expr,
            pat,
        } => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            let default_doc = match default_expr {
                Some(expr) => Doc::concat(vec![
                    Doc::text("="),
                    print_expression_with_comments(state, expr, cmt_tbl, arena),
                ]),
                None => Doc::nil(),
            };

            let label_with_pattern = match (label, &pat.ppat_desc) {
                (ArgLabel::Nolabel, _) => print_pattern(state, pat, cmt_tbl, arena),
                // ~d (punning)
                (ArgLabel::Labelled(lbl) | ArgLabel::Optional(lbl), PatternDesc::Ppat_var(var))
                    if arena.get_string(lbl.txt) == var.txt =>
                {
                    Doc::concat(vec![
                        print_attributes(state, &pat.ppat_attributes, cmt_tbl, arena),
                        Doc::text("~"),
                        print_ident_like(arena.get_string(lbl.txt), false, false),
                    ])
                }
                // ~d: typ (punning with type)
                (
                    ArgLabel::Labelled(lbl) | ArgLabel::Optional(lbl),
                    PatternDesc::Ppat_constraint(inner, typ),
                ) if matches!(&inner.ppat_desc, PatternDesc::Ppat_var(v) if arena.get_string(lbl.txt) == v.txt) => {
                    Doc::concat(vec![
                        print_attributes(state, &pat.ppat_attributes, cmt_tbl, arena),
                        Doc::text("~"),
                        print_ident_like(arena.get_string(lbl.txt), false, false),
                        Doc::text(": "),
                        print_typ_expr(state, typ, cmt_tbl, arena),
                    ])
                }
                // ~d as x or ~d=pat
                (ArgLabel::Labelled(lbl), _) => Doc::concat(vec![
                    Doc::text("~"),
                    print_ident_like(arena.get_string(lbl.txt), false, false),
                    Doc::text(" as "),
                    print_pattern(state, pat, cmt_tbl, arena),
                ]),
                (ArgLabel::Optional(lbl), _) => Doc::concat(vec![
                    Doc::text("~"),
                    print_ident_like(arena.get_string(lbl.txt), false, false),
                    Doc::text(" as "),
                    print_pattern(state, pat, cmt_tbl, arena),
                ]),
            };

            let opt_marker = match label {
                ArgLabel::Optional(_) => Doc::text("=?"),
                _ => Doc::nil(),
            };

            Doc::group(Doc::concat(vec![
                attrs_doc,
                label_with_pattern,
                default_doc,
                opt_marker,
            ]))
        }
    }
}

/// Add async marker to a doc.
fn add_async(doc: Doc) -> Doc {
    Doc::concat(vec![Doc::text("async "), doc])
}

/// Try to print %obj extension as JS object literal: {"key": value}
/// Returns None if this is not an %obj extension with a record payload.
fn try_print_obj_extension(
    state: &PrinterState,
    ext: &Extension,
    _loc: LocIdx,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Option<Doc> {
    let (name, payload) = ext;
    if name.txt != "obj" {
        return None;
    }

    // Match the pattern: %obj with PStr containing a single Pstr_eval of a Pexp_record
    if let Payload::PStr(items) = payload {
        if let [single] = &items[..] {
            if let StructureItemDesc::Pstr_eval(expr, attrs) = &single.pstr_desc {
                if attrs.is_empty() {
                    if let ExpressionDesc::Pexp_record(fields, None) = &expr.pexp_desc {
                        // Check if object is written over multiple lines
                        // Use the structure item's location (pstr_loc)
                        let pstr_loc = arena.get_location(single.pstr_loc);
                        let start_pos = arena.get_position(pstr_loc.loc_start);
                        let end_pos = arena.get_position(pstr_loc.loc_end);
                        let force_break = start_pos.line < end_pos.line;

                        let rows_doc = Doc::join(
                            Doc::concat(vec![Doc::text(","), Doc::line()]),
                            fields
                                .iter()
                                .map(|field| print_bs_object_row(state, field, cmt_tbl, arena))
                                .collect(),
                        );

                        return Some(Doc::breakable_group(
                            Doc::concat(vec![
                                Doc::lbrace(),
                                Doc::indent(Doc::concat(vec![Doc::soft_line(), rows_doc])),
                                Doc::trailing_comma(),
                                Doc::soft_line(),
                                Doc::rbrace(),
                            ]),
                            force_break,
                        ));
                    }
                }
            }
        }
    }
    None
}

/// Print a row of a BS/JS object with string key: "key": value
fn print_bs_object_row(
    state: &PrinterState,
    field: &ExpressionRecordField,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let longident = arena.get_longident(field.lid.txt);
    let lbl_doc = Doc::concat(vec![
        Doc::text("\""),
        print_longident(arena, longident),
        Doc::text("\""),
    ]);
    let lbl_doc = print_comments(lbl_doc, cmt_tbl, field.lid.loc, arena);

    let expr_doc = print_expression_with_comments(state, &field.expr, cmt_tbl, arena);
    let expr_doc = match parens::expr(arena, &field.expr) {
        ParenKind::Parenthesized => add_parens(expr_doc),
        ParenKind::Braced(loc) => print_braces(expr_doc, &field.expr, loc, arena),
        ParenKind::Nothing => expr_doc,
    };

    Doc::concat(vec![lbl_doc, Doc::text(": "), expr_doc])
}

/// Try to print %re extension as raw regex string
/// Returns None if this is not an %re extension with a string constant.
fn try_print_re_extension(ext: &Extension) -> Option<Doc> {
    let (name, payload) = ext;
    if name.txt != "re" {
        return None;
    }

    // Match the pattern: %re with PStr containing a single Pstr_eval of a Pexp_constant string
    if let Payload::PStr(items) = payload {
        if let [single] = &items[..] {
            if let StructureItemDesc::Pstr_eval(expr, attrs) = &single.pstr_desc {
                if attrs.is_empty() {
                    if let ExpressionDesc::Pexp_constant(Constant::String(s, _)) = &expr.pexp_desc {
                        return Some(Doc::text(s.clone()));
                    }
                }
            }
        }
    }
    None
}

/// Print record expression.
fn print_record_expression(
    state: &PrinterState,
    loc: LocIdx,
    spread: Option<&Expression>,
    fields: &[ExpressionRecordField],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Calculate force_break: record written on multiple lines should break the group
    let force_break = match (spread, fields.first()) {
        (Some(expr), _) => {
            // If there's a spread, compare with spread expression's location
            arena.loc_start(loc).line < arena.loc_start(expr.pexp_loc).line
        }
        (None, Some(first_field)) => {
            // Otherwise, compare with the first row's location
            arena.loc_start(loc).line < arena.loc_start(first_field.lid.loc).line
        }
        (None, None) => false,
    };

    // Disallow punning for single-element records without spread
    let punning_allowed = !(spread.is_none() && fields.len() == 1);

    let spread_doc = match spread {
        Some(expr) => Doc::concat(vec![
            Doc::dotdotdot(),
            print_expression_with_comments(state, expr, cmt_tbl, arena),
            Doc::text(","),
            Doc::line(),
        ]),
        None => Doc::nil(),
    };
    let fields_doc = Doc::join(
        Doc::concat(vec![Doc::text(","), Doc::line()]),
        fields
            .iter()
            .map(|field| {
                let field_name = print_lident(arena, arena.get_longident(field.lid.txt));
                let expr_doc = print_expression_with_comments(state, &field.expr, cmt_tbl, arena);
                // Check for punning
                if punning_allowed && is_punned_record_field(arena, field) {
                    // Punned: `?name` or `name`
                    if field.opt {
                        Doc::concat(vec![Doc::text("?"), field_name])
                    } else {
                        field_name
                    }
                } else {
                    // Non-punned: `name: ?value` or `name: value`
                    // Check if the expression needs parens/braces
                    let expr_doc = match parens::expr_record_row_rhs(arena, field.opt, &field.expr) {
                        ParenKind::Parenthesized => add_parens(expr_doc),
                        ParenKind::Braced(loc) => print_braces(expr_doc, &field.expr, loc, arena),
                        ParenKind::Nothing => expr_doc,
                    };
                    let opt_marker = if field.opt { Doc::text("?") } else { Doc::nil() };
                    Doc::group(Doc::concat(vec![
                        field_name,
                        Doc::text(": "),
                        opt_marker,
                        expr_doc,
                    ]))
                }
            })
            .collect(),
    );
    Doc::breakable_group(
        Doc::concat(vec![
            Doc::lbrace(),
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                spread_doc,
                fields_doc,
            ])),
            Doc::trailing_comma(),
            Doc::soft_line(),
            Doc::rbrace(),
        ]),
        force_break,
    )
}

/// Get the last component of a longident (like OCaml's Longident.last).
fn longident_last<'a>(arena: &'a ParseArena, lid: &Longident) -> &'a str {
    match lid {
        Longident::Lident(s) => arena.get_string(*s),
        Longident::Ldot(_, s) => arena.get_string(*s),
        Longident::Lapply(_, _) => "",
    }
}

/// Check if a record field is punned.
/// A field is punned when the LAST component of the label matches the identifier expression,
/// e.g., `{a}` where a is Pexp_ident with txt = "a", or `{A.a}` where a is "a".
fn is_punned_record_field(arena: &ParseArena, field: &ExpressionRecordField) -> bool {
    // Expression must have no attributes
    if !field.expr.pexp_attributes.is_empty() {
        return false;
    }

    // Expression must be a simple Lident
    if let ExpressionDesc::Pexp_ident(path) = &field.expr.pexp_desc {
        if let Longident::Lident(ident_idx) = arena.get_longident(path.txt) {
            // Compare the LAST component of the label with the expression identifier
            let label_last = longident_last(arena, arena.get_longident(field.lid.txt));
            let ident_str = arena.get_string(*ident_idx);
            return label_last == ident_str;
        }
    }
    false
}

/// Check if expression is a send-set: `#=(lhs, rhs)` where lhs is a Pexp_send.
fn is_send_set_expr(arena: &ParseArena, funct: &Expression, args: &[(ArgLabel, Expression)]) -> bool {
    if args.len() != 2 {
        return false;
    }
    match &funct.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            arena.is_lident(lid.txt, "#=")
        }
        _ => false,
    }
}

/// Print a ternary expression (cond ? then : else).
fn print_ternary_expression(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (parts, alternate) = parsetree_viewer::collect_ternary_parts(e);

    let ternary_doc = match parts.as_slice() {
        [(condition1, consequent1), rest @ ..] => {
            let rest_doc = rest
                .iter()
                .map(|(condition, consequent)| {
                    Doc::concat(vec![
                        Doc::line(),
                        Doc::text(": "),
                        print_ternary_operand(state, condition, cmt_tbl, arena),
                        Doc::line(),
                        Doc::text("? "),
                        print_ternary_operand(state, consequent, cmt_tbl, arena),
                    ])
                })
                .collect::<Vec<_>>();

            Doc::group(Doc::concat(vec![
                print_ternary_operand(state, condition1, cmt_tbl, arena),
                Doc::indent(Doc::concat(vec![
                    Doc::line(),
                    Doc::indent(Doc::concat(vec![
                        Doc::text("? "),
                        print_ternary_operand(state, consequent1, cmt_tbl, arena),
                    ])),
                    Doc::concat(rest_doc),
                    Doc::line(),
                    Doc::text(": "),
                    Doc::indent(print_ternary_operand(state, alternate, cmt_tbl, arena)),
                ])),
            ]))
        }
        _ => Doc::nil(),
    };

    // Check if any of the printable attributes need parens wrapping
    // Note: Don't print attributes here - they're printed by print_expression's generic handler
    let needs_parens = e.pexp_attributes
        .iter()
        .filter(|attr| attr.0.txt != "res.ternary")
        .any(|attr| parsetree_viewer::is_printable_attribute(attr));

    if needs_parens {
        add_parens(ternary_doc)
    } else {
        ternary_doc
    }
}

/// Print a ternary operand with appropriate parenthesization.
fn print_ternary_operand(
    state: &PrinterState,
    expr: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);

    // Check for braces attribute first
    if let Some(attr) = parsetree_viewer::process_braces_attr(expr) {
        return print_braces(doc, expr, attr.0.loc, arena);
    }

    // Check if expression needs parentheses
    match &expr.pexp_desc {
        // Pexp_constraint on Pexp_pack doesn't need parens
        ExpressionDesc::Pexp_constraint(inner, _)
            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_)) =>
        {
            doc
        }
        // Other constraints need parens
        ExpressionDesc::Pexp_constraint(_, _) => add_parens(doc),
        _ => doc,
    }
}

/// Print if expression chain.
/// Handles `if`, `else if`, and `if let` chains properly.
fn print_if_expression(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    print_if_chain(state, e, cmt_tbl, arena)
}

/// Print an if-expression chain with proper `else if` flattening.
fn print_if_chain(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (ifs, else_expr) = parsetree_viewer::collect_if_expressions(arena,e);

    let if_docs: Vec<Doc> = ifs
        .iter()
        .enumerate()
        .map(|(i, (_outer_loc, condition_kind, then_expr))| {
            let if_txt = if i > 0 {
                Doc::text("else if ")
            } else {
                Doc::text("if ")
            };

            let doc = match condition_kind {
                parsetree_viewer::IfConditionKind::If(if_expr) => {
                    let condition = if parsetree_viewer::is_block_expr(if_expr) {
                        print_expression_block(state, true, if_expr, cmt_tbl, arena)
                    } else {
                        let doc = print_expression_with_comments(state, if_expr, cmt_tbl, arena);
                        match parens::expr(arena,if_expr) {
                            ParenKind::Parenthesized => add_parens(doc),
                            ParenKind::Braced(loc) => print_braces(doc, if_expr, loc, arena),
                            ParenKind::Nothing => Doc::if_breaks(add_parens(doc.clone()), doc),
                        }
                    };

                    // Strip braces attribute from then_expr if present (from Reason conversion)
                    let then_expr_to_print = if let Some(_) = parsetree_viewer::process_braces_attr(then_expr) {
                        then_expr
                    } else {
                        then_expr
                    };

                    Doc::concat(vec![
                        if_txt,
                        Doc::group(condition),
                        Doc::space(),
                        print_expression_block(state, true, then_expr_to_print, cmt_tbl, arena),
                    ])
                }
                parsetree_viewer::IfConditionKind::IfLet(pattern, condition_expr) => {
                    let condition_doc = {
                        let doc = print_expression_with_comments(state, condition_expr, cmt_tbl, arena);
                        match parens::expr(arena,condition_expr) {
                            ParenKind::Parenthesized => add_parens(doc),
                            ParenKind::Braced(loc) => print_braces(doc, condition_expr, loc, arena),
                            ParenKind::Nothing => doc,
                        }
                    };

                    Doc::concat(vec![
                        if_txt,
                        Doc::text("let "),
                        print_pattern(state, pattern, cmt_tbl, arena),
                        Doc::text(" = "),
                        condition_doc,
                        Doc::space(),
                        print_expression_block(state, true, then_expr, cmt_tbl, arena),
                    ])
                }
            };

            // TODO: Print leading comments for outer_loc
            doc
        })
        .collect();

    let if_docs = Doc::join(Doc::space(), if_docs);

    let else_doc = match else_expr {
        None => Doc::nil(),
        Some(expr) => Doc::concat(vec![
            Doc::text(" else "),
            print_expression_block(state, true, expr, cmt_tbl, arena),
        ]),
    };

    // Note: Don't print attributes here - they're printed by print_expression's generic handler
    Doc::concat(vec![
        if_docs,
        else_doc,
    ])
}

/// Print expression block (with braces).
fn print_expression_block(
    state: &PrinterState,
    braces: bool,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match &e.pexp_desc {
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            let bindings_doc = print_value_bindings(state, bindings, rec_flag, cmt_tbl, arena);
            let body_doc = print_expression_block(state, false, body, cmt_tbl, arena);
            if braces {
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::hard_line(),
                        bindings_doc,
                        Doc::hard_line(),
                        body_doc,
                    ])),
                    Doc::hard_line(),
                    Doc::rbrace(),
                ])
            } else {
                Doc::concat(vec![
                    bindings_doc,
                    Doc::hard_line(),
                    body_doc,
                ])
            }
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            let e1_doc = print_expression_with_comments(state, e1, cmt_tbl, arena);
            // Check if expression needs parentheses
            let e1_doc = match parens::expr(arena,e1) {
                ParenKind::Parenthesized => add_parens(e1_doc),
                ParenKind::Braced(loc) => print_braces(e1_doc, e1, loc, arena),
                ParenKind::Nothing => e1_doc,
            };
            let e2_doc = print_expression_block(state, false, e2, cmt_tbl, arena);
            if braces {
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::hard_line(),
                        e1_doc,
                        Doc::hard_line(),
                        e2_doc,
                    ])),
                    Doc::hard_line(),
                    Doc::rbrace(),
                ])
            } else {
                Doc::concat(vec![e1_doc, Doc::hard_line(), e2_doc])
            }
        }
        ExpressionDesc::Pexp_letmodule(name, mod_expr, body) => {
            let mod_doc = print_mod_expr(state, mod_expr, cmt_tbl, arena);
            let body_doc = print_expression_block(state, false, body, cmt_tbl, arena);
            if braces {
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::hard_line(),
                        Doc::text("module "),
                        Doc::text(&name.txt),
                        Doc::text(" = "),
                        mod_doc,
                        Doc::hard_line(),
                        body_doc,
                    ])),
                    Doc::hard_line(),
                    Doc::rbrace(),
                ])
            } else {
                Doc::concat(vec![
                    Doc::text("module "),
                    Doc::text(&name.txt),
                    Doc::text(" = "),
                    mod_doc,
                    Doc::hard_line(),
                    body_doc,
                ])
            }
        }
        ExpressionDesc::Pexp_letexception(ext_constr, body) => {
            let ext_doc = print_extension_constructor(state, ext_constr, cmt_tbl, arena);
            let body_doc = print_expression_block(state, false, body, cmt_tbl, arena);
            if braces {
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::hard_line(),
                        Doc::text("exception "),
                        ext_doc,
                        Doc::hard_line(),
                        body_doc,
                    ])),
                    Doc::hard_line(),
                    Doc::rbrace(),
                ])
            } else {
                Doc::concat(vec![
                    Doc::text("exception "),
                    ext_doc,
                    Doc::hard_line(),
                    body_doc,
                ])
            }
        }
        ExpressionDesc::Pexp_open(override_flag, lid, body) => {
            let override_doc = match override_flag {
                OverrideFlag::Override => Doc::text("!"),
                OverrideFlag::Fresh => Doc::nil(),
            };
            let body_doc = print_expression_block(state, false, body, cmt_tbl, arena);
            if braces {
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::hard_line(),
                        Doc::text("open"),
                        override_doc,
                        Doc::text(" "),
                        print_longident(arena, arena.get_longident(lid.txt)),
                        Doc::hard_line(),
                        body_doc,
                    ])),
                    Doc::hard_line(),
                    Doc::rbrace(),
                ])
            } else {
                Doc::concat(vec![
                    Doc::text("open"),
                    override_doc,
                    Doc::text(" "),
                    print_longident(arena, arena.get_longident(lid.txt)),
                    Doc::hard_line(),
                    body_doc,
                ])
            }
        }
        _ => {
            let doc = print_expression_with_comments(state, e, cmt_tbl, arena);
            // Check if expression needs parentheses
            let doc = match parens::expr(arena,e) {
                ParenKind::Parenthesized => add_parens(doc),
                ParenKind::Braced(loc) => print_braces(doc, e, loc, arena),
                ParenKind::Nothing => doc,
            };
            if braces {
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![Doc::hard_line(), doc])),
                    Doc::hard_line(),
                    Doc::rbrace(),
                ])
            } else {
                doc
            }
        }
    }
}

/// Print function application.
fn print_pexp_apply(
    state: &PrinterState,
    expr: &Expression,
    funct: &Expression,
    args: &[(ArgLabel, Expression)],
    partial: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Check for binary expression
    if args.len() == 2 {
        if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
            if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                let op = arena.get_string(*op_idx);
                if parsetree_viewer::is_binary_operator_str(op) {
                    let doc = print_binary_expression(state, op, args, cmt_tbl, arena);
                    // Check if the binary expression needs parens (when it has printable attributes)
                    // We filter to only printable attributes for this check
                    let printable_attrs: Vec<&Attribute> = expr.pexp_attributes
                        .iter()
                        .filter(|attr| parsetree_viewer::is_printable_attribute(attr))
                        .collect();
                    if !printable_attrs.is_empty() {
                        return add_parens(doc);
                    }
                    return doc;
                }
            }
        }
    }

    // Check for unary expression
    if args.len() == 1 {
        if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
            if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                let op = arena.get_string(*op_idx);
                if parsetree_viewer::is_unary_operator_str(op) {
                    return print_unary_expression(state, op, args, cmt_tbl, arena);
                }
            }
        }
    }

    // Regular function application
    let funct_doc = print_expression_with_comments(state, funct, cmt_tbl, arena);
    let funct_doc = match parens::call_expr(arena,funct) {
        ParenKind::Parenthesized => add_parens(funct_doc),
        ParenKind::Braced(loc) => print_braces(funct_doc, funct, loc, arena),
        ParenKind::Nothing => funct_doc,
    };
    let args_doc = print_arguments(state, args, partial, cmt_tbl, arena);
    Doc::group(Doc::concat(vec![funct_doc, args_doc]))
}

/// Check if a binary operand needs parens considering parent operator precedence.
fn binary_operand_needs_parens(arena: &ParseArena, is_lhs: bool, parent_op: &str, operand: &Expression) -> ParenKind {
    // First check braces attribute
    if let Some(attr) = parsetree_viewer::process_braces_attr(operand) {
        return ParenKind::Braced(attr.0.loc.clone());
    }

    // Check if it's a binary expression - if so, use precedence comparison
    if let Some(child_op) = parsetree_viewer::get_binary_operator(arena, operand) {
        // Use precedence-based check
        if parens::sub_binary_expr_operand(parent_op, &child_op) {
            return ParenKind::Parenthesized;
        }
        // For RHS, also check right-associativity
        if !is_lhs && parens::rhs_binary_expr_operand(arena, parent_op, operand) {
            return ParenKind::Parenthesized;
        }
        // Check if operand has printable attributes
        if parsetree_viewer::has_printable_attributes(&operand.pexp_attributes) {
            return ParenKind::Parenthesized;
        }
        return ParenKind::Nothing;
    }

    // Fall back to the general binary_expr_operand check for non-binary expressions
    parens::binary_expr_operand(arena, is_lhs, operand)
}

/// Print binary expression.
fn print_binary_expression(
    state: &PrinterState,
    op: &str,
    args: &[(ArgLabel, Expression)],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if args.len() != 2 {
        return Doc::nil();
    }
    let (_, lhs) = &args[0];
    let (_, rhs) = &args[1];

    // Print operands with appropriate parenthesization using precedence-aware check
    let lhs_doc = print_expression_with_comments(state, lhs, cmt_tbl, arena);
    let lhs_doc = match binary_operand_needs_parens(arena, true, op, lhs) {
        ParenKind::Parenthesized => add_parens(lhs_doc),
        ParenKind::Braced(loc) => print_braces(lhs_doc, lhs, loc, arena),
        ParenKind::Nothing => lhs_doc,
    };

    let rhs_doc = print_expression_with_comments(state, rhs, cmt_tbl, arena);
    let rhs_doc = match binary_operand_needs_parens(arena, false, op, rhs) {
        ParenKind::Parenthesized => add_parens(rhs_doc),
        ParenKind::Braced(loc) => print_braces(rhs_doc, rhs, loc, arena),
        ParenKind::Nothing => rhs_doc,
    };

    // Determine if the rhs should be on a new line
    let should_indent = parens::flatten_operand_rhs(arena, op, rhs);

    // Pipe-first (->) doesn't have spaces around it
    let is_pipe_first = op == "->";

    if should_indent {
        Doc::group(Doc::concat(vec![
            lhs_doc,
            if is_pipe_first { Doc::nil() } else { Doc::space() },
            Doc::text(op),
            Doc::group(Doc::indent(Doc::concat(vec![Doc::line(), rhs_doc]))),
        ]))
    } else {
        Doc::concat(vec![
            lhs_doc,
            if is_pipe_first { Doc::nil() } else { Doc::space() },
            Doc::text(op),
            if is_pipe_first { Doc::nil() } else { Doc::space() },
            rhs_doc,
        ])
    }
}

/// Print unary expression.
fn print_unary_expression(
    state: &PrinterState,
    op: &str,
    args: &[(ArgLabel, Expression)],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if args.is_empty() {
        return Doc::nil();
    }
    let (_, operand) = &args[0];
    let operand_doc = print_expression_with_comments(state, operand, cmt_tbl, arena);
    let operand_doc = match parens::unary_expr_operand(arena,operand) {
        ParenKind::Parenthesized => add_parens(operand_doc),
        ParenKind::Braced(loc) => print_braces(operand_doc, operand, loc, arena),
        ParenKind::Nothing => operand_doc,
    };

    // Convert internal operator names
    let printed_op = match op {
        "~-" => "-",
        "~-." => "-.",
        "~+" => "+",
        "~+." => "+.",
        "~~~" => "~~~",
        "not" => "!",
        _ => op,
    };

    Doc::concat(vec![Doc::text(printed_op), operand_doc])
}

/// Print function arguments.
fn print_arguments(
    state: &PrinterState,
    args: &[(ArgLabel, Expression)],
    partial: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if args.is_empty() {
        return Doc::text("()");
    }

    // Special case: single unit argument () -> just print ()
    if let [(ArgLabel::Nolabel, expr)] = args {
        if is_unit_expr(arena, expr) {
            // Check for leading comments
            if has_leading_line_comment(cmt_tbl, expr.pexp_loc) {
                let cmt = print_comments(Doc::nil(), cmt_tbl, expr.pexp_loc, arena);
                return Doc::concat(vec![
                    Doc::lparen(),
                    Doc::indent(Doc::group(Doc::concat(vec![Doc::soft_line(), cmt]))),
                    Doc::rparen(),
                ]);
            }
            return Doc::text("()");
        }
    }

    // Special case: single huggable argument (array, tuple, record, etc.)
    if let [(ArgLabel::Nolabel, arg)] = args {
        if parsetree_viewer::is_huggable_expression(arena, arg) {
            let arg_doc = {
                let doc = print_expression_with_comments(state, arg, cmt_tbl, arena);
                match parens::expr(arena, arg) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, arg, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };
            return Doc::concat(vec![Doc::lparen(), arg_doc, Doc::rparen()]);
        }
    }

    let args_doc: Vec<Doc> = args
        .iter()
        .map(|(label, expr)| {
            // Print expression with proper parenthesization
            let expr_doc = {
                let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                match parens::expr(arena, expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };
            match label {
                ArgLabel::Nolabel => expr_doc,
                ArgLabel::Labelled(name) => {
                    let name_str = arena.get_string(name.txt);
                    // Check for punning: ~name where expr is Pexp_ident(Lident(name))
                    if let ExpressionDesc::Pexp_ident(lid) = &expr.pexp_desc {
                        if let Longident::Lident(ident_idx) = arena.get_longident(lid.txt) {
                            let ident_str = arena.get_string(*ident_idx);
                            if ident_str == name_str && expr.pexp_attributes.is_empty() && !parsetree_viewer::is_braced_expr(expr) {
                                return Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false)]);
                            }
                        }
                    }
                    // Check for punned with type constraint: ~name: type where expr is Pexp_constraint(Pexp_ident(Lident(name)), typ)
                    if let ExpressionDesc::Pexp_constraint(inner_expr, typ) = &expr.pexp_desc {
                        if expr.pexp_attributes.is_empty() {
                            if let ExpressionDesc::Pexp_ident(lid) = &inner_expr.pexp_desc {
                                if let Longident::Lident(ident_idx) = arena.get_longident(lid.txt) {
                                    let ident_str = arena.get_string(*ident_idx);
                                    if ident_str == name_str && !parsetree_viewer::is_braced_expr(inner_expr) {
                                        let typ_doc = print_typ_expr(state, typ, cmt_tbl, arena);
                                        return Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text(": "), typ_doc]);
                                    }
                                }
                            }
                        }
                    }
                    Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text("="), expr_doc])
                }
                ArgLabel::Optional(name) => {
                    // Check for punning: ~name=? where expr is Pexp_ident(Lident(name))
                    let name_str = arena.get_string(name.txt);
                    if let ExpressionDesc::Pexp_ident(lid) = &expr.pexp_desc {
                        if let Longident::Lident(ident_idx) = arena.get_longident(lid.txt) {
                            let ident_str = arena.get_string(*ident_idx);
                            if ident_str == name_str && expr.pexp_attributes.is_empty() {
                                return Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text("?")]);
                            }
                        }
                    }
                    Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text("=?"), expr_doc])
                }
            }
        })
        .collect();

    // If partial, add ... at the end
    let mut all_docs = args_doc;
    if partial {
        all_docs.push(Doc::text("..."));
    }

    Doc::group(Doc::concat(vec![
        Doc::lparen(),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), all_docs),
        ])),
        // Don't add trailing comma for partial application
        if partial { Doc::nil() } else { Doc::trailing_comma() },
        Doc::soft_line(),
        Doc::rparen(),
    ]))
}

/// Check if an expression is unit: `()`
fn is_unit_expr(arena: &ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_construct(lid, None) => arena.is_lident(lid.txt, "()"),
        _ => false,
    }
}

/// Check if there's a leading line comment at a location.
fn has_leading_line_comment(cmt_tbl: &CommentTable, loc: LocIdx) -> bool {
    if let Some(comments) = cmt_tbl.leading.get(&loc) {
        comments.iter().any(|c| c.is_single_line())
    } else {
        false
    }
}

// ============================================================================
// Pattern Printing
// ============================================================================

/// Print a pattern with comments.
pub fn print_pattern_with_comments(
    state: &PrinterState,
    pat: &Pattern,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let doc = print_pattern(state, pat, cmt_tbl, arena);
    print_comments(doc, cmt_tbl, pat.ppat_loc, arena)
}

/// Print a pattern.
pub fn print_pattern(
    state: &PrinterState,
    pat: &Pattern,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let pattern_without_attrs = match &pat.ppat_desc {
        // _
        PatternDesc::Ppat_any => Doc::text("_"),
        // x
        PatternDesc::Ppat_var(var) => print_ident_like(&var.txt, false, false),
        // 42, "hello", 'a', etc.
        PatternDesc::Ppat_constant(c) => {
            let template_literal = parsetree_viewer::has_template_literal_attr(&pat.ppat_attributes);
            print_constant(template_literal, c)
        }
        // (p1, p2, ...)
        PatternDesc::Ppat_tuple(patterns) => {
            Doc::group(Doc::concat(vec![
                Doc::lparen(),
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(
                        Doc::concat(vec![Doc::text(","), Doc::line()]),
                        patterns
                            .iter()
                            .map(|p| print_pattern(state, p, cmt_tbl, arena))
                            .collect(),
                    ),
                ])),
                Doc::trailing_comma(),
                Doc::soft_line(),
                Doc::rparen(),
            ]))
        }
        // []
        PatternDesc::Ppat_array(patterns) if patterns.is_empty() => {
            Doc::concat(vec![
                Doc::lbracket(),
                print_comments_inside(cmt_tbl, pat.ppat_loc, arena),
                Doc::rbracket(),
            ])
        }
        // [p1, p2, ...]
        PatternDesc::Ppat_array(patterns) => {
            Doc::group(Doc::concat(vec![
                Doc::text("["),
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(
                        Doc::concat(vec![Doc::text(","), Doc::line()]),
                        patterns
                            .iter()
                            .map(|p| print_pattern(state, p, cmt_tbl, arena))
                            .collect(),
                    ),
                ])),
                Doc::trailing_comma(),
                Doc::soft_line(),
                Doc::text("]"),
            ]))
        }
        // ()
        PatternDesc::Ppat_construct(lid, None)
            if arena.is_lident(lid.txt, "()") =>
        {
            Doc::concat(vec![
                Doc::lparen(),
                print_comments_inside(cmt_tbl, pat.ppat_loc, arena),
                Doc::rparen(),
            ])
        }
        // list{}
        PatternDesc::Ppat_construct(lid, None)
            if arena.is_lident(lid.txt, "[]") =>
        {
            Doc::concat(vec![
                Doc::text("list{"),
                print_comments_inside(cmt_tbl, pat.ppat_loc, arena),
                Doc::rbrace(),
            ])
        }
        // list{p1, p2, ...spread}
        PatternDesc::Ppat_construct(lid, _)
            if arena.is_lident(lid.txt, "::") =>
        {
            print_list_pattern(state, pat, cmt_tbl, arena)
        }
        // Constructor(args)
        PatternDesc::Ppat_construct(constr_name, constructor_args) => {
            let constr = print_longident_location(constr_name, cmt_tbl, arena);
            let args_doc = match constructor_args {
                None => Doc::nil(),
                Some(arg) => match &arg.ppat_desc {
                    PatternDesc::Ppat_construct(lid, None)
                        if arena.is_lident(lid.txt, "()") =>
                    {
                        Doc::concat(vec![
                            Doc::lparen(),
                            print_comments_inside(cmt_tbl, arg.ppat_loc, arena),
                            Doc::rparen(),
                        ])
                    }
                    // Some((1, 2))
                    PatternDesc::Ppat_tuple(pats) if pats.len() == 1 => {
                        if let PatternDesc::Ppat_tuple(_) = &pats[0].ppat_desc {
                            Doc::concat(vec![
                                Doc::lparen(),
                                print_pattern(state, &pats[0], cmt_tbl, arena),
                                Doc::rparen(),
                            ])
                        } else {
                            print_pattern_constructor_args(state, Some(arg), cmt_tbl, arena)
                        }
                    }
                    PatternDesc::Ppat_tuple(patterns) => {
                        Doc::concat(vec![
                            Doc::lparen(),
                            Doc::indent(Doc::concat(vec![
                                Doc::soft_line(),
                                Doc::join(
                                    Doc::concat(vec![Doc::comma(), Doc::line()]),
                                    patterns
                                        .iter()
                                        .map(|p| print_pattern(state, p, cmt_tbl, arena))
                                        .collect(),
                                ),
                            ])),
                            Doc::trailing_comma(),
                            Doc::soft_line(),
                            Doc::rparen(),
                        ])
                    }
                    _ => {
                        let arg_doc = print_pattern(state, arg, cmt_tbl, arena);
                        let should_hug = is_huggable_pattern(arg);
                        Doc::concat(vec![
                            Doc::lparen(),
                            if should_hug {
                                arg_doc
                            } else {
                                Doc::concat(vec![
                                    Doc::indent(Doc::concat(vec![Doc::soft_line(), arg_doc])),
                                    Doc::trailing_comma(),
                                    Doc::soft_line(),
                                ])
                            },
                            Doc::rparen(),
                        ])
                    }
                },
            };
            Doc::group(Doc::concat(vec![constr, args_doc]))
        }
        // #variant
        PatternDesc::Ppat_variant(label, None) => {
            Doc::concat(vec![Doc::text("#"), print_poly_var_ident(label)])
        }
        // #variant(args)
        PatternDesc::Ppat_variant(label, Some(arg)) => {
            let variant_name = Doc::concat(vec![Doc::text("#"), print_poly_var_ident(label)]);
            let args_doc = match &arg.ppat_desc {
                PatternDesc::Ppat_construct(lid, None)
                    if arena.is_lident(lid.txt, "()") =>
                {
                    Doc::text("()")
                }
                PatternDesc::Ppat_tuple(patterns) => {
                    Doc::concat(vec![
                        Doc::lparen(),
                        Doc::indent(Doc::concat(vec![
                            Doc::soft_line(),
                            Doc::join(
                                Doc::concat(vec![Doc::comma(), Doc::line()]),
                                patterns
                                    .iter()
                                    .map(|p| print_pattern(state, p, cmt_tbl, arena))
                                    .collect(),
                            ),
                        ])),
                        Doc::trailing_comma(),
                        Doc::soft_line(),
                        Doc::rparen(),
                    ])
                }
                _ => {
                    let arg_doc = print_pattern(state, arg, cmt_tbl, arena);
                    let should_hug = is_huggable_pattern(arg);
                    Doc::concat(vec![
                        Doc::lparen(),
                        if should_hug {
                            arg_doc
                        } else {
                            Doc::concat(vec![
                                Doc::indent(Doc::concat(vec![Doc::soft_line(), arg_doc])),
                                Doc::trailing_comma(),
                                Doc::soft_line(),
                            ])
                        },
                        Doc::rparen(),
                    ])
                }
            };
            Doc::group(Doc::concat(vec![variant_name, args_doc]))
        }
        // {field1, field2: pat, ...spread}
        PatternDesc::Ppat_record(fields, closed) => {
            print_record_pattern(state, fields, &closed, cmt_tbl, arena)
        }
        // p | p
        PatternDesc::Ppat_or(_, _) => {
            // Collect the entire or-chain into a flat list
            let or_chain = parsetree_viewer::collect_or_pattern_chain(pat);
            let docs: Vec<Doc> = or_chain
                .iter()
                .enumerate()
                .map(|(i, p)| {
                    let pattern_doc = print_pattern(state, p, cmt_tbl, arena);
                    // Wrap nested or-patterns in parentheses
                    let pattern_doc = if matches!(&p.ppat_desc, PatternDesc::Ppat_or(_, _)) {
                        add_parens(pattern_doc)
                    } else {
                        pattern_doc
                    };
                    if i == 0 {
                        pattern_doc
                    } else {
                        Doc::concat(vec![Doc::line(), Doc::text("| "), pattern_doc])
                    }
                })
                .collect();

            // Check if the pattern spans multiple lines in source
            let is_spread_over_multiple_lines = if let (Some(first), Some(last)) =
                (or_chain.first(), or_chain.last())
            {
                arena.loc_start(first.ppat_loc).line < arena.loc_end(last.ppat_loc).line
            } else {
                false
            };

            Doc::breakable_group(Doc::concat(docs), is_spread_over_multiple_lines)
        }
        // module(M: S) - first-class module with package type
        PatternDesc::Ppat_constraint(inner, typ)
            if matches!(&inner.ppat_desc, PatternDesc::Ppat_unpack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
        {
            if let (
                PatternDesc::Ppat_unpack(name),
                CoreTypeDesc::Ptyp_package(package_type),
            ) = (&inner.ppat_desc, &typ.ptyp_desc)
            {
                let name_doc = Doc::text(&name.txt);
                let name_doc = print_comments(name_doc, cmt_tbl, inner.ppat_loc, arena);
                let pkg_doc = print_package_type(state, package_type, false, cmt_tbl, arena);
                let pkg_doc = print_comments(pkg_doc, cmt_tbl, typ.ptyp_loc, arena);
                Doc::concat(vec![
                    Doc::text("module("),
                    name_doc,
                    Doc::text(": "),
                    pkg_doc,
                    Doc::text(")"),
                ])
            } else {
                unreachable!()
            }
        }
        // p : type
        PatternDesc::Ppat_constraint(pat, typ) => {
            let pat_doc = print_pattern(state, pat, cmt_tbl, arena);
            let typ_doc = print_typ_expr(state, typ, cmt_tbl, arena);
            Doc::concat(vec![pat_doc, Doc::text(": "), typ_doc])
        }
        // p as x
        PatternDesc::Ppat_alias(pat, alias) => {
            let needs_parens = matches!(
                &pat.ppat_desc,
                PatternDesc::Ppat_or(_, _) | PatternDesc::Ppat_alias(_, _)
            );
            let pat_doc = print_pattern(state, pat, cmt_tbl, arena);
            let pat_doc = if needs_parens {
                Doc::concat(vec![Doc::text("("), pat_doc, Doc::text(")")])
            } else {
                pat_doc
            };
            Doc::concat(vec![pat_doc, Doc::text(" as "), Doc::text(&alias.txt)])
        }
        // module(M)
        PatternDesc::Ppat_unpack(name) => {
            Doc::concat(vec![Doc::text("module("), Doc::text(&name.txt), Doc::text(")")])
        }
        // exception pat
        PatternDesc::Ppat_exception(pat) => {
            let needs_parens = matches!(
                &pat.ppat_desc,
                PatternDesc::Ppat_or(_, _) | PatternDesc::Ppat_alias(_, _)
            );
            let pat_doc = print_pattern(state, pat, cmt_tbl, arena);
            let pat_doc = if needs_parens {
                Doc::concat(vec![Doc::text("("), pat_doc, Doc::text(")")])
            } else {
                pat_doc
            };
            Doc::group(Doc::concat(vec![Doc::text("exception"), Doc::line(), pat_doc]))
        }
        // %extension
        PatternDesc::Ppat_extension(ext) => {
            print_extension(state, ext, false, cmt_tbl, arena)
        }
        // `type identifier
        PatternDesc::Ppat_type(lid) => {
            // Use print_lident for proper escaping of exotic identifiers
            let doc = print_lident(arena, arena.get_longident(lid.txt));
            let doc = print_comments(doc, cmt_tbl, lid.loc, arena);
            Doc::concat(vec![Doc::text("#..."), doc])
        }
        // interval: 'a' .. 'z'
        PatternDesc::Ppat_interval(c1, c2) => {
            Doc::concat(vec![
                print_constant(false, c1),
                Doc::text(" .. "),
                print_constant(false, c2),
            ])
        }
        // open M.(pat)
        PatternDesc::Ppat_open(lid, pat) => {
            Doc::concat(vec![
                print_longident(arena, arena.get_longident(lid.txt)),
                Doc::text("."),
                Doc::lparen(),
                print_pattern(state, pat, cmt_tbl, arena),
                Doc::rparen(),
            ])
        }
    };

    // Handle attributes
    if pat.ppat_attributes.is_empty() {
        pattern_without_attrs
    } else {
        let attrs = print_attributes(state, &pat.ppat_attributes, cmt_tbl, arena);
        Doc::concat(vec![attrs, pattern_without_attrs])
    }
}

/// Check if a pattern is "huggable" (can be printed without line breaks).
fn is_huggable_pattern(pat: &Pattern) -> bool {
    matches!(
        &pat.ppat_desc,
        PatternDesc::Ppat_array(_)
            | PatternDesc::Ppat_tuple(_)
            | PatternDesc::Ppat_record(_, _)
            | PatternDesc::Ppat_variant(_, _)
            | PatternDesc::Ppat_construct(_, _)
    )
}

/// Print pattern constructor args.
fn print_pattern_constructor_args(
    state: &PrinterState,
    arg: Option<&Pattern>,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match arg {
        None => Doc::nil(),
        Some(p) => {
            let doc = print_pattern(state, p, cmt_tbl, arena);
            let should_hug = is_huggable_pattern(p);
            Doc::concat(vec![
                Doc::lparen(),
                if should_hug {
                    doc
                } else {
                    Doc::concat(vec![
                        Doc::indent(Doc::concat(vec![Doc::soft_line(), doc])),
                        Doc::trailing_comma(),
                        Doc::soft_line(),
                    ])
                },
                Doc::rparen(),
            ])
        }
    }
}

/// Print list pattern.
fn print_list_pattern(
    state: &PrinterState,
    pat: &Pattern,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (patterns, tail) = parsetree_viewer::collect_list_patterns(arena,pat);

    let should_hug = matches!((&patterns[..], tail),
        ([single], Some(t)) if is_huggable_pattern(single)
            && matches!(&t.ppat_desc, PatternDesc::Ppat_construct(lid, None)
                if arena.is_lident(lid.txt, "[]")));

    let tail_doc = match tail {
        Some(t) => match &t.ppat_desc {
            PatternDesc::Ppat_construct(lid, None)
                if arena.is_lident(lid.txt, "[]") =>
            {
                Doc::nil()
            }
            _ => {
                Doc::concat(vec![
                    Doc::text(","),
                    Doc::line(),
                    Doc::text("..."),
                    print_pattern(state, t, cmt_tbl, arena),
                ])
            }
        },
        None => Doc::nil(),
    };

    let children = Doc::concat(vec![
        if should_hug { Doc::nil() } else { Doc::soft_line() },
        Doc::join(
            Doc::concat(vec![Doc::text(","), Doc::line()]),
            patterns
                .iter()
                .map(|p| print_pattern(state, *p, cmt_tbl, arena))
                .collect(),
        ),
        tail_doc,
    ]);

    Doc::group(Doc::concat(vec![
        Doc::text("list{"),
        if should_hug {
            children
        } else {
            Doc::concat(vec![
                Doc::indent(children),
                Doc::if_breaks(Doc::text(","), Doc::nil()),
                Doc::soft_line(),
            ])
        },
        Doc::rbrace(),
    ]))
}

/// Print record pattern.
fn print_record_pattern(
    state: &PrinterState,
    fields: &[PatternRecordField],
    closed: &ClosedFlag,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let fields_doc = Doc::join(
        Doc::concat(vec![Doc::text(","), Doc::line()]),
        fields
            .iter()
            .map(|field| {
                let label = print_lident(arena, arena.get_longident(field.lid.txt));
                // Check for punning
                if is_punned_pattern_field(arena, field) {
                    if field.opt {
                        Doc::concat(vec![Doc::text("?"), label])
                    } else {
                        label
                    }
                } else {
                    let pat_doc = print_pattern(state, &field.pat, cmt_tbl, arena);
                    // Check if pattern needs parentheses in record row context
                    let pat_doc = if parens::pattern_record_row_rhs(&field.pat) {
                        add_parens(pat_doc)
                    } else {
                        pat_doc
                    };
                    if field.opt {
                        Doc::concat(vec![label, Doc::text(": "), Doc::text("?"), pat_doc])
                    } else {
                        Doc::concat(vec![label, Doc::text(": "), pat_doc])
                    }
                }
            })
            .collect(),
    );

    let open_doc = match closed {
        ClosedFlag::Closed => Doc::nil(),
        ClosedFlag::Open => {
            if fields.is_empty() {
                Doc::text("_")
            } else {
                Doc::concat(vec![Doc::text(","), Doc::line(), Doc::text("_")])
            }
        }
    };

    Doc::group(Doc::concat(vec![
        Doc::lbrace(),
        Doc::indent(Doc::concat(vec![Doc::soft_line(), fields_doc, open_doc])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::rbrace(),
    ]))
}

/// Check if a pattern record field is punned.
fn is_punned_pattern_field(arena: &ParseArena, field: &PatternRecordField) -> bool {
    match (arena.get_longident(field.lid.txt), &field.pat.ppat_desc) {
        (Longident::Lident(name_idx), PatternDesc::Ppat_var(var)) => {
            let name_str = arena.get_string(*name_idx);
            name_str == &var.txt
        }
        _ => false,
    }
}

// ============================================================================
// Type Printing
// ============================================================================

/// Print a type expression.
pub fn print_typ_expr(
    state: &PrinterState,
    typ: &CoreType,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let rendered_type = match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => Doc::text("_"),

        CoreTypeDesc::Ptyp_var(name) => {
            Doc::concat(vec![Doc::text("'"), print_ident_like(name, true, false)])
        }

        CoreTypeDesc::Ptyp_extension(ext) => print_extension(state, ext, false, cmt_tbl, arena),

        CoreTypeDesc::Ptyp_alias(inner_typ, alias) => {
            let needs_parens = matches!(&inner_typ.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. });
            let typ_doc = print_typ_expr(state, inner_typ, cmt_tbl, arena);
            if needs_parens {
                Doc::concat(vec![
                    Doc::lparen(),
                    typ_doc,
                    Doc::rparen(),
                    Doc::text(" as "),
                    Doc::concat(vec![Doc::text("'"), print_ident_like(alias, false, false)]),
                ])
            } else {
                Doc::concat(vec![
                    typ_doc,
                    Doc::text(" as "),
                    Doc::concat(vec![Doc::text("'"), print_ident_like(alias, false, false)]),
                ])
            }
        }

        CoreTypeDesc::Ptyp_object(fields, open_flag) => {
            print_object(state, fields, open_flag, false, cmt_tbl, arena)
        }

        CoreTypeDesc::Ptyp_arrow { arity, .. } => {
            print_arrow_type(state, typ, arity.clone(), cmt_tbl, arena)
        }

        CoreTypeDesc::Ptyp_constr(lid, args) => {
            // Handle special case: object type inside type constructor
            if let [single_arg] = args.as_slice() {
                if let CoreTypeDesc::Ptyp_object(fields, open_flag) = &single_arg.ptyp_desc {
                    let constr_name = print_lident_path(lid, cmt_tbl, arena);
                    Doc::concat(vec![
                        constr_name,
                        Doc::less_than(),
                        print_object(state, fields, open_flag, true, cmt_tbl, arena),
                        Doc::greater_than(),
                    ])
                }
                // Handle tuple inside constructor
                else if let CoreTypeDesc::Ptyp_tuple(types) = &single_arg.ptyp_desc {
                    let constr_name = print_lident_path(lid, cmt_tbl, arena);
                    Doc::group(Doc::concat(vec![
                        constr_name,
                        Doc::less_than(),
                        print_tuple_type(state, types, true, cmt_tbl, arena),
                        Doc::greater_than(),
                    ]))
                } else {
                    // Regular single arg case
                    let constr_name = print_lident_path(lid, cmt_tbl, arena);
                    Doc::group(Doc::concat(vec![
                        constr_name,
                        Doc::less_than(),
                        Doc::indent(Doc::concat(vec![
                            Doc::soft_line(),
                            print_typ_expr(state, single_arg, cmt_tbl, arena),
                        ])),
                        Doc::trailing_comma(),
                        Doc::soft_line(),
                        Doc::greater_than(),
                    ]))
                }
            } else {
                let constr_name = print_lident_path(lid, cmt_tbl, arena);
                if args.is_empty() {
                    constr_name
                } else {
                    Doc::group(Doc::concat(vec![
                        constr_name,
                        Doc::less_than(),
                        Doc::indent(Doc::concat(vec![
                            Doc::soft_line(),
                            Doc::join(
                                Doc::concat(vec![Doc::text(","), Doc::line()]),
                                args.iter()
                                    .map(|arg| print_typ_expr(state, arg, cmt_tbl, arena))
                                    .collect(),
                            ),
                        ])),
                        Doc::trailing_comma(),
                        Doc::soft_line(),
                        Doc::greater_than(),
                    ]))
                }
            }
        }

        CoreTypeDesc::Ptyp_tuple(types) => print_tuple_type(state, types, false, cmt_tbl, arena),

        CoreTypeDesc::Ptyp_poly(vars, inner_typ) => {
            if vars.is_empty() {
                print_typ_expr(state, inner_typ, cmt_tbl, arena)
            } else {
                Doc::concat(vec![
                    Doc::join(
                        Doc::space(),
                        vars.iter()
                            .map(|var| {
                                Doc::concat(vec![Doc::text("'"), Doc::text(var.txt.clone())])
                            })
                            .collect(),
                    ),
                    Doc::text("."),
                    Doc::space(),
                    print_typ_expr(state, inner_typ, cmt_tbl, arena),
                ])
            }
        }

        CoreTypeDesc::Ptyp_package(package_type) => {
            print_package_type(state, package_type, true, cmt_tbl, arena)
        }

        CoreTypeDesc::Ptyp_variant(row_fields, closed_flag, labels_opt) => {
            print_variant_type(state, row_fields, closed_flag, labels_opt, typ, cmt_tbl, arena)
        }
    };

    // Handle attributes
    let should_print_its_own_attributes =
        matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. });

    if !typ.ptyp_attributes.is_empty() && !should_print_its_own_attributes {
        let (doc_comment_attrs, other_attrs) =
            parsetree_viewer::partition_doc_comment_attributes(&typ.ptyp_attributes);
        let comment_doc = if doc_comment_attrs.is_empty() {
            Doc::nil()
        } else {
            print_doc_comments(state, &doc_comment_attrs, cmt_tbl, arena)
        };
        let attrs_doc = if other_attrs.is_empty() {
            Doc::nil()
        } else {
            print_attributes_from_refs(state, &other_attrs, cmt_tbl, arena)
        };
        Doc::group(Doc::concat(vec![comment_doc, attrs_doc, rendered_type]))
    } else {
        rendered_type
    }
}

/// Print an arrow type.
fn print_arrow_type(
    state: &PrinterState,
    typ: &CoreType,
    arity: Arity,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let max_arity = match &arity {
        Arity::Full(n) => Some(*n),
        Arity::Unknown => None,
    };

    let (attrs_before, args, return_type) = parsetree_viewer::arrow_type(typ, max_arity);

    let return_type_needs_parens =
        matches!(return_type.ptyp_desc, CoreTypeDesc::Ptyp_alias(_, _));

    let return_doc = {
        let doc = print_typ_expr(state, return_type, cmt_tbl, arena);
        if return_type_needs_parens {
            Doc::concat(vec![Doc::lparen(), doc, Doc::rparen()])
        } else {
            doc
        }
    };

    if args.is_empty() {
        return Doc::nil();
    }

    // Single unlabeled argument with no attrs
    if args.len() == 1 {
        let arg = &args[0];
        if matches!(arg.lbl, ArgLabel::Nolabel) && arg.attrs.is_empty() {
            let has_attrs_before = !attrs_before.is_empty();
            let attrs = if has_attrs_before {
                print_attributes_inline(state, attrs_before, cmt_tbl, arena)
            } else {
                Doc::nil()
            };
            let typ_doc = {
                let doc = print_typ_expr(state, arg.typ, cmt_tbl, arena);
                match &arg.typ.ptyp_desc {
                    CoreTypeDesc::Ptyp_arrow { .. }
                    | CoreTypeDesc::Ptyp_tuple(_)
                    | CoreTypeDesc::Ptyp_alias(_, _) => add_parens(doc),
                    _ => doc,
                }
            };
            return Doc::group(Doc::concat(vec![
                Doc::group(attrs),
                Doc::group(if has_attrs_before {
                    Doc::concat(vec![
                        Doc::lparen(),
                        Doc::indent(Doc::concat(vec![
                            Doc::soft_line(),
                            typ_doc,
                            Doc::text(" => "),
                            return_doc,
                        ])),
                        Doc::soft_line(),
                        Doc::rparen(),
                    ])
                } else {
                    Doc::concat(vec![typ_doc, Doc::text(" => "), return_doc])
                }),
            ]));
        }
    }

    // Multiple arguments or labeled argument
    let attrs = print_attributes_inline(state, attrs_before, cmt_tbl, arena);
    let rendered_args = Doc::concat(vec![
        attrs,
        Doc::text("("),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(
                Doc::concat(vec![Doc::text(","), Doc::line()]),
                args.iter()
                    .map(|param| print_type_parameter(state, param, cmt_tbl, arena))
                    .collect(),
            ),
        ])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::text(")"),
    ]);

    Doc::group(Doc::concat(vec![
        rendered_args,
        Doc::text(" => "),
        return_doc,
    ]))
}

/// Print a type parameter (argument in an arrow type).
fn print_type_parameter(
    state: &PrinterState,
    param: &parsetree_viewer::TypeParameter<'_>,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs = print_attributes(state, param.attrs, cmt_tbl, arena);
    let label_doc = match param.lbl {
        ArgLabel::Nolabel => Doc::nil(),
        ArgLabel::Labelled(name) => Doc::concat(vec![Doc::text("~"), print_ident_like(arena.get_string(name.txt), false, false), Doc::text(": ")]),
        ArgLabel::Optional(name) => Doc::concat(vec![Doc::text("~"), print_ident_like(arena.get_string(name.txt), false, false), Doc::text(": ")]),
    };
    let optional_indicator = match param.lbl {
        ArgLabel::Optional(_) => Doc::text("=?"),
        _ => Doc::nil(),
    };
    let typ_doc = print_typ_expr(state, param.typ, cmt_tbl, arena);

    Doc::group(Doc::concat(vec![attrs, label_doc, typ_doc, optional_indicator]))
}

/// Print tuple type.
fn print_tuple_type(
    state: &PrinterState,
    types: &[CoreType],
    inline: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let tuple = Doc::concat(vec![
        Doc::lparen(),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(
                Doc::concat(vec![Doc::text(","), Doc::line()]),
                types
                    .iter()
                    .map(|t| print_typ_expr(state, t, cmt_tbl, arena))
                    .collect(),
            ),
        ])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::rparen(),
    ]);
    if inline {
        tuple
    } else {
        Doc::group(tuple)
    }
}

/// Print object type.
fn print_object(
    state: &PrinterState,
    fields: &[ObjectField],
    open_flag: &ClosedFlag,
    inline: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if fields.is_empty() {
        let dot_or_dotdot = match open_flag {
            ClosedFlag::Closed => Doc::text("."),
            ClosedFlag::Open => Doc::text(".."),
        };
        return Doc::concat(vec![Doc::lbrace(), dot_or_dotdot, Doc::rbrace()]);
    }

    let open_doc = match open_flag {
        ClosedFlag::Closed => Doc::nil(),
        ClosedFlag::Open => {
            // Check if first field is inherit
            if matches!(fields.first(), Some(ObjectField::Oinherit(_))) {
                Doc::text(".. ")
            } else {
                Doc::text("..")
            }
        }
    };

    let fields_doc = Doc::join(
        Doc::concat(vec![Doc::text(","), Doc::line()]),
        fields
            .iter()
            .map(|field| print_object_field(state, field, cmt_tbl, arena))
            .collect(),
    );

    let doc = Doc::concat(vec![
        Doc::lbrace(),
        open_doc,
        Doc::indent(Doc::concat(vec![Doc::soft_line(), fields_doc])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::rbrace(),
    ]);

    if inline {
        doc
    } else {
        Doc::group(doc)
    }
}

/// Print an object field.
fn print_object_field(
    state: &PrinterState,
    field: &ObjectField,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match field {
        ObjectField::Otag(label, attrs, typ) => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            let label_doc = Doc::text(format!("\"{}\"", label.txt));
            let typ_doc = print_typ_expr(state, typ, cmt_tbl, arena);
            Doc::concat(vec![attrs_doc, label_doc, Doc::text(": "), typ_doc])
        }
        ObjectField::Oinherit(typ) => {
            Doc::concat(vec![Doc::text("..."), print_typ_expr(state, typ, cmt_tbl, arena)])
        }
    }
}

/// Print polymorphic variant type.
fn print_variant_type(
    state: &PrinterState,
    row_fields: &[RowField],
    closed_flag: &ClosedFlag,
    labels_opt: &Option<Vec<String>>,
    typ: &CoreType,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let force_break = arena.loc_start(typ.ptyp_loc).line < arena.loc_end(typ.ptyp_loc).line;

    let docs: Vec<Doc> = row_fields
        .iter()
        .enumerate()
        .map(|(i, field)| print_row_field(state, i, field, cmt_tbl, arena))
        .collect();

    let cases = Doc::join(Doc::line(), docs);

    let opening_symbol = if *closed_flag == ClosedFlag::Open {
        Doc::concat(vec![Doc::greater_than(), Doc::line()])
    } else if labels_opt.is_none() {
        Doc::soft_line()
    } else {
        Doc::concat(vec![Doc::less_than(), Doc::line()])
    };

    let labels = match labels_opt {
        None => Doc::nil(),
        Some(labels) if labels.is_empty() => Doc::nil(),
        Some(labels) => Doc::concat(
            labels
                .iter()
                .map(|label| {
                    Doc::concat(vec![Doc::line(), Doc::text("#"), print_poly_var_ident(label)])
                })
                .collect(),
        ),
    };

    let closing_symbol = match labels_opt {
        None => Doc::nil(),
        Some(labels) if labels.is_empty() => Doc::nil(),
        _ => Doc::text(" >"),
    };

    Doc::breakable_group(
        Doc::concat(vec![
            Doc::lbracket(),
            Doc::indent(Doc::concat(vec![
                opening_symbol,
                cases,
                closing_symbol,
                labels,
            ])),
            Doc::soft_line(),
            Doc::rbracket(),
        ]),
        force_break,
    )
}

/// Print a row field (polymorphic variant constructor).
fn print_row_field(
    state: &PrinterState,
    index: usize,
    field: &RowField,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match field {
        RowField::Rtag(label, attrs, has_empty_payload, types) => {
            let (doc_attrs, other_attrs) =
                parsetree_viewer::partition_doc_comment_attributes(attrs);
            let comment_doc = if doc_attrs.is_empty() {
                Doc::nil()
            } else {
                print_doc_comments(state, &doc_attrs, cmt_tbl, arena)
            };
            let bar = if index > 0 || !doc_attrs.is_empty() {
                Doc::text("| ")
            } else {
                Doc::if_breaks(Doc::text("| "), Doc::nil())
            };

            let types_doc = if types.is_empty() {
                Doc::nil()
            } else {
                let printed_types: Vec<Doc> = types
                    .iter()
                    .map(|t| {
                        if matches!(t.ptyp_desc, CoreTypeDesc::Ptyp_tuple(_)) {
                            print_typ_expr(state, t, cmt_tbl, arena)
                        } else {
                            Doc::concat(vec![
                                Doc::lparen(),
                                print_typ_expr(state, t, cmt_tbl, arena),
                                Doc::rparen(),
                            ])
                        }
                    })
                    .collect();
                let cases = Doc::join(
                    Doc::concat(vec![Doc::line(), Doc::text("& ")]),
                    printed_types,
                );
                if *has_empty_payload {
                    Doc::concat(vec![Doc::line(), Doc::text("& "), cases])
                } else {
                    cases
                }
            };

            let attrs_doc = print_attributes_from_refs(state, &other_attrs, cmt_tbl, arena);
            let tag_doc = Doc::group(Doc::concat(vec![
                attrs_doc,
                Doc::concat(vec![Doc::text("#"), print_poly_var_ident(&label.txt)]),
                types_doc,
            ]));

            Doc::concat(vec![comment_doc, bar, tag_doc])
        }
        RowField::Rinherit(typ) => {
            let bar = if index > 0 {
                Doc::text("| ")
            } else {
                Doc::if_breaks(Doc::text("| "), Doc::nil())
            };
            Doc::concat(vec![bar, print_typ_expr(state, typ, cmt_tbl, arena)])
        }
    }
}


/// Print a package type.
fn print_package_type(
    state: &PrinterState,
    package_type: &PackageType,
    print_module_keyword: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (lid, constraints) = package_type;
    let lid_doc = print_longident(arena, arena.get_longident(lid.txt));

    let doc = if constraints.is_empty() {
        Doc::group(lid_doc)
    } else {
        // Print constraints with proper line breaking
        let constraint_docs: Vec<Doc> = constraints
            .iter()
            .enumerate()
            .map(|(i, (name, typ))| {
                let prefix = if i == 0 { "type " } else { "and type " };
                Doc::concat(vec![
                    Doc::text(prefix),
                    print_longident(arena, arena.get_longident(name.txt)),
                    Doc::text(" = "),
                    print_typ_expr(state, typ, cmt_tbl, arena),
                ])
            })
            .collect();

        Doc::group(Doc::concat(vec![
            lid_doc,
            Doc::text(" with"),
            Doc::indent(Doc::concat(vec![
                Doc::line(),
                Doc::join(Doc::line(), constraint_docs),
            ])),
            Doc::soft_line(),
        ]))
    };

    if print_module_keyword {
        Doc::concat(vec![Doc::text("module("), doc, Doc::rparen()])
    } else {
        doc
    }
}

/// Print doc comment attributes.
fn print_doc_comments(
    _state: &PrinterState,
    doc_attrs: &[&Attribute],
    _cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let docs: Vec<Doc> = doc_attrs
        .iter()
        .filter_map(|attr| {
            // Doc comments are stored as Payload::PStr containing a Pstr_eval
            // with a Pexp_constant(Constant::String(...))
            if let Payload::PStr(items) = &attr.1 {
                if let Some(item) = items.first() {
                    if let StructureItemDesc::Pstr_eval(expr, _) = &item.pstr_desc {
                        if let ExpressionDesc::Pexp_constant(Constant::String(content, _)) =
                            &expr.pexp_desc
                        {
                            return Some(Doc::concat(vec![
                                Doc::text("/** "),
                                Doc::text(content.clone()),
                                Doc::text(" */"),
                                Doc::hard_line(),
                            ]));
                        }
                    }
                }
            }
            None
        })
        .collect();
    Doc::concat(docs)
}

/// Print attributes from borrowed references.
fn print_attributes_from_refs(
    state: &PrinterState,
    attrs: &[&Attribute],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if attrs.is_empty() {
        return Doc::nil();
    }
    let attrs_doc: Vec<Doc> = attrs
        .iter()
        .map(|attr| print_attribute(state, attr, cmt_tbl, arena))
        .collect();
    // Join with space between attributes, add trailing space
    Doc::concat(vec![Doc::group(Doc::join(Doc::space(), attrs_doc)), Doc::space()])
}

/// Print attributes inline (for arrow type parameters).
/// Attributes are joined with line (space when not breaking) and followed by a space.
fn print_attributes_inline(
    state: &PrinterState,
    attrs: &Attributes,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if attrs.is_empty() {
        return Doc::nil();
    }
    let attrs_doc: Vec<Doc> = attrs
        .iter()
        .map(|attr| print_attribute(state, attr, cmt_tbl, arena))
        .collect();
    // Join with line (space when not breaking), add trailing space
    Doc::concat(vec![
        Doc::group(Doc::join(Doc::line(), attrs_doc)),
        Doc::space(),
    ])
}


// ============================================================================
// Module Printing
// ============================================================================

/// Print a module expression.
fn print_mod_expr(
    state: &PrinterState,
    mod_expr: &ModuleExpr,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let doc = match &mod_expr.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => {
            print_longident_location(lid, cmt_tbl, arena)
        }

        ModuleExprDesc::Pmod_structure(structure) if structure.is_empty() => {
            let should_break = arena.loc_start(mod_expr.pmod_loc).line < arena.loc_end(mod_expr.pmod_loc).line;
            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::lbrace(),
                    print_comments_inside(cmt_tbl, mod_expr.pmod_loc, arena),
                    Doc::rbrace(),
                ]),
                should_break,
            )
        }

        ModuleExprDesc::Pmod_structure(structure) => {
            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::soft_line(),
                        print_structure(state, structure, cmt_tbl, arena),
                    ])),
                    Doc::soft_line(),
                    Doc::rbrace(),
                ]),
                true,
            )
        }

        ModuleExprDesc::Pmod_unpack(expr) => {
            // Check if we should "hug" the expression
            let should_hug = match &expr.pexp_desc {
                ExpressionDesc::Pexp_let(_, _, _) => true,
                ExpressionDesc::Pexp_constraint(inner, typ) => {
                    matches!(&inner.pexp_desc, ExpressionDesc::Pexp_let(_, _, _))
                        && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_))
                }
                _ => false,
            };

            // Extract module constraint if present
            let (expr_to_print, module_constraint) = match &expr.pexp_desc {
                ExpressionDesc::Pexp_constraint(inner_expr, typ) => {
                    match &typ.ptyp_desc {
                        CoreTypeDesc::Ptyp_package(package_type) => {
                            let package_doc = print_package_type(state, package_type, false, cmt_tbl, arena);
                            let package_doc = print_comments(package_doc, cmt_tbl, typ.ptyp_loc, arena);
                            let type_doc = Doc::group(Doc::concat(vec![
                                Doc::text(":"),
                                Doc::indent(Doc::concat(vec![Doc::line(), package_doc])),
                            ]));
                            (inner_expr.as_ref(), type_doc)
                        }
                        _ => (expr.as_ref(), Doc::nil()),
                    }
                }
                _ => (expr.as_ref(), Doc::nil()),
            };

            let unpack_doc = Doc::group(Doc::concat(vec![
                print_expression_with_comments(state, expr_to_print, cmt_tbl, arena),
                module_constraint,
            ]));

            Doc::group(Doc::concat(vec![
                Doc::text("unpack("),
                if should_hug {
                    unpack_doc
                } else {
                    Doc::concat(vec![
                        Doc::indent(Doc::concat(vec![Doc::soft_line(), unpack_doc])),
                        Doc::soft_line(),
                    ])
                },
                Doc::rparen(),
            ]))
        }

        ModuleExprDesc::Pmod_extension(ext) => {
            print_extension_at_module_level(state, ext, cmt_tbl, arena, false)
        }

        ModuleExprDesc::Pmod_apply(_, _) => {
            let (args, call_expr) = parsetree_viewer::mod_expr_apply(mod_expr);

            let is_unit_sugar = match args.as_slice() {
                [arg] => matches!(&arg.pmod_desc, ModuleExprDesc::Pmod_structure(s) if s.is_empty()),
                _ => false,
            };

            let should_hug = match args.as_slice() {
                [arg] => matches!(&arg.pmod_desc, ModuleExprDesc::Pmod_structure(_)),
                _ => false,
            };

            Doc::group(Doc::concat(vec![
                print_mod_expr(state, call_expr, cmt_tbl, arena),
                if is_unit_sugar {
                    print_mod_apply_arg(state, args[0], cmt_tbl, arena)
                } else {
                    Doc::concat(vec![
                        Doc::lparen(),
                        if should_hug {
                            print_mod_apply_arg(state, args[0], cmt_tbl, arena)
                        } else {
                            Doc::indent(Doc::concat(vec![
                                Doc::soft_line(),
                                Doc::join(
                                    Doc::concat(vec![Doc::comma(), Doc::line()]),
                                    args.iter()
                                        .map(|arg| print_mod_apply_arg(state, arg, cmt_tbl, arena))
                                        .collect(),
                                ),
                            ]))
                        },
                        if !should_hug {
                            Doc::concat(vec![Doc::trailing_comma(), Doc::soft_line()])
                        } else {
                            Doc::nil()
                        },
                        Doc::rparen(),
                    ])
                },
            ]))
        }

        ModuleExprDesc::Pmod_constraint(mod_expr_inner, mod_type) => {
            Doc::concat(vec![
                print_mod_expr(state, mod_expr_inner, cmt_tbl, arena),
                Doc::text(": "),
                print_module_type(state, mod_type, cmt_tbl, arena),
            ])
        }

        ModuleExprDesc::Pmod_functor(_, _, _) => {
            print_mod_functor(state, mod_expr, cmt_tbl, arena)
        }
    };

    // Handle await attribute
    let doc = if parsetree_viewer::has_await_attribute(&mod_expr.pmod_attributes) {
        match &mod_expr.pmod_desc {
            ModuleExprDesc::Pmod_constraint(_, _) => {
                Doc::concat(vec![Doc::text("await "), Doc::lparen(), doc, Doc::rparen()])
            }
            _ => Doc::concat(vec![Doc::text("await "), doc]),
        }
    } else {
        doc
    };

    print_comments(doc, cmt_tbl, mod_expr.pmod_loc, arena)
}

/// Print a functor module expression.
fn print_mod_functor(
    state: &PrinterState,
    mod_expr: &ModuleExpr,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (parameters, return_mod_expr) = parsetree_viewer::mod_expr_functor(mod_expr);

    // Extract return constraint if present
    let (return_constraint, return_body) = match &return_mod_expr.pmod_desc {
        ModuleExprDesc::Pmod_constraint(inner_mod_expr, mod_type) => {
            let constraint_doc = print_module_type(state, mod_type, cmt_tbl, arena);
            let constraint_doc = if parens::mod_expr_functor_constraint(mod_type) {
                add_parens(constraint_doc)
            } else {
                constraint_doc
            };
            let mod_constraint = Doc::concat(vec![Doc::text(": "), constraint_doc]);
            (mod_constraint, print_mod_expr(state, inner_mod_expr, cmt_tbl, arena))
        }
        _ => (Doc::nil(), print_mod_expr(state, return_mod_expr, cmt_tbl, arena)),
    };

    let parameters_doc = match parameters.as_slice() {
        // Unit parameter: `()`
        [param] if param.lbl.txt == "*" && param.mod_type.is_none() => {
            Doc::group(Doc::concat(vec![
                print_attributes(state, param.attrs, cmt_tbl, arena),
                Doc::text("()"),
            ]))
        }
        // Single unlabeled parameter without type: just the name
        [param] if param.attrs.is_empty() && param.mod_type.is_none() => Doc::text(&param.lbl.txt),
        // Multiple parameters
        _ => {
            Doc::group(Doc::concat(vec![
                Doc::lparen(),
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(
                        Doc::concat(vec![Doc::comma(), Doc::line()]),
                        parameters
                            .iter()
                            .map(|param| print_mod_functor_param(state, param, cmt_tbl, arena))
                            .collect(),
                    ),
                ])),
                Doc::trailing_comma(),
                Doc::soft_line(),
                Doc::rparen(),
            ]))
        }
    };

    Doc::group(Doc::concat(vec![
        parameters_doc,
        return_constraint,
        Doc::text(" => "),
        return_body,
    ]))
}

/// Print a functor parameter.
fn print_mod_functor_param(
    state: &PrinterState,
    param: &parsetree_viewer::FunctorParam<'_>,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Use the label's location for comment attachment
    // (The old code tried to span to mod_type.pmty_loc, but with LocIdx we use the simpler approach)
    let cmt_loc = param.lbl.loc;

    let attrs = print_attributes(state, param.attrs, cmt_tbl, arena);
    let lbl_doc = if param.lbl.txt == "*" {
        Doc::text("()")
    } else {
        Doc::text(&param.lbl.txt)
    };
    let lbl_doc = print_comments(lbl_doc, cmt_tbl, param.lbl.loc, arena);

    let doc = Doc::group(Doc::concat(vec![
        attrs,
        lbl_doc,
        match param.mod_type {
            None => Doc::nil(),
            Some(mod_type) => Doc::concat(vec![
                Doc::text(": "),
                print_module_type(state, mod_type, cmt_tbl, arena),
            ]),
        },
    ]));

    print_comments(doc, cmt_tbl, cmt_loc, arena)
}

/// Print a module apply argument.
fn print_mod_apply_arg(
    state: &PrinterState,
    mod_expr: &ModuleExpr,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match &mod_expr.pmod_desc {
        ModuleExprDesc::Pmod_structure(s) if s.is_empty() => Doc::text("()"),
        _ => print_mod_expr(state, mod_expr, cmt_tbl, arena),
    }
}

/// Print a module type.
fn print_module_type(
    state: &PrinterState,
    mty: &ModuleType,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let doc = match &mty.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => {
            print_longident_location(lid, cmt_tbl, arena)
        }

        ModuleTypeDesc::Pmty_signature(signature) if signature.is_empty() => {
            let should_break = arena.loc_start(mty.pmty_loc).line < arena.loc_end(mty.pmty_loc).line;
            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::lbrace(),
                    print_comments_inside(cmt_tbl, mty.pmty_loc, arena),
                    Doc::rbrace(),
                ]),
                should_break,
            )
        }

        ModuleTypeDesc::Pmty_signature(signature) => {
            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::soft_line(),
                        print_signature(state, signature, cmt_tbl, arena),
                    ])),
                    Doc::soft_line(),
                    Doc::rbrace(),
                ]),
                true,
            )
        }

        ModuleTypeDesc::Pmty_functor(_, _, _) => {
            print_module_type_functor(state, mty, cmt_tbl, arena)
        }

        ModuleTypeDesc::Pmty_with(mod_type, with_constraints) => {
            let base_doc = print_module_type(state, mod_type, cmt_tbl, arena);
            let constraints_doc = Doc::join(
                Doc::line(),
                with_constraints
                    .iter()
                    .enumerate()
                    .map(|(i, constraint)| {
                        let prefix = if i == 0 { "with " } else { "and " };
                        Doc::concat(vec![
                            Doc::text(prefix),
                            print_with_constraint(state, constraint, cmt_tbl, arena),
                        ])
                    })
                    .collect(),
            );
            Doc::group(Doc::concat(vec![
                base_doc,
                Doc::indent(Doc::concat(vec![Doc::line(), constraints_doc])),
            ]))
        }

        ModuleTypeDesc::Pmty_typeof(mod_expr) => {
            Doc::concat(vec![
                Doc::text("module type of "),
                print_mod_expr(state, mod_expr, cmt_tbl, arena),
            ])
        }

        ModuleTypeDesc::Pmty_extension(ext) => {
            print_extension_at_module_level(state, ext, cmt_tbl, arena, false)
        }

        ModuleTypeDesc::Pmty_alias(lid) => {
            Doc::concat(vec![
                Doc::text("module "),
                print_longident_location(lid, cmt_tbl, arena),
            ])
        }
    };

    let doc = if parsetree_viewer::has_await_attribute(&mty.pmty_attributes) {
        Doc::concat(vec![Doc::text("await "), doc])
    } else {
        doc
    };

    print_comments(doc, cmt_tbl, mty.pmty_loc, arena)
}

/// Print a functor module type.
fn print_module_type_functor(
    state: &PrinterState,
    mty: &ModuleType,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (parameters, return_mty) = mod_type_functor(mty);

    let parameters_doc = match parameters.as_slice() {
        // Unit parameter: `()`
        [(lbl, None)] if lbl.txt == "*" => Doc::text("()"),
        // Single unlabeled parameter without type: just the name
        [(lbl, None)] => Doc::text(&lbl.txt),
        // Multiple parameters
        _ => {
            Doc::group(Doc::concat(vec![
                Doc::lparen(),
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(
                        Doc::concat(vec![Doc::comma(), Doc::line()]),
                        parameters
                            .iter()
                            .map(|(lbl, opt_mty)| {
                                let lbl_doc = if lbl.txt == "*" {
                                    Doc::text("()")
                                } else {
                                    Doc::text(&lbl.txt)
                                };
                                let lbl_doc = print_comments(lbl_doc, cmt_tbl, lbl.loc, arena);
                                match opt_mty {
                                    None => lbl_doc,
                                    Some(mty) => Doc::group(Doc::concat(vec![
                                        lbl_doc,
                                        Doc::text(": "),
                                        print_module_type(state, mty, cmt_tbl, arena),
                                    ])),
                                }
                            })
                            .collect(),
                    ),
                ])),
                Doc::trailing_comma(),
                Doc::soft_line(),
                Doc::rparen(),
            ]))
        }
    };

    Doc::group(Doc::concat(vec![
        parameters_doc,
        Doc::text(" => "),
        print_module_type(state, return_mty, cmt_tbl, arena),
    ]))
}

/// Extract functor parameters from a module type.
fn mod_type_functor(mty: &ModuleType) -> (Vec<(&StringLoc, Option<&ModuleType>)>, &ModuleType) {
    fn loop_functor<'a>(
        acc: Vec<(&'a StringLoc, Option<&'a ModuleType>)>,
        mty: &'a ModuleType,
    ) -> (Vec<(&'a StringLoc, Option<&'a ModuleType>)>, &'a ModuleType) {
        match &mty.pmty_desc {
            ModuleTypeDesc::Pmty_functor(lbl, mod_type, return_mty) => {
                let mut new_acc = acc;
                new_acc.push((lbl, mod_type.as_ref().map(|t| t.as_ref())));
                loop_functor(new_acc, return_mty)
            }
            _ => (acc, mty),
        }
    }
    loop_functor(Vec::new(), mty)
}

/// Print a with constraint.
fn print_with_constraint(
    state: &PrinterState,
    constraint: &WithConstraint,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match constraint {
        WithConstraint::Pwith_type(lid, type_decl) => {
            Doc::concat(vec![
                Doc::text("type "),
                print_type_declaration_with_lid(state, lid, type_decl, cmt_tbl, arena),
            ])
        }
        WithConstraint::Pwith_module(lid1, lid2) => {
            Doc::concat(vec![
                Doc::text("module "),
                print_longident_location(lid1, cmt_tbl, arena),
                Doc::text(" = "),
                print_longident_location(lid2, cmt_tbl, arena),
            ])
        }
        WithConstraint::Pwith_typesubst(lid, type_decl) => {
            Doc::concat(vec![
                Doc::text("type "),
                print_type_declaration_with_lid(state, lid, type_decl, cmt_tbl, arena),
            ])
        }
        WithConstraint::Pwith_modsubst(lid1, lid2) => {
            Doc::concat(vec![
                Doc::text("module "),
                print_longident_location(lid1, cmt_tbl, arena),
                Doc::text(" := "),
                print_longident_location(lid2, cmt_tbl, arena),
            ])
        }
    }
}

/// Print a type parameter with variance (covariant +, contravariant -, or invariant).
fn print_type_param(
    state: &PrinterState,
    typ: &CoreType,
    variance: &Variance,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let variance_doc = match variance {
        Variance::Covariant => Doc::text("+"),
        Variance::Contravariant => Doc::text("-"),
        Variance::Invariant => Doc::nil(),
    };
    Doc::concat(vec![variance_doc, print_typ_expr(state, typ, cmt_tbl, arena)])
}

/// Print a type declaration with its longident.
fn print_type_declaration_with_lid(
    state: &PrinterState,
    lid: &crate::parse_arena::Located<crate::parse_arena::LidentIdx>,
    decl: &TypeDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let name_doc = print_lident(arena, arena.get_longident(lid.txt));
    let params_doc = if decl.ptype_params.is_empty() {
        Doc::nil()
    } else {
        let params: Vec<Doc> = decl
            .ptype_params
            .iter()
            .map(|(typ, variance)| print_type_param(state, typ, variance, cmt_tbl, arena))
            .collect();
        Doc::concat(vec![
            Doc::less_than(),
            Doc::join(Doc::concat(vec![Doc::text(","), Doc::space()]), params),
            Doc::greater_than(),
        ])
    };

    let manifest_doc = match &decl.ptype_manifest {
        Some(typ) => Doc::concat(vec![Doc::text(" = "), print_typ_expr(state, typ, cmt_tbl, arena)]),
        None => Doc::nil(),
    };

    Doc::concat(vec![name_doc, params_doc, manifest_doc])
}

/// Print extension at module level.
fn print_extension_at_module_level(
    state: &PrinterState,
    ext: &Extension,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
    _at_module_lvl: bool,
) -> Doc {
    let (name, payload) = ext;
    let name_doc = Doc::text(format!("%{}", name.txt));

    let payload_doc = match payload {
        Payload::PStr(items) if items.is_empty() => Doc::nil(),
        Payload::PStr(items) => {
            Doc::concat(vec![
                Doc::text("("),
                print_structure(state, items, cmt_tbl, arena),
                Doc::text(")"),
            ])
        }
        Payload::PTyp(typ) => {
            Doc::concat(vec![
                Doc::text("("),
                print_typ_expr(state, typ, cmt_tbl, arena),
                Doc::text(")"),
            ])
        }
        Payload::PSig(items) => {
            Doc::concat(vec![
                Doc::text("("),
                print_signature(state, items, cmt_tbl, arena),
                Doc::text(")"),
            ])
        }
        Payload::PPat(pat, guard) => {
            let pat_doc = print_pattern(state, pat, cmt_tbl, arena);
            match guard {
                Some(expr) => Doc::concat(vec![
                    Doc::text("("),
                    pat_doc,
                    Doc::text(" when "),
                    print_expression_with_comments(state, expr, cmt_tbl, arena),
                    Doc::text(")"),
                ]),
                None => Doc::concat(vec![Doc::text("("), pat_doc, Doc::text(")")]),
            }
        }
    };

    Doc::concat(vec![name_doc, payload_doc])
}

// ============================================================================
// Match/Try Printing (placeholder)
// ============================================================================

/// Print match expression.
fn print_match_expression(
    state: &PrinterState,
    expr: &Expression,
    cases: &[Case],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let scrutinee = print_expression_with_comments(state, expr, cmt_tbl, arena);
    let cases_doc = print_cases(state, cases, cmt_tbl, arena);
    Doc::concat(vec![
        Doc::text("switch "),
        scrutinee,
        Doc::text(" {"),
        cases_doc,
        Doc::hard_line(),
        Doc::text("}"),
    ])
}

/// Print try expression.
fn print_try_expression(
    state: &PrinterState,
    expr: &Expression,
    cases: &[Case],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let body = print_expression_with_comments(state, expr, cmt_tbl, arena);
    let cases_doc = print_cases(state, cases, cmt_tbl, arena);
    Doc::concat(vec![
        Doc::text("try "),
        body,
        Doc::text(" catch {"),
        cases_doc,
        Doc::hard_line(),
        Doc::text("}"),
    ])
}

/// Check if a pattern needs parentheses in a case context.
/// Constraint patterns like `(p: typ)` need parens at the case level.
fn case_pattern_needs_parens(pattern: &Pattern) -> bool {
    match &pattern.ppat_desc {
        // Constraint patterns need parens: | (p: typ) => ...
        // Exception: module(M: S) doesn't need extra parens
        PatternDesc::Ppat_constraint(inner, typ) => {
            !(matches!(&inner.ppat_desc, PatternDesc::Ppat_unpack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)))
        }
        _ => false,
    }
}

/// Print match cases.
fn print_cases(
    state: &PrinterState,
    cases: &[Case],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let cases_doc: Vec<Doc> = cases
        .iter()
        .map(|case| print_case(state, case, cmt_tbl, arena))
        .collect();
    Doc::concat(cases_doc)
}

/// Print a single match case.
fn print_case(
    state: &PrinterState,
    case: &Case,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Print the RHS
    let rhs = match &case.pc_rhs.pexp_desc {
        ExpressionDesc::Pexp_let(_, _, _)
        | ExpressionDesc::Pexp_letmodule(_, _, _)
        | ExpressionDesc::Pexp_letexception(_, _)
        | ExpressionDesc::Pexp_open(_, _, _)
        | ExpressionDesc::Pexp_sequence(_, _) => {
            let braces = parsetree_viewer::is_braced_expr(&case.pc_rhs);
            print_expression_block(state, braces, &case.pc_rhs, cmt_tbl, arena)
        }
        _ => {
            let doc = print_expression_with_comments(state, &case.pc_rhs, cmt_tbl, arena);
            match parens::expr(arena, &case.pc_rhs) {
                ParenKind::Parenthesized => add_parens(doc),
                _ => doc,
            }
        }
    };

    // Print guard if present
    let guard = match &case.pc_guard {
        None => Doc::nil(),
        Some(expr) => Doc::group(Doc::concat(vec![
            Doc::line(),
            Doc::text("if "),
            print_expression_with_comments(state, expr, cmt_tbl, arena),
        ])),
    };

    // Check if RHS should be on the same line
    let should_inline_rhs = match &case.pc_rhs.pexp_desc {
        ExpressionDesc::Pexp_construct(lid, _) => {
            let name = match arena.get_longident(lid.txt) {
                crate::parser::longident::Longident::Lident(s) => arena.get_string(*s),
                _ => "",
            };
            matches!(name, "()" | "true" | "false")
        }
        ExpressionDesc::Pexp_constant(_) | ExpressionDesc::Pexp_ident(_) => true,
        _ if parsetree_viewer::is_huggable_rhs(&case.pc_rhs) => true,
        _ => false,
    };

    // Check if pattern should be indented
    let should_indent_pattern = !matches!(
        &case.pc_lhs.ppat_desc,
        PatternDesc::Ppat_or(_, _)
    );

    // Print pattern
    let pattern_doc = {
        let doc = print_pattern(state, &case.pc_lhs, cmt_tbl, arena);
        if case_pattern_needs_parens(&case.pc_lhs) {
            add_parens(doc)
        } else {
            doc
        }
    };

    let content = Doc::concat(vec![
        if should_indent_pattern {
            Doc::indent(pattern_doc)
        } else {
            pattern_doc
        },
        Doc::indent(guard),
        Doc::text(" =>"),
        Doc::indent(Doc::concat(vec![
            if should_inline_rhs { Doc::space() } else { Doc::line() },
            rhs,
        ])),
    ]);

    Doc::concat(vec![
        Doc::hard_line(),
        Doc::group(Doc::concat(vec![Doc::text("| "), content])),
    ])
}

// ============================================================================
// Value Bindings
// ============================================================================

/// Print value bindings.
fn print_value_bindings(
    state: &PrinterState,
    bindings: &[ValueBinding],
    rec_flag: &RecFlag,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let rec_doc = match rec_flag {
        RecFlag::Nonrecursive => Doc::nil(),
        RecFlag::Recursive => Doc::text("rec "),
    };

    let docs: Vec<Doc> = bindings
        .iter()
        .enumerate()
        .map(|(i, vb)| {
            // Check for let.unwrap attribute
            let has_unwrap = vb.pvb_attributes.iter().any(|attr| attr.0.txt == "let.unwrap");

            // Print attributes (filtering out let.unwrap)
            let attrs: Vec<&Attribute> = vb.pvb_attributes
                .iter()
                .filter(|attr| attr.0.txt != "let.unwrap")
                .collect();
            let attrs_doc = if attrs.is_empty() {
                Doc::nil()
            } else {
                let attr_docs: Vec<Doc> = attrs
                    .iter()
                    .filter(|attr| parsetree_viewer::is_printable_attribute(attr))
                    .map(|attr| print_attribute(state, attr, cmt_tbl, arena))
                    .collect();
                if attr_docs.is_empty() {
                    Doc::nil()
                } else {
                    Doc::concat(vec![Doc::group(Doc::join(Doc::space(), attr_docs)), Doc::hard_line()])
                }
            };

            let header = if i == 0 {
                let let_kw = if has_unwrap { "let? " } else { "let " };
                Doc::concat(vec![Doc::text(let_kw), rec_doc.clone()])
            } else {
                Doc::text("and ")
            };

            let pat = print_pattern(state, &vb.pvb_pat, cmt_tbl, arena);
            // Print expression and check if it needs parens/braces
            let doc = print_expression_with_comments(state, &vb.pvb_expr, cmt_tbl, arena);
            let printed_expr = match parens::expr(arena, &vb.pvb_expr) {
                ParenKind::Parenthesized => add_parens(doc),
                ParenKind::Braced(loc) => print_braces(doc, &vb.pvb_expr, loc, arena),
                ParenKind::Nothing => doc,
            };

            // Check if expression should be indented after `=`
            // This happens when:
            // - Expression has printable attributes
            // - Or is a binary expression (without braces)
            // - Or is a JSX element
            // - Or is an array access
            let opt_braces = parsetree_viewer::process_braces_attr(&vb.pvb_expr);
            let should_indent = opt_braces.is_none() && (
                parsetree_viewer::has_attributes(&vb.pvb_expr.pexp_attributes)
                || parsetree_viewer::is_binary_expression(arena, &vb.pvb_expr)
                || matches!(&vb.pvb_expr.pexp_desc, ExpressionDesc::Pexp_jsx_element(_))
                || parsetree_viewer::is_array_access(arena, &vb.pvb_expr)
            );

            let rhs = if should_indent {
                Doc::indent(Doc::concat(vec![Doc::line(), printed_expr]))
            } else {
                Doc::concat(vec![Doc::space(), printed_expr])
            };

            Doc::group(Doc::concat(vec![attrs_doc, header, pat, Doc::text(" ="), rhs]))
        })
        .collect();
    Doc::join(Doc::hard_line(), docs)
}

// ============================================================================
// Attributes (placeholder)
// ============================================================================

/// Print attributes with a trailing separator.
/// When inline=true, uses space after attributes.
/// When inline=false (default), uses Doc::line which can break if needed.
fn print_attributes(
    state: &PrinterState,
    attrs: &[Attribute],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    print_attributes_with_sep(state, attrs, cmt_tbl, arena, false)
}

/// Print attributes with configurable separator.
fn print_attributes_with_sep(
    state: &PrinterState,
    attrs: &[Attribute],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
    inline: bool,
) -> Doc {
    if attrs.is_empty() {
        return Doc::nil();
    }
    let docs: Vec<Doc> = attrs
        .iter()
        .filter(|attr| parsetree_viewer::is_printable_attribute(attr))
        .map(|attr| print_attribute(state, attr, cmt_tbl, arena))
        .collect();
    if docs.is_empty() {
        Doc::nil()
    } else {
        let sep = if inline { Doc::space() } else { Doc::line() };
        Doc::concat(vec![Doc::group(Doc::join(Doc::space(), docs)), sep])
    }
}

/// Print a single attribute.
fn print_attribute(
    state: &PrinterState,
    attr: &Attribute,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (name, payload) = attr;
    let attr_name = Doc::text(format!("@{}", name.txt));

    let payload_doc = match payload {
        Payload::PStr(items) if items.is_empty() => Doc::nil(),
        Payload::PStr(items) => {
            // Print structure items - for simple cases, try to extract the expression
            if items.len() == 1 {
                if let StructureItemDesc::Pstr_eval(expr, _) = &items[0].pstr_desc {
                    return Doc::concat(vec![
                        attr_name,
                        Doc::text("("),
                        print_expression_with_comments(state, expr, cmt_tbl, arena),
                        Doc::text(")"),
                    ]);
                }
            }
            // Fall back to a simple representation for complex payloads
            Doc::concat(vec![Doc::text("("), Doc::text("..."), Doc::text(")")])
        }
        Payload::PTyp(typ) => {
            Doc::concat(vec![
                Doc::text("("),
                print_typ_expr(state, typ, cmt_tbl, arena),
                Doc::text(")"),
            ])
        }
        Payload::PSig(_) => Doc::text("(: ...)"),
        Payload::PPat(pat, guard) => {
            let pat_doc = print_pattern(state, pat, cmt_tbl, arena);
            match guard {
                Some(expr) => Doc::concat(vec![
                    Doc::text("(? "),
                    pat_doc,
                    Doc::text(" when "),
                    print_expression_with_comments(state, expr, cmt_tbl, arena),
                    Doc::text(")"),
                ]),
                None => Doc::concat(vec![Doc::text("(? "), pat_doc, Doc::text(")")]),
            }
        }
    };

    Doc::concat(vec![attr_name, payload_doc])
}

// ============================================================================
// Extension (placeholder)
// ============================================================================

/// Print an extension point.
fn print_extension(
    state: &PrinterState,
    ext: &Extension,
    at_module_lvl: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (name, payload) = ext;
    let ext_name = Doc::concat(vec![
        Doc::text("%"),
        if at_module_lvl {
            Doc::text("%")
        } else {
            Doc::nil()
        },
        Doc::text(&name.txt),
    ]);
    let ext_name = print_comments(ext_name, cmt_tbl, name.loc, arena);
    Doc::group(Doc::concat(vec![
        ext_name,
        print_payload(state, payload, cmt_tbl, arena),
    ]))
}

/// Print a payload of an attribute or extension.
fn print_payload(state: &PrinterState, payload: &Payload, cmt_tbl: &mut CommentTable, arena: &ParseArena) -> Doc {
    match payload {
        Payload::PStr(items) if items.is_empty() => Doc::nil(),
        Payload::PStr(items) => {
            if let [single] = &items[..] {
                match &single.pstr_desc {
                    StructureItemDesc::Pstr_eval(expr, attrs) => {
                        let expr_doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                        let needs_parens = !attrs.is_empty();
                        let should_hug = parsetree_viewer::is_huggable_expression(arena, expr);
                        if should_hug {
                            Doc::concat(vec![
                                Doc::lparen(),
                                print_attributes(state, attrs, cmt_tbl, arena),
                                if needs_parens {
                                    add_parens(expr_doc)
                                } else {
                                    expr_doc
                                },
                                Doc::rparen(),
                            ])
                        } else {
                            Doc::concat(vec![
                                Doc::lparen(),
                                Doc::indent(Doc::concat(vec![
                                    Doc::soft_line(),
                                    print_attributes(state, attrs, cmt_tbl, arena),
                                    if needs_parens {
                                        add_parens(expr_doc)
                                    } else {
                                        expr_doc
                                    },
                                ])),
                                Doc::soft_line(),
                                Doc::rparen(),
                            ])
                        }
                    }
                    StructureItemDesc::Pstr_value(_, _) => {
                        // Single value binding - wrap in parens
                        add_parens(print_structure_item(state, single, cmt_tbl, arena))
                    }
                    _ => {
                        // General single structure item - wrap in parens
                        add_parens(print_structure_item(state, single, cmt_tbl, arena))
                    }
                }
            } else {
                // Multiple items - print full structure
                add_parens(print_structure(state, items, cmt_tbl, arena))
            }
        }
        Payload::PSig(signature) => Doc::concat(vec![
            Doc::lparen(),
            Doc::text(":"),
            Doc::indent(Doc::concat(vec![
                Doc::line(),
                print_signature(state, signature, cmt_tbl, arena),
            ])),
            Doc::soft_line(),
            Doc::rparen(),
        ]),
        Payload::PTyp(typ) => Doc::concat(vec![
            Doc::text("(: "),
            print_typ_expr(state, typ, cmt_tbl, arena),
            Doc::text(")"),
        ]),
        Payload::PPat(pat, guard) => {
            let pat_doc = print_pattern(state, pat, cmt_tbl, arena);
            let guard_doc = match guard {
                Some(g) => Doc::concat(vec![
                    Doc::text(" when "),
                    print_expression_with_comments(state, g, cmt_tbl, arena),
                ]),
                None => Doc::nil(),
            };
            Doc::concat(vec![Doc::text("(? "), pat_doc, guard_doc, Doc::text(")")])
        }
    }
}

// ============================================================================
// Structure Printing
// ============================================================================

/// Print a structure (list of structure items).
pub fn print_structure(
    state: &PrinterState,
    structure: &[StructureItem],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if structure.is_empty() {
        return print_comments_inside_file(cmt_tbl, arena);
    }

    print_list(
        |item: &StructureItem| item.pstr_loc,
        structure,
        |item: &StructureItem, cmt_tbl: &mut CommentTable| print_structure_item(state, item, cmt_tbl, arena),
        cmt_tbl,
        arena,
        false,
    )
}

/// Print a signature (list of signature items).
pub fn print_signature(
    state: &PrinterState,
    signature: &[SignatureItem],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if signature.is_empty() {
        return print_comments_inside_file(cmt_tbl, arena);
    }

    print_list(
        |item: &SignatureItem| item.psig_loc,
        signature,
        |item: &SignatureItem, cmt_tbl: &mut CommentTable| print_signature_item(state, item, cmt_tbl, arena),
        cmt_tbl,
        arena,
        false,
    )
}

/// Print a signature item.
pub fn print_signature_item(
    state: &PrinterState,
    item: &SignatureItem,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match &item.psig_desc {
        SignatureItemDesc::Psig_value(val_desc) => {
            print_value_description(state, val_desc, cmt_tbl, arena)
        }

        SignatureItemDesc::Psig_type(rec_flag, type_decls) => {
            let rec_doc = match rec_flag {
                RecFlag::Nonrecursive => Doc::nil(),
                RecFlag::Recursive => Doc::text("rec "),
            };
            // Extract attributes from first type declaration to print before "type"
            let attrs_doc = if let Some(first_decl) = type_decls.first() {
                print_attributes(state, &first_decl.ptype_attributes, cmt_tbl, arena)
            } else {
                Doc::nil()
            };
            Doc::concat(vec![
                attrs_doc,
                Doc::text("type "),
                rec_doc,
                print_type_declarations_no_first_attrs(state, type_decls, cmt_tbl, arena),
            ])
        }

        SignatureItemDesc::Psig_typext(type_ext) => {
            print_type_extension(state, type_ext, cmt_tbl, arena)
        }

        SignatureItemDesc::Psig_exception(ext_constr) => {
            print_exception_def(state, ext_constr, cmt_tbl, arena)
        }

        SignatureItemDesc::Psig_module(mod_decl) => {
            print_module_declaration(state, mod_decl, cmt_tbl, arena)
        }

        SignatureItemDesc::Psig_recmodule(mod_decls) => {
            print_rec_module_declarations(state, mod_decls, cmt_tbl, arena)
        }

        SignatureItemDesc::Psig_modtype(mod_type_decl) => {
            print_module_type_declaration(state, mod_type_decl, cmt_tbl, arena)
        }

        SignatureItemDesc::Psig_open(open_desc) => {
            print_open_description(state, open_desc, cmt_tbl, arena)
        }

        SignatureItemDesc::Psig_include(include_desc) => {
            print_include_description(state, include_desc, cmt_tbl, arena)
        }

        SignatureItemDesc::Psig_attribute(attr) => {
            Doc::concat(vec![Doc::text("@@"), print_attribute(state, attr, cmt_tbl, arena)])
        }

        SignatureItemDesc::Psig_extension(ext, attrs) => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            Doc::concat(vec![
                attrs_doc,
                print_extension(state, ext, true, cmt_tbl, arena),
            ])
        }
    }
}

/// Print a module declaration.
fn print_module_declaration(
    state: &PrinterState,
    decl: &ModuleDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &decl.pmd_attributes, cmt_tbl, arena);
    let name_doc = Doc::text(&decl.pmd_name.txt);

    // Check if this is a module alias (Pmty_alias) - uses "=" instead of ":"
    let body_doc = match &decl.pmd_type.pmty_desc {
        ModuleTypeDesc::Pmty_alias(lid) => {
            Doc::concat(vec![
                Doc::text(" = "),
                print_longident(arena, arena.get_longident(lid.txt)),
            ])
        }
        _ => {
            Doc::concat(vec![
                Doc::text(": "),
                print_module_type(state, &decl.pmd_type, cmt_tbl, arena),
            ])
        }
    };

    Doc::concat(vec![
        attrs_doc,
        Doc::text("module "),
        name_doc,
        body_doc,
    ])
}

/// Print recursive module declarations.
fn print_rec_module_declarations(
    state: &PrinterState,
    decls: &[ModuleDeclaration],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let docs: Vec<Doc> = decls
        .iter()
        .enumerate()
        .map(|(i, decl)| {
            let prefix = if i == 0 {
                Doc::text("module rec ")
            } else {
                Doc::text("and ")
            };
            let name_doc = Doc::text(&decl.pmd_name.txt);
            let type_doc = print_module_type(state, &decl.pmd_type, cmt_tbl, arena);
            Doc::concat(vec![prefix, name_doc, Doc::text(": "), type_doc])
        })
        .collect();
    Doc::join(Doc::hard_line(), docs)
}

/// Print an include description.
fn print_include_description(
    state: &PrinterState,
    include_desc: &IncludeDescription,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &include_desc.pincl_attributes, cmt_tbl, arena);
    let mod_doc = print_module_type(state, &include_desc.pincl_mod, cmt_tbl, arena);

    Doc::concat(vec![attrs_doc, Doc::text("include "), mod_doc])
}

/// Print a structure item.
pub fn print_structure_item(
    state: &PrinterState,
    item: &StructureItem,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match &item.pstr_desc {
        StructureItemDesc::Pstr_value(rec_flag, bindings) => {
            print_value_bindings(state, bindings, rec_flag, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_type(rec_flag, type_decls) => {
            let rec_doc = match rec_flag {
                RecFlag::Nonrecursive => Doc::nil(),
                RecFlag::Recursive => Doc::text("rec "),
            };
            // Extract attributes from first type declaration to print before "type"
            let attrs_doc = if let Some(first_decl) = type_decls.first() {
                print_attributes(state, &first_decl.ptype_attributes, cmt_tbl, arena)
            } else {
                Doc::nil()
            };
            Doc::concat(vec![
                attrs_doc,
                Doc::text("type "),
                rec_doc,
                print_type_declarations_no_first_attrs(state, type_decls, cmt_tbl, arena),
            ])
        }

        StructureItemDesc::Pstr_primitive(val_desc) => {
            print_value_description(state, val_desc, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_typext(type_ext) => {
            print_type_extension(state, type_ext, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_exception(ext_constr) => {
            print_exception_def(state, ext_constr, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_module(mod_binding) => {
            print_module_binding(state, mod_binding, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_recmodule(mod_bindings) => {
            print_rec_module_bindings(state, mod_bindings, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_modtype(mod_type_decl) => {
            print_module_type_declaration(state, mod_type_decl, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_open(open_desc) => {
            print_open_description(state, open_desc, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_include(include_decl) => {
            print_include_declaration(state, include_decl, cmt_tbl, arena)
        }

        StructureItemDesc::Pstr_attribute(attr) => {
            Doc::concat(vec![Doc::text("@@"), print_attribute(state, attr, cmt_tbl, arena)])
        }

        StructureItemDesc::Pstr_extension(ext, attrs) => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            Doc::concat(vec![
                attrs_doc,
                print_extension(state, ext, true, cmt_tbl, arena),
            ])
        }

        StructureItemDesc::Pstr_eval(expr, attrs) => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            Doc::concat(vec![
                attrs_doc,
                print_expression_with_comments(state, expr, cmt_tbl, arena),
            ])
        }
    }
}

/// Print type declarations.
fn print_type_declarations(
    state: &PrinterState,
    decls: &[TypeDeclaration],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let docs: Vec<Doc> = decls
        .iter()
        .enumerate()
        .map(|(i, decl)| {
            let prefix = if i == 0 { Doc::nil() } else { Doc::text("and ") };
            Doc::concat(vec![prefix, print_type_declaration(state, decl, cmt_tbl, arena)])
        })
        .collect();
    Doc::join(Doc::hard_line(), docs)
}

/// Print type declarations without attributes on the first declaration.
/// Used when attributes are printed before the "type" keyword.
fn print_type_declarations_no_first_attrs(
    state: &PrinterState,
    decls: &[TypeDeclaration],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let docs: Vec<Doc> = decls
        .iter()
        .enumerate()
        .map(|(i, decl)| {
            let prefix = if i == 0 { Doc::nil() } else { Doc::text("and ") };
            if i == 0 {
                // Skip attributes for first declaration - they're printed before "type"
                Doc::concat(vec![prefix, print_type_declaration_no_attrs(state, decl, cmt_tbl, arena)])
            } else {
                Doc::concat(vec![prefix, print_type_declaration(state, decl, cmt_tbl, arena)])
            }
        })
        .collect();
    Doc::join(Doc::hard_line(), docs)
}

/// Print a type declaration.
fn print_type_declaration(
    state: &PrinterState,
    decl: &TypeDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    print_type_declaration_inner(state, decl, cmt_tbl, arena, true)
}

/// Print a type declaration without attributes.
fn print_type_declaration_no_attrs(
    state: &PrinterState,
    decl: &TypeDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    print_type_declaration_inner(state, decl, cmt_tbl, arena, false)
}

/// Print a type declaration with optional attributes.
fn print_type_declaration_inner(
    state: &PrinterState,
    decl: &TypeDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
    include_attrs: bool,
) -> Doc {
    let attrs_doc = if include_attrs {
        print_attributes(state, &decl.ptype_attributes, cmt_tbl, arena)
    } else {
        Doc::nil()
    };
    let name_doc = print_ident_like(&decl.ptype_name.txt, false, false);

    let params_doc = if decl.ptype_params.is_empty() {
        Doc::nil()
    } else {
        let params: Vec<Doc> = decl
            .ptype_params
            .iter()
            .map(|(typ, variance)| print_type_param(state, typ, variance, cmt_tbl, arena))
            .collect();
        Doc::concat(vec![
            Doc::less_than(),
            Doc::join(Doc::concat(vec![Doc::text(","), Doc::space()]), params),
            Doc::greater_than(),
        ])
    };

    // Helper for private flag
    let private_doc = match decl.ptype_private {
        PrivateFlag::Private => Doc::text("private "),
        PrivateFlag::Public => Doc::nil(),
    };

    // For abstract types: " = private? <manifest type>"
    // For non-abstract types: manifest is handled separately
    let manifest_doc = match &decl.ptype_kind {
        TypeKind::Ptype_abstract => {
            match &decl.ptype_manifest {
                Some(typ) => Doc::concat(vec![
                    Doc::text(" = "),
                    private_doc.clone(),
                    print_typ_expr(state, typ, cmt_tbl, arena),
                ]),
                None => Doc::nil(),
            }
        }
        TypeKind::Ptype_variant(_) => {
            // Manifest (if any) is printed without private flag
            match &decl.ptype_manifest {
                Some(typ) => Doc::concat(vec![
                    Doc::text(" = "),
                    print_typ_expr(state, typ, cmt_tbl, arena),
                ]),
                None => Doc::nil(),
            }
        }
        TypeKind::Ptype_record(_) => {
            // Manifest (if any) is printed without private flag
            match &decl.ptype_manifest {
                Some(typ) => Doc::concat(vec![
                    Doc::text(" = "),
                    print_typ_expr(state, typ, cmt_tbl, arena),
                ]),
                None => Doc::nil(),
            }
        }
        TypeKind::Ptype_open => Doc::nil(),
    };

    let kind_doc = match &decl.ptype_kind {
        TypeKind::Ptype_abstract => Doc::nil(),
        TypeKind::Ptype_variant(constrs) => {
            Doc::concat(vec![
                // No trailing space - print_constructor_declarations starts with Doc::line()
                Doc::text(" ="),
                print_constructor_declarations(state, constrs, &decl.ptype_private, cmt_tbl, arena),
            ])
        }
        TypeKind::Ptype_record(fields) => {
            Doc::concat(vec![
                Doc::text(" = "),
                private_doc.clone(),
                print_record_declaration(state, fields, Some(decl.ptype_loc), cmt_tbl, arena),
            ])
        }
        TypeKind::Ptype_open => Doc::concat(vec![
            Doc::text(" = "),
            private_doc,
            Doc::text(".."),
        ]),
    };

    // Print type constraints (constraint 'a = int)
    let constraints_doc = print_type_constraints(state, &decl.ptype_cstrs, cmt_tbl, arena);

    Doc::concat(vec![attrs_doc, name_doc, params_doc, manifest_doc, kind_doc, constraints_doc])
}

/// Print type definition constraints.
fn print_type_constraints(
    state: &PrinterState,
    constraints: &[(CoreType, CoreType, Location)],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if constraints.is_empty() {
        return Doc::nil();
    }
    let constraint_docs: Vec<Doc> = constraints
        .iter()
        .map(|(typ1, typ2, _loc)| {
            Doc::concat(vec![
                Doc::text("constraint "),
                print_typ_expr(state, typ1, cmt_tbl, arena),
                Doc::text(" = "),
                print_typ_expr(state, typ2, cmt_tbl, arena),
            ])
        })
        .collect();
    Doc::indent(Doc::group(Doc::concat(vec![
        Doc::line(),
        Doc::group(Doc::join(Doc::line(), constraint_docs)),
    ])))
}

/// Print constructor declarations.
fn print_constructor_declarations(
    state: &PrinterState,
    constrs: &[ConstructorDeclaration],
    private_flag: &PrivateFlag,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let private_doc = match private_flag {
        PrivateFlag::Private => Doc::concat(vec![Doc::text("private"), Doc::line()]),
        PrivateFlag::Public => Doc::nil(),
    };
    // Check if first constructor has attributes - if so, always show the bar
    let first_has_attrs = constrs.first().map_or(false, |c| !c.pcd_attributes.is_empty());
    let docs: Vec<Doc> = constrs
        .iter()
        .enumerate()
        .map(|(i, constr)| {
            let bar = if i == 0 {
                if first_has_attrs {
                    // Always show bar when first constructor has attributes
                    Doc::text("| ")
                } else {
                    Doc::if_breaks(Doc::text("| "), Doc::nil())
                }
            } else {
                Doc::text("| ")
            };
            Doc::concat(vec![bar, print_constructor_declaration(state, constr, cmt_tbl, arena)])
        })
        .collect();
    Doc::group(Doc::concat(vec![
        Doc::indent(Doc::concat(vec![Doc::line(), private_doc, Doc::join(Doc::line(), docs)])),
    ]))
}

/// Print a constructor declaration.
fn print_constructor_declaration(
    state: &PrinterState,
    constr: &ConstructorDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &constr.pcd_attributes, cmt_tbl, arena);
    let name_doc = Doc::text(&constr.pcd_name.txt);

    let args_doc = match &constr.pcd_args {
        ConstructorArguments::Pcstr_tuple(types) if types.is_empty() => Doc::nil(),
        ConstructorArguments::Pcstr_tuple(types) => {
            let type_docs: Vec<Doc> = types
                .iter()
                .map(|t| print_typ_expr(state, t, cmt_tbl, arena))
                .collect();
            Doc::concat(vec![
                Doc::text("("),
                Doc::join(Doc::concat(vec![Doc::text(","), Doc::space()]), type_docs),
                Doc::text(")"),
            ])
        }
        ConstructorArguments::Pcstr_record(fields) => {
            Doc::concat(vec![
                Doc::text("("),
                print_record_declaration(state, fields, None, cmt_tbl, arena),
                Doc::text(")"),
            ])
        }
    };

    let res_doc = match &constr.pcd_res {
        Some(typ) => Doc::concat(vec![Doc::text(": "), print_typ_expr(state, typ, cmt_tbl, arena)]),
        None => Doc::nil(),
    };

    Doc::concat(vec![attrs_doc, name_doc, args_doc, res_doc])
}

/// Print record declaration.
fn print_record_declaration(
    state: &PrinterState,
    fields: &[LabelDeclaration],
    record_loc: Option<LocIdx>,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Calculate force_break: if the opening brace and first field are on different lines
    let force_break = match (record_loc, fields.first()) {
        (Some(loc), Some(first_field)) => {
            // For spread fields (...), use the type location instead
            let field_line = if first_field.pld_name.txt == "..." {
                arena.loc_start(first_field.pld_type.ptyp_loc).line
            } else {
                arena.loc_start(first_field.pld_loc).line
            };
            arena.loc_start(loc).line < field_line
        }
        _ => false,
    };

    let field_docs: Vec<Doc> = fields
        .iter()
        .map(|field| print_label_declaration(state, field, cmt_tbl, arena))
        .collect();
    Doc::breakable_group(
        Doc::concat(vec![
            Doc::lbrace(),
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), field_docs),
            ])),
            Doc::trailing_comma(),
            Doc::soft_line(),
            Doc::rbrace(),
        ]),
        force_break,
    )
}

/// Print a label declaration (record field).
fn print_label_declaration(
    state: &PrinterState,
    field: &LabelDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Handle spread fields: ...typename
    if field.pld_name.txt == "..." {
        let typ_doc = print_typ_expr(state, &field.pld_type, cmt_tbl, arena);
        return Doc::concat(vec![Doc::dotdotdot(), typ_doc]);
    }

    let attrs_doc = print_attributes(state, &field.pld_attributes, cmt_tbl, arena);
    let mutable_doc = if field.pld_mutable == MutableFlag::Mutable {
        Doc::text("mutable ")
    } else {
        Doc::nil()
    };
    let name_doc = print_ident_like(&field.pld_name.txt, false, false);
    let optional_doc = if field.pld_optional {
        Doc::text("?")
    } else {
        Doc::nil()
    };
    let typ_doc = print_typ_expr(state, &field.pld_type, cmt_tbl, arena);

    Doc::group(Doc::concat(vec![
        attrs_doc,
        mutable_doc,
        name_doc,
        optional_doc,
        Doc::text(": "),
        typ_doc,
    ]))
}

/// Print value description (external or let declaration in signatures).
fn print_value_description(
    state: &PrinterState,
    val_desc: &ValueDescription,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let is_external = !val_desc.pval_prim.is_empty();
    let attrs_doc = print_attributes(state, &val_desc.pval_attributes, cmt_tbl, arena);
    let name_doc = print_ident_like(&val_desc.pval_name.txt, false, false);
    let typ_doc = print_typ_expr(state, &val_desc.pval_type, cmt_tbl, arena);
    let header = if is_external { "external " } else { "let " };

    let prim_doc = if is_external {
        let prims: Vec<Doc> = val_desc
            .pval_prim
            .iter()
            .map(|s| Doc::concat(vec![Doc::text("\""), Doc::text(s.clone()), Doc::text("\"")]))
            .collect();
        Doc::group(Doc::concat(vec![
            Doc::text(" ="),
            Doc::indent(Doc::concat(vec![
                Doc::line(),
                Doc::join(Doc::line(), prims),
            ])),
        ]))
    } else {
        Doc::nil()
    };

    Doc::group(Doc::concat(vec![
        attrs_doc,
        Doc::text(header),
        name_doc,
        Doc::text(": "),
        typ_doc,
        prim_doc,
    ]))
}

/// Print type extension.
fn print_type_extension(
    state: &PrinterState,
    type_ext: &TypeExtension,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &type_ext.ptyext_attributes, cmt_tbl, arena);
    let path_doc = print_lident(arena, arena.get_longident(type_ext.ptyext_path.txt));

    let params_doc = if type_ext.ptyext_params.is_empty() {
        Doc::nil()
    } else {
        let params: Vec<Doc> = type_ext
            .ptyext_params
            .iter()
            .map(|(typ, variance)| print_type_param(state, typ, variance, cmt_tbl, arena))
            .collect();
        Doc::concat(vec![
            Doc::less_than(),
            Doc::join(Doc::concat(vec![Doc::text(","), Doc::space()]), params),
            Doc::greater_than(),
        ])
    };

    let private_doc = match type_ext.ptyext_private {
        PrivateFlag::Private => Doc::text("private "),
        PrivateFlag::Public => Doc::nil(),
    };

    let constructors_doc: Vec<Doc> = type_ext
        .ptyext_constructors
        .iter()
        .map(|constr| print_extension_constructor(state, constr, cmt_tbl, arena))
        .collect();

    Doc::concat(vec![
        attrs_doc,
        Doc::text("type "),
        path_doc,
        params_doc,
        Doc::text(" += "),
        private_doc,
        Doc::join(Doc::concat(vec![Doc::line(), Doc::text("| ")]), constructors_doc),
    ])
}

/// Print extension constructor.
fn print_extension_constructor(
    state: &PrinterState,
    ext_constr: &ExtensionConstructor,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &ext_constr.pext_attributes, cmt_tbl, arena);
    let name_doc = Doc::text(&ext_constr.pext_name.txt);

    let kind_doc = match &ext_constr.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            let args_doc = match args {
                ConstructorArguments::Pcstr_tuple(types) if types.is_empty() => Doc::nil(),
                ConstructorArguments::Pcstr_tuple(types) => {
                    let type_docs: Vec<Doc> = types
                        .iter()
                        .map(|t| print_typ_expr(state, t, cmt_tbl, arena))
                        .collect();
                    Doc::concat(vec![
                        Doc::text("("),
                        Doc::join(Doc::concat(vec![Doc::text(","), Doc::space()]), type_docs),
                        Doc::text(")"),
                    ])
                }
                ConstructorArguments::Pcstr_record(fields) => {
                    Doc::concat(vec![
                        Doc::text("("),
                        print_record_declaration(state, fields, None, cmt_tbl, arena),
                        Doc::text(")"),
                    ])
                }
            };
            let res_doc = match res {
                Some(typ) => Doc::concat(vec![Doc::text(": "), print_typ_expr(state, typ, cmt_tbl, arena)]),
                None => Doc::nil(),
            };
            Doc::concat(vec![args_doc, res_doc])
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            Doc::concat(vec![Doc::text(" = "), print_longident(arena, arena.get_longident(lid.txt))])
        }
    };

    Doc::concat(vec![attrs_doc, name_doc, kind_doc])
}

/// Print exception definition (for exception declarations in signatures and structures).
fn print_exception_def(
    state: &PrinterState,
    ext_constr: &ExtensionConstructor,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &ext_constr.pext_attributes, cmt_tbl, arena);
    let name_doc = Doc::text(&ext_constr.pext_name.txt);

    let kind_doc = match &ext_constr.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            let args_doc = match args {
                ConstructorArguments::Pcstr_tuple(types) if types.is_empty() => Doc::nil(),
                ConstructorArguments::Pcstr_tuple(types) => {
                    let type_docs: Vec<Doc> = types
                        .iter()
                        .map(|t| print_typ_expr(state, t, cmt_tbl, arena))
                        .collect();
                    Doc::concat(vec![
                        Doc::text("("),
                        Doc::join(Doc::concat(vec![Doc::text(","), Doc::space()]), type_docs),
                        Doc::text(")"),
                    ])
                }
                ConstructorArguments::Pcstr_record(fields) => {
                    Doc::concat(vec![
                        Doc::text("("),
                        print_record_declaration(state, fields, None, cmt_tbl, arena),
                        Doc::text(")"),
                    ])
                }
            };
            let res_doc = match res {
                Some(typ) => Doc::concat(vec![Doc::text(": "), print_typ_expr(state, typ, cmt_tbl, arena)]),
                None => Doc::nil(),
            };
            Doc::concat(vec![args_doc, res_doc])
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            Doc::indent(Doc::concat(vec![
                Doc::text(" ="),
                Doc::line(),
                print_longident(arena, arena.get_longident(lid.txt)),
            ]))
        }
    };

    Doc::group(Doc::concat(vec![
        attrs_doc,
        Doc::text("exception "),
        name_doc,
        kind_doc,
    ]))
}

/// Print module binding.
fn print_module_binding(
    state: &PrinterState,
    binding: &ModuleBinding,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &binding.pmb_attributes, cmt_tbl, arena);
    let name_doc = Doc::text(&binding.pmb_name.txt);
    let expr_doc = print_mod_expr(state, &binding.pmb_expr, cmt_tbl, arena);

    Doc::concat(vec![
        attrs_doc,
        Doc::text("module "),
        name_doc,
        Doc::text(" = "),
        expr_doc,
    ])
}

/// Print recursive module bindings.
fn print_rec_module_bindings(
    state: &PrinterState,
    bindings: &[ModuleBinding],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let docs: Vec<Doc> = bindings
        .iter()
        .enumerate()
        .map(|(i, binding)| {
            let prefix = if i == 0 {
                Doc::text("module rec ")
            } else {
                Doc::text("and ")
            };
            let name_doc = Doc::text(&binding.pmb_name.txt);
            let expr_doc = print_mod_expr(state, &binding.pmb_expr, cmt_tbl, arena);
            Doc::concat(vec![prefix, name_doc, Doc::text(" = "), expr_doc])
        })
        .collect();
    Doc::join(Doc::hard_line(), docs)
}

/// Print module type declaration.
fn print_module_type_declaration(
    state: &PrinterState,
    decl: &ModuleTypeDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &decl.pmtd_attributes, cmt_tbl, arena);
    let name_doc = Doc::text(&decl.pmtd_name.txt);
    let typ_doc = match &decl.pmtd_type {
        Some(mty) => Doc::concat(vec![Doc::text(" = "), print_module_type(state, mty, cmt_tbl, arena)]),
        None => Doc::nil(),
    };

    Doc::concat(vec![
        attrs_doc,
        Doc::text("module type "),
        name_doc,
        typ_doc,
    ])
}

/// Print open description.
fn print_open_description(
    _state: &PrinterState,
    open_desc: &OpenDescription,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(_state, &open_desc.popen_attributes, cmt_tbl, arena);
    let lid_doc = print_longident(arena, arena.get_longident(open_desc.popen_lid.txt));
    let lid_doc_with_comments = print_comments(lid_doc, cmt_tbl, open_desc.popen_lid.loc, arena);
    let override_doc = match open_desc.popen_override {
        OverrideFlag::Fresh => Doc::space(),
        OverrideFlag::Override => Doc::text("! "),
    };

    Doc::concat(vec![attrs_doc, Doc::text("open"), override_doc, lid_doc_with_comments])
}

/// Print include declaration.
fn print_include_declaration(
    state: &PrinterState,
    include_decl: &IncludeDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &include_decl.pincl_attributes, cmt_tbl, arena);
    let mod_doc = print_mod_expr(state, &include_decl.pincl_mod, cmt_tbl, arena);

    Doc::concat(vec![attrs_doc, Doc::text("include "), mod_doc])
}

// ============================================================================
// Main Entry Points
// ============================================================================

/// Default print width.
pub const DEFAULT_PRINT_WIDTH: i32 = 100;

/// Print a structure with comments to a string.
pub fn print_implementation(
    structure: &[StructureItem],
    comments: Vec<Comment>,
    width: i32,
    arena: &mut ParseArena,
) -> String {
    let mut cmt_tbl = CommentTable::new();
    walk_structure(structure, &mut cmt_tbl, comments, arena);
    let state = PrinterState::init();
    let doc = print_structure(&state, structure, &mut cmt_tbl, arena);
    let output = doc.to_string(width);
    if output.is_empty() {
        output
    } else {
        format!("{}\n", output)
    }
}

/// Print a structure with comments using default width.
pub fn print_structure_with_comments(structure: &[StructureItem], comments: Vec<Comment>, arena: &mut ParseArena) -> String {
    print_implementation(structure, comments, DEFAULT_PRINT_WIDTH, arena)
}

/// Print a signature/interface to a string.
pub fn print_interface(
    signature: &[SignatureItem],
    _comments: Vec<Comment>,
    width: i32,
    arena: &mut ParseArena,
) -> String {
    // TODO: Add walk_signature for comment attachment
    let mut cmt_tbl = CommentTable::new();
    let state = PrinterState::init();
    let doc = print_signature(&state, signature, &mut cmt_tbl, arena);
    let output = doc.to_string(width);
    if output.is_empty() {
        output
    } else {
        format!("{}\n", output)
    }
}

/// Print a signature with comments using default width.
pub fn print_signature_with_comments(signature: &[SignatureItem], comments: Vec<Comment>, arena: &mut ParseArena) -> String {
    print_interface(signature, comments, DEFAULT_PRINT_WIDTH, arena)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_classify_ident_content() {
        // Normal identifiers
        assert_eq!(
            classify_ident_content("foo", false, false),
            IdentifierStyle::NormalIdent
        );
        assert_eq!(
            classify_ident_content("foo_bar", false, false),
            IdentifierStyle::NormalIdent
        );
        assert_eq!(
            classify_ident_content("foo123", false, false),
            IdentifierStyle::NormalIdent
        );
        assert_eq!(
            classify_ident_content("_foo", false, false),
            IdentifierStyle::NormalIdent
        );

        // Keywords are exotic
        assert_eq!(
            classify_ident_content("let", false, false),
            IdentifierStyle::ExoticIdent
        );
        assert_eq!(
            classify_ident_content("type", false, false),
            IdentifierStyle::ExoticIdent
        );

        // Uppercase without allow_uident
        assert_eq!(
            classify_ident_content("Foo", false, false),
            IdentifierStyle::ExoticIdent
        );

        // Uppercase with allow_uident
        assert_eq!(
            classify_ident_content("Foo", true, false),
            IdentifierStyle::NormalIdent
        );

        // Hyphen without allow_hyphen
        assert_eq!(
            classify_ident_content("foo-bar", false, false),
            IdentifierStyle::ExoticIdent
        );

        // Hyphen with allow_hyphen
        assert_eq!(
            classify_ident_content("foo-bar", false, true),
            IdentifierStyle::NormalIdent
        );

        // Backslash prefix
        assert_eq!(
            classify_ident_content("\\foo", false, false),
            IdentifierStyle::UppercaseExoticIdent
        );
    }

    #[test]
    fn test_is_valid_numeric_polyvar_number() {
        assert!(is_valid_numeric_polyvar_number("0"));
        assert!(is_valid_numeric_polyvar_number("1"));
        assert!(is_valid_numeric_polyvar_number("123"));
        assert!(!is_valid_numeric_polyvar_number("01")); // Leading zero
        assert!(!is_valid_numeric_polyvar_number("abc"));
        assert!(!is_valid_numeric_polyvar_number(""));
    }
}
