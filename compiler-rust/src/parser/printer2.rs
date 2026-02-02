//! ReScript AST printer - Doc-based implementation.
//!
//! This is a 1:1 port of OCaml's `res_printer.ml`, using Doc structures
//! for pretty-printing instead of direct string output.

use crate::location::Location as FullLocation;
use crate::parse_arena::{LocIdx, ParseArena};
use crate::parser::ast::*;
use crate::parser::comment::Comment;
use crate::parser::comment_table::{walk_signature, walk_structure, CommentTable, PosRange};
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

/// Trim leading/trailing spaces (not newlines) from comment text.
/// This matches OCaml's Comment.trim_spaces which only trims ' ' characters.
fn trim_spaces(s: &str) -> &str {
    let bytes = s.as_bytes();
    let len = bytes.len();
    if len == 0 {
        return s;
    }

    // Find start (skip leading spaces only)
    let mut start = 0;
    while start < len && bytes[start] == b' ' {
        start += 1;
    }

    // Find end (skip trailing spaces only)
    let mut end = len;
    while end > start && bytes[end - 1] == b' ' {
        end -= 1;
    }

    if start < end {
        &s[start..end]
    } else {
        ""
    }
}

/// Print multiline comment content with proper formatting.
/// Matches OCaml's print_multiline_comment_content behavior:
/// - For single line, wraps with /* and */
/// - For multiline, keeps first line on same line as /*, aligns * prefixed lines
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
            let rest = &lines[1..];

            // Helper function similar to OCaml's indent_stars
            // Processes lines and returns (doc, early_exit)
            // early_exit is true if we hit a line that doesn't start with *
            fn indent_stars(lines: &[&str], txt: &str) -> (Doc, bool) {
                let mut docs = Vec::new();

                for (i, line) in lines.iter().enumerate() {
                    let trimmed = line.trim();
                    let is_last = i == lines.len() - 1;

                    if is_last {
                        // Last line handling
                        let trailing_space = if trimmed.is_empty() { "" } else { " " };
                        docs.push(Doc::hard_line());
                        docs.push(Doc::text(format!(" {}{}", trimmed, trailing_space)));
                    } else if !trimmed.is_empty() && trimmed.starts_with('*') {
                        // Star-prefixed line
                        docs.push(Doc::hard_line());
                        docs.push(Doc::text(format!(" {}", trimmed)));
                    } else {
                        // Non-star line: bail out and return original content
                        let trailing_space = if txt.ends_with(' ') { Doc::space() } else { Doc::nil() };
                        let content = trim_spaces(txt);
                        return (Doc::concat(vec![Doc::text(content), trailing_space]), true);
                    }
                }

                (Doc::concat(docs), false)
            }

            let (indent_doc, early_exit) = indent_stars(rest, txt);

            let mut docs = vec![Doc::text("/*")];

            // Add space before first line if not empty and not just "*"
            if !first_line.is_empty() && first_line != "*" {
                docs.push(Doc::space());
            }

            if early_exit {
                // Early exit case: just use the returned doc (original content)
                docs.push(indent_doc);
            } else {
                // Normal case: first line + processed rest
                docs.push(Doc::text(first_line));
                docs.push(indent_doc);
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

/// Print leading comments for a node (takes comments directly).
fn print_leading_comments_with(
    node: Doc,
    comments: Vec<Comment>,
    full_loc: &FullLocation,
) -> Doc {
    if comments.is_empty() {
        return node;
    }

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

/// Print trailing comments for a node (takes comments directly).
fn print_trailing_comments_with(
    node: Doc,
    comments: Vec<Comment>,
    full_loc: &FullLocation,
) -> Doc {
    if comments.is_empty() {
        return node;
    }

    let mut acc = Vec::new();
    let mut prev_loc = full_loc.clone();

    for comment in &comments {
        acc.push(print_trailing_comment(&prev_loc, full_loc, comment));
        prev_loc = comment.loc().clone();
    }

    Doc::concat(vec![node, Doc::concat(acc)])
}

/// Print a node with its leading and trailing comments.
/// Uses position-based fallback when LocIdx lookup fails.
pub fn print_comments(doc: Doc, cmt_tbl: &mut CommentTable, loc: LocIdx, arena: &ParseArena) -> Doc {
    let full_loc = arena.to_location(loc);

    // Get leading comments with position-based fallback
    let leading_comments = cmt_tbl.remove_leading_comments_by_loc(loc, arena);
    let doc_with_leading = print_leading_comments_with(doc, leading_comments, &full_loc);

    // Get trailing comments with position-based fallback
    let trailing_comments = cmt_tbl.remove_trailing_comments_by_loc(loc, arena);
    print_trailing_comments_with(doc_with_leading, trailing_comments, &full_loc)
}

/// Print a node with its leading and trailing comments using position range directly.
/// Useful when we need to retrieve comments from a combined location without creating a new LocIdx.
fn print_comments_by_pos(doc: Doc, cmt_tbl: &mut CommentTable, pos_range: PosRange, full_loc: &FullLocation) -> Doc {
    // Get leading comments by position
    let leading_comments = cmt_tbl.remove_leading_comments_by_pos(pos_range);
    let doc_with_leading = print_leading_comments_with(doc, leading_comments, full_loc);

    // Get trailing comments by position
    let trailing_comments = cmt_tbl.remove_trailing_comments_by_pos(pos_range);
    print_trailing_comments_with(doc_with_leading, trailing_comments, full_loc)
}

/// Print a node with only its leading comments.
/// Used for if-chains where we want to attach "else if" leading comments.
fn print_leading_comments(doc: Doc, cmt_tbl: &mut CommentTable, loc: LocIdx, arena: &ParseArena) -> Doc {
    let full_loc = arena.to_location(loc);
    let leading_comments = cmt_tbl.remove_leading_comments_by_loc(loc, arena);
    print_leading_comments_with(doc, leading_comments, &full_loc)
}

/// Get the first leading comment for a location, if any.
/// Uses position-based keys like OCaml's Location.t.
pub fn get_first_leading_comment<'a>(cmt_tbl: &'a CommentTable, loc: LocIdx, arena: &ParseArena) -> Option<&'a Comment> {
    cmt_tbl.get_first_leading_comment(loc, arena)
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
        let start_pos = match get_first_leading_comment(cmt_tbl, loc, arena) {
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
        let start_pos = match get_first_leading_comment(cmt_tbl, loc, arena) {
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
/// Uses position-based fallback when LocIdx lookup fails.
pub fn print_comments_inside(cmt_tbl: &mut CommentTable, loc: LocIdx, arena: &ParseArena) -> Doc {
    let force_break = arena.loc_start(loc).line != arena.loc_end(loc).line;

    // Get inside comments with position-based fallback
    let comments = cmt_tbl.remove_inside_comments_by_loc(loc, arena);
    if comments.is_empty() {
        return Doc::nil();
    }

    // Build the comment docs following OCaml's pattern:
    // For n comments, OCaml produces: soft_line, c1, line, c2, line, ..., cn
    // Where blank lines between comments produce double hard_line instead of line
    let mut acc = Vec::new();
    acc.push(Doc::soft_line());

    for (i, comment) in comments.iter().enumerate() {
        let single_line = comment.is_single_line();
        let txt = comment.txt();
        let cmt_doc = if single_line {
            Doc::text(format!("//{}", txt))
        } else {
            print_multiline_comment_content(txt)
        };

        acc.push(cmt_doc);

        if i < comments.len() - 1 {
            // Check if there's a blank line between this comment and the next
            let this_end = comment.loc().loc_end.line as u32;
            let next_start = comments[i + 1].loc().loc_start.line as u32;
            let sep = if next_start.saturating_sub(this_end) > 1 {
                // Blank line between comments - preserve it
                Doc::concat(vec![Doc::hard_line(), Doc::hard_line()])
            } else {
                Doc::line()
            };
            acc.push(sep);
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

/// Print comments inside an empty file.
pub fn print_comments_inside_file(cmt_tbl: &mut CommentTable, _arena: &ParseArena) -> Doc {
    // Use PosRange::none() which corresponds to Location::none()
    match cmt_tbl.inside.remove(&PosRange::none()) {
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
            // Print the inner apply with __x replaced by _ at print time
            print_underscore_apply(state, rhs, cmt_tbl, arena)
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
            let field = print_comments(field, cmt_tbl, longident_loc.loc, arena);
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
                let field = print_comments(field, cmt_tbl, longident_loc.loc, arena);
                Doc::concat(vec![expr_doc, Doc::dot(), field])
            };
            let rhs_doc = {
                let doc = print_expression_with_comments(state, value, cmt_tbl, arena);
                match parens::set_field_expr_rhs(value) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, value, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };
            // Only indent RHS if it's a binary expression (matching OCaml)
            let should_indent = parsetree_viewer::is_binary_expression(arena, value);
            let doc = Doc::group(Doc::concat(vec![
                lhs,
                Doc::text(" ="),
                if should_indent {
                    Doc::group(Doc::indent(Doc::concat(vec![Doc::line(), rhs_doc])))
                } else {
                    Doc::concat(vec![Doc::space(), rhs_doc])
                },
            ]));
            // Print attributes - Pexp_setfield handles its own attributes
            let attrs = &e.pexp_attributes;
            if attrs.is_empty() {
                doc
            } else {
                let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
                Doc::group(Doc::concat(vec![attrs_doc, doc]))
            }
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
            let parent_doc = {
                let doc = print_expression_with_comments(state, parent_expr, cmt_tbl, arena);
                match parens::unary_expr_operand(arena, parent_expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(braces) => print_braces(doc, parent_expr, braces, arena),
                    ParenKind::Nothing => doc,
                }
            };
            let member_doc = {
                let doc = print_expression_with_comments(state, member_expr, cmt_tbl, arena);
                let doc = match parens::expr(arena, member_expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(braces) => print_braces(doc, member_expr, braces, arena),
                    ParenKind::Nothing => doc,
                };
                // For simple expressions (constants, identifiers), inline them directly
                // For complex expressions, wrap with soft_line to allow breaking
                let should_inline = matches!(
                    &member_expr.pexp_desc,
                    ExpressionDesc::Pexp_constant(_) | ExpressionDesc::Pexp_ident(_)
                );
                if should_inline {
                    doc
                } else {
                    Doc::concat(vec![
                        Doc::indent(Doc::concat(vec![Doc::soft_line(), doc])),
                        Doc::soft_line(),
                    ])
                }
            };
            // Print attributes - Pexp_apply handles its own attributes
            let attrs_doc = print_attributes(state, &e.pexp_attributes, cmt_tbl, arena);
            Doc::group(Doc::concat(vec![
                attrs_doc,
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
            let parent_doc = {
                let doc = print_expression_with_comments(state, parent_expr, cmt_tbl, arena);
                match parens::unary_expr_operand(arena, parent_expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(braces) => print_braces(doc, parent_expr, braces, arena),
                    ParenKind::Nothing => doc,
                }
            };
            let member_doc = {
                let doc = print_expression_with_comments(state, member_expr, cmt_tbl, arena);
                let doc = match parens::expr(arena, member_expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(braces) => print_braces(doc, member_expr, braces, arena),
                    ParenKind::Nothing => doc,
                };
                // For simple expressions (constants, identifiers), inline them directly
                // For complex expressions, wrap with soft_line to allow breaking
                let should_inline = matches!(
                    &member_expr.pexp_desc,
                    ExpressionDesc::Pexp_constant(_) | ExpressionDesc::Pexp_ident(_)
                );
                if should_inline {
                    doc
                } else {
                    Doc::concat(vec![
                        Doc::indent(Doc::concat(vec![Doc::soft_line(), doc])),
                        Doc::soft_line(),
                    ])
                }
            };
            let target_doc = {
                let doc = print_expression_with_comments(state, target_expr, cmt_tbl, arena);
                match parens::expr(arena, target_expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(braces) => print_braces(doc, target_expr, braces, arena),
                    ParenKind::Nothing => doc,
                }
            };
            let should_indent_target = if parsetree_viewer::is_braced_expr(target_expr) {
                false
            } else if parsetree_viewer::is_binary_expression(arena, target_expr) {
                true
            } else if let ExpressionDesc::Pexp_ifthenelse(_, _, _) = &target_expr.pexp_desc {
                // Check for ternary
                if parsetree_viewer::has_ternary_attribute(&target_expr.pexp_attributes) {
                    if let ExpressionDesc::Pexp_ifthenelse(if_expr, _, _) = &target_expr.pexp_desc {
                        parsetree_viewer::is_binary_expression(arena, if_expr)
                            || parsetree_viewer::has_attributes(&if_expr.pexp_attributes)
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else if matches!(&target_expr.pexp_desc, ExpressionDesc::Pexp_newtype(_, _)) {
                false
            } else {
                parsetree_viewer::has_attributes(&target_expr.pexp_attributes)
                    || parsetree_viewer::is_array_access(arena, target_expr)
            };
            let rhs_doc = if should_indent_target {
                Doc::indent(Doc::concat(vec![Doc::line(), target_doc]))
            } else {
                Doc::concat(vec![Doc::space(), target_doc])
            };
            // Print attributes - Pexp_apply handles its own attributes
            let attrs_doc = print_attributes(state, &e.pexp_attributes, cmt_tbl, arena);
            Doc::group(Doc::concat(vec![
                attrs_doc,
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
            // Print attributes - Pexp_apply handles its own attributes
            let attrs_doc = print_attributes(state, &e.pexp_attributes, cmt_tbl, arena);
            Doc::group(Doc::concat(vec![
                attrs_doc,
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
            match (&inner.pexp_desc, &typ.ptyp_desc) {
                (ExpressionDesc::Pexp_pack(mod_expr), CoreTypeDesc::Ptyp_package(package_type)) => {
                    let mod_doc = print_mod_expr(state, mod_expr, cmt_tbl, arena);
                    // Don't print "module(...)" wrapper - we're already inside module()
                    let type_doc = print_comments(
                        print_package_type(state, package_type, false, cmt_tbl, arena),
                        cmt_tbl,
                        typ.ptyp_loc,
                        arena,
                    );
                    Doc::group(Doc::concat(vec![
                        Doc::text("module("),
                        Doc::indent(Doc::concat(vec![Doc::soft_line(), mod_doc])),
                        Doc::text(": "),
                        type_doc,
                        Doc::soft_line(),
                        Doc::rparen(),
                    ]))
                }
                _ => Doc::nil(),
            }
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
        // Note: OCaml filters out @res.braces before checking if parens are needed.
        // This means `await {x}` prints as `await x` because the braces attr is ignored.
        ExpressionDesc::Pexp_await(expr) => {
            let rhs = print_expression_with_comments(state, expr, cmt_tbl, arena);
            let rhs = match parens::assert_or_await_expr_rhs_ignore_braces(arena, true, expr) {
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
    // Some expression types print their own attributes (fun, newtype, apply, etc.)
    // so we skip attribute printing here for those cases to avoid duplication.
    // This matches OCaml's should_print_its_own_attributes check.
    let should_print_its_own_attributes = match &e.pexp_desc {
        ExpressionDesc::Pexp_apply { .. }
        | ExpressionDesc::Pexp_fun { .. }
        | ExpressionDesc::Pexp_newtype(_, _)
        | ExpressionDesc::Pexp_setfield(_, _, _)
        | ExpressionDesc::Pexp_ifthenelse(_, _, _) => true,
        ExpressionDesc::Pexp_match(_, _) if parsetree_viewer::is_if_let_expr(e) => true,
        ExpressionDesc::Pexp_jsx_element(_) => true,
        _ => false,
    };

    let attrs = &e.pexp_attributes;
    if attrs.is_empty() || should_print_its_own_attributes {
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
    print_pexp_fun(state, InCallback::NoCallback, e, cmt_tbl, arena)
}

/// Print a function expression (Pexp_fun/Pexp_newtype) with callback mode.
/// This is used by callback printing to try different layouts.
fn print_pexp_fun(
    state: &PrinterState,
    in_callback: InCallback,
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
        in_callback,
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
                Doc::concat(vec![
                    Doc::indent(Doc::concat(vec![Doc::line(), return_doc])),
                    // In callback mode, add soft_line after return expr
                    // This pushes trailing comments to after the function call
                    match in_callback {
                        InCallback::FitsOnOneLine | InCallback::ArgumentsFitOnOneLine => Doc::soft_line(),
                        InCallback::NoCallback => Doc::nil(),
                    },
                ])
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

    Doc::concat(vec![
        attrs,
        parameters_doc,
        typ_constraint_doc,
        Doc::text(" =>"),
        return_expr_doc,
    ])
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
    // Note: we filter out parsing attrs (like res.braces) when checking for empty attrs
    match parameters {
        // let f = _ => ()
        [FunParam::Parameter {
            attrs,
            label: ArgLabel::Nolabel,
            default_expr: None,
            pat,
        }] if parsetree_viewer::filter_parsing_attrs(attrs).is_empty()
            && matches!(&pat.ppat_desc, PatternDesc::Ppat_any) =>
        {
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
        }] if parsetree_viewer::filter_parsing_attrs(attrs).is_empty()
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
        }] if parsetree_viewer::filter_parsing_attrs(attrs).is_empty()
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
        FunParam::NewTypes { attrs, names } => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            // Print all type names space-separated, e.g., "type t u v"
            let names_doc = Doc::join(
                Doc::space(),
                names
                    .iter()
                    .map(|name| {
                        print_comments(
                            print_ident_like(&name.txt, false, false),
                            cmt_tbl,
                            name.loc,
                            arena,
                        )
                    })
                    .collect(),
            );
            Doc::group(Doc::concat(vec![attrs_doc, Doc::text("type "), names_doc]))
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

            // Only add =? for optional arguments without a default value
            // If there's a default value (=expr), we don't need =?
            let opt_marker = match (label, default_expr) {
                (ArgLabel::Optional(_), None) => Doc::text("=?"),
                _ => Doc::nil(),
            };

            let doc = Doc::group(Doc::concat(vec![
                attrs_doc,
                label_with_pattern,
                default_doc,
                opt_marker,
            ]));

            // Wrap with comments using combined location (like OCaml's print_exp_fun_parameter)
            // The location spans from the label (if present) to the end of pattern or default_expr
            let lbl_loc = match label {
                ArgLabel::Labelled(lbl) | ArgLabel::Optional(lbl)
                    if arena.to_location(lbl.loc) != FullLocation::none() =>
                {
                    lbl.loc
                }
                _ => pat.ppat_loc,
            };
            let end_loc = match default_expr {
                Some(expr) => expr.pexp_loc,
                None => pat.ppat_loc,
            };
            let (pos_range, full_loc) = make_combined_pos_range(lbl_loc, end_loc, arena);
            print_comments_by_pos(doc, cmt_tbl, pos_range, &full_loc)
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

    let doc = Doc::concat(vec![lbl_doc, Doc::text(": "), expr_doc]);
    // cmt_loc spans from label start to expression end, matching OCaml
    let (pos_range, full_loc) = make_combined_pos_range(field.lid.loc, field.expr.pexp_loc, arena);
    print_comments_by_pos(doc, cmt_tbl, pos_range, &full_loc)
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
    // Special case: empty record (no spread, no fields) - just print inside comments
    if fields.is_empty() && spread.is_none() {
        return Doc::concat(vec![
            Doc::lbrace(),
            print_comments_inside(cmt_tbl, loc, arena),
            Doc::rbrace(),
        ]);
    }

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
        Some(expr) => {
            // Print the expression, checking if it's just an identifier
            let expr_doc = match &expr.pexp_desc {
                ExpressionDesc::Pexp_ident(path) => {
                    print_lident(arena, arena.get_longident(path.txt))
                }
                _ => print_expression(state, expr, cmt_tbl, arena),
            };
            // Apply parens if needed
            let expr_doc = match parens::expr(arena, expr) {
                ParenKind::Parenthesized => add_parens(expr_doc),
                ParenKind::Braced(loc) => print_braces(expr_doc, expr, loc, arena),
                ParenKind::Nothing => expr_doc,
            };
            // Combine ... with the expression
            let doc_with_spread = Doc::concat(vec![Doc::dotdotdot(), expr_doc]);
            // Wrap with comments using expr.pexp_loc - this puts leading comments BEFORE ...
            let doc_with_comments = print_comments(doc_with_spread, cmt_tbl, expr.pexp_loc, arena);
            Doc::concat(vec![doc_with_comments, Doc::text(","), Doc::line()])
        }
        None => Doc::nil(),
    };
    let fields_doc = Doc::join(
        Doc::concat(vec![Doc::text(","), Doc::line()]),
        fields
            .iter()
            .map(|field| {
                // Like OCaml's print_expression_record_row, print the label with its comments
                let field_name_with_comments = {
                    let doc = print_lident(arena, arena.get_longident(field.lid.txt));
                    print_comments(doc, cmt_tbl, field.lid.loc, arena)
                };
                let expr_doc = print_expression_with_comments(state, &field.expr, cmt_tbl, arena);
                // Check for punning
                let row_doc = if punning_allowed && is_punned_record_field(arena, field) {
                    // Punned: `?name` or `name`
                    if field.opt {
                        Doc::concat(vec![Doc::text("?"), field_name_with_comments])
                    } else {
                        field_name_with_comments
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
                        field_name_with_comments,
                        Doc::text(": "),
                        opt_marker,
                        expr_doc,
                    ]))
                };
                // Like OCaml, wrap with comments using combined location (from label to expression)
                let (pos_range, full_loc) = make_combined_pos_range(field.lid.loc, field.expr.pexp_loc, arena);
                print_comments_by_pos(row_doc, cmt_tbl, pos_range, &full_loc)
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

    // Print attributes - ternary handles its own attributes
    // First filter out res.ternary attribute
    let attrs: Vec<&Attribute> = parsetree_viewer::filter_ternary_attributes(&e.pexp_attributes);

    // Check if any of the printable attributes need parens wrapping
    let needs_parens = parsetree_viewer::filter_parsing_attrs(
        &attrs.iter().map(|a| (*a).clone()).collect::<Vec<_>>()
    ).iter().any(|attr| parsetree_viewer::is_printable_attribute(attr));

    let attrs_doc = print_attributes(state, &attrs.into_iter().cloned().collect::<Vec<_>>(), cmt_tbl, arena);
    Doc::concat(vec![
        attrs_doc,
        if needs_parens {
            add_parens(ternary_doc)
        } else {
            ternary_doc
        },
    ])
}

/// Print a ternary operand with appropriate parenthesization.
fn print_ternary_operand(
    state: &PrinterState,
    expr: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);

    // Use parens::ternary_operand to determine parenthesization
    match parens::ternary_operand(expr) {
        ParenKind::Parenthesized => add_parens(doc),
        ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
        ParenKind::Nothing => doc,
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
        .map(|(i, (outer_loc, condition_kind, then_expr))| {
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

            // Print leading comments for outer_loc
            // This handles comments that appear between `}` and `else if`
            print_leading_comments(doc, cmt_tbl, **outer_loc, arena)
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

    // Print attributes - Pexp_ifthenelse handles its own attributes
    // Filter out fragile match attributes (res.braces, res.ternary)
    let attrs = parsetree_viewer::filter_fragile_match_attributes(&e.pexp_attributes);
    let attrs_doc = print_attributes(state, &attrs.into_iter().cloned().collect::<Vec<_>>(), cmt_tbl, arena);
    Doc::concat(vec![
        attrs_doc,
        if_docs,
        else_doc,
    ])
}

/// A row in an expression block.
/// Uses PosRange for comment lookup (matches what the walker stores) plus line positions for spacing.
struct BlockRow {
    /// Position range for comment lookup - must match what the walker created
    pos_range: PosRange,
    /// Start line of the row
    start_line: usize,
    /// End line of the row
    end_line: usize,
    doc: Doc,
}

/// Print expression block (with braces).
/// This matches OCaml's print_expression_block which uses collect_rows and print_list.
fn print_expression_block(
    state: &PrinterState,
    braces: bool,
    expr: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Collect all rows (statements) in the block
    let rows = collect_block_rows(state, expr, cmt_tbl, arena);

    // Print rows with position-based comment lookup
    let block = print_block_rows(&rows, cmt_tbl, arena);

    Doc::breakable_group(
        if braces {
            Doc::concat(vec![
                Doc::lbrace(),
                Doc::indent(Doc::concat(vec![Doc::line(), block])),
                Doc::line(),
                Doc::rbrace(),
            ])
        } else {
            block
        },
        true, // force_break
    )
}

/// Print block rows with proper comment handling and line breaks.
/// Uses PosRange directly for comment lookup to match what the walker stored.
fn print_block_rows(rows: &[BlockRow], cmt_tbl: &mut CommentTable, arena: &ParseArena) -> Doc {
    if rows.is_empty() {
        return Doc::nil();
    }

    let mut docs = Vec::with_capacity(rows.len() * 2);

    // First row
    let first = &rows[0];
    let first_doc = print_block_row_with_comments(&first.doc, first.pos_range, first.start_line, first.end_line, cmt_tbl, arena);
    docs.push(first_doc);

    let mut prev_end_line = first.end_line;

    // Remaining rows
    for row in &rows[1..] {
        // Determine separator based on line distance
        // Match OCaml: look up first leading comment AFTER row docs are built
        // If comment was consumed during print_expression, it won't be found,
        // and we fall back to row.start_line (matching OCaml's loc.loc_start behavior)
        let start_line = match cmt_tbl.get_first_leading_comment_by_pos(row.pos_range) {
            Some(comment) => comment.loc().loc_start.line as usize,
            None => row.start_line,
        };

        let sep = if start_line.saturating_sub(prev_end_line) > 1 {
            Doc::concat(vec![Doc::hard_line(), Doc::hard_line()])
        } else {
            Doc::hard_line()
        };

        docs.push(sep);

        let doc = print_block_row_with_comments(&row.doc, row.pos_range, row.start_line, row.end_line, cmt_tbl, arena);
        docs.push(doc);

        prev_end_line = row.end_line;
    }

    Doc::concat(docs)
}

/// Print a block row with its leading and trailing comments using position-based lookup.
fn print_block_row_with_comments(
    doc: &Doc,
    pos_range: PosRange,
    start_line: usize,
    end_line: usize,
    cmt_tbl: &mut CommentTable,
    _arena: &ParseArena,
) -> Doc {
    let leading_comments = cmt_tbl.remove_leading_comments_by_pos(pos_range);
    let trailing_comments = cmt_tbl.remove_trailing_comments_by_pos(pos_range);

    // Create a fake full location for printing helpers with actual line numbers
    let full_loc = crate::location::Location {
        loc_start: crate::location::Position {
            file_name: crate::intern::StrIdx::default(),
            line: start_line as i32,
            bol: 0,
            cnum: pos_range.start,
        },
        loc_end: crate::location::Position {
            file_name: crate::intern::StrIdx::default(),
            line: end_line as i32,
            bol: 0,
            cnum: pos_range.end,
        },
        loc_ghost: false,
    };

    // Use the existing comment printing helpers
    let doc_with_leading = print_leading_comments_with(doc.clone(), leading_comments, &full_loc);
    print_trailing_comments_with(doc_with_leading, trailing_comments, &full_loc)
}

/// Collect rows from an expression block, matching OCaml's collect_rows.
fn collect_block_rows(
    state: &PrinterState,
    expr: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Vec<BlockRow> {
    let mut rows = Vec::new();
    collect_block_rows_inner(state, expr, &mut rows, cmt_tbl, arena);
    rows
}

/// Inner recursive function for collect_block_rows.
fn collect_block_rows_inner(
    state: &PrinterState,
    expr: &Expression,
    rows: &mut Vec<BlockRow>,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_letmodule(name, mod_expr, body) => {
            // Use combined location (expr start to mod_expr end) to match walker
            let start = arena.loc_start(expr.pexp_loc);
            let end = arena.loc_end(mod_expr.pmod_loc);
            let pos_range = PosRange { start: start.cnum, end: end.cnum };

            // Print module name with comments
            let name_doc = {
                let doc = Doc::text(&name.txt);
                print_comments(doc, cmt_tbl, name.loc, arena)
            };

            // Handle module constraint: module M: T = E
            let (name_doc, mod_expr_to_print) = match &mod_expr.pmod_desc {
                ModuleExprDesc::Pmod_constraint(inner_mod, mod_type)
                    if !parsetree_viewer::has_await_attribute(&mod_expr.pmod_attributes) =>
                {
                    let name_with_type = Doc::concat(vec![
                        name_doc,
                        Doc::text(": "),
                        print_module_type(state, mod_type, cmt_tbl, arena),
                    ]);
                    (name_with_type, inner_mod.as_ref())
                }
                _ => (name_doc, mod_expr),
            };

            let let_module_doc = Doc::concat(vec![
                Doc::text("module "),
                name_doc,
                Doc::text(" = "),
                print_mod_expr(state, mod_expr_to_print, cmt_tbl, arena),
            ]);

            rows.push(BlockRow {
                pos_range,
                start_line: start.line as usize,
                end_line: end.line as usize,
                doc: let_module_doc,
            });
            collect_block_rows_inner(state, body, rows, cmt_tbl, arena);
        }

        ExpressionDesc::Pexp_letexception(ext_constr, body) => {
            // Use combined location (expr start to ext_constr end) to match walker
            let base_start = arena.loc_start(expr.pexp_loc);
            let end = arena.loc_end(ext_constr.pext_loc);
            let pos_range = PosRange { start: base_start.cnum, end: end.cnum };

            // Extend start_line to leading comment if present (matching OCaml's location extension)
            let start_line = match cmt_tbl.get_first_leading_comment_by_pos(pos_range) {
                Some(comment) => comment.loc().loc_start.line as usize,
                None => base_start.line as usize,
            };

            let let_exception_doc = print_exception_def(state, ext_constr, cmt_tbl, arena);

            rows.push(BlockRow {
                pos_range,
                start_line,
                end_line: end.line as usize,
                doc: let_exception_doc,
            });
            collect_block_rows_inner(state, body, rows, cmt_tbl, arena);
        }

        ExpressionDesc::Pexp_open(override_flag, lid, body) => {
            // Use combined location (expr start to lid end) to match walker
            let start = arena.loc_start(expr.pexp_loc);
            let end = arena.loc_end(lid.loc);
            let pos_range = PosRange { start: start.cnum, end: end.cnum };

            let open_doc = Doc::concat(vec![
                Doc::text("open"),
                match override_flag {
                    OverrideFlag::Override => Doc::text("!"),
                    OverrideFlag::Fresh => Doc::nil(),
                },
                Doc::space(),
                print_longident_location(lid, cmt_tbl, arena),
            ]);

            rows.push(BlockRow {
                pos_range,
                start_line: start.line as usize,
                end_line: end.line as usize,
                doc: open_doc,
            });
            collect_block_rows_inner(state, body, rows, cmt_tbl, arena);
        }

        ExpressionDesc::Pexp_sequence(e1, e2) => {
            // Use e1's location directly
            let start = arena.loc_start(e1.pexp_loc);
            let end = arena.loc_end(e1.pexp_loc);
            let pos_range = PosRange { start: start.cnum, end: end.cnum };

            let expr_doc = {
                let doc = print_expression(state, e1, cmt_tbl, arena);
                match parens::expr(arena, e1) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, e1, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };

            rows.push(BlockRow {
                pos_range,
                start_line: start.line as usize,
                end_line: end.line as usize,
                doc: expr_doc,
            });
            collect_block_rows_inner(state, e2, rows, cmt_tbl, arena);
        }

        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            // Use first binding's location for comment lookup, matching OCaml's logic
            let (pos_range, base_start_line, end_line) = if let (Some(first), Some(last)) = (bindings.first(), bindings.last()) {
                // Get the binding location matching what the walker uses
                let binding_loc = if arena.loc_ghost(first.pvb_pat.ppat_loc) {
                    first.pvb_expr.pexp_loc
                } else {
                    first.pvb_loc
                };
                let base_start = arena.loc_start(binding_loc);
                let end = arena.loc_end(last.pvb_loc);
                let pos_range = PosRange { start: base_start.cnum, end: end.cnum };

                (
                    pos_range,
                    base_start.line as usize,
                    end.line as usize,
                )
            } else {
                let start = arena.loc_start(expr.pexp_loc);
                let end = arena.loc_end(expr.pexp_loc);
                let pos_range = PosRange { start: start.cnum, end: end.cnum };
                (
                    pos_range,
                    start.line as usize,
                    end.line as usize,
                )
            };

            // Extend start_line to leading comment if present (matching OCaml's location extension)
            let start_line = match cmt_tbl.get_first_leading_comment_by_pos(pos_range) {
                Some(comment) => comment.loc().loc_start.line as usize,
                None => base_start_line,
            };

            let rec_doc = match rec_flag {
                RecFlag::Nonrecursive => Doc::nil(),
                RecFlag::Recursive => Doc::text("rec "),
            };

            let let_doc = print_value_bindings_with_rec(state, bindings, &rec_doc, cmt_tbl, arena);

            rows.push(BlockRow {
                pos_range,
                start_line,
                end_line,
                doc: let_doc,
            });

            // Special case: if body is just (), don't print it
            // This matches OCaml: let () = { let () = foo(); () }
            if !is_unit_expr(arena, body) {
                collect_block_rows_inner(state, body, rows, cmt_tbl, arena);
            }
        }

        _ => {
            // Terminal expression
            let start = arena.loc_start(expr.pexp_loc);
            let end = arena.loc_end(expr.pexp_loc);
            let pos_range = PosRange { start: start.cnum, end: end.cnum };

            let expr_doc = {
                let doc = print_expression(state, expr, cmt_tbl, arena);
                match parens::expr(arena, expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };

            rows.push(BlockRow {
                pos_range,
                start_line: start.line as usize,
                end_line: end.line as usize,
                doc: expr_doc,
            });
        }
    }
}

/// Print value bindings with a pre-computed rec_doc (for use in expression blocks).
fn print_value_bindings_with_rec(
    state: &PrinterState,
    bindings: &[ValueBinding],
    rec_doc: &Doc,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    print_listi(
        |vb: &ValueBinding| vb.pvb_loc,
        bindings,
        |vb, cmt_tbl_inner, i| {
            print_value_binding(state, vb, i, rec_doc, cmt_tbl_inner, arena)
        },
        cmt_tbl,
        arena,
        false, // ignore_empty_lines
        false, // force_break
    )
}

/// Check if an expression is an array of tuples (for dict detection)
fn is_tuple_array(expr: &Expression) -> bool {
    if let ExpressionDesc::Pexp_array(items) = &expr.pexp_desc {
        items.iter().all(|e| matches!(&e.pexp_desc, ExpressionDesc::Pexp_tuple(_)))
    } else {
        false
    }
}

/// Try to print dict{} syntax for Primitive_dict.make([("key", value), ...])
fn try_print_dict_expr(
    state: &PrinterState,
    expr: &Expression,
    funct: &Expression,
    args: &[(ArgLabel, Expression)],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Option<Doc> {
    // Check if funct is Primitive_dict.make
    if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
        let longident = arena.get_longident(lid.txt);
        if let Longident::Ldot(parent_box, make_idx) = longident {
            let make_str = arena.get_string(*make_idx);
            if let Longident::Lident(prim_idx) = parent_box.as_ref() {
                let prim_str = arena.get_string(*prim_idx);
                if prim_str == "Primitive_dict" && make_str == "make" {
                    // Check if args has exactly one Nolabel argument that's an array of tuples
                    if args.len() == 1 {
                        if let (ArgLabel::Nolabel, key_values) = &args[0] {
                            if is_tuple_array(key_values) {
                                return Some(print_dict_expr(state, expr, key_values, cmt_tbl, arena));
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Print dict{key: value, ...} syntax
fn print_dict_expr(
    state: &PrinterState,
    expr: &Expression,
    key_values: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let start = arena.loc_start(expr.pexp_loc);
    let end = arena.loc_end(expr.pexp_loc);
    let force_break = start.line < end.line;

    let rows = if let ExpressionDesc::Pexp_array(items) = &key_values.pexp_desc {
        items
            .iter()
            .filter_map(|e| {
                if let ExpressionDesc::Pexp_tuple(tuple_items) = &e.pexp_desc {
                    if tuple_items.len() == 2 {
                        let key_expr = &tuple_items[0];
                        let value_expr = &tuple_items[1];
                        // Key should be a string constant
                        if let ExpressionDesc::Pexp_constant(Constant::String(txt, _)) =
                            &key_expr.pexp_desc
                        {
                            let key_doc = Doc::concat(vec![
                                Doc::text("\""),
                                Doc::text(txt.clone()),
                                Doc::text("\""),
                            ]);
                            let value_doc =
                                print_expression_with_comments(state, value_expr, cmt_tbl, arena);
                            return Some(Doc::concat(vec![key_doc, Doc::text(": "), value_doc]));
                        }
                    }
                }
                None
            })
            .collect::<Vec<_>>()
    } else {
        vec![]
    };

    let inner = if rows.is_empty() {
        print_comments_inside(cmt_tbl, expr.pexp_loc, arena)
    } else {
        Doc::concat(vec![
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), rows),
            ])),
            Doc::trailing_comma(),
            Doc::soft_line(),
        ])
    };

    Doc::breakable_group(
        Doc::concat(vec![Doc::text("dict{"), inner, Doc::rbrace()]),
        force_break,
    )
}

/// Check if expression has res.taggedTemplate attribute
fn is_tagged_template_literal(expr: &Expression) -> bool {
    expr.pexp_attributes.iter().any(|(name, _)| name.txt == "res.taggedTemplate")
}

/// Print tagged template literal: tag`string ${expr} string`
fn print_tagged_template_literal(
    state: &PrinterState,
    funct: &Expression,
    args: &[(ArgLabel, Expression)],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // args should be [strings_array, values_array]
    if args.len() != 2 {
        return Doc::nil();
    }

    let strings = match &args[0].1.pexp_desc {
        ExpressionDesc::Pexp_array(items) => items,
        _ => return Doc::nil(),
    };

    let values = match &args[1].1.pexp_desc {
        ExpressionDesc::Pexp_array(items) => items,
        _ => return Doc::nil(),
    };

    // Build the template content by interleaving strings and ${values}
    let mut parts: Vec<Doc> = Vec::new();

    for (i, s) in strings.iter().enumerate() {
        // Extract string content
        if let ExpressionDesc::Pexp_constant(Constant::String(txt, _)) = &s.pexp_desc {
            parts.push(print_string_contents(txt));
        }

        // Add interpolation if there's a corresponding value
        if i < values.len() {
            parts.push(Doc::text("${"));
            parts.push(print_expression_with_comments(state, &values[i], cmt_tbl, arena));
            parts.push(Doc::text("}"));
        }
    }

    let tag_doc = print_expression_with_comments(state, funct, cmt_tbl, arena);

    Doc::concat(vec![
        tag_doc,
        Doc::text("`"),
        Doc::concat(parts),
        Doc::text("`"),
    ])
}

/// Print underscore apply sugar: `(__x) => f(a, __x, c)` prints as `f(a, _, c)`.
/// This prints the inner apply expression with __x replaced by _ at print time.
fn print_underscore_apply(
    state: &PrinterState,
    inner: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match &inner.pexp_desc {
        ExpressionDesc::Pexp_apply { funct, args, partial, .. } => {
            // Print function part normally
            let funct_doc = print_expression_with_comments(state, funct, cmt_tbl, arena);
            let funct_doc = match parens::call_expr(arena, funct) {
                ParenKind::Parenthesized => add_parens(funct_doc),
                ParenKind::Braced(loc) => print_braces(funct_doc, funct, loc, arena),
                ParenKind::Nothing => funct_doc,
            };

            // Print arguments, but substitute __x with _
            let args_doc = print_arguments_with_underscore_subst(state, args, *partial, cmt_tbl, arena);
            Doc::group(Doc::concat(vec![funct_doc, args_doc]))
        }
        _ => print_expression_with_comments(state, inner, cmt_tbl, arena),
    }
}

/// Print a single argument with __x substituted by _.
fn print_argument_with_underscore_subst(
    state: &PrinterState,
    label: &ArgLabel,
    expr: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Check if this is __x and print _ instead
    if parsetree_viewer::is_underscore_ident(expr, arena) {
        match label {
            ArgLabel::Nolabel => {
                let doc = Doc::text("_");
                print_comments(doc, cmt_tbl, expr.pexp_loc, arena)
            }
            ArgLabel::Labelled(name) => {
                let name_str = arena.get_string(name.txt);
                Doc::concat(vec![
                    Doc::text("~"),
                    print_ident_like(name_str, false, false),
                    Doc::text("=_"),
                ])
            }
            ArgLabel::Optional(name) => {
                let name_str = arena.get_string(name.txt);
                Doc::concat(vec![
                    Doc::text("~"),
                    print_ident_like(name_str, false, false),
                    Doc::text("=?_"),
                ])
            }
        }
    } else {
        // Use regular argument printing for non-__x arguments
        print_argument(state, label, expr, cmt_tbl, arena)
    }
}

/// Print arguments with __x substituted by _.
fn print_arguments_with_underscore_subst(
    state: &PrinterState,
    args: &[(ArgLabel, Expression)],
    partial: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if args.is_empty() {
        return Doc::text("()");
    }

    let all_docs: Vec<Doc> = args
        .iter()
        .map(|(lbl, arg)| print_argument_with_underscore_subst(state, lbl, arg, cmt_tbl, arena))
        .collect();

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
    // Check for tagged template literal: tag`...`
    if is_tagged_template_literal(expr) {
        return print_tagged_template_literal(state, funct, args, cmt_tbl, arena);
    }

    // Check for dict{} syntax: Primitive_dict.make([("key", value), ...])
    if let Some(doc) = try_print_dict_expr(state, expr, funct, args, cmt_tbl, arena) {
        return doc;
    }

    // Check for binary expression
    if args.len() == 2 {
        if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
            if let Longident::Lident(op_idx) = arena.get_longident(ident.txt) {
                let op = arena.get_string(*op_idx);
                if parsetree_viewer::is_binary_operator_str(op) {
                    let doc = print_binary_expression(state, expr, op, args, cmt_tbl, arena);
                    // Print attributes - binary expressions handle their own attributes
                    let printable_attrs: Vec<&Attribute> = expr.pexp_attributes
                        .iter()
                        .filter(|attr| parsetree_viewer::is_printable_attribute(attr))
                        .collect();
                    if !printable_attrs.is_empty() {
                        // Print attributes and wrap in parens if there are any
                        let attrs_doc = print_attributes(state, &expr.pexp_attributes, cmt_tbl, arena);
                        return Doc::concat(vec![attrs_doc, add_parens(doc)]);
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

    // Print attributes - Pexp_apply handles its own attributes
    let attrs_doc = print_attributes(state, &expr.pexp_attributes, cmt_tbl, arena);

    // Check for callback in first or last position for special formatting
    if parsetree_viewer::requires_special_callback_printing_first_arg(args) {
        let args_doc = print_arguments_with_callback_in_first_position(state, args, partial, cmt_tbl, arena);
        Doc::concat(vec![attrs_doc, funct_doc, args_doc])
    } else if parsetree_viewer::requires_special_callback_printing_last_arg(args) {
        let args_doc = print_arguments_with_callback_in_last_position(state, args, partial, cmt_tbl, arena);
        // Check if args doc will break - if so, add break_parent
        // Fixes layout issues with nested callbacks
        let maybe_break_parent = if args_doc.will_break() {
            Doc::break_parent()
        } else {
            Doc::nil()
        };
        Doc::concat(vec![maybe_break_parent, attrs_doc, funct_doc, args_doc])
    } else {
        let args_doc = print_arguments(state, args, partial, cmt_tbl, arena);
        Doc::group(Doc::concat(vec![attrs_doc, funct_doc, args_doc]))
    }
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

    // Special case for Pexp_setfield: OCaml's print_operand handles this specially
    // - only needs parens when on LHS of a binary expression
    // - does NOT go through the general binary_expr_operand check
    if matches!(&operand.pexp_desc, ExpressionDesc::Pexp_setfield(_, _, _)) {
        if is_lhs {
            return ParenKind::Parenthesized;
        } else {
            return ParenKind::Nothing;
        }
    }

    // Special case for #= (JS object set): OCaml's print_operand handles this similarly to setfield
    // (node["left"] = value)->pipe should keep the parens
    if let ExpressionDesc::Pexp_apply { funct, args, .. } = &operand.pexp_desc {
        if args.len() == 2 {
            if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                if arena.is_lident(lid.txt, "#=") {
                    if is_lhs {
                        return ParenKind::Parenthesized;
                    } else {
                        return ParenKind::Nothing;
                    }
                }
            }
        }
    }

    // Fall back to the general binary_expr_operand check for non-binary expressions
    parens::binary_expr_operand(arena, is_lhs, operand)
}

/// Print binary expression.
/// This matches OCaml's print_binary_expression logic for spacing and indentation.
fn print_binary_expression(
    state: &PrinterState,
    expr: &Expression,
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

    // Pipe-first (->) has special handling
    let is_pipe_first = op == "->";

    if is_pipe_first {
        // Pipe operator: no spaces, use soft_line for break opportunities
        Doc::group(Doc::concat(vec![
            lhs_doc,
            Doc::soft_line(),
            Doc::text(op),
            rhs_doc,
        ]))
    } else {
        // Non-pipe binary operators
        // Check if RHS should be inlined (always space) or can break (line that becomes space or newline)
        let inline_rhs = parsetree_viewer::should_inline_rhs_binary_expr(rhs);

        // Spacing after operator: space if inline, line (breakable) if not inline
        let spacing_after = if inline_rhs {
            Doc::space()
        } else {
            Doc::line()
        };

        // Build operator + rhs, potentially with indentation
        // Use should_indent_binary_expr which checks:
        // 1. Is operator an equality operator?
        // 2. Is LHS NOT a same-precedence sub-expression?
        // 3. Is operator `:=`?
        let should_indent = parsetree_viewer::should_indent_binary_expr(arena, expr);

        let operator_with_rhs = Doc::concat(vec![
            Doc::space(),
            Doc::text(op),
            spacing_after,
            rhs_doc,
        ]);

        let right = if should_indent {
            Doc::group(Doc::indent(operator_with_rhs))
        } else {
            operator_with_rhs
        };

        Doc::group(Doc::concat(vec![lhs_doc, right]))
    }
}

/// Check if operator is an equality operator (==, ===, !=, !==).
fn is_equality_operator(op: &str) -> bool {
    matches!(op, "==" | "===" | "!=" | "!==")
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

/// Print label prefix for argument (used in callback printing).
fn print_arg_label_prefix(label: &ArgLabel, arena: &ParseArena) -> Doc {
    match label {
        ArgLabel::Nolabel => Doc::nil(),
        ArgLabel::Labelled(name) => {
            let name_str = arena.get_string(name.txt);
            Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text("=")])
        }
        ArgLabel::Optional(name) => {
            let name_str = arena.get_string(name.txt);
            Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text("=?")])
        }
    }
}

/// Print arguments with callback in last position.
/// Uses custom_layout to try different layouts for callback formatting.
fn print_arguments_with_callback_in_last_position(
    state: &PrinterState,
    args: &[(ArgLabel, Expression)],
    partial: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Because the same subtree gets printed twice, we need to copy the cmt_tbl.
    // Consumed comments need to be marked not-consumed and reprinted.
    let state = state.next_custom_layout();

    // If we should break callback due to nesting depth, just break all args
    // IMPORTANT: Check this BEFORE computing anything to avoid infinite recursion
    if state.should_break_callback() {
        return print_arguments(&state, args, partial, cmt_tbl, arena);
    }

    let cmt_tbl_copy = cmt_tbl.copy();
    let cmt_tbl_copy2 = cmt_tbl.copy();

    // Split args into non-callback args and the callback
    let (non_callback_args, callback) = if let Some(((label, expr), rest)) = args.split_last() {
        // Print label prefix for the callback
        let lbl_doc = print_arg_label_prefix(label, arena);

        // Print non-callback args
        let mut printed_args = Vec::new();
        for (lbl, arg) in rest {
            printed_args.push(print_argument(&state, lbl, arg, cmt_tbl, arena));
        }
        let printed_args_doc = if printed_args.is_empty() {
            Doc::nil()
        } else {
            Doc::concat(vec![
                Doc::join(Doc::concat(vec![Doc::comma(), Doc::line()]), printed_args),
                Doc::comma(),
                Doc::line(),
            ])
        };

        // Callback that fits on one line
        let callback_fits_on_one_line = {
            let mut cmt_tbl_for_callback = cmt_tbl.copy();
            let pexp_fun_doc = print_pexp_fun(&state, InCallback::FitsOnOneLine, expr, &mut cmt_tbl_for_callback, arena);
            let doc = Doc::concat(vec![lbl_doc.clone(), pexp_fun_doc]);
            print_comments(doc, &mut cmt_tbl_for_callback, expr.pexp_loc, arena)
        };

        // Callback with arguments fitting on one line (body may break)
        let callback_args_fit_on_one_line = {
            let mut cmt_tbl_for_callback = cmt_tbl_copy.clone();
            let pexp_fun_doc = print_pexp_fun(&state, InCallback::ArgumentsFitOnOneLine, expr, &mut cmt_tbl_for_callback, arena);
            let doc = Doc::concat(vec![lbl_doc, pexp_fun_doc]);
            print_comments(doc, &mut cmt_tbl_for_callback, expr.pexp_loc, arena)
        };

        (printed_args_doc, (callback_fits_on_one_line, callback_args_fit_on_one_line))
    } else {
        return Doc::text("()");
    };

    let (callback_fits_on_one_line, callback_args_fit_on_one_line) = callback;

    // If we should break callback due to nesting depth, just break all args
    // IMPORTANT: Check this BEFORE computing break_all_args to avoid infinite recursion
    if state.should_break_callback() {
        let mut cmt_tbl_for_break = cmt_tbl_copy2;
        return print_arguments(&state, args, partial, &mut cmt_tbl_for_break, arena);
    }

    // Thing.map(
    //   arg1,
    //   arg2,
    //   arg3,
    //   (param1, parm2) => doStuff(param1, parm2)
    // )
    let break_all_args = {
        let mut cmt_tbl_for_break = cmt_tbl_copy2;
        print_arguments(&state, args, partial, &mut cmt_tbl_for_break, arena)
    };

    // Check if any non-callback args will break - check before consuming non_callback_args
    if non_callback_args.will_break() {
        return break_all_args;
    }

    // Thing.map(foo, (arg1, arg2) => MyModuleBlah.toList(argument))
    let fits_on_one_line = Doc::concat(vec![
        Doc::lparen(),
        non_callback_args.clone(),
        callback_fits_on_one_line,
        Doc::rparen(),
    ]);

    // Thing.map(longArgument, veryLooooongArgument, (arg1, arg2) =>
    //   MyModuleBlah.toList(argument)
    // )
    let arguments_fit_on_one_line = Doc::concat(vec![
        Doc::lparen(),
        non_callback_args,
        Doc::breakable_group(callback_args_fit_on_one_line, true),
        Doc::rparen(),
    ]);

    Doc::custom_layout(vec![
        fits_on_one_line,
        arguments_fit_on_one_line,
        break_all_args,
    ])
}

/// Print arguments with callback in first position.
/// Uses custom_layout to try different layouts for callback formatting.
fn print_arguments_with_callback_in_first_position(
    state: &PrinterState,
    args: &[(ArgLabel, Expression)],
    partial: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Because the same subtree gets printed twice, we need to copy the cmt_tbl.
    let state = state.next_custom_layout();

    // If we should break callback due to nesting depth, just break all args
    // IMPORTANT: Check this BEFORE computing anything to avoid infinite recursion
    if state.should_break_callback() {
        return print_arguments(&state, args, partial, cmt_tbl, arena);
    }

    let cmt_tbl_copy = cmt_tbl.copy();

    // Split args into callback and non-callback args
    let (callback_doc, printed_args_doc) = if let Some(((label, expr), rest)) = args.split_first() {
        // Print label prefix for the callback
        let lbl_doc = print_arg_label_prefix(label, arena);

        // Print callback
        let callback = {
            let pexp_fun_doc = print_pexp_fun(&state, InCallback::FitsOnOneLine, expr, cmt_tbl, arena);
            let doc = Doc::concat(vec![lbl_doc, pexp_fun_doc]);
            print_comments(doc, cmt_tbl, expr.pexp_loc, arena)
        };

        // Print non-callback args
        let mut printed_args = Vec::new();
        for (lbl, arg) in rest {
            printed_args.push(print_argument(&state, lbl, arg, cmt_tbl, arena));
        }
        let printed_args_doc = Doc::join(Doc::concat(vec![Doc::comma(), Doc::line()]), printed_args);

        (callback, printed_args_doc)
    } else {
        return Doc::text("()");
    };

    // Thing.map((arg1, arg2) => MyModuleBlah.toList(argument), foo)
    // Thing.map((arg1, arg2) => {
    //   MyModuleBlah.toList(argument)
    // }, longArgument, veryLooooongArgument)
    let fits_on_one_line = Doc::concat(vec![
        Doc::lparen(),
        callback_doc,
        Doc::comma(),
        Doc::line(),
        printed_args_doc.clone(),
        Doc::rparen(),
    ]);

    // Thing.map(
    //   (param1, parm2) => doStuff(param1, parm2),
    //   arg1,
    //   arg2,
    //   arg3,
    // )
    let break_all_args = {
        let mut cmt_tbl_for_break = cmt_tbl_copy;
        print_arguments(&state, args, partial, &mut cmt_tbl_for_break, arena)
    };

    // Check if any non-callback args will break
    if printed_args_doc.will_break() {
        return break_all_args;
    }

    Doc::custom_layout(vec![fits_on_one_line, break_all_args])
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
            if has_leading_line_comment(cmt_tbl, expr.pexp_loc, arena) {
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
        .map(|(label, expr)| print_argument(state, label, expr, cmt_tbl, arena))
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

/// Print a single function argument (like OCaml's print_argument).
/// Properly handles comments attached to the combined label+expression location.
fn print_argument(
    state: &PrinterState,
    label: &ArgLabel,
    expr: &Expression,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match label {
        ArgLabel::Nolabel => {
            // Unlabelled argument - just print the expression with its comments
            let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
            match parens::expr(arena, expr) {
                ParenKind::Parenthesized => add_parens(doc),
                ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                ParenKind::Nothing => doc,
            }
        }
        ArgLabel::Labelled(name) => {
            let name_str = arena.get_string(name.txt);
            // Check for punning: ~name where expr is Pexp_ident(Lident(name))
            if let ExpressionDesc::Pexp_ident(lid) = &expr.pexp_desc {
                if let Longident::Lident(ident_idx) = arena.get_longident(lid.txt) {
                    let ident_str = arena.get_string(*ident_idx);
                    if ident_str == name_str && expr.pexp_attributes.is_empty() && !parsetree_viewer::is_braced_expr(expr) {
                        // Punned: ~name - create combined location from label to expression end
                        let (pos_range, full_loc) = make_combined_pos_range(name.loc, expr.pexp_loc, arena);
                        let doc = Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false)]);
                        return print_comments_by_pos(doc, cmt_tbl, pos_range, &full_loc);
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
                                // Punned with type: ~name: type
                                let (pos_range, full_loc) = make_combined_pos_range(name.loc, expr.pexp_loc, arena);
                                let typ_doc = print_typ_expr(state, typ, cmt_tbl, arena);
                                let doc = Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text(": "), typ_doc]);
                                return print_comments_by_pos(doc, cmt_tbl, pos_range, &full_loc);
                            }
                        }
                    }
                }
            }
            // Regular labelled argument: ~name=expr
            // Print the label part with comments from label location
            let lbl_doc = Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text("=")]);
            let lbl_doc = print_comments(lbl_doc, cmt_tbl, name.loc, arena);
            // Print expression with comments
            let expr_doc = {
                let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                match parens::expr(arena, expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };
            // Create combined location from label to expression end and wrap
            let (pos_range, full_loc) = make_combined_pos_range(name.loc, expr.pexp_loc, arena);
            let doc = Doc::concat(vec![lbl_doc, expr_doc]);
            print_comments_by_pos(doc, cmt_tbl, pos_range, &full_loc)
        }
        ArgLabel::Optional(name) => {
            let name_str = arena.get_string(name.txt);
            // Check for punning: ~name? where expr is Pexp_ident(Lident(name))
            if let ExpressionDesc::Pexp_ident(lid) = &expr.pexp_desc {
                if let Longident::Lident(ident_idx) = arena.get_longident(lid.txt) {
                    let ident_str = arena.get_string(*ident_idx);
                    if ident_str == name_str && expr.pexp_attributes.is_empty() {
                        // Punned optional: ~name?
                        let doc = Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text("?")]);
                        return print_comments(doc, cmt_tbl, name.loc, arena);
                    }
                }
            }
            // Regular optional argument: ~name=?expr
            let lbl_doc = Doc::concat(vec![Doc::text("~"), print_ident_like(name_str, false, false), Doc::text("=?")]);
            let lbl_doc = print_comments(lbl_doc, cmt_tbl, name.loc, arena);
            let expr_doc = {
                let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                match parens::expr(arena, expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                    ParenKind::Nothing => doc,
                }
            };
            let (pos_range, full_loc) = make_combined_pos_range(name.loc, expr.pexp_loc, arena);
            let doc = Doc::concat(vec![lbl_doc, expr_doc]);
            print_comments_by_pos(doc, cmt_tbl, pos_range, &full_loc)
        }
    }
}

/// Create a combined position range and full location spanning from start loc to end of end_loc.
/// Used for argument locations that span label to expression.
fn make_combined_pos_range(start_loc: LocIdx, end_loc: LocIdx, arena: &ParseArena) -> (PosRange, FullLocation) {
    let start = arena.loc_start(start_loc);
    let end = arena.loc_end(end_loc);
    let pos_range = PosRange {
        start: start.cnum,
        end: end.cnum,
    };
    let full_loc = FullLocation::from_positions(start.clone(), end.clone());
    (pos_range, full_loc)
}

/// Check if an expression is unit: `()`
fn is_unit_expr(arena: &ParseArena, expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_construct(lid, None) => arena.is_lident(lid.txt, "()"),
        _ => false,
    }
}

/// Check if there's a leading line comment at a location.
fn has_leading_line_comment(cmt_tbl: &CommentTable, loc: LocIdx, arena: &ParseArena) -> bool {
    if let Some(comments) = cmt_tbl.get_leading_comments(loc, arena) {
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
            // Wrap alias with print_comments for its location (like OCaml reference)
            let alias_doc = print_ident_like(&alias.txt, false, false);
            let alias_doc = print_comments(alias_doc, cmt_tbl, alias.loc, arena);
            Doc::concat(vec![pat_doc, Doc::text(" as "), alias_doc])
        }
        // module(M)
        PatternDesc::Ppat_unpack(name) => {
            // Wrap name with print_comments for its location (like OCaml reference)
            let name_doc = Doc::text(&name.txt);
            let name_doc = print_comments(name_doc, cmt_tbl, name.loc, arena);
            Doc::concat(vec![Doc::text("module("), name_doc, Doc::text(")")])
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
    let doc = if pat.ppat_attributes.is_empty() {
        pattern_without_attrs
    } else {
        let attrs = print_attributes(state, &pat.ppat_attributes, cmt_tbl, arena);
        Doc::group(Doc::concat(vec![attrs, pattern_without_attrs]))
    };

    // Wrap with print_comments to attach leading/trailing comments
    print_comments(doc, cmt_tbl, pat.ppat_loc, arena)
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
                    // For punned patterns, wrap label with print_comments (like OCaml reference)
                    let label = print_comments(label, cmt_tbl, field.lid.loc, arena);
                    if field.opt {
                        Doc::concat(vec![Doc::text("?"), label])
                    } else {
                        label
                    }
                } else {
                    // For non-punned patterns, comments are attached to BOTH:
                    // 1. Individual component locations (label and pattern)
                    // 2. The combined row location
                    // Like OCaml: print_lident_path (with comments on label), then print_comments on row
                    let label = print_comments(label, cmt_tbl, field.lid.loc, arena);
                    let pat_doc = print_pattern(state, &field.pat, cmt_tbl, arena);
                    // Check if pattern needs parentheses in record row context
                    let pat_doc = if parens::pattern_record_row_rhs(&field.pat) {
                        add_parens(pat_doc)
                    } else {
                        pat_doc
                    };
                    let rhs_doc = if field.opt {
                        Doc::concat(vec![Doc::text("?"), pat_doc])
                    } else {
                        pat_doc
                    };
                    let doc = Doc::group(Doc::concat(vec![label, Doc::text(": "), rhs_doc]));
                    // Create combined location and use position-based lookup explicitly
                    // This is needed because during walking, a new LocIdx is created for the combined
                    // location that won't match any AST node's LocIdx during printing
                    let pos_range = PosRange {
                        start: arena.loc_start(field.lid.loc).cnum,
                        end: arena.loc_end(field.pat.ppat_loc).cnum,
                    };
                    // Use explicit position-based lookup (not the generic print_comments)
                    let leading = cmt_tbl.remove_leading_comments_by_pos(pos_range);
                    let trailing = cmt_tbl.remove_trailing_comments_by_pos(pos_range);
                    let full_loc = FullLocation {
                        loc_start: arena.loc_start(field.lid.loc).clone(),
                        loc_end: arena.loc_end(field.pat.ppat_loc).clone(),
                        loc_ghost: false,
                    };
                    let doc = print_leading_comments_with(doc, leading, &full_loc);
                    print_trailing_comments_with(doc, trailing, &full_loc)
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

    let doc = if !typ.ptyp_attributes.is_empty() && !should_print_its_own_attributes {
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
    };

    // Wrap with print_comments to attach leading/trailing comments (like OCaml reference)
    print_comments(doc, cmt_tbl, typ.ptyp_loc, arena)
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
    // Wrap lid with print_comments for its location (like OCaml reference)
    let lid_doc = print_comments(lid_doc, cmt_tbl, lid.loc, arena);

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
        .map(|attr| print_attribute(state, attr, false, cmt_tbl, arena))
        .collect();
    // Join with line (space when flat, newline when breaking), add trailing line
    Doc::concat(vec![Doc::group(Doc::join(Doc::line(), attrs_doc)), Doc::line()])
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
        .map(|attr| print_attribute(state, attr, false, cmt_tbl, arena))
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

    // Use the full span from label to end of module type (if present) for comment attachment.
    // This matches OCaml's behavior where cmt_loc spans {lbl.loc with loc_end = mod_type.pmty_loc.loc_end}
    match param.mod_type {
        None => print_comments(doc, cmt_tbl, param.lbl.loc, arena),
        Some(mod_type) => {
            let (pos_range, full_loc) = make_combined_pos_range(param.lbl.loc, mod_type.pmty_loc, arena);
            print_comments_by_pos(doc, cmt_tbl, pos_range, &full_loc)
        }
    }
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
    // Track whether attributes are printed inside the match arm
    let attrs_already_printed = matches!(
        &mty.pmty_desc,
        ModuleTypeDesc::Pmty_functor(_, _, _)
            | ModuleTypeDesc::Pmty_signature(_)
            | ModuleTypeDesc::Pmty_ident(_)
    );

    let doc = match &mty.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => {
            // Print attributes with the identifier
            Doc::concat(vec![
                print_attributes(state, &mty.pmty_attributes, cmt_tbl, arena),
                print_longident_location(lid, cmt_tbl, arena),
            ])
        }

        ModuleTypeDesc::Pmty_signature(signature) if signature.is_empty() => {
            let comments_inside = print_comments_inside(cmt_tbl, mty.pmty_loc, arena);
            if !matches!(comments_inside, Doc::Nil) {
                // Has comments inside
                Doc::concat(vec![
                    print_attributes(state, &mty.pmty_attributes, cmt_tbl, arena),
                    Doc::lbrace(),
                    comments_inside,
                    Doc::rbrace(),
                ])
            } else {
                // No comments - use two soft_line elements
                let should_break = arena.loc_start(mty.pmty_loc).line < arena.loc_end(mty.pmty_loc).line;
                Doc::concat(vec![
                    print_attributes(state, &mty.pmty_attributes, cmt_tbl, arena),
                    Doc::breakable_group(
                        Doc::concat(vec![
                            Doc::lbrace(),
                            Doc::soft_line(),
                            Doc::soft_line(),
                            Doc::rbrace(),
                        ]),
                        should_break,
                    ),
                ])
            }
        }

        ModuleTypeDesc::Pmty_signature(signature) => {
            Doc::concat(vec![
                print_attributes(state, &mty.pmty_attributes, cmt_tbl, arena),
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
                ),
            ])
        }

        ModuleTypeDesc::Pmty_functor(_, _, _) => {
            print_module_type_functor(state, mty, cmt_tbl, arena)
        }

        ModuleTypeDesc::Pmty_with(mod_type, with_constraints) => {
            let base_doc = print_module_type(state, mod_type, cmt_tbl, arena);
            let base_doc = if parens::mod_type_with_operand(mod_type) {
                Doc::concat(vec![Doc::lparen(), base_doc, Doc::rparen()])
            } else {
                base_doc
            };
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

    // Handle await attribute
    let doc = if parsetree_viewer::has_await_attribute(&mty.pmty_attributes) {
        Doc::concat(vec![Doc::text("await "), doc])
    } else {
        doc
    };

    // Add attributes for types that don't print them internally
    let doc = if !attrs_already_printed && !mty.pmty_attributes.is_empty() {
        Doc::concat(vec![
            print_attributes(state, &mty.pmty_attributes, cmt_tbl, arena),
            doc,
        ])
    } else {
        doc
    };

    print_comments(doc, cmt_tbl, mty.pmty_loc, arena)
}

/// Print a functor module type.
/// Matches OCaml's print_mod_type for Pmty_functor.
fn print_module_type_functor(
    state: &PrinterState,
    mty: &ModuleType,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (parameters, return_mty) = mod_type_functor(mty);

    let parameters_doc = match parameters.as_slice() {
        // Empty parameters list - shouldn't happen but handle gracefully
        [] => Doc::nil(),
        // Unit parameter: `()`
        [(attrs, lbl, None)] if lbl.txt == "*" => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            let doc = Doc::concat(vec![attrs_doc, Doc::text("()")]);
            print_comments(doc, cmt_tbl, lbl.loc, arena)
        }
        // Single anonymous parameter with type: just print the type (no `_:`)
        // Use the param_mty location for comments since it covers the whole param
        [(attrs, _lbl, Some(param_mty))] if _lbl.txt == "_" => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
            let doc = Doc::concat(vec![
                attrs_doc,
                print_module_type(state, param_mty, cmt_tbl, arena),
            ]);
            // Comments are handled by print_module_type, just return doc
            doc
        }
        // Multiple parameters - use parens
        params => {
            Doc::group(Doc::concat(vec![
                Doc::lparen(),
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(
                        Doc::concat(vec![Doc::comma(), Doc::line()]),
                        params
                            .iter()
                            .map(|(attrs, lbl, opt_mty)| {
                                let attrs_doc = print_attributes(state, attrs, cmt_tbl, arena);
                                // For "_" label, don't print the label
                                let lbl_doc = if lbl.txt == "_" {
                                    Doc::nil()
                                } else if lbl.txt == "*" {
                                    print_comments(Doc::text("()"), cmt_tbl, lbl.loc, arena)
                                } else {
                                    print_comments(Doc::text(&lbl.txt), cmt_tbl, lbl.loc, arena)
                                };
                                let doc = match opt_mty {
                                    None => Doc::concat(vec![attrs_doc, lbl_doc]),
                                    Some(mty) => {
                                        // For "_" label, don't add `: ` prefix
                                        let sep = if lbl.txt == "_" { Doc::nil() } else { Doc::text(": ") };
                                        Doc::concat(vec![
                                            attrs_doc,
                                            lbl_doc,
                                            sep,
                                            print_module_type(state, mty, cmt_tbl, arena),
                                        ])
                                    }
                                };
                                // Create combined position range for comments: from label start to mod_type end
                                // This matches OCaml which wraps the whole parameter with print_comments cmt_loc
                                let (pos_range, full_loc) = match opt_mty {
                                    None => {
                                        let fl = arena.to_location(lbl.loc);
                                        (PosRange::from_location(&fl), fl)
                                    }
                                    Some(mty) => {
                                        let start = arena.loc_start(lbl.loc);
                                        let end = arena.loc_end(mty.pmty_loc);
                                        let full_loc = FullLocation::from_positions(start.clone(), end.clone());
                                        (PosRange::from_location(&full_loc), full_loc)
                                    }
                                };
                                // Wrap entire parameter doc with comments using combined location
                                print_comments_by_pos(doc, cmt_tbl, pos_range, &full_loc)
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

    let return_doc = {
        let doc = print_module_type(state, return_mty, cmt_tbl, arena);
        if parens::mod_type_functor_return(return_mty) {
            add_parens(doc)
        } else {
            doc
        }
    };

    Doc::group(Doc::concat(vec![
        parameters_doc,
        Doc::group(Doc::concat(vec![
            Doc::text(" =>"),
            Doc::line(),
            return_doc,
        ])),
    ]))
}

/// Extract functor parameters from a module type.
/// Returns a list of (attributes, label, optional param type) and the return type.
fn mod_type_functor(mty: &ModuleType) -> (Vec<(&[Attribute], &StringLoc, Option<&ModuleType>)>, &ModuleType) {
    fn loop_functor<'a>(
        acc: Vec<(&'a [Attribute], &'a StringLoc, Option<&'a ModuleType>)>,
        mty: &'a ModuleType,
    ) -> (Vec<(&'a [Attribute], &'a StringLoc, Option<&'a ModuleType>)>, &'a ModuleType) {
        match &mty.pmty_desc {
            ModuleTypeDesc::Pmty_functor(lbl, mod_type, return_mty) => {
                let mut new_acc = acc;
                // Attributes on functor are for the parameter
                new_acc.push((&mty.pmty_attributes, lbl, mod_type.as_ref().map(|t| t.as_ref())));
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
                print_type_declaration_with_lid(state, lid, type_decl, false, cmt_tbl, arena),
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
                print_type_declaration_with_lid(state, lid, type_decl, true, cmt_tbl, arena),
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
/// `is_substitution` determines whether to use `:=` (true) or `=` (false).
fn print_type_declaration_with_lid(
    state: &PrinterState,
    lid: &crate::parse_arena::Located<crate::parse_arena::LidentIdx>,
    decl: &TypeDeclaration,
    is_substitution: bool,
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
        Doc::group(Doc::concat(vec![
            Doc::less_than(),
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), params),
            ])),
            Doc::trailing_comma(),
            Doc::soft_line(),
            Doc::greater_than(),
        ]))
    };

    let eq_sign = if is_substitution { " := " } else { " = " };
    let manifest_doc = match &decl.ptype_manifest {
        Some(typ) => Doc::concat(vec![Doc::text(eq_sign), print_typ_expr(state, typ, cmt_tbl, arena)]),
        None => Doc::nil(),
    };

    // Print type constraints (constraint 'a = int)
    let constraints_doc = print_type_constraints(state, &decl.ptype_cstrs, cmt_tbl, arena);

    Doc::concat(vec![name_doc, params_doc, manifest_doc, constraints_doc])
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
    // print_cases_with_braces returns the full { cases } block
    let cases_block = print_cases_with_braces(state, cases, cmt_tbl, arena);
    Doc::concat(vec![
        Doc::text("switch "),
        scrutinee,
        Doc::space(),
        cases_block,
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
    let body = {
        let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
        match parens::expr(arena, expr) {
            ParenKind::Parenthesized => add_parens(doc),
            ParenKind::Braced(braces) => print_braces(doc, expr, braces, arena),
            ParenKind::Nothing => doc,
        }
    };
    let cases_block = print_cases_with_braces(state, cases, cmt_tbl, arena);
    Doc::concat(vec![
        Doc::text("try "),
        body,
        Doc::text(" catch "),
        cases_block,
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

/// Get the position range for a case (from pattern start to rhs end).
/// This is used for comment attachment.
fn get_case_pos_range(case: &Case, arena: &ParseArena) -> PosRange {
    let start = arena.loc_start(case.pc_lhs.ppat_loc).cnum;
    let end = match parsetree_viewer::process_braces_attr(&case.pc_rhs) {
        Some(attr) => arena.loc_end(attr.0.loc).cnum,
        None => arena.loc_end(case.pc_rhs.pexp_loc).cnum,
    };
    PosRange { start, end }
}

/// Print match cases with surrounding braces.
/// Returns the full `{ cases }` block with proper formatting.
/// This matches OCaml's print_cases which returns a breakable_group with force_break.
fn print_cases_with_braces(
    state: &PrinterState,
    cases: &[Case],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let cases_doc = print_cases(state, cases, cmt_tbl, arena);
    Doc::breakable_group(
        Doc::concat(vec![
            Doc::text("{"),
            Doc::concat(vec![
                Doc::line(), // Line break after { (like OCaml's Doc.line)
                cases_doc,
            ]),
            Doc::line(), // Line break before }
            Doc::text("}"),
        ]),
        true, // force_break
    )
}

/// Print match cases.
/// This is similar to OCaml's print_list but specialized for cases.
fn print_cases(
    state: &PrinterState,
    cases: &[Case],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    if cases.is_empty() {
        return Doc::nil();
    }

    let mut result = Vec::new();
    let mut prev_end_line = 0i32;

    for (i, case) in cases.iter().enumerate() {
        let pos_range = get_case_pos_range(case, arena);
        let case_start_line = arena.loc_start(case.pc_lhs.ppat_loc).line as i32;

        // Add separator between cases (not before first case)
        if i > 0 {
            // Check if there was a blank line in the source
            let sep = if case_start_line - prev_end_line > 1 {
                // More than 1 line between: preserve blank line
                Doc::concat(vec![Doc::hard_line(), Doc::hard_line()])
            } else {
                Doc::hard_line()
            };
            result.push(sep);
        }

        // Print the case (without leading hard_line, that's handled above)
        let doc = print_case_content(state, case, cmt_tbl, arena);

        // Print leading and trailing comments for this case
        let leading = cmt_tbl.remove_leading_comments_by_pos(pos_range);
        let trailing = cmt_tbl.remove_trailing_comments_by_pos(pos_range);
        let full_loc = crate::location::Location {
            loc_start: arena.loc_start(case.pc_lhs.ppat_loc).clone(),
            loc_end: arena.loc_end(case.pc_rhs.pexp_loc).clone(),
            loc_ghost: false,
        };
        let doc_with_leading = print_leading_comments_with(doc, leading, &full_loc);
        let doc_with_comments = print_trailing_comments_with(doc_with_leading, trailing, &full_loc);

        result.push(doc_with_comments);

        // Track end line for next iteration
        prev_end_line = arena.loc_end(case.pc_rhs.pexp_loc).line as i32;
    }

    Doc::concat(result)
}

/// Print a single match case (content only, without leading hard_line).
/// The leading line breaks are handled by print_cases.
fn print_case_content(
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

    // Return just the grouped case content, without leading hard_line
    // (line breaks between cases are handled by print_cases)
    Doc::group(Doc::concat(vec![Doc::text("| "), content]))
}

// ============================================================================
// Value Bindings
// ============================================================================

/// Print value bindings using print_listi for proper comment handling.
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

    print_listi(
        |vb: &ValueBinding| vb.pvb_loc,
        bindings,
        |vb, cmt_tbl_inner, i| {
            print_value_binding(state, vb, i, &rec_doc, cmt_tbl_inner, arena)
        },
        cmt_tbl,
        arena,
        false, // ignore_empty_lines
        false, // force_break (will be calculated by print_listi)
    )
}

/// Print a single value binding.
fn print_value_binding(
    state: &PrinterState,
    vb: &ValueBinding,
    index: usize,
    rec_doc: &Doc,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Check for let.unwrap attribute
    let has_unwrap = vb.pvb_attributes.iter().any(|attr| attr.0.txt == "let.unwrap");

    // Print attributes (filtering out let.unwrap)
    let attrs: Vec<&Attribute> = vb.pvb_attributes
        .iter()
        .filter(|attr| attr.0.txt != "let.unwrap")
        .collect();
    // Use print_attributes_with_loc to get proper line breaking behavior
    // (uses Doc::line() which can collapse to space when the outer group fits)
    let attrs_doc = {
        let attrs_slice: Vec<Attribute> = attrs.into_iter().cloned().collect();
        print_attributes_with_loc(state, &attrs_slice, Some(vb.pvb_pat.ppat_loc), cmt_tbl, arena, false)
    };

    let header = if index == 0 {
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
    print_attributes_with_loc(state, attrs, None, cmt_tbl, arena, false)
}

/// Print attributes with location for proper line breaking.
/// If `loc` is provided and the attributed item starts on a different line
/// than the last attribute ends, uses hard_line instead of line.
/// Separates doc comments from regular attributes - doc comments go on their own line.
fn print_attributes_with_loc(
    state: &PrinterState,
    attrs: &[Attribute],
    loc: Option<Location>,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
    inline: bool,
) -> Doc {
    // Filter to printable attributes first
    let filtered: Vec<&Attribute> = attrs
        .iter()
        .filter(|attr| parsetree_viewer::is_printable_attribute(attr))
        .collect();
    if filtered.is_empty() {
        return Doc::nil();
    }

    // Partition doc comments from regular attributes
    // Only "res.doc" is a doc comment (printed as /** ... */), not "ocaml.doc"
    let (doc_comment_attrs, regular_attrs): (Vec<_>, Vec<_>) = filtered
        .into_iter()
        .partition(|attr| attr.0.txt == "res.doc");

    // Print doc comments with hard_line after each (they go on their own lines)
    let doc_comment_doc = if doc_comment_attrs.is_empty() {
        Doc::nil()
    } else {
        let docs: Vec<Doc> = doc_comment_attrs
            .iter()
            .map(|attr| print_attribute(state, attr, false, cmt_tbl, arena))
            .collect();
        // Doc comments each on their own line, with hard_line after
        Doc::concat(vec![Doc::join(Doc::hard_line(), docs), Doc::hard_line()])
    };

    // Print regular attributes
    let attrs_doc = if regular_attrs.is_empty() {
        Doc::nil()
    } else {
        let docs: Vec<Doc> = regular_attrs
            .iter()
            .map(|attr| print_attribute(state, attr, false, cmt_tbl, arena))
            .collect();

        // Determine trailing separator: use hard_line if attributed item is on a different line
        let has_doc_comments = !doc_comment_attrs.is_empty();
        let line_break = if inline {
            Doc::space()
        } else {
            match (loc, regular_attrs.last()) {
                (Some(item_loc), Some(last_attr)) => {
                    let (attr_name, _) = last_attr;
                    // If item starts on a later line than the last attribute, force hard line
                    if arena.loc_start(item_loc).line > arena.loc_end(attr_name.loc).line {
                        Doc::hard_line()
                    } else if has_doc_comments {
                        // If there were doc comments, use space (so @attr let x stays together)
                        Doc::space()
                    } else {
                        Doc::line()
                    }
                }
                _ => if has_doc_comments { Doc::space() } else { Doc::line() },
            }
        };

        Doc::concat(vec![Doc::group(Doc::join(Doc::line(), docs)), line_break])
    };

    Doc::concat(vec![doc_comment_doc, attrs_doc])
}

/// Print attributes with configurable separator (deprecated, use print_attributes_with_loc).
fn print_attributes_with_sep(
    state: &PrinterState,
    attrs: &[Attribute],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
    inline: bool,
) -> Doc {
    print_attributes_with_loc(state, attrs, None, cmt_tbl, arena, inline)
}

/// Print a single attribute.
/// If `standalone` is true, uses `@@` prefix (for floating attributes).
/// Otherwise uses `@` prefix.
fn print_attribute(
    state: &PrinterState,
    attr: &Attribute,
    standalone: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let (name, payload) = attr;

    // Handle doc comments specially
    if name.txt == "res.doc" {
        if let Payload::PStr(items) = payload {
            if let [single] = &items[..] {
                if let StructureItemDesc::Pstr_eval(expr, _) = &single.pstr_desc {
                    if let ExpressionDesc::Pexp_constant(Constant::String(txt, _)) = &expr.pexp_desc {
                        return Doc::concat(vec![
                            Doc::text(if standalone { "/***" } else { "/**" }),
                            Doc::text(txt.clone()),
                            Doc::text("*/"),
                        ]);
                    }
                }
            }
        }
    }

    let prefix = if standalone { "@@" } else { "@" };
    let attr_name = Doc::text(format!("{}{}", prefix, name.txt));

    Doc::group(Doc::concat(vec![
        attr_name,
        print_payload(state, payload, cmt_tbl, arena),
    ]))
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
            // Use print_type_declarations_with_rec_flag which handles everything
            // including the "type " prefix, using print_listi for proper comment handling
            print_type_declarations_with_rec_flag(state, rec_flag, type_decls, cmt_tbl, arena)
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
            print_attribute(state, attr, true, cmt_tbl, arena)
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
    let name_doc = print_comments(name_doc, cmt_tbl, decl.pmd_name.loc, arena);

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

/// Print recursive module declarations using print_listi for proper comment handling.
fn print_rec_module_declarations(
    state: &PrinterState,
    decls: &[ModuleDeclaration],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Use print_listi to properly handle comments between declarations
    print_listi(
        |decl: &ModuleDeclaration| decl.pmd_loc,
        decls,
        |decl: &ModuleDeclaration, cmt_tbl: &mut CommentTable, i: usize| {
            let attrs_doc = print_attributes(state, &decl.pmd_attributes, cmt_tbl, arena);
            let prefix = if i == 0 {
                Doc::text("module rec ")
            } else {
                Doc::text("and ")
            };
            let name_doc = Doc::text(&decl.pmd_name.txt);
            let name_doc = print_comments(name_doc, cmt_tbl, decl.pmd_name.loc, arena);

            // Handle alias vs regular module type
            let body_doc = match &decl.pmd_type.pmty_desc {
                ModuleTypeDesc::Pmty_alias(lid) => {
                    Doc::concat(vec![
                        Doc::text(" = "),
                        print_longident(arena, arena.get_longident(lid.txt)),
                    ])
                }
                ModuleTypeDesc::Pmty_with(..) => {
                    // Needs parens for with constraints
                    Doc::concat(vec![
                        Doc::text(": "),
                        Doc::lparen(),
                        print_module_type(state, &decl.pmd_type, cmt_tbl, arena),
                        Doc::rparen(),
                    ])
                }
                _ => {
                    Doc::concat(vec![
                        Doc::text(": "),
                        print_module_type(state, &decl.pmd_type, cmt_tbl, arena),
                    ])
                }
            };

            Doc::concat(vec![attrs_doc, prefix, name_doc, body_doc])
        },
        cmt_tbl,
        arena,
        false, // don't ignore empty lines
        false, // don't force break
    )
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
            // Use print_type_declarations_with_rec_flag which handles everything
            // including the "type " prefix, using print_listi for proper comment handling
            print_type_declarations_with_rec_flag(state, rec_flag, type_decls, cmt_tbl, arena)
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
            print_attribute(state, attr, true, cmt_tbl, arena)
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
            let expr_doc = {
                let doc = print_expression_with_comments(state, expr, cmt_tbl, arena);
                match parens::structure_expr(arena, expr) {
                    parens::ParenKind::Parenthesized => add_parens(doc),
                    parens::ParenKind::Braced(loc) => print_braces(doc, expr, loc, arena),
                    parens::ParenKind::Nothing => doc,
                }
            };
            Doc::concat(vec![attrs_doc, expr_doc])
        }
    }
}

/// Print type declarations.
/// Print type declarations with proper comment handling using print_listi.
/// This matches the OCaml implementation which uses print_listi for type declarations.
/// The rec_flag parameter is printed as part of the first declaration's prefix.
fn print_type_declarations_with_rec_flag(
    state: &PrinterState,
    rec_flag: &RecFlag,
    decls: &[TypeDeclaration],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let rec_doc = match rec_flag {
        RecFlag::Nonrecursive => Doc::nil(),
        RecFlag::Recursive => Doc::text("rec "),
    };

    // Use print_listi to properly handle comments between declarations
    // The "type " keyword is included in the first element's prefix,
    // so print_listi's print_comments wraps the entire declaration correctly.
    print_listi(
        |decl: &TypeDeclaration| decl.ptype_loc,
        decls,
        |decl: &TypeDeclaration, cmt_tbl: &mut CommentTable, i: usize| {
            // Match OCaml's print_type_declaration2 prefix logic:
            // if i > 0 then "and " else "type " ^ rec_flag
            let prefix = if i > 0 {
                Doc::text("and ")
            } else {
                Doc::concat(vec![Doc::text("type "), rec_doc.clone()])
            };
            // Print attributes + prefix + name + params + manifest + kind + constraints
            let attrs = print_attributes_with_loc(
                state,
                &decl.ptype_attributes,
                Some(decl.ptype_loc),
                cmt_tbl,
                arena,
                false,
            );
            Doc::group(Doc::concat(vec![
                attrs,
                prefix,
                print_type_declaration_inner(state, decl, cmt_tbl, arena, false),
            ]))
        },
        cmt_tbl,
        arena,
        false, // don't ignore empty lines
        false, // don't force break
    )
}

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
    // Use type declaration location for proper attribute line breaking
    let attrs_doc = if include_attrs {
        print_attributes_with_loc(
            state,
            &decl.ptype_attributes,
            Some(decl.ptype_loc),
            cmt_tbl,
            arena,
            false,
        )
    } else {
        Doc::nil()
    };
    let name_doc = print_ident_like(&decl.ptype_name.txt, false, false);
    let name_doc = print_comments(name_doc, cmt_tbl, decl.ptype_name.loc, arena);

    let params_doc = if decl.ptype_params.is_empty() {
        Doc::nil()
    } else {
        let params: Vec<Doc> = decl
            .ptype_params
            .iter()
            .map(|(typ, variance)| print_type_param(state, typ, variance, cmt_tbl, arena))
            .collect();
        Doc::group(Doc::concat(vec![
            Doc::less_than(),
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), params),
            ])),
            Doc::trailing_comma(),
            Doc::soft_line(),
            Doc::greater_than(),
        ]))
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
/// Print constructor declarations using print_listi for proper comment handling.
fn print_constructor_declarations(
    state: &PrinterState,
    constrs: &[ConstructorDeclaration],
    private_flag: &PrivateFlag,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Check if constructors span multiple lines (force_break)
    let force_break = match (constrs.first(), constrs.last()) {
        (Some(first), Some(last)) => {
            arena.loc_start(first.pcd_loc).line < arena.loc_end(last.pcd_loc).line
        }
        _ => false,
    };

    let private_doc = match private_flag {
        PrivateFlag::Private => Doc::concat(vec![Doc::text("private"), Doc::line()]),
        PrivateFlag::Public => Doc::nil(),
    };

    // Check if first constructor has attributes or is spread - if so, always show the bar
    let first_has_attrs = constrs.first().map_or(false, |c| {
        !c.pcd_attributes.is_empty() || c.pcd_name.txt == "..."
    });

    // Use print_listi for proper comment handling between constructors
    let rows = print_listi(
        |constr: &ConstructorDeclaration| constr.pcd_loc,
        constrs,
        |constr: &ConstructorDeclaration, cmt_tbl: &mut CommentTable, i: usize| {
            let bar = if i == 0 {
                if first_has_attrs {
                    // Always show bar when first constructor has attributes or is spread
                    Doc::text("| ")
                } else {
                    Doc::if_breaks(Doc::text("| "), Doc::nil())
                }
            } else {
                Doc::text("| ")
            };
            let doc = Doc::concat(vec![bar, print_constructor_declaration(state, constr, cmt_tbl, arena)]);
            // Wrap with print_comments for this constructor's location (matches OCaml)
            print_comments(doc, cmt_tbl, constr.pcd_loc, arena)
        },
        cmt_tbl,
        arena,
        true,  // ignore_empty_lines (OCaml passes this as true for constructors)
        force_break,
    );

    Doc::breakable_group(
        Doc::indent(Doc::concat(vec![Doc::line(), private_doc, rows])),
        force_break,
    )
}

/// Print a constructor declaration.
fn print_constructor_declaration(
    state: &PrinterState,
    constr: &ConstructorDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &constr.pcd_attributes, cmt_tbl, arena);
    let is_spread = constr.pcd_name.txt == "...";
    // Wrap constructor name with print_comments for its location (like OCaml)
    let name_doc = Doc::text(&constr.pcd_name.txt);
    let name_doc = print_comments(name_doc, cmt_tbl, constr.pcd_name.loc, arena);

    let args_doc = if is_spread {
        match &constr.pcd_args {
            ConstructorArguments::Pcstr_tuple(types) if types.len() == 1 => {
                // Spread constructors print the type directly without parens: ...aa
                print_typ_expr(state, &types[0], cmt_tbl, arena)
            }
            _ => print_constructor_arguments(state, &constr.pcd_args, true, true, cmt_tbl, arena),
        }
    } else {
        print_constructor_arguments(state, &constr.pcd_args, false, true, cmt_tbl, arena)
    };

    let res_doc = match &constr.pcd_res {
        Some(typ) => Doc::concat(vec![Doc::text(": "), print_typ_expr(state, typ, cmt_tbl, arena)]),
        None => Doc::nil(),
    };

    // Wrap in group so constructor can be flat even when outer group breaks
    Doc::group(Doc::concat(vec![attrs_doc, name_doc, args_doc, res_doc]))
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
        .map(|field| {
            let doc = print_label_declaration(state, field, cmt_tbl, arena);
            // Wrap with print_comments for field location (like OCaml)
            print_comments(doc, cmt_tbl, field.pld_loc, arena)
        })
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
    // Like OCaml, print comments around the "..." name itself
    if field.pld_name.txt == "..." {
        let spread_doc = Doc::dotdotdot();
        let spread_doc = print_comments(spread_doc, cmt_tbl, field.pld_name.loc, arena);
        let typ_doc = print_typ_expr(state, &field.pld_type, cmt_tbl, arena);
        return Doc::concat(vec![spread_doc, typ_doc]);
    }

    // Pass field name location for proper attribute line breaking
    let attrs_doc = print_attributes_with_loc(
        state,
        &field.pld_attributes,
        Some(field.pld_name.loc),
        cmt_tbl,
        arena,
        false,
    );
    let mutable_doc = if field.pld_mutable == MutableFlag::Mutable {
        Doc::text("mutable ")
    } else {
        Doc::nil()
    };
    let name_doc = print_ident_like(&field.pld_name.txt, false, false);
    let name_doc = print_comments(name_doc, cmt_tbl, field.pld_name.loc, arena);
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

/// Print constructor arguments (tuple or record).
/// Matches OCaml's print_constructor_arguments.
fn print_constructor_arguments(
    state: &PrinterState,
    args: &ConstructorArguments,
    is_dot_dot_dot: bool,
    indent: bool,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    match args {
        ConstructorArguments::Pcstr_tuple(types) if types.is_empty() => Doc::nil(),
        ConstructorArguments::Pcstr_tuple(types) => {
            let type_docs: Vec<Doc> = types
                .iter()
                .map(|t| print_typ_expr(state, t, cmt_tbl, arena))
                .collect();
            let inner = Doc::concat(vec![
                if is_dot_dot_dot { Doc::nil() } else { Doc::lparen() },
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), type_docs),
                ])),
                Doc::trailing_comma(),
                Doc::soft_line(),
                if is_dot_dot_dot { Doc::nil() } else { Doc::rparen() },
            ]);
            let result = Doc::group(if indent { Doc::indent(inner) } else { inner });
            result
        }
        ConstructorArguments::Pcstr_record(fields) => {
            // Manually inline the printRecordDeclaration, gives better layout
            let field_docs: Vec<Doc> = fields
                .iter()
                .map(|ld| {
                    let doc = print_label_declaration(state, ld, cmt_tbl, arena);
                    print_comments(doc, cmt_tbl, ld.pld_loc, arena)
                })
                .collect();
            let inner = Doc::concat(vec![
                Doc::lparen(),
                Doc::lbrace(),
                Doc::indent(Doc::concat(vec![
                    Doc::soft_line(),
                    Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), field_docs),
                ])),
                Doc::trailing_comma(),
                Doc::soft_line(),
                Doc::rbrace(),
                Doc::rparen(),
            ]);
            let result = Doc::group(if indent { Doc::indent(inner) } else { inner });
            result
        }
    }
}

/// Print value description (external or let declaration in signatures).
fn print_value_description(
    state: &PrinterState,
    val_desc: &ValueDescription,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let is_external = !val_desc.pval_prim.is_empty();
    // Use value name location for proper attribute line breaking
    let attrs_doc = print_attributes_with_loc(
        state,
        &val_desc.pval_attributes,
        Some(val_desc.pval_name.loc),
        cmt_tbl,
        arena,
        false,
    );
    let name_doc = print_ident_like(&val_desc.pval_name.txt, false, false);
    let name_doc = print_comments(name_doc, cmt_tbl, val_desc.pval_name.loc, arena);
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
    // Pass path location for proper attribute line breaking:
    // @attr\ntype t += ... should preserve the line break
    let attrs_doc = print_attributes_with_loc(
        state,
        &type_ext.ptyext_attributes,
        Some(type_ext.ptyext_path.loc),
        cmt_tbl,
        arena,
        false,
    );
    let path_doc = print_lident_path(&type_ext.ptyext_path, cmt_tbl, arena);

    let params_doc = if type_ext.ptyext_params.is_empty() {
        Doc::nil()
    } else {
        let params: Vec<Doc> = type_ext
            .ptyext_params
            .iter()
            .map(|(typ, variance)| print_type_param(state, typ, variance, cmt_tbl, arena))
            .collect();
        Doc::group(Doc::concat(vec![
            Doc::less_than(),
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), params),
            ])),
            Doc::trailing_comma(),
            Doc::soft_line(),
            Doc::greater_than(),
        ]))
    };

    let ecs = &type_ext.ptyext_constructors;
    // Determine if we should force a break based on locations
    let force_break = if let (Some(first), Some(last)) = (ecs.first(), ecs.last()) {
        let path_end = arena.loc_end(type_ext.ptyext_path.loc);
        let first_start = arena.loc_start(first.pext_loc);
        let last_end = arena.loc_end(last.pext_loc);
        first_start.line > path_end.line || first_start.line < last_end.line
    } else {
        false
    };

    let private_doc = match type_ext.ptyext_private {
        PrivateFlag::Private => Doc::concat(vec![Doc::text("private"), Doc::line()]),
        PrivateFlag::Public => Doc::nil(),
    };

    let constructors_doc = print_listi(
        |constr: &ExtensionConstructor| constr.pext_loc,
        ecs,
        |constr, cmt_tbl_inner, i| print_extension_constructor(state, constr, i, cmt_tbl_inner, arena),
        cmt_tbl,
        arena,
        false, // ignore_empty_lines
        force_break,
    );

    Doc::group(Doc::concat(vec![
        attrs_doc,
        Doc::text("type "),
        path_doc,
        params_doc,
        Doc::text(" +="),
        Doc::breakable_group(
            Doc::indent(Doc::concat(vec![
                Doc::line(),
                private_doc,
                constructors_doc,
            ])),
            force_break,
        ),
    ]))
}

/// Print extension constructor.
fn print_extension_constructor(
    state: &PrinterState,
    ext_constr: &ExtensionConstructor,
    index: usize,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &ext_constr.pext_attributes, cmt_tbl, arena);
    let name_doc = print_comments(
        Doc::text(&ext_constr.pext_name.txt),
        cmt_tbl,
        ext_constr.pext_name.loc,
        arena,
    );

    // First constructor uses if_breaks, others always show |
    let bar = if index > 0 {
        Doc::text("| ")
    } else {
        Doc::if_breaks(Doc::text("| "), Doc::nil())
    };

    let kind_doc = match &ext_constr.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            let gadt_doc = match res {
                Some(typ) => Doc::concat(vec![Doc::text(": "), print_typ_expr(state, typ, cmt_tbl, arena)]),
                None => Doc::nil(),
            };
            Doc::concat(vec![
                print_constructor_arguments(state, args, false, false, cmt_tbl, arena),
                gadt_doc,
            ])
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            Doc::indent(Doc::concat(vec![
                Doc::text(" ="),
                Doc::line(),
                print_longident_location(&lid, cmt_tbl, arena),
            ]))
        }
    };

    Doc::concat(vec![bar, Doc::group(Doc::concat(vec![attrs_doc, name_doc, kind_doc]))])
}

/// Print exception definition (for exception declarations in signatures and structures).
fn print_exception_def(
    state: &PrinterState,
    ext_constr: &ExtensionConstructor,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &ext_constr.pext_attributes, cmt_tbl, arena);
    let name_doc = {
        let doc = Doc::text(&ext_constr.pext_name.txt);
        print_comments(doc, cmt_tbl, ext_constr.pext_name.loc, arena)
    };

    let kind_doc = match &ext_constr.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            let gadt_doc = match res {
                Some(typ) => Doc::concat(vec![Doc::text(": "), print_typ_expr(state, typ, cmt_tbl, arena)]),
                None => Doc::nil(),
            };
            Doc::concat(vec![
                print_constructor_arguments(state, args, false, false, cmt_tbl, arena),
                gadt_doc,
            ])
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            Doc::indent(Doc::concat(vec![
                Doc::text(" ="),
                Doc::line(),
                print_longident_location(lid, cmt_tbl, arena),
            ]))
        }
    };

    let doc = Doc::group(Doc::concat(vec![
        attrs_doc,
        Doc::text("exception "),
        name_doc,
        kind_doc,
    ]));
    print_comments(doc, cmt_tbl, ext_constr.pext_loc, arena)
}

/// Print module binding.
fn print_module_binding(
    state: &PrinterState,
    binding: &ModuleBinding,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &binding.pmb_attributes, cmt_tbl, arena);

    // Wrap module name with print_comments for its location (like OCaml reference)
    let name_doc = Doc::text(&binding.pmb_name.txt);
    let name_doc = print_comments(name_doc, cmt_tbl, binding.pmb_name.loc, arena);

    // Handle Pmod_constraint specially - print constraint before equals sign (like OCaml)
    // module X: Int = Y  vs  module X = Y: Int
    let (expr_doc, constraint_doc) = match &binding.pmb_expr.pmod_desc {
        ModuleExprDesc::Pmod_constraint(mod_expr, mod_type)
            if !parsetree_viewer::has_await_attribute(&binding.pmb_expr.pmod_attributes) =>
        {
            let expr_doc = print_mod_expr(state, mod_expr, cmt_tbl, arena);
            // Wrap in parens based on the outer binding expr (not inner)
            let expr_doc = if parens::mod_expr_parens(&binding.pmb_expr) {
                Doc::concat(vec![Doc::lparen(), expr_doc, Doc::rparen()])
            } else {
                expr_doc
            };
            let constraint_doc = Doc::concat(vec![
                Doc::text(": "),
                print_module_type(state, mod_type, cmt_tbl, arena),
            ]);
            (expr_doc, constraint_doc)
        }
        _ => {
            let expr_doc = print_mod_expr(state, &binding.pmb_expr, cmt_tbl, arena);
            (expr_doc, Doc::nil())
        }
    };

    let doc = Doc::concat(vec![
        attrs_doc,
        Doc::text("module "),
        name_doc,
        constraint_doc,
        Doc::text(" = "),
        expr_doc,
    ]);

    // Wrap entire result with print_comments for binding location (like OCaml reference)
    print_comments(doc, cmt_tbl, binding.pmb_loc, arena)
}

/// Print recursive module bindings using print_listi for proper comment handling.
fn print_rec_module_bindings(
    state: &PrinterState,
    bindings: &[ModuleBinding],
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    // Use print_listi to properly handle comments between bindings
    print_listi(
        |binding: &ModuleBinding| binding.pmb_loc,
        bindings,
        |binding: &ModuleBinding, cmt_tbl: &mut CommentTable, i: usize| {
            let attrs_doc = print_attributes(state, &binding.pmb_attributes, cmt_tbl, arena);
            let prefix = if i == 0 {
                Doc::text("module rec ")
            } else {
                Doc::text("and ")
            };
            // Wrap module name with print_comments for its location
            let name_doc = Doc::text(&binding.pmb_name.txt);
            let name_doc = print_comments(name_doc, cmt_tbl, binding.pmb_name.loc, arena);

            // Handle Pmod_constraint specially - print constraint before equals sign (like OCaml)
            // module rec X: Int = Y  vs  module rec X = Y: Int
            let (expr_doc, constraint_doc) = match &binding.pmb_expr.pmod_desc {
                ModuleExprDesc::Pmod_constraint(mod_expr, mod_type)
                    if !parsetree_viewer::has_await_attribute(&binding.pmb_expr.pmod_attributes) =>
                {
                    let expr_doc = print_mod_expr(state, mod_expr, cmt_tbl, arena);
                    // Wrap in parens based on the outer binding expr (not inner)
                    let expr_doc = if parens::mod_expr_parens(&binding.pmb_expr) {
                        Doc::concat(vec![Doc::lparen(), expr_doc, Doc::rparen()])
                    } else {
                        expr_doc
                    };
                    let constraint_doc = Doc::concat(vec![
                        Doc::text(": "),
                        print_module_type(state, mod_type, cmt_tbl, arena),
                    ]);
                    (expr_doc, constraint_doc)
                }
                _ => {
                    let expr_doc = print_mod_expr(state, &binding.pmb_expr, cmt_tbl, arena);
                    (expr_doc, Doc::nil())
                }
            };

            Doc::concat(vec![attrs_doc, prefix, name_doc, constraint_doc, Doc::text(" = "), expr_doc])
        },
        cmt_tbl,
        arena,
        false, // don't ignore empty lines
        false, // don't force break
    )
}

/// Print module type declaration.
fn print_module_type_declaration(
    state: &PrinterState,
    decl: &ModuleTypeDeclaration,
    cmt_tbl: &mut CommentTable,
    arena: &ParseArena,
) -> Doc {
    let attrs_doc = print_attributes(state, &decl.pmtd_attributes, cmt_tbl, arena);
    // Wrap module type name with print_comments for its location (like OCaml reference)
    let name_doc = Doc::text(&decl.pmtd_name.txt);
    let name_doc = print_comments(name_doc, cmt_tbl, decl.pmtd_name.loc, arena);
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

    // Special case: include Ident({single type alias})
    // Try to keep on one line if it fits
    let mod_doc = if let ModuleExprDesc::Pmod_apply(_, _) = &include_decl.pincl_mod.pmod_desc {
        let (args, call_expr) = parsetree_viewer::mod_expr_apply(&include_decl.pincl_mod);

        // Check if callee is an ident and single arg is structure with single type alias
        if args.len() == 1 && matches!(&call_expr.pmod_desc, ModuleExprDesc::Pmod_ident(_)) {
            if let ModuleExprDesc::Pmod_structure(items) = &args[0].pmod_desc {
                if items.len() == 1 {
                    if let StructureItemDesc::Pstr_type(_, decls) = &items[0].pstr_desc {
                        if decls.len() == 1
                            && matches!(decls[0].ptype_kind, TypeKind::Ptype_abstract)
                            && decls[0].ptype_manifest.is_some()
                        {
                            // This is the special case - use force_break:false
                            return Doc::concat(vec![
                                attrs_doc,
                                Doc::text("include "),
                                print_mod_expr(state, call_expr, cmt_tbl, arena),
                                Doc::lparen(),
                                Doc::breakable_group(
                                    Doc::concat(vec![
                                        Doc::lbrace(),
                                        Doc::indent(Doc::concat(vec![
                                            Doc::soft_line(),
                                            print_structure_item(state, &items[0], cmt_tbl, arena),
                                        ])),
                                        Doc::soft_line(),
                                        Doc::rbrace(),
                                    ]),
                                    false, // force_break = false
                                ),
                                Doc::rparen(),
                            ]);
                        }
                    }
                }
            }
        }
        // Not the special case, use normal printing
        print_mod_expr(state, &include_decl.pincl_mod, cmt_tbl, arena)
    } else {
        print_mod_expr(state, &include_decl.pincl_mod, cmt_tbl, arena)
    };

    let include_doc = if parens::include_mod_expr(&include_decl.pincl_mod) {
        Doc::concat(vec![Doc::lparen(), mod_doc, Doc::rparen()])
    } else {
        mod_doc
    };

    Doc::concat(vec![attrs_doc, Doc::text("include "), include_doc])
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
    comments: Vec<Comment>,
    width: i32,
    arena: &mut ParseArena,
) -> String {
    let mut cmt_tbl = CommentTable::new();
    walk_signature(signature, &mut cmt_tbl, comments, arena);
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
