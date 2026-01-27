//! ReScript AST printer - Doc-based implementation.
//!
//! This is a 1:1 port of OCaml's `res_printer.ml`, using Doc structures
//! for pretty-printing instead of direct string output.

use crate::location::Location;
use crate::parser::ast::*;
use crate::parser::comment::Comment;
use crate::parser::comment_table::CommentTable;
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
fn print_longident_aux(lid: &Longident, accu: &mut Vec<Doc>) {
    match lid {
        Longident::Lident(s) => {
            accu.push(Doc::text(s.clone()));
        }
        Longident::Ldot(inner, s) => {
            print_longident_aux(inner, accu);
            accu.push(Doc::text(s.clone()));
        }
        Longident::Lapply(lid1, lid2) => {
            let mut docs1 = Vec::new();
            print_longident_aux(lid1, &mut docs1);
            let d1 = Doc::join(Doc::dot(), docs1);

            let mut docs2 = Vec::new();
            print_longident_aux(lid2, &mut docs2);
            let d2 = Doc::join(Doc::dot(), docs2);

            accu.push(Doc::concat(vec![d1, Doc::lparen(), d2, Doc::rparen()]));
        }
    }
}

/// Print a longident without escaping.
pub fn print_longident(lid: &Longident) -> Doc {
    match lid {
        Longident::Lident(txt) => Doc::text(txt.clone()),
        _ => {
            let mut docs = Vec::new();
            print_longident_aux(lid, &mut docs);
            Doc::join(Doc::dot(), docs)
        }
    }
}

/// Print a longident with escaping for exotic identifiers.
pub fn print_lident(lid: &Longident) -> Doc {
    match lid {
        Longident::Lident(txt) => print_ident(txt),
        Longident::Ldot(path, txt) => {
            // Flatten the path
            fn flat_lid(lid: &Longident) -> Option<Vec<String>> {
                match lid {
                    Longident::Lident(txt) => Some(vec![txt.clone()]),
                    Longident::Ldot(inner, txt) => {
                        let mut result = flat_lid(inner)?;
                        result.push(txt.clone());
                        Some(result)
                    }
                    Longident::Lapply(_, _) => None,
                }
            }

            match flat_lid(path) {
                Some(txts) => {
                    let mut docs: Vec<Doc> = txts.into_iter().map(|t| Doc::text(t)).collect();
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
fn print_trailing_comment(prev_loc: &Location, node_loc: &Location, comment: &Comment) -> Doc {
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
    leading: &mut HashMap<Location, Vec<Comment>>,
    loc: &Location,
) -> Doc {
    match leading.remove(loc) {
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
                    loc.loc_start.line as i32 - last_comment.loc().loc_end.line as i32;
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
    trailing: &mut HashMap<Location, Vec<Comment>>,
    loc: &Location,
) -> Doc {
    match trailing.remove(loc) {
        None => node,
        Some(comments) if comments.is_empty() => node,
        Some(comments) => {
            let mut acc = Vec::new();
            let mut prev_loc = loc.clone();

            for comment in &comments {
                acc.push(print_trailing_comment(&prev_loc, loc, comment));
                prev_loc = comment.loc().clone();
            }

            Doc::concat(vec![node, Doc::concat(acc)])
        }
    }
}

/// Print a node with its leading and trailing comments.
pub fn print_comments(doc: Doc, cmt_tbl: &mut CommentTable, loc: &Location) -> Doc {
    let doc_with_leading = print_leading_comments(doc, &mut cmt_tbl.leading, loc);
    print_trailing_comments(doc_with_leading, &mut cmt_tbl.trailing, loc)
}

/// Get the first leading comment for a location, if any.
pub fn get_first_leading_comment<'a>(cmt_tbl: &'a CommentTable, loc: &Location) -> Option<&'a Comment> {
    cmt_tbl.leading.get(loc).and_then(|comments| comments.first())
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
    force_break: bool,
) -> Doc
where
    F: Fn(&'a T) -> &'a Location,
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
        print_comments(doc, cmt_tbl, first_loc)
    };
    docs.push(first_doc);

    let mut prev_loc = first_loc.clone();

    for node in &nodes[1..] {
        let loc = get_loc(node);

        // Determine separator based on line distance
        let start_pos = match get_first_leading_comment(cmt_tbl, loc) {
            Some(comment) => comment.loc().loc_start.clone(),
            None => loc.loc_start.clone(),
        };

        let sep = if start_pos.line as i32 - prev_loc.loc_end.line as i32 > 1 {
            Doc::concat(vec![Doc::hard_line(), Doc::hard_line()])
        } else {
            Doc::hard_line()
        };

        docs.push(sep);

        let doc = print(node, cmt_tbl);
        let doc = print_comments(doc, cmt_tbl, loc);
        docs.push(doc);

        prev_loc = loc.clone();
    }

    // Determine if we need to force break based on span
    let last_loc = get_loc(&nodes[nodes.len() - 1]);
    let should_break = force_break || first_loc.loc_start.line != last_loc.loc_end.line;

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
    ignore_empty_lines: bool,
    force_break: bool,
) -> Doc
where
    F: Fn(&'a T) -> &'a Location,
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
        print_comments(doc, cmt_tbl, first_loc)
    };
    docs.push(first_doc);

    let mut prev_loc = first_loc.clone();

    for (i, node) in nodes[1..].iter().enumerate() {
        let loc = get_loc(node);

        // Determine separator based on line distance
        let start_pos = match get_first_leading_comment(cmt_tbl, loc) {
            Some(comment) => comment.loc().loc_start.clone(),
            None => loc.loc_start.clone(),
        };

        let sep = if start_pos.line as i32 - prev_loc.loc_end.line as i32 > 1 && !ignore_empty_lines
        {
            Doc::concat(vec![Doc::hard_line(), Doc::hard_line()])
        } else {
            Doc::line()
        };

        docs.push(sep);

        let doc = print(node, cmt_tbl, i + 1);
        let doc = print_comments(doc, cmt_tbl, loc);
        docs.push(doc);

        prev_loc = loc.clone();
    }

    // Determine if we need to force break based on span
    let last_loc = get_loc(&nodes[nodes.len() - 1]);
    let should_break = force_break || first_loc.loc_start.line != last_loc.loc_end.line;

    Doc::breakable_group(Doc::concat(docs), should_break)
}

// ============================================================================
// Comments Inside Empty Constructs
// ============================================================================

/// Print comments inside an empty construct (like empty block {}).
pub fn print_comments_inside(cmt_tbl: &mut CommentTable, loc: &Location) -> Doc {
    let force_break = loc.loc_start.line != loc.loc_end.line;

    match cmt_tbl.inside.remove(loc) {
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
pub fn print_comments_inside_file(cmt_tbl: &mut CommentTable) -> Doc {
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
                Doc::concat(vec![Doc::text("#\""), Doc::text(&txt[1..]), Doc::text("\"")])
            } else {
                Doc::concat(vec![Doc::text("#\""), Doc::text(txt), Doc::text("\"")])
            }
        }
        IdentifierStyle::ExoticIdent => {
            Doc::concat(vec![Doc::text("#\""), Doc::text(txt), Doc::text("\"")])
        }
        IdentifierStyle::NormalIdent => Doc::text(txt),
    }
}

// ============================================================================
// String Printing
// ============================================================================

/// Print a string location (identifier with location).
pub fn print_string_loc(sloc: &StringLoc, cmt_tbl: &mut CommentTable) -> Doc {
    let doc = print_ident(&sloc.txt);
    print_comments(doc, cmt_tbl, &sloc.loc)
}

/// Print a longident location.
pub fn print_longident_loc(lid: &Loc<Longident>, cmt_tbl: &mut CommentTable) -> Doc {
    let doc = print_lident(&lid.txt);
    print_comments(doc, cmt_tbl, &lid.loc)
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
