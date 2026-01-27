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
pub fn print_string_loc(sloc: &StringLoc, cmt_tbl: &mut CommentTable) -> Doc {
    let doc = print_ident(&sloc.txt);
    print_comments(doc, cmt_tbl, &sloc.loc)
}

/// Print a longident location.
/// Print a longident location with escaping (for value identifiers).
pub fn print_longident_loc(lid: &Loc<Longident>, cmt_tbl: &mut CommentTable) -> Doc {
    let doc = print_lident(&lid.txt);
    print_comments(doc, cmt_tbl, &lid.loc)
}

/// Print a longident location without escaping (for module paths).
pub fn print_longident_location(lid: &Loc<Longident>, cmt_tbl: &mut CommentTable) -> Doc {
    let doc = print_longident(&lid.txt);
    print_comments(doc, cmt_tbl, &lid.loc)
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
pub fn print_braces(doc: Doc, expr: &Expression, braces_loc: Location) -> Doc {
    let over_multiple_lines =
        braces_loc.loc_start.line != braces_loc.loc_end.line;
    Doc::breakable_group(
        Doc::concat(vec![
            Doc::lbrace(),
            Doc::indent(Doc::concat(vec![Doc::soft_line(), doc])),
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
) -> Doc {
    let doc = print_expression(state, expr, cmt_tbl);
    print_comments(doc, cmt_tbl, &expr.pexp_loc)
}

/// Print an expression.
pub fn print_expression(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
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
                Some(rewritten) => print_expression_with_comments(state, &rewritten, cmt_tbl),
                None => print_expression_with_comments(state, e, cmt_tbl),
            }
        }
        // Function expressions
        ExpressionDesc::Pexp_fun { .. } | ExpressionDesc::Pexp_newtype(_, _) => {
            print_arrow_expression(state, e, cmt_tbl)
        }
        // Constants
        ExpressionDesc::Pexp_constant(c) => {
            print_constant(parsetree_viewer::is_template_literal(e), c)
        }
        // Unit constructor: ()
        ExpressionDesc::Pexp_construct(lid, None)
            if lid.txt == Longident::Lident("()".to_string()) =>
        {
            Doc::text("()")
        }
        // Empty list: []
        ExpressionDesc::Pexp_construct(lid, None)
            if lid.txt == Longident::Lident("[]".to_string()) =>
        {
            Doc::concat(vec![
                Doc::text("list{"),
                print_comments_inside(cmt_tbl, &e.pexp_loc),
                Doc::rbrace(),
            ])
        }
        // List constructor: ::
        ExpressionDesc::Pexp_construct(lid, _)
            if lid.txt == Longident::Lident("::".to_string()) =>
        {
            print_list_expression(state, e, cmt_tbl)
        }
        // Other constructors
        ExpressionDesc::Pexp_construct(longident_loc, args) => {
            let constr = print_longident_location(longident_loc, cmt_tbl);
            let args_doc = match args {
                None => Doc::nil(),
                Some(arg) => match &arg.pexp_desc {
                    ExpressionDesc::Pexp_construct(lid, None)
                        if lid.txt == Longident::Lident("()".to_string()) =>
                    {
                        Doc::text("()")
                    }
                    // Some((1, 2))
                    ExpressionDesc::Pexp_tuple(exprs) if exprs.len() == 1 => {
                        if let ExpressionDesc::Pexp_tuple(_) = &exprs[0].pexp_desc {
                            let doc =
                                print_expression_with_comments(state, &exprs[0], cmt_tbl);
                            let doc = match parens::expr(&exprs[0]) {
                                ParenKind::Parenthesized => add_parens(doc),
                                ParenKind::Braced(loc) => print_braces(doc, &exprs[0], loc),
                                ParenKind::Nothing => doc,
                            };
                            Doc::concat(vec![Doc::lparen(), doc, Doc::rparen()])
                        } else {
                            print_constructor_args(state, args.as_deref(), cmt_tbl)
                        }
                    }
                    ExpressionDesc::Pexp_tuple(exprs) => {
                        print_tuple_args(state, exprs, cmt_tbl)
                    }
                    _ => {
                        let arg_doc = print_expression_with_comments(state, arg, cmt_tbl);
                        let arg_doc = match parens::expr(arg) {
                            ParenKind::Parenthesized => add_parens(arg_doc),
                            ParenKind::Braced(loc) => print_braces(arg_doc, arg, loc),
                            ParenKind::Nothing => arg_doc,
                        };
                        let should_hug = parsetree_viewer::is_huggable_expression(arg);
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
        ExpressionDesc::Pexp_ident(path) => print_lident_path(path, cmt_tbl),
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
                                    print_expression_with_comments(state, expr, cmt_tbl);
                                match parens::expr(expr) {
                                    ParenKind::Parenthesized => add_parens(doc),
                                    ParenKind::Braced(loc) => print_braces(doc, expr, loc),
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
                print_comments_inside(cmt_tbl, &e.pexp_loc),
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
                                    print_expression_with_comments(state, expr, cmt_tbl);
                                match parens::expr(expr) {
                                    ParenKind::Parenthesized => add_parens(doc),
                                    ParenKind::Braced(loc) => print_braces(doc, expr, loc),
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
                        if lid.txt == Longident::Lident("()".to_string()) =>
                    {
                        Doc::text("()")
                    }
                    ExpressionDesc::Pexp_tuple(exprs) => {
                        print_tuple_args(state, exprs, cmt_tbl)
                    }
                    _ => {
                        let arg_doc = print_expression_with_comments(state, arg, cmt_tbl);
                        let arg_doc = match parens::expr(arg) {
                            ParenKind::Parenthesized => add_parens(arg_doc),
                            ParenKind::Braced(loc) => print_braces(arg_doc, arg, loc),
                            ParenKind::Nothing => arg_doc,
                        };
                        let should_hug = parsetree_viewer::is_huggable_expression(arg);
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
            print_record_expression(state, spread.as_ref().map(|e| e.as_ref()), fields, cmt_tbl)
        }
        // Field access: expr.field
        ExpressionDesc::Pexp_field(expr, longident_loc) => {
            let lhs = print_expression_with_comments(state, expr, cmt_tbl);
            let lhs = match parens::field_expr(expr) {
                ParenKind::Parenthesized => add_parens(lhs),
                ParenKind::Braced(loc) => print_braces(lhs, expr, loc),
                ParenKind::Nothing => lhs,
            };
            let field = print_lident(&longident_loc.txt);
            Doc::concat(vec![lhs, Doc::dot(), field])
        }
        // Field set: expr.field = value
        ExpressionDesc::Pexp_setfield(expr, longident_loc, value) => {
            let lhs = {
                let expr_doc = print_expression_with_comments(state, expr, cmt_tbl);
                let expr_doc = match parens::field_expr(expr) {
                    ParenKind::Parenthesized => add_parens(expr_doc),
                    ParenKind::Braced(loc) => print_braces(expr_doc, expr, loc),
                    ParenKind::Nothing => expr_doc,
                };
                let field = print_lident(&longident_loc.txt);
                Doc::concat(vec![expr_doc, Doc::dot(), field])
            };
            let rhs = {
                let doc = print_expression_with_comments(state, value, cmt_tbl);
                match parens::set_field_expr_rhs(value) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, value, loc),
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
            print_ternary_expression(state, e, cmt_tbl)
        }
        ExpressionDesc::Pexp_ifthenelse(_, _, _) => {
            print_if_expression(state, e, cmt_tbl)
        }
        // Sequence: expr1; expr2
        ExpressionDesc::Pexp_sequence(_, _) => {
            print_expression_block(state, true, e, cmt_tbl)
        }
        // Let expression
        ExpressionDesc::Pexp_let(_, _, _) => {
            print_expression_block(state, true, e, cmt_tbl)
        }
        // Array access: Array.get(arr, idx) -> arr[idx]
        ExpressionDesc::Pexp_apply { funct, args, .. }
            if parsetree_viewer::is_array_access(e)
                && !parsetree_viewer::is_rewritten_underscore_apply_sugar(&args[0].1) =>
        {
            let parent_expr = &args[0].1;
            let member_expr = &args[1].1;
            let parent_doc = print_expression_with_comments(state, parent_expr, cmt_tbl);
            let member_doc = print_expression_with_comments(state, member_expr, cmt_tbl);
            Doc::group(Doc::concat(vec![
                print_attributes(state, &e.pexp_attributes, cmt_tbl),
                parent_doc,
                Doc::lbracket(),
                member_doc,
                Doc::rbracket(),
            ]))
        }
        // Array set: Array.set(arr, idx, value) -> arr[idx] = value
        ExpressionDesc::Pexp_apply { args, .. }
            if parsetree_viewer::is_array_set(e) =>
        {
            let parent_expr = &args[0].1;
            let member_expr = &args[1].1;
            let target_expr = &args[2].1;
            let parent_doc = print_expression_with_comments(state, parent_expr, cmt_tbl);
            let member_doc = print_expression_with_comments(state, member_expr, cmt_tbl);
            let target_doc = print_expression_with_comments(state, target_expr, cmt_tbl);
            let should_indent = parsetree_viewer::is_binary_expression(target_expr);
            let rhs_doc = if should_indent {
                Doc::group(Doc::indent(Doc::concat(vec![Doc::line(), target_doc])))
            } else {
                Doc::concat(vec![Doc::space(), target_doc])
            };
            Doc::group(Doc::concat(vec![
                print_attributes(state, &e.pexp_attributes, cmt_tbl),
                parent_doc,
                Doc::lbracket(),
                member_doc,
                Doc::rbracket(),
                Doc::text(" ="),
                rhs_doc,
            ]))
        }
        // String access: String.get(str, idx) -> str[idx]
        ExpressionDesc::Pexp_apply { args, .. }
            if parsetree_viewer::is_string_access(e)
                && !parsetree_viewer::is_rewritten_underscore_apply_sugar(&args[0].1) =>
        {
            let parent_expr = &args[0].1;
            let member_expr = &args[1].1;
            let parent_doc = print_expression_with_comments(state, parent_expr, cmt_tbl);
            let member_doc = print_expression_with_comments(state, member_expr, cmt_tbl);
            Doc::group(Doc::concat(vec![
                print_attributes(state, &e.pexp_attributes, cmt_tbl),
                parent_doc,
                Doc::lbracket(),
                member_doc,
                Doc::rbracket(),
            ]))
        }
        // Send-set: obj#prop = value -> obj["prop"] = value
        ExpressionDesc::Pexp_apply { funct, args, .. }
            if is_send_set_expr(funct, args) =>
        {
            let lhs = &args[0].1;
            let rhs = &args[1].1;
            let rhs_doc = print_expression_with_comments(state, rhs, cmt_tbl);
            let should_indent = !parsetree_viewer::is_braced_expr(rhs)
                && parsetree_viewer::is_binary_expression(rhs);
            let rhs_doc = if should_indent {
                Doc::group(Doc::indent(Doc::concat(vec![Doc::line(), rhs_doc])))
            } else {
                Doc::concat(vec![Doc::space(), rhs_doc])
            };
            let doc = Doc::group(Doc::concat(vec![
                print_expression_with_comments(state, lhs, cmt_tbl),
                Doc::text(" ="),
                rhs_doc,
            ]));
            if e.pexp_attributes.is_empty() {
                doc
            } else {
                Doc::group(Doc::concat(vec![
                    print_attributes(state, &e.pexp_attributes, cmt_tbl),
                    doc,
                ]))
            }
        }
        // Function application
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            print_pexp_apply(state, e, funct, args, cmt_tbl)
        }
        // Constraint with pack: module(M: S)
        ExpressionDesc::Pexp_constraint(inner, typ)
            if matches!(&inner.pexp_desc, ExpressionDesc::Pexp_pack(_))
                && matches!(&typ.ptyp_desc, CoreTypeDesc::Ptyp_package(_)) =>
        {
            if let ExpressionDesc::Pexp_pack(mod_expr) = &inner.pexp_desc {
                if let CoreTypeDesc::Ptyp_package(package_type) = &typ.ptyp_desc {
                    let mod_doc = print_mod_expr(state, mod_expr, cmt_tbl);
                    // Don't print "module(...)" wrapper - we're already inside module()
                    let type_doc = print_comments(
                        print_package_type(state, package_type, false, cmt_tbl),
                        cmt_tbl,
                        &typ.ptyp_loc,
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
                let doc = print_expression_with_comments(state, expr, cmt_tbl);
                match parens::expr(expr) {
                    parens::ParenKind::Parenthesized => add_parens(doc),
                    parens::ParenKind::Braced(braces) => print_braces(doc, expr, braces),
                    parens::ParenKind::Nothing => doc,
                }
            };
            let typ_doc = print_typ_expr(state, typ, cmt_tbl);
            Doc::concat(vec![expr_doc, Doc::text(": "), typ_doc])
        }
        // Coerce: (expr :> typ)
        ExpressionDesc::Pexp_coerce(expr, _, typ) => {
            let expr_doc = print_expression_with_comments(state, expr, cmt_tbl);
            let typ_doc = print_typ_expr(state, typ, cmt_tbl);
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
            print_extension(state, ext, cmt_tbl)
        }
        // Match expression
        ExpressionDesc::Pexp_match(expr, cases) => {
            print_match_expression(state, expr, cases, cmt_tbl)
        }
        // Try expression
        ExpressionDesc::Pexp_try(expr, cases) => {
            print_try_expression(state, expr, cases, cmt_tbl)
        }
        // Assert
        ExpressionDesc::Pexp_assert(expr) => {
            let rhs = print_expression_with_comments(state, expr, cmt_tbl);
            Doc::concat(vec![Doc::text("assert("), rhs, Doc::text(")")])
        }
        // Await
        ExpressionDesc::Pexp_await(expr) => {
            let rhs = print_expression_with_comments(state, expr, cmt_tbl);
            let rhs = match parens::assert_or_await_expr_rhs(true, expr) {
                ParenKind::Parenthesized => add_parens(rhs),
                ParenKind::Braced(loc) => print_braces(rhs, expr, loc),
                ParenKind::Nothing => rhs,
            };
            Doc::concat(vec![Doc::text("await "), rhs])
        }
        // Open expression
        ExpressionDesc::Pexp_open(_, _, _) => {
            print_expression_block(state, true, e, cmt_tbl)
        }
        // Let module
        ExpressionDesc::Pexp_letmodule(_, _, _) => {
            print_expression_block(state, true, e, cmt_tbl)
        }
        // Let exception
        ExpressionDesc::Pexp_letexception(_, _) => {
            print_expression_block(state, true, e, cmt_tbl)
        }
        // While loop
        ExpressionDesc::Pexp_while(cond, body) => {
            let cond_doc = print_expression_with_comments(state, cond, cmt_tbl);
            Doc::concat(vec![
                Doc::text("while "),
                cond_doc,
                Doc::text(" {"),
                Doc::indent(Doc::concat(vec![
                    Doc::hard_line(),
                    print_expression_with_comments(state, body, cmt_tbl),
                ])),
                Doc::hard_line(),
                Doc::text("}"),
            ])
        }
        // For loop
        ExpressionDesc::Pexp_for(pat, start, finish, direction, body) => {
            let dir = match direction {
                DirectionFlag::Upto => " to ",
                DirectionFlag::Downto => " downto ",
            };
            Doc::concat(vec![
                Doc::text("for "),
                print_pattern(state, pat, cmt_tbl),
                Doc::text(" in "),
                print_expression_with_comments(state, start, cmt_tbl),
                Doc::text(dir),
                print_expression_with_comments(state, finish, cmt_tbl),
                Doc::text(" {"),
                Doc::indent(Doc::concat(vec![
                    Doc::hard_line(),
                    print_expression_with_comments(state, body, cmt_tbl),
                ])),
                Doc::hard_line(),
                Doc::text("}"),
            ])
        }
        // Pack: module(M)
        ExpressionDesc::Pexp_pack(mod_expr) => {
            Doc::concat(vec![
                Doc::text("module("),
                print_mod_expr(state, mod_expr, cmt_tbl),
                Doc::text(")"),
            ])
        }
        // JSX element (placeholder - full implementation needed)
        ExpressionDesc::Pexp_jsx_element(_) => {
            Doc::text("<jsx />")  // Simplified placeholder
        }
        // Send (method call): expr#method -> expr["method"]
        ExpressionDesc::Pexp_send(parent_expr, label) => {
            let parent_doc = print_expression_with_comments(state, parent_expr, cmt_tbl);
            // Apply parenthesization if needed (like unary_expr_operand)
            let parent_doc = match parens::unary_expr_operand(parent_expr) {
                parens::ParenKind::Parenthesized => add_parens(parent_doc),
                parens::ParenKind::Braced(braces) => print_braces(parent_doc, parent_expr, braces),
                parens::ParenKind::Nothing => parent_doc,
            };
            let member_doc = print_comments(Doc::text(&label.txt), cmt_tbl, &label.loc);
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
        let attrs_doc = print_attributes(state, attrs, cmt_tbl);
        Doc::concat(vec![attrs_doc, printed_expression])
    }
}

// ============================================================================
// Expression Helper Functions
// ============================================================================

/// Print a longident path.
fn print_lident_path(path: &Loc<Longident>, cmt_tbl: &mut CommentTable) -> Doc {
    let doc = print_lident(&path.txt);
    print_comments(doc, cmt_tbl, &path.loc)
}

/// Print tuple arguments.
fn print_tuple_args(
    state: &PrinterState,
    exprs: &[Expression],
    cmt_tbl: &mut CommentTable,
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
                        let doc = print_expression_with_comments(state, expr, cmt_tbl);
                        match parens::expr(expr) {
                            ParenKind::Parenthesized => add_parens(doc),
                            ParenKind::Braced(loc) => print_braces(doc, expr, loc),
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
) -> Doc {
    match arg {
        None => Doc::nil(),
        Some(arg) => {
            let arg_doc = print_expression_with_comments(state, arg, cmt_tbl);
            let arg_doc = match parens::expr(arg) {
                ParenKind::Parenthesized => add_parens(arg_doc),
                ParenKind::Braced(loc) => print_braces(arg_doc, arg, loc),
                ParenKind::Nothing => arg_doc,
            };
            let should_hug = parsetree_viewer::is_huggable_expression(arg);
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
) -> Doc {
    let (expressions, spread) = parsetree_viewer::collect_list_expressions(e);
    let spread_doc = match spread {
        Some(expr) => Doc::concat(vec![
            Doc::text(","),
            Doc::line(),
            Doc::dotdotdot(),
            {
                let doc = print_expression_with_comments(state, expr, cmt_tbl);
                match parens::expr(expr) {
                    ParenKind::Parenthesized => add_parens(doc),
                    ParenKind::Braced(loc) => print_braces(doc, expr, loc),
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
                        let doc = print_expression_with_comments(state, expr, cmt_tbl);
                        match parens::expr(expr) {
                            ParenKind::Parenthesized => add_parens(doc),
                            ParenKind::Braced(loc) => print_braces(doc, *expr, loc),
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
            let doc = print_expression_with_comments(state, return_expr, cmt_tbl);
            match parens::expr(return_expr) {
                ParenKind::Parenthesized => add_parens(doc),
                ParenKind::Braced(loc) => print_braces(doc, return_expr, loc),
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
            let typ_doc = print_typ_expr(state, typ, cmt_tbl);
            let typ_doc = if parens::arrow_return_typ_expr(typ) {
                add_parens(typ_doc)
            } else {
                typ_doc
            };
            Doc::concat(vec![Doc::text(": "), typ_doc])
        }
        None => Doc::nil(),
    };

    let attrs = print_attributes(state, attrs_on_arrow, cmt_tbl);

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
            let doc = print_comments(doc, cmt_tbl, &pat.ppat_loc);
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
                return print_comments(doc, cmt_tbl, &string_loc.loc);
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
                    if lid.txt == Longident::Lident("()".to_string())
            ) =>
        {
            let doc = Doc::text("()");
            let doc = if is_async { add_async(doc) } else { doc };
            return print_comments(doc, cmt_tbl, &pat.ppat_loc);
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
                .map(|p| print_exp_fun_parameter(state, p, cmt_tbl))
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
) -> Doc {
    use parsetree_viewer::FunParam;
    match param {
        FunParam::NewType { attrs, name } => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl);
            Doc::group(Doc::concat(vec![
                attrs_doc,
                Doc::text("type "),
                print_comments(print_ident_like(&name.txt, false, false), cmt_tbl, &name.loc),
            ]))
        }
        FunParam::Parameter {
            attrs,
            label,
            default_expr,
            pat,
        } => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl);
            let default_doc = match default_expr {
                Some(expr) => Doc::concat(vec![
                    Doc::text("="),
                    print_expression_with_comments(state, expr, cmt_tbl),
                ]),
                None => Doc::nil(),
            };

            let label_with_pattern = match (label, &pat.ppat_desc) {
                (ArgLabel::Nolabel, _) => print_pattern(state, pat, cmt_tbl),
                // ~d (punning)
                (ArgLabel::Labelled(lbl) | ArgLabel::Optional(lbl), PatternDesc::Ppat_var(var))
                    if lbl.txt == var.txt =>
                {
                    Doc::concat(vec![
                        print_attributes(state, &pat.ppat_attributes, cmt_tbl),
                        Doc::text("~"),
                        print_ident_like(&lbl.txt, false, false),
                    ])
                }
                // ~d: typ (punning with type)
                (
                    ArgLabel::Labelled(lbl) | ArgLabel::Optional(lbl),
                    PatternDesc::Ppat_constraint(inner, typ),
                ) if matches!(&inner.ppat_desc, PatternDesc::Ppat_var(v) if lbl.txt == v.txt) => {
                    Doc::concat(vec![
                        print_attributes(state, &pat.ppat_attributes, cmt_tbl),
                        Doc::text("~"),
                        print_ident_like(&lbl.txt, false, false),
                        Doc::text(": "),
                        print_typ_expr(state, typ, cmt_tbl),
                    ])
                }
                // ~d as x or ~d=pat
                (ArgLabel::Labelled(lbl), _) => Doc::concat(vec![
                    Doc::text("~"),
                    print_ident_like(&lbl.txt, false, false),
                    Doc::text(" as "),
                    print_pattern(state, pat, cmt_tbl),
                ]),
                (ArgLabel::Optional(lbl), _) => Doc::concat(vec![
                    Doc::text("~"),
                    print_ident_like(&lbl.txt, false, false),
                    Doc::text(" as "),
                    print_pattern(state, pat, cmt_tbl),
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

/// Print record expression.
fn print_record_expression(
    state: &PrinterState,
    spread: Option<&Expression>,
    fields: &[ExpressionRecordField],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let spread_doc = match spread {
        Some(expr) => Doc::concat(vec![
            Doc::dotdotdot(),
            print_expression_with_comments(state, expr, cmt_tbl),
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
                let field_name = print_lident(&field.lid.txt);
                let expr_doc = print_expression_with_comments(state, &field.expr, cmt_tbl);
                // Check for punning
                if is_punned_record_field(field) {
                    if field.opt {
                        Doc::concat(vec![field_name, Doc::text("?")])
                    } else {
                        field_name
                    }
                } else {
                    let opt_marker = if field.opt { Doc::text("?") } else { Doc::nil() };
                    Doc::group(Doc::concat(vec![
                        field_name,
                        opt_marker,
                        Doc::text(": "),
                        expr_doc,
                    ]))
                }
            })
            .collect(),
    );
    Doc::group(Doc::concat(vec![
        Doc::lbrace(),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            spread_doc,
            fields_doc,
        ])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::rbrace(),
    ]))
}

/// Check if a record field is punned.
fn is_punned_record_field(field: &ExpressionRecordField) -> bool {
    match (&field.lid.txt, &field.expr.pexp_desc) {
        (Longident::Lident(name), ExpressionDesc::Pexp_ident(path)) => {
            if let Longident::Lident(ident) = &path.txt {
                name == ident
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Check if expression is a send-set: `#=(lhs, rhs)` where lhs is a Pexp_send.
fn is_send_set_expr(funct: &Expression, args: &[(ArgLabel, Expression)]) -> bool {
    if args.len() != 2 {
        return false;
    }
    match &funct.pexp_desc {
        ExpressionDesc::Pexp_ident(lid) => {
            matches!(&lid.txt, Longident::Lident(op) if op == "#=")
        }
        _ => false,
    }
}

/// Print a ternary expression (cond ? then : else).
fn print_ternary_expression(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
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
                        print_ternary_operand(state, condition, cmt_tbl),
                        Doc::line(),
                        Doc::text("? "),
                        print_ternary_operand(state, consequent, cmt_tbl),
                    ])
                })
                .collect::<Vec<_>>();

            Doc::group(Doc::concat(vec![
                print_ternary_operand(state, condition1, cmt_tbl),
                Doc::indent(Doc::concat(vec![
                    Doc::line(),
                    Doc::indent(Doc::concat(vec![
                        Doc::text("? "),
                        print_ternary_operand(state, consequent1, cmt_tbl),
                    ])),
                    Doc::concat(rest_doc),
                    Doc::line(),
                    Doc::text(": "),
                    Doc::indent(print_ternary_operand(state, alternate, cmt_tbl)),
                ])),
            ]))
        }
        _ => Doc::nil(),
    };

    let attrs = parsetree_viewer::filter_ternary_attributes(&e.pexp_attributes);
    // Check if any of the filtered attrs are printable (non-parsing attrs)
    let needs_parens = attrs.iter().any(|attr| parsetree_viewer::is_printable_attribute(attr));

    Doc::concat(vec![
        print_attributes_from_refs(state, &attrs, cmt_tbl),
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
) -> Doc {
    let doc = print_expression_with_comments(state, expr, cmt_tbl);

    // Check for braces attribute first
    if let Some(attr) = parsetree_viewer::process_braces_attr(expr) {
        return print_braces(doc, expr, attr.0.loc.clone());
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

/// Print if expression.
fn print_if_expression(
    state: &PrinterState,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    match &e.pexp_desc {
        ExpressionDesc::Pexp_ifthenelse(cond, then_expr, else_expr) => {
            let cond_doc = print_expression_with_comments(state, cond, cmt_tbl);
            let then_doc = print_expression_block(state, true, then_expr, cmt_tbl);
            let else_doc = match else_expr {
                Some(expr) => Doc::concat(vec![
                    Doc::text(" else "),
                    print_expression_block(state, true, expr, cmt_tbl),
                ]),
                None => Doc::nil(),
            };
            Doc::concat(vec![
                Doc::text("if "),
                cond_doc,
                Doc::text(" "),
                then_doc,
                else_doc,
            ])
        }
        _ => Doc::nil(),
    }
}

/// Print expression block (with braces).
fn print_expression_block(
    state: &PrinterState,
    braces: bool,
    e: &Expression,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    match &e.pexp_desc {
        ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
            let rec_doc = match rec_flag {
                RecFlag::Nonrecursive => Doc::nil(),
                RecFlag::Recursive => Doc::text("rec "),
            };
            let bindings_doc = print_value_bindings(state, bindings, cmt_tbl);
            let body_doc = print_expression_block(state, false, body, cmt_tbl);
            if braces {
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::hard_line(),
                        Doc::text("let "),
                        rec_doc,
                        bindings_doc,
                        Doc::hard_line(),
                        body_doc,
                    ])),
                    Doc::hard_line(),
                    Doc::rbrace(),
                ])
            } else {
                Doc::concat(vec![
                    Doc::text("let "),
                    rec_doc,
                    bindings_doc,
                    Doc::hard_line(),
                    body_doc,
                ])
            }
        }
        ExpressionDesc::Pexp_sequence(e1, e2) => {
            let e1_doc = print_expression_with_comments(state, e1, cmt_tbl);
            let e2_doc = print_expression_block(state, false, e2, cmt_tbl);
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
            let mod_doc = print_mod_expr(state, mod_expr, cmt_tbl);
            let body_doc = print_expression_block(state, false, body, cmt_tbl);
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
            let ext_doc = print_extension_constructor(state, ext_constr, cmt_tbl);
            let body_doc = print_expression_block(state, false, body, cmt_tbl);
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
            let body_doc = print_expression_block(state, false, body, cmt_tbl);
            if braces {
                Doc::concat(vec![
                    Doc::lbrace(),
                    Doc::indent(Doc::concat(vec![
                        Doc::hard_line(),
                        Doc::text("open"),
                        override_doc,
                        Doc::text(" "),
                        print_longident(&lid.txt),
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
                    print_longident(&lid.txt),
                    Doc::hard_line(),
                    body_doc,
                ])
            }
        }
        _ => {
            let doc = print_expression_with_comments(state, e, cmt_tbl);
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
    _expr: &Expression,
    funct: &Expression,
    args: &[(ArgLabel, Expression)],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    // Check for binary expression
    if args.len() == 2 {
        if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
            if let Longident::Lident(op) = &ident.txt {
                if parsetree_viewer::is_binary_operator_str(op) {
                    return print_binary_expression(state, op, args, cmt_tbl);
                }
            }
        }
    }

    // Check for unary expression
    if args.len() == 1 {
        if let ExpressionDesc::Pexp_ident(ident) = &funct.pexp_desc {
            if let Longident::Lident(op) = &ident.txt {
                if parsetree_viewer::is_unary_operator_str(op) {
                    return print_unary_expression(state, op, args, cmt_tbl);
                }
            }
        }
    }

    // Regular function application
    let funct_doc = print_expression_with_comments(state, funct, cmt_tbl);
    let funct_doc = match parens::call_expr(funct) {
        ParenKind::Parenthesized => add_parens(funct_doc),
        ParenKind::Braced(loc) => print_braces(funct_doc, funct, loc),
        ParenKind::Nothing => funct_doc,
    };
    let args_doc = print_arguments(state, args, cmt_tbl);
    Doc::group(Doc::concat(vec![funct_doc, args_doc]))
}

/// Print binary expression.
fn print_binary_expression(
    state: &PrinterState,
    op: &str,
    args: &[(ArgLabel, Expression)],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    if args.len() != 2 {
        return Doc::nil();
    }
    let (_, lhs) = &args[0];
    let (_, rhs) = &args[1];

    // Print operands with appropriate parenthesization
    let lhs_doc = print_expression_with_comments(state, lhs, cmt_tbl);
    let lhs_doc = match parens::binary_expr_operand(true, lhs) {
        ParenKind::Parenthesized => add_parens(lhs_doc),
        ParenKind::Braced(loc) => print_braces(lhs_doc, lhs, loc),
        ParenKind::Nothing => lhs_doc,
    };

    let rhs_doc = print_expression_with_comments(state, rhs, cmt_tbl);
    let rhs_doc = match parens::binary_expr_operand(false, rhs) {
        ParenKind::Parenthesized => add_parens(rhs_doc),
        ParenKind::Braced(loc) => print_braces(rhs_doc, rhs, loc),
        ParenKind::Nothing => rhs_doc,
    };

    // Determine if the rhs should be on a new line
    let should_indent = parens::flatten_operand_rhs(op, rhs);

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
) -> Doc {
    if args.is_empty() {
        return Doc::nil();
    }
    let (_, operand) = &args[0];
    let operand_doc = print_expression_with_comments(state, operand, cmt_tbl);
    let operand_doc = match parens::unary_expr_operand(operand) {
        ParenKind::Parenthesized => add_parens(operand_doc),
        ParenKind::Braced(loc) => print_braces(operand_doc, operand, loc),
        ParenKind::Nothing => operand_doc,
    };

    // Convert internal operator names
    let printed_op = match op {
        "~-" | "~-." => "-",
        "~+" | "~+." => "+",
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
    cmt_tbl: &mut CommentTable,
) -> Doc {
    if args.is_empty() {
        return Doc::text("()");
    }

    // Special case: single unit argument () -> just print ()
    if let [(ArgLabel::Nolabel, expr)] = args {
        if is_unit_expr(expr) {
            // Check for leading comments
            if has_leading_line_comment(cmt_tbl, &expr.pexp_loc) {
                let cmt = print_comments(Doc::nil(), cmt_tbl, &expr.pexp_loc);
                return Doc::concat(vec![
                    Doc::lparen(),
                    Doc::indent(Doc::group(Doc::concat(vec![Doc::soft_line(), cmt]))),
                    Doc::rparen(),
                ]);
            }
            return Doc::text("()");
        }
    }

    let args_doc: Vec<Doc> = args
        .iter()
        .map(|(label, expr)| {
            let expr_doc = print_expression_with_comments(state, expr, cmt_tbl);
            match label {
                ArgLabel::Nolabel => expr_doc,
                ArgLabel::Labelled(name) => {
                    // Check for punning: ~name where expr is Pexp_ident(Lident(name))
                    if let ExpressionDesc::Pexp_ident(lid) = &expr.pexp_desc {
                        if let Longident::Lident(ident) = &lid.txt {
                            if ident == &name.txt && expr.pexp_attributes.is_empty() {
                                return Doc::concat(vec![Doc::text("~"), print_ident_like(&name.txt, false, false)]);
                            }
                        }
                    }
                    Doc::concat(vec![Doc::text("~"), print_ident_like(&name.txt, false, false), Doc::text("="), expr_doc])
                }
                ArgLabel::Optional(name) => {
                    // Check for punning: ~name=? where expr is Pexp_ident(Lident(name))
                    if let ExpressionDesc::Pexp_ident(lid) = &expr.pexp_desc {
                        if let Longident::Lident(ident) = &lid.txt {
                            if ident == &name.txt && expr.pexp_attributes.is_empty() {
                                return Doc::concat(vec![Doc::text("~"), print_ident_like(&name.txt, false, false), Doc::text("?")]);
                            }
                        }
                    }
                    Doc::concat(vec![Doc::text("~"), print_ident_like(&name.txt, false, false), Doc::text("=?"), expr_doc])
                }
            }
        })
        .collect();

    Doc::group(Doc::concat(vec![
        Doc::lparen(),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), args_doc),
        ])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::rparen(),
    ]))
}

/// Check if an expression is unit: `()`
fn is_unit_expr(expr: &Expression) -> bool {
    match &expr.pexp_desc {
        ExpressionDesc::Pexp_construct(lid, None) => {
            matches!(&lid.txt, Longident::Lident(s) if s == "()")
        }
        _ => false,
    }
}

/// Check if there's a leading line comment at a location.
fn has_leading_line_comment(cmt_tbl: &CommentTable, loc: &Location) -> bool {
    if let Some(comments) = cmt_tbl.leading.get(loc) {
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
) -> Doc {
    let doc = print_pattern(state, pat, cmt_tbl);
    print_comments(doc, cmt_tbl, &pat.ppat_loc)
}

/// Print a pattern.
pub fn print_pattern(
    state: &PrinterState,
    pat: &Pattern,
    cmt_tbl: &mut CommentTable,
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
                            .map(|p| print_pattern(state, p, cmt_tbl))
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
                print_comments_inside(cmt_tbl, &pat.ppat_loc),
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
                            .map(|p| print_pattern(state, p, cmt_tbl))
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
            if lid.txt == Longident::Lident("()".to_string()) =>
        {
            Doc::concat(vec![
                Doc::lparen(),
                print_comments_inside(cmt_tbl, &pat.ppat_loc),
                Doc::rparen(),
            ])
        }
        // list{}
        PatternDesc::Ppat_construct(lid, None)
            if lid.txt == Longident::Lident("[]".to_string()) =>
        {
            Doc::concat(vec![
                Doc::text("list{"),
                print_comments_inside(cmt_tbl, &pat.ppat_loc),
                Doc::rbrace(),
            ])
        }
        // list{p1, p2, ...spread}
        PatternDesc::Ppat_construct(lid, _)
            if lid.txt == Longident::Lident("::".to_string()) =>
        {
            print_list_pattern(state, pat, cmt_tbl)
        }
        // Constructor(args)
        PatternDesc::Ppat_construct(constr_name, constructor_args) => {
            let constr = print_longident_location(constr_name, cmt_tbl);
            let args_doc = match constructor_args {
                None => Doc::nil(),
                Some(arg) => match &arg.ppat_desc {
                    PatternDesc::Ppat_construct(lid, None)
                        if lid.txt == Longident::Lident("()".to_string()) =>
                    {
                        Doc::concat(vec![
                            Doc::lparen(),
                            print_comments_inside(cmt_tbl, &arg.ppat_loc),
                            Doc::rparen(),
                        ])
                    }
                    // Some((1, 2))
                    PatternDesc::Ppat_tuple(pats) if pats.len() == 1 => {
                        if let PatternDesc::Ppat_tuple(_) = &pats[0].ppat_desc {
                            Doc::concat(vec![
                                Doc::lparen(),
                                print_pattern(state, &pats[0], cmt_tbl),
                                Doc::rparen(),
                            ])
                        } else {
                            print_pattern_constructor_args(state, Some(arg), cmt_tbl)
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
                                        .map(|p| print_pattern(state, p, cmt_tbl))
                                        .collect(),
                                ),
                            ])),
                            Doc::trailing_comma(),
                            Doc::soft_line(),
                            Doc::rparen(),
                        ])
                    }
                    _ => {
                        let arg_doc = print_pattern(state, arg, cmt_tbl);
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
                    if lid.txt == Longident::Lident("()".to_string()) =>
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
                                    .map(|p| print_pattern(state, p, cmt_tbl))
                                    .collect(),
                            ),
                        ])),
                        Doc::trailing_comma(),
                        Doc::soft_line(),
                        Doc::rparen(),
                    ])
                }
                _ => {
                    let arg_doc = print_pattern(state, arg, cmt_tbl);
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
            print_record_pattern(state, fields, &closed, cmt_tbl)
        }
        // p | p
        PatternDesc::Ppat_or(p1, p2) => {
            let p1_doc = print_pattern(state, p1, cmt_tbl);
            let p2_doc = print_pattern(state, p2, cmt_tbl);
            Doc::group(Doc::concat(vec![p1_doc, Doc::text(" | "), p2_doc]))
        }
        // p : type
        PatternDesc::Ppat_constraint(pat, typ) => {
            let pat_doc = print_pattern(state, pat, cmt_tbl);
            let typ_doc = print_typ_expr(state, typ, cmt_tbl);
            Doc::concat(vec![pat_doc, Doc::text(": "), typ_doc])
        }
        // p as x
        PatternDesc::Ppat_alias(pat, alias) => {
            let pat_doc = print_pattern(state, pat, cmt_tbl);
            Doc::concat(vec![pat_doc, Doc::text(" as "), Doc::text(&alias.txt)])
        }
        // module(M)
        PatternDesc::Ppat_unpack(name) => {
            Doc::concat(vec![Doc::text("module("), Doc::text(&name.txt), Doc::text(")")])
        }
        // exception pat
        PatternDesc::Ppat_exception(pat) => {
            Doc::concat(vec![Doc::text("exception "), print_pattern(state, pat, cmt_tbl)])
        }
        // %extension
        PatternDesc::Ppat_extension(ext) => {
            Doc::concat(vec![Doc::text("%"), Doc::text(&ext.0.txt)])
        }
        // `type identifier
        PatternDesc::Ppat_type(lid) => {
            Doc::concat(vec![Doc::text("#..."), print_longident(&lid.txt)])
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
                print_longident(&lid.txt),
                Doc::text("."),
                Doc::lparen(),
                print_pattern(state, pat, cmt_tbl),
                Doc::rparen(),
            ])
        }
    };

    // Handle attributes
    if pat.ppat_attributes.is_empty() {
        pattern_without_attrs
    } else {
        let attrs = print_attributes(state, &pat.ppat_attributes, cmt_tbl);
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
) -> Doc {
    match arg {
        None => Doc::nil(),
        Some(p) => {
            let doc = print_pattern(state, p, cmt_tbl);
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
) -> Doc {
    let (patterns, tail) = parsetree_viewer::collect_list_patterns(pat);

    let should_hug = matches!((&patterns[..], tail),
        ([single], Some(t)) if is_huggable_pattern(single)
            && matches!(&t.ppat_desc, PatternDesc::Ppat_construct(lid, None)
                if lid.txt == Longident::Lident("[]".to_string())));

    let tail_doc = match tail {
        Some(t) => match &t.ppat_desc {
            PatternDesc::Ppat_construct(lid, None)
                if lid.txt == Longident::Lident("[]".to_string()) =>
            {
                Doc::nil()
            }
            _ => {
                Doc::concat(vec![
                    Doc::text(","),
                    Doc::line(),
                    Doc::text("..."),
                    print_pattern(state, t, cmt_tbl),
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
                .map(|p| print_pattern(state, *p, cmt_tbl))
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
) -> Doc {
    let fields_doc = Doc::join(
        Doc::concat(vec![Doc::text(","), Doc::line()]),
        fields
            .iter()
            .map(|field| {
                let label = print_lident(&field.lid.txt);
                // Check for punning
                if is_punned_pattern_field(field) {
                    if field.opt {
                        Doc::concat(vec![label, Doc::text("?")])
                    } else {
                        label
                    }
                } else {
                    let pat_doc = print_pattern(state, &field.pat, cmt_tbl);
                    if field.opt {
                        Doc::concat(vec![label, Doc::text(": "), pat_doc, Doc::text("?")])
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
fn is_punned_pattern_field(field: &PatternRecordField) -> bool {
    match (&field.lid.txt, &field.pat.ppat_desc) {
        (Longident::Lident(name), PatternDesc::Ppat_var(var)) => name == &var.txt,
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
) -> Doc {
    let rendered_type = match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => Doc::text("_"),

        CoreTypeDesc::Ptyp_var(name) => {
            Doc::concat(vec![Doc::text("'"), print_ident_like(name, true, false)])
        }

        CoreTypeDesc::Ptyp_extension(ext) => print_extension(state, ext, cmt_tbl),

        CoreTypeDesc::Ptyp_alias(inner_typ, alias) => {
            let needs_parens = matches!(&inner_typ.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. });
            let typ_doc = print_typ_expr(state, inner_typ, cmt_tbl);
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
            print_object(state, fields, open_flag, false, cmt_tbl)
        }

        CoreTypeDesc::Ptyp_arrow { arity, .. } => {
            print_arrow_type(state, typ, arity.clone(), cmt_tbl)
        }

        CoreTypeDesc::Ptyp_constr(lid, args) => {
            // Handle special case: object type inside type constructor
            if let [single_arg] = args.as_slice() {
                if let CoreTypeDesc::Ptyp_object(fields, open_flag) = &single_arg.ptyp_desc {
                    let constr_name = print_lident_path(lid, cmt_tbl);
                    return Doc::concat(vec![
                        constr_name,
                        Doc::less_than(),
                        print_object(state, fields, open_flag, true, cmt_tbl),
                        Doc::greater_than(),
                    ]);
                }
                // Handle tuple inside constructor
                if let CoreTypeDesc::Ptyp_tuple(types) = &single_arg.ptyp_desc {
                    let constr_name = print_lident_path(lid, cmt_tbl);
                    return Doc::group(Doc::concat(vec![
                        constr_name,
                        Doc::less_than(),
                        print_tuple_type(state, types, true, cmt_tbl),
                        Doc::greater_than(),
                    ]));
                }
            }

            let constr_name = print_lident_path(lid, cmt_tbl);
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
                                .map(|arg| print_typ_expr(state, arg, cmt_tbl))
                                .collect(),
                        ),
                    ])),
                    Doc::trailing_comma(),
                    Doc::soft_line(),
                    Doc::greater_than(),
                ]))
            }
        }

        CoreTypeDesc::Ptyp_tuple(types) => print_tuple_type(state, types, false, cmt_tbl),

        CoreTypeDesc::Ptyp_poly(vars, inner_typ) => {
            if vars.is_empty() {
                print_typ_expr(state, inner_typ, cmt_tbl)
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
                    print_typ_expr(state, inner_typ, cmt_tbl),
                ])
            }
        }

        CoreTypeDesc::Ptyp_package(package_type) => {
            print_package_type(state, package_type, true, cmt_tbl)
        }

        CoreTypeDesc::Ptyp_variant(row_fields, closed_flag, labels_opt) => {
            print_variant_type(state, row_fields, closed_flag, labels_opt, typ, cmt_tbl)
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
            print_doc_comments(state, &doc_comment_attrs, cmt_tbl)
        };
        let attrs_doc = if other_attrs.is_empty() {
            Doc::nil()
        } else {
            print_attributes_from_refs(state, &other_attrs, cmt_tbl)
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
) -> Doc {
    let max_arity = match &arity {
        Arity::Full(n) => Some(*n),
        Arity::Unknown => None,
    };

    let (attrs_before, args, return_type) = parsetree_viewer::arrow_type(typ, max_arity);

    let return_type_needs_parens =
        matches!(return_type.ptyp_desc, CoreTypeDesc::Ptyp_alias(_, _));

    let return_doc = {
        let doc = print_typ_expr(state, return_type, cmt_tbl);
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
                print_attributes_inline(state, attrs_before, cmt_tbl)
            } else {
                Doc::nil()
            };
            let typ_doc = {
                let doc = print_typ_expr(state, arg.typ, cmt_tbl);
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
    let attrs = print_attributes_inline(state, attrs_before, cmt_tbl);
    let rendered_args = Doc::concat(vec![
        attrs,
        Doc::text("("),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(
                Doc::concat(vec![Doc::text(","), Doc::line()]),
                args.iter()
                    .map(|param| print_type_parameter(state, param, cmt_tbl))
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
) -> Doc {
    let attrs = print_attributes(state, param.attrs, cmt_tbl);
    let label_doc = match param.lbl {
        ArgLabel::Nolabel => Doc::nil(),
        ArgLabel::Labelled(name) => Doc::concat(vec![Doc::text("~"), print_ident_like(&name.txt, false, false), Doc::text(": ")]),
        ArgLabel::Optional(name) => Doc::concat(vec![Doc::text("~"), print_ident_like(&name.txt, false, false), Doc::text(": ")]),
    };
    let optional_indicator = match param.lbl {
        ArgLabel::Optional(_) => Doc::text("=?"),
        _ => Doc::nil(),
    };
    let typ_doc = print_typ_expr(state, param.typ, cmt_tbl);

    Doc::group(Doc::concat(vec![attrs, label_doc, typ_doc, optional_indicator]))
}

/// Print tuple type.
fn print_tuple_type(
    state: &PrinterState,
    types: &[CoreType],
    inline: bool,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let tuple = Doc::concat(vec![
        Doc::lparen(),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(
                Doc::concat(vec![Doc::text(","), Doc::line()]),
                types
                    .iter()
                    .map(|t| print_typ_expr(state, t, cmt_tbl))
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
            .map(|field| print_object_field(state, field, cmt_tbl))
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
) -> Doc {
    match field {
        ObjectField::Otag(label, attrs, typ) => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl);
            let label_doc = Doc::text(format!("\"{}\"", label.txt));
            let typ_doc = print_typ_expr(state, typ, cmt_tbl);
            Doc::concat(vec![attrs_doc, label_doc, Doc::text(": "), typ_doc])
        }
        ObjectField::Oinherit(typ) => {
            Doc::concat(vec![Doc::text("..."), print_typ_expr(state, typ, cmt_tbl)])
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
) -> Doc {
    let force_break = typ.ptyp_loc.loc_start.line < typ.ptyp_loc.loc_end.line;

    let docs: Vec<Doc> = row_fields
        .iter()
        .enumerate()
        .map(|(i, field)| print_row_field(state, i, field, cmt_tbl))
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
) -> Doc {
    match field {
        RowField::Rtag(label, attrs, has_empty_payload, types) => {
            let (doc_attrs, other_attrs) =
                parsetree_viewer::partition_doc_comment_attributes(attrs);
            let comment_doc = if doc_attrs.is_empty() {
                Doc::nil()
            } else {
                print_doc_comments(state, &doc_attrs, cmt_tbl)
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
                            print_typ_expr(state, t, cmt_tbl)
                        } else {
                            Doc::concat(vec![
                                Doc::lparen(),
                                print_typ_expr(state, t, cmt_tbl),
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

            let attrs_doc = print_attributes_from_refs(state, &other_attrs, cmt_tbl);
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
            Doc::concat(vec![bar, print_typ_expr(state, typ, cmt_tbl)])
        }
    }
}


/// Print a package type.
fn print_package_type(
    state: &PrinterState,
    package_type: &PackageType,
    print_module_keyword: bool,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let (lid, constraints) = package_type;
    let lid_doc = print_longident(&lid.txt);

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
                    print_longident(&name.txt),
                    Doc::text(" = "),
                    print_typ_expr(state, typ, cmt_tbl),
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
) -> Doc {
    if attrs.is_empty() {
        return Doc::nil();
    }
    let attrs_doc: Vec<Doc> = attrs
        .iter()
        .map(|attr| print_attribute(state, attr, cmt_tbl))
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
) -> Doc {
    if attrs.is_empty() {
        return Doc::nil();
    }
    let attrs_doc: Vec<Doc> = attrs
        .iter()
        .map(|attr| print_attribute(state, attr, cmt_tbl))
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
) -> Doc {
    let doc = match &mod_expr.pmod_desc {
        ModuleExprDesc::Pmod_ident(lid) => {
            print_longident_location(lid, cmt_tbl)
        }

        ModuleExprDesc::Pmod_structure(structure) if structure.is_empty() => {
            let should_break = mod_expr.pmod_loc.loc_start.line < mod_expr.pmod_loc.loc_end.line;
            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::lbrace(),
                    print_comments_inside(cmt_tbl, &mod_expr.pmod_loc),
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
                        print_structure(state, structure, cmt_tbl),
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
                            let package_doc = print_package_type(state, package_type, false, cmt_tbl);
                            let package_doc = print_comments(package_doc, cmt_tbl, &typ.ptyp_loc);
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
                print_expression_with_comments(state, expr_to_print, cmt_tbl),
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
            print_extension_at_module_level(state, ext, cmt_tbl, false)
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
                print_mod_expr(state, call_expr, cmt_tbl),
                if is_unit_sugar {
                    print_mod_apply_arg(state, args[0], cmt_tbl)
                } else {
                    Doc::concat(vec![
                        Doc::lparen(),
                        if should_hug {
                            print_mod_apply_arg(state, args[0], cmt_tbl)
                        } else {
                            Doc::indent(Doc::concat(vec![
                                Doc::soft_line(),
                                Doc::join(
                                    Doc::concat(vec![Doc::comma(), Doc::line()]),
                                    args.iter()
                                        .map(|arg| print_mod_apply_arg(state, arg, cmt_tbl))
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
                print_mod_expr(state, mod_expr_inner, cmt_tbl),
                Doc::text(": "),
                print_module_type(state, mod_type, cmt_tbl),
            ])
        }

        ModuleExprDesc::Pmod_functor(_, _, _) => {
            print_mod_functor(state, mod_expr, cmt_tbl)
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

    print_comments(doc, cmt_tbl, &mod_expr.pmod_loc)
}

/// Print a functor module expression.
fn print_mod_functor(
    state: &PrinterState,
    mod_expr: &ModuleExpr,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let (parameters, return_mod_expr) = parsetree_viewer::mod_expr_functor(mod_expr);

    // Extract return constraint if present
    let (return_constraint, return_body) = match &return_mod_expr.pmod_desc {
        ModuleExprDesc::Pmod_constraint(inner_mod_expr, mod_type) => {
            let constraint_doc = print_module_type(state, mod_type, cmt_tbl);
            let constraint_doc = if parens::mod_expr_functor_constraint(mod_type) {
                add_parens(constraint_doc)
            } else {
                constraint_doc
            };
            let mod_constraint = Doc::concat(vec![Doc::text(": "), constraint_doc]);
            (mod_constraint, print_mod_expr(state, inner_mod_expr, cmt_tbl))
        }
        _ => (Doc::nil(), print_mod_expr(state, return_mod_expr, cmt_tbl)),
    };

    let parameters_doc = match parameters.as_slice() {
        // Unit parameter: `()`
        [param] if param.lbl.txt == "*" && param.mod_type.is_none() => {
            Doc::group(Doc::concat(vec![
                print_attributes(state, param.attrs, cmt_tbl),
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
                            .map(|param| print_mod_functor_param(state, param, cmt_tbl))
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
) -> Doc {
    let cmt_loc = match param.mod_type {
        None => param.lbl.loc.clone(),
        Some(mod_type) => {
            let mut loc = param.lbl.loc.clone();
            loc.loc_end = mod_type.pmty_loc.loc_end.clone();
            loc
        }
    };

    let attrs = print_attributes(state, param.attrs, cmt_tbl);
    let lbl_doc = if param.lbl.txt == "*" {
        Doc::text("()")
    } else {
        Doc::text(&param.lbl.txt)
    };
    let lbl_doc = print_comments(lbl_doc, cmt_tbl, &param.lbl.loc);

    let doc = Doc::group(Doc::concat(vec![
        attrs,
        lbl_doc,
        match param.mod_type {
            None => Doc::nil(),
            Some(mod_type) => Doc::concat(vec![
                Doc::text(": "),
                print_module_type(state, mod_type, cmt_tbl),
            ]),
        },
    ]));

    print_comments(doc, cmt_tbl, &cmt_loc)
}

/// Print a module apply argument.
fn print_mod_apply_arg(
    state: &PrinterState,
    mod_expr: &ModuleExpr,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    match &mod_expr.pmod_desc {
        ModuleExprDesc::Pmod_structure(s) if s.is_empty() => Doc::text("()"),
        _ => print_mod_expr(state, mod_expr, cmt_tbl),
    }
}

/// Print a module type.
fn print_module_type(
    state: &PrinterState,
    mty: &ModuleType,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let doc = match &mty.pmty_desc {
        ModuleTypeDesc::Pmty_ident(lid) => {
            print_longident_location(lid, cmt_tbl)
        }

        ModuleTypeDesc::Pmty_signature(signature) if signature.is_empty() => {
            let should_break = mty.pmty_loc.loc_start.line < mty.pmty_loc.loc_end.line;
            Doc::breakable_group(
                Doc::concat(vec![
                    Doc::lbrace(),
                    print_comments_inside(cmt_tbl, &mty.pmty_loc),
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
                        print_signature(state, signature, cmt_tbl),
                    ])),
                    Doc::soft_line(),
                    Doc::rbrace(),
                ]),
                true,
            )
        }

        ModuleTypeDesc::Pmty_functor(_, _, _) => {
            print_module_type_functor(state, mty, cmt_tbl)
        }

        ModuleTypeDesc::Pmty_with(mod_type, with_constraints) => {
            let base_doc = print_module_type(state, mod_type, cmt_tbl);
            let constraints_doc = Doc::join(
                Doc::line(),
                with_constraints
                    .iter()
                    .enumerate()
                    .map(|(i, constraint)| {
                        let prefix = if i == 0 { "with " } else { "and " };
                        Doc::concat(vec![
                            Doc::text(prefix),
                            print_with_constraint(state, constraint, cmt_tbl),
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
                print_mod_expr(state, mod_expr, cmt_tbl),
            ])
        }

        ModuleTypeDesc::Pmty_extension(ext) => {
            print_extension_at_module_level(state, ext, cmt_tbl, false)
        }

        ModuleTypeDesc::Pmty_alias(lid) => {
            Doc::concat(vec![
                Doc::text("module "),
                print_longident_location(lid, cmt_tbl),
            ])
        }
    };

    let doc = if parsetree_viewer::has_await_attribute(&mty.pmty_attributes) {
        Doc::concat(vec![Doc::text("await "), doc])
    } else {
        doc
    };

    print_comments(doc, cmt_tbl, &mty.pmty_loc)
}

/// Print a functor module type.
fn print_module_type_functor(
    state: &PrinterState,
    mty: &ModuleType,
    cmt_tbl: &mut CommentTable,
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
                                let lbl_doc = print_comments(lbl_doc, cmt_tbl, &lbl.loc);
                                match opt_mty {
                                    None => lbl_doc,
                                    Some(mty) => Doc::group(Doc::concat(vec![
                                        lbl_doc,
                                        Doc::text(": "),
                                        print_module_type(state, mty, cmt_tbl),
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
        print_module_type(state, return_mty, cmt_tbl),
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
) -> Doc {
    match constraint {
        WithConstraint::Pwith_type(lid, type_decl) => {
            Doc::concat(vec![
                Doc::text("type "),
                print_type_declaration_with_lid(state, lid, type_decl, cmt_tbl),
            ])
        }
        WithConstraint::Pwith_module(lid1, lid2) => {
            Doc::concat(vec![
                Doc::text("module "),
                print_longident_location(lid1, cmt_tbl),
                Doc::text(" = "),
                print_longident_location(lid2, cmt_tbl),
            ])
        }
        WithConstraint::Pwith_typesubst(lid, type_decl) => {
            Doc::concat(vec![
                Doc::text("type "),
                print_type_declaration_with_lid(state, lid, type_decl, cmt_tbl),
            ])
        }
        WithConstraint::Pwith_modsubst(lid1, lid2) => {
            Doc::concat(vec![
                Doc::text("module "),
                print_longident_location(lid1, cmt_tbl),
                Doc::text(" := "),
                print_longident_location(lid2, cmt_tbl),
            ])
        }
    }
}

/// Print a type declaration with its longident.
fn print_type_declaration_with_lid(
    state: &PrinterState,
    lid: &Loc<Longident>,
    decl: &TypeDeclaration,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let name_doc = print_lident(&lid.txt);
    let params_doc = if decl.ptype_params.is_empty() {
        Doc::nil()
    } else {
        let params: Vec<Doc> = decl
            .ptype_params
            .iter()
            .map(|(typ, _variance)| print_typ_expr(state, typ, cmt_tbl))
            .collect();
        Doc::concat(vec![
            Doc::less_than(),
            Doc::join(Doc::concat(vec![Doc::text(","), Doc::space()]), params),
            Doc::greater_than(),
        ])
    };

    let manifest_doc = match &decl.ptype_manifest {
        Some(typ) => Doc::concat(vec![Doc::text(" = "), print_typ_expr(state, typ, cmt_tbl)]),
        None => Doc::nil(),
    };

    Doc::concat(vec![name_doc, params_doc, manifest_doc])
}

/// Print extension at module level.
fn print_extension_at_module_level(
    state: &PrinterState,
    ext: &Extension,
    cmt_tbl: &mut CommentTable,
    _at_module_lvl: bool,
) -> Doc {
    let (name, payload) = ext;
    let name_doc = Doc::text(format!("%{}", name.txt));

    let payload_doc = match payload {
        Payload::PStr(items) if items.is_empty() => Doc::nil(),
        Payload::PStr(items) => {
            Doc::concat(vec![
                Doc::text("("),
                print_structure(state, items, cmt_tbl),
                Doc::text(")"),
            ])
        }
        Payload::PTyp(typ) => {
            Doc::concat(vec![
                Doc::text("("),
                print_typ_expr(state, typ, cmt_tbl),
                Doc::text(")"),
            ])
        }
        Payload::PSig(items) => {
            Doc::concat(vec![
                Doc::text("("),
                print_signature(state, items, cmt_tbl),
                Doc::text(")"),
            ])
        }
        Payload::PPat(pat, guard) => {
            let pat_doc = print_pattern(state, pat, cmt_tbl);
            match guard {
                Some(expr) => Doc::concat(vec![
                    Doc::text("("),
                    pat_doc,
                    Doc::text(" when "),
                    print_expression_with_comments(state, expr, cmt_tbl),
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
) -> Doc {
    let scrutinee = print_expression_with_comments(state, expr, cmt_tbl);
    let cases_doc = print_cases(state, cases, cmt_tbl);
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
) -> Doc {
    let body = print_expression_with_comments(state, expr, cmt_tbl);
    let cases_doc = print_cases(state, cases, cmt_tbl);
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
) -> Doc {
    let cases_doc: Vec<Doc> = cases
        .iter()
        .map(|case| {
            let pat = print_pattern(state, &case.pc_lhs, cmt_tbl);
            let pat = if case_pattern_needs_parens(&case.pc_lhs) {
                Doc::concat(vec![Doc::text("("), pat, Doc::text(")")])
            } else {
                pat
            };
            let body = print_expression_with_comments(state, &case.pc_rhs, cmt_tbl);
            Doc::concat(vec![
                Doc::hard_line(),
                Doc::text("| "),
                pat,
                Doc::text(" => "),
                body,
            ])
        })
        .collect();
    Doc::concat(cases_doc)
}

// ============================================================================
// Value Bindings
// ============================================================================

/// Print value bindings.
fn print_value_bindings(
    state: &PrinterState,
    bindings: &[ValueBinding],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let docs: Vec<Doc> = bindings
        .iter()
        .enumerate()
        .map(|(i, vb)| {
            let pat = print_pattern(state, &vb.pvb_pat, cmt_tbl);
            let expr = print_expression_with_comments(state, &vb.pvb_expr, cmt_tbl);
            if i == 0 {
                Doc::concat(vec![pat, Doc::text(" = "), expr])
            } else {
                Doc::concat(vec![Doc::text("and "), pat, Doc::text(" = "), expr])
            }
        })
        .collect();
    Doc::join(Doc::hard_line(), docs)
}

// ============================================================================
// Attributes (placeholder)
// ============================================================================

/// Print attributes.
fn print_attributes(
    state: &PrinterState,
    attrs: &[Attribute],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    if attrs.is_empty() {
        return Doc::nil();
    }
    let docs: Vec<Doc> = attrs
        .iter()
        .filter(|attr| parsetree_viewer::is_printable_attribute(attr))
        .map(|attr| print_attribute(state, attr, cmt_tbl))
        .collect();
    if docs.is_empty() {
        Doc::nil()
    } else {
        Doc::concat(vec![Doc::group(Doc::join(Doc::space(), docs)), Doc::space()])
    }
}

/// Print a single attribute.
fn print_attribute(
    state: &PrinterState,
    attr: &Attribute,
    cmt_tbl: &mut CommentTable,
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
                        print_expression_with_comments(state, expr, cmt_tbl),
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
                print_typ_expr(state, typ, cmt_tbl),
                Doc::text(")"),
            ])
        }
        Payload::PSig(_) => Doc::text("(: ...)"),
        Payload::PPat(pat, guard) => {
            let pat_doc = print_pattern(state, pat, cmt_tbl);
            match guard {
                Some(expr) => Doc::concat(vec![
                    Doc::text("(? "),
                    pat_doc,
                    Doc::text(" when "),
                    print_expression_with_comments(state, expr, cmt_tbl),
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

/// Print an extension.
fn print_extension(
    _state: &PrinterState,
    ext: &Extension,
    _cmt_tbl: &mut CommentTable,
) -> Doc {
    Doc::concat(vec![Doc::text("%"), Doc::text(&ext.0.txt)])
}

// ============================================================================
// Structure Printing
// ============================================================================

/// Print a structure (list of structure items).
pub fn print_structure(
    state: &PrinterState,
    structure: &[StructureItem],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    if structure.is_empty() {
        return print_comments_inside_file(cmt_tbl);
    }

    print_list(
        |item: &StructureItem| &item.pstr_loc,
        structure,
        |item: &StructureItem, cmt_tbl: &mut CommentTable| print_structure_item(state, item, cmt_tbl),
        cmt_tbl,
        false,
    )
}

/// Print a signature (list of signature items).
pub fn print_signature(
    state: &PrinterState,
    signature: &[SignatureItem],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    if signature.is_empty() {
        return print_comments_inside_file(cmt_tbl);
    }

    print_list(
        |item: &SignatureItem| &item.psig_loc,
        signature,
        |item: &SignatureItem, cmt_tbl: &mut CommentTable| print_signature_item(state, item, cmt_tbl),
        cmt_tbl,
        false,
    )
}

/// Print a signature item.
pub fn print_signature_item(
    state: &PrinterState,
    item: &SignatureItem,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    match &item.psig_desc {
        SignatureItemDesc::Psig_value(val_desc) => {
            print_value_description(state, val_desc, cmt_tbl)
        }

        SignatureItemDesc::Psig_type(rec_flag, type_decls) => {
            let rec_doc = match rec_flag {
                RecFlag::Nonrecursive => Doc::nil(),
                RecFlag::Recursive => Doc::text("rec "),
            };
            // Extract attributes from first type declaration to print before "type"
            let attrs_doc = if let Some(first_decl) = type_decls.first() {
                print_attributes(state, &first_decl.ptype_attributes, cmt_tbl)
            } else {
                Doc::nil()
            };
            Doc::concat(vec![
                attrs_doc,
                Doc::text("type "),
                rec_doc,
                print_type_declarations_no_first_attrs(state, type_decls, cmt_tbl),
            ])
        }

        SignatureItemDesc::Psig_typext(type_ext) => {
            print_type_extension(state, type_ext, cmt_tbl)
        }

        SignatureItemDesc::Psig_exception(ext_constr) => {
            Doc::concat(vec![
                Doc::text("exception "),
                print_extension_constructor(state, ext_constr, cmt_tbl),
            ])
        }

        SignatureItemDesc::Psig_module(mod_decl) => {
            print_module_declaration(state, mod_decl, cmt_tbl)
        }

        SignatureItemDesc::Psig_recmodule(mod_decls) => {
            print_rec_module_declarations(state, mod_decls, cmt_tbl)
        }

        SignatureItemDesc::Psig_modtype(mod_type_decl) => {
            print_module_type_declaration(state, mod_type_decl, cmt_tbl)
        }

        SignatureItemDesc::Psig_open(open_desc) => {
            print_open_description(state, open_desc, cmt_tbl)
        }

        SignatureItemDesc::Psig_include(include_desc) => {
            print_include_description(state, include_desc, cmt_tbl)
        }

        SignatureItemDesc::Psig_attribute(attr) => {
            Doc::concat(vec![Doc::text("@@"), print_attribute(state, attr, cmt_tbl)])
        }

        SignatureItemDesc::Psig_extension(ext, attrs) => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl);
            Doc::concat(vec![
                attrs_doc,
                Doc::text("%%"),
                print_extension(state, ext, cmt_tbl),
            ])
        }
    }
}

/// Print a module declaration.
fn print_module_declaration(
    state: &PrinterState,
    decl: &ModuleDeclaration,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let attrs_doc = print_attributes(state, &decl.pmd_attributes, cmt_tbl);
    let name_doc = Doc::text(&decl.pmd_name.txt);
    let type_doc = print_module_type(state, &decl.pmd_type, cmt_tbl);

    Doc::concat(vec![
        attrs_doc,
        Doc::text("module "),
        name_doc,
        Doc::text(": "),
        type_doc,
    ])
}

/// Print recursive module declarations.
fn print_rec_module_declarations(
    state: &PrinterState,
    decls: &[ModuleDeclaration],
    cmt_tbl: &mut CommentTable,
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
            let type_doc = print_module_type(state, &decl.pmd_type, cmt_tbl);
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
) -> Doc {
    let attrs_doc = print_attributes(state, &include_desc.pincl_attributes, cmt_tbl);
    let mod_doc = print_module_type(state, &include_desc.pincl_mod, cmt_tbl);

    Doc::concat(vec![attrs_doc, Doc::text("include "), mod_doc])
}

/// Print a structure item.
pub fn print_structure_item(
    state: &PrinterState,
    item: &StructureItem,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    match &item.pstr_desc {
        StructureItemDesc::Pstr_value(rec_flag, bindings) => {
            let rec_doc = match rec_flag {
                RecFlag::Nonrecursive => Doc::nil(),
                RecFlag::Recursive => Doc::text("rec "),
            };
            Doc::concat(vec![
                Doc::text("let "),
                rec_doc,
                print_value_bindings(state, bindings, cmt_tbl),
            ])
        }

        StructureItemDesc::Pstr_type(rec_flag, type_decls) => {
            let rec_doc = match rec_flag {
                RecFlag::Nonrecursive => Doc::nil(),
                RecFlag::Recursive => Doc::text("rec "),
            };
            // Extract attributes from first type declaration to print before "type"
            let attrs_doc = if let Some(first_decl) = type_decls.first() {
                print_attributes(state, &first_decl.ptype_attributes, cmt_tbl)
            } else {
                Doc::nil()
            };
            Doc::concat(vec![
                attrs_doc,
                Doc::text("type "),
                rec_doc,
                print_type_declarations_no_first_attrs(state, type_decls, cmt_tbl),
            ])
        }

        StructureItemDesc::Pstr_primitive(val_desc) => {
            print_value_description(state, val_desc, cmt_tbl)
        }

        StructureItemDesc::Pstr_typext(type_ext) => {
            print_type_extension(state, type_ext, cmt_tbl)
        }

        StructureItemDesc::Pstr_exception(ext_constr) => {
            Doc::concat(vec![
                Doc::text("exception "),
                print_extension_constructor(state, ext_constr, cmt_tbl),
            ])
        }

        StructureItemDesc::Pstr_module(mod_binding) => {
            print_module_binding(state, mod_binding, cmt_tbl)
        }

        StructureItemDesc::Pstr_recmodule(mod_bindings) => {
            print_rec_module_bindings(state, mod_bindings, cmt_tbl)
        }

        StructureItemDesc::Pstr_modtype(mod_type_decl) => {
            print_module_type_declaration(state, mod_type_decl, cmt_tbl)
        }

        StructureItemDesc::Pstr_open(open_desc) => {
            print_open_description(state, open_desc, cmt_tbl)
        }

        StructureItemDesc::Pstr_include(include_decl) => {
            print_include_declaration(state, include_decl, cmt_tbl)
        }

        StructureItemDesc::Pstr_attribute(attr) => {
            Doc::concat(vec![Doc::text("@@"), print_attribute(state, attr, cmt_tbl)])
        }

        StructureItemDesc::Pstr_extension(ext, attrs) => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl);
            Doc::concat(vec![
                attrs_doc,
                Doc::text("%%"),
                print_extension(state, ext, cmt_tbl),
            ])
        }

        StructureItemDesc::Pstr_eval(expr, attrs) => {
            let attrs_doc = print_attributes(state, attrs, cmt_tbl);
            Doc::concat(vec![
                attrs_doc,
                print_expression_with_comments(state, expr, cmt_tbl),
            ])
        }
    }
}

/// Print type declarations.
fn print_type_declarations(
    state: &PrinterState,
    decls: &[TypeDeclaration],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let docs: Vec<Doc> = decls
        .iter()
        .enumerate()
        .map(|(i, decl)| {
            let prefix = if i == 0 { Doc::nil() } else { Doc::text("and ") };
            Doc::concat(vec![prefix, print_type_declaration(state, decl, cmt_tbl)])
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
) -> Doc {
    let docs: Vec<Doc> = decls
        .iter()
        .enumerate()
        .map(|(i, decl)| {
            let prefix = if i == 0 { Doc::nil() } else { Doc::text("and ") };
            if i == 0 {
                // Skip attributes for first declaration - they're printed before "type"
                Doc::concat(vec![prefix, print_type_declaration_no_attrs(state, decl, cmt_tbl)])
            } else {
                Doc::concat(vec![prefix, print_type_declaration(state, decl, cmt_tbl)])
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
) -> Doc {
    print_type_declaration_inner(state, decl, cmt_tbl, true)
}

/// Print a type declaration without attributes.
fn print_type_declaration_no_attrs(
    state: &PrinterState,
    decl: &TypeDeclaration,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    print_type_declaration_inner(state, decl, cmt_tbl, false)
}

/// Print a type declaration with optional attributes.
fn print_type_declaration_inner(
    state: &PrinterState,
    decl: &TypeDeclaration,
    cmt_tbl: &mut CommentTable,
    include_attrs: bool,
) -> Doc {
    let attrs_doc = if include_attrs {
        print_attributes(state, &decl.ptype_attributes, cmt_tbl)
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
            .map(|(typ, _variance)| print_typ_expr(state, typ, cmt_tbl))
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
                    print_typ_expr(state, typ, cmt_tbl),
                ]),
                None => Doc::nil(),
            }
        }
        TypeKind::Ptype_variant(_) => {
            // Manifest (if any) is printed without private flag
            match &decl.ptype_manifest {
                Some(typ) => Doc::concat(vec![
                    Doc::text(" = "),
                    print_typ_expr(state, typ, cmt_tbl),
                ]),
                None => Doc::nil(),
            }
        }
        TypeKind::Ptype_record(_) => {
            // Manifest (if any) is printed without private flag
            match &decl.ptype_manifest {
                Some(typ) => Doc::concat(vec![
                    Doc::text(" = "),
                    print_typ_expr(state, typ, cmt_tbl),
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
                print_constructor_declarations(state, constrs, &decl.ptype_private, cmt_tbl),
            ])
        }
        TypeKind::Ptype_record(fields) => {
            Doc::concat(vec![
                Doc::text(" = "),
                private_doc.clone(),
                print_record_declaration(state, fields, cmt_tbl),
            ])
        }
        TypeKind::Ptype_open => Doc::concat(vec![
            Doc::text(" = "),
            private_doc,
            Doc::text(".."),
        ]),
    };

    // Print type constraints (constraint 'a = int)
    let constraints_doc = print_type_constraints(state, &decl.ptype_cstrs, cmt_tbl);

    Doc::concat(vec![attrs_doc, name_doc, params_doc, manifest_doc, kind_doc, constraints_doc])
}

/// Print type definition constraints.
fn print_type_constraints(
    state: &PrinterState,
    constraints: &[(CoreType, CoreType, Location)],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    if constraints.is_empty() {
        return Doc::nil();
    }
    let constraint_docs: Vec<Doc> = constraints
        .iter()
        .map(|(typ1, typ2, _loc)| {
            Doc::concat(vec![
                Doc::text("constraint "),
                print_typ_expr(state, typ1, cmt_tbl),
                Doc::text(" = "),
                print_typ_expr(state, typ2, cmt_tbl),
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
            Doc::concat(vec![bar, print_constructor_declaration(state, constr, cmt_tbl)])
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
) -> Doc {
    let attrs_doc = print_attributes(state, &constr.pcd_attributes, cmt_tbl);
    let name_doc = Doc::text(&constr.pcd_name.txt);

    let args_doc = match &constr.pcd_args {
        ConstructorArguments::Pcstr_tuple(types) if types.is_empty() => Doc::nil(),
        ConstructorArguments::Pcstr_tuple(types) => {
            let type_docs: Vec<Doc> = types
                .iter()
                .map(|t| print_typ_expr(state, t, cmt_tbl))
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
                print_record_declaration(state, fields, cmt_tbl),
                Doc::text(")"),
            ])
        }
    };

    let res_doc = match &constr.pcd_res {
        Some(typ) => Doc::concat(vec![Doc::text(": "), print_typ_expr(state, typ, cmt_tbl)]),
        None => Doc::nil(),
    };

    Doc::concat(vec![attrs_doc, name_doc, args_doc, res_doc])
}

/// Print record declaration.
fn print_record_declaration(
    state: &PrinterState,
    fields: &[LabelDeclaration],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let field_docs: Vec<Doc> = fields
        .iter()
        .map(|field| print_label_declaration(state, field, cmt_tbl))
        .collect();
    Doc::group(Doc::concat(vec![
        Doc::lbrace(),
        Doc::indent(Doc::concat(vec![
            Doc::soft_line(),
            Doc::join(Doc::concat(vec![Doc::text(","), Doc::line()]), field_docs),
        ])),
        Doc::trailing_comma(),
        Doc::soft_line(),
        Doc::rbrace(),
    ]))
}

/// Print a label declaration (record field).
fn print_label_declaration(
    state: &PrinterState,
    field: &LabelDeclaration,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let attrs_doc = print_attributes(state, &field.pld_attributes, cmt_tbl);
    let mutable_doc = if field.pld_mutable == MutableFlag::Mutable {
        Doc::text("mutable ")
    } else {
        Doc::nil()
    };
    let name_doc = print_ident_like(&field.pld_name.txt, false, false);
    let typ_doc = print_typ_expr(state, &field.pld_type, cmt_tbl);

    Doc::concat(vec![attrs_doc, mutable_doc, name_doc, Doc::text(": "), typ_doc])
}

/// Print value description (external declaration).
fn print_value_description(
    state: &PrinterState,
    val_desc: &ValueDescription,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let attrs_doc = print_attributes(state, &val_desc.pval_attributes, cmt_tbl);
    let name_doc = Doc::text(&val_desc.pval_name.txt);
    let typ_doc = print_typ_expr(state, &val_desc.pval_type, cmt_tbl);

    let prim_doc = if val_desc.pval_prim.is_empty() {
        Doc::nil()
    } else {
        let prims: Vec<Doc> = val_desc.pval_prim.iter().map(|s| Doc::text(format!("\"{}\"", s))).collect();
        Doc::concat(vec![Doc::text(" = "), Doc::join(Doc::space(), prims)])
    };

    Doc::concat(vec![
        attrs_doc,
        Doc::text("external "),
        name_doc,
        Doc::text(": "),
        typ_doc,
        prim_doc,
    ])
}

/// Print type extension.
fn print_type_extension(
    state: &PrinterState,
    type_ext: &TypeExtension,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let attrs_doc = print_attributes(state, &type_ext.ptyext_attributes, cmt_tbl);
    let path_doc = print_lident(&type_ext.ptyext_path.txt);

    let params_doc = if type_ext.ptyext_params.is_empty() {
        Doc::nil()
    } else {
        let params: Vec<Doc> = type_ext
            .ptyext_params
            .iter()
            .map(|(typ, _variance)| print_typ_expr(state, typ, cmt_tbl))
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
        .map(|constr| print_extension_constructor(state, constr, cmt_tbl))
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
) -> Doc {
    let attrs_doc = print_attributes(state, &ext_constr.pext_attributes, cmt_tbl);
    let name_doc = Doc::text(&ext_constr.pext_name.txt);

    let kind_doc = match &ext_constr.pext_kind {
        ExtensionConstructorKind::Pext_decl(args, res) => {
            let args_doc = match args {
                ConstructorArguments::Pcstr_tuple(types) if types.is_empty() => Doc::nil(),
                ConstructorArguments::Pcstr_tuple(types) => {
                    let type_docs: Vec<Doc> = types
                        .iter()
                        .map(|t| print_typ_expr(state, t, cmt_tbl))
                        .collect();
                    Doc::concat(vec![
                        Doc::text("("),
                        Doc::join(Doc::concat(vec![Doc::text(","), Doc::space()]), type_docs),
                        Doc::text(")"),
                    ])
                }
                ConstructorArguments::Pcstr_record(fields) => {
                    Doc::concat(vec![
                        Doc::text("({"),
                        print_record_declaration(state, fields, cmt_tbl),
                        Doc::text("})"),
                    ])
                }
            };
            let res_doc = match res {
                Some(typ) => Doc::concat(vec![Doc::text(": "), print_typ_expr(state, typ, cmt_tbl)]),
                None => Doc::nil(),
            };
            Doc::concat(vec![args_doc, res_doc])
        }
        ExtensionConstructorKind::Pext_rebind(lid) => {
            Doc::concat(vec![Doc::text(" = "), print_longident(&lid.txt)])
        }
    };

    Doc::concat(vec![attrs_doc, name_doc, kind_doc])
}

/// Print module binding.
fn print_module_binding(
    state: &PrinterState,
    binding: &ModuleBinding,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let attrs_doc = print_attributes(state, &binding.pmb_attributes, cmt_tbl);
    let name_doc = Doc::text(&binding.pmb_name.txt);
    let expr_doc = print_mod_expr(state, &binding.pmb_expr, cmt_tbl);

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
            let expr_doc = print_mod_expr(state, &binding.pmb_expr, cmt_tbl);
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
) -> Doc {
    let attrs_doc = print_attributes(state, &decl.pmtd_attributes, cmt_tbl);
    let name_doc = Doc::text(&decl.pmtd_name.txt);
    let typ_doc = match &decl.pmtd_type {
        Some(mty) => Doc::concat(vec![Doc::text(" = "), print_module_type(state, mty, cmt_tbl)]),
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
    state: &PrinterState,
    open_desc: &OpenDescription,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let attrs_doc = print_attributes(state, &open_desc.popen_attributes, cmt_tbl);
    let lid_doc = print_longident(&open_desc.popen_lid.txt);

    Doc::concat(vec![attrs_doc, Doc::text("open "), lid_doc])
}

/// Print include declaration.
fn print_include_declaration(
    state: &PrinterState,
    include_decl: &IncludeDeclaration,
    cmt_tbl: &mut CommentTable,
) -> Doc {
    let attrs_doc = print_attributes(state, &include_decl.pincl_attributes, cmt_tbl);
    let mod_doc = print_mod_expr(state, &include_decl.pincl_mod, cmt_tbl);

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
) -> String {
    let mut cmt_tbl = CommentTable::new();
    cmt_tbl.walk_structure(structure, comments);
    let state = PrinterState::init();
    let doc = print_structure(&state, structure, &mut cmt_tbl);
    let output = doc.to_string(width);
    if output.is_empty() {
        output
    } else {
        format!("{}\n", output)
    }
}

/// Print a structure with comments using default width.
pub fn print_structure_with_comments(structure: &[StructureItem], comments: Vec<Comment>) -> String {
    print_implementation(structure, comments, DEFAULT_PRINT_WIDTH)
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
