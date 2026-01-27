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
            let constr = print_longident_loc(longident_loc, cmt_tbl);
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
        // If-then-else
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
        // Function application
        ExpressionDesc::Pexp_apply { funct, args, .. } => {
            print_pexp_apply(state, e, funct, args, cmt_tbl)
        }
        // Constraint: (expr : typ)
        ExpressionDesc::Pexp_constraint(expr, typ) => {
            let expr_doc = print_expression_with_comments(state, expr, cmt_tbl);
            let typ_doc = print_typ_expr(state, typ, cmt_tbl);
            Doc::concat(vec![
                Doc::lparen(),
                expr_doc,
                Doc::text(" : "),
                typ_doc,
                Doc::rparen(),
            ])
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
            let rhs = match parens::assert_or_await_expr_rhs(false, expr) {
                ParenKind::Parenthesized => add_parens(rhs),
                ParenKind::Braced(loc) => print_braces(rhs, expr, loc),
                ParenKind::Nothing => rhs,
            };
            Doc::concat(vec![Doc::text("assert "), rhs])
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
        // Send (method call): expr#method
        ExpressionDesc::Pexp_send(expr, method) => {
            let lhs = print_expression_with_comments(state, expr, cmt_tbl);
            Doc::concat(vec![lhs, Doc::text("#"), Doc::text(&method.txt)])
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

/// Print arrow expression (function).
fn print_arrow_expression(
    _state: &PrinterState,
    _e: &Expression,
    _cmt_tbl: &mut CommentTable,
) -> Doc {
    // Simplified placeholder - full implementation needed
    Doc::text("() => ...")
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
                    return print_binary_expression(state, funct, args, cmt_tbl);
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
    _funct: &Expression,
    args: &[(ArgLabel, Expression)],
    cmt_tbl: &mut CommentTable,
) -> Doc {
    if args.len() != 2 {
        return Doc::nil();
    }
    let (_, lhs) = &args[0];
    let (_, rhs) = &args[1];

    // For now, simplified implementation
    let lhs_doc = print_expression_with_comments(state, lhs, cmt_tbl);
    let rhs_doc = print_expression_with_comments(state, rhs, cmt_tbl);

    Doc::concat(vec![
        lhs_doc,
        Doc::space(),
        Doc::text("op"),
        Doc::space(),
        rhs_doc,
    ])
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

    let args_doc: Vec<Doc> = args
        .iter()
        .map(|(label, expr)| {
            let expr_doc = print_expression_with_comments(state, expr, cmt_tbl);
            match label {
                ArgLabel::Nolabel => expr_doc,
                ArgLabel::Labelled(name) => {
                    Doc::concat(vec![Doc::text("~"), Doc::text(&name.txt), Doc::text("="), expr_doc])
                }
                ArgLabel::Optional(name) => {
                    Doc::concat(vec![Doc::text("~"), Doc::text(&name.txt), Doc::text("=?"), expr_doc])
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

// ============================================================================
// Pattern Printing (placeholder)
// ============================================================================

/// Print a pattern.
pub fn print_pattern(
    _state: &PrinterState,
    pat: &Pattern,
    _cmt_tbl: &mut CommentTable,
) -> Doc {
    // Simplified placeholder - full implementation needed
    match &pat.ppat_desc {
        PatternDesc::Ppat_any => Doc::text("_"),
        PatternDesc::Ppat_var(name) => Doc::text(&name.txt),
        PatternDesc::Ppat_constant(c) => print_constant(false, c),
        PatternDesc::Ppat_tuple(pats) => {
            let docs: Vec<Doc> = pats.iter().map(|_| Doc::text("_")).collect();
            Doc::concat(vec![
                Doc::lparen(),
                Doc::join(Doc::text(", "), docs),
                Doc::rparen(),
            ])
        }
        _ => Doc::text("<pattern>"),
    }
}

// ============================================================================
// Type Printing (placeholder)
// ============================================================================

/// Print a type expression.
pub fn print_typ_expr(
    _state: &PrinterState,
    typ: &CoreType,
    _cmt_tbl: &mut CommentTable,
) -> Doc {
    // Simplified placeholder - full implementation needed
    match &typ.ptyp_desc {
        CoreTypeDesc::Ptyp_any => Doc::text("_"),
        CoreTypeDesc::Ptyp_var(name) => Doc::concat(vec![Doc::text("'"), Doc::text(name.clone())]),
        CoreTypeDesc::Ptyp_constr(lid, _) => print_longident(&lid.txt),
        _ => Doc::text("<type>"),
    }
}

// ============================================================================
// Module Printing (placeholder)
// ============================================================================

/// Print a module expression.
fn print_mod_expr(
    _state: &PrinterState,
    _mod_expr: &ModuleExpr,
    _cmt_tbl: &mut CommentTable,
) -> Doc {
    // Simplified placeholder
    Doc::text("<module>")
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
    _state: &PrinterState,
    _attrs: &[Attribute],
    _cmt_tbl: &mut CommentTable,
) -> Doc {
    // Simplified - attributes need full implementation
    Doc::nil()
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
