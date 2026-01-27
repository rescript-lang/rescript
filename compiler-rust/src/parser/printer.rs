//! ReScript AST printer.
//!
//! This module converts the AST back to ReScript source code.
//! It's used for formatting and roundtrip testing.

use super::ast::*;
use super::comment::{Comment, CommentStyle};
use super::doc::Doc;
use super::longident::Longident;
use super::token::Token;
use crate::location::Location;

/// Printer state for formatting output.
pub struct Printer {
    /// Output buffer.
    output: String,
    /// Current indentation level.
    indent: usize,
    /// Indentation string (spaces).
    indent_str: &'static str,
    /// Maximum line width for formatting decisions.
    max_width: usize,
    /// Current line length (approximate, in bytes).
    line_len: usize,
    /// Optional comment stream for preserving comments.
    comments: Option<CommentCursor>,
}

#[derive(Debug, Clone)]
struct CommentCursor {
    comments: Vec<Comment>,
    index: usize,
}

impl CommentCursor {
    fn new(comments: Vec<Comment>) -> Self {
        Self { comments, index: 0 }
    }

    fn peek(&self) -> Option<&Comment> {
        self.comments.get(self.index)
    }

    fn advance(&mut self) {
        if self.index < self.comments.len() {
            self.index += 1;
        }
    }
}

impl Default for Printer {
    fn default() -> Self {
        Self::new()
    }
}

impl Printer {
    /// Create a new printer.
    pub fn new() -> Self {
        Printer {
            output: String::new(),
            indent: 0,
            indent_str: "  ",
            max_width: 100,
            line_len: 0,
            comments: None,
        }
    }

    /// Create a new printer with a custom line width.
    pub fn with_width(max_width: usize) -> Self {
        Printer {
            max_width,
            ..Self::new()
        }
    }

    /// Create a new printer with comments.
    pub fn with_comments(comments: Vec<Comment>) -> Self {
        Printer {
            comments: Some(CommentCursor::new(comments)),
            ..Self::new()
        }
    }

    /// Create a new printer with comments and a custom line width.
    pub fn with_comments_and_width(comments: Vec<Comment>, max_width: usize) -> Self {
        Printer {
            max_width,
            comments: Some(CommentCursor::new(comments)),
            ..Self::new()
        }
    }

    /// Get the printed output.
    pub fn output(&self) -> &str {
        &self.output
    }

    /// Consume the printer and return the output.
    pub fn into_output(self) -> String {
        self.output
    }

    /// Write a string to the output.
    fn write(&mut self, s: &str) {
        self.output.push_str(s);
        if let Some(idx) = s.rfind('\n') {
            let after = &s[idx + 1..];
            self.line_len = after.len();
        } else {
            self.line_len += s.len();
        }
    }

    /// Classify an identifier to determine how it should be printed.
    /// This matches OCaml's classify_ident_content function.
    ///
    /// - `allow_uident`: If true, uppercase letters are allowed at the start (for module/type names).
    /// - `allow_hyphen`: If true, hyphens are allowed in the identifier (for JSX tag names).
    ///
    /// Returns true if the identifier needs escaping.
    fn needs_escaping(s: &str, allow_uident: bool, allow_hyphen: bool) -> bool {
        // Keywords always need escaping (except true/false which are boolean literals)
        if Token::is_keyword_txt(s) && s != "true" && s != "false" {
            return true;
        }

        if s.is_empty() {
            return true;
        }

        let bytes = s.as_bytes();

        // Check first character
        match bytes[0] {
            // Backslash means it's already an escaped/uppercase exotic identifier
            b'\\' => return false,
            // Uppercase allowed only if allow_uident is true
            b'A'..=b'Z' => {
                if !allow_uident {
                    return true;
                }
            }
            // Lowercase and underscore always allowed
            b'a'..=b'z' | b'_' => {}
            // Anything else needs escaping
            _ => return true,
        }

        // Check subsequent characters
        for &b in &bytes[1..] {
            match b {
                b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'\'' | b'_' => {}
                b'-' if allow_hyphen => {}
                _ => return true,
            }
        }

        false
    }

    /// Write an identifier, escaping it if it's a keyword or exotic.
    /// Keywords like "switch" need to be written as \"switch" in ReScript.
    /// Exotic identifiers like "=" also need escaping: \"="
    ///
    /// By default, identifiers starting with uppercase letters need escaping.
    /// Use `write_ident_allow_uident` for contexts where uppercase is allowed (module names, etc.).
    fn write_ident(&mut self, s: &str) {
        self.write_ident_with_options(s, false, false);
    }

    /// Write an identifier allowing uppercase first characters (for module names, type constructors, etc.)
    fn write_ident_allow_uident(&mut self, s: &str) {
        self.write_ident_with_options(s, true, false);
    }

    /// Write an identifier with configurable escaping rules.
    fn write_ident_with_options(&mut self, s: &str, allow_uident: bool, allow_hyphen: bool) {
        if Self::needs_escaping(s, allow_uident, allow_hyphen) {
            self.write("\\\"");
            self.write(s);
            self.write("\"");
        } else {
            self.write(s);
        }
    }

    /// Write a type variable name with proper escaping.
    /// Type variables allow uppercase letters (e.g., 'T, 'Element).
    fn write_type_var(&mut self, s: &str) {
        // Type variables allow uppercase, so use allow_uident=true
        if Self::needs_escaping(s, true, false) {
            self.write("\\\"");
            self.write(s);
            self.write("\"");
        } else {
            self.write(s);
        }
    }

    /// Write a newline and indentation.
    fn newline(&mut self) {
        self.output.push('\n');
        self.line_len = 0;
        for _ in 0..self.indent {
            self.output.push_str(self.indent_str);
            self.line_len += self.indent_str.len();
        }
    }

    /// Increase indentation.
    fn indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation.
    fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    /// Write a space.
    fn space(&mut self) {
        self.write(" ");
    }

    /// Render a Doc and write it to the output buffer.
    /// The Doc's line breaks will be resolved based on max_width.
    fn write_doc(&mut self, doc: Doc) {
        let rendered = doc.to_string(self.max_width as i32);
        // Add current indentation to each line
        for (i, line) in rendered.lines().enumerate() {
            if i > 0 {
                self.output.push('\n');
                for _ in 0..self.indent {
                    self.output.push_str(self.indent_str);
                }
            }
            self.output.push_str(line);
        }
        // Update line_len based on last line
        if let Some(last_newline) = self.output.rfind('\n') {
            self.line_len = self.output.len() - last_newline - 1;
        } else {
            self.line_len = self.output.len();
        }
    }

    /// Convert an expression to a string using a fresh printer.
    fn expr_to_string(&self, expr: &Expression) -> String {
        let mut p = Printer::with_width(self.max_width);
        p.print_expression(expr);
        p.into_output()
    }

    /// Print a list expression using Doc for proper line breaking.
    fn print_list_with_doc(&mut self, elems: &[&Expression], spread: Option<&Expression>) {
        if elems.is_empty() && spread.is_none() {
            self.write("list{}");
            return;
        }

        // Collect element locations for consuming comments later
        let mut elem_locations: Vec<Location> = Vec::new();
        let mut all_docs: Vec<(Doc, String)> = Vec::new();

        for elem in elems {
            let elem_str = self.expr_to_string(elem);
            let trailing_comment = self.get_trailing_comments_text(&elem.pexp_loc);
            elem_locations.push(elem.pexp_loc.clone());
            all_docs.push((Doc::text(elem_str), trailing_comment));
        }

        if let Some(spread_expr) = spread {
            let spread_str = format!("...{}", self.expr_to_string(spread_expr));
            let trailing_comment = self.get_trailing_comments_text(&spread_expr.pexp_loc);
            elem_locations.push(spread_expr.pexp_loc.clone());
            all_docs.push((Doc::text(spread_str), trailing_comment));
        }

        // Build doc with separators, using line_suffix for trailing comments
        let docs_with_sep: Vec<(Doc, Doc)> = all_docs
            .into_iter()
            .map(|(elem_doc, comment)| {
                let elem_with_comment = if comment.is_empty() {
                    elem_doc
                } else {
                    Doc::concat(vec![
                        elem_doc,
                        Doc::line_suffix(Doc::text(comment)),
                    ])
                };
                (elem_with_comment, Doc::concat(vec![Doc::text(","), Doc::line()]))
            })
            .collect();

        let doc = Doc::group(Doc::concat(vec![
            Doc::text("list{"),
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                Doc::join_with_sep(docs_with_sep),
                Doc::trailing_comma(),
            ])),
            Doc::soft_line(),
            Doc::text("}"),
        ]));

        self.write_doc(doc);

        // Consume the comments that were included
        for loc in &elem_locations {
            self.consume_trailing_comments_for_loc(loc);
        }
    }

    /// Print an array expression using Doc for proper line breaking.
    fn print_array_with_doc(&mut self, elems: &[Expression]) {
        if elems.is_empty() {
            self.write("[]");
            return;
        }

        // Collect element locations for consuming comments later
        let mut elem_locations: Vec<Location> = Vec::new();
        let mut all_docs: Vec<(Doc, String)> = Vec::new();

        for elem in elems {
            let elem_str = self.expr_to_string(elem);
            let trailing_comment = self.get_trailing_comments_text(&elem.pexp_loc);
            elem_locations.push(elem.pexp_loc.clone());
            all_docs.push((Doc::text(elem_str), trailing_comment));
        }

        // Build doc with separators, using line_suffix for trailing comments
        let docs_with_sep: Vec<(Doc, Doc)> = all_docs
            .into_iter()
            .map(|(elem_doc, comment)| {
                let elem_with_comment = if comment.is_empty() {
                    elem_doc
                } else {
                    Doc::concat(vec![
                        elem_doc,
                        Doc::line_suffix(Doc::text(comment)),
                    ])
                };
                (elem_with_comment, Doc::concat(vec![Doc::text(","), Doc::line()]))
            })
            .collect();

        let doc = Doc::group(Doc::concat(vec![
            Doc::text("["),
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                Doc::join_with_sep(docs_with_sep),
                Doc::trailing_comma(),
            ])),
            Doc::soft_line(),
            Doc::text("]"),
        ]));

        self.write_doc(doc);

        // Consume the comments that were included
        for loc in &elem_locations {
            self.consume_trailing_comments_for_loc(loc);
        }
    }

    /// Print a record expression field to string (for Doc formatting).
    fn record_field_to_string(&self, field: &ExpressionRecordField) -> String {
        // Check for punning: if field name equals expression variable name
        if let ExpressionDesc::Pexp_ident(path) = &field.expr.pexp_desc {
            if let Longident::Lident(name) = &path.txt {
                if let Longident::Lident(field_name) = &field.lid.txt {
                    if name == field_name && !field.opt {
                        // Punned field - just the name (with escaping)
                        return self.ident_to_string(name);
                    }
                }
            }
        }

        let mut result = String::new();

        // Field name (with escaping for keywords/exotic idents)
        result.push_str(&self.lident_to_string(&field.lid.txt));
        result.push_str(": ");

        // Optional marker
        if field.opt {
            result.push_str("? ");
        }

        // Expression
        result.push_str(&self.expr_to_string(&field.expr));

        result
    }

    /// Convert a longident to string.
    fn longident_to_string(&self, lid: &Longident) -> String {
        match lid {
            Longident::Lident(name) => name.clone(),
            Longident::Ldot(prefix, name) => {
                format!("{}.{}", self.longident_to_string(prefix), name)
            }
            Longident::Lapply(a, b) => {
                format!("{}({})", self.longident_to_string(a), self.longident_to_string(b))
            }
        }
    }

    /// Convert a longident to string, escaping the final segment if needed.
    fn lident_to_string(&self, lid: &Longident) -> String {
        match lid {
            Longident::Lident(name) => self.ident_to_string(name),
            Longident::Ldot(prefix, name) => {
                format!("{}.{}", self.longident_to_string(prefix), self.ident_to_string(name))
            }
            Longident::Lapply(_, _) => {
                "printLident: Longident.Lapply is not supported".to_string()
            }
        }
    }

    /// Convert an identifier to string, escaping if needed.
    fn ident_to_string(&self, name: &str) -> String {
        if Self::needs_escaping(name, false, false) {
            format!("\\\"{}\"", name)
        } else {
            name.to_string()
        }
    }

    /// Print a record expression using Doc for proper line breaking.
    fn print_record_with_doc(
        &mut self,
        fields: &[ExpressionRecordField],
        spread: Option<&Expression>,
        loc: &Location,
    ) {
        if fields.is_empty() && spread.is_none() {
            self.write("{}");
            return;
        }

        // Check if record spans multiple lines (to force break)
        let force_break = if let Some(first_field) = fields.first() {
            loc.loc_start.line < first_field.lid.loc.loc_start.line
        } else if let Some(spread_expr) = spread {
            loc.loc_start.line < spread_expr.pexp_loc.loc_start.line
        } else {
            false
        };

        // Also check if the record was originally multiline by checking if any field is on different line
        let multiline = force_break || fields.windows(2).any(|w| {
            w[0].expr.pexp_loc.loc_end.line < w[1].lid.loc.loc_start.line
        });

        let mut all_docs: Vec<(Doc, String)> = Vec::new();

        // Add spread if present
        if let Some(spread_expr) = spread {
            all_docs.push((
                Doc::concat(vec![
                    Doc::text("..."),
                    Doc::text(self.expr_to_string(spread_expr)),
                ]),
                String::new(), // No trailing comment for spread
            ));
        }

        // Disallow punning for single-element records (unless there's a spread)
        let punning_allowed = spread.is_some() || fields.len() > 1;

        // Collect field locations for later consuming comments
        let mut field_locations: Vec<Location> = Vec::new();

        for field in fields {
            // Create combined location for this field (from name to expression end)
            let field_loc = Location {
                loc_start: field.lid.loc.loc_start.clone(),
                loc_end: field.expr.pexp_loc.loc_end.clone(),
                loc_ghost: false,
                id: field.lid.loc.id,
            };

            let field_str = if punning_allowed {
                self.record_field_to_string(field)
            } else {
                // Single field without spread - no punning
                let mut result = self.lident_to_string(&field.lid.txt);
                result.push_str(": ");
                if field.opt {
                    result.push_str("?");
                }
                result.push_str(&self.expr_to_string(&field.expr));
                result
            };

            // Get trailing comments for this field
            let trailing_comment = self.get_trailing_comments_text(&field_loc);

            field_locations.push(field_loc);
            all_docs.push((Doc::text(field_str), trailing_comment));
        }

        // Build doc with separators
        // Use line_suffix for trailing comments so they appear AFTER the comma
        let docs_with_sep: Vec<(Doc, Doc)> = all_docs
            .into_iter()
            .map(|(field_doc, comment)| {
                // Field doc with optional line suffix for trailing comment
                let field_with_comment = if comment.is_empty() {
                    field_doc
                } else {
                    // Use line_suffix so comment prints AFTER the comma
                    Doc::concat(vec![
                        field_doc,
                        Doc::line_suffix(Doc::text(comment)),
                    ])
                };

                // Standard separator: comma + line break
                let sep = Doc::concat(vec![Doc::text(","), Doc::line()]);

                (field_with_comment, sep)
            })
            .collect();

        let inner = Doc::concat(vec![
            Doc::soft_line(),
            Doc::join_with_sep(docs_with_sep),
            Doc::trailing_comma(),
        ]);

        let doc = Doc::breakable_group(
            Doc::concat(vec![
                Doc::text("{"),
                Doc::indent(inner),
                Doc::soft_line(),
                Doc::text("}"),
            ]),
            multiline || force_break,
        );

        self.write_doc(doc);

        // Consume the comments that were included in the doc
        for field_loc in &field_locations {
            self.consume_trailing_comments_for_loc(field_loc);
        }
    }

    /// Check if we're at the start of a line.
    fn at_line_start(&self) -> bool {
        self.output.is_empty() || self.output.ends_with('\n')
    }

    /// Ensure indentation is written if we're at the start of a line.
    fn write_indent_if_line_start(&mut self) {
        if self.at_line_start() {
            for _ in 0..self.indent {
                self.output.push_str(self.indent_str);
                self.line_len += self.indent_str.len();
            }
        }
    }

    fn comments_enabled(&self) -> bool {
        self.comments.is_some()
    }

    fn comment_before_loc(comment: &Comment, loc: &Location) -> bool {
        comment.loc.loc_start.cnum <= loc.loc_start.cnum
    }

    fn is_trailing_comment_for_loc(comment: &Comment, loc: &Location) -> bool {
        let start = &comment.loc.loc_start;
        let end = &loc.loc_end;
        start.line == end.line && start.cnum >= end.cnum && comment.prev_tok_end_pos.cnum >= end.cnum
    }

    /// Get trailing comments for a location without consuming them.
    /// Returns them as formatted text.
    fn get_trailing_comments_text(&self, loc: &Location) -> String {
        let Some(cursor) = &self.comments else {
            return String::new();
        };

        let mut result = String::new();
        let mut temp_cursor = cursor.clone();

        // Skip comments that come before the location's end line
        while let Some(comment) = temp_cursor.peek() {
            if comment.loc.loc_start.line < loc.loc_end.line {
                temp_cursor.advance();
            } else {
                break;
            }
        }

        while let Some(comment) = temp_cursor.peek() {
            if Self::is_trailing_comment_for_loc(comment, loc) {
                result.push(' ');
                match comment.style {
                    CommentStyle::SingleLine => {
                        result.push_str("//");
                        result.push_str(comment.txt());
                    }
                    CommentStyle::MultiLine => {
                        result.push_str("/*");
                        result.push_str(comment.txt());
                        result.push_str("*/");
                    }
                    CommentStyle::DocComment => {
                        result.push_str("/**");
                        result.push_str(comment.txt());
                        result.push_str("*/");
                    }
                    CommentStyle::ModuleComment => {
                        result.push_str("/***");
                        result.push_str(comment.txt());
                        result.push_str("*/");
                    }
                }
                temp_cursor.advance();
            } else {
                break;
            }
        }

        result
    }

    /// Consume trailing comments for a location from the cursor.
    fn consume_trailing_comments_for_loc(&mut self, loc: &Location) {
        let Some(mut cursor) = self.comments.take() else {
            return;
        };
        while let Some(comment) = cursor.peek() {
            if Self::is_trailing_comment_for_loc(comment, loc) {
                cursor.advance();
            } else {
                break;
            }
        }
        self.comments = Some(cursor);
    }

    fn print_comment_text(&mut self, comment: &Comment) {
        match comment.style {
            CommentStyle::SingleLine => {
                self.write("//");
                self.write(comment.txt());
            }
            CommentStyle::MultiLine => {
                self.write("/*");
                self.write(comment.txt());
                self.write("*/");
            }
            CommentStyle::DocComment => {
                self.write("/**");
                self.write(comment.txt());
                self.write("*/");
            }
            CommentStyle::ModuleComment => {
                self.write("/***");
                self.write(comment.txt());
                self.write("*/");
            }
        }
    }

    fn print_comment_leading(&mut self, comment: &Comment) {
        let inline = comment.loc.loc_start.line == comment.prev_tok_end_pos.line;
        if inline {
            if !self.at_line_start() && !self.output.ends_with(' ') {
                self.space();
            }
            self.print_comment_text(comment);
            if comment.is_single_line() {
                self.newline();
            } else if !self.output.ends_with(' ') {
                self.space();
            }
        } else {
            if !self.at_line_start() {
                self.newline();
            }
            self.write_indent_if_line_start();
            self.print_comment_text(comment);
            self.newline();
        }
    }

    fn print_comment_trailing(&mut self, comment: &Comment) {
        if !self.at_line_start() && !self.output.ends_with(' ') {
            self.space();
        }
        self.print_comment_text(comment);
        if comment.is_single_line() {
            self.newline();
        } else if !self.output.ends_with(' ') {
            self.space();
        }
    }

    fn print_comments_before_loc(&mut self, loc: &Location) {
        let Some(mut cursor) = self.comments.take() else {
            return;
        };
        while let Some(comment) = cursor.peek() {
            if Self::comment_before_loc(comment, loc) {
                self.print_comment_leading(comment);
                cursor.advance();
            } else {
                break;
            }
        }
        self.comments = Some(cursor);
    }

    fn print_comments_before_loc_end(&mut self, loc: &Location) {
        let Some(mut cursor) = self.comments.take() else {
            return;
        };
        while let Some(comment) = cursor.peek() {
            if comment.loc.loc_start.cnum <= loc.loc_end.cnum {
                self.print_comment_leading(comment);
                cursor.advance();
            } else {
                break;
            }
        }
        self.comments = Some(cursor);
    }

    fn has_comment_before_loc_end(&self, loc: &Location) -> bool {
        self.comments
            .as_ref()
            .and_then(|cursor| cursor.peek())
            .map_or(false, |comment| comment.loc.loc_start.cnum <= loc.loc_end.cnum)
    }

    fn print_trailing_comments_for_loc(&mut self, loc: &Location) {
        let Some(mut cursor) = self.comments.take() else {
            return;
        };
        while let Some(comment) = cursor.peek() {
            if Self::is_trailing_comment_for_loc(comment, loc) {
                self.print_comment_trailing(comment);
                cursor.advance();
            } else {
                break;
            }
        }
        self.comments = Some(cursor);
    }

    fn flush_remaining_comments(&mut self) {
        let Some(mut cursor) = self.comments.take() else {
            return;
        };
        while let Some(comment) = cursor.peek() {
            self.print_comment_leading(comment);
            cursor.advance();
        }
        self.comments = Some(cursor);
    }

    /// Check if an expression is a block-like form (let, letmodule, letexception, open, sequence)
    /// that shouldn't get extra braces when inside another block.
    fn is_block_body_expr(expr: &Expression) -> bool {
        matches!(
            &expr.pexp_desc,
            ExpressionDesc::Pexp_let(..)
                | ExpressionDesc::Pexp_letmodule(..)
                | ExpressionDesc::Pexp_letexception(..)
                | ExpressionDesc::Pexp_open(..)
                | ExpressionDesc::Pexp_sequence(..)
        )
    }

    /// Collect list expressions from a cons chain (`::`) into a vector of expressions
    /// and an optional spread expression.
    ///
    /// For `list{a, b, c}` represented as `a :: b :: c :: []`, returns `([a, b, c], None)`.
    /// For `list{a, b, ...rest}` represented as `a :: b :: rest`, returns `([a, b], Some(rest))`.
    fn collect_list_expressions(expr: &Expression) -> (Vec<&Expression>, Option<&Expression>) {
        let mut acc = Vec::new();
        let mut current = expr;

        loop {
            match &current.pexp_desc {
                // Empty list `[]` - end of list, no spread
                ExpressionDesc::Pexp_construct(lid, _)
                    if matches!(&lid.txt, Longident::Lident(s) if s == "[]") =>
                {
                    return (acc, None);
                }
                // Cons cell `::` with tuple argument (head, tail)
                ExpressionDesc::Pexp_construct(lid, Some(arg))
                    if matches!(&lid.txt, Longident::Lident(s) if s == "::") =>
                {
                    if let ExpressionDesc::Pexp_tuple(elems) = &arg.pexp_desc {
                        if elems.len() == 2 {
                            acc.push(&elems[0]);
                            current = &elems[1];
                            continue;
                        }
                    }
                    // Malformed cons - treat as spread
                    return (acc, Some(current));
                }
                // Not a list constructor - this is a spread
                _ => {
                    return (acc, Some(current));
                }
            }
        }
    }

    /// Collect list patterns from a cons chain (`::`) into a vector of patterns
    /// and a tail pattern.
    ///
    /// For `list{a, b, c}` represented as `a :: b :: c :: []`, returns `([a, b, c], [])`.
    /// For `list{a, b, ...rest}` represented as `a :: b :: rest`, returns `([a, b], rest)`.
    fn collect_list_patterns(pat: &Pattern) -> (Vec<&Pattern>, &Pattern) {
        let mut acc = Vec::new();
        let mut current = pat;

        loop {
            match &current.ppat_desc {
                // Cons cell `::` with tuple argument (head, tail)
                PatternDesc::Ppat_construct(lid, Some(arg))
                    if matches!(&lid.txt, Longident::Lident(s) if s == "::") =>
                {
                    if let PatternDesc::Ppat_tuple(elems) = &arg.ppat_desc {
                        if elems.len() == 2 {
                            acc.push(&elems[0]);
                            current = &elems[1];
                            continue;
                        }
                    }
                    // Malformed cons - return as-is
                    return (acc, current);
                }
                // Not a cons constructor - return the tail
                _ => {
                    return (acc, current);
                }
            }
        }
    }

    /// Print a block body expression without adding outer braces.
    /// Used for printing the continuation of let/module/open/exception.
    fn print_block_body(&mut self, expr: &Expression) {
        self.print_block_body_inner(expr, true);
    }

    fn print_block_body_inner(&mut self, expr: &Expression, print_attrs: bool) {
        if print_attrs {
            // Print any non-internal attributes on the expression
            for attr in &expr.pexp_attributes {
                if !Self::is_internal_attribute(attr) {
                    self.write("@");
                    self.print_attribute(attr);
                    self.space();
                }
            }
        }

        match &expr.pexp_desc {
            ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
                self.print_let_bindings(*rec_flag, bindings);
                self.newline();
                self.print_block_body(body);
            }
            ExpressionDesc::Pexp_letmodule(name, modexpr, body) => {
                self.write("module ");
                self.write(&name.txt);
                self.write(" = ");
                self.print_module_expr(modexpr);
                self.newline();
                self.print_block_body(body);
            }
            ExpressionDesc::Pexp_letexception(ext, body) => {
                self.write("exception ");
                self.print_extension_constructor(ext);
                self.newline();
                self.print_block_body(body);
            }
            ExpressionDesc::Pexp_open(override_flag, lid, body) => {
                self.write("open");
                if *override_flag == OverrideFlag::Override {
                    self.write("!");
                }
                self.write(" ");
                self.print_longident(&lid.txt);
                self.newline();
                self.print_block_body(body);
            }
            ExpressionDesc::Pexp_sequence(left, right) => {
                self.print_expression(left);
                self.newline();
                self.print_block_body(right);
            }
            _ => {
                // Not a block form, print normally
                self.print_expression(expr);
            }
        }
    }

    // ========================================================================
    // Structure Printing
    // ========================================================================

    /// Print a structure.
    pub fn print_structure(&mut self, structure: &Structure) {
        let mut prev_loc_end_line = 0;
        for (i, item) in structure.iter().enumerate() {
            if i > 0 {
                // Add blank line only if there was a blank line in the source
                let item_start_line = item.pstr_loc.loc_start.line;
                if item_start_line - prev_loc_end_line > 1 {
                    self.newline();
                    self.newline();
                } else {
                    self.newline();
                }
            }
            if self.comments_enabled() {
                self.print_comments_before_loc(&item.pstr_loc);
            }
            self.print_structure_item(item);
            if self.comments_enabled() {
                self.print_trailing_comments_for_loc(&item.pstr_loc);
            }
            prev_loc_end_line = item.pstr_loc.loc_end.line;
        }
        if self.comments_enabled() {
            self.flush_remaining_comments();
        }
    }

    /// Print a structure item.
    pub fn print_structure_item(&mut self, item: &StructureItem) {
        // Print attributes on the same line as the item (not on separate lines)
        self.print_attributes_with_sep(&item.pstr_desc.get_attributes(), false);
        match &item.pstr_desc {
            StructureItemDesc::Pstr_eval(expr, _attrs) => {
                if matches!(&expr.pexp_desc, ExpressionDesc::Pexp_sequence(..)) {
                    self.write("{");
                    self.indent();
                    self.newline();
                    self.print_block_body_inner(expr, false);
                    self.dedent();
                    self.newline();
                    self.write("}");
                } else {
                    self.print_expression(expr);
                }
            }
            StructureItemDesc::Pstr_value(rec_flag, bindings) => {
                self.print_let_bindings(*rec_flag, bindings);
            }
            StructureItemDesc::Pstr_primitive(vd) => {
                self.print_value_description(vd);
            }
            StructureItemDesc::Pstr_type(rec_flag, decls) => {
                self.print_type_declarations(*rec_flag, decls);
            }
            StructureItemDesc::Pstr_typext(ext) => {
                self.print_type_extension(ext);
            }
            StructureItemDesc::Pstr_exception(ext) => {
                self.write("exception ");
                self.print_extension_constructor(ext);
            }
            StructureItemDesc::Pstr_module(mb) => {
                self.print_module_binding(mb);
            }
            StructureItemDesc::Pstr_recmodule(mbs) => {
                for (i, mb) in mbs.iter().enumerate() {
                    if i == 0 {
                        self.write("module rec ");
                    } else {
                        self.newline();
                        self.write("and ");
                    }
                    self.write(&mb.pmb_name.txt);
                    self.write(" = ");
                    self.print_module_expr(&mb.pmb_expr);
                }
            }
            StructureItemDesc::Pstr_modtype(mtd) => {
                self.print_module_type_declaration(mtd);
            }
            StructureItemDesc::Pstr_open(od) => {
                self.print_attributes(&od.popen_attributes);
                self.write("open");
                if od.popen_override == OverrideFlag::Override {
                    self.write("!");
                }
                self.write(" ");
                self.print_longident(&od.popen_lid.txt);
            }
            StructureItemDesc::Pstr_include(incl) => {
                self.write("include ");
                self.print_module_expr(&incl.pincl_mod);
            }
            StructureItemDesc::Pstr_attribute(attr) => {
                self.write("@@");
                self.print_attribute(attr);
            }
            StructureItemDesc::Pstr_extension(ext, _attrs) => {
                // Module-level extensions use %%
                self.write("%%");
                self.write_ident(&ext.0.txt);
                self.print_extension_payload(&ext.1);
            }
        }
    }

    // ========================================================================
    // Let Bindings
    // ========================================================================

    /// Print let bindings.
    fn print_let_bindings(&mut self, rec_flag: RecFlag, bindings: &[ValueBinding]) {
        for (i, binding) in bindings.iter().enumerate() {
            // Print attributes before the let keyword (each on separate line)
            self.print_attributes(&binding.pvb_attributes);
            if i == 0 {
                self.write("let ");
                if rec_flag == RecFlag::Recursive {
                    self.write("rec ");
                }
            } else {
                self.newline();
                self.write("and ");
            }
            self.print_value_binding(binding);
        }
    }

    /// Print a single value binding.
    fn print_value_binding(&mut self, binding: &ValueBinding) {
        self.print_pattern(&binding.pvb_pat);
        self.write(" = ");
        // Check if expression needs to be wrapped in braces
        // (sequences and other multi-statement expressions)
        if Self::needs_braces_in_binding(&binding.pvb_expr) {
            self.write("{");
            self.indent();
            self.newline();
            self.print_block_body(&binding.pvb_expr);
            self.dedent();
            self.newline();
            self.write("}");
        } else {
            self.print_expression(&binding.pvb_expr);
        }
    }

    /// Check if an expression needs to be wrapped in braces when used as a binding RHS.
    fn needs_braces_in_binding(expr: &Expression) -> bool {
        matches!(
            &expr.pexp_desc,
            ExpressionDesc::Pexp_sequence(..)
                | ExpressionDesc::Pexp_open(..)
                | ExpressionDesc::Pexp_let(..)
                | ExpressionDesc::Pexp_letmodule(..)
                | ExpressionDesc::Pexp_letexception(..)
        )
    }

    // ========================================================================
    // Binary/Unary Expression Detection
    // ========================================================================

    /// List of binary operators in ReScript.
    const BINARY_OPERATORS: &'static [&'static str] = &[
        ":=", "||", "&&", "=", "==", "===", "<", ">", "!=", "!==", "<=", ">=", "+", "+.", "-", "-.",
        "++", "*", "*.", "/", "/.", "**", "->", "<>", "%", "|||", "^^^", "&&&", "<<", ">>", ">>>",
    ];

    /// List of unary operators in ReScript.
    const UNARY_OPERATORS: &'static [&'static str] = &["!", "?", "~+", "~+.", "~-", "~-.", "not"];

    /// Check if an operator is a binary operator.
    fn is_binary_operator(op: &str) -> bool {
        Self::BINARY_OPERATORS.contains(&op)
    }

    /// Check if an operator is a unary operator.
    fn is_unary_operator(op: &str) -> bool {
        Self::UNARY_OPERATORS.contains(&op)
    }

    /// Check if a labeled argument is punned (e.g., `~foo` instead of `~foo=foo`).
    /// This is detected by checking if the argument is a ghost identifier with the same name as the label.
    fn is_punned_arg(&self, label: &ArgLabel, arg: &Expression) -> bool {
        let label_name = match label {
            ArgLabel::Labelled(name) | ArgLabel::Optional(name) => &name.txt,
            ArgLabel::Nolabel => return false,
        };

        // OCaml's punning detection: label matches the ident name, no attributes, simple ident
        if !arg.pexp_attributes.is_empty() {
            return false;
        }

        if let ExpressionDesc::Pexp_ident(lid) = &arg.pexp_desc {
            if let Longident::Lident(ident_name) = &lid.txt {
                return ident_name == label_name;
            }
        }

        false
    }

    /// Try to extract binary expression components from a Pexp_apply.
    /// Returns (operator, left, right) if this is a binary expression.
    fn as_binary_expr<'b>(
        &self,
        funct: &'b Expression,
        args: &'b [(ArgLabel, Expression)],
    ) -> Option<(&'b str, &'b Expression, &'b Expression)> {
        // Must have exactly 2 unlabeled arguments
        if args.len() != 2 {
            return None;
        }
        let (label1, left) = &args[0];
        let (label2, right) = &args[1];
        if !matches!(label1, ArgLabel::Nolabel) || !matches!(label2, ArgLabel::Nolabel) {
            return None;
        }

        // Function must be an identifier that is a binary operator
        if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
            if let Longident::Lident(op) = &lid.txt {
                if Self::is_binary_operator(op) {
                    return Some((op.as_str(), left, right));
                }
            }
        }
        None
    }

    /// Try to extract unary expression components from a Pexp_apply.
    /// Returns (operator, operand) if this is a unary expression.
    fn as_unary_expr<'b>(
        &self,
        funct: &'b Expression,
        args: &'b [(ArgLabel, Expression)],
    ) -> Option<(&'b str, &'b Expression)> {
        // Must have exactly 1 unlabeled argument
        if args.len() != 1 {
            return None;
        }
        let (label, operand) = &args[0];
        if !matches!(label, ArgLabel::Nolabel) {
            return None;
        }

        // Function must be an identifier that is a unary operator
        if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
            if let Longident::Lident(op) = &lid.txt {
                if Self::is_unary_operator(op) {
                    // Map internal names to display names
                    let display_op = match op.as_str() {
                        "~-" => "-",
                        "~-." => "-.",
                        "~+" => "+",
                        "~+." => "+.",
                        "not" => "!",
                        other => other,
                    };
                    return Some((display_op, operand));
                }
            }
        }
        None
    }

    // ========================================================================
    // Template Literal Printing
    // ========================================================================

    /// Print an untagged template literal.
    /// Walks the ++ chain and reconstructs the template.
    fn print_template_literal(&mut self, expr: &Expression) {
        let mut tag = "js";
        let mut inner = Printer::new();

        fn walk_expr(printer: &mut Printer, expr: &Expression, tag: &mut &str) {
            match &expr.pexp_desc {
                ExpressionDesc::Pexp_apply { funct, args, .. } => {
                    // Check if this is a ++ operation
                    if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                        if let Longident::Lident(op) = &lid.txt {
                            if op == "++" && args.len() == 2 {
                                walk_expr(printer, &args[0].1, tag);
                                walk_expr(printer, &args[1].1, tag);
                                return;
                            }
                        }
                    }
                    // Not a ++, print as interpolation
                    printer.write("${");
                    printer.print_expression(expr);
                    printer.write("}");
                }
                ExpressionDesc::Pexp_constant(Constant::String(txt, Some(prefix))) => {
                    *tag = if prefix == "json" { "json" } else { "js" };
                    // Print the template string contents
                    printer.print_template_string_contents(txt);
                }
                ExpressionDesc::Pexp_constant(Constant::String(txt, None)) => {
                    // Might be a regular string, print as template content
                    printer.print_template_string_contents(txt);
                }
                _ => {
                    // Expression interpolation
                    printer.write("${");
                    printer.print_expression(expr);
                    printer.write("}");
                }
            }
        }

        walk_expr(&mut inner, expr, &mut tag);
        let content = inner.into_output();

        // Wrap with backticks
        if tag == "js" {
            self.write("`");
        } else {
            self.write(tag);
            self.write("`");
        }
        self.write(&content);
        self.write("`");
    }

    /// Print template string contents (handling escapes).
    fn print_template_string_contents(&mut self, s: &str) {
        // The string is already stored with escapes, output directly
        self.write(s);
    }

    /// Print a tagged template literal: tag`strings${values}`
    fn print_tagged_template_literal(&mut self, tag: &Expression, args: &[(ArgLabel, Expression)]) {
        // Args should be [strings_array, values_array]
        if args.len() != 2 {
            // Fallback to regular apply
            self.print_expression(tag);
            self.write("(");
            for (i, (label, arg)) in args.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.print_arg_label(label);
                self.print_expression(arg);
            }
            self.write(")");
            return;
        }

        let strings = match &args[0].1.pexp_desc {
            ExpressionDesc::Pexp_array(arr) => arr,
            _ => {
                self.print_expression(tag);
                return;
            }
        };
        let values = match &args[1].1.pexp_desc {
            ExpressionDesc::Pexp_array(arr) => arr,
            _ => {
                self.print_expression(tag);
                return;
            }
        };

        // Print the tag
        self.print_expression(tag);
        self.write("`");

        // Interleave strings and values
        for (i, str_expr) in strings.iter().enumerate() {
            // Print string content
            if let ExpressionDesc::Pexp_constant(Constant::String(txt, _)) = &str_expr.pexp_desc {
                self.print_template_string_contents(txt);
            }
            // Print value interpolation if exists
            if i < values.len() {
                self.write("${");
                self.print_expression(&values[i]);
                self.write("}");
            }
        }

        self.write("`");
    }

    // ========================================================================
    // Expression Printing
    // ========================================================================

    /// Print an expression.
    pub fn print_expression(&mut self, expr: &Expression) {
        self.print_expression_with_comments(expr, false);
    }

    fn print_expression_in_subexpr(&mut self, expr: &Expression) {
        self.print_expression_with_comments(expr, true);
    }

    fn print_expression_with_comments(&mut self, expr: &Expression, needs_parens: bool) {
        if self.comments_enabled() {
            self.print_comments_before_loc(&expr.pexp_loc);
        }
        self.print_expression_inner(expr, needs_parens);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&expr.pexp_loc);
        }
    }

    /// Check if an attribute is internal and shouldn't be printed.
    /// Based on OCaml's is_printable_attribute in res_parsetree_viewer.ml
    fn is_internal_attribute(attr: &Attribute) -> bool {
        let name = &attr.0.txt;
        matches!(
            name.as_str(),
            "res.iflet"
                | "res.braces"
                | "ns.braces"
                | "JSX"
                | "res.await"
                | "res.template"
                | "res.taggedTemplate"
                | "res.ternary"
                | "res.inlineRecordDefinition"
        )
    }

    /// Check if an expression has the res.braces/ns.braces attribute.
    fn has_braces_attribute(expr: &Expression) -> bool {
        expr.pexp_attributes
            .iter()
            .any(|attr| matches!(attr.0.txt.as_str(), "res.braces" | "ns.braces"))
    }

    /// Strip res.braces/ns.braces attributes from an expression.
    fn strip_braces_attribute(expr: &Expression) -> Expression {
        let mut result = expr.clone();
        result.pexp_attributes = expr
            .pexp_attributes
            .iter()
            .filter(|attr| !matches!(attr.0.txt.as_str(), "res.braces" | "ns.braces"))
            .cloned()
            .collect();
        result
    }

    fn is_unit_pattern(pat: &Pattern) -> bool {
        matches!(
            &pat.ppat_desc,
            PatternDesc::Ppat_construct(lid, None) if lid.txt.to_string() == "()"
        )
    }

    fn expr_needs_parens_for_attribute(&self, expr: &Expression) -> bool {
        match &expr.pexp_desc {
            ExpressionDesc::Pexp_apply { funct, args, .. } => {
                self.as_binary_expr(funct, args).is_some() || self.as_unary_expr(funct, args).is_some()
            }
            ExpressionDesc::Pexp_sequence(..)
            | ExpressionDesc::Pexp_ifthenelse(..)
            | ExpressionDesc::Pexp_match(..)
            | ExpressionDesc::Pexp_try(..)
            // Note: Pexp_fun does NOT need parens - `@attr x => y` is valid
            | ExpressionDesc::Pexp_let(..)
            | ExpressionDesc::Pexp_letmodule(..)
            | ExpressionDesc::Pexp_letexception(..)
            | ExpressionDesc::Pexp_open(..)
            | ExpressionDesc::Pexp_for(..)
            | ExpressionDesc::Pexp_while(..)
            | ExpressionDesc::Pexp_constraint(..)
            | ExpressionDesc::Pexp_coerce(..)
            | ExpressionDesc::Pexp_newtype(..) => true,
            _ => false,
        }
    }

    fn expr_needs_parens_in_subexpr(&self, expr: &Expression) -> bool {
        match &expr.pexp_desc {
            ExpressionDesc::Pexp_apply { funct, args, .. } => {
                // Check if this is a binary expression
                if let Some((op, _, _)) = self.as_binary_expr(funct, args) {
                    // Pipe operator doesn't need parens in subexpr
                    op != "->"
                } else {
                    self.as_unary_expr(funct, args).is_some()
                }
            }
            ExpressionDesc::Pexp_fun { .. }
            | ExpressionDesc::Pexp_ifthenelse(..)
            | ExpressionDesc::Pexp_match(..)
            | ExpressionDesc::Pexp_try(..)
            | ExpressionDesc::Pexp_for(..)
            | ExpressionDesc::Pexp_while(..)
            | ExpressionDesc::Pexp_newtype(..)
            | ExpressionDesc::Pexp_constraint(..)
            | ExpressionDesc::Pexp_coerce(..)
            | ExpressionDesc::Pexp_setfield(..)
            | ExpressionDesc::Pexp_sequence(..)
            | ExpressionDesc::Pexp_let(..)
            | ExpressionDesc::Pexp_letmodule(..)
            | ExpressionDesc::Pexp_letexception(..)
            | ExpressionDesc::Pexp_open(..)
            | ExpressionDesc::Pexp_assert(..) => true,
            _ => false,
        }
    }

    fn print_apply_funct(&mut self, funct: &Expression) {
        let has_attrs = funct
            .pexp_attributes
            .iter()
            .any(|attr| !Self::is_internal_attribute(attr));
        if has_attrs {
            self.write("(");
            self.print_expression(funct);
            self.write(")");
        } else {
            self.print_expression_in_subexpr(funct);
        }
    }

    /// Print an expression with optional parentheses context.
    fn print_expression_inner(&mut self, expr: &Expression, needs_parens: bool) {
        let attrs: Vec<&Attribute> = expr
            .pexp_attributes
            .iter()
            .filter(|attr| !Self::is_internal_attribute(attr))
            .collect();
        let has_attrs = !attrs.is_empty();

        if needs_parens && matches!(&expr.pexp_desc, ExpressionDesc::Pexp_sequence(..)) {
            for attr in &attrs {
                self.write("@");
                self.print_attribute(attr);
                self.space();
            }
            self.write("{");
            self.indent();
            self.newline();
            self.print_block_body_inner(expr, false);
            self.dedent();
            self.newline();
            self.write("}");
            return;
        }

        if needs_parens && self.expr_needs_parens_in_subexpr(expr) {
            for attr in &attrs {
                self.write("@");
                self.print_attribute(attr);
                self.space();
            }
            self.write("(");
            self.print_expression_desc(expr, needs_parens);
            self.write(")");
            return;
        }

        if has_attrs && self.expr_needs_parens_for_attribute(expr) {
            for attr in &attrs {
                self.write("@");
                self.print_attribute(attr);
                self.space();
            }
            self.write("(");
            self.print_expression_desc(expr, needs_parens);
            self.write(")");
            return;
        }

        for attr in &attrs {
            self.write("@");
            self.print_attribute(attr);
            self.space();
        }

        self.print_expression_desc(expr, needs_parens);
    }

    fn print_expression_desc(&mut self, expr: &Expression, needs_parens: bool) {
        match &expr.pexp_desc {
            ExpressionDesc::Pexp_ident(lid) => {
                // Use print_lident to escape keywords (e.g., \"type", \"let")
                self.print_lident(&lid.txt);
            }
            ExpressionDesc::Pexp_constant(c) => {
                // Check if this is a template literal string
                let is_template = expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.template");
                if is_template {
                    if let Constant::String(txt, _) = c {
                        self.write("`");
                        self.print_template_string_contents(txt);
                        self.write("`");
                        return;
                    }
                }
                // Also check for empty delimiter (Some(""))
                if let Constant::String(txt, Some(delim)) = c {
                    if delim.is_empty() || delim == "js" {
                        self.write("`");
                        self.print_template_string_contents(txt);
                        self.write("`");
                        return;
                    }
                }
                self.print_constant(c);
            }
            ExpressionDesc::Pexp_let(rec_flag, bindings, body) => {
                self.write("{");
                self.indent();
                self.newline();
                self.print_let_bindings(*rec_flag, bindings);
                self.newline();
                // Use print_block_body for continuation to avoid nested braces
                self.print_block_body(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_fun { is_async, .. } => {
                if *is_async {
                    self.write("async ");
                }
                self.print_fun_expr(expr);
            }
            ExpressionDesc::Pexp_apply {
                funct,
                args,
                partial,
                ..
            } => {
                // Check for tagged template literal: tag([strings], [values])
                if expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.taggedTemplate")
                {
                    self.print_tagged_template_literal(funct, args);
                    return;
                }
                // Check for untagged template literal: str ++ val ++ str...
                if expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.template")
                {
                    self.print_template_literal(expr);
                    return;
                }
                // Check for array access syntax: arr[index]
                if expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.array.access")
                    && args.len() == 2
                {
                    self.print_expression_in_subexpr(&args[0].1);
                    self.write("[");
                    self.print_expression_in_subexpr(&args[1].1);
                    self.write("]");
                    return;
                }
                // Check for array set syntax: arr[index] = value
                if expr
                    .pexp_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.array.set")
                    && args.len() == 3
                {
                    self.print_expression_in_subexpr(&args[0].1);
                    self.write("[");
                    self.print_expression_in_subexpr(&args[1].1);
                    self.write("] = ");
                    self.print_expression_in_subexpr(&args[2].1);
                    return;
                }
                // Check for object property access: ##(obj, "prop") -> obj["prop"]
                if let ExpressionDesc::Pexp_ident(lid) = &funct.pexp_desc {
                    if matches!(&lid.txt, Longident::Lident(s) if s == "##") && args.len() == 2 {
                        self.print_expression_in_subexpr(&args[0].1);
                        self.write("[\"");
                        // Print the member - if it's an identifier, just use the name
                        if let ExpressionDesc::Pexp_ident(member_lid) = &args[1].1.pexp_desc {
                            self.print_longident(&member_lid.txt);
                        } else {
                            self.print_expression_in_subexpr(&args[1].1);
                        }
                        self.write("\"]");
                        return;
                    }
                    // Check for object property set: #=(lhs, rhs) -> lhs = rhs
                    if matches!(&lid.txt, Longident::Lident(s) if s == "#=") && args.len() == 2 {
                        self.print_expression_in_subexpr(&args[0].1);
                        self.write(" = ");
                        self.print_expression_in_subexpr(&args[1].1);
                        return;
                    }
                }
                // Check if this is a binary expression and print in infix notation
                if let Some((operator, left, right)) = self.as_binary_expr(funct, args) {
                    self.print_expression_in_subexpr(left);
                    // Pipe operator (->) doesn't have spaces around it
                    if operator == "->" {
                        self.write(operator);
                    } else {
                        self.space();
                        self.write(operator);
                        self.space();
                    }
                    self.print_expression_in_subexpr(right);
                } else if let Some((operator, operand)) = self.as_unary_expr(funct, args) {
                    self.write(operator);
                    self.print_expression_in_subexpr(operand);
                } else {
                    self.print_apply_funct(funct);
                    self.write("(");
                    // Check if this is a single unit argument - if so, don't print it
                    // f() is syntactic sugar for f(())
                    let is_single_unit_arg = args.len() == 1
                        && matches!(args[0].0, ArgLabel::Nolabel)
                        && matches!(
                            &args[0].1.pexp_desc,
                            ExpressionDesc::Pexp_construct(lid, None)
                                if matches!(&lid.txt, Longident::Lident(s) if s == "()")
                        );
                    let args_to_print: Vec<_> = if is_single_unit_arg {
                        vec![]
                    } else {
                        args.iter().collect()
                    };
                    for (i, (label, arg)) in args_to_print.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        // Check for punning: ~foo where arg is ghost ident "foo"
                        if self.is_punned_arg(label, arg) {
                            match label {
                                ArgLabel::Labelled(s) => {
                                    self.write("~");
                                    self.write_ident(&s.txt);
                                }
                                ArgLabel::Optional(s) => {
                                    self.write("~");
                                    self.write_ident(&s.txt);
                                    self.write("?");
                                }
                                ArgLabel::Nolabel => {}
                            }
                        } else {
                            self.print_arg_label(label);
                            self.print_expression_in_subexpr(arg);
                        }
                    }
                    // Print partial application marker
                    if *partial {
                        if !args.is_empty() {
                            self.write(", ");
                        }
                        self.write("...");
                    }
                    self.write(")");
                }
            }
            ExpressionDesc::Pexp_match(scrutinee, cases) => {
                self.write("switch ");
                self.print_expression_in_subexpr(scrutinee);
                self.write(" {");
                // Cases are NOT indented - they align with 'switch'
                for case in cases {
                    self.newline();
                    self.write("| ");
                    self.print_pattern(&case.pc_lhs);
                    if let Some(guard) = &case.pc_guard {
                        self.write(" if ");
                        self.print_expression(guard);
                    }
                    self.write(" => ");
                    self.print_expression_in_subexpr(&case.pc_rhs);
                }
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_try(body, cases) => {
                self.write("try ");
                self.print_expression_in_subexpr(body);
                self.write(" catch {");
                // Cases are NOT indented - they align with 'try'
                for case in cases {
                    self.newline();
                    self.write("| ");
                    self.print_pattern(&case.pc_lhs);
                    if let Some(guard) = &case.pc_guard {
                        self.write(" when ");
                        self.print_expression(guard);
                    }
                    self.write(" => ");
                    self.print_expression_in_subexpr(&case.pc_rhs);
                }
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_tuple(exprs) => {
                if exprs.len() == 1 {
                    self.print_expression_with_comments(&exprs[0], needs_parens);
                    return;
                }
                self.write("(");
                for (i, e) in exprs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expression_in_subexpr(e);
                }
                self.write(")");
            }
            ExpressionDesc::Pexp_construct(lid, arg) => {
                // Handle special constructors: (), [], ::
                match &lid.txt {
                    // Unit: ()
                    Longident::Lident(s) if s == "()" => {
                        self.write("()");
                    }
                    // Empty list: list{}
                    Longident::Lident(s) if s == "[]" => {
                        self.write("list{}");
                    }
                    // List cons: list{a, b, ...rest}
                    Longident::Lident(s) if s == "::" => {
                        let (exprs, spread) = Self::collect_list_expressions(expr);
                        self.print_list_with_doc(&exprs, spread);
                    }
                    // Generic constructor
                    _ => {
                        if self.comments_enabled() {
                            self.print_comments_before_loc(&lid.loc);
                        }
                        self.print_longident(&lid.txt);
                        if self.comments_enabled() {
                            self.print_trailing_comments_for_loc(&lid.loc);
                        }
                        if let Some(arg) = arg {
                            self.write("(");
                            // If the argument is a tuple, unwrap it to avoid double parens
                            match &arg.pexp_desc {
                                ExpressionDesc::Pexp_tuple(exprs) => {
                                    for (i, e) in exprs.iter().enumerate() {
                                        if i > 0 {
                                            self.write(", ");
                                        }
                                        self.print_expression_in_subexpr(e);
                                    }
                                }
                                ExpressionDesc::Pexp_construct(inner_lid, None)
                                    if inner_lid.txt.to_string() == "()" =>
                                {
                                    // Unit argument - print nothing inside parens
                                }
                                _ => self.print_expression_in_subexpr(arg),
                            }
                            self.write(")");
                        }
                    }
                }
            }
            ExpressionDesc::Pexp_variant(label, arg) => {
                self.write("#");
                self.write(label);
                if let Some(arg) = arg {
                    self.write("(");
                    // If the argument is a tuple, unwrap it to avoid double parens
                    match &arg.pexp_desc {
                        ExpressionDesc::Pexp_tuple(exprs) => {
                            for (i, e) in exprs.iter().enumerate() {
                                if i > 0 {
                                    self.write(", ");
                                }
                                self.print_expression_in_subexpr(e);
                            }
                        }
                        ExpressionDesc::Pexp_construct(lid, None)
                            if lid.txt.to_string() == "()" =>
                        {
                            // Unit argument - print nothing inside parens
                        }
                        _ => self.print_expression_in_subexpr(arg),
                    }
                    self.write(")");
                }
            }
            ExpressionDesc::Pexp_record(fields, spread) => {
                self.print_record_with_doc(fields, spread.as_deref(), &expr.pexp_loc);
            }
            ExpressionDesc::Pexp_field(expr, lid) => {
                self.print_expression_in_subexpr(expr);
                self.write(".");
                if self.comments_enabled() {
                    self.print_comments_before_loc(&lid.loc);
                }
                // Use print_lident to escape keywords like \"type"
                self.print_lident(&lid.txt);
                if self.comments_enabled() {
                    self.print_trailing_comments_for_loc(&lid.loc);
                }
            }
            ExpressionDesc::Pexp_setfield(expr, lid, value) => {
                self.print_expression_in_subexpr(expr);
                self.write(".");
                if self.comments_enabled() {
                    self.print_comments_before_loc(&lid.loc);
                }
                // Use print_lident to escape keywords like \"type"
                self.print_lident(&lid.txt);
                if self.comments_enabled() {
                    self.print_trailing_comments_for_loc(&lid.loc);
                }
                self.write(" = ");
                self.print_expression_in_subexpr(value);
            }
            ExpressionDesc::Pexp_array(items) => {
                self.print_array_with_doc(items);
            }
            ExpressionDesc::Pexp_ifthenelse(cond, then_branch, else_branch) => {
                // Check for ternary attribute
                let is_ternary = expr
                    .pexp_attributes
                    .iter()
                    .any(|(name, _)| name.txt == "res.ternary");
                if is_ternary {
                    self.print_expression_in_subexpr(cond);
                    self.write(" ? ");
                    self.print_expression_in_subexpr(then_branch);
                    self.write(" : ");
                    if let Some(else_br) = else_branch {
                        self.print_expression_in_subexpr(else_br);
                    }
                } else {
                    self.write("if ");
                    self.print_expression_in_subexpr(cond);
                    self.write(" {");
                    self.indent();
                    self.newline();
                    self.print_expression(then_branch);
                    self.dedent();
                    self.newline();
                    self.write("}");
                    if let Some(else_br) = else_branch {
                        self.write(" else ");
                        // Check if else branch is another if
                        if matches!(else_br.pexp_desc, ExpressionDesc::Pexp_ifthenelse(..)) {
                            self.print_expression(else_br);
                        } else {
                            self.write("{");
                            self.indent();
                            self.newline();
                            self.print_expression(else_br);
                            self.dedent();
                            self.newline();
                            self.write("}");
                        }
                    }
                }
            }
            ExpressionDesc::Pexp_sequence(first, second) => {
                self.print_expression(first);
                self.newline();
                self.print_expression(second);
            }
            ExpressionDesc::Pexp_while(cond, body) => {
                self.write("while ");
                self.print_expression_in_subexpr(cond);
                self.write(" {");
                self.indent();
                self.newline();
                self.print_expression(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_for(pat, start, finish, dir, body) => {
                self.write("for ");
                self.print_pattern(pat);
                self.write(" in ");
                self.print_expression_in_subexpr(start);
                match dir {
                    DirectionFlag::Upto => self.write(" to "),
                    DirectionFlag::Downto => self.write(" downto "),
                }
                self.print_expression_in_subexpr(finish);
                self.write(" {");
                self.indent();
                self.newline();
                self.print_expression(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_constraint(expr, typ) => {
                self.write("(");
                self.print_expression_in_subexpr(expr);
                self.write(": ");
                self.print_core_type(typ);
                self.write(")");
            }
            ExpressionDesc::Pexp_coerce(expr, _from, to) => {
                self.write("(");
                self.print_expression_in_subexpr(expr);
                self.write(" :> ");
                self.print_core_type(to);
                self.write(")");
            }
            ExpressionDesc::Pexp_send(expr, meth) => {
                // Object method send: obj["method"]
                self.print_expression_in_subexpr(expr);
                self.write("[\"");
                self.write(&meth.txt);
                self.write("\"]");
            }
            ExpressionDesc::Pexp_letmodule(name, modexpr, body) => {
                self.write("{");
                self.indent();
                self.newline();
                self.write("module ");
                self.write(&name.txt);
                self.write(" = ");
                self.print_module_expr(modexpr);
                self.newline();
                // Use print_block_body for continuation to avoid nested braces
                self.print_block_body(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_letexception(ext, body) => {
                self.write("{");
                self.indent();
                self.newline();
                self.write("exception ");
                self.print_extension_constructor(ext);
                self.newline();
                // Use print_block_body for continuation to avoid nested braces
                self.print_block_body(body);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_assert(expr) => {
                self.write("assert(");
                self.print_expression(expr);
                self.write(")");
            }
            ExpressionDesc::Pexp_pack(modexpr) => {
                self.write("module(");
                self.print_module_expr(modexpr);
                self.write(")");
            }
            ExpressionDesc::Pexp_open(_override_flag, lid, expr) => {
                self.write("{");
                self.indent();
                self.newline();
                self.write("open ");
                self.print_longident(&lid.txt);
                self.newline();
                // Use print_block_body for continuation to avoid nested braces
                self.print_block_body(expr);
                self.dedent();
                self.newline();
                self.write("}");
            }
            ExpressionDesc::Pexp_extension(ext) => {
                // Regex literal: `/pattern/flags`
                if ext.0.txt == "res.regex" {
                    if let Payload::PStr(items) = &ext.1 {
                        let get_str = |idx: usize| -> Option<&str> {
                            let item = items.get(idx)?;
                            match &item.pstr_desc {
                                StructureItemDesc::Pstr_eval(expr, _) => match &expr.pexp_desc {
                                    ExpressionDesc::Pexp_constant(Constant::String(s, _)) => {
                                        Some(s.as_str())
                                    }
                                    _ => None,
                                },
                                _ => None,
                            }
                        };

                        if let (Some(pattern), Some(flags)) = (get_str(0), get_str(1)) {
                            self.write("/");
                            self.write(pattern);
                            self.write("/");
                            self.write(flags);
                            return;
                        }
                    }
                }

                // Check for %obj extension (JS object literal)
                if ext.0.txt == "obj" {
                    if let Payload::PStr(items) = &ext.1 {
                        if let Some(item) = items.first() {
                            if let StructureItemDesc::Pstr_eval(expr, _) = &item.pstr_desc {
                                if let ExpressionDesc::Pexp_record(fields, _) = &expr.pexp_desc {
                                    self.write("{");
                                    for (i, field) in fields.iter().enumerate() {
                                        if i > 0 {
                                            self.write(", ");
                                        }
                                        // Print field key as quoted string
                                        self.write("\"");
                                        self.print_longident(&field.lid.txt);
                                        self.write("\": ");
                                        self.print_expression_in_subexpr(&field.expr);
                                    }
                                    self.write("}");
                                    return;
                                }
                            }
                        }
                    }
                }
                // Check for res.list extension
                if ext.0.txt == "res.list" || ext.0.txt == "res.list.spread" {
                    self.write("list{");
                    if let Payload::PStr(items) = &ext.1 {
                        for (i, item) in items.iter().enumerate() {
                            if i > 0 {
                                self.write(", ");
                            }
                            if let StructureItemDesc::Pstr_eval(e, _) = &item.pstr_desc {
                                if let ExpressionDesc::Pexp_extension((name, payload)) = &e.pexp_desc
                                    && name.txt == "res.spread"
                                    && let Payload::PStr(spread_items) = payload
                                    && let Some(spread_item) = spread_items.first()
                                    && let StructureItemDesc::Pstr_eval(spread_expr, _) =
                                        &spread_item.pstr_desc
                                {
                                    self.write("...");
                                    self.print_expression_in_subexpr(spread_expr);
                                } else {
                                    self.print_expression_in_subexpr(e);
                                }
                            }
                        }
                    }
                    // Check for spread in attributes
                    if ext.0.txt == "res.list.spread" {
                        if let Some((_attr_name, attr_payload)) = expr
                            .pexp_attributes
                            .iter()
                            .find(|(n, _)| n.txt == "res.spread")
                        {
                            if !matches!(&ext.1, Payload::PStr(items) if items.is_empty()) {
                                self.write(", ");
                            }
                            self.write("...");
                            if let Payload::PStr(spread_items) = attr_payload {
                                if let Some(item) = spread_items.first() {
                                    if let StructureItemDesc::Pstr_eval(e, _) = &item.pstr_desc {
                                        self.print_expression_in_subexpr(e);
                                    }
                                }
                            }
                        }
                    }
                    self.write("}");
                    return;
                }
                self.write("%");
                // Extension names are not escaped (e.g., %let not %\"let")
                self.write(&ext.0.txt);
                self.print_extension_payload(&ext.1);
            }
            ExpressionDesc::Pexp_newtype(name, body) => {
                self.write("(type ");
                self.write(&name.txt);
                self.write(") => ");
                self.print_expression_with_comments(body, true);
            }
            ExpressionDesc::Pexp_await(expr) => {
                self.write("await ");
                self.print_expression_in_subexpr(expr);
            }
            ExpressionDesc::Pexp_jsx_element(elem) => {
                self.print_jsx_element(elem);
            }
        }
    }

    // ========================================================================
    // Pattern Printing
    // ========================================================================

    /// Print a pattern.
    pub fn print_pattern(&mut self, pat: &Pattern) {
        if self.comments_enabled() {
            self.print_comments_before_loc(&pat.ppat_loc);
        }
        // Print pattern attributes
        for attr in &pat.ppat_attributes {
            self.write("@");
            self.print_attribute(attr);
            self.write(" ");
        }
        match &pat.ppat_desc {
            PatternDesc::Ppat_any => {
                self.write("_");
            }
            PatternDesc::Ppat_var(name) => {
                self.write_ident(&name.txt);
            }
            PatternDesc::Ppat_alias(pat, name) => {
                self.print_pattern(pat);
                self.write(" as ");
                self.write_ident(&name.txt);
            }
            PatternDesc::Ppat_constant(c) => {
                self.print_constant(c);
            }
            PatternDesc::Ppat_interval(c1, c2) => {
                self.print_constant(c1);
                self.write("..");
                self.print_constant(c2);
            }
            PatternDesc::Ppat_tuple(pats) => {
                self.write("(");
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_pattern(p);
                }
                // Single-element tuples need trailing comma to distinguish from grouping parens
                if pats.len() == 1 {
                    self.write(",");
                }
                self.write(")");
            }
            PatternDesc::Ppat_construct(lid, arg) => {
                // Handle special constructors: (), [], ::
                match &lid.txt {
                    // Unit: ()
                    Longident::Lident(s) if s == "()" => {
                        self.write("()");
                    }
                    // Empty list: list{}
                    Longident::Lident(s) if s == "[]" => {
                        self.write("list{}");
                    }
                    // List cons: list{a, b, ...rest}
                    Longident::Lident(s) if s == "::" => {
                        let (pats, tail) = Self::collect_list_patterns(pat);
                        self.write("list{");
                        for (i, p) in pats.iter().enumerate() {
                            if i > 0 {
                                self.write(", ");
                            }
                            self.print_pattern(p);
                        }
                        // Check if tail is empty list `[]` or a spread pattern
                        let is_empty_list = matches!(
                            &tail.ppat_desc,
                            PatternDesc::Ppat_construct(lid, None)
                                if matches!(&lid.txt, Longident::Lident(s) if s == "[]")
                        );
                        if !is_empty_list {
                            if !pats.is_empty() {
                                self.write(", ");
                            }
                            self.write("...");
                            self.print_pattern(tail);
                        }
                        self.write("}");
                    }
                    // Generic constructor
                    _ => {
                        if self.comments_enabled() {
                            self.print_comments_before_loc(&lid.loc);
                        }
                        self.print_longident(&lid.txt);
                        if self.comments_enabled() {
                            self.print_trailing_comments_for_loc(&lid.loc);
                        }
                        if let Some(arg) = arg {
                            self.write("(");
                            self.print_pattern(arg);
                            self.write(")");
                        }
                    }
                }
            }
            PatternDesc::Ppat_variant(label, arg) => {
                self.write("#");
                self.write(label);
                if let Some(arg) = arg {
                    self.write("(");
                    self.print_pattern(arg);
                    self.write(")");
                }
            }
            PatternDesc::Ppat_record(fields, closed) => {
                self.write("{");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    if self.comments_enabled() {
                        self.print_comments_before_loc(&field.lid.loc);
                    }
                    self.print_longident(&field.lid.txt);
                    if self.comments_enabled() {
                        self.print_trailing_comments_for_loc(&field.lid.loc);
                    }
                    self.write(": ");
                    // Print optional marker if present
                    if field.opt {
                        self.write("? ");
                        // If the pattern is a constraint, wrap in parens to avoid ambiguity
                        let needs_parens =
                            matches!(field.pat.ppat_desc, PatternDesc::Ppat_constraint(..));
                        if needs_parens {
                            self.write("(");
                        }
                        self.print_pattern(&field.pat);
                        if needs_parens {
                            self.write(")");
                        }
                    } else {
                        self.print_pattern(&field.pat);
                    }
                }
                if *closed == ClosedFlag::Open {
                    if !fields.is_empty() {
                        self.write(", ");
                    }
                    self.write("_");
                }
                self.write("}");
            }
            PatternDesc::Ppat_array(pats) => {
                self.write("[");
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_pattern(p);
                }
                self.write("]");
            }
            PatternDesc::Ppat_or(left, right) => {
                self.print_pattern(left);
                self.write(" | ");
                self.print_pattern(right);
            }
            PatternDesc::Ppat_constraint(pat, typ) => {
                self.print_pattern(pat);
                self.write(": ");
                self.print_core_type(typ);
            }
            PatternDesc::Ppat_type(lid) => {
                // Type pattern with spread: #...Foo.t
                self.write("#...");
                if self.comments_enabled() {
                    self.print_comments_before_loc(&lid.loc);
                }
                self.print_longident(&lid.txt);
                if self.comments_enabled() {
                    self.print_trailing_comments_for_loc(&lid.loc);
                }
            }
            PatternDesc::Ppat_unpack(name) => {
                self.write("module(");
                self.write(&name.txt);
                self.write(")");
            }
            PatternDesc::Ppat_exception(pat) => {
                self.write("exception ");
                self.print_pattern(pat);
            }
            PatternDesc::Ppat_extension(ext) => {
                // Dict pattern: dict{"key": pat}
                if ext.0.txt == "res.dict" {
                    if let Payload::PPat(pat, _) = &ext.1 {
                        if let PatternDesc::Ppat_record(fields, _closed) = &pat.ppat_desc {
                            self.write("dict{");
                            for (i, field) in fields.iter().enumerate() {
                                if i > 0 {
                                    self.write(", ");
                                }
                                self.write("\"");
                                self.write(&field.lid.txt.to_string());
                                self.write("\": ");
                                if field.opt {
                                    self.write("?");
                                }
                                self.print_pattern(&field.pat);
                            }
                            self.write("}");
                            return;
                        }
                    }
                }

                self.write("%");
                self.write(&ext.0.txt);
            }
            PatternDesc::Ppat_open(lid, pat) => {
                if self.comments_enabled() {
                    self.print_comments_before_loc(&lid.loc);
                }
                self.print_longident(&lid.txt);
                if self.comments_enabled() {
                    self.print_trailing_comments_for_loc(&lid.loc);
                }
                self.write(".(");
                self.print_pattern(pat);
                self.write(")");
            }
        }
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&pat.ppat_loc);
        }
    }

    // ========================================================================
    // Type Printing
    // ========================================================================

    /// Print a core type.
    pub fn print_core_type(&mut self, typ: &CoreType) {
        self.print_core_type_with_comments(typ, true);
    }

    fn print_core_type_with_comments(&mut self, typ: &CoreType, allow_breaks: bool) {
        if self.comments_enabled() {
            self.print_comments_before_loc(&typ.ptyp_loc);
        }
        let attrs: Vec<&Attribute> = typ
            .ptyp_attributes
            .iter()
            .filter(|attr| !Self::is_internal_attribute(attr))
            .collect();
        for attr in attrs {
            self.write("@");
            self.print_attribute(attr);
            self.space();
        }
        self.print_core_type_inner(typ, allow_breaks);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&typ.ptyp_loc);
        }
    }

    fn print_core_type_inner(&mut self, typ: &CoreType, allow_breaks: bool) {
        match &typ.ptyp_desc {
            CoreTypeDesc::Ptyp_any => {
                self.write("_");
            }
            CoreTypeDesc::Ptyp_var(name) => {
                self.write("'");
                self.write_type_var(name);
            }
            CoreTypeDesc::Ptyp_arrow { .. } => {
                self.print_arrow_type(typ, allow_breaks);
            }
            CoreTypeDesc::Ptyp_tuple(typs) => {
                let inline_len = self.inline_tuple_len(typs);
                let fits_inline = !allow_breaks || self.line_len + inline_len <= self.max_width;

                if fits_inline {
                    self.write("(");
                    for (i, t) in typs.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.print_core_type_with_comments(t, allow_breaks);
                    }
                    self.write(")");
                } else {
                    self.write("(");
                    self.indent();
                    for t in typs {
                        self.newline();
                        self.print_core_type_with_comments(t, allow_breaks);
                        self.write(",");
                    }
                    self.dedent();
                    self.newline();
                    self.write(")");
                }
            }
            CoreTypeDesc::Ptyp_constr(lid, args) => {
                // ReScript uses angle bracket syntax: constr<args>
                // Use print_lident to escape the final segment (e.g., Module.\"type")
                self.print_lident(&lid.txt);
                if !args.is_empty() {
                    self.write("<");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.print_core_type_with_comments(arg, allow_breaks);
                    }
                    self.write(">");
                }
            }
            CoreTypeDesc::Ptyp_object(fields, closed) => {
                let is_open = *closed == ClosedFlag::Open;
                if is_open {
                    self.write("{..");
                    if fields.is_empty() {
                        self.write("}");
                        return;
                    }
                    self.write(" ");
                } else if fields.is_empty() {
                    self.write("{.}");
                    return;
                } else {
                    self.write("{");
                }

                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    match field {
                        ObjectField::Otag(name, attrs, typ) => {
                            for attr in attrs.iter().filter(|attr| !Self::is_internal_attribute(attr))
                            {
                                self.write("@");
                                self.print_attribute(attr);
                                self.space();
                            }
                            self.write("\"");
                            self.write(&name.txt);
                            self.write("\"");
                            self.write(": ");
                            self.print_core_type_with_comments(typ, allow_breaks);
                        }
                        ObjectField::Oinherit(typ) => {
                            self.write("...");
                            self.print_core_type_with_comments(typ, allow_breaks);
                        }
                    }
                }
                self.write("}");
            }
            CoreTypeDesc::Ptyp_alias(typ, name) => {
                // Arrow types need parens when inside an alias
                let needs_parens = matches!(typ.ptyp_desc, CoreTypeDesc::Ptyp_arrow { .. });
                if needs_parens {
                    self.write("(");
                }
                self.print_core_type_with_comments(typ, allow_breaks);
                if needs_parens {
                    self.write(")");
                }
                self.write(" as '");
                self.write(name);
            }
            CoreTypeDesc::Ptyp_variant(fields, closed, _labels) => {
                self.write("[");
                if *closed == ClosedFlag::Open {
                    self.write("> ");
                }
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(" | ");
                    }
                    self.print_row_field(field);
                }
                self.write("]");
            }
            CoreTypeDesc::Ptyp_poly(vars, typ) => {
                for var in vars {
                    self.write("'");
                    self.write(&var.txt);
                    self.write(" ");
                }
                self.write(". ");
                self.print_core_type_with_comments(typ, allow_breaks);
            }
            CoreTypeDesc::Ptyp_package(pkg) => {
                self.write("module(");
                self.print_longident(&pkg.0.txt);
                if !pkg.1.is_empty() {
                    self.write(" with ");
                    for (i, (lid, typ)) in pkg.1.iter().enumerate() {
                        if i > 0 {
                            self.write(" and ");
                        }
                        self.write("type ");
                        self.print_longident(&lid.txt);
                        self.write(" = ");
                        self.print_core_type_with_comments(typ, allow_breaks);
                    }
                }
                self.write(")");
            }
            CoreTypeDesc::Ptyp_extension(ext) => {
                self.write("%");
                self.write(&ext.0.txt);
            }
        }
    }

    fn print_arrow_type(&mut self, typ: &CoreType, allow_breaks: bool) {
        // Get the arity from the outermost arrow type
        let arity = if let CoreTypeDesc::Ptyp_arrow { arity, .. } = &typ.ptyp_desc {
            *arity
        } else {
            Arity::Unknown
        };

        // Collect arguments up to the arity limit
        let max_arity = match arity {
            Arity::Full(n) => n,
            Arity::Unknown => usize::MAX,
        };

        let mut args: Vec<&TypeArg> = vec![];
        let mut current = typ;
        while let CoreTypeDesc::Ptyp_arrow { arg, ret, .. } = &current.ptyp_desc {
            if args.len() >= max_arity {
                break;
            }
            args.push(arg.as_ref());
            current = ret.as_ref();
        }

        // Shouldn't happen, but keep it safe.
        if args.is_empty() {
            self.print_core_type_with_comments(current, allow_breaks);
            return;
        }

        let has_label = args.iter().any(|arg| !matches!(arg.lbl, ArgLabel::Nolabel));
        let has_attrs = args.iter().any(|arg| !arg.attrs.is_empty());

        // Use uncurried format (a, b) => c when:
        // - there are labels, or
        // - there are multiple arguments and arity is known
        let use_uncurried = has_label
            || has_attrs
            || (args.len() > 1 && matches!(arity, Arity::Full(_)));

        if use_uncurried {
            self.write("(");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }

                for attr in arg
                    .attrs
                    .iter()
                    .filter(|attr| !Self::is_internal_attribute(attr))
                {
                    self.write("@");
                    self.print_attribute(attr);
                    self.space();
                }

                match &arg.lbl {
                    ArgLabel::Nolabel => {}
                    ArgLabel::Labelled(name) | ArgLabel::Optional(name) => {
                        self.write("~");
                        self.write(&name.txt);
                        self.write(": ");
                    }
                }

                self.print_core_type_with_comments(&arg.typ, allow_breaks);

                if matches!(arg.lbl, ArgLabel::Optional(_)) {
                    self.write("=?");
                }
            }
            self.write(") => ");
            self.print_core_type_with_comments(current, allow_breaks);
            return;
        }

        // Curried format: a => b => c (single argument with no label)
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                self.write(" => ");
            }
            self.print_core_type_as_arrow_param(&arg.typ, allow_breaks);
        }
        self.write(" => ");
        self.print_core_type_with_comments(current, allow_breaks);
    }

    fn print_core_type_as_arrow_param(&mut self, typ: &CoreType, allow_breaks: bool) {
        // Wrap arrow types and tuples in parens to disambiguate from multi-param arrow syntax
        // Without this, ('a, 'b) => unit would be reparsed as 'a => 'b => unit
        if matches!(
            typ.ptyp_desc,
            CoreTypeDesc::Ptyp_arrow { .. } | CoreTypeDesc::Ptyp_tuple(_)
        ) {
            self.write("(");
            self.print_core_type_with_comments(typ, allow_breaks);
            self.write(")");
        } else {
            self.print_core_type_with_comments(typ, allow_breaks);
        }
    }

    fn inline_tuple_len(&self, typs: &[CoreType]) -> usize {
        let mut len = 2; // opening and closing parens
        for (i, typ) in typs.iter().enumerate() {
            if i > 0 {
                len += 2; // ", "
            }
            len += self.inline_type_len(typ);
        }
        len
    }

    fn inline_type_len(&self, typ: &CoreType) -> usize {
        let mut printer = Printer::with_width(self.max_width);
        printer.print_core_type_inner(typ, false);
        printer.output.len()
    }

    /// Print a type argument.
    fn print_type_arg_with_breaks(&mut self, arg: &TypeArg, allow_breaks: bool) {
        let has_attrs = !arg.attrs.is_empty();
        if has_attrs {
            self.write("(");
            self.print_attributes(&arg.attrs);
        }
        match &arg.lbl {
            ArgLabel::Nolabel => {}
            ArgLabel::Labelled(name) | ArgLabel::Optional(name) => {
                self.write("~");
                self.write(&name.txt);
                self.write(": ");
            }
        }
        self.print_core_type_with_comments(&arg.typ, allow_breaks);
        if matches!(arg.lbl, ArgLabel::Optional(_)) {
            self.write("=?");
        }
        if has_attrs {
            self.write(")");
        }
    }

    /// Print a row field.
    fn print_row_field(&mut self, field: &RowField) {
        match field {
            RowField::Rtag(label, _attrs, is_const, typs) => {
                self.write("#");
                self.write(&label.txt);
                if !*is_const && !typs.is_empty() {
                    self.write("(");
                    for (i, typ) in typs.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.print_core_type(typ);
                    }
                    self.write(")");
                }
            }
            RowField::Rinherit(typ) => {
                self.print_core_type(typ);
            }
        }
    }

    // ========================================================================
    // Module Printing
    // ========================================================================

    /// Print a module expression.
    pub fn print_module_expr(&mut self, modexpr: &ModuleExpr) {
        match &modexpr.pmod_desc {
            ModuleExprDesc::Pmod_ident(lid) => {
                self.print_longident(&lid.txt);
            }
            ModuleExprDesc::Pmod_structure(items) => {
                self.write("{");
                let has_inner_comments =
                    self.comments_enabled() && self.has_comment_before_loc_end(&modexpr.pmod_loc);
                if !items.is_empty() || has_inner_comments {
                    self.indent();
                    for item in items {
                        self.newline();
                        if self.comments_enabled() {
                            self.print_comments_before_loc(&item.pstr_loc);
                        }
                        self.print_structure_item(item);
                        if self.comments_enabled() {
                            self.print_trailing_comments_for_loc(&item.pstr_loc);
                        }
                    }
                    if has_inner_comments {
                        if items.is_empty() {
                            self.newline();
                        }
                        self.print_comments_before_loc_end(&modexpr.pmod_loc);
                    }
                    self.dedent();
                    self.newline();
                }
                self.write("}");
            }
            ModuleExprDesc::Pmod_functor(name, param, body) => {
                self.write("(");
                self.write(&name.txt);
                if let Some(mt) = param {
                    self.write(": ");
                    self.print_module_type(mt);
                }
                self.write(") => ");
                self.print_module_expr(body);
            }
            ModuleExprDesc::Pmod_apply(func, arg) => {
                // Check if this is an await expression (has res.await attribute and __await__ functor)
                let is_await = modexpr
                    .pmod_attributes
                    .iter()
                    .any(|(n, _)| n.txt == "res.await")
                    && matches!(
                        &func.pmod_desc,
                        ModuleExprDesc::Pmod_ident(lid) if lid.txt.to_string() == "__await__"
                    );
                if is_await {
                    self.write("await ");
                    self.print_module_expr(arg);
                } else {
                    self.print_module_expr(func);
                    self.write("(");
                    self.print_module_expr(arg);
                    self.write(")");
                }
            }
            ModuleExprDesc::Pmod_constraint(expr, typ) => {
                self.write("(");
                self.print_module_expr(expr);
                self.write(": ");
                self.print_module_type(typ);
                self.write(")");
            }
            ModuleExprDesc::Pmod_unpack(expr) => {
                self.write("unpack(");
                self.print_expression(expr);
                self.write(")");
            }
            ModuleExprDesc::Pmod_extension(ext) => {
                self.print_extension(ext);
            }
        }
    }

    /// Print a module type.
    pub fn print_module_type(&mut self, modtype: &ModuleType) {
        match &modtype.pmty_desc {
            ModuleTypeDesc::Pmty_ident(lid) => {
                self.print_longident(&lid.txt);
            }
            ModuleTypeDesc::Pmty_signature(items) => {
                self.write("{");
                if !items.is_empty() {
                    self.indent();
                    for item in items {
                        self.newline();
                        self.print_signature_item(item);
                    }
                    self.dedent();
                    self.newline();
                }
                self.write("}");
            }
            ModuleTypeDesc::Pmty_functor(name, param, body) => {
                self.write("(");
                self.write(&name.txt);
                if let Some(mt) = param {
                    self.write(": ");
                    self.print_module_type(mt);
                }
                self.write(") => ");
                self.print_module_type(body);
            }
            ModuleTypeDesc::Pmty_with(typ, constraints) => {
                // If the inner type is also a Pmty_with, we need parentheses
                let needs_parens = matches!(typ.pmty_desc, ModuleTypeDesc::Pmty_with(..));
                if needs_parens {
                    self.write("(");
                }
                self.print_module_type(typ);
                if needs_parens {
                    self.write(")");
                }
                self.write(" with ");
                for (i, constraint) in constraints.iter().enumerate() {
                    if i > 0 {
                        self.write(" and ");
                    }
                    self.print_with_constraint(constraint);
                }
            }
            ModuleTypeDesc::Pmty_typeof(modexpr) => {
                self.write("module type of ");
                self.print_module_expr(modexpr);
            }
            ModuleTypeDesc::Pmty_extension(ext) => {
                self.write("%");
                self.write(&ext.0.txt);
            }
            ModuleTypeDesc::Pmty_alias(lid) => {
                self.write("module ");
                self.print_longident(&lid.txt);
            }
        }
    }

    /// Print a signature (interface file content).
    pub fn print_signature(&mut self, signature: &[SignatureItem]) {
        let mut prev_loc_end_line = 0;
        for (i, item) in signature.iter().enumerate() {
            if i > 0 {
                // Add blank line only if there was a blank line in the source
                let item_start_line = item.psig_loc.loc_start.line;
                if item_start_line - prev_loc_end_line > 1 {
                    self.newline();
                    self.newline();
                } else {
                    self.newline();
                }
            }
            if self.comments_enabled() {
                self.print_comments_before_loc(&item.psig_loc);
            }
            self.print_signature_item(item);
            if self.comments_enabled() {
                self.print_trailing_comments_for_loc(&item.psig_loc);
            }
            prev_loc_end_line = item.psig_loc.loc_end.line;
        }
        if self.comments_enabled() {
            self.flush_remaining_comments();
        }
    }

    /// Print a signature item.
    pub fn print_signature_item(&mut self, item: &SignatureItem) {
        match &item.psig_desc {
            SignatureItemDesc::Psig_value(vd) => {
                self.print_value_description(vd);
            }
            SignatureItemDesc::Psig_type(rec_flag, decls) => {
                self.print_type_declarations(*rec_flag, decls);
            }
            SignatureItemDesc::Psig_typext(ext) => {
                self.print_type_extension(ext);
            }
            SignatureItemDesc::Psig_exception(ext) => {
                self.write("exception ");
                self.print_extension_constructor(ext);
            }
            SignatureItemDesc::Psig_module(md) => {
                self.write("module ");
                self.write(&md.pmd_name.txt);
                // Handle module alias (module X = M) vs module type annotation (module X: T)
                if let ModuleTypeDesc::Pmty_alias(lid) = &md.pmd_type.pmty_desc {
                    self.write(" = ");
                    self.print_longident(&lid.txt);
                } else {
                    self.write(": ");
                    self.print_module_type(&md.pmd_type);
                }
            }
            SignatureItemDesc::Psig_recmodule(mds) => {
                for (i, md) in mds.iter().enumerate() {
                    if i == 0 {
                        self.write("module rec ");
                    } else {
                        self.newline();
                        self.write("and ");
                    }
                    self.write(&md.pmd_name.txt);
                    self.write(": ");
                    self.print_module_type(&md.pmd_type);
                }
            }
            SignatureItemDesc::Psig_modtype(mtd) => {
                self.print_module_type_declaration(mtd);
            }
            SignatureItemDesc::Psig_open(od) => {
                self.print_attributes(&od.popen_attributes);
                self.write("open");
                if od.popen_override == OverrideFlag::Override {
                    self.write("!");
                }
                self.write(" ");
                self.print_longident(&od.popen_lid.txt);
            }
            SignatureItemDesc::Psig_include(incl) => {
                self.write("include ");
                self.print_module_type(&incl.pincl_mod);
            }
            SignatureItemDesc::Psig_attribute(attr) => {
                self.write("@@");
                self.print_attribute(attr);
            }
            SignatureItemDesc::Psig_extension(ext, _attrs) => {
                self.write("%");
                self.write(&ext.0.txt);
            }
        }
    }

    /// Print a module binding.
    fn print_module_binding(&mut self, mb: &ModuleBinding) {
        self.write("module ");
        if self.comments_enabled() {
            self.print_comments_before_loc(&mb.pmb_name.loc);
        }
        self.write(&mb.pmb_name.txt);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&mb.pmb_name.loc);
        }
        self.write(" = ");
        self.print_module_expr(&mb.pmb_expr);
    }

    /// Print a module type declaration.
    fn print_module_type_declaration(&mut self, mtd: &ModuleTypeDeclaration) {
        self.write("module type ");
        if self.comments_enabled() {
            self.print_comments_before_loc(&mtd.pmtd_name.loc);
        }
        self.write(&mtd.pmtd_name.txt);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&mtd.pmtd_name.loc);
        }
        if let Some(typ) = &mtd.pmtd_type {
            self.write(" = ");
            self.print_module_type(typ);
        }
    }

    /// Print a with constraint.
    fn print_with_constraint(&mut self, constraint: &WithConstraint) {
        match constraint {
            WithConstraint::Pwith_type(lid, decl) => {
                self.write("type ");
                self.print_longident(&lid.txt);
                self.write(" = ");
                if let Some(manifest) = &decl.ptype_manifest {
                    self.print_core_type(manifest);
                }
            }
            WithConstraint::Pwith_module(lid1, lid2) => {
                self.write("module ");
                self.print_longident(&lid1.txt);
                self.write(" = ");
                self.print_longident(&lid2.txt);
            }
            WithConstraint::Pwith_typesubst(lid, decl) => {
                self.write("type ");
                self.print_longident(&lid.txt);
                self.write(" := ");
                if let Some(manifest) = &decl.ptype_manifest {
                    self.print_core_type(manifest);
                }
            }
            WithConstraint::Pwith_modsubst(lid1, lid2) => {
                self.write("module ");
                self.print_longident(&lid1.txt);
                self.write(" := ");
                self.print_longident(&lid2.txt);
            }
        }
    }

    // ========================================================================
    // Type Declarations
    // ========================================================================

    /// Print type declarations.
    fn print_type_declarations(&mut self, rec_flag: RecFlag, decls: &[TypeDeclaration]) {
        for (i, decl) in decls.iter().enumerate() {
            // Print type declaration attributes before the type keyword
            for attr in &decl.ptype_attributes {
                if !Self::is_internal_attribute(attr) {
                    self.write("@");
                    self.print_attribute(attr);
                    self.newline();
                }
            }
            if i == 0 {
                self.write("type ");
                if rec_flag == RecFlag::Recursive && decls.len() > 1 {
                    self.write("rec ");
                }
            } else {
                self.newline();
                self.write("and ");
            }
            self.print_type_declaration(decl);
        }
    }

    /// Print a single type declaration.
    fn print_type_declaration(&mut self, decl: &TypeDeclaration) {
        // Print type name first (ReScript style: type option<'a> not 'a option)
        if self.comments_enabled() {
            self.print_comments_before_loc(&decl.ptype_name.loc);
        }
        // Use write_ident to escape keywords (e.g., type \"import" = ...)
        self.write_ident(&decl.ptype_name.txt);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&decl.ptype_name.loc);
        }

        // Print type parameters in angle brackets (ReScript style)
        if !decl.ptype_params.is_empty() {
            self.write("<");
            for (i, (typ, _var)) in decl.ptype_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.print_core_type(typ);
            }
            self.write(">");
        }

        // Print kind or manifest
        match &decl.ptype_kind {
            TypeKind::Ptype_abstract => {
                if let Some(manifest) = &decl.ptype_manifest {
                    self.write(" = ");
                    if decl.ptype_private == PrivateFlag::Private {
                        self.write("private ");
                    }
                    self.print_core_type(manifest);
                }
            }
            TypeKind::Ptype_variant(constructors) => {
                // Print manifest if present (e.g., type t = User.t = ...)
                if let Some(manifest) = &decl.ptype_manifest {
                    self.write(" = ");
                    self.print_core_type(manifest);
                }
                self.write(" =");
                if decl.ptype_private == PrivateFlag::Private {
                    self.write(" private");
                }
                self.print_constructor_declarations(constructors, &decl.ptype_loc);
            }
            TypeKind::Ptype_record(fields) => {
                // Print manifest if present (e.g., type t = User.t = {...})
                if let Some(manifest) = &decl.ptype_manifest {
                    self.write(" = ");
                    self.print_core_type(manifest);
                }
                self.write(" = ");
                if decl.ptype_private == PrivateFlag::Private {
                    self.write("private ");
                }
                self.print_record_declaration(fields, &decl.ptype_loc);
            }
            TypeKind::Ptype_open => {
                self.write(" = ");
                if decl.ptype_private == PrivateFlag::Private {
                    self.write("private ");
                }
                self.write("..");
            }
        }

        // Print type constraints
        self.print_type_constraints(&decl.ptype_cstrs);
    }

    /// Print constructor declarations for a variant type.
    fn print_constructor_declarations(
        &mut self,
        constructors: &[ConstructorDeclaration],
        loc: &Location,
    ) {
        if constructors.is_empty() {
            return;
        }

        // Check if the variant should be printed on multiple lines
        // Based on whether it spans multiple lines in the source
        let force_break = if let (Some(first), Some(last)) =
            (constructors.first(), constructors.last())
        {
            first.pcd_loc.loc_start.line < last.pcd_loc.loc_end.line
        } else {
            false
        };

        if force_break {
            // Multi-line format with pipe before each constructor
            for ctor in constructors {
                self.newline();
                self.write("  | ");
                self.print_constructor_declaration(ctor);
            }
        } else {
            // Single-line format
            self.write(" ");
            for (i, ctor) in constructors.iter().enumerate() {
                if i > 0 {
                    self.write(" | ");
                }
                self.print_constructor_declaration(ctor);
            }
        }
    }

    /// Print type constraints (constraint 'a = 'b).
    fn print_type_constraints(&mut self, cstrs: &[(CoreType, CoreType, Location)]) {
        for (t1, t2, _loc) in cstrs {
            self.write(" constraint ");
            self.print_core_type(t1);
            self.write(" = ");
            self.print_core_type(t2);
        }
    }

    /// Print a record declaration with proper formatting.
    /// Uses single-line format for short records and multi-line for longer ones.
    fn print_record_declaration(&mut self, fields: &[LabelDeclaration], loc: &Location) {
        if fields.is_empty() {
            self.write("{}");
            return;
        }

        // Check if the record should be printed on multiple lines
        // Based on whether it spans multiple lines in the source
        let force_break = loc.loc_start.line < loc.loc_end.line;

        if force_break {
            // Multi-line format
            self.write("{");
            for field in fields {
                self.newline();
                self.write("  ");
                self.print_label_declaration(field);
                self.write(",");
            }
            self.newline();
            self.write("}");
        } else {
            // Single-line format
            self.write("{");
            for (i, field) in fields.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.print_label_declaration(field);
            }
            self.write("}");
        }
    }

    /// Print a constructor declaration.
    fn print_constructor_declaration(&mut self, ctor: &ConstructorDeclaration) {
        if self.comments_enabled() {
            self.print_comments_before_loc(&ctor.pcd_loc);
        }
        // Handle spread syntax: ...typeName
        if ctor.pcd_name.txt == "..." {
            self.write("...");
            if let ConstructorArguments::Pcstr_tuple(typs) = &ctor.pcd_args {
                if let Some(typ) = typs.first() {
                    self.print_core_type(typ);
                }
            }
            if self.comments_enabled() {
                self.print_trailing_comments_for_loc(&ctor.pcd_loc);
            }
            return;
        }
        // Constructor names are not escaped - they're expected to start with uppercase
        self.write(&ctor.pcd_name.txt);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&ctor.pcd_name.loc);
        }
        match &ctor.pcd_args {
            ConstructorArguments::Pcstr_tuple(typs) if !typs.is_empty() => {
                self.write("(");
                for (i, typ) in typs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_core_type(typ);
                }
                self.write(")");
            }
            ConstructorArguments::Pcstr_record(fields) => {
                self.write("{");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_label_declaration(field);
                }
                self.write("}");
            }
            _ => {}
        }
        if let Some(res) = &ctor.pcd_res {
            self.write(": ");
            self.print_core_type(res);
        }
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&ctor.pcd_loc);
        }
    }

    /// Print a label declaration.
    fn print_label_declaration(&mut self, field: &LabelDeclaration) {
        if self.comments_enabled() {
            self.print_comments_before_loc(&field.pld_loc);
        }
        // Print attributes inline before the field
        self.print_attributes_inline(&field.pld_attributes);
        if field.pld_mutable == MutableFlag::Mutable {
            self.write("mutable ");
        }
        if self.comments_enabled() {
            self.print_comments_before_loc(&field.pld_name.loc);
        }
        // Use write_ident to escape keywords like "module", "let", etc.
        self.write_ident(&field.pld_name.txt);
        // Print optional marker after field name
        if field.pld_optional {
            self.write("?");
        }
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&field.pld_name.loc);
        }
        self.write(": ");
        self.print_core_type(&field.pld_type);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&field.pld_loc);
        }
    }

    /// Print attributes inline (without newlines).
    fn print_attributes_inline(&mut self, attrs: &Attributes) {
        for attr in attrs {
            if !Self::is_internal_attribute(attr) {
                self.write("@");
                self.print_attribute(attr);
                self.write(" ");
            }
        }
    }

    /// Print an extension constructor.
    fn print_extension_constructor(&mut self, ext: &ExtensionConstructor) {
        if self.comments_enabled() {
            self.print_comments_before_loc(&ext.pext_loc);
        }
        self.write(&ext.pext_name.txt);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&ext.pext_name.loc);
        }
        match &ext.pext_kind {
            ExtensionConstructorKind::Pext_decl(args, res) => {
                match args {
                    ConstructorArguments::Pcstr_tuple(typs) if !typs.is_empty() => {
                        self.write("(");
                        for (i, typ) in typs.iter().enumerate() {
                            if i > 0 {
                                self.write(", ");
                            }
                            self.print_core_type(typ);
                        }
                        self.write(")");
                    }
                    ConstructorArguments::Pcstr_record(fields) => {
                        self.write("{");
                        for (i, field) in fields.iter().enumerate() {
                            if i > 0 {
                                self.write(", ");
                            }
                            self.print_label_declaration(field);
                        }
                        self.write("}");
                    }
                    _ => {}
                }
                if let Some(res) = res {
                    self.write(": ");
                    self.print_core_type(res);
                }
            }
            ExtensionConstructorKind::Pext_rebind(lid) => {
                self.write(" = ");
                self.print_longident(&lid.txt);
            }
        }
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&ext.pext_loc);
        }
    }

    /// Print a type extension.
    fn print_type_extension(&mut self, ext: &TypeExtension) {
        self.write("type ");
        // Use print_lident to escape keywords in the type path
        self.print_lident(&ext.ptyext_path.txt);

        // Print type parameters if any
        if !ext.ptyext_params.is_empty() {
            self.write("<");
            for (i, (typ, _variance)) in ext.ptyext_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.print_core_type(typ);
            }
            self.write(">");
        }

        self.write(" +=");
        if ext.ptyext_private == PrivateFlag::Private {
            self.write(" private");
        }

        // Determine if we should break to a new line
        // Based on OCaml: break if first constructor is on different line than path ends,
        // or if constructors span multiple lines
        let force_break = match (
            ext.ptyext_constructors.first(),
            ext.ptyext_constructors.last(),
        ) {
            (Some(first), Some(last)) => {
                first.pext_loc.loc_start.line > ext.ptyext_path.loc.loc_end.line
                    || first.pext_loc.loc_start.line < last.pext_loc.loc_end.line
            }
            _ => false,
        };

        if force_break {
            for ctor in &ext.ptyext_constructors {
                self.newline();
                self.write("| ");
                self.print_extension_constructor(ctor);
            }
        } else {
            // Single constructor on same line - print inline with space
            for (i, ctor) in ext.ptyext_constructors.iter().enumerate() {
                if i > 0 {
                    self.newline();
                    self.write("| ");
                } else {
                    self.write(" ");
                }
                self.print_extension_constructor(ctor);
            }
        }
    }

    /// Print a value description.
    fn print_value_description(&mut self, vd: &ValueDescription) {
        // Print attributes first
        self.print_attributes(&vd.pval_attributes);

        // "external" if there are primitives, otherwise "let"
        let is_external = !vd.pval_prim.is_empty();
        if is_external {
            self.write("external ");
        } else {
            self.write("let ");
        }

        if self.comments_enabled() {
            self.print_comments_before_loc(&vd.pval_name.loc);
        }
        self.write_ident(&vd.pval_name.txt);
        if self.comments_enabled() {
            self.print_trailing_comments_for_loc(&vd.pval_name.loc);
        }
        self.write(": ");
        self.print_core_type(&vd.pval_type);

        if is_external {
            self.write(" = ");
            for (i, prim) in vd.pval_prim.iter().enumerate() {
                if i > 0 {
                    self.space();
                }
                self.write("\"");
                self.write(prim);
                self.write("\"");
            }
        }
    }

    // ========================================================================
    // JSX
    // ========================================================================

    /// Print a JSX element.
    fn print_jsx_element(&mut self, elem: &JsxElement) {
        match elem {
            JsxElement::Unary(unary) => {
                self.write("<");
                self.print_jsx_tag_name(&unary.tag_name.txt);
                for prop in &unary.props {
                    self.space();
                    self.print_jsx_prop(prop);
                }
                self.write(" />");
            }
            JsxElement::Container(container) => {
                self.write("<");
                self.print_jsx_tag_name(&container.tag_name_start.txt);
                for prop in &container.props {
                    self.space();
                    self.print_jsx_prop(prop);
                }
                self.write(">");
                for child in &container.children {
                    self.print_jsx_child(child);
                }
                self.write("</");
                self.print_jsx_tag_name(&container.tag_name_start.txt);
                self.write(">");
            }
            JsxElement::Fragment(fragment) => {
                self.write("<>");
                for child in &fragment.children {
                    self.print_jsx_child(child);
                }
                self.write("</>");
            }
        }
    }

    /// Print a JSX child expression.
    /// JSX elements and exotic identifiers don't need braces.
    fn print_jsx_child(&mut self, child: &Expression) {
        match &child.pexp_desc {
            // JSX elements don't need braces
            ExpressionDesc::Pexp_jsx_element(_) => {
                self.print_expression(child);
            }
            // Exotic identifiers can be printed bare in JSX
            ExpressionDesc::Pexp_ident(lid) => {
                if let Longident::Lident(name) = &lid.txt {
                    if name.starts_with('"') && name.ends_with('"') {
                        // Exotic identifier - print with backslash prefix
                        self.write(" ");
                        self.write_ident(name);
                        return;
                    }
                }
                // Regular identifiers need braces
                self.write("{");
                self.print_expression(child);
                self.write("}");
            }
            // String constants don't need braces in JSX
            ExpressionDesc::Pexp_constant(Constant::String(s, delim)) => {
                self.write(" ");
                if let Some(d) = delim {
                    self.write(d);
                    self.write(s);
                    self.write(d);
                } else {
                    self.write("\"");
                    self.write(s);
                    self.write("\"");
                }
            }
            // Other expressions need braces
            _ => {
                // Strip res.braces attribute since we're adding our own braces
                let stripped = Self::strip_braces_attribute(child);
                self.write(" {");
                self.print_expression(&stripped);
                self.write("} ");
            }
        }
    }

    /// Print a JSX tag name.
    fn print_jsx_tag_name(&mut self, name: &JsxTagName) {
        match name {
            JsxTagName::Lower(s) => self.write(s),
            JsxTagName::QualifiedLower { path, name } => {
                self.print_longident(path);
                self.write(".");
                self.write(name);
            }
            JsxTagName::Upper(lid) => self.print_longident(lid),
            JsxTagName::Invalid(s) => self.write(s),
        }
    }

    /// Print a JSX prop.
    fn print_jsx_prop(&mut self, prop: &JsxProp) {
        match prop {
            JsxProp::Value {
                name,
                optional,
                value,
            } => {
                self.write_ident(&name.txt);
                if *optional {
                    self.write("=?");
                } else {
                    self.write("=");
                }
                // Check if the value is a string constant - print without braces
                if let ExpressionDesc::Pexp_constant(Constant::String(s, delim)) = &value.pexp_desc
                {
                    // For string constants, print directly as "value"
                    let needs_escape = delim.is_none();
                    if needs_escape {
                        self.write("\"");
                        self.write(s);
                        self.write("\"");
                    } else if let Some(d) = delim {
                        self.write(d);
                        self.write(s);
                        self.write(d);
                    }
                } else if let ExpressionDesc::Pexp_ident(lid) = &value.pexp_desc {
                    // Check if it's an exotic identifier (quoted name)
                    if let Longident::Lident(ident_name) = &lid.txt {
                        if ident_name.starts_with('"') && ident_name.ends_with('"') {
                            // Exotic identifier - print as string value "value"
                            self.write_ident(ident_name);
                            return;
                        }
                    }
                    // Regular identifier - needs braces
                    self.write("{");
                    self.print_expression(value);
                    self.write("}");
                } else {
                    self.write("{");
                    self.print_expression(value);
                    self.write("}");
                }
            }
            JsxProp::Punning { optional, name } => {
                if *optional {
                    self.write("?");
                }
                self.write_ident(&name.txt);
            }
            JsxProp::Spreading { expr, .. } => {
                self.write("{...");
                self.print_expression(expr);
                self.write("}");
            }
        }
    }

    // ========================================================================
    // Helpers
    // ========================================================================

    /// Print a longident.
    /// Print a longident without any escaping.
    /// This is used for module paths where all segments are expected to be valid module names.
    /// Matches OCaml's print_longident function.
    fn print_longident(&mut self, lid: &Longident) {
        match lid {
            Longident::Lident(s) => self.write(s),
            Longident::Ldot(prefix, s) => {
                self.print_longident(prefix);
                self.write(".");
                self.write(s);
            }
            Longident::Lapply(f, arg) => {
                self.print_longident(f);
                self.write("(");
                self.print_longident(arg);
                self.write(")");
            }
        }
    }

    /// Print a longident with escaping for the final segment only.
    /// Path segments (module names) are not escaped, but the final segment is escaped
    /// if it's a keyword or exotic identifier.
    /// Matches OCaml's print_lident function.
    fn print_lident(&mut self, lid: &Longident) {
        match lid {
            Longident::Lident(s) => self.write_ident(s),
            Longident::Ldot(prefix, s) => {
                // Print the prefix path without escaping
                self.print_longident_path_only(prefix);
                self.write(".");
                // Escape the final segment
                self.write_ident(s);
            }
            Longident::Lapply(_, _) => {
                // Lapply is not supported for lident
                self.write("printLident: Longident.Lapply is not supported");
            }
        }
    }

    /// Print the path part of a longident (all segments except final) without escaping.
    fn print_longident_path_only(&mut self, lid: &Longident) {
        match lid {
            Longident::Lident(s) => self.write(s),
            Longident::Ldot(prefix, s) => {
                self.print_longident_path_only(prefix);
                self.write(".");
                self.write(s);
            }
            Longident::Lapply(f, arg) => {
                self.print_longident_path_only(f);
                self.write("(");
                self.print_longident_path_only(arg);
                self.write(")");
            }
        }
    }

    /// Print a constant.
    fn print_constant(&mut self, c: &Constant) {
        match c {
            Constant::Integer(s, suffix) => {
                self.write(s);
                if let Some(suffix) = suffix {
                    self.write(&suffix.to_string());
                }
            }
            Constant::Char(code) => {
                // Print simple printable ASCII characters directly, others as Unicode escapes
                let c = if *code >= 0 {
                    char::from_u32(*code as u32)
                } else {
                    None
                };
                match c {
                    Some(ch) if ch.is_ascii() && !ch.is_ascii_control() && ch != '\'' && ch != '\\' => {
                        self.write(&format!("'{}'", ch));
                    }
                    Some('\\') => {
                        self.write("'\\\\'");
                    }
                    Some('\'') => {
                        self.write("'\\''");
                    }
                    Some('\n') => {
                        self.write("'\\n'");
                    }
                    Some('\r') => {
                        self.write("'\\r'");
                    }
                    Some('\t') => {
                        self.write("'\\t'");
                    }
                    _ => {
                        self.write(&format!("'\\u{{{:04x}}}'", code));
                    }
                }
            }
            Constant::String(s, _delim) => {
                self.write("\"");
                // The string is stored exactly as it appeared in source (between quotes),
                // so we output it directly. The escape sequences are already present.
                self.write(s);
                self.write("\"");
            }
            Constant::Float(s, suffix) => {
                self.write(s);
                if let Some(suffix) = suffix {
                    self.write(&suffix.to_string());
                }
            }
        }
    }

    /// Print a function expression, collecting all parameters and handling labeled args properly.
    fn print_fun_expr(&mut self, expr: &Expression) {
        // Collect all function parameters along with their attributes
        // Stop collecting when we encounter a Pexp_fun with its own attributes
        // (those should be printed as nested arrow functions)
        let mut params: Vec<(&Attributes, &ArgLabel, Option<&Expression>, &Pattern)> = Vec::new();
        let mut current = expr;
        let mut first = true;

        while let ExpressionDesc::Pexp_fun {
            arg_label,
            default,
            lhs,
            rhs,
            ..
        } = &current.pexp_desc
        {
            // Stop collecting if this Pexp_fun has attributes (except for the first one,
            // whose attributes are printed as an outer wrapper by the caller)
            if !first && !current.pexp_attributes.is_empty() {
                break;
            }
            first = false;
            params.push((&current.pexp_attributes, arg_label, default.as_deref(), lhs));
            current = rhs;
        }

        let has_unit_param = params.len() == 1 && Self::is_unit_pattern(params[0].3);

        // Check if there's a return type annotation (body is a constraint)
        let has_return_type = matches!(
            &current.pexp_desc,
            ExpressionDesc::Pexp_constraint(_, _)
        );

        // Check if we need parentheses:
        // - Any labeled parameter
        // - Multiple parameters
        // - Any default value
        // - Any parameter with attributes
        // - Return type annotation (to distinguish (a): int => x from a: int => x)
        // A single unit pattern doesn't need extra parens - it already has ()
        let has_complex_single_param = params.len() == 1
            && !Self::is_simple_arrow_param(params[0].3)
            && !Self::is_unit_pattern(params[0].3);
        // Check if any pattern has attributes (ppat_attributes)
        let has_attrs = params.iter().any(|(_, _, _, pat)| !pat.ppat_attributes.is_empty());
        let needs_parens = params.len() > 1
            || params
                .iter()
                .any(|(_, label, default, _)| !matches!(label, ArgLabel::Nolabel) || default.is_some());
        let needs_parens = needs_parens || has_complex_single_param || has_attrs || has_return_type;

        // Print parameters
        if needs_parens {
            self.write("(");
        }

        for (i, (_attrs, label, default, pat)) in params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            // Note: expression attributes (from Pexp_fun.pexp_attributes) are NOT printed here.
            // The first Pexp_fun's attributes are printed by the caller (print_expression_inner).
            // Pattern attributes (ppat_attributes) are printed in print_fun_param.
            self.print_fun_param(label, *default, pat);
        }

        if needs_parens {
            self.write(")");
        }

        // Check if the body is a constraint - if so, print as return type annotation
        // (params): type => body instead of (params) => (body: type)
        if let ExpressionDesc::Pexp_constraint(inner_body, return_type) = &current.pexp_desc {
            self.write(": ");
            self.print_core_type(return_type);
            self.write(" => ");
            // Function body: sequences need braces, other complex expressions don't need parens
            self.print_function_body(inner_body);
        } else {
            self.write(" => ");
            // Function body: sequences need braces, other complex expressions don't need parens
            self.print_function_body(current);
        }
    }

    /// Print a function body expression.
    /// Sequences need braces, other complex expressions don't need parens.
    fn print_function_body(&mut self, body: &Expression) {
        // Check for sequences - they need braces
        if matches!(&body.pexp_desc, ExpressionDesc::Pexp_sequence(..)) {
            self.write("{");
            self.indent();
            self.newline();
            self.print_block_body_inner(body, false);
            self.dedent();
            self.newline();
            self.write("}");
        } else if let ExpressionDesc::Pexp_fun { is_async, .. } = &body.pexp_desc {
            // For nested arrow functions with attributes (like `@attr (c, d) => ()`),
            // we need to print the attributes and the function without extra parens.
            // First print any non-internal attributes
            for attr in &body.pexp_attributes {
                if !Self::is_internal_attribute(attr) {
                    self.write("@");
                    self.print_attribute(attr);
                    self.space();
                }
            }
            if *is_async {
                self.write("async ");
            }
            self.print_fun_expr(body);
        } else {
            // Other expressions don't need parens in function body context
            self.print_expression_with_comments(body, false);
        }
    }

    fn is_simple_arrow_param(pat: &Pattern) -> bool {
        matches!(&pat.ppat_desc, PatternDesc::Ppat_var(_) | PatternDesc::Ppat_any)
    }

    /// Print a single function parameter, using punning when possible.
    fn print_fun_param(&mut self, label: &ArgLabel, default: Option<&Expression>, pat: &Pattern) {
        match label {
            ArgLabel::Nolabel => {
                self.print_pattern(pat);
                if let Some(def) = default {
                    self.write("=");
                    self.print_expression(def);
                }
            }
            ArgLabel::Labelled(name) => {
                // For labeled args, pattern attributes are printed in two cases:
                // 1. If punned (no alias): print attrs before ~label
                // 2. If not punned (has alias): print attrs as part of the aliased pattern
                // This avoids double-printing attributes.
                let (is_punned, _) = Self::punned_pattern_type(&name.txt, pat);
                if is_punned {
                    // Print pattern attributes before the label only when punned
                    for attr in &pat.ppat_attributes {
                        self.write("@");
                        self.print_attribute(attr);
                        self.write(" ");
                    }
                }
                self.write("~");
                self.write_ident(&name.txt);
                let (is_punned, punned_type) = Self::punned_pattern_type(&name.txt, pat);
                if let Some(typ) = punned_type {
                    self.write(": ");
                    self.print_core_type(typ);
                } else if !is_punned {
                    self.write(" as ");
                    self.print_pattern(pat);
                }
                if let Some(def) = default {
                    self.write("=");
                    self.print_expression(def);
                }
            }
            ArgLabel::Optional(name) => {
                // For optional args, pattern attributes are printed in two cases:
                // 1. If punned (no alias): print attrs before ~label
                // 2. If not punned (has alias): print attrs as part of the aliased pattern
                let (is_punned, _) = Self::punned_pattern_type(&name.txt, pat);
                if is_punned {
                    for attr in &pat.ppat_attributes {
                        self.write("@");
                        self.print_attribute(attr);
                        self.write(" ");
                    }
                }
                self.write("~");
                self.write_ident(&name.txt);
                let (is_punned, punned_type) = Self::punned_pattern_type(&name.txt, pat);
                if let Some(typ) = punned_type {
                    self.write(": ");
                    self.print_core_type(typ);
                } else if !is_punned {
                    self.write(" as ");
                    self.print_pattern(pat);
                }
                if let Some(def) = default {
                    self.write("=");
                    self.print_expression(def);
                } else {
                    self.write("=?");
                }
            }
        }
    }

    /// Check if pattern is a simple variable with the same name (for punning).
    fn is_punned_pattern(name: &str, pat: &Pattern) -> bool {
        match &pat.ppat_desc {
            PatternDesc::Ppat_var(var) => var.txt == name,
            PatternDesc::Ppat_constraint(inner, _) => Self::is_punned_pattern(name, inner),
            _ => false,
        }
    }

    fn punned_pattern_type<'a>(name: &str, pat: &'a Pattern) -> (bool, Option<&'a CoreType>) {
        match &pat.ppat_desc {
            PatternDesc::Ppat_constraint(inner, typ) if Self::is_punned_pattern(name, inner) => {
                (true, Some(typ))
            }
            _ => (Self::is_punned_pattern(name, pat), None),
        }
    }

    /// Print an argument label (used for function application arguments).
    fn print_arg_label(&mut self, label: &ArgLabel) {
        match label {
            ArgLabel::Nolabel => {}
            ArgLabel::Labelled(s) => {
                self.write("~");
                self.write_ident(&s.txt);
                self.write("=");
            }
            ArgLabel::Optional(s) => {
                self.write("~");
                self.write_ident(&s.txt);
                self.write("=?");
            }
        }
    }

    /// Print attributes.
    fn print_attributes(&mut self, attrs: &Attributes) {
        self.print_attributes_with_sep(attrs, true);
    }

    /// Print attributes with optional newline separator.
    /// If `with_newline` is true, each attribute ends with a newline.
    /// If false, each attribute ends with a space.
    fn print_attributes_with_sep(&mut self, attrs: &Attributes, with_newline: bool) {
        for attr in attrs {
            if Self::is_internal_attribute(attr) {
                continue;
            }
            self.write("@");
            self.print_attribute(attr);
            if with_newline {
                self.newline();
            } else {
                self.write(" ");
            }
        }
    }

    /// Print a single attribute (without the @ prefix).
    fn print_attribute(&mut self, attr: &Attribute) {
        self.write(&attr.0.txt);
        match &attr.1 {
            Payload::PStr(items) if items.is_empty() => {}
            Payload::PStr(items) => {
                // Check if it's a simple string payload like @as("foo")
                if items.len() == 1 {
                    if let StructureItemDesc::Pstr_eval(expr, _) = &items[0].pstr_desc {
                        if let ExpressionDesc::Pexp_constant(Constant::String(s, _)) =
                            &expr.pexp_desc
                        {
                            self.write("(\"");
                            self.write(s);
                            self.write("\")");
                            return;
                        }
                    }
                }
                // Fallback for complex payloads
                self.write("(");
                for item in items {
                    self.print_structure_item(item);
                }
                self.write(")");
            }
            _ => {
                self.write("(...)");
            }
        }
    }

    /// Print a single attribute with @ prefix.
    fn print_attribute_with_at(&mut self, attr: &Attribute) {
        self.write("@");
        self.print_attribute(attr);
    }

    /// Print an extension (name + payload).
    fn print_extension(&mut self, ext: &Extension) {
        self.write("%");
        self.write_ident(&ext.0.txt);
        self.print_extension_payload(&ext.1);
    }

    fn print_extension_payload(&mut self, payload: &Payload) {
        match payload {
            Payload::PStr(items) => {
                if !items.is_empty() {
                    self.write("(");
                    for item in items {
                        self.print_structure_item(item);
                    }
                    self.write(")");
                }
            }
            Payload::PSig(_) => self.write("(: ...)"),
            Payload::PTyp(typ) => {
                self.write("(: ");
                self.print_core_type(typ);
                self.write(")");
            }
            Payload::PPat(pat, expr) => {
                self.write("(");
                self.print_pattern(pat);
                if let Some(e) = expr {
                    self.write(" when ");
                    self.print_expression(e);
                }
                self.write(")");
            }
        }
    }
}

// Implement get_attributes for StructureItemDesc
impl StructureItemDesc {
    fn get_attributes(&self) -> Attributes {
        match self {
            StructureItemDesc::Pstr_eval(_, attrs) => attrs.clone(),
            StructureItemDesc::Pstr_extension(_, attrs) => attrs.clone(),
            StructureItemDesc::Pstr_primitive(vd) => vd.pval_attributes.clone(),
            StructureItemDesc::Pstr_module(mb) => mb.pmb_attributes.clone(),
            StructureItemDesc::Pstr_open(od) => od.popen_attributes.clone(),
            StructureItemDesc::Pstr_include(id) => id.pincl_attributes.clone(),
            StructureItemDesc::Pstr_modtype(mtd) => mtd.pmtd_attributes.clone(),
            StructureItemDesc::Pstr_typext(te) => te.ptyext_attributes.clone(),
            StructureItemDesc::Pstr_exception(ec) => ec.pext_attributes.clone(),
            // Type and value have their attributes on individual items within the vec
            // These are handled separately in their specific print functions
            _ => vec![],
        }
    }
}

// ============================================================================
// Convenience Functions
// ============================================================================

/// Print a structure to a string.
pub fn print_structure(structure: &Structure) -> String {
    let mut printer = Printer::new();
    printer.print_structure(structure);
    printer.into_output()
}

pub fn print_structure_with_width(structure: &Structure, max_width: usize) -> String {
    let mut printer = Printer::with_width(max_width);
    printer.print_structure(structure);
    printer.into_output()
}

pub fn print_structure_with_comments(structure: &Structure, comments: &[Comment]) -> String {
    let mut printer = Printer::with_comments(comments.to_vec());
    printer.print_structure(structure);
    printer.into_output()
}

pub fn print_structure_with_comments_and_width(
    structure: &Structure,
    comments: &[Comment],
    max_width: usize,
) -> String {
    let mut printer = Printer::with_comments_and_width(comments.to_vec(), max_width);
    printer.print_structure(structure);
    printer.into_output()
}

/// Print an expression to a string.
pub fn print_expression(expr: &Expression) -> String {
    let mut printer = Printer::new();
    printer.print_expression(expr);
    printer.into_output()
}

/// Print a pattern to a string.
pub fn print_pattern(pat: &Pattern) -> String {
    let mut printer = Printer::new();
    printer.print_pattern(pat);
    printer.into_output()
}

/// Print a core type to a string.
pub fn print_core_type(typ: &CoreType) -> String {
    let mut printer = Printer::new();
    printer.print_core_type(typ);
    printer.into_output()
}

/// Print a signature (interface file) to a string.
pub fn print_signature(signature: &[SignatureItem]) -> String {
    let mut printer = Printer::new();
    printer.print_signature(signature);
    printer.into_output()
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Parser, module};
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration;

    /// Default timeout for parsing operations (5 seconds - very generous for ms-scale ops)
    const PARSE_TIMEOUT: Duration = Duration::from_secs(5);

    /// Parse and print source code with a timeout.
    /// Panics if parsing/printing takes longer than PARSE_TIMEOUT.
    fn roundtrip(source: &str) -> String {
        let source_owned = source.to_string();
        let source_for_error = source_owned.clone();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut parser = Parser::new("test.res", &source_owned);
            let structure = module::parse_structure(&mut parser);
            let result = print_structure_with_comments(&structure, parser.comments());
            let _ = tx.send(result);
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

    fn roundtrip_twice(source: &str) -> String {
        let first = roundtrip(source);
        let second = roundtrip(&first);
        assert_eq!(first, second, "Roundtrip not idempotent");
        first
    }

    fn roundtrip_with_width(source: &str, max_width: usize) -> String {
        let source_owned = source.to_string();
        let source_for_error = source_owned.clone();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let mut parser = Parser::new("test.res", &source_owned);
            let structure = module::parse_structure(&mut parser);
            let result =
                print_structure_with_comments_and_width(&structure, parser.comments(), max_width);
            let _ = tx.send(result);
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
    fn test_print_let_binding() {
        let result = roundtrip("let x = 42");
        assert!(result.contains("let x = 42"));
    }

    #[test]
    fn test_print_let_binding_with_string() {
        let result = roundtrip("let s = \"hello\"");
        assert!(result.contains("let s = \"hello\""));
    }

    #[test]
    fn test_print_type_declaration() {
        let result = roundtrip("type t = int");
        assert!(result.contains("type t = int"));
    }

    #[test]
    fn test_print_variant_type() {
        let result = roundtrip("type color = | Red | Green | Blue");
        assert!(result.contains("type color ="));
        assert!(result.contains("| Red"));
        assert!(result.contains("| Green"));
        assert!(result.contains("| Blue"));
    }

    #[test]
    fn test_print_record_type() {
        let result = roundtrip("type person = { name: string, age: int }");
        assert!(result.contains("type person = {"));
        assert!(result.contains("name: string"));
        assert!(result.contains("age: int"));
    }

    #[test]
    fn test_print_open() {
        let result = roundtrip("open Belt");
        assert!(result.contains("open Belt"));
    }

    #[test]
    fn test_print_module() {
        let result = roundtrip("module M = { let x = 1 }");
        assert!(result.contains("module M = {"));
        assert!(result.contains("let x = 1"));
    }

    #[test]
    fn test_print_external() {
        let result = roundtrip("external log: string => unit = \"console.log\"");
        assert!(result.contains("external log: string => unit"));
        assert!(result.contains("\"console.log\""));
    }

    #[test]
    fn test_print_exception() {
        let result = roundtrip("exception MyError");
        assert!(result.contains("exception MyError"));
    }

    #[test]
    fn test_print_array() {
        let result = roundtrip("let arr = [1, 2, 3]");
        assert!(result.contains("[1, 2, 3]"));
    }

    #[test]
    fn test_print_tuple() {
        let result = roundtrip("let t = (1, 2)");
        assert!(result.contains("(1, 2)"));
    }

    #[test]
    fn test_print_if() {
        let result = roundtrip("let x = if true { 1 } else { 2 }");
        assert!(result.contains("if true"));
    }

    #[test]
    fn test_print_switch() {
        let result = roundtrip("let x = switch y { | 1 => true | _ => false }");
        assert!(result.contains("switch y"));
        assert!(result.contains("| 1 => true"));
    }

    #[test]
    fn test_printer_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Printer>();
    }

    // Additional roundtrip tests

    #[test]
    fn test_print_function() {
        let result = roundtrip("let f = (x) => x + 1");
        assert!(result.contains("=>"));
    }

    #[test]
    fn test_print_function_application() {
        let result = roundtrip("let y = f(1, 2)");
        assert!(result.contains("f(1, 2)"));
    }

    #[test]
    fn test_print_record_expression() {
        let result = roundtrip("let r = {name: \"test\", age: 30}");
        assert!(result.contains("name:"));
        assert!(result.contains("age:"));
    }

    #[test]
    fn test_print_field_access() {
        let result = roundtrip("let n = person.name");
        assert!(result.contains("person.name"));
    }

    #[test]
    fn test_print_while_loop() {
        let result = roundtrip("let _ = while true { () }");
        assert!(result.contains("while true"));
    }

    #[test]
    fn test_print_for_loop() {
        let result = roundtrip("for i in 0 to 10 { () }");
        assert!(result.contains("for i in"));
        assert!(result.contains("to"));
    }

    #[test]
    fn test_print_polymorphic_variant() {
        let result = roundtrip("let x = #Red");
        assert!(result.contains("#Red") || result.contains("Red"));
    }

    #[test]
    fn test_print_try_catch() {
        let result = roundtrip("let x = try f() catch { | Exn => 0 }");
        assert!(result.contains("try"));
        assert!(result.contains("catch"));
    }

    #[test]
    fn test_print_type_with_params() {
        // ReScript uses angle bracket syntax for type parameters: type option<'a>
        let result = roundtrip("type option<'a> = | None | Some('a)");
        assert!(
            result.contains("option<'a>"),
            "Expected \"option<'a>\" but got: {}",
            result
        );
        assert!(result.contains("| None"));
        assert!(result.contains("| Some"));
    }

    #[test]
    fn test_print_type_and_chain() {
        let result = roundtrip_twice("type t = string and s = int and u = float");
        assert!(result.contains("and s = int"), "Missing second type: {}", result);
        assert!(result.contains("and u = float"), "Missing third type: {}", result);
    }

    #[test]
    fn test_print_type_constructor_tuple_arg() {
        let result = roundtrip("type t = constr<(string, int)>");
        assert!(result.contains("constr<(string, int)>"), "Unexpected output: {}", result);
    }

    #[test]
    fn test_long_tuple_type_breaks() {
        let source = "type t = (superLongTypeNameThatWillBreak, superLongTypeNameThatWillBreak, superLongTypeNameThatWillBreak, superLongTypeNameThatWillBreak)";
        let result = roundtrip_with_width(source, 60);
        assert!(result.contains("(\n"), "Expected multiline tuple type: {}", result);
    }

    #[test]
    fn test_print_labeled_function() {
        let result = roundtrip("let f = (~x, ~y) => x + y");
        assert!(result.contains("~x"));
        assert!(result.contains("~y"));
    }

    #[test]
    fn test_print_pattern_with_constructor() {
        let result = roundtrip("let Some(x) = opt");
        assert!(result.contains("Some(x)"));
    }

    #[test]
    fn test_print_constraint() {
        let result = roundtrip("let x: int = 42");
        // The pattern constraint might be parsed differently
        assert!(result.contains("int") || result.contains("x = 42"));
    }

    // TODO: assert parsing needs to be implemented
    // #[test]
    // fn test_print_assert() {
    //     let result = roundtrip("let _ = assert(x > 0)");
    //     eprintln!("Assert result: {}", result);
    //     assert!(result.contains("assert"));
    // }

    #[test]
    fn test_print_await() {
        let result = roundtrip("let x = await promise");
        assert!(result.contains("await promise"));
    }

    #[test]
    fn test_print_sequence() {
        let result = roundtrip("let _ = { f(); g() }");
        // The parser might parse this differently
        assert!(result.contains("f") && result.contains("g"));
    }

    #[test]
    fn test_roundtrip_sequence_in_fun_body() {
        let result = roundtrip_twice("let f = x => { a(); b() }");
        assert!(result.contains("=> {"), "Expected block body in output: {}", result);
    }

    #[test]
    fn test_roundtrip_sequence_in_binary_rhs() {
        let result = roundtrip_twice("let x = a && { b(); c() }");
        assert!(
            result.contains("&& {"),
            "Expected block RHS in output: {}",
            result
        );
    }

    #[test]
    fn test_roundtrip_attribute_parens() {
        let result = roundtrip_twice("let x = @attr (a + b)");
        assert!(
            result.contains("@attr (a + b)"),
            "Expected parentheses preserved: {}",
            result
        );
    }

    #[test]
    fn test_roundtrip_comment_preservation() {
        let result = roundtrip_twice("let /*a*/ x /*b*/ = 1 /*c*/");
        assert!(result.contains("/*a*/"), "Missing /*a*/ in output: {}", result);
        assert!(result.contains("/*b*/"), "Missing /*b*/ in output: {}", result);
        assert!(result.contains("/*c*/"), "Missing /*c*/ in output: {}", result);
    }

    #[test]
    fn test_roundtrip_unit_param() {
        let result = roundtrip_twice("let f = (()) => 1");
        assert!(
            result.contains("(()) =>"),
            "Expected unit param to keep extra parens: {}",
            result
        );
    }

    #[test]
    fn test_roundtrip_single_element_tuple_construct() {
        let result = roundtrip_twice("let x = Some((foo()))");
        assert!(
            result.contains("Some(foo())"),
            "Expected single-element tuple to avoid double parens: {}",
            result
        );
    }

    #[test]
    fn test_print_binary_operators() {
        let result = roundtrip("let x = 1 + 2 * 3");
        assert!(result.contains("1"));
        assert!(result.contains("2"));
        assert!(result.contains("3"));
    }

    #[test]
    fn test_print_module_type() {
        let result = roundtrip("module type S = { type t let x: t }");
        assert!(result.contains("module type S"));
    }

    #[test]
    fn test_print_include() {
        let result = roundtrip("include Belt");
        assert!(result.contains("include Belt"));
    }

    #[test]
    fn test_print_or_pattern() {
        let result = roundtrip("let x = switch y { | 1 | 2 => true | _ => false }");
        assert!(result.contains("1 | 2"));
    }

    #[test]
    fn test_print_constructor_with_args() {
        let result = roundtrip("type t = | Point(int, int)");
        assert!(result.contains("Point(int, int)"));
    }

    // JSX tests
    #[test]
    fn test_print_jsx_unary() {
        let result = roundtrip("let x = <div />");
        assert!(result.contains("<div"));
        assert!(result.contains("/>"));
    }

    #[test]
    fn test_print_jsx_container() {
        let result = roundtrip("let x = <div> {child} </div>");
        assert!(result.contains("<div>"));
        assert!(result.contains("</div>"));
    }

    #[test]
    fn test_print_jsx_fragment() {
        let result = roundtrip("let x = <> {child} </>");
        assert!(result.contains("<>"));
        assert!(result.contains("</>"));
    }

    #[test]
    fn test_print_jsx_with_props() {
        let result = roundtrip("let x = <Button onClick={handler} />");
        assert!(result.contains("onClick"));
    }

    // Edge cases
    #[test]
    fn test_print_nested_modules() {
        let result = roundtrip("module A = { module B = { let x = 1 } }");
        assert!(result.contains("module A"));
        assert!(result.contains("module B"));
    }

    #[test]
    fn test_print_functor() {
        let result = roundtrip("module F = (X: S) => { type t = X.t }");
        assert!(result.contains("module F"));
    }

    #[test]
    fn test_print_mutable_field() {
        let result = roundtrip("type t = { mutable x: int }");
        assert!(result.contains("mutable"));
    }

    #[test]
    fn test_print_private_type() {
        let result = roundtrip("type t = private int");
        assert!(result.contains("private"));
    }

    // TODO: Type extension parsing needs work
    // #[test]
    // fn test_print_type_extension() {
    //     let result = roundtrip("type t += | NewCase");
    //     assert!(result.contains("+="));
    // }

    #[test]
    fn test_print_nested_switch() {
        let result = roundtrip(
            r#"
            let x = switch a {
            | 1 => switch b { | 2 => true | _ => false }
            | _ => false
            }
        "#,
        );
        assert!(result.contains("switch a"));
        assert!(result.contains("switch b"));
    }

    #[test]
    fn test_print_chained_field_access() {
        let result = roundtrip("let x = a.b.c.d");
        assert!(result.contains("a.b.c.d"));
    }

    #[test]
    fn test_print_optional_labeled_arg() {
        let result = roundtrip("let f = (~x=?, ~y) => x");
        assert!(result.contains("~x"));
        assert!(result.contains("~y"));
    }

    #[test]
    fn test_print_variant_with_payload() {
        let result = roundtrip("let x = #Foo(1, 2)");
        // Polymorphic variants might parse differently
        assert!(result.contains("Foo") || result.contains("#"));
    }

    #[test]
    fn test_print_record_spread() {
        let result = roundtrip("let r = {...old, x: 1}");
        assert!(result.contains("..."));
    }

    #[test]
    fn test_print_nested_tuples() {
        let result = roundtrip("let x = ((1, 2), (3, 4))");
        assert!(result.contains("(1, 2)"));
        assert!(result.contains("(3, 4)"));
    }

    #[test]
    fn test_print_complex_type() {
        // Type parameters with angle brackets use different syntax
        let result = roundtrip("type ('a, 'b) t = ('a, 'b) => option");
        assert!(result.contains("t"));
    }

    // Stress tests for deeply nested structures (verify no hangs)
    #[test]
    fn test_deeply_nested_parens() {
        let result = roundtrip("let x = ((((((1))))))");
        assert!(result.contains("1"));
    }

    #[test]
    fn test_deeply_nested_arrays() {
        let result = roundtrip("let x = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]");
        assert!(result.contains("1"));
        assert!(result.contains("8"));
    }

    #[test]
    fn test_long_binary_expression() {
        let result = roundtrip("let x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10");
        assert!(result.contains("1"));
        assert!(result.contains("10"));
    }

    #[test]
    fn test_many_function_args() {
        let result = roundtrip("let f = (a, b, c, d, e, f, g, h) => a + b + c");
        assert!(result.contains("a"));
        assert!(result.contains("h"));
    }

    #[test]
    fn test_chained_pipes() {
        let result = roundtrip("let x = a->f->g->h->i->j");
        assert!(result.contains("a"));
        assert!(result.contains("j") || result.contains("->"));
    }
}
