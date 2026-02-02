//! Pretty-printing document representation.
//!
//! This module implements a document representation for pretty-printing,
//! based on Wadler's "A Prettier Printer" algorithm.
//!
//! The key idea is to build a document tree that describes the structure of the
//! output, then render it to a string respecting a maximum line width.

use std::cell::Cell;
use std::rc::Rc;

/// Line break styles
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LineStyle {
    /// Classic line break - replaced with space when in flat mode
    Classic,
    /// Soft line break - replaced with nothing when in flat mode
    Soft,
    /// Hard line break - always included, forces breaks in parent groups
    Hard,
    /// Literal line break - always included, doesn't increase indentation
    /// Used for template literals, multiline string content
    Literal,
}

/// Rendering mode
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    /// Break mode - line breaks are rendered as newlines
    Break,
    /// Flat mode - line breaks are rendered as spaces (Classic) or nothing (Soft)
    Flat,
}

/// A pretty-printing document
#[derive(Debug, Clone)]
pub enum Doc {
    /// Empty document
    Nil,
    /// Literal text
    Text(String),
    /// Concatenation of documents
    Concat(Vec<Doc>),
    /// Indent the document by 2 spaces
    Indent(Box<Doc>),
    /// Different output depending on whether we're in break or flat mode
    IfBreaks {
        yes: Box<Doc>,
        no: Box<Doc>,
        /// When true, always use the `yes` branch
        broken: Rc<Cell<bool>>,
    },
    /// Content to print at the end of the line (before the newline)
    LineSuffix(Box<Doc>),
    /// A line break with the given style
    LineBreak(LineStyle),
    /// A group that can be broken or kept flat
    Group {
        /// Whether this group should be broken (forced or computed)
        should_break: Rc<Cell<bool>>,
        /// The document inside the group
        doc: Box<Doc>,
    },
    /// Multiple layout choices - picks first that fits, or last if none fit
    CustomLayout(Vec<Doc>),
    /// Force the parent group to break
    BreakParent,
}

impl Doc {
    // ========== Constructors ==========

    pub fn nil() -> Doc {
        Doc::Nil
    }

    pub fn text(s: impl Into<String>) -> Doc {
        Doc::Text(s.into())
    }

    pub fn line() -> Doc {
        Doc::LineBreak(LineStyle::Classic)
    }

    pub fn soft_line() -> Doc {
        Doc::LineBreak(LineStyle::Soft)
    }

    pub fn hard_line() -> Doc {
        Doc::LineBreak(LineStyle::Hard)
    }

    pub fn literal_line() -> Doc {
        Doc::LineBreak(LineStyle::Literal)
    }

    /// Concatenate documents, with optimization to collapse nested concats and remove nils
    pub fn concat(docs: Vec<Doc>) -> Doc {
        let mut result = Vec::with_capacity(docs.len());
        for doc in docs {
            match doc {
                Doc::Nil => {}
                Doc::Concat(inner) => result.extend(inner),
                other => result.push(other),
            }
        }
        if result.is_empty() {
            Doc::Nil
        } else if result.len() == 1 {
            result.pop().unwrap()
        } else {
            Doc::Concat(result)
        }
    }

    pub fn indent(doc: Doc) -> Doc {
        Doc::Indent(Box::new(doc))
    }

    pub fn if_breaks(yes: Doc, no: Doc) -> Doc {
        Doc::IfBreaks {
            yes: Box::new(yes),
            no: Box::new(no),
            broken: Rc::new(Cell::new(false)),
        }
    }

    pub fn line_suffix(doc: Doc) -> Doc {
        Doc::LineSuffix(Box::new(doc))
    }

    pub fn group(doc: Doc) -> Doc {
        Doc::Group {
            should_break: Rc::new(Cell::new(false)),
            doc: Box::new(doc),
        }
    }

    pub fn breakable_group(doc: Doc, force_break: bool) -> Doc {
        Doc::Group {
            should_break: Rc::new(Cell::new(force_break)),
            doc: Box::new(doc),
        }
    }

    pub fn custom_layout(layouts: Vec<Doc>) -> Doc {
        Doc::CustomLayout(layouts)
    }

    pub fn break_parent() -> Doc {
        Doc::BreakParent
    }

    // ========== Common combinators ==========

    pub fn space() -> Doc {
        Doc::text(" ")
    }

    pub fn comma() -> Doc {
        Doc::text(",")
    }

    pub fn dot() -> Doc {
        Doc::text(".")
    }

    pub fn dotdot() -> Doc {
        Doc::text("..")
    }

    pub fn dotdotdot() -> Doc {
        Doc::text("...")
    }

    pub fn less_than() -> Doc {
        Doc::text("<")
    }

    pub fn greater_than() -> Doc {
        Doc::text(">")
    }

    pub fn lbrace() -> Doc {
        Doc::text("{")
    }

    pub fn rbrace() -> Doc {
        Doc::text("}")
    }

    pub fn lparen() -> Doc {
        Doc::text("(")
    }

    pub fn rparen() -> Doc {
        Doc::text(")")
    }

    pub fn lbracket() -> Doc {
        Doc::text("[")
    }

    pub fn rbracket() -> Doc {
        Doc::text("]")
    }

    pub fn question() -> Doc {
        Doc::text("?")
    }

    pub fn tilde() -> Doc {
        Doc::text("~")
    }

    pub fn equal() -> Doc {
        Doc::text("=")
    }

    pub fn double_quote() -> Doc {
        Doc::text("\"")
    }

    /// A trailing comma that only appears when the group is broken
    pub fn trailing_comma() -> Doc {
        Doc::if_breaks(Doc::comma(), Doc::nil())
    }

    /// Join documents with a separator
    pub fn join(sep: Doc, docs: Vec<Doc>) -> Doc {
        let mut result = Vec::with_capacity(docs.len() * 2);
        let mut first = true;
        for doc in docs {
            if !first {
                result.push(sep.clone());
            }
            first = false;
            result.push(doc);
        }
        Doc::concat(result)
    }

    /// Join documents with separators that may differ per element
    pub fn join_with_sep(docs_with_sep: Vec<(Doc, Doc)>) -> Doc {
        let mut result = Vec::with_capacity(docs_with_sep.len() * 2);
        let len = docs_with_sep.len();
        for (i, (doc, sep)) in docs_with_sep.into_iter().enumerate() {
            result.push(doc);
            if i < len - 1 {
                result.push(sep);
            }
        }
        Doc::concat(result)
    }

    // ========== Rendering ==========

    /// Propagate forced breaks up through the document tree.
    /// This sets the `should_break` flag on groups that contain hard breaks.
    pub fn propagate_forced_breaks(&self) -> bool {
        match self {
            Doc::Text(_) | Doc::Nil | Doc::LineSuffix(_) => false,
            Doc::BreakParent => true,
            Doc::LineBreak(LineStyle::Hard) | Doc::LineBreak(LineStyle::Literal) => true,
            Doc::LineBreak(LineStyle::Classic) | Doc::LineBreak(LineStyle::Soft) => false,
            Doc::Indent(child) => child.propagate_forced_breaks(),
            Doc::IfBreaks { yes, no, broken } => {
                let no_forces = no.propagate_forced_breaks();
                if no_forces {
                    yes.propagate_forced_breaks();
                    broken.set(true);
                    true
                } else {
                    yes.propagate_forced_breaks()
                }
            }
            Doc::Group { should_break, doc } => {
                let child_forces = doc.propagate_forced_breaks();
                let should = should_break.get() || child_forces;
                should_break.set(should);
                should
            }
            Doc::Concat(children) => {
                let mut forces = false;
                for child in children {
                    if child.propagate_forced_breaks() {
                        forces = true;
                    }
                }
                forces
            }
            Doc::CustomLayout(children) => {
                // Don't propagate forced breaks up from CustomLayout,
                // but do propagate within sublayouts
                for child in children {
                    child.propagate_forced_breaks();
                }
                false
            }
        }
    }

    /// Check if the document will definitely break (contains hard breaks or forced groups)
    pub fn will_break(&self) -> bool {
        match self {
            Doc::LineBreak(LineStyle::Hard) | Doc::LineBreak(LineStyle::Literal) => true,
            Doc::BreakParent => true,
            Doc::Group { should_break, .. } if should_break.get() => true,
            Doc::Group { doc, .. } => doc.will_break(),
            Doc::Indent(doc) => doc.will_break(),
            Doc::CustomLayout(docs) if !docs.is_empty() => docs[0].will_break(),
            Doc::Concat(docs) => docs.iter().any(|d| d.will_break()),
            Doc::IfBreaks { yes, no, .. } => yes.will_break() || no.will_break(),
            _ => false,
        }
    }

    /// Check if the document fits within the given width.
    /// Takes the current item and the rest of the stack as a slice to avoid cloning.
    fn fits(width: i32, first: (i32, Mode, &Doc), rest: &[(i32, Mode, &Doc)]) -> bool {
        let mut width = width;
        // Create a stack: first the rest items (same order - they'll be processed later),
        // then the first item (which will be popped and processed first)
        let mut stack: Vec<(i32, Mode, &Doc)> = Vec::with_capacity(rest.len() + 1);
        // Push rest items in order (they'll be processed after first)
        for item in rest.iter() {
            stack.push(*item);
        }
        // Push the first item last so it gets processed first (popped first)
        stack.push(first);

        // Limit iterations to prevent exponential blowup with deeply nested custom layouts
        let mut iterations = 0;
        const MAX_ITERATIONS: i32 = 10000;

        while let Some((indent, mode, doc)) = stack.pop() {
            iterations += 1;
            if iterations > MAX_ITERATIONS || width < 0 {
                return false;
            }

            match (mode, doc) {
                (_, Doc::Nil) | (_, Doc::LineSuffix(_)) | (_, Doc::BreakParent) => {}
                (_, Doc::Text(txt)) => {
                    width -= txt.len() as i32;
                }
                (_, Doc::Indent(doc)) => {
                    stack.push((indent + 2, mode, doc));
                }
                (Mode::Flat, Doc::LineBreak(LineStyle::Hard))
                | (Mode::Flat, Doc::LineBreak(LineStyle::Literal)) => {
                    return true;
                }
                (Mode::Flat, Doc::LineBreak(LineStyle::Classic)) => {
                    width -= 1;
                }
                (Mode::Flat, Doc::LineBreak(LineStyle::Soft)) => {}
                (Mode::Break, Doc::LineBreak(_)) => {
                    return true;
                }
                (_, Doc::Group { should_break, doc }) if should_break.get() => {
                    stack.push((indent, Mode::Break, doc));
                }
                (_, Doc::Group { doc, .. }) => {
                    stack.push((indent, mode, doc));
                }
                (_, Doc::IfBreaks { yes, broken, .. }) if broken.get() => {
                    stack.push((indent, mode, yes));
                }
                (Mode::Break, Doc::IfBreaks { yes, .. }) => {
                    stack.push((indent, mode, yes));
                }
                (Mode::Flat, Doc::IfBreaks { no, .. }) => {
                    stack.push((indent, mode, no));
                }
                (_, Doc::Concat(docs)) => {
                    // Push in reverse order so first doc is processed first
                    for doc in docs.iter().rev() {
                        stack.push((indent, mode, doc));
                    }
                }
                (_, Doc::CustomLayout(docs)) if !docs.is_empty() => {
                    stack.push((indent, mode, &docs[0]));
                }
                (_, Doc::CustomLayout(_)) => {}
            }
        }

        width >= 0
    }

    /// Render the document to a string with the given maximum line width
    pub fn to_string(&self, width: i32) -> String {
        // First propagate forced breaks
        self.propagate_forced_breaks();

        let mut buffer = String::with_capacity(1024);
        let mut pos = 0i32;
        let mut line_suffices: Vec<(i32, Mode, &Doc)> = Vec::new();
        let mut stack: Vec<(i32, Mode, &Doc)> = vec![(0, Mode::Flat, self)];

        while let Some((indent, mode, doc)) = stack.pop() {
            match doc {
                Doc::Nil | Doc::BreakParent => {}
                Doc::Text(txt) => {
                    buffer.push_str(txt);
                    pos += txt.len() as i32;
                }
                Doc::LineSuffix(doc) => {
                    line_suffices.push((indent, mode, doc));
                }
                Doc::Concat(docs) => {
                    // Push in reverse order
                    for doc in docs.iter().rev() {
                        stack.push((indent, mode, doc));
                    }
                }
                Doc::Indent(doc) => {
                    stack.push((indent + 2, mode, doc));
                }
                Doc::IfBreaks { yes, broken, .. } if broken.get() => {
                    stack.push((indent, mode, yes));
                }
                Doc::IfBreaks { yes, no, .. } => {
                    if mode == Mode::Break {
                        stack.push((indent, mode, yes));
                    } else {
                        stack.push((indent, mode, no));
                    }
                }
                Doc::LineBreak(style) => {
                    if mode == Mode::Break {
                        if !line_suffices.is_empty() {
                            // Process line suffices first, then come back to this line break
                            stack.push((indent, mode, doc));
                            for suffix in line_suffices.drain(..).rev() {
                                stack.push(suffix);
                            }
                        } else if *style == LineStyle::Literal {
                            buffer.push('\n');
                            pos = 0;
                        } else {
                            // Trim trailing spaces only (not all whitespace - preserve newlines!)
                            // This mimics OCaml's flush_newline which only trims ' ' characters
                            let trimmed_len = buffer.trim_end_matches(' ').len();
                            buffer.truncate(trimmed_len);
                            buffer.push('\n');
                            for _ in 0..indent {
                                buffer.push(' ');
                            }
                            pos = indent;
                        }
                    } else {
                        // Flat mode
                        match style {
                            LineStyle::Classic => {
                                buffer.push(' ');
                                pos += 1;
                            }
                            LineStyle::Hard => {
                                // Trim trailing spaces only (not all whitespace - preserve newlines!)
                                // This mimics OCaml's flush_newline which only trims ' ' characters
                                let trimmed_len = buffer.trim_end_matches(' ').len();
                                buffer.truncate(trimmed_len);
                                buffer.push('\n');
                                pos = 0;
                            }
                            LineStyle::Literal => {
                                buffer.push('\n');
                                pos = 0;
                            }
                            LineStyle::Soft => {}
                        }
                    }
                }
                Doc::Group { should_break, doc } => {
                    // Include remaining stack in fits check (like OCaml)
                    // Pass stack as slice reference to avoid cloning
                    if should_break.get()
                        || !Self::fits(width - pos, (indent, Mode::Flat, doc), &stack)
                    {
                        stack.push((indent, Mode::Break, doc));
                    } else {
                        stack.push((indent, Mode::Flat, doc));
                    }
                }
                Doc::CustomLayout(docs) => {
                    // Find the first layout that fits, or use the last one
                    // Include remaining stack in fits check (like OCaml)
                    // Pass stack as slice reference to avoid cloning
                    let mut chosen = docs.last();
                    for doc in docs.iter() {
                        if Self::fits(width - pos, (indent, Mode::Flat, doc), &stack) {
                            chosen = Some(doc);
                            break;
                        }
                    }
                    if let Some(doc) = chosen {
                        stack.push((indent, Mode::Flat, doc));
                    }
                }
            }
        }

        // Process any remaining line suffices
        for (indent, mode, doc) in line_suffices.into_iter().rev() {
            stack.push((indent, mode, doc));
        }
        while let Some((indent, mode, doc)) = stack.pop() {
            match doc {
                Doc::Nil | Doc::BreakParent | Doc::LineSuffix(_) => {}
                Doc::Text(txt) => {
                    buffer.push_str(txt);
                    pos += txt.len() as i32;
                }
                Doc::Concat(docs) => {
                    for doc in docs.iter().rev() {
                        stack.push((indent, mode, doc));
                    }
                }
                Doc::Indent(doc) => {
                    stack.push((indent + 2, mode, doc));
                }
                Doc::LineBreak(style) => {
                    if mode == Mode::Break {
                        if *style == LineStyle::Literal {
                            buffer.push('\n');
                            pos = 0;
                        } else {
                            let trimmed_len = buffer.trim_end_matches(' ').len();
                            buffer.truncate(trimmed_len);
                            buffer.push('\n');
                            if indent > 0 {
                                buffer.push_str(&" ".repeat(indent as usize));
                            }
                            pos = indent;
                        }
                    } else {
                        // Flat mode
                        match style {
                            LineStyle::Classic => {
                                buffer.push(' ');
                                pos += 1;
                            }
                            LineStyle::Hard => {
                                let trimmed_len = buffer.trim_end_matches(' ').len();
                                buffer.truncate(trimmed_len);
                                buffer.push('\n');
                                pos = 0;
                            }
                            LineStyle::Literal => {
                                buffer.push('\n');
                                pos = 0;
                            }
                            LineStyle::Soft => {}
                        }
                    }
                }
                Doc::Group { doc: contents, should_break } => {
                    let mode = if should_break.get() { Mode::Break } else { Mode::Flat };
                    stack.push((indent, mode, contents));
                }
                Doc::IfBreaks { yes, no, .. } => {
                    if mode == Mode::Break {
                        stack.push((indent, mode, yes));
                    } else {
                        stack.push((indent, mode, no));
                    }
                }
                Doc::CustomLayout(docs) => {
                    // Use the first option
                    if let Some(first) = docs.first() {
                        stack.push((indent, mode, first));
                    }
                }
            }
        }

        buffer
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_text() {
        let doc = Doc::text("hello");
        assert_eq!(doc.to_string(80), "hello");
    }

    #[test]
    fn test_concat() {
        let doc = Doc::concat(vec![Doc::text("hello"), Doc::space(), Doc::text("world")]);
        assert_eq!(doc.to_string(80), "hello world");
    }

    #[test]
    fn test_group_fits() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("hello"),
            Doc::line(),
            Doc::text("world"),
        ]));
        assert_eq!(doc.to_string(80), "hello world");
    }

    #[test]
    fn test_group_breaks() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("hello"),
            Doc::line(),
            Doc::text("world"),
        ]));
        assert_eq!(doc.to_string(5), "hello\nworld");
    }

    #[test]
    fn test_indent() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("{"),
            Doc::indent(Doc::concat(vec![Doc::line(), Doc::text("x")])),
            Doc::line(),
            Doc::text("}"),
        ]));
        assert_eq!(doc.to_string(3), "{\n  x\n}");
    }

    #[test]
    fn test_trailing_comma() {
        let doc = Doc::group(Doc::concat(vec![
            Doc::text("["),
            Doc::indent(Doc::concat(vec![
                Doc::soft_line(),
                Doc::text("a"),
                Doc::text(","),
                Doc::line(),
                Doc::text("b"),
                Doc::trailing_comma(),
            ])),
            Doc::soft_line(),
            Doc::text("]"),
        ]));
        // When fits: [a, b]
        assert_eq!(doc.to_string(80), "[a, b]");
        // When breaks: [\n  a,\n  b,\n]
        assert_eq!(doc.to_string(5), "[\n  a,\n  b,\n]");
    }

    #[test]
    fn test_double_hard_line() {
        // Test that two consecutive hard_lines produce a blank line
        let doc = Doc::concat(vec![
            Doc::text("a"),
            Doc::hard_line(),
            Doc::hard_line(),
            Doc::text("b"),
        ]);
        assert_eq!(doc.to_string(80), "a\n\nb");
    }

    #[test]
    fn test_double_hard_line_in_breakable_group() {
        // Test that two consecutive hard_lines work inside breakable_group
        let doc = Doc::breakable_group(
            Doc::concat(vec![
                Doc::text("a"),
                Doc::hard_line(),
                Doc::hard_line(),
                Doc::text("b"),
            ]),
            true,
        );
        assert_eq!(doc.to_string(80), "a\n\nb");
    }
}
