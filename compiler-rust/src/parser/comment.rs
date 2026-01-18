//! Comment representation for the ReScript parser.
//!
//! This module provides types for representing comments in ReScript source code,
//! preserving their location, style, and content for pretty-printing and
//! documentation extraction.

use crate::location::{Location, Position};
use serde::{Deserialize, Serialize};

/// The style of a comment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CommentStyle {
    /// Single-line comment starting with `//`
    SingleLine,
    /// Multi-line comment starting with `/*` and ending with `*/`
    MultiLine,
    /// Documentation comment starting with `/**` (attached to next item)
    DocComment,
    /// Module-level documentation comment (standalone `/**` at top of file)
    ModuleComment,
}

impl CommentStyle {
    /// Convert the style to a string representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            CommentStyle::SingleLine => "SingleLine",
            CommentStyle::MultiLine => "MultiLine",
            CommentStyle::DocComment => "DocComment",
            CommentStyle::ModuleComment => "ModuleComment",
        }
    }
}

/// A comment in ReScript source code.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Comment {
    /// The text content of the comment (without delimiters).
    pub txt: String,
    /// The style of the comment.
    pub style: CommentStyle,
    /// The location of the comment in source.
    pub loc: Location,
    /// The end position of the previous token (for formatting).
    pub prev_tok_end_pos: Position,
}

impl Comment {
    /// Get the location of the comment.
    pub fn loc(&self) -> &Location {
        &self.loc
    }

    /// Get the text content of the comment.
    pub fn txt(&self) -> &str {
        &self.txt
    }

    /// Get the end position of the previous token.
    pub fn prev_tok_end_pos(&self) -> &Position {
        &self.prev_tok_end_pos
    }

    /// Set the end position of the previous token.
    pub fn set_prev_tok_end_pos(&mut self, pos: Position) {
        self.prev_tok_end_pos = pos;
    }

    /// Check if this is a single-line comment.
    pub fn is_single_line(&self) -> bool {
        self.style == CommentStyle::SingleLine
    }

    /// Check if this is a doc comment.
    pub fn is_doc_comment(&self) -> bool {
        self.style == CommentStyle::DocComment
    }

    /// Check if this is a module comment.
    pub fn is_module_comment(&self) -> bool {
        self.style == CommentStyle::ModuleComment
    }

    /// Create a single-line comment.
    pub fn make_single_line(loc: Location, txt: String) -> Self {
        Self {
            txt,
            loc,
            style: CommentStyle::SingleLine,
            prev_tok_end_pos: Position::default(),
        }
    }

    /// Create a multi-line comment.
    ///
    /// If `doc_comment` is true and `standalone` is true, creates a ModuleComment.
    /// If `doc_comment` is true and `standalone` is false, creates a DocComment.
    /// Otherwise creates a regular MultiLine comment.
    pub fn make_multi_line(
        loc: Location,
        txt: String,
        doc_comment: bool,
        standalone: bool,
    ) -> Self {
        let style = if doc_comment {
            if standalone {
                CommentStyle::ModuleComment
            } else {
                CommentStyle::DocComment
            }
        } else {
            CommentStyle::MultiLine
        };

        Self {
            txt,
            loc,
            style,
            prev_tok_end_pos: Position::default(),
        }
    }

    /// Trim leading and trailing spaces from a string.
    pub fn trim_spaces(s: &str) -> &str {
        s.trim_matches(' ')
    }
}

impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(txt: {}\nstyle: {}\nlocation: {})",
            self.txt,
            self.style.as_str(),
            self.loc
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comment_style() {
        assert_eq!(CommentStyle::SingleLine.as_str(), "SingleLine");
        assert_eq!(CommentStyle::MultiLine.as_str(), "MultiLine");
        assert_eq!(CommentStyle::DocComment.as_str(), "DocComment");
        assert_eq!(CommentStyle::ModuleComment.as_str(), "ModuleComment");
    }

    #[test]
    fn test_single_line_comment() {
        let loc = Location::in_file("test.res");
        let comment = Comment::make_single_line(loc, "this is a comment".to_string());

        assert!(comment.is_single_line());
        assert!(!comment.is_doc_comment());
        assert_eq!(comment.txt(), "this is a comment");
    }

    #[test]
    fn test_multi_line_comment() {
        let loc = Location::in_file("test.res");
        let comment = Comment::make_multi_line(loc, "multi\nline".to_string(), false, false);

        assert!(!comment.is_single_line());
        assert!(!comment.is_doc_comment());
        assert_eq!(comment.style, CommentStyle::MultiLine);
    }

    #[test]
    fn test_doc_comment() {
        let loc = Location::in_file("test.res");
        let comment = Comment::make_multi_line(loc, "documentation".to_string(), true, false);

        assert!(comment.is_doc_comment());
        assert!(!comment.is_module_comment());
    }

    #[test]
    fn test_module_comment() {
        let loc = Location::in_file("test.res");
        let comment = Comment::make_multi_line(loc, "module doc".to_string(), true, true);

        assert!(!comment.is_doc_comment());
        assert!(comment.is_module_comment());
    }

    #[test]
    fn test_trim_spaces() {
        assert_eq!(Comment::trim_spaces("  hello  "), "hello");
        assert_eq!(Comment::trim_spaces("hello"), "hello");
        assert_eq!(Comment::trim_spaces(""), "");
        assert_eq!(Comment::trim_spaces("   "), "");
    }

    #[test]
    fn test_comment_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Comment>();
        assert_send_sync::<CommentStyle>();
    }
}
