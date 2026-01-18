//! Parser diagnostics for error reporting.
//!
//! This module provides types for representing parsing errors and warnings,
//! including location information and contextual hints.

use crate::location::{Location, Position};
use serde::{Deserialize, Serialize};

use super::token::Token;

/// A category of diagnostic message.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DiagnosticCategory {
    /// An unexpected token was encountered.
    Unexpected {
        /// The unexpected token.
        token: Token,
        /// The parsing context (stack of grammar rules being parsed).
        context: Vec<(String, Position)>,
    },
    /// An expected token was not found.
    Expected {
        /// The expected grammar context.
        context: Option<String>,
        /// Position of the previous token.
        pos: Position,
        /// The expected token.
        token: Token,
    },
    /// A general message.
    Message(String),
    /// Expected an uppercase identifier.
    Uident(Token),
    /// Expected a lowercase identifier.
    Lident(Token),
    /// Unclosed string literal.
    UnclosedString,
    /// Unclosed template literal.
    UnclosedTemplate,
    /// Unclosed comment.
    UnclosedComment,
    /// Unknown character.
    UnknownUchar(char),
}

/// A parser diagnostic with location information.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParserDiagnostic {
    /// Start position of the diagnostic.
    pub start_pos: Position,
    /// End position of the diagnostic.
    pub end_pos: Position,
    /// The diagnostic category.
    pub category: DiagnosticCategory,
}

impl ParserDiagnostic {
    /// Create a new parser diagnostic.
    pub fn new(start_pos: Position, end_pos: Position, category: DiagnosticCategory) -> Self {
        Self {
            start_pos,
            end_pos,
            category,
        }
    }

    /// Create a diagnostic from a location.
    pub fn from_location(loc: &Location, category: DiagnosticCategory) -> Self {
        Self {
            start_pos: loc.loc_start.clone(),
            end_pos: loc.loc_end.clone(),
            category,
        }
    }

    /// Get the start position.
    pub fn start_pos(&self) -> &Position {
        &self.start_pos
    }

    /// Get the end position.
    pub fn end_pos(&self) -> &Position {
        &self.end_pos
    }

    /// Get the location as a Location struct.
    pub fn location(&self) -> Location {
        Location::from_positions(self.start_pos.clone(), self.end_pos.clone())
    }

    /// Explain the diagnostic as a human-readable message.
    pub fn explain(&self) -> String {
        match &self.category {
            DiagnosticCategory::Unexpected { token, context: _ } => {
                if token.is_keyword() {
                    let name = format!("{}", token);
                    format!(
                        "`{}` is a reserved keyword. Keywords need to be escaped: \\\"{}\"",
                        name, name
                    )
                } else {
                    format!(
                        "I'm not sure what to parse here when looking at \"{}\".",
                        token
                    )
                }
            }
            DiagnosticCategory::Expected { token, context, .. } => {
                let hint = match context {
                    Some(grammar) => format!(" It signals the start of {}", grammar),
                    None => String::new(),
                };
                format!("Did you forget a `{}` here?{}", token, hint)
            }
            DiagnosticCategory::Message(txt) => txt.clone(),
            DiagnosticCategory::Uident(current_token) => match current_token {
                Token::Lident(lident) => {
                    let guess = capitalize_first(lident);
                    format!("Did you mean `{}` instead of `{}`?", guess, lident)
                }
                t if t.is_keyword() => {
                    format!("`{}` is a reserved keyword.", t)
                }
                _ => "At this point, I'm looking for an uppercased name like `Belt` or `Array`"
                    .to_string(),
            },
            DiagnosticCategory::Lident(current_token) => match current_token {
                Token::Uident(uident) => {
                    let guess = uncapitalize_first(uident);
                    format!("Did you mean `{}` instead of `{}`?", guess, uident)
                }
                t if t.is_keyword() => {
                    let token = format!("{}", t);
                    format!(
                        "`{}` is a reserved keyword. Keywords need to be escaped: \\\"{}\"",
                        token, token
                    )
                }
                Token::Underscore => "`_` isn't a valid name.".to_string(),
                _ => "I'm expecting a lowercase name like `user` or `age`".to_string(),
            },
            DiagnosticCategory::UnclosedString => {
                "This string is missing a double quote at the end".to_string()
            }
            DiagnosticCategory::UnclosedTemplate => {
                "Did you forget to close this template expression with a backtick?".to_string()
            }
            DiagnosticCategory::UnclosedComment => {
                "This comment seems to be missing a closing `*/`".to_string()
            }
            DiagnosticCategory::UnknownUchar(ch) => {
                format!("Not sure what to do with this character: \"{}\".", ch)
            }
        }
    }
}

/// Capitalize the first letter of a string.
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Uncapitalize the first letter of a string.
fn uncapitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_lowercase().collect::<String>() + chars.as_str(),
    }
}

/// Convenience constructors for diagnostic categories.
impl DiagnosticCategory {
    /// Create an unexpected token diagnostic.
    pub fn unexpected(token: Token, context: Vec<(String, Position)>) -> Self {
        DiagnosticCategory::Unexpected { token, context }
    }

    /// Create an expected token diagnostic.
    pub fn expected(token: Token, pos: Position, context: Option<String>) -> Self {
        DiagnosticCategory::Expected {
            token,
            pos,
            context,
        }
    }

    /// Create a message diagnostic.
    pub fn message(txt: impl Into<String>) -> Self {
        DiagnosticCategory::Message(txt.into())
    }

    /// Create an unclosed string diagnostic.
    pub fn unclosed_string() -> Self {
        DiagnosticCategory::UnclosedString
    }

    /// Create an unclosed template diagnostic.
    pub fn unclosed_template() -> Self {
        DiagnosticCategory::UnclosedTemplate
    }

    /// Create an unclosed comment diagnostic.
    pub fn unclosed_comment() -> Self {
        DiagnosticCategory::UnclosedComment
    }

    /// Create an unknown character diagnostic.
    pub fn unknown_uchar(ch: char) -> Self {
        DiagnosticCategory::UnknownUchar(ch)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unclosed_string() {
        let diag = ParserDiagnostic::new(
            Position::default(),
            Position::default(),
            DiagnosticCategory::unclosed_string(),
        );
        assert!(diag.explain().contains("missing a double quote"));
    }

    #[test]
    fn test_unexpected_keyword() {
        let diag = ParserDiagnostic::new(
            Position::default(),
            Position::default(),
            DiagnosticCategory::unexpected(Token::If, vec![]),
        );
        assert!(diag.explain().contains("reserved keyword"));
    }

    #[test]
    fn test_capitalize() {
        assert_eq!(capitalize_first("hello"), "Hello");
        assert_eq!(capitalize_first(""), "");
        assert_eq!(capitalize_first("A"), "A");
    }

    #[test]
    fn test_uncapitalize() {
        assert_eq!(uncapitalize_first("Hello"), "hello");
        assert_eq!(uncapitalize_first(""), "");
        assert_eq!(uncapitalize_first("a"), "a");
    }
}
