//! Parser diagnostics for error reporting.
//!
//! This module provides types for representing parsing errors and warnings,
//! including location information and contextual hints.

use crate::location::{Location, Position};
use serde::{Deserialize, Serialize};

use super::grammar::Grammar;
use super::token::Token;

/// A category of diagnostic message.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DiagnosticCategory {
    /// An unexpected token was encountered.
    Unexpected {
        /// The unexpected token.
        token: Token,
        /// The parsing context (stack of grammar rules being parsed).
        context: Vec<(Grammar, Position)>,
    },
    /// An expected token was not found.
    Expected {
        /// The expected grammar context.
        context: Option<Grammar>,
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
            DiagnosticCategory::Unexpected { token, context } => {
                explain_unexpected(token, context)
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
                // OCaml has a typo in this message - missing backtick after "user"
                _ => "I'm expecting a lowercase name like `user or `age`".to_string(),
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

/// Default message for unexpected tokens.
fn default_unexpected(token: &Token) -> String {
    format!(
        "I'm not sure what to parse here when looking at \"{}\".",
        token
    )
}

/// Explain an unexpected token based on parsing context (breadcrumbs).
/// This matches OCaml's explain function in res_diagnostics.ml.
///
/// Note: In OCaml, breadcrumbs are stored with the most recent at the head (prepended).
/// In Rust, we append to Vec, so the most recent breadcrumb is at the END.
/// We need to iterate from the end to match OCaml's pattern matching behavior.
fn explain_unexpected(token: &Token, context: &[(Grammar, Position)]) -> String {
    use super::grammar;

    // Check if token is a reserved keyword first (OCaml does this at the end, but it's the same)
    let reserved_keyword = |t: &Token| {
        let name = format!("{}", t);
        format!(
            "`{}` is a reserved keyword. Keywords need to be escaped: \\\"{}\"",
            name, name
        )
    };

    if context.is_empty() {
        if token.is_keyword() {
            return reserved_keyword(token);
        }
        return default_unexpected(token);
    }

    // OCaml uses (head :: rest) pattern, where head is most recent.
    // In Rust Vec, most recent is at the end, so we use last() and slice before it.
    let (most_recent, _) = context.last().unwrap();
    let rest = &context[..context.len() - 1];

    // Helper to get the "next" breadcrumb (second most recent)
    let next_grammar = rest.last().map(|(g, _)| g);

    match most_recent {
        // AtomicTypExpr context
        Grammar::AtomicTypExpr => {
            // Check next grammar context
            if let Some(next) = next_grammar {
                match (next, token) {
                    // In record field declarations
                    (Grammar::StringFieldDeclarations | Grammar::FieldDeclarations,
                     Token::String(_) | Token::At | Token::Rbrace | Token::Comma | Token::Eof) => {
                        return "I'm missing a type here".to_string();
                    }
                    _ => {}
                }
            }
            // Check if token is a structure item start or EOF
            if grammar::is_structure_item_start(token) || *token == Token::Eof {
                return "Missing a type here".to_string();
            }
            default_unexpected(token)
        }

        // ExprOperand context
        Grammar::ExprOperand => {
            if let Some(next) = next_grammar {
                match (next, token) {
                    // Empty expression block
                    (Grammar::ExprBlock, Token::Rbrace) => {
                        return "It seems that this expression block is empty".to_string();
                    }
                    // Pattern matching
                    (Grammar::ExprBlock, Token::Bar) => {
                        return "Looks like there might be an expression missing here".to_string();
                    }
                    // Record field mutation
                    (Grammar::ExprSetField, _) => {
                        return "It seems that this record field mutation misses an expression".to_string();
                    }
                    // Array mutation
                    (Grammar::ExprArrayMutation, _) => {
                        return "Seems that an expression is missing, with what do I mutate the array?".to_string();
                    }
                    // After binary operator or unary
                    (Grammar::ExprBinaryAfterOp(_) | Grammar::ExprUnary, _) => {
                        return "Did you forget to write an expression here?".to_string();
                    }
                    // Let binding
                    (Grammar::LetBinding, _) => {
                        return "This let-binding misses an expression".to_string();
                    }
                    _ => {}
                }
            }
            // Check rest of the context (has remaining breadcrumbs)
            if !rest.is_empty() && matches!(token, Token::Rbracket | Token::Rbrace | Token::Eof) {
                return "Missing expression".to_string();
            }
            default_unexpected(token)
        }

        // TypeParam context
        Grammar::TypeParam => {
            if let Token::Lident(ident) = token {
                return format!("Did you mean '{}? A Type parameter starts with a quote.", ident);
            }
            default_unexpected(token)
        }

        // Pattern context
        Grammar::Pattern => {
            if let Some(next) = next_grammar {
                match (token, next) {
                    // Missing name in let binding
                    (Token::Equal, Grammar::LetBinding) => {
                        return "I was expecting a name for this let-binding. Example: `let message = \"hello\"`".to_string();
                    }
                    // For loop
                    (Token::In, Grammar::ExprFor) => {
                        return "A for-loop has the following form: `for i in 0 to 10`. Did you forget to supply a name before `in`?".to_string();
                    }
                    // Pattern match case
                    (Token::EqualGreater, Grammar::PatternMatchCase) => {
                        return "I was expecting a pattern to match on before the `=>`".to_string();
                    }
                    _ => {}
                }
            }
            if token.is_keyword() {
                return reserved_keyword(token);
            }
            default_unexpected(token)
        }

        // Default case
        _ => {
            if token.is_keyword() {
                return reserved_keyword(token);
            }
            default_unexpected(token)
        }
    }
}

/// Convenience constructors for diagnostic categories.
impl DiagnosticCategory {
    /// Create an unexpected token diagnostic.
    pub fn unexpected(token: Token, context: Vec<(Grammar, Position)>) -> Self {
        DiagnosticCategory::Unexpected { token, context }
    }

    /// Create an expected token diagnostic.
    pub fn expected(token: Token, pos: Position, context: Option<Grammar>) -> Self {
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
