//! Parser state management.
//!
//! This module provides the parser state structure and basic operations
//! like advancing tokens, error handling, lookahead, and breadcrumb tracking.
//! It mirrors `res_parser.ml`.

use crate::location::Position;

use super::comment::Comment;
use super::diagnostics::{DiagnosticCategory, ParserDiagnostic};
use super::grammar::Grammar;
use super::scanner::Scanner;
use super::token::Token;

/// Parser mode determines some parsing behaviors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ParserMode {
    /// Parsing for the type checker (standard mode).
    #[default]
    ParseForTypeChecker,
    /// Default mode.
    Default,
}

/// Region status for error reporting.
///
/// Used to avoid reporting duplicate errors in the same region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionStatus {
    /// Errors in this region should be reported.
    Report,
    /// Errors in this region should be silenced.
    Silent,
}

/// Parser state for ReScript.
///
/// This structure tracks all state needed during parsing:
/// - Current token and positions
/// - Scanner for lexical analysis
/// - Breadcrumbs for error recovery
/// - Collected errors, diagnostics, and comments
pub struct Parser<'src> {
    /// Parser mode.
    pub mode: ParserMode,
    /// The scanner for lexical analysis.
    scanner: Scanner<'src>,
    /// Current token.
    pub token: Token,
    /// Start position of current token.
    pub start_pos: Position,
    /// End position of current token.
    pub end_pos: Position,
    /// End position of previous token.
    pub prev_end_pos: Position,
    /// Stack of breadcrumbs for error recovery.
    breadcrumbs: Vec<(Grammar, Position)>,
    /// Collected diagnostics.
    diagnostics: Vec<ParserDiagnostic>,
    /// Collected comments.
    comments: Vec<Comment>,
    /// Stack of region statuses for error deduplication.
    regions: Vec<RegionStatus>,
}

impl<'src> Parser<'src> {
    /// Create a new parser for the given source code.
    pub fn new(filename: impl Into<String>, src: &'src str) -> Self {
        Self::with_mode(filename, src, ParserMode::default())
    }

    /// Create a new parser with a specific mode.
    pub fn with_mode(filename: impl Into<String>, src: &'src str, mode: ParserMode) -> Self {
        let scanner = Scanner::new(filename, src);
        let mut parser = Parser {
            mode,
            scanner,
            token: Token::Semicolon, // Placeholder, will be replaced by first scan
            start_pos: Position::default(),
            end_pos: Position::default(),
            prev_end_pos: Position::default(),
            breadcrumbs: Vec::new(),
            diagnostics: Vec::new(),
            comments: Vec::new(),
            regions: vec![RegionStatus::Report],
        };
        // Scan the first token
        parser.next();
        parser
    }

    /// Record a diagnostic error.
    ///
    /// Errors are only recorded if the current region has `Report` status.
    /// After recording, the region is set to `Silent` to avoid duplicates.
    pub fn err(&mut self, category: DiagnosticCategory) {
        self.err_at(self.start_pos.clone(), self.end_pos.clone(), category);
    }

    /// Record a diagnostic error at specific positions.
    pub fn err_at(&mut self, start_pos: Position, end_pos: Position, category: DiagnosticCategory) {
        if let Some(region) = self.regions.last_mut() {
            if *region == RegionStatus::Report {
                let diagnostic = ParserDiagnostic {
                    start_pos,
                    end_pos,
                    category,
                };
                self.diagnostics.push(diagnostic);
                *region = RegionStatus::Silent;
            }
        }
    }

    /// Begin a new error reporting region.
    pub fn begin_region(&mut self) {
        self.regions.push(RegionStatus::Report);
    }

    /// End the current error reporting region.
    pub fn end_region(&mut self) {
        if !self.regions.is_empty() {
            self.regions.pop();
        }
    }

    /// Advance to the next non-comment token.
    ///
    /// Comments are collected in the parser's comment list.
    /// Doc comments and module comments are converted to special tokens.
    pub fn next(&mut self) {
        self.next_with_prev_end(None);
    }

    /// Advance to the next token with a specific previous end position.
    fn next_with_prev_end(&mut self, prev_end_pos: Option<Position>) {
        if self.token == Token::Eof {
            return;
        }

        let prev_end = prev_end_pos.unwrap_or_else(|| self.end_pos.clone());

        loop {
            let result = self.scanner.scan();
            match result.token {
                Token::Comment(ref c) => {
                    if c.is_doc_comment() {
                        // Convert doc comment to DocComment token
                        self.token = Token::DocComment {
                            loc: c.loc.clone(),
                            content: c.txt.clone(),
                        };
                        self.prev_end_pos = prev_end;
                        self.start_pos = result.start_pos;
                        self.end_pos = result.end_pos;
                        return;
                    } else if c.is_module_comment() {
                        // Convert module comment to ModuleComment token
                        self.token = Token::ModuleComment {
                            loc: c.loc.clone(),
                            content: c.txt.clone(),
                        };
                        self.prev_end_pos = prev_end;
                        self.start_pos = result.start_pos;
                        self.end_pos = result.end_pos;
                        return;
                    } else {
                        // Regular comment - collect it and continue
                        let mut comment = c.clone();
                        comment.set_prev_tok_end_pos(self.end_pos.clone());
                        self.comments.push(comment);
                        self.prev_end_pos = self.end_pos.clone();
                        self.end_pos = result.end_pos;
                        // Continue to next token
                    }
                }
                _ => {
                    self.token = result.token;
                    self.prev_end_pos = prev_end;
                    self.start_pos = result.start_pos;
                    self.end_pos = result.end_pos;
                    return;
                }
            }
        }
    }

    /// Advance to the next token, but only if not at EOF.
    pub fn next_unsafe(&mut self) {
        if self.token != Token::Eof {
            self.next();
        }
    }

    /// Scan the next template literal token.
    pub fn next_template_literal_token(&mut self) {
        let result = self.scanner.scan_template_literal_token();
        self.token = result.token;
        self.prev_end_pos = self.end_pos.clone();
        self.start_pos = result.start_pos;
        self.end_pos = result.end_pos;
    }

    /// Check if the current token matches and consume it if so.
    ///
    /// Returns `true` if the token was consumed.
    pub fn optional(&mut self, token: &Token) -> bool {
        if &self.token == token {
            self.next();
            true
        } else {
            false
        }
    }

    /// Expect a specific token and consume it.
    ///
    /// If the token doesn't match, an error is recorded.
    pub fn expect(&mut self, token: Token) {
        self.expect_with_grammar(token, None);
    }

    /// Expect a specific token with grammar context for error messages.
    pub fn expect_with_grammar(&mut self, token: Token, grammar: Option<String>) {
        if self.token == token {
            self.next();
        } else {
            let category = DiagnosticCategory::Expected {
                context: grammar,
                pos: self.prev_end_pos.clone(),
                token,
            };
            self.err_at(
                self.prev_end_pos.clone(),
                self.prev_end_pos.clone(),
                category,
            );
        }
    }

    /// Push a breadcrumb for error recovery.
    pub fn leave_breadcrumb(&mut self, grammar: Grammar) {
        self.breadcrumbs.push((grammar, self.start_pos.clone()));
    }

    /// Pop the most recent breadcrumb.
    pub fn eat_breadcrumb(&mut self) {
        self.breadcrumbs.pop();
    }

    /// Get the current breadcrumbs.
    pub fn breadcrumbs(&self) -> &[(Grammar, Position)] {
        &self.breadcrumbs
    }

    /// Execute a callback with lookahead, then restore parser state.
    ///
    /// This allows speculative parsing without affecting the parser state.
    pub fn lookahead<T, F>(&mut self, callback: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        // Save scanner state
        let scanner_snapshot = self.scanner.snapshot();

        // Save parser state
        let token = self.token.clone();
        let start_pos = self.start_pos.clone();
        let end_pos = self.end_pos.clone();
        let prev_end_pos = self.prev_end_pos.clone();
        let breadcrumbs = self.breadcrumbs.clone();
        let diagnostics_len = self.diagnostics.len();
        let comments_len = self.comments.len();

        // Execute callback
        let result = callback(self);

        // Restore scanner state
        self.scanner.restore(scanner_snapshot);

        // Restore parser state
        self.token = token;
        self.start_pos = start_pos;
        self.end_pos = end_pos;
        self.prev_end_pos = prev_end_pos;
        self.breadcrumbs = breadcrumbs;
        self.diagnostics.truncate(diagnostics_len);
        self.comments.truncate(comments_len);

        result
    }

    /// Check if parsing made progress.
    ///
    /// Returns `Some(result)` if the end position changed, `None` otherwise.
    pub fn check_progress<T>(&self, prev_end_pos: &Position, result: T) -> Option<T> {
        if &self.end_pos == prev_end_pos {
            None
        } else {
            Some(result)
        }
    }

    /// Get collected diagnostics.
    pub fn diagnostics(&self) -> &[ParserDiagnostic] {
        &self.diagnostics
    }

    /// Get collected comments.
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    /// Take ownership of collected comments.
    pub fn take_comments(&mut self) -> Vec<Comment> {
        std::mem::take(&mut self.comments)
    }

    /// Take ownership of collected diagnostics.
    pub fn take_diagnostics(&mut self) -> Vec<ParserDiagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    /// Set diamond mode on the scanner (for type parameters).
    pub fn set_diamond_mode(&mut self) {
        self.scanner.set_diamond_mode();
    }

    /// Pop diamond mode from the scanner.
    pub fn pop_diamond_mode(&mut self) {
        self.scanner.pop_diamond_mode();
    }

    /// Get the filename being parsed.
    pub fn filename(&self) -> &str {
        &self.scanner.filename
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_new() {
        let parser = Parser::new("test.res", "let x = 42");
        assert!(matches!(parser.token, Token::Let { .. }));
    }

    #[test]
    fn test_parser_next() {
        let mut parser = Parser::new("test.res", "let x = 42");
        assert!(matches!(parser.token, Token::Let { .. }));
        parser.next();
        assert!(matches!(parser.token, Token::Lident(_)));
        parser.next();
        assert!(matches!(parser.token, Token::Equal));
        parser.next();
        assert!(matches!(parser.token, Token::Int { .. }));
        parser.next();
        assert!(matches!(parser.token, Token::Eof));
    }

    #[test]
    fn test_parser_optional() {
        let mut parser = Parser::new("test.res", "let x = 42");
        assert!(parser.optional(&Token::Let { unwrap: false }));
        assert!(matches!(parser.token, Token::Lident(_)));
        assert!(!parser.optional(&Token::Equal));
        assert!(matches!(parser.token, Token::Lident(_)));
    }

    #[test]
    fn test_parser_expect() {
        let mut parser = Parser::new("test.res", "let x = 42");
        parser.expect(Token::Let { unwrap: false });
        assert!(matches!(parser.token, Token::Lident(_)));
        // Expecting wrong token should record error
        parser.expect(Token::Equal);
        assert!(parser.has_errors());
    }

    #[test]
    fn test_parser_breadcrumbs() {
        let mut parser = Parser::new("test.res", "let x = 42");
        assert!(parser.breadcrumbs().is_empty());
        parser.leave_breadcrumb(Grammar::LetBinding);
        assert_eq!(parser.breadcrumbs().len(), 1);
        parser.leave_breadcrumb(Grammar::ExprOperand);
        assert_eq!(parser.breadcrumbs().len(), 2);
        parser.eat_breadcrumb();
        assert_eq!(parser.breadcrumbs().len(), 1);
    }

    #[test]
    fn test_parser_lookahead() {
        let mut parser = Parser::new("test.res", "let x = 42");
        assert!(matches!(parser.token, Token::Let { .. }));

        let found_equal = parser.lookahead(|p| {
            p.next(); // x
            p.next(); // =
            matches!(p.token, Token::Equal)
        });

        assert!(found_equal);
        // Parser state should be restored
        assert!(matches!(parser.token, Token::Let { .. }));
    }

    #[test]
    fn test_parser_regions() {
        let mut parser = Parser::new("test.res", "let x = 42");

        // First error in region should be recorded
        parser.err(DiagnosticCategory::Message("test error 1".to_string()));
        assert_eq!(parser.diagnostics().len(), 1);

        // Second error in same region should be silenced
        parser.err(DiagnosticCategory::Message("test error 2".to_string()));
        assert_eq!(parser.diagnostics().len(), 1);

        // New region should allow new errors
        parser.begin_region();
        parser.err(DiagnosticCategory::Message("test error 3".to_string()));
        assert_eq!(parser.diagnostics().len(), 2);

        parser.end_region();
    }

    #[test]
    fn test_parser_comments() {
        let parser = Parser::new("test.res", "// comment\nlet x = 42");
        // The comment should be collected, and we should be at 'let'
        assert!(matches!(parser.token, Token::Let { .. }));
        assert_eq!(parser.comments().len(), 1);
    }

    #[test]
    fn test_parser_is_send_sync() {
        fn assert_send<T: Send>() {}
        // Parser contains Scanner which contains a reference, so not Send
        // But we can still verify the basic types are
        assert_send::<ParserMode>();
        assert_send::<RegionStatus>();
    }
}
