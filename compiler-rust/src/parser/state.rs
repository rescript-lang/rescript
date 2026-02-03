//! Parser state management.
//!
//! This module provides the parser state structure and basic operations
//! like advancing tokens, error handling, lookahead, and breadcrumb tracking.
//! It mirrors `res_parser.ml`.

use std::sync::atomic::{AtomicU32, Ordering};

use crate::intern::StrIdx;
use crate::location::{Location, Position};
use crate::parse_arena::{LidentIdx, LocIdx, ParseArena, PosIdx};

use super::longident::Longident;

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

/// Snapshot of parser state for backtracking.
///
/// Used by `parse_attributes_and_binding` to speculatively parse attributes
/// and restore state if they're not followed by `and`.
pub struct ParserSnapshot {
    scanner_snapshot: super::scanner::ScannerSnapshot,
    token: Token,
    start_pos: Position,
    end_pos: Position,
    prev_end_pos: Position,
    breadcrumbs: Vec<(Grammar, Position)>,
    diagnostics_len: usize,
    comments_len: usize,
    location_id_counter: u32,
    /// Arena position count at snapshot time (for truncation on restore).
    arena_positions_len: usize,
    /// Arena location count at snapshot time (for truncation on restore).
    arena_locations_len: usize,
}

/// Parser state for ReScript.
///
/// This structure tracks all state needed during parsing:
/// - Current token and positions
/// - Scanner for lexical analysis
/// - Breadcrumbs for error recovery
/// - Collected errors, diagnostics, and comments
/// - Arena for interned positions and locations
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
    /// Counter for generating unique LocationIds (legacy, for Location-based code).
    /// Each call to `mk_loc` gets a new ID, enabling identity-based sharing in Marshal.
    /// Uses atomic to allow mk_loc to take &self (avoiding borrow conflicts).
    location_id_counter: AtomicU32,
    /// Arena for interned positions and locations.
    /// Used for efficient location storage with LocIdx/PosIdx indices.
    arena: ParseArena,
    /// Whether we're currently parsing an external definition.
    /// Used for arity calculation of @as(_) labeled parameters.
    pub in_external: bool,
    /// Cache for prev_end_pos sharing: (position value, arena index).
    /// When multiple locations end at the same prev_end_pos, they share the same PosIdx.
    /// This is cleared when prev_end_pos changes (on next token consumption).
    cached_prev_end_pos_idx: Option<PosIdx>,
    /// Current recursion depth for JSX parsing.
    /// Used to prevent stack overflow from deeply nested JSX elements.
    pub jsx_depth: usize,
    /// General parse depth counter to prevent stack overflow from deeply nested parsing.
    /// Incremented at major parsing entry points like parse_expr, parse_typ_expr, etc.
    pub parse_depth: usize,
}

impl<'src> Parser<'src> {
    /// Create a new parser for the given source code.
    pub fn new(filename: impl Into<String>, src: &'src str) -> Self {
        Self::with_mode(filename, src, ParserMode::default())
    }

    /// Create a new parser with a specific mode.
    pub fn with_mode(filename: impl Into<String>, src: &'src str, mode: ParserMode) -> Self {
        // Create arena first so we can intern the filename
        let mut arena = ParseArena::new();
        // Intern the filename - this StrIdx will be shared by all positions in this file
        let filename_idx = arena.intern_string(&filename.into());
        let scanner = Scanner::new(filename_idx, src);
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
            location_id_counter: AtomicU32::new(1), // Start at 1, 0 is reserved for default/uninitialized
            arena,
            in_external: false,
            cached_prev_end_pos_idx: None,
            jsx_depth: 0,
            parse_depth: 0,
        };
        // Scan the first token
        parser.next();
        parser
    }

    /// Create a location from positions and push to arena.
    ///
    /// Returns a LocIdx that can be used to look up the location later.
    /// Note: Positions must be cloned before calling this to avoid borrow conflicts.
    pub fn mk_loc(&mut self, start: &Position, end: &Position) -> LocIdx {
        self.arena.mk_loc_from_positions(start, end)
    }

    /// Create a location from start position to prev_end_pos.
    /// This is a common pattern that avoids borrow conflicts.
    /// Uses cached prev_end_pos_idx for the end position to enable sharing when
    /// multiple locations have the same end position (matching OCaml's behavior
    /// where the same p.prev_end_pos variable is reused).
    pub fn mk_loc_to_prev_end(&mut self, start: &Position) -> LocIdx {
        let start_idx = self.arena.push_position(*start);
        let end_idx = self.get_or_push_prev_end_pos();
        self.arena.mk_loc(start_idx, end_idx)
    }

    /// Create a location from start position to end_pos.
    /// This is a common pattern that avoids borrow conflicts.
    pub fn mk_loc_to_end(&mut self, start: &Position) -> LocIdx {
        let end = self.end_pos.clone();
        self.arena.mk_loc_from_positions(start, &end)
    }

    /// Create a location for the current token (start_pos to end_pos).
    pub fn mk_loc_current(&mut self) -> LocIdx {
        let start = self.start_pos.clone();
        let end = self.end_pos.clone();
        self.arena.mk_loc_from_positions(&start, &end)
    }

    /// Create a location from start_pos to prev_end_pos.
    /// Uses cached prev_end_pos_idx for the end position to enable sharing.
    pub fn mk_loc_start_to_prev_end(&mut self) -> LocIdx {
        let start_idx = self.arena.push_position(self.start_pos);
        let end_idx = self.get_or_push_prev_end_pos();
        self.arena.mk_loc(start_idx, end_idx)
    }

    // ========== Arena-based location methods ==========

    /// Get the internal ParseArena for marshalling.
    /// Only use this after parsing is complete.
    pub fn into_arena(self) -> ParseArena {
        self.arena
    }

    /// Get a reference to the parse arena.
    pub fn arena(&self) -> &ParseArena {
        &self.arena
    }

    /// Get a mutable reference to the parse arena.
    pub fn arena_mut(&mut self) -> &mut ParseArena {
        &mut self.arena
    }

    /// Push a position into the arena and return its index.
    pub fn push_pos(&mut self, pos: Position) -> PosIdx {
        self.arena.push_position(pos)
    }

    /// Push the current start position into the arena.
    pub fn push_start_pos(&mut self) -> PosIdx {
        self.arena.push_position(self.start_pos.clone())
    }

    /// Push the current end position into the arena.
    pub fn push_end_pos(&mut self) -> PosIdx {
        self.arena.push_position(self.end_pos.clone())
    }

    /// Push the previous end position into the arena.
    /// Uses cache for sharing when multiple locations end at the same point.
    pub fn push_prev_end_pos(&mut self) -> PosIdx {
        self.get_or_push_prev_end_pos()
    }

    /// Create and push a location from position indices.
    pub fn mk_loc_idx(&mut self, start: PosIdx, end: PosIdx) -> LocIdx {
        self.arena.mk_loc(start, end)
    }

    /// Create a location from a start PosIdx to prev_end_pos.
    /// This is the PosIdx-based equivalent of mk_loc_to_prev_end.
    /// Uses cached prev_end_pos PosIdx if available, otherwise pushes and caches.
    pub fn mk_loc_idx_to_prev_end(&mut self, start: PosIdx) -> LocIdx {
        let end = self.get_or_push_prev_end_pos();
        self.arena.mk_loc(start, end)
    }

    /// Get or push the prev_end_pos, using cache for sharing.
    /// When multiple locations end at the same prev_end_pos (before consuming more tokens),
    /// they will share the same PosIdx in the marshal output.
    fn get_or_push_prev_end_pos(&mut self) -> PosIdx {
        if let Some(cached_idx) = self.cached_prev_end_pos_idx {
            cached_idx
        } else {
            let idx = self.arena.push_position(self.prev_end_pos);
            self.cached_prev_end_pos_idx = Some(idx);
            idx
        }
    }

    /// Create a location from start and end PosIdx values.
    /// Both positions are reused, enabling full position sharing.
    pub fn mk_loc_idx_full(&mut self, start: PosIdx, end: PosIdx) -> LocIdx {
        self.arena.mk_loc(start, end)
    }

    /// Push prev_end_pos and return both the location and the end PosIdx.
    /// Useful when the end position needs to be reused by multiple locations.
    /// Uses cache for sharing.
    pub fn mk_loc_idx_to_prev_end_with_end(&mut self, start: PosIdx) -> (LocIdx, PosIdx) {
        let end = self.get_or_push_prev_end_pos();
        let loc = self.arena.mk_loc(start, end);
        (loc, end)
    }

    /// Create a location with the start and ghost flag from an existing location,
    /// but with end set to prev_end_pos. This is equivalent to OCaml's
    /// `{loc with loc_end = p.prev_end_pos}`.
    pub fn mk_loc_extend_to_prev_end(&mut self, loc: LocIdx) -> LocIdx {
        let loc_info = self.arena.get_location(loc);
        let start_idx = loc_info.loc_start;
        let is_ghost = loc_info.loc_ghost;
        let end_idx = self.get_or_push_prev_end_pos();
        self.arena.push_loc(start_idx, end_idx, is_ghost)
    }

    /// Create and push a location from raw positions.
    pub fn mk_loc_from_positions(&mut self, start: &Position, end: &Position) -> LocIdx {
        self.arena.mk_loc_from_positions(start, end)
    }

    /// Get the "none" location index.
    pub fn none_loc(&self) -> LocIdx {
        self.arena.none_loc()
    }

    /// Get the start position of a location (cloned).
    pub fn loc_start(&self, loc: LocIdx) -> Position {
        self.arena.loc_start(loc).clone()
    }

    /// Get the end position of a location (cloned).
    pub fn loc_end(&self, loc: LocIdx) -> Position {
        self.arena.loc_end(loc).clone()
    }

    /// Get the start position index of a location.
    pub fn loc_start_idx(&self, loc: LocIdx) -> PosIdx {
        self.arena.loc_start_idx(loc)
    }

    /// Get the end position index of a location.
    pub fn loc_end_idx(&self, loc: LocIdx) -> PosIdx {
        self.arena.loc_end_idx(loc)
    }

    /// Check if a location is ghost.
    pub fn loc_ghost(&self, loc: LocIdx) -> bool {
        self.arena.loc_ghost(loc)
    }

    /// Convert a LocIdx to a full Location struct.
    pub fn to_location(&self, loc: LocIdx) -> Location {
        self.arena.to_location(loc)
    }

    /// Convert a full Location struct to LocIdx (push to arena).
    /// Use this when converting from Token locations (which use crate::location::Location)
    /// to AST locations (which use LocIdx).
    pub fn from_location(&mut self, loc: &Location) -> LocIdx {
        self.arena.from_location(loc)
    }

    /// Create a location spanning from one location's start to another's end.
    pub fn mk_loc_spanning(&mut self, start_loc: LocIdx, end_loc: LocIdx) -> LocIdx {
        self.arena.mk_loc_spanning(start_loc, end_loc)
    }

    /// Like mk_loc_spanning, but preserves the ghost flag from the start location.
    pub fn mk_loc_spanning_preserve_ghost(&mut self, start_loc: LocIdx, end_loc: LocIdx) -> LocIdx {
        self.arena.mk_loc_spanning_preserve_ghost(start_loc, end_loc)
    }

    /// Create a location from a start Position to another location's end.
    /// Useful pattern: `p.mk_loc_to_end_of(&start_pos, body.some_loc)`
    /// Reuses the end location's PosIdx for position sharing.
    pub fn mk_loc_to_end_of(&mut self, start: &Position, end_loc: LocIdx) -> LocIdx {
        let start_idx = self.arena.push_position(*start);
        let end_idx = self.arena.loc_end_idx(end_loc);
        self.arena.mk_loc(start_idx, end_idx)
    }

    /// Create a location from another location's start to an end Position.
    /// Useful pattern: `p.mk_loc_from_start_of(body.some_loc, &end_pos)`
    /// Reuses the start location's PosIdx for position sharing.
    pub fn mk_loc_from_start_of(&mut self, start_loc: LocIdx, end: &Position) -> LocIdx {
        let start_idx = self.arena.loc_start_idx(start_loc);
        let end_idx = self.arena.push_position(*end);
        self.arena.mk_loc(start_idx, end_idx)
    }

    // ========== Longident arena methods ==========

    /// Push a longident into the arena and return its index.
    pub fn push_longident(&mut self, lid: Longident) -> LidentIdx {
        self.arena.push_longident(lid)
    }

    /// Push a simple identifier (Lident) into the arena.
    pub fn push_lident(&mut self, name: impl Into<String>) -> LidentIdx {
        self.arena.push_lident(name)
    }

    /// Push a simple identifier (Lident) with a static/interned string.
    /// Use this for hardcoded identifiers like "()", "::", "[]", etc.
    /// The string is interned so it will be shared during marshalling.
    pub fn push_lident_static(&mut self, name: &'static str) -> LidentIdx {
        self.arena.push_lident_static(name)
    }

    /// Push a dotted identifier (Ldot) into the arena.
    /// Takes a Longident for the prefix (will be pushed first).
    pub fn push_ldot(&mut self, prefix: Longident, name: impl Into<String>) -> LidentIdx {
        let str_idx = self.arena.push_string(name.into());
        let lid = Longident::ldot(prefix, str_idx);
        self.arena.push_longident(lid)
    }

    /// Push a dotted identifier (Ldot) by extending an existing arena entry.
    pub fn push_ldot_idx(&mut self, prefix: LidentIdx, name: impl Into<String>) -> LidentIdx {
        self.arena.push_ldot(prefix, name)
    }

    /// Get a longident by index.
    pub fn get_longident(&self, idx: LidentIdx) -> &Longident {
        self.arena.get_longident(idx)
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

    /// Record a diagnostic error without silencing subsequent errors.
    /// Use this for errors that should allow additional errors to be reported.
    pub fn err_multiple(&mut self, start_pos: Position, end_pos: Position, category: DiagnosticCategory) {
        let diagnostic = ParserDiagnostic {
            start_pos,
            end_pos,
            category,
        };
        self.diagnostics.push(diagnostic);
    }

    /// Record an unexpected token error using the current breadcrumbs.
    /// This emits an error with context-aware messaging based on the parsing context.
    pub fn err_unexpected(&mut self) {
        let context: Vec<(Grammar, Position)> = self.breadcrumbs.clone();
        self.err(DiagnosticCategory::Unexpected {
            token: self.token.clone(),
            context,
        });
    }

    /// Record an unexpected token error at specific positions using the current breadcrumbs.
    pub fn err_unexpected_at(&mut self, start_pos: Position) {
        let context: Vec<(Grammar, Position)> = self.breadcrumbs.clone();
        self.err_at(
            start_pos,
            self.end_pos.clone(),
            DiagnosticCategory::Unexpected {
                token: self.token.clone(),
                context,
            },
        );
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

    /// Maximum parse depth to prevent stack overflow from deeply nested constructs.
    pub const MAX_PARSE_DEPTH: usize = 50;

    /// Check if we've exceeded the maximum parse depth.
    /// Returns true if we're at or beyond the limit.
    #[inline]
    pub fn exceeded_parse_depth(&self) -> bool {
        self.parse_depth >= Self::MAX_PARSE_DEPTH
    }

    /// Increment parse depth before a potentially recursive parse.
    /// Call `dec_parse_depth` when done.
    #[inline]
    pub fn inc_parse_depth(&mut self) {
        self.parse_depth += 1;
    }

    /// Decrement parse depth after a recursive parse.
    #[inline]
    pub fn dec_parse_depth(&mut self) {
        self.parse_depth = self.parse_depth.saturating_sub(1);
    }

    /// Advance to the next non-comment token.
    ///
    /// Comments are collected in the parser's comment list.
    /// Note: Doc/module comments are currently treated as comments (not tokens).
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

            // Propagate scanner diagnostics to parser
            let scanner_diagnostics = self.scanner.take_diagnostics();
            self.diagnostics.extend(scanner_diagnostics);

            match result.token {
                Token::Comment(ref c) => {
                    // Collect comment and continue scanning.
                    let mut comment = c.clone();
                    comment.set_prev_tok_end_pos(self.end_pos.clone());
                    self.comments.push(comment);
                    self.prev_end_pos = self.end_pos.clone();
                    self.cached_prev_end_pos_idx = None; // Clear cache when prev_end_pos changes
                    self.end_pos = result.end_pos;
                    // Continue to next token
                }
                _ => {
                    self.token = result.token;
                    self.prev_end_pos = prev_end;
                    self.cached_prev_end_pos_idx = None; // Clear cache when prev_end_pos changes
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

        // Propagate scanner diagnostics to parser
        let scanner_diagnostics = self.scanner.take_diagnostics();
        self.diagnostics.extend(scanner_diagnostics);

        self.token = result.token;
        self.prev_end_pos = self.end_pos.clone();
        self.cached_prev_end_pos_idx = None; // Clear cache when prev_end_pos changes
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
    ///
    /// When the expected token is not found, an error is generated spanning from
    /// the previous token's end position to the current token's end position.
    /// This matches OCaml's `expect` which uses `~start_pos:p.prev_end_pos` with
    /// default `~end_pos:p.end_pos`.
    pub fn expect_with_grammar(&mut self, token: Token, grammar: Option<Grammar>) {
        if self.token == token {
            self.next();
        } else {
            let category = DiagnosticCategory::Expected {
                context: grammar,
                pos: self.prev_end_pos.clone(),
                token,
            };
            // OCaml uses prev_end_pos for start and end_pos (current token's end) for end.
            // This creates a span from where we expected the token to the current position.
            self.err_at(
                self.prev_end_pos.clone(),
                self.end_pos.clone(),
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
        self.cached_prev_end_pos_idx = None; // Clear cache when prev_end_pos changes
        self.breadcrumbs = breadcrumbs;
        self.diagnostics.truncate(diagnostics_len);
        self.comments.truncate(comments_len);

        result
    }

    /// Create a snapshot of the current parser state.
    ///
    /// Used for speculative parsing with manual backtracking.
    /// Call `restore` to return to this state.
    pub fn snapshot(&self) -> ParserSnapshot {
        ParserSnapshot {
            scanner_snapshot: self.scanner.snapshot(),
            token: self.token.clone(),
            start_pos: self.start_pos.clone(),
            end_pos: self.end_pos.clone(),
            prev_end_pos: self.prev_end_pos.clone(),
            breadcrumbs: self.breadcrumbs.clone(),
            diagnostics_len: self.diagnostics.len(),
            comments_len: self.comments.len(),
            location_id_counter: self.location_id_counter.load(Ordering::Relaxed),
            arena_positions_len: self.arena.position_count(),
            arena_locations_len: self.arena.location_count(),
        }
    }

    /// Restore parser state from a snapshot.
    ///
    /// Used to backtrack after speculative parsing.
    /// Note: Arena allocations made during speculative parsing are not freed,
    /// but the indices won't be stored in the AST since we're restoring state.
    pub fn restore(&mut self, snapshot: ParserSnapshot) {
        self.scanner.restore(snapshot.scanner_snapshot);
        self.token = snapshot.token;
        self.start_pos = snapshot.start_pos;
        self.end_pos = snapshot.end_pos;
        self.prev_end_pos = snapshot.prev_end_pos;
        self.cached_prev_end_pos_idx = None; // Clear cache when prev_end_pos changes
        self.breadcrumbs = snapshot.breadcrumbs;
        self.diagnostics.truncate(snapshot.diagnostics_len);
        self.comments.truncate(snapshot.comments_len);
        self.location_id_counter.store(snapshot.location_id_counter, Ordering::Relaxed);
        // Note: We don't truncate arena allocations as it's harmless to keep them.
        // The indices from speculative parsing won't be stored in the final AST.
        let _ = (snapshot.arena_positions_len, snapshot.arena_locations_len);
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

    /// Check if there's a newline between the previous token and the current token.
    ///
    /// This is used for disambiguating syntax like `[` which can be either
    /// an array literal (when starting a new line) or array indexing (when
    /// following an expression on the same line).
    pub fn has_newline_before(&self) -> bool {
        self.start_pos.line > self.prev_end_pos.line
    }

    /// Check if the current token has whitespace on both sides.
    ///
    /// Used to disambiguate operators like `-` which can be:
    /// - Binary (subtraction) when there's whitespace on both sides
    /// - Unary (negation) when at the start of a line without surrounding whitespace
    pub fn is_binary_op(&self) -> bool {
        Scanner::is_binary_op(
            self.scanner.src(),
            self.start_pos.cnum as usize,
            self.end_pos.cnum as usize,
        )
    }

    /// Set diamond mode on the scanner (for type parameters).
    pub fn set_diamond_mode(&mut self) {
        self.scanner.set_diamond_mode();
    }

    /// Pop diamond mode from the scanner.
    pub fn pop_diamond_mode(&mut self) {
        self.scanner.pop_diamond_mode();
    }

    /// Get the filename index being parsed.
    pub fn filename(&self) -> StrIdx {
        self.scanner.filename
    }

    /// Get the filename string being parsed.
    /// Looks up the StrIdx in the arena.
    pub fn filename_str(&self) -> &str {
        self.arena.get_string(self.scanner.filename)
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
