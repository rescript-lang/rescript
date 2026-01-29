//! Arena for positions and locations during parsing.
//!
//! This module provides efficient storage for source locations in the AST.
//! Instead of storing full Position/Location values everywhere, we store
//! lightweight indices that can be compared cheaply.
//!
//! # Architecture
//!
//! - `PosIdx` and `LocIdx` are lightweight index types (4 bytes each)
//! - `ParseArena` contains arenas for positions and locations
//! - When parsing, positions are pushed to get indices
//! - During marshalling, indices are looked up to get actual values
//!
//! # Example
//!
//! ```ignore
//! use rescript_compiler::parse_arena::{ParseArena, LocIdx, Located};
//! use rescript_compiler::location::Position;
//!
//! let mut arena = ParseArena::new();
//! let start = arena.push_pos("test.res", 1, 0, 0);
//! let end = arena.push_pos("test.res", 1, 0, 10);
//! let loc = arena.mk_loc(start, end);
//!
//! let located = Located::new("hello", loc);
//! assert_eq!(located.txt, "hello");
//! ```

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::arena::{Arena, ArenaIdx};
use crate::intern::Interner;
use crate::location::{Location, Position};

/// Key for position deduplication.
/// Two positions with the same (file_name, line, bol, cnum) are the same position.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PosKey {
    file_name: String,
    line: i32,
    bol: i32,
    cnum: i32,
}

impl PosKey {
    fn from_position(pos: &Position) -> Self {
        Self {
            file_name: pos.file_name.clone(),
            line: pos.line,
            bol: pos.bol,
            cnum: pos.cnum,
        }
    }
}

// Re-export StrIdx for convenience
pub use crate::intern::StrIdx;

// ============================================================================
// Index Types
// ============================================================================

/// Index into the position arena.
///
/// This is a lightweight handle (4 bytes) that can be copied and compared
/// cheaply. Two PosIdx values are equal if and only if they refer to the
/// same arena slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct PosIdx(pub u32);

impl ArenaIdx for PosIdx {
    fn from_u32(idx: u32) -> Self {
        PosIdx(idx)
    }
    fn to_u32(self) -> u32 {
        self.0
    }
}

/// Index into the location arena.
///
/// This is a lightweight handle (4 bytes) that can be copied and compared
/// cheaply. Two LocIdx values are equal if and only if they refer to the
/// same arena slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct LocIdx(pub u32);

impl ArenaIdx for LocIdx {
    fn from_u32(idx: u32) -> Self {
        LocIdx(idx)
    }
    fn to_u32(self) -> u32 {
        self.0
    }
}

impl LocIdx {
    /// Check if this is the "none" location (index 0).
    /// The "none" location is pre-allocated at index 0 in every ParseArena.
    pub fn is_none(&self) -> bool {
        self.0 == 0
    }

    /// Create the "none" location index (same as default).
    pub fn none() -> Self {
        Self(0)
    }
}

// ============================================================================
// InternedLocation (stored in arena)
// ============================================================================

/// A location in a source file (stored in arena).
///
/// This is stored in the ParseArena. Users work with `LocIdx` handles.
/// Note that `loc_start` and `loc_end` are `PosIdx`, not `Position`, so
/// locations reference positions by index rather than storing them inline.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternedLocation {
    /// Start position index.
    pub loc_start: PosIdx,
    /// End position index.
    pub loc_end: PosIdx,
    /// Whether this is a "ghost" location (synthesized, not from source).
    pub loc_ghost: bool,
}

impl InternedLocation {
    /// Create a new location from position indices.
    pub fn new(start: PosIdx, end: PosIdx, ghost: bool) -> Self {
        Self {
            loc_start: start,
            loc_end: end,
            loc_ghost: ghost,
        }
    }
}

impl Default for InternedLocation {
    fn default() -> Self {
        // Default to the "none" location (position index 0, ghost = true)
        Self {
            loc_start: PosIdx(0),
            loc_end: PosIdx(0),
            loc_ghost: true,
        }
    }
}

// ============================================================================
// ParseArena
// ============================================================================

/// Arena for positions and locations during parsing.
///
/// This provides:
/// - Efficient storage: positions/locations stored once, referenced by index
/// - Lightweight handles: indices are 4 bytes, cheap to copy/compare
/// - Identity for marshalling: same index = same object
///
/// This arena deduplicates positions by content - two positions with the same
/// (file_name, line, bol, cnum) will get the same index. This matches OCaml's
/// behavior where the same position captured at different times gets shared.
#[derive(Debug)]
pub struct ParseArena {
    /// Arena of positions (returns PosIdx).
    positions: Arena<Position, PosIdx>,
    /// Arena of locations (returns LocIdx).
    locations: Arena<InternedLocation, LocIdx>,
    /// Position deduplication table - maps position content to its index.
    position_dedup: HashMap<PosKey, PosIdx>,
    /// String interner for identifier tokens.
    /// Identifiers with the same content get the same StrIdx, enabling
    /// identity-based sharing when marshalling (matching OCaml's behavior).
    strings: Interner,
}

impl Default for ParseArena {
    fn default() -> Self {
        Self {
            positions: Arena::default(),
            locations: Arena::default(),
            position_dedup: HashMap::new(),
            strings: Interner::new(),
        }
    }
}

impl ParseArena {
    /// Create a new arena with the "none" position and location pre-allocated at index 0.
    ///
    /// This ensures that `LocIdx::default()` (which is `LocIdx(0)`) always refers to
    /// a valid "none" location, even without calling `none_loc()` first.
    pub fn new() -> Self {
        let mut arena = Self::default();
        // Pre-allocate none position and location at index 0
        // This way LocIdx::default() == LocIdx(0) is always valid
        let none_pos = Position::new("_none_", 1, 0, -1);
        let pos_idx = arena.positions.push(none_pos);
        let none_loc = InternedLocation::new(pos_idx, pos_idx, true);
        arena.locations.push(none_loc);
        arena
    }

    // ========== Position methods ==========

    /// Push a position into the arena and return its index.
    /// Each call creates a new entry. The position is registered in the
    /// dedup table only if no entry exists yet (keep first position).
    pub fn push_position(&mut self, pos: Position) -> PosIdx {
        let key = PosKey::from_position(&pos);
        let idx = self.positions.push(pos);
        // Only register if not already in table (keep first position)
        self.position_dedup.entry(key).or_insert(idx);
        idx
    }

    /// Get the index of a position if it exists in the dedup table.
    /// Returns None if this position content hasn't been seen before.
    #[allow(dead_code)]
    pub fn get_position_idx(&self, pos: &Position) -> Option<PosIdx> {
        let key = PosKey::from_position(pos);
        self.position_dedup.get(&key).copied()
    }

    /// Push a position with deduplication - reuses an existing index if the position
    /// content matches.
    pub fn push_position_dedup(&mut self, pos: Position) -> PosIdx {
        let key = PosKey::from_position(&pos);
        if let Some(&idx) = self.position_dedup.get(&key) {
            return idx;
        }
        let idx = self.positions.push(pos);
        self.position_dedup.insert(key, idx);
        idx
    }

    /// Push a position from components.
    /// Uses deduplication to reuse existing positions with the same content.
    pub fn push_pos(&mut self, file_name: impl Into<String>, line: i32, bol: i32, cnum: i32) -> PosIdx {
        self.push_position_dedup(Position::new(file_name, line, bol, cnum))
    }

    /// Get the "none" position index (for _none_ file).
    /// Returns the pre-allocated index 0.
    pub fn none_pos(&self) -> PosIdx {
        PosIdx(0)
    }

    /// Look up a position by index.
    pub fn get_position(&self, idx: PosIdx) -> &Position {
        self.positions.get(idx)
    }

    /// Get all positions (for marshalling).
    pub fn positions(&self) -> &[Position] {
        self.positions.as_slice()
    }

    /// Get total number of positions.
    pub fn position_count(&self) -> usize {
        self.positions.len()
    }

    // ========== Location methods ==========

    /// Push a location into the arena and return its index.
    pub fn push_location(&mut self, loc: InternedLocation) -> LocIdx {
        self.locations.push(loc)
    }

    /// Push a location from position indices.
    pub fn push_loc(&mut self, start: PosIdx, end: PosIdx, ghost: bool) -> LocIdx {
        self.push_location(InternedLocation::new(start, end, ghost))
    }

    /// Create a location from two position indices and push it (non-ghost).
    pub fn mk_loc(&mut self, start: PosIdx, end: PosIdx) -> LocIdx {
        self.push_loc(start, end, false)
    }

    /// Create a location from two raw positions (push both positions first).
    /// Uses deduplication to reuse existing positions with the same content.
    pub fn mk_loc_from_positions(&mut self, start: &Position, end: &Position) -> LocIdx {
        let start_idx = self.push_position_dedup(start.clone());
        let end_idx = self.push_position_dedup(end.clone());
        self.mk_loc(start_idx, end_idx)
    }

    /// Create a location spanning from one location's start to another's end.
    pub fn mk_loc_spanning(&mut self, start_loc: LocIdx, end_loc: LocIdx) -> LocIdx {
        let start_pos = self.loc_start(start_loc).clone();
        let end_pos = self.loc_end(end_loc).clone();
        self.mk_loc_from_positions(&start_pos, &end_pos)
    }

    /// Get the "none" location index (ghost location with none positions).
    /// Returns the pre-allocated index 0.
    pub fn none_loc(&self) -> LocIdx {
        LocIdx(0)
    }

    /// Look up a location by index.
    pub fn get_location(&self, idx: LocIdx) -> &InternedLocation {
        self.locations.get(idx)
    }

    /// Get all locations (for marshalling).
    pub fn locations(&self) -> &[InternedLocation] {
        self.locations.as_slice()
    }

    /// Get total number of locations.
    pub fn location_count(&self) -> usize {
        self.locations.len()
    }

    // ========== Location accessor helpers ==========

    /// Get the start position of a location.
    pub fn loc_start(&self, idx: LocIdx) -> &Position {
        let loc = self.get_location(idx);
        self.get_position(loc.loc_start)
    }

    /// Get the end position of a location.
    pub fn loc_end(&self, idx: LocIdx) -> &Position {
        let loc = self.get_location(idx);
        self.get_position(loc.loc_end)
    }

    /// Get the start position index of a location.
    pub fn loc_start_idx(&self, idx: LocIdx) -> PosIdx {
        self.get_location(idx).loc_start
    }

    /// Get the end position index of a location.
    pub fn loc_end_idx(&self, idx: LocIdx) -> PosIdx {
        self.get_location(idx).loc_end
    }

    /// Check if a location is ghost.
    pub fn loc_ghost(&self, idx: LocIdx) -> bool {
        self.get_location(idx).loc_ghost
    }

    /// Create a ghost version of an existing location.
    /// Returns a new LocIdx with the same positions but loc_ghost=true.
    pub fn mk_ghost_loc(&mut self, idx: LocIdx) -> LocIdx {
        let loc = self.get_location(idx);
        let start = loc.loc_start;
        let end = loc.loc_end;
        self.push_loc(start, end, true)
    }

    /// Reconstruct a full Location struct from a LocIdx.
    /// Useful for code that needs the full Location type (e.g., printer).
    pub fn to_location(&self, idx: LocIdx) -> Location {
        let loc = self.get_location(idx);
        Location {
            loc_start: self.get_position(loc.loc_start).clone(),
            loc_end: self.get_position(loc.loc_end).clone(),
            loc_ghost: loc.loc_ghost,
            id: crate::location::LocationId::default_id(),
        }
    }

    /// Convert a full Location to a LocIdx by pushing its positions to the arena.
    /// Useful for migrating code that creates Location values.
    /// Uses deduplication to reuse existing positions with the same content.
    pub fn from_location(&mut self, loc: &Location) -> LocIdx {
        let start_idx = self.push_position_dedup(loc.loc_start.clone());
        let end_idx = self.push_position_dedup(loc.loc_end.clone());
        self.push_loc(start_idx, end_idx, loc.loc_ghost)
    }

    /// Convert a Located<T> using crate::location::Location to one using LocIdx.
    pub fn from_located<T: Clone>(&mut self, located: &crate::location::Located<T>) -> Located<T> {
        Located {
            txt: located.txt.clone(),
            loc: self.from_location(&located.loc),
        }
    }

    /// Convert a Located<T> using LocIdx to one using crate::location::Location.
    pub fn to_located<T: Clone>(&self, located: &Located<T>) -> crate::location::Located<T> {
        crate::location::Located {
            txt: located.txt.clone(),
            loc: self.to_location(located.loc),
        }
    }

    // ========================================================================
    // String interning
    // ========================================================================

    /// Intern a string, returning its index.
    ///
    /// If the string was previously interned, returns the existing index.
    /// Otherwise, stores the string and returns a new index.
    ///
    /// Use this for strings that should be shared in Marshal output
    /// (matching OCaml's interning behavior).
    pub fn intern(&mut self, s: &str) -> StrIdx {
        self.strings.intern(s)
    }

    /// Get an interned string by index.
    pub fn get_interned(&self, idx: StrIdx) -> &str {
        self.strings.get(idx)
    }
}

// ============================================================================
// Located<T> - A value with its source location
// ============================================================================

/// A value annotated with its source location (using LocIdx).
///
/// This is equivalent to OCaml's `'a loc` type, but uses `LocIdx` for efficiency.
/// This is the type to use in the AST for minimal memory footprint.
///
/// Note: There is also `crate::location::Located<T>` which stores a full
/// `Location` instead of `LocIdx`. Use that when you don't have access to
/// a ParseArena or need the full location data inline.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Located<T> {
    /// The value.
    pub txt: T,
    /// Location in source (as index).
    pub loc: LocIdx,
}

impl<T> Located<T> {
    /// Create a new located value.
    pub fn new(txt: T, loc: LocIdx) -> Self {
        Self { txt, loc }
    }

    /// Create a located value with the default "none" location.
    ///
    /// This uses `LocIdx::default()` which is `LocIdx(0)`, the pre-allocated
    /// none location in every ParseArena.
    pub fn mknoloc(txt: T) -> Self {
        Self {
            txt,
            loc: LocIdx::default(),
        }
    }

    /// Map the value while keeping the location.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Located<U> {
        Located {
            txt: f(self.txt),
            loc: self.loc,
        }
    }

    /// Get a reference to the text.
    pub fn as_ref(&self) -> Located<&T> {
        Located {
            txt: &self.txt,
            loc: self.loc,
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_push() {
        let mut arena = ParseArena::new();
        let base_count = arena.position_count();
        assert_eq!(base_count, 1); // Pre-allocated none position

        let idx1 = arena.push_pos("test.res", 1, 0, 5);
        let idx2 = arena.push_pos("test.res", 1, 0, 5);
        let idx3 = arena.push_pos("test.res", 1, 0, 10);

        // Each push creates a new entry (no deduplication)
        assert_ne!(idx1, idx2);
        assert_ne!(idx1, idx3);
        assert_ne!(idx2, idx3);

        assert_eq!(arena.position_count(), base_count + 3);
    }

    #[test]
    fn test_location_push() {
        let mut arena = ParseArena::new();
        let base_count = arena.location_count();
        assert_eq!(base_count, 1); // Pre-allocated none location

        let start = arena.push_pos("test.res", 1, 0, 0);
        let end = arena.push_pos("test.res", 1, 0, 10);

        let loc1 = arena.mk_loc(start, end);
        let loc2 = arena.mk_loc(start, end);

        // Each push creates a new entry (no deduplication)
        assert_ne!(loc1, loc2);

        assert_eq!(arena.location_count(), base_count + 2);
    }

    #[test]
    fn test_none_at_index_zero() {
        let arena = ParseArena::new();

        let none_pos = arena.none_pos();
        let none_loc = arena.none_loc();

        assert_eq!(none_pos, PosIdx(0));
        assert_eq!(none_loc, LocIdx(0));
        assert_eq!(LocIdx::default(), none_loc);

        let pos = arena.get_position(none_pos);
        assert_eq!(pos.file_name, "_none_");
        assert!(arena.loc_ghost(none_loc));
    }

    #[test]
    fn test_located() {
        let arena = ParseArena::new();
        let loc = arena.none_loc();

        let located = Located::new("hello".to_string(), loc);
        assert_eq!(located.txt, "hello");
        assert_eq!(located.loc, loc);
    }

    #[test]
    fn test_mk_loc_from_positions() {
        let mut arena = ParseArena::new();

        let start = Position::new("test.res", 1, 0, 0);
        let end = Position::new("test.res", 1, 0, 10);

        let loc = arena.mk_loc_from_positions(&start, &end);

        assert_eq!(arena.loc_start(loc).line, 1);
        assert_eq!(arena.loc_end(loc).cnum, 10);
    }

    #[test]
    fn test_to_location() {
        let mut arena = ParseArena::new();

        let start = Position::new("test.res", 1, 0, 0);
        let end = Position::new("test.res", 1, 0, 10);

        let loc_idx = arena.mk_loc_from_positions(&start, &end);
        let loc = arena.to_location(loc_idx);

        assert_eq!(loc.loc_start.line, 1);
        assert_eq!(loc.loc_end.cnum, 10);
        assert!(!loc.loc_ghost);
    }

    #[test]
    fn test_mk_loc_spanning() {
        let mut arena = ParseArena::new();

        let pos1 = arena.push_pos("test.res", 1, 0, 0);
        let pos2 = arena.push_pos("test.res", 1, 0, 5);
        let pos3 = arena.push_pos("test.res", 2, 20, 25);
        let pos4 = arena.push_pos("test.res", 2, 20, 30);

        let loc1 = arena.mk_loc(pos1, pos2);
        let loc2 = arena.mk_loc(pos3, pos4);

        let spanning = arena.mk_loc_spanning(loc1, loc2);

        assert_eq!(arena.loc_start(spanning).line, 1);
        assert_eq!(arena.loc_start(spanning).cnum, 0);
        assert_eq!(arena.loc_end(spanning).line, 2);
        assert_eq!(arena.loc_end(spanning).cnum, 30);
    }
}
