//! Source location types.
//!
//! This module provides source location information for error reporting
//! and diagnostics. Unlike the OCaml `location.ml` which uses global
//! `input_name` ref, all location information is self-contained.
//!
//! # Example
//!
//! ```rust
//! use rescript_compiler::location::{Location, Position};
//!
//! let start = Position::new("test.res", 1, 0, 0);
//! let end_pos = Position::new("test.res", 1, 0, 10);
//! let loc = Location::from_positions(start, end_pos);
//!
//! assert_eq!(loc.file_name(), "test.res");
//! assert_eq!(loc.start_line(), 1);
//! ```

use serde::{Deserialize, Serialize};
use std::fmt;

/// A position in a source file.
///
/// This corresponds to `Lexing.position` in OCaml.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct Position {
    /// File name.
    pub file_name: String,
    /// Line number (1-indexed).
    pub line: i32,
    /// Column offset from beginning of line (0-indexed).
    pub bol: i32,
    /// Character offset from beginning of file (0-indexed).
    pub cnum: i32,
}

impl Position {
    /// Create a new position.
    pub fn new(file_name: impl Into<String>, line: i32, bol: i32, cnum: i32) -> Self {
        Self {
            file_name: file_name.into(),
            line,
            bol,
            cnum,
        }
    }

    /// Create a position at the start of a file.
    pub fn at_file_start(file_name: impl Into<String>) -> Self {
        Self {
            file_name: file_name.into(),
            line: 1,
            bol: 0,
            cnum: 0,
        }
    }

    /// Get the column number (0-indexed).
    pub fn column(&self) -> i32 {
        self.cnum - self.bol
    }

    /// Get the column number (1-indexed, for display).
    pub fn column_1indexed(&self) -> i32 {
        self.column() + 1
    }
}

/// A location span in source code.
///
/// This corresponds to `Location.t` in OCaml.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Location {
    /// Start position.
    pub loc_start: Position,
    /// End position.
    pub loc_end: Position,
    /// Whether this is a "ghost" location (compiler-generated).
    pub loc_ghost: bool,
}

impl Default for Location {
    fn default() -> Self {
        Self::none()
    }
}

impl Location {
    /// Create a new location from start and end positions.
    pub fn from_positions(start: Position, end: Position) -> Self {
        Self {
            loc_start: start,
            loc_end: end,
            loc_ghost: false,
        }
    }

    /// Create a location from character offsets in a file.
    ///
    /// This is a convenience constructor when you don't have full position info.
    pub fn new(file_name: &str, start_offset: usize, end_offset: usize) -> Self {
        Self {
            loc_start: Position {
                file_name: file_name.to_string(),
                line: 1,
                bol: 0,
                cnum: start_offset as i32,
            },
            loc_end: Position {
                file_name: file_name.to_string(),
                line: 1,
                bol: 0,
                cnum: end_offset as i32,
            },
            loc_ghost: false,
        }
    }

    /// Create a ghost location at the start of a file.
    pub fn in_file(name: impl Into<String>) -> Self {
        let name = name.into();
        let pos = Position {
            file_name: name,
            line: 1,
            bol: 0,
            cnum: -1,
        };
        Self {
            loc_start: pos.clone(),
            loc_end: pos,
            loc_ghost: true,
        }
    }

    /// Create a "none" location (placeholder).
    pub fn none() -> Self {
        Self::in_file("_none_")
    }

    /// Check if this is a "none" location.
    pub fn is_none(&self) -> bool {
        self.loc_start.file_name == "_none_"
    }

    /// Get the file name.
    pub fn file_name(&self) -> &str {
        &self.loc_start.file_name
    }

    /// Get the start line number.
    pub fn start_line(&self) -> i32 {
        self.loc_start.line
    }

    /// Get the start column (0-indexed).
    pub fn start_column(&self) -> i32 {
        self.loc_start.column()
    }

    /// Get the end line number.
    pub fn end_line(&self) -> i32 {
        self.loc_end.line
    }

    /// Get the end column (0-indexed).
    pub fn end_column(&self) -> i32 {
        self.loc_end.column()
    }

    /// Check if this is a ghost location.
    pub fn is_ghost(&self) -> bool {
        self.loc_ghost
    }

    /// Create a ghost version of this location.
    pub fn ghostify(&self) -> Self {
        Self {
            loc_start: self.loc_start.clone(),
            loc_end: self.loc_end.clone(),
            loc_ghost: true,
        }
    }

    /// Merge two locations into a span covering both.
    pub fn merge(&self, other: &Location) -> Self {
        Self {
            loc_start: self.loc_start.clone(),
            loc_end: other.loc_end.clone(),
            loc_ghost: self.loc_ghost && other.loc_ghost,
        }
    }

    /// Normalize the range for display.
    ///
    /// Returns `None` if the location has invalid positions.
    /// Otherwise returns `Some((start_line, start_col, end_line, end_col))`
    /// with 1-indexed columns.
    pub fn normalize_range(&self) -> Option<(i32, i32, i32, i32)> {
        let start_line = self.loc_start.line;
        let start_char = self.loc_start.column();
        let end_line = self.loc_end.line;
        let end_char = self.loc_end.column();

        // Invalid positions
        if start_char == -1 || end_char == -1 {
            return None;
        }

        // Same line with invalid range - point to same char
        if start_line == end_line && start_char >= end_char {
            let same_char = start_char + 1;
            return Some((start_line, same_char, end_line, same_char));
        }

        // Normal case: convert to 1-indexed start column
        Some((start_line, start_char + 1, end_line, end_char))
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let file = if self.file_name() == "_none_" {
            "(unknown)"
        } else {
            self.file_name()
        };

        match self.normalize_range() {
            None => write!(f, "{}", file),
            Some((start_line, start_col, end_line, end_col)) => {
                if start_line == end_line {
                    if start_col == end_col {
                        write!(f, "{}:{}:{}", file, start_line, start_col)
                    } else {
                        write!(f, "{}:{}:{}-{}", file, start_line, start_col, end_col)
                    }
                } else {
                    write!(
                        f,
                        "{}:{}:{}-{}:{}",
                        file, start_line, start_col, end_line, end_col
                    )
                }
            }
        }
    }
}

/// A value with an associated location.
///
/// This corresponds to `'a Location.loc` in OCaml.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Located<T> {
    /// The value.
    pub txt: T,
    /// The location.
    pub loc: Location,
}

impl<T> Located<T> {
    /// Create a new located value.
    pub fn new(txt: T, loc: Location) -> Self {
        Self { txt, loc }
    }

    /// Create a located value with no location.
    pub fn mknoloc(txt: T) -> Self {
        Self {
            txt,
            loc: Location::none(),
        }
    }

    /// Map the value while keeping the location.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Located<U> {
        Located {
            txt: f(self.txt),
            loc: self.loc,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position() {
        let pos = Position::new("test.res", 5, 40, 45);
        assert_eq!(pos.column(), 5);
        assert_eq!(pos.column_1indexed(), 6);
    }

    #[test]
    fn test_location_none() {
        let loc = Location::none();
        assert!(loc.is_none());
        assert!(loc.is_ghost());
    }

    #[test]
    fn test_location_in_file() {
        let loc = Location::in_file("test.res");
        assert_eq!(loc.file_name(), "test.res");
        assert!(loc.is_ghost());
    }

    #[test]
    fn test_location_merge() {
        let start = Position::new("test.res", 1, 0, 0);
        let mid = Position::new("test.res", 1, 0, 5);
        let end_pos = Position::new("test.res", 2, 10, 20);

        let loc1 = Location::from_positions(start, mid.clone());
        let loc2 = Location::from_positions(mid, end_pos);

        let merged = loc1.merge(&loc2);
        assert_eq!(merged.start_line(), 1);
        assert_eq!(merged.end_line(), 2);
    }

    #[test]
    fn test_location_display() {
        let start = Position::new("test.res", 5, 40, 45);
        let end_pos = Position::new("test.res", 5, 40, 50);
        let loc = Location::from_positions(start, end_pos);

        assert_eq!(format!("{}", loc), "test.res:5:6-10");
    }

    #[test]
    fn test_located() {
        let located = Located::new("hello", Location::in_file("test.res"));
        assert_eq!(located.txt, "hello");

        let mapped = located.map(|s| s.to_uppercase());
        assert_eq!(mapped.txt, "HELLO");
    }

    #[test]
    fn test_location_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Location>();
        assert_send_sync::<Position>();
        assert_send_sync::<Located<String>>();
    }
}
