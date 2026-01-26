//! Format module - OCaml-compatible pretty printing
//!
//! This module provides a pretty printing system similar to OCaml's Format module.
//! It tracks output position and breaks lines when they exceed the margin.

use std::io::Write;

/// Default margin (line width)
pub const DEFAULT_MARGIN: usize = 80;

/// A pretty printer that tracks position and handles line breaking
pub struct PPrint<W: Write> {
    out: W,
    /// Current column position
    col: usize,
    /// Target line width
    margin: usize,
    /// Stack of indentation levels (box positions)
    indent_stack: Vec<usize>,
}

impl<W: Write> PPrint<W> {
    /// Create a new pretty printer
    pub fn new(out: W) -> Self {
        PPrint {
            out,
            col: 0,
            margin: DEFAULT_MARGIN,
            indent_stack: vec![0],
        }
    }

    /// Set the margin (line width)
    pub fn set_margin(&mut self, margin: usize) {
        self.margin = margin;
    }

    /// Get current column
    pub fn col(&self) -> usize {
        self.col
    }

    /// Get current indentation level
    pub fn indent(&self) -> usize {
        *self.indent_stack.last().unwrap_or(&0)
    }

    /// Print a string directly
    pub fn string(&mut self, s: &str) {
        let _ = self.out.write_all(s.as_bytes());
        self.col += s.chars().count();
    }

    /// Print a character
    pub fn char(&mut self, c: char) {
        let _ = write!(self.out, "{}", c);
        self.col += 1;
    }

    /// Print a space
    pub fn space(&mut self) {
        self.string(" ");
    }

    /// Print a newline and indent to current level
    pub fn newline(&mut self) {
        let _ = self.out.write_all(b"\n");
        let indent = self.indent();
        for _ in 0..indent {
            let _ = self.out.write_all(b" ");
        }
        self.col = indent;
    }

    /// Break - print space if it fits, otherwise newline with indent
    /// `nspaces`: number of spaces if not breaking
    /// `offset`: additional indent offset if breaking (can be negative)
    pub fn break_(&mut self, nspaces: usize, offset: i32) {
        if self.col >= self.margin {
            // Break - newline + indent + offset
            let _ = self.out.write_all(b"\n");
            let base_indent = self.indent() as i32;
            let actual_indent = (base_indent + offset).max(0) as usize;
            for _ in 0..actual_indent {
                let _ = self.out.write_all(b" ");
            }
            self.col = actual_indent;
        } else {
            // Don't break - just spaces
            for _ in 0..nspaces {
                let _ = self.out.write_all(b" ");
            }
            self.col += nspaces;
        }
    }

    /// Space that can become a newline (equivalent to @; in OCaml)
    pub fn sp(&mut self) {
        self.break_(1, 0);
    }

    /// Cut - zero-width break point (equivalent to @, in OCaml)
    pub fn cut(&mut self) {
        self.break_(0, 0);
    }

    /// Open a box with given indent added to current column
    /// (equivalent to @[<n> in OCaml)
    pub fn open_box(&mut self, indent: i32) {
        let new_indent = (self.col as i32 + indent).max(0) as usize;
        self.indent_stack.push(new_indent);
    }

    /// Open a box at absolute indent level
    pub fn open_box_abs(&mut self, indent: usize) {
        self.indent_stack.push(indent);
    }

    /// Close the current box (equivalent to @] in OCaml)
    pub fn close_box(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    /// Check if we're past the margin
    pub fn over_margin(&self) -> bool {
        self.col > self.margin
    }

    /// Flush the output
    pub fn flush(&mut self) {
        let _ = self.out.flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_output() {
        let mut buf = Vec::new();
        {
            let mut pp = PPrint::new(&mut buf);
            pp.string("hello");
            pp.space();
            pp.string("world");
        }
        assert_eq!(String::from_utf8(buf).unwrap(), "hello world");
    }

    #[test]
    fn test_box_indent() {
        let mut buf = Vec::new();
        {
            let mut pp = PPrint::new(&mut buf);
            pp.set_margin(20);
            pp.string("let x =");
            pp.open_box(2);
            pp.sp();
            pp.string("some_very_long_expression");
            pp.close_box();
        }
        // Should break because it exceeds margin
        let result = String::from_utf8(buf).unwrap();
        assert!(result.contains("\n") || result.len() <= 20);
    }
}
