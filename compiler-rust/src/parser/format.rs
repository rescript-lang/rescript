//! Format module - OCaml-compatible pretty printing
//!
//! This module provides a simple pretty printing system similar to OCaml's Format module.
//! It supports boxes with automatic line breaking based on margin width.

use std::io::Write;

/// Default margin (line width)
const DEFAULT_MARGIN: usize = 80;

/// A token in the pretty printing stream
#[derive(Debug, Clone)]
enum Token {
    /// A string to print
    String(String),
    /// A break hint - can become space or newline
    /// (num_spaces if not breaking, indent_offset if breaking)
    Break(usize, i32),
    /// Open a box with given indent
    OpenBox(BoxKind, i32),
    /// Close the current box
    CloseBox,
}

/// Kind of box for grouping content
#[derive(Debug, Clone, Copy)]
pub enum BoxKind {
    /// Horizontal box - never breaks
    H,
    /// Vertical box - always breaks
    V,
    /// Horizontal-or-vertical box - all on one line or each on separate line
    HV,
    /// Horizontal-or-vertical overflow box - fits as much as possible on each line
    HOV,
}

/// A formatter that builds output with automatic line breaking
pub struct Formatter {
    /// Accumulated tokens
    tokens: Vec<Token>,
    /// Target line width
    margin: usize,
}

impl Formatter {
    /// Create a new formatter
    pub fn new() -> Self {
        Formatter {
            tokens: Vec::new(),
            margin: DEFAULT_MARGIN,
        }
    }

    /// Set the margin (line width)
    pub fn set_margin(&mut self, margin: usize) {
        self.margin = margin;
    }

    /// Print a string
    pub fn print_string(&mut self, s: &str) {
        self.tokens.push(Token::String(s.to_string()));
    }

    /// Print a break hint (space or newline)
    /// nspaces: number of spaces if not breaking
    /// offset: indentation offset if breaking
    pub fn print_break(&mut self, nspaces: usize, offset: i32) {
        self.tokens.push(Token::Break(nspaces, offset));
    }

    /// Print a space that can become a newline
    pub fn print_space(&mut self) {
        self.print_break(1, 0);
    }

    /// Print a cut (zero-width break)
    pub fn print_cut(&mut self) {
        self.print_break(0, 0);
    }

    /// Open a box
    pub fn open_box(&mut self, kind: BoxKind, indent: i32) {
        self.tokens.push(Token::OpenBox(kind, indent));
    }

    /// Open a horizontal-or-vertical box with given indent
    pub fn open_hvbox(&mut self, indent: i32) {
        self.open_box(BoxKind::HV, indent);
    }

    /// Open a horizontal-or-vertical overflow box with given indent
    pub fn open_hovbox(&mut self, indent: i32) {
        self.open_box(BoxKind::HOV, indent);
    }

    /// Open a horizontal box
    pub fn open_hbox(&mut self) {
        self.open_box(BoxKind::H, 0);
    }

    /// Open a vertical box with given indent
    pub fn open_vbox(&mut self, indent: i32) {
        self.open_box(BoxKind::V, indent);
    }

    /// Close the current box
    pub fn close_box(&mut self) {
        self.tokens.push(Token::CloseBox);
    }

    /// Render the formatted output to a writer
    pub fn render(&self, out: &mut impl Write) {
        let mut renderer = Renderer::new(self.margin);
        renderer.render(&self.tokens, out);
    }

    /// Render to a string
    pub fn to_string(&self) -> String {
        let mut buf = Vec::new();
        self.render(&mut buf);
        String::from_utf8(buf).unwrap_or_default()
    }
}

impl Default for Formatter {
    fn default() -> Self {
        Self::new()
    }
}

/// State for rendering
struct Renderer {
    margin: usize,
    /// Current column position
    col: usize,
    /// Stack of (box_kind, indent_level)
    box_stack: Vec<(BoxKind, usize)>,
    /// Current indentation level
    indent: usize,
}

impl Renderer {
    fn new(margin: usize) -> Self {
        Renderer {
            margin,
            col: 0,
            box_stack: Vec::new(),
            indent: 0,
        }
    }

    fn render(&mut self, tokens: &[Token], out: &mut impl Write) {
        // First pass: compute sizes and determine breaking
        let breaks = self.compute_breaks(tokens);

        // Second pass: render with breaks
        self.render_with_breaks(tokens, &breaks, out);
    }

    /// Compute which breaks should be taken
    fn compute_breaks(&self, tokens: &[Token]) -> Vec<bool> {
        let mut breaks = vec![false; tokens.len()];
        let mut pos = 0usize;
        let mut box_stack: Vec<(BoxKind, usize, bool)> = Vec::new(); // (kind, start_pos, force_break)

        for (i, token) in tokens.iter().enumerate() {
            match token {
                Token::String(s) => {
                    pos += s.chars().count();
                }
                Token::Break(nspaces, _) => {
                    // Check if we need to break
                    let should_break = if let Some((kind, _start, force)) = box_stack.last() {
                        match kind {
                            BoxKind::H => false,
                            BoxKind::V => true,
                            BoxKind::HV => *force || pos > self.margin,
                            BoxKind::HOV => pos > self.margin,
                        }
                    } else {
                        pos > self.margin
                    };

                    if should_break {
                        breaks[i] = true;
                        // Find indent level
                        let indent = box_stack.last().map(|(_, _, _)| self.indent).unwrap_or(0);
                        pos = indent;
                        // Mark the box as needing breaks for HV
                        if let Some((BoxKind::HV, _, force)) = box_stack.last_mut() {
                            *force = true;
                        }
                    } else {
                        pos += nspaces;
                    }
                }
                Token::OpenBox(kind, indent) => {
                    let new_indent = pos + (*indent as usize);
                    box_stack.push((*kind, pos, false));
                    // For simple implementation, we'll check if content fits
                }
                Token::CloseBox => {
                    box_stack.pop();
                }
            }
        }

        breaks
    }

    /// Render with pre-computed breaks
    fn render_with_breaks(&mut self, tokens: &[Token], breaks: &[bool], out: &mut impl Write) {
        for (i, token) in tokens.iter().enumerate() {
            match token {
                Token::String(s) => {
                    let _ = out.write_all(s.as_bytes());
                    self.col += s.chars().count();
                }
                Token::Break(nspaces, offset) => {
                    if breaks[i] {
                        let _ = out.write_all(b"\n");
                        let indent = (self.indent as i32 + offset).max(0) as usize;
                        for _ in 0..indent {
                            let _ = out.write_all(b" ");
                        }
                        self.col = indent;
                    } else {
                        for _ in 0..*nspaces {
                            let _ = out.write_all(b" ");
                        }
                        self.col += nspaces;
                    }
                }
                Token::OpenBox(_, indent) => {
                    let new_indent = self.col + (*indent as usize);
                    self.box_stack.push((BoxKind::H, self.indent)); // Store old indent
                    self.indent = new_indent;
                }
                Token::CloseBox => {
                    if let Some((_, old_indent)) = self.box_stack.pop() {
                        self.indent = old_indent;
                    }
                }
            }
        }
    }
}

/// Macro-like helper for building formatted output
/// Similar to OCaml's pp f "format" args
pub struct PP<'a, W: Write> {
    out: &'a mut W,
    margin: usize,
    col: usize,
    indent_stack: Vec<usize>,
}

impl<'a, W: Write> PP<'a, W> {
    pub fn new(out: &'a mut W) -> Self {
        PP {
            out,
            margin: DEFAULT_MARGIN,
            col: 0,
            indent_stack: vec![0],
        }
    }

    /// Get current column
    pub fn col(&self) -> usize {
        self.col
    }

    /// Get current indent
    pub fn indent(&self) -> usize {
        *self.indent_stack.last().unwrap_or(&0)
    }

    /// Print a string
    pub fn str(&mut self, s: &str) {
        let _ = self.out.write_all(s.as_bytes());
        // Update column - handle newlines in string
        if let Some(last_newline) = s.rfind('\n') {
            self.col = s.len() - last_newline - 1;
        } else {
            self.col += s.chars().count();
        }
    }

    /// Print a space
    pub fn space(&mut self) {
        self.str(" ");
    }

    /// Print a newline with current indentation
    pub fn newline(&mut self) {
        let _ = self.out.write_all(b"\n");
        let indent = self.indent();
        for _ in 0..indent {
            let _ = self.out.write_all(b" ");
        }
        self.col = indent;
    }

    /// Print a break - space if fits, newline otherwise
    pub fn break_(&mut self, nspaces: usize, offset: i32) {
        if self.col > self.margin {
            self.newline();
            // Apply offset
            let extra = offset.max(0) as usize;
            for _ in 0..extra {
                let _ = self.out.write_all(b" ");
            }
            self.col += extra;
        } else {
            for _ in 0..nspaces {
                let _ = self.out.write_all(b" ");
            }
            self.col += nspaces;
        }
    }

    /// Print a breakable space
    pub fn sp(&mut self) {
        self.break_(1, 0);
    }

    /// Print a cut (zero-width break)
    pub fn cut(&mut self) {
        self.break_(0, 0);
    }

    /// Push indentation level
    pub fn push_indent(&mut self, indent: usize) {
        self.indent_stack.push(indent);
    }

    /// Pop indentation level
    pub fn pop_indent(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    /// Open a box at current position + offset
    pub fn open_box(&mut self, offset: i32) {
        let new_indent = (self.col as i32 + offset).max(0) as usize;
        self.push_indent(new_indent);
    }

    /// Close the current box
    pub fn close_box(&mut self) {
        self.pop_indent();
    }

    /// Check if we should break (column exceeds margin)
    pub fn should_break(&self) -> bool {
        self.col > self.margin
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_output() {
        let mut f = Formatter::new();
        f.print_string("hello");
        f.print_space();
        f.print_string("world");
        assert_eq!(f.to_string(), "hello world");
    }

    #[test]
    fn test_pp_basic() {
        let mut buf = Vec::new();
        {
            let mut pp = PP::new(&mut buf);
            pp.str("hello");
            pp.space();
            pp.str("world");
        }
        assert_eq!(String::from_utf8(buf).unwrap(), "hello world");
    }
}
