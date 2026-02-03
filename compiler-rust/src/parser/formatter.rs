//! OCaml-compatible Format module implementation
//!
//! This module provides a pretty-printing formatter that matches OCaml's Format module
//! semantics. It uses a box-based system with deferred line-break decisions.
//!
//! # Box Types
//!
//! - **H (horizontal)**: Never breaks - content always on one line
//! - **V (vertical)**: Always breaks at every break hint
//! - **HV (horizontal-or-vertical)**: All on one line OR all broken (exclusive choice)
//! - **HOV (horizontal-or-vertical packing)**: Pack as much as fits, break when necessary
//!
//! # Example
//!
//! ```ignore
//! let mut f = Formatter::new(output);
//! f.open_hovbox(2);  // @[<hov2>
//! f.string("let ");
//! f.string("x");
//! f.space();         // @;
//! f.string("=");
//! f.space();         // @;
//! f.string("42");
//! f.close_box();     // @]
//! f.flush();
//! ```

use std::io::Write;

/// Default margin (line width) - same as OCaml's default
/// OCaml's Format module uses 78 as the default margin, not 80
pub const DEFAULT_MARGIN: usize = 78;

/// Box types matching OCaml's Format module
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoxKind {
    /// Horizontal - never break (open_hbox)
    H,
    /// Vertical - always break at hints (open_vbox)
    V,
    /// Horizontal-or-Vertical exclusive (open_hvbox)
    /// Either all on one line OR all broken
    HV,
    /// Horizontal-or-Vertical packing (open_hovbox)
    /// Pack as much as fits, break when necessary
    HOV,
    /// Structural box (open_box / @[<n>)
    /// Like HOV packing but with additional indentation-aware break logic:
    /// 1. If line was just broken, don't break again (stay on same line)
    /// 2. If content doesn't fit, break
    /// 3. If current indent > margin - width + offset, break (indentation improvement)
    /// 4. Otherwise, same line
    Box,
}

/// Tokens in the output stream
#[derive(Clone, Debug)]
enum Token {
    /// Raw string to output
    String(String),
    /// Break hint: space if fits, newline+indent if not
    /// nspaces: number of spaces if not breaking
    /// offset: additional indent offset if breaking
    Break { nspaces: usize, offset: i32 },
    /// Open a new box
    OpenBox { kind: BoxKind, indent: i32 },
    /// Close current box
    CloseBox,
    /// Force a newline
    Newline,
}

/// A box containing buffered tokens
#[derive(Debug)]
struct Box {
    kind: BoxKind,
    indent: i32,
    tokens: Vec<Token>,
}

impl Box {
    fn new(kind: BoxKind, indent: i32) -> Self {
        Box {
            kind,
            indent,
            tokens: Vec::new(),
        }
    }
}

/// Pretty-printing formatter matching OCaml's Format module
pub struct Formatter<W: Write> {
    out: W,
    /// Target line width
    margin: usize,
    /// Current column position (0-based)
    col: usize,
    /// The current top-level box (only one at a time, nested boxes are tokens)
    current_box: Option<Box>,
    /// Depth of nested boxes (0 means we're in the top-level box)
    box_depth: usize,
    /// Stack of indentation levels
    indent_stack: Vec<usize>,
    /// Stack tracking whether each box level has content with embedded newlines
    /// When content has newlines, cuts in HOV packing mode should break
    content_newline_stack: Vec<bool>,
    /// Stack of space_left values when each box was opened (OCaml's 'width' in format_stack)
    /// Used by Pp_box for the indentation improvement check
    width_stack: Vec<usize>,
    /// Whether we're at the beginning of a new line (just after a break)
    /// Used by Pp_box to avoid breaking immediately after a break
    is_new_line: bool,
    /// Current indentation level (set by break_new_line, used by Pp_box)
    /// OCaml's pp_current_indent
    pp_current_indent: usize,
    /// Maximum indentation (OCaml's pp_max_indent = pp_margin - pp_min_space_left)
    /// Default: 68 (78 - 10)
    pp_max_indent: usize,
    /// Stack tracking whether each box level is in "fits" mode (Pp_fits in OCaml).
    /// When a box fits on one line, pp_force_break_line should not break inside it.
    fits_stack: Vec<bool>,
}

impl<W: Write> Formatter<W> {
    /// Create a new formatter writing to the given output
    pub fn new(out: W) -> Self {
        Formatter {
            out,
            margin: DEFAULT_MARGIN,
            col: 0,
            current_box: None,
            box_depth: 0,
            indent_stack: vec![0],
            content_newline_stack: vec![false],
            width_stack: vec![DEFAULT_MARGIN],
            is_new_line: true,
            pp_current_indent: 0,
            pp_max_indent: DEFAULT_MARGIN - 10, // OCaml: pp_margin - pp_min_space_left = 78 - 10 = 68
            fits_stack: vec![false],
        }
    }

    /// Set the margin (line width)
    pub fn set_margin(&mut self, margin: usize) {
        self.margin = margin;
    }

    /// Get current column position
    pub fn column(&self) -> usize {
        self.col
    }

    /// Get current indentation level
    fn current_indent(&self) -> usize {
        *self.indent_stack.last().unwrap_or(&0)
    }

    // ========================================================================
    // Low-level output methods (bypass boxing)
    // ========================================================================

    /// Write a string directly to output, updating column
    /// Also marks the current box as broken if string contains newlines
    fn write_raw(&mut self, s: &str) {
        let _ = self.out.write_all(s.as_bytes());
        // OCaml's pp_print_string advances the position counter by the string length,
        // WITHOUT tracking newlines within the string. This means multiline strings
        // "consume" space from the remaining margin, making subsequent breaks more
        // likely to fire. We match this behavior for parity.
        self.col += s.chars().count();
        if !s.is_empty() {
            self.is_new_line = false;
        }
    }

    /// Write n spaces directly
    fn write_spaces(&mut self, n: usize) {
        for _ in 0..n {
            let _ = self.out.write_all(b" ");
        }
        self.col += n;
        // Spaces don't reset is_new_line - they're part of the break/indent
    }

    /// Write newline and indent to current level + offset
    /// Note: does NOT set content_newline flag since this is a formatter break, not content
    fn write_newline_indent(&mut self, offset: i32) {
        let _ = self.out.write_all(b"\n");
        let indent = (self.current_indent() as i32 + offset).max(0) as usize;
        // OCaml: let real_indent = min pp_max_indent indent
        let real_indent = indent.min(self.pp_max_indent);
        for _ in 0..real_indent {
            let _ = self.out.write_all(b" ");
        }
        self.col = real_indent;
        self.is_new_line = true;
        self.pp_current_indent = real_indent;
    }

    /// OCaml-compatible break_new_line: computes indent from margin - width + offset
    /// Used for structural box breaks
    fn write_break_new_line(&mut self, offset: i32, width: usize) {
        let _ = self.out.write_all(b"\n");
        let indent = (self.margin as i32 - width as i32 + offset).max(0) as usize;
        let max_indent = self.margin - 1; // pp_max_indent
        let real_indent = indent.min(max_indent);
        for _ in 0..real_indent {
            let _ = self.out.write_all(b" ");
        }
        self.col = real_indent;
        self.is_new_line = true;
        self.pp_current_indent = real_indent;
    }

    /// Check if current box has content with embedded newlines
    fn has_content_newline(&self) -> bool {
        *self.content_newline_stack.last().unwrap_or(&false)
    }

    /// OCaml's pp_force_break_line: if the current column exceeds pp_max_indent,
    /// force a line break in the parent box to prevent boxes from opening too far right.
    /// Only breaks if the parent box has room (width > space_left) and the parent
    /// box type supports breaking (not hbox or fits).
    fn pp_force_break_line(&mut self) {
        // Don't force break if parent box is in "fits" mode (Pp_fits or Pp_hbox)
        if *self.fits_stack.last().unwrap_or(&false) {
            return;
        }
        let insertion_point = self.col;
        if insertion_point > self.pp_max_indent {
            // Get the parent box's width from the stack
            if let Some(&width) = self.width_stack.last() {
                let space_left = self.margin.saturating_sub(self.col);
                if width > space_left {
                    // Force a break using the parent box's width
                    let _ = self.out.write_all(b"\n");
                    let indent = self.margin.saturating_sub(width);
                    let real_indent = indent.min(self.pp_max_indent);
                    for _ in 0..real_indent {
                        let _ = self.out.write_all(b" ");
                    }
                    self.col = real_indent;
                    self.is_new_line = true;
                    self.pp_current_indent = real_indent;
                }
            }
        }
    }

    // ========================================================================
    // Token handling
    // ========================================================================

    /// Add a token to the current box, or write directly if no box is open
    fn emit(&mut self, token: Token) {
        if let Some(box_) = self.current_box.as_mut() {
            box_.tokens.push(token);
        } else {
            // No box open - render immediately
            self.render_token(&token, false);
        }
    }

    // ========================================================================
    // Public API - Box management
    // ========================================================================

    /// Open a box with the given kind and indent offset
    ///
    /// The indent is relative to the current column when content is rendered.
    pub fn open_box(&mut self, kind: BoxKind, indent: i32) {
        if self.current_box.is_none() {
            // Top-level box - start fresh
            self.current_box = Some(Box::new(kind, indent));
            self.box_depth = 1;
        } else {
            // Nested box - add token to current box
            self.emit(Token::OpenBox { kind, indent });
            self.box_depth += 1;
        }
    }

    /// Open a horizontal box (never breaks)
    pub fn open_hbox(&mut self) {
        self.open_box(BoxKind::H, 0);
    }

    /// Open a vertical box (always breaks)
    pub fn open_vbox(&mut self, indent: i32) {
        self.open_box(BoxKind::V, indent);
    }

    /// Open a horizontal-or-vertical box (exclusive choice)
    pub fn open_hvbox(&mut self, indent: i32) {
        self.open_box(BoxKind::HV, indent);
    }

    /// Open a horizontal-or-vertical packing box
    pub fn open_hovbox(&mut self, indent: i32) {
        self.open_box(BoxKind::HOV, indent);
    }

    /// Close the current box and render its contents
    pub fn close_box(&mut self) {
        if self.box_depth == 0 {
            return;
        }

        self.box_depth -= 1;

        if self.box_depth > 0 {
            // Closing a nested box - just emit CloseBox token
            self.emit(Token::CloseBox);
        } else {
            // Closing the top-level box - render everything
            if let Some(box_) = self.current_box.take() {
                // Calculate if box fits on one line
                let size = self.calculate_size(&box_.tokens);
                let fits = self.col + size <= self.margin;

                // Push new indent level and broken state for this box
                let box_indent = (self.col as i32 + box_.indent).max(0) as usize;
                let width = self.margin.saturating_sub(self.col);
                self.indent_stack.push(box_indent);
                self.content_newline_stack.push(false);
                self.width_stack.push(width);

                let is_fits = fits || matches!(box_.kind, BoxKind::H);
                self.fits_stack.push(is_fits);

                // Render based on box kind and whether it fits
                match box_.kind {
                    BoxKind::H => {
                        // Always horizontal
                        self.render_tokens(&box_.tokens, false);
                    }
                    BoxKind::V => {
                        // Always vertical
                        self.render_tokens(&box_.tokens, true);
                    }
                    BoxKind::HV => {
                        // Exclusive: all horizontal OR all vertical
                        self.render_tokens(&box_.tokens, !fits);
                    }
                    BoxKind::HOV => {
                        // Packing: break only when necessary
                        self.render_tokens_packing(&box_.tokens);
                    }
                    BoxKind::Box => {
                        // Structural box: packing with indentation-aware breaks
                        self.render_tokens_structural(&box_.tokens);
                    }
                }

                self.fits_stack.pop();
                // Pop indent level and broken state
                self.indent_stack.pop();
                self.content_newline_stack.pop();
                self.width_stack.pop();
            }
        }
    }

    // ========================================================================
    // Public API - Content
    // ========================================================================

    /// Print a string
    pub fn string(&mut self, s: &str) {
        self.emit(Token::String(s.to_string()));
    }

    /// Print a string slice (alias for string)
    pub fn str(&mut self, s: &str) {
        self.string(s);
    }

    /// Print formatted content using format_args!
    ///
    /// Usage: `f.print(format_args!("{} = {}", name, value))`
    pub fn print(&mut self, args: std::fmt::Arguments<'_>) {
        self.string(&args.to_string());
    }

    /// Break hint: space if fits, newline+indent if not
    ///
    /// - `nspaces`: number of spaces if not breaking
    /// - `offset`: additional indent offset if breaking
    pub fn break_(&mut self, nspaces: usize, offset: i32) {
        self.emit(Token::Break { nspaces, offset });
    }

    /// Space that may become a newline (equivalent to @; in OCaml)
    pub fn space(&mut self) {
        self.break_(1, 0);
    }

    /// Cut - zero-width break point (equivalent to @, in OCaml)
    pub fn cut(&mut self) {
        self.break_(0, 0);
    }

    /// Force a newline
    pub fn newline(&mut self) {
        self.emit(Token::Newline);
    }

    /// Flush all pending output
    pub fn flush(&mut self) {
        // Close any remaining boxes
        while self.box_depth > 0 {
            self.close_box();
        }
        let _ = self.out.flush();
    }

    // ========================================================================
    // Size calculation
    // ========================================================================

    /// Calculate the horizontal size of tokens (if all on one line)
    /// This includes breaks at ALL nesting levels since we're calculating
    /// the size if everything was rendered horizontally.
    /// In OCaml's Format, forced newlines (pp_force_newline) have size=0 and length=0,
    /// meaning they don't affect the parent box's "does it fit" calculation.
    /// We track box nesting depth so that Newlines inside nested boxes have size 0
    /// (matching OCaml behavior) while Newlines at the top level force the box to break.
    fn calculate_size(&self, tokens: &[Token]) -> usize {
        let mut size = 0;
        let mut depth = 0;

        for token in tokens {
            match token {
                Token::String(s) => {
                    size += s.chars().count();
                }
                Token::Break { nspaces, .. } => {
                    // Count break spaces at all levels since we're measuring
                    // the horizontal (non-breaking) size
                    size += nspaces;
                }
                Token::OpenBox { .. } => {
                    depth += 1;
                }
                Token::CloseBox => {
                    if depth > 0 {
                        depth -= 1;
                    }
                }
                Token::Newline => {
                    // In OCaml's Format, pp_force_newline has size=0 and length=0.
                    // Forced newlines always render as newlines but don't affect
                    // the box's "does it fit" calculation.
                }
            }
        }
        size
    }

    /// Calculate size of tokens up to next break (for packing)
    fn size_until_break(&self, tokens: &[Token], start: usize) -> usize {
        let mut size = 0;
        let mut depth = 0;

        for token in tokens.iter().skip(start) {
            match token {
                Token::String(s) => {
                    size += s.chars().count();
                }
                Token::Break { nspaces, .. } if depth == 0 => {
                    // Found a break at current level.
                    // Include its nspaces in the size, matching OCaml's scan_push
                    // behavior where the next break's length is included in the
                    // current break's computed size.
                    size += nspaces;
                    return size;
                }
                Token::Break { nspaces, .. } => {
                    size += nspaces;
                }
                Token::OpenBox { .. } => {
                    depth += 1;
                }
                Token::CloseBox => {
                    if depth > 0 {
                        depth -= 1;
                    }
                }
                Token::Newline => {
                    // Forced newlines have size 0 for measurement (OCaml behavior)
                }
            }
        }
        size
    }

    // ========================================================================
    // Rendering
    // ========================================================================

    /// Render a single token
    fn render_token(&mut self, token: &Token, breaking: bool) {
        match token {
            Token::String(s) => {
                self.write_raw(s);
            }
            Token::Break { nspaces, offset } => {
                if breaking {
                    self.write_newline_indent(*offset);
                } else {
                    self.write_spaces(*nspaces);
                }
            }
            Token::OpenBox { .. } => {
                // OpenBox tokens are handled by render_tokens, not render_token
                // This should not be reached in normal usage
            }
            Token::CloseBox => {
                // CloseBox tokens are handled by render_tokens, not render_token
                // This should not be reached in normal usage
            }
            Token::Newline => {
                self.write_newline_indent(0);
            }
        }
    }

    /// Render tokens with uniform break decision
    fn render_tokens(&mut self, tokens: &[Token], breaking: bool) {
        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];
            match token {
                Token::OpenBox { kind, indent } => {
                    // Find matching close and render nested box
                    let (nested_tokens, end_idx) = self.extract_nested_box(tokens, i + 1);

                    // OCaml: force break if opening box beyond pp_max_indent
                    self.pp_force_break_line();

                    // Push indent, broken state, and width for nested box
                    let box_indent = (self.col as i32 + indent).max(0) as usize;
                    let width = self.margin.saturating_sub(self.col);
                    self.indent_stack.push(box_indent);
                    self.content_newline_stack.push(false);
                    self.width_stack.push(width);

                    // Calculate if nested box fits
                    let size = self.calculate_size(&nested_tokens);
                    let fits = self.col + size <= self.margin;

                    // OCaml: if box fits, it becomes Pp_fits (except Vbox)
                    // H boxes are always "fits" (they never break)
                    let is_fits = fits || matches!(kind, BoxKind::H);
                    self.fits_stack.push(is_fits);

                    // Render based on box kind
                    match kind {
                        BoxKind::H => self.render_tokens(&nested_tokens, false),
                        BoxKind::V => self.render_tokens(&nested_tokens, true),
                        BoxKind::HV => self.render_tokens(&nested_tokens, !fits),
                        BoxKind::HOV => self.render_tokens_packing(&nested_tokens),
                        BoxKind::Box => self.render_tokens_structural(&nested_tokens),
                    }

                    self.fits_stack.pop();
                    self.indent_stack.pop();
                    self.content_newline_stack.pop();
                    self.width_stack.pop();
                    i = end_idx + 1;
                }
                Token::CloseBox => {
                    // Skip - handled by extract_nested_box
                    i += 1;
                }
                _ => {
                    self.render_token(token, breaking);
                    i += 1;
                }
            }
        }
    }

    /// Render tokens with packing (break only when necessary)
    /// In HOV packing mode, once the box is "broken" (had a line break),
    /// all subsequent breaks also break. This matches OCaml's Format behavior.
    fn render_tokens_packing(&mut self, tokens: &[Token]) {
        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];
            match token {
                Token::OpenBox { kind, indent } => {
                    // Find matching close and render nested box
                    let (nested_tokens, end_idx) = self.extract_nested_box(tokens, i + 1);

                    // OCaml: force break if opening box beyond pp_max_indent
                    self.pp_force_break_line();

                    // Push indent, broken state, and width for nested box
                    let box_indent = (self.col as i32 + indent).max(0) as usize;
                    let width = self.margin.saturating_sub(self.col);
                    self.indent_stack.push(box_indent);
                    self.content_newline_stack.push(false);
                    self.width_stack.push(width);

                    // Calculate if nested box fits
                    let size = self.calculate_size(&nested_tokens);
                    let fits = self.col + size <= self.margin;

                    let is_fits = fits || matches!(kind, BoxKind::H);
                    self.fits_stack.push(is_fits);

                    // Render based on box kind
                    match kind {
                        BoxKind::H => self.render_tokens(&nested_tokens, false),
                        BoxKind::V => self.render_tokens(&nested_tokens, true),
                        BoxKind::HV => self.render_tokens(&nested_tokens, !fits),
                        BoxKind::HOV => self.render_tokens_packing(&nested_tokens),
                        BoxKind::Box => self.render_tokens_structural(&nested_tokens),
                    }

                    self.fits_stack.pop();
                    self.indent_stack.pop();
                    self.content_newline_stack.pop();
                    self.width_stack.pop();
                    i = end_idx + 1;
                }
                Token::CloseBox => {
                    i += 1;
                }
                Token::Break { nspaces, offset } => {
                    // In HOV packing: each break is evaluated independently based on fitness.
                    // OCaml: if size > state.pp_space_left then break_new_line else break_same_line
                    // where size = this_break.nspaces + content + next_break.nspaces
                    // (OCaml's scan_push includes the break's own length in the size computation)
                    // size_until_break already includes the next break's nspaces, so we add
                    // the current break's nspaces to match OCaml's size computation.
                    let size_ahead = self.size_until_break(tokens, i + 1);
                    let space_left = self.margin.saturating_sub(self.col);
                    if nspaces + size_ahead > space_left {
                        // Doesn't fit - break
                        self.write_newline_indent(*offset);
                    } else {
                        // Fits - use spaces
                        self.write_spaces(*nspaces);
                    }
                    i += 1;
                }
                Token::String(s) => {
                    self.write_raw(s);
                    i += 1;
                }
                Token::Newline => {
                    self.write_newline_indent(0);
                    i += 1;
                }
            }
        }
    }

    /// Render tokens with structural box (Pp_box) semantics
    /// Like HOV packing but with additional indentation-aware break logic:
    /// 1. If line was just broken (is_new_line), don't break (stay on same line)
    /// 2. If content doesn't fit, break
    /// 3. If current indent > margin - width + offset, break (indentation improvement)
    /// 4. Otherwise, same line
    fn render_tokens_structural(&mut self, tokens: &[Token]) {
        // Get the width (space_left when this box was opened)
        let box_width = *self.width_stack.last().unwrap_or(&self.margin);
        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];
            match token {
                Token::OpenBox { kind, indent } => {
                    let (nested_tokens, end_idx) = self.extract_nested_box(tokens, i + 1);

                    // OCaml: force break if opening box beyond pp_max_indent
                    self.pp_force_break_line();

                    let box_indent = (self.col as i32 + indent).max(0) as usize;
                    let width = self.margin.saturating_sub(self.col);
                    self.indent_stack.push(box_indent);
                    self.content_newline_stack.push(false);
                    self.width_stack.push(width);

                    let size = self.calculate_size(&nested_tokens);
                    let fits = self.col + size <= self.margin;

                    let is_fits = fits || matches!(kind, BoxKind::H);
                    self.fits_stack.push(is_fits);

                    match kind {
                        BoxKind::H => self.render_tokens(&nested_tokens, false),
                        BoxKind::V => self.render_tokens(&nested_tokens, true),
                        BoxKind::HV => self.render_tokens(&nested_tokens, !fits),
                        BoxKind::HOV => self.render_tokens_packing(&nested_tokens),
                        BoxKind::Box => self.render_tokens_structural(&nested_tokens),
                    }

                    self.fits_stack.pop();
                    self.indent_stack.pop();
                    self.content_newline_stack.pop();
                    self.width_stack.pop();
                    i = end_idx + 1;
                }
                Token::CloseBox => {
                    i += 1;
                }
                Token::Break { nspaces, offset } => {
                    // Pp_box break logic from OCaml's format.ml:
                    // if pp_is_new_line then break_same_line
                    // else if size > space_left then break_new_line
                    // else if pp_current_indent > pp_margin - width + off then break_new_line
                    // else break_same_line
                    // where 'width' = space_left when this box was opened
                    //
                    // Note: margin - width + off = col_at_box_open + box_indent + off
                    // which equals current_indent + off (what write_newline_indent computes)
                    if self.is_new_line {
                        // 1. Line was just broken - stay on same line
                        self.write_spaces(*nspaces);
                    } else {
                        let size_ahead = self.size_until_break(tokens, i + 1);
                        let space_left = self.margin.saturating_sub(self.col);
                        // OCaml: size > space_left where size includes this break's nspaces
                        // (OCaml's scan_push includes the break's own length in size computation)
                        // size_until_break includes next break's nspaces, we add current nspaces
                        if nspaces + size_ahead > space_left {
                            // 2. Doesn't fit - break
                            self.write_newline_indent(*offset);
                        } else {
                            // 3. Check indentation improvement:
                            // OCaml: pp_current_indent > pp_margin - width + off
                            // Equivalent: pp_current_indent > box_start_col + box_indent + off
                            // = pp_current_indent > current_indent + off
                            let target_indent = (self.current_indent() as i32 + offset).max(0) as usize;
                            if self.pp_current_indent > target_indent {
                                // Breaking would reduce indentation - do it
                                self.write_newline_indent(*offset);
                            } else {
                                // 4. Same line
                                self.write_spaces(*nspaces);
                            }
                        }
                    }
                    i += 1;
                }
                Token::String(s) => {
                    self.write_raw(s);
                    i += 1;
                }
                Token::Newline => {
                    self.write_newline_indent(0);
                    i += 1;
                }
            }
        }
    }

    /// Extract nested box tokens, returning (tokens, index of CloseBox)
    fn extract_nested_box(&self, tokens: &[Token], start: usize) -> (Vec<Token>, usize) {
        let mut result = Vec::new();
        let mut depth = 1;
        let mut i = start;

        while i < tokens.len() && depth > 0 {
            match &tokens[i] {
                Token::OpenBox { .. } => {
                    depth += 1;
                    result.push(tokens[i].clone());
                }
                Token::CloseBox => {
                    depth -= 1;
                    if depth > 0 {
                        result.push(tokens[i].clone());
                    }
                }
                _ => {
                    result.push(tokens[i].clone());
                }
            }
            i += 1;
        }

        (result, i - 1)
    }
}

// ============================================================================
// Convenience macros
// ============================================================================

/// Print formatted content to a Formatter
///
/// Usage: `pp!(f, "let {} = {}", name, value)`
#[macro_export]
macro_rules! pp {
    ($f:expr, $($arg:tt)*) => {
        $f.print(format_args!($($arg)*))
    };
}

/// Re-export for use in other modules
pub use pp;

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn format_to_string<F>(f: F) -> String
    where
        F: for<'a> FnOnce(&mut Formatter<&'a mut Vec<u8>>),
    {
        let mut buf = Vec::new();
        {
            let mut fmt = Formatter::new(&mut buf);
            f(&mut fmt);
            fmt.flush();
        }
        String::from_utf8(buf).unwrap()
    }

    #[test]
    fn test_simple_string() {
        let result = format_to_string(|f| {
            f.string("hello world");
        });
        assert_eq!(result, "hello world");
    }

    #[test]
    fn test_hovbox_fits() {
        let result = format_to_string(|f| {
            f.open_hovbox(2);
            f.string("let");
            f.space();
            f.string("x");
            f.space();
            f.string("=");
            f.space();
            f.string("42");
            f.close_box();
        });
        assert_eq!(result, "let x = 42");
    }

    #[test]
    fn test_hovbox_breaks() {
        let result = format_to_string(|f| {
            f.set_margin(15);
            f.open_hovbox(2);
            f.string("let");
            f.space();
            f.string("x");
            f.space();
            f.string("=");
            f.space();
            f.string("very_long_value");
            f.close_box();
        });
        // Should break because total length > 15
        assert!(result.contains('\n'));
    }

    #[test]
    fn test_hvbox_all_or_nothing() {
        let result = format_to_string(|f| {
            f.set_margin(20);
            f.open_hvbox(2);
            f.string("a");
            f.space();
            f.string("b");
            f.space();
            f.string("c");
            f.space();
            f.string("d");
            f.space();
            f.string("e");
            f.space();
            f.string("f");
            f.close_box();
        });
        // HV box: if it doesn't fit, ALL breaks become newlines
        let newlines = result.matches('\n').count();
        assert!(newlines == 0 || newlines == 5, "HV box should be all-or-nothing");
    }

    #[test]
    fn test_vbox_always_breaks() {
        let result = format_to_string(|f| {
            f.open_vbox(0);
            f.string("a");
            f.space();
            f.string("b");
            f.space();
            f.string("c");
            f.close_box();
        });
        // V box: always breaks at hints
        assert_eq!(result.matches('\n').count(), 2);
    }

    #[test]
    fn test_hbox_never_breaks() {
        let result = format_to_string(|f| {
            f.set_margin(5); // Very small margin
            f.open_hbox();
            f.string("hello");
            f.space();
            f.string("world");
            f.close_box();
        });
        // H box: never breaks
        assert!(!result.contains('\n'));
        assert_eq!(result, "hello world");
    }

    #[test]
    fn test_nested_boxes() {
        let result = format_to_string(|f| {
            f.open_hovbox(0);
            f.string("outer");
            f.space();
            f.open_hovbox(2);
            f.string("inner");
            f.space();
            f.string("content");
            f.close_box();
            f.close_box();
        });
        assert_eq!(result, "outer inner content");
    }

    #[test]
    fn test_indent() {
        let result = format_to_string(|f| {
            f.set_margin(10);
            f.open_hvbox(2);
            f.string("let x =");
            f.space();
            f.string("very_long");
            f.close_box();
        });
        // Should break and indent by 2
        assert!(result.contains("\n  "));
    }

    #[test]
    fn test_cut() {
        let result = format_to_string(|f| {
            f.set_margin(5);
            f.open_hvbox(0);
            f.string("ab");
            f.cut();
            f.string("cd");
            f.close_box();
        });
        // Cut is zero-width break
        assert!(result == "abcd" || result == "ab\ncd");
    }

    #[test]
    fn test_pp_macro() {
        let result = format_to_string(|f| {
            let name = "x";
            let value = 42;
            pp!(f, "let {} = {}", name, value);
        });
        assert_eq!(result, "let x = 42");
    }
}
