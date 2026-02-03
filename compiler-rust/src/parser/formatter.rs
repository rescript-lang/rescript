//! OCaml-compatible Format module implementation
//!
//! This module provides a pretty-printing formatter that matches OCaml's Format module
//! semantics exactly. It uses a queue-based scan algorithm that defers line-break decisions
//! until enough content has been seen, matching OCaml's streaming approach.
//!
//! # Box Types
//!
//! - **H (horizontal)**: Never breaks - content always on one line
//! - **V (vertical)**: Always breaks at every break hint
//! - **HV (horizontal-or-vertical)**: All on one line OR all broken (exclusive choice)
//! - **HOV (horizontal-or-vertical packing)**: Pack as much as fits, break when necessary
//! - **Box**: Like HOV but with indentation-aware break logic
//!
//! # Algorithm
//!
//! Tokens are enqueued with initially unknown sizes. A scan stack tracks unresolved
//! breaks and box-opens. When the next break arrives, the previous break's size is
//! resolved. `advance_left` processes the queue front whenever sizes are known or
//! the pending content exceeds available space (assigning pp_infinity to force breaks).

use std::collections::VecDeque;
use std::io::Write;

/// Default margin (line width) - same as OCaml's default
pub const DEFAULT_MARGIN: usize = 78;

/// Large value for default token size (matching OCaml's pp_infinity)
const PP_INFINITY: i32 = 1000000010;

/// Box types matching OCaml's Format module
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoxKind {
    /// Horizontal - never break (Pp_hbox)
    H,
    /// Vertical - always break at hints (Pp_vbox)
    V,
    /// Horizontal-or-Vertical exclusive (Pp_hvbox)
    HV,
    /// Horizontal-or-Vertical packing (Pp_hovbox)
    HOV,
    /// Structural box (Pp_box)
    Box,
    /// Box that fits on one line (Pp_fits) - computed at format time
    Fits,
}

/// Tokens in the pretty-printer queue (matching OCaml's pp_token)
#[derive(Clone, Debug)]
enum PpToken {
    /// Text to print (Pp_text)
    Text(String),
    /// Break hint (Pp_break)
    /// nspaces: spaces if not breaking, offset: indent offset if breaking
    Break { nspaces: i32, offset: i32 },
    /// Open a box (Pp_begin)
    Begin { indent: i32, kind: BoxKind },
    /// Close a box (Pp_end)
    End,
    /// Force a newline (Pp_newline)
    Newline,
}

/// Queue element with mutable size
#[derive(Clone, Debug)]
struct QueueElem {
    /// Size: negative means unknown (= -(pp_right_total at creation time))
    /// When resolved: total content from here to the next break/box-close
    size: i32,
    token: PpToken,
    /// Declared length of this token
    length: i32,
}

/// Scan stack element
#[derive(Clone, Debug)]
struct ScanElem {
    /// Value of pp_left_total when this element was enqueued
    left_total: i32,
    /// Index into the queue
    queue_index: usize,
}

/// Format stack element (describes an active box)
#[derive(Clone, Debug)]
struct FormatElem {
    box_type: BoxKind,
    width: i32,
}

/// Pretty-printing formatter matching OCaml's Format module
pub struct Formatter<W: Write> {
    out: W,
    /// Target line width (pp_margin)
    margin: i32,
    /// Minimal space left before margin when opening a box (pp_min_space_left)
    min_space_left: i32,
    /// Maximum indentation (pp_max_indent = pp_margin - pp_min_space_left)
    max_indent: i32,
    /// Space remaining on current line (pp_space_left)
    space_left: i32,
    /// Current indentation (pp_current_indent)
    current_indent: i32,
    /// True when line was just broken (pp_is_new_line)
    is_new_line: bool,
    /// Total width of tokens already printed (pp_left_total)
    left_total: i32,
    /// Total width of tokens ever enqueued (pp_right_total)
    right_total: i32,
    /// Current box depth (pp_curr_depth)
    curr_depth: i32,
    /// The queue of pending tokens
    queue: VecDeque<QueueElem>,
    /// The scan stack (tracks unresolved breaks and box-opens)
    scan_stack: Vec<ScanElem>,
    /// The format stack (tracks active boxes during rendering)
    format_stack: Vec<FormatElem>,
    /// Global monotonic index for queue elements
    queue_offset: usize,
}

impl<W: Write> Formatter<W> {
    /// Create a new formatter writing to the given output
    pub fn new(out: W) -> Self {
        let margin = DEFAULT_MARGIN as i32;
        let min_space_left = 10;
        let mut f = Formatter {
            out,
            margin,
            min_space_left,
            max_indent: margin - min_space_left,
            space_left: margin,
            current_indent: 0,
            is_new_line: true,
            left_total: 1,
            right_total: 1,
            curr_depth: 0,
            queue: VecDeque::new(),
            scan_stack: Vec::new(),
            format_stack: Vec::new(),
            queue_offset: 0,
        };
        // Initialize scan stack with sentinel (matching OCaml's initialize_scan_stack)
        f.initialize_scan_stack();
        // Open the system box (matching OCaml's pp_open_sys_box)
        f.pp_open_sys_box();
        f
    }

    /// Set the margin (line width)
    pub fn set_margin(&mut self, margin: usize) {
        self.margin = margin as i32;
        self.max_indent = self.margin - self.min_space_left;
        // Reset state like OCaml's pp_rinit
        self.pp_rinit();
    }

    /// Get current column position
    pub fn column(&self) -> usize {
        (self.margin - self.space_left).max(0) as usize
    }

    // ========================================================================
    // Output functions
    // ========================================================================

    fn pp_output_string(&mut self, s: &str) {
        let _ = self.out.write_all(s.as_bytes());
    }

    fn pp_output_newline(&mut self) {
        let _ = self.out.write_all(b"\n");
    }

    fn pp_output_spaces(&mut self, n: i32) {
        for _ in 0..n {
            let _ = self.out.write_all(b" ");
        }
    }

    fn pp_output_indent(&mut self, n: i32) {
        for _ in 0..n {
            let _ = self.out.write_all(b" ");
        }
    }

    // ========================================================================
    // Format helpers (matching OCaml's format.ml)
    // ========================================================================

    /// Format text: reduce space_left, output string
    fn format_pp_text(&mut self, size: i32, s: &str) {
        self.space_left -= size;
        self.pp_output_string(s);
        self.is_new_line = false;
    }

    /// Format string if non-empty
    fn format_string(&mut self, s: &str) {
        if !s.is_empty() {
            self.format_pp_text(s.len() as i32, s);
        }
    }

    /// Break to new line with given offset from box width
    fn break_new_line(&mut self, offset: i32, width: i32) {
        self.pp_output_newline();
        self.is_new_line = true;
        let indent = self.margin - width + offset;
        let real_indent = indent.min(self.max_indent).max(0);
        self.current_indent = real_indent;
        self.space_left = self.margin - self.current_indent;
        self.pp_output_indent(self.current_indent);
    }

    /// Break line (no offset)
    fn break_line(&mut self, width: i32) {
        self.break_new_line(0, width);
    }

    /// Stay on same line with given number of spaces
    fn break_same_line(&mut self, width: i32) {
        self.space_left -= width;
        self.pp_output_spaces(width);
    }

    /// Force break line if box is too far right (matching OCaml's pp_force_break_line)
    fn pp_force_break_line(&mut self) {
        match self.format_stack.last() {
            None => {
                self.pp_output_newline();
            }
            Some(elem) => {
                let width = elem.width;
                if width > self.space_left {
                    match elem.box_type {
                        BoxKind::Fits | BoxKind::H => {}
                        _ => {
                            self.break_line(width);
                        }
                    }
                }
            }
        }
    }

    // ========================================================================
    // Format token (matching OCaml's format_pp_token)
    // ========================================================================

    fn format_pp_token(&mut self, size: i32, token: &PpToken) {
        match token {
            PpToken::Text(s) => {
                self.format_pp_text(size, s);
            }

            PpToken::Begin { indent, kind } => {
                let insertion_point = self.margin - self.space_left;
                if insertion_point > self.max_indent {
                    self.pp_force_break_line();
                }
                let width = self.space_left - indent;
                let box_type = match kind {
                    BoxKind::V => BoxKind::V,
                    _ => {
                        if size > self.space_left {
                            *kind
                        } else {
                            BoxKind::Fits
                        }
                    }
                };
                self.format_stack.push(FormatElem { box_type, width });
            }

            PpToken::End => {
                self.format_stack.pop();
            }

            PpToken::Newline => {
                match self.format_stack.last() {
                    None => {
                        self.pp_output_newline();
                    }
                    Some(elem) => {
                        let width = elem.width;
                        self.break_line(width);
                    }
                }
            }

            PpToken::Break { nspaces, offset } => {
                match self.format_stack.last() {
                    None => {}
                    Some(elem) => {
                        let box_type = elem.box_type;
                        let width = elem.width;
                        match box_type {
                            BoxKind::HOV => {
                                if size > self.space_left {
                                    self.break_new_line(*offset, width);
                                } else {
                                    self.break_same_line(*nspaces);
                                }
                            }
                            BoxKind::Box => {
                                // Have the line just been broken here?
                                if self.is_new_line {
                                    self.break_same_line(*nspaces);
                                } else if size > self.space_left {
                                    self.break_new_line(*offset, width);
                                } else if self.current_indent > self.margin - width + *offset {
                                    self.break_new_line(*offset, width);
                                } else {
                                    self.break_same_line(*nspaces);
                                }
                            }
                            BoxKind::HV => {
                                self.break_new_line(*offset, width);
                            }
                            BoxKind::Fits => {
                                self.break_same_line(*nspaces);
                            }
                            BoxKind::V => {
                                self.break_new_line(*offset, width);
                            }
                            BoxKind::H => {
                                self.break_same_line(*nspaces);
                            }
                        }
                    }
                }
            }
        }
    }

    // ========================================================================
    // Queue and scan stack management
    // ========================================================================

    /// Enqueue a token (matching OCaml's pp_enqueue)
    fn pp_enqueue(&mut self, elem: QueueElem) {
        self.right_total += elem.length;
        self.queue.push_back(elem);
    }

    /// Clear the queue (matching OCaml's pp_clear_queue)
    fn pp_clear_queue(&mut self) {
        self.left_total = 1;
        self.right_total = 1;
        self.queue.clear();
        self.queue_offset = 0;
    }

    /// Initialize scan stack with sentinel
    fn initialize_scan_stack(&mut self) {
        self.scan_stack.clear();
        let queue_elem = QueueElem {
            size: -1, // Size.unknown
            token: PpToken::Text(String::new()),
            length: 0,
        };
        // Push sentinel with left_total = -1
        // The sentinel queue elem doesn't go in the main queue
        // We use a special index that won't match any real queue position
        self.scan_stack.push(ScanElem {
            left_total: -1,
            queue_index: usize::MAX,
        });
        // Store the sentinel element - we don't actually need it in the queue
        // since it's only used as a guard in set_size
        let _ = queue_elem;
    }

    /// Get queue element by absolute index
    fn queue_get(&self, abs_index: usize) -> Option<&QueueElem> {
        if abs_index == usize::MAX {
            return None; // sentinel
        }
        if abs_index >= self.queue_offset {
            self.queue.get(abs_index - self.queue_offset)
        } else {
            None
        }
    }

    /// Get mutable queue element by absolute index
    fn queue_get_mut(&mut self, abs_index: usize) -> Option<&mut QueueElem> {
        if abs_index == usize::MAX {
            return None; // sentinel
        }
        if abs_index >= self.queue_offset {
            self.queue.get_mut(abs_index - self.queue_offset)
        } else {
            None
        }
    }

    /// Current absolute index for the next element to be pushed
    fn queue_abs_end(&self) -> usize {
        self.queue_offset + self.queue.len()
    }

    /// Set size of top scan stack element (matching OCaml's set_size)
    /// ty=true: resolve breaks, ty=false: resolve box-opens
    fn set_size(&mut self, ty: bool) {
        if let Some(scan_elem) = self.scan_stack.last() {
            let left_total = scan_elem.left_total;
            let queue_index = scan_elem.queue_index;

            // Check if scan stack data is obsolete
            if left_total < self.left_total {
                self.initialize_scan_stack();
                return;
            }

            if let Some(queue_elem) = self.queue_get(queue_index) {
                let old_size = queue_elem.size;
                let token_is_break = matches!(queue_elem.token, PpToken::Break { .. });
                let token_is_begin = matches!(queue_elem.token, PpToken::Begin { .. });

                if token_is_break && ty {
                    // Resolve break size
                    let new_size = self.right_total + old_size;
                    if let Some(elem) = self.queue_get_mut(queue_index) {
                        elem.size = new_size;
                    }
                    self.scan_stack.pop();
                } else if token_is_begin && !ty {
                    // Resolve box-open size
                    let new_size = self.right_total + old_size;
                    if let Some(elem) = self.queue_get_mut(queue_index) {
                        elem.size = new_size;
                    }
                    self.scan_stack.pop();
                }
            }
        }
    }

    /// Push a token on the scan stack (matching OCaml's scan_push)
    /// If b is true, set_size is called first (for breaks)
    fn scan_push(&mut self, b: bool, elem: QueueElem) {
        let queue_index = self.queue_abs_end();
        self.pp_enqueue(elem);
        if b {
            self.set_size(true);
        }
        let scan_elem = ScanElem {
            left_total: self.right_total,
            queue_index,
        };
        self.scan_stack.push(scan_elem);
    }

    /// Process tokens from the queue front (matching OCaml's advance_left)
    fn advance_left(&mut self) {
        loop {
            let (size, token, length) = match self.queue.front() {
                None => return,
                Some(elem) => {
                    let size = elem.size;
                    let pending_count = self.right_total - self.left_total;
                    if size >= 0 || pending_count >= self.space_left {
                        (size, elem.token.clone(), elem.length)
                    } else {
                        return;
                    }
                }
            };

            self.queue.pop_front();
            self.queue_offset += 1;

            let actual_size = if size >= 0 { size } else { PP_INFINITY };
            self.format_pp_token(actual_size, &token);
            self.left_total += length;
        }
    }

    /// Enqueue and advance (matching OCaml's enqueue_advance)
    fn enqueue_advance(&mut self, elem: QueueElem) {
        self.pp_enqueue(elem);
        self.advance_left();
    }

    /// Enqueue a string (matching OCaml's enqueue_string_as)
    fn enqueue_string_as(&mut self, size: i32, s: String) {
        self.enqueue_advance(QueueElem {
            size,
            token: PpToken::Text(s),
            length: size,
        });
    }

    /// Initialize/reset the formatter (matching OCaml's pp_rinit)
    fn pp_rinit(&mut self) {
        self.pp_clear_queue();
        self.initialize_scan_stack();
        self.format_stack.clear();
        self.current_indent = 0;
        self.curr_depth = 0;
        self.space_left = self.margin;
        self.pp_open_sys_box();
    }

    /// Open the system box (matching OCaml's pp_open_sys_box)
    fn pp_open_sys_box(&mut self) {
        self.pp_open_box_gen(0, BoxKind::HOV);
    }

    // ========================================================================
    // Public API - Box management
    // ========================================================================

    /// Open a box (matching OCaml's pp_open_box_gen)
    fn pp_open_box_gen(&mut self, indent: i32, kind: BoxKind) {
        self.curr_depth += 1;
        let size = -self.right_total;
        let elem = QueueElem {
            size,
            token: PpToken::Begin { indent, kind },
            length: 0,
        };
        self.scan_push(false, elem);
    }

    /// Open a box with the given kind and indent offset
    pub fn open_box(&mut self, kind: BoxKind, indent: i32) {
        self.pp_open_box_gen(indent, kind);
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

    /// Close the current box (matching OCaml's pp_close_box)
    pub fn close_box(&mut self) {
        if self.curr_depth > 1 {
            self.enqueue_advance(QueueElem {
                size: 0,
                token: PpToken::End,
                length: 0,
            });
            self.set_size(true);
            self.set_size(false);
            self.curr_depth -= 1;
        }
    }

    // ========================================================================
    // Public API - Content
    // ========================================================================

    /// Print a string
    pub fn string(&mut self, s: &str) {
        let len = s.chars().count() as i32;
        self.enqueue_string_as(len, s.to_string());
    }

    /// Print a string slice (alias for string)
    pub fn str(&mut self, s: &str) {
        self.string(s);
    }

    /// Print formatted content using format_args!
    pub fn print(&mut self, args: std::fmt::Arguments<'_>) {
        self.string(&args.to_string());
    }

    /// Break hint (matching OCaml's pp_print_break → pp_print_custom_break)
    pub fn break_(&mut self, nspaces: usize, offset: i32) {
        let nspaces = nspaces as i32;
        let size = -self.right_total;
        let elem = QueueElem {
            size,
            token: PpToken::Break { nspaces, offset },
            length: nspaces,
        };
        self.scan_push(true, elem);
    }

    /// Space that may become a newline (equivalent to @; in OCaml)
    pub fn space(&mut self) {
        self.break_(1, 0);
    }

    /// Cut - zero-width break point (equivalent to @, in OCaml)
    pub fn cut(&mut self) {
        self.break_(0, 0);
    }

    /// Force a newline (matching OCaml's pp_force_newline)
    pub fn newline(&mut self) {
        self.enqueue_advance(QueueElem {
            size: 0,
            token: PpToken::Newline,
            length: 0,
        });
    }

    /// Flush all pending output (matching OCaml's pp_flush_queue)
    pub fn flush(&mut self) {
        // Close all boxes except the system box
        while self.curr_depth > 1 {
            self.close_box();
        }
        // Set right_total to infinity to force all remaining tokens to be processed
        self.right_total = PP_INFINITY;
        self.advance_left();
        // Re-initialize
        self.pp_rinit();
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
        assert!(newlines == 0 || newlines == 5, "HV box should be all-or-nothing, got: {:?}", result);
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
