//! Code frame formatting for error messages.
//!
//! This module provides functions to format source code snippets with
//! line numbers and highlighting for error reporting, matching the
//! OCaml implementation in `compiler/ml/code_frame.ml`.

use crate::location::Position;

/// Count the number of digits in a number.
fn digits_count(n: i32) -> usize {
    if n == 0 {
        return 1;
    }
    let mut count = 0;
    let mut n = n.abs();
    while n > 0 {
        count += 1;
        n /= 10;
    }
    count
}

/// Seek backwards to find the start position for showing 2 lines before the error.
/// Returns (char_offset, line_number) of where to start.
fn seek_2_lines_before(src: &str, pos: &Position) -> (usize, i32) {
    let original_line = pos.line;
    let mut current_line = 1i32;
    let mut current_char = 0usize;

    let bytes = src.as_bytes();
    while current_char < bytes.len() {
        if current_line + 2 >= original_line {
            return (current_char, current_line);
        }
        if bytes[current_char] == b'\n' {
            current_line += 1;
        }
        current_char += 1;
    }
    (current_char, current_line)
}

/// Seek forward to find the end position for showing 2 lines after the error.
/// Returns (char_offset, line_number) of where to end.
fn seek_2_lines_after(src: &str, pos: &Position) -> (usize, i32) {
    let original_line = pos.line;
    let mut current_line = original_line;
    let mut current_char = pos.cnum as usize;

    let bytes = src.as_bytes();
    while current_char < bytes.len() {
        if bytes[current_char] == b'\n' {
            if current_line == original_line + 2 {
                return (current_char, current_line);
            }
            current_line += 1;
        }
        current_char += 1;
    }
    (current_char, current_line)
}

/// Count leading spaces in a string.
fn leading_space_count(s: &str) -> usize {
    s.chars().take_while(|&c| c == ' ').count()
}

/// Gutter content - either a line number or elided marker.
#[derive(Clone)]
enum Gutter {
    Number(i32),
    Elided,
}

/// A highlighted portion of a line.
struct HighlightedString {
    s: String,
    start: usize,
    end: usize,
}

/// A line with gutter and content.
struct Line {
    gutter: Gutter,
    content: Vec<HighlightedString>,
}

/// Print a code frame with source context around an error.
///
/// This formats source code with:
/// - 2 lines of context before and after the error
/// - Line numbers in a gutter
/// - Highlighting of the error region (when colors enabled)
/// - Elision of middle lines for large error spans
pub fn print(src: &str, start_pos: &Position, end_pos: &Position) -> String {
    let indent = 2;
    let highlight_line_start = start_pos.line;
    let highlight_line_end = end_pos.line;

    let (start_offset, first_shown_line) = seek_2_lines_before(src, start_pos);
    let (end_offset, last_shown_line) = seek_2_lines_after(src, end_pos);

    let more_than_5_highlighted_lines = highlight_line_end - highlight_line_start + 1 > 5;
    let max_line_digits = digits_count(last_shown_line);

    // 3 for separator + the 2 spaces around it
    let _line_width = 78 - max_line_digits - indent - 3;

    // Extract the relevant portion of source
    let lines: Vec<(Gutter, &str)> = if start_offset < end_offset && end_offset <= src.len() {
        let snippet = &src[start_offset..end_offset];
        snippet
            .split('\n')
            .enumerate()
            .filter_map(|(i, line)| {
                let line_number = first_shown_line + i as i32;
                if more_than_5_highlighted_lines {
                    if line_number == highlight_line_start + 2 {
                        Some((Gutter::Elided, line))
                    } else if line_number > highlight_line_start + 2
                        && line_number < highlight_line_end - 1
                    {
                        None
                    } else {
                        Some((Gutter::Number(line_number), line))
                    }
                } else {
                    Some((Gutter::Number(line_number), line))
                }
            })
            .collect()
    } else {
        vec![]
    };

    // Calculate leading space to strip (minimum non-empty line indentation)
    let leading_space_to_cut = lines
        .iter()
        .filter_map(|(_, line)| {
            let spaces = leading_space_count(line);
            if line.len() == spaces {
                None // Line is all spaces, don't count
            } else {
                Some(spaces)
            }
        })
        .min()
        .unwrap_or(0);

    let separator = if leading_space_to_cut == 0 { "│" } else { "┆" };

    // Process lines with highlighting info
    let processed_lines: Vec<Line> = lines
        .iter()
        .map(|(gutter, line)| {
            let stripped = if line.len() > leading_space_to_cut {
                &line[leading_space_to_cut..]
            } else {
                ""
            };

            let content = match gutter {
                Gutter::Elided => vec![HighlightedString {
                    s: stripped.to_string(),
                    start: 0,
                    end: 0,
                }],
                Gutter::Number(line_number) => {
                    let highlight_start_col =
                        (start_pos.cnum - start_pos.bol) as usize;
                    let highlight_end_col = (end_pos.cnum - end_pos.bol) as usize;

                    let start = if *line_number == highlight_line_start {
                        highlight_start_col.saturating_sub(leading_space_to_cut)
                    } else {
                        0
                    };

                    let end = if *line_number < highlight_line_start {
                        0
                    } else if *line_number == highlight_line_start
                        && *line_number == highlight_line_end
                    {
                        highlight_end_col.saturating_sub(leading_space_to_cut)
                    } else if *line_number == highlight_line_start {
                        stripped.len()
                    } else if *line_number > highlight_line_start
                        && *line_number < highlight_line_end
                    {
                        stripped.len()
                    } else if *line_number == highlight_line_end {
                        highlight_end_col.saturating_sub(leading_space_to_cut)
                    } else {
                        0
                    };

                    vec![HighlightedString {
                        s: stripped.to_string(),
                        start,
                        end,
                    }]
                }
            };

            Line {
                gutter: gutter.clone(),
                content,
            }
        })
        .collect();

    // Build output string
    let mut buf = String::with_capacity(1024);

    for line in &processed_lines {
        match &line.gutter {
            Gutter::Elided => {
                // Draw elided gutter
                for _ in 0..(max_line_digits + indent - 1) {
                    buf.push(' ');
                }
                buf.push_str(".");
                buf.push(' ');
                buf.push_str(separator);
                buf.push_str(" ...\n");
            }
            Gutter::Number(line_number) => {
                for hs in &line.content {
                    // Draw gutter with line number
                    let num_str = line_number.to_string();
                    for _ in 0..(max_line_digits + indent - num_str.len()) {
                        buf.push(' ');
                    }
                    buf.push_str(&num_str);
                    buf.push(' ');
                    buf.push_str(separator);
                    buf.push(' ');

                    // Draw content (no colors for now, just plain text)
                    buf.push_str(&hs.s);
                    buf.push('\n');
                }
            }
        }
    }

    buf
}

/// Format a location as "filename:line:col" or "filename:line:col-endcol"
pub fn format_location(filename: &str, start_pos: &Position, end_pos: &Position) -> String {
    let start_col = start_pos.cnum - start_pos.bol + 1;
    let end_col = end_pos.cnum - end_pos.bol;

    if start_pos.line == end_pos.line && end_col > start_col {
        format!("{}:{}:{}-{}", filename, start_pos.line, start_col, end_col)
    } else if start_pos.line == end_pos.line {
        format!("{}:{}:{}", filename, start_pos.line, start_col)
    } else {
        format!(
            "{}:{}:{}-{}:{}",
            filename, start_pos.line, start_col, end_pos.line, end_col
        )
    }
}

/// Format a complete error report with header, location, code frame, and message.
pub fn format_error(
    filename: &str,
    src: &str,
    start_pos: &Position,
    end_pos: &Position,
    message: &str,
) -> String {
    let mut result = String::new();

    // Header
    result.push_str("\n  Syntax error!\n");

    // Location
    result.push_str("  ");
    result.push_str(&format_location(filename, start_pos, end_pos));
    result.push_str("\n\n");

    // Code frame
    result.push_str(&print(src, start_pos, end_pos));
    // Extra blank line after code frame
    result.push_str("\n");

    // Message
    result.push_str("  ");
    result.push_str(message);
    // Extra blank line after message
    result.push_str("\n\n");

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digits_count() {
        assert_eq!(digits_count(0), 1);
        assert_eq!(digits_count(1), 1);
        assert_eq!(digits_count(9), 1);
        assert_eq!(digits_count(10), 2);
        assert_eq!(digits_count(99), 2);
        assert_eq!(digits_count(100), 3);
        assert_eq!(digits_count(1000), 4);
    }

    #[test]
    fn test_leading_space_count() {
        assert_eq!(leading_space_count(""), 0);
        assert_eq!(leading_space_count("hello"), 0);
        assert_eq!(leading_space_count("  hello"), 2);
        assert_eq!(leading_space_count("    "), 4);
    }

    #[test]
    fn test_format_location() {
        let start = Position {
            line: 5,
            bol: 100,
            cnum: 110,
            ..Default::default()
        };
        let end = Position {
            line: 5,
            bol: 100,
            cnum: 115,
            ..Default::default()
        };
        assert_eq!(format_location("test.res", &start, &end), "test.res:5:11-15");
    }

    #[test]
    fn test_simple_code_frame() {
        let src = "let x = 1\nlet y = 2\nlet z = 3\n";
        let start = Position {
            line: 2,
            bol: 10,
            cnum: 14,
            ..Default::default()
        };
        let end = Position {
            line: 2,
            bol: 10,
            cnum: 15,
            ..Default::default()
        };
        let frame = print(src, &start, &end);
        assert!(frame.contains("1 │ let x = 1"));
        assert!(frame.contains("2 │ let y = 2"));
        assert!(frame.contains("3 │ let z = 3"));
    }
}
