use regex::Regex;
use std::path::PathBuf;

/// A structured diagnostic produced by bsc (the ReScript compiler).
///
/// Positions are **1-based** as emitted by bsc. Consumers (e.g. the LSP layer)
/// are responsible for converting to their own coordinate system.
#[derive(Debug, Clone, PartialEq)]
pub struct BscDiagnostic {
    pub file: PathBuf,
    pub range: SourceRange,
    pub severity: Severity,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceRange {
    pub start: SourcePosition,
    pub end: SourcePosition,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourcePosition {
    /// 1-based line number
    pub line: u32,
    /// 1-based column
    pub character: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Severity {
    Error,
    Warning,
}

/// Strip ANSI escape sequences from a string.
pub fn strip_ansi(s: &str) -> String {
    let re = Regex::new(r"\x1b\[[0-9;]*m").expect("valid regex");
    re.replace_all(s, "").to_string()
}

/// Parse file and range from a line like:
///   `path/to/file.res:10:20-30:11`
///
/// Supported formats:
/// - `file:line:col`
/// - `file:line:col-endcol`
/// - `file:line:col-endline:endcol`
fn parse_file_and_range(line: &str) -> Option<(PathBuf, SourceRange)> {
    let re = Regex::new(r"^(.+):(\d+):(\d+)(?:-(\d+)(?::(\d+))?)?$").expect("valid regex");
    let caps = re.captures(line.trim())?;

    let file = PathBuf::from(caps.get(1)?.as_str());
    let start_line: u32 = caps.get(2)?.as_str().parse().ok()?;
    let start_col: u32 = caps.get(3)?.as_str().parse().ok()?;

    let (end_line, end_col) = match (caps.get(4), caps.get(5)) {
        // file:startLine:startCol-endLine:endCol
        (Some(end_line_match), Some(end_col_match)) => {
            let end_line: u32 = end_line_match.as_str().parse().ok()?;
            let end_col: u32 = end_col_match.as_str().parse().ok()?;
            (end_line, end_col)
        }
        // file:startLine:startCol-endCol (same line)
        (Some(end_col_match), None) => {
            let end_col: u32 = end_col_match.as_str().parse().ok()?;
            (start_line, end_col)
        }
        // file:startLine:startCol (point range)
        _ => (start_line, start_col),
    };

    Some((
        file,
        SourceRange {
            start: SourcePosition {
                line: start_line,
                character: start_col,
            },
            end: SourcePosition {
                line: end_line,
                character: end_col,
            },
        },
    ))
}

/// Returns true if the line is a gutter line from bsc's code display.
/// These look like: `  1 │ let main = ` or `  . │`
fn is_gutter_line(line: &str) -> bool {
    // Match lines with a gutter separator (│ or ┆)
    let re = Regex::new(r"^\s+(\d+| +|\.)\s*(│|┆)").expect("valid regex");
    re.is_match(line)
}

/// Parse raw bsc stderr output into structured diagnostics.
///
/// Handles three types of diagnostic headers:
/// - `  We've found a bug for you!` (error)
/// - `  Syntax error!` (error)
/// - `  Warning number N` (warning)
///
/// Each header is followed by a file+range line and then indented message lines.
/// The output may contain ANSI escape sequences which are stripped before parsing.
pub fn parse_compiler_output(stderr: &str) -> Vec<BscDiagnostic> {
    let cleaned = strip_ansi(stderr);
    let lines: Vec<&str> = cleaned.lines().collect();
    let mut diagnostics = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let trimmed = lines[i].trim();

        let severity = if trimmed == "We've found a bug for you!" || trimmed == "Syntax error!" {
            Some(Severity::Error)
        } else if trimmed.starts_with("Warning number ") {
            Some(Severity::Warning)
        } else {
            None
        };

        if let Some(severity) = severity {
            i += 1;

            // Next non-empty line should be the file+range
            while i < lines.len() && lines[i].trim().is_empty() {
                i += 1;
            }

            if i >= lines.len() {
                break;
            }

            if let Some((file, range)) = parse_file_and_range(lines[i]) {
                i += 1;

                // Skip empty lines after file+range
                while i < lines.len() && lines[i].trim().is_empty() {
                    i += 1;
                }

                // Skip gutter lines (code display)
                while i < lines.len() && is_gutter_line(lines[i]) {
                    i += 1;
                }

                // Skip empty lines after gutter
                while i < lines.len() && lines[i].trim().is_empty() {
                    i += 1;
                }

                // Collect message lines (indented with at least 2 spaces, non-empty)
                let mut message_lines = Vec::new();
                while i < lines.len() {
                    let line = lines[i];
                    if line.trim().is_empty() {
                        // Check if next non-empty line is still part of the message
                        // (indented continuation) or a new diagnostic header
                        let mut j = i + 1;
                        while j < lines.len() && lines[j].trim().is_empty() {
                            j += 1;
                        }
                        if j < lines.len() {
                            let next_trimmed = lines[j].trim();
                            if next_trimmed == "We've found a bug for you!"
                                || next_trimmed == "Syntax error!"
                                || next_trimmed.starts_with("Warning number ")
                            {
                                break;
                            }
                            // If the next content line starts with spaces and isn't
                            // a gutter line, it's a continuation of the message
                            if lines[j].starts_with("  ") && !is_gutter_line(lines[j]) {
                                message_lines.push("");
                                i += 1;
                                continue;
                            }
                        }
                        break;
                    }
                    if is_gutter_line(line) {
                        i += 1;
                        continue;
                    }
                    // Message lines are indented with 2 spaces
                    if line.starts_with("  ") {
                        message_lines.push(line.trim());
                    } else {
                        break;
                    }
                    i += 1;
                }

                // Remove trailing empty lines from message
                while message_lines.last() == Some(&"") {
                    message_lines.pop();
                }

                let message = message_lines.join("\n");
                if !message.is_empty() {
                    diagnostics.push(BscDiagnostic {
                        file,
                        range,
                        severity,
                        message,
                    });
                }
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_syntax_error() {
        let stderr = r#"
  Syntax error!
  /path/to/file.res:1:11-2:0

  1 │ let main =
  2 │

  This let-binding misses an expression
"#;

        let diagnostics = parse_compiler_output(stderr);
        assert_eq!(diagnostics.len(), 1);
        let d = &diagnostics[0];
        assert_eq!(d.file, PathBuf::from("/path/to/file.res"));
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.range.start.line, 1);
        assert_eq!(d.range.start.character, 11);
        assert_eq!(d.range.end.line, 2);
        assert_eq!(d.range.end.character, 0);
        assert_eq!(d.message, "This let-binding misses an expression");
    }

    #[test]
    fn test_parse_type_error() {
        let stderr = r#"
  We've found a bug for you!
  /path/to/file.res:1:14-20

  1 │ let x: int = "hello"
  2 │

  This has type: string
  But it's expected to have type: int

  You can convert string to int with Int.fromString.
"#;

        let diagnostics = parse_compiler_output(stderr);
        assert_eq!(diagnostics.len(), 1);
        let d = &diagnostics[0];
        assert_eq!(d.file, PathBuf::from("/path/to/file.res"));
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.range.start.line, 1);
        assert_eq!(d.range.start.character, 14);
        assert_eq!(d.range.end.line, 1);
        assert_eq!(d.range.end.character, 20);
        assert!(d.message.contains("This has type: string"));
        assert!(d.message.contains("But it's expected to have type: int"));
        assert!(d.message.contains("Int.fromString"));
    }

    #[test]
    fn test_parse_warning() {
        let stderr = r#"
  Warning number 26
  /path/to/file.res:2:7

  1 │ let x = {
  2 │   let f = 12
  3 │   13
  4 │ }

  unused variable f.
"#;

        let diagnostics = parse_compiler_output(stderr);
        assert_eq!(diagnostics.len(), 1);
        let d = &diagnostics[0];
        assert_eq!(d.file, PathBuf::from("/path/to/file.res"));
        assert_eq!(d.severity, Severity::Warning);
        assert_eq!(d.range.start.line, 2);
        assert_eq!(d.range.start.character, 7);
        assert_eq!(d.message, "unused variable f.");
    }

    #[test]
    fn test_parse_with_ansi_codes() {
        // Simulate bsc output with ANSI escape codes
        let stderr = "\n  \x1b[1;31mSyntax error!\x1b[0m\n  \x1b[36m/path/to/file.res\x1b[0m:\x1b[2m1:11-2:0\x1b[0m\n\n  \x1b[1;31m1\x1b[0m \x1b[2m\u{2502}\x1b[0m let main =\x1b[1;31m \x1b[0m\n  \x1b[1;31m2\x1b[0m \x1b[2m\u{2502}\x1b[0m \n\n  This let-binding misses an expression\n";

        let diagnostics = parse_compiler_output(stderr);
        assert_eq!(diagnostics.len(), 1);
        let d = &diagnostics[0];
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.message, "This let-binding misses an expression");
    }

    #[test]
    fn test_parse_multiple_diagnostics() {
        let stderr = r#"
  Syntax error!
  /path/to/a.res:1:11-2:0

  1 │ let main =
  2 │

  This let-binding misses an expression

  We've found a bug for you!
  /path/to/b.res:3:5-10

  3 │ let x = y

  The value y can't be found
"#;

        let diagnostics = parse_compiler_output(stderr);
        assert_eq!(diagnostics.len(), 2);
        assert_eq!(diagnostics[0].file, PathBuf::from("/path/to/a.res"));
        assert_eq!(diagnostics[0].severity, Severity::Error);
        assert_eq!(diagnostics[1].file, PathBuf::from("/path/to/b.res"));
        assert_eq!(diagnostics[1].severity, Severity::Error);
    }

    #[test]
    fn test_parse_empty_input() {
        let diagnostics = parse_compiler_output("");
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_parse_file_range_variants() {
        // file:line:col
        let (_, range) = parse_file_and_range("  /path/file.res:10:20").unwrap();
        assert_eq!(range.start.line, 10);
        assert_eq!(range.start.character, 20);
        assert_eq!(range.end.line, 10);
        assert_eq!(range.end.character, 20);

        // file:line:col-endcol
        let (_, range) = parse_file_and_range("  /path/file.res:10:20-30").unwrap();
        assert_eq!(range.start.line, 10);
        assert_eq!(range.start.character, 20);
        assert_eq!(range.end.line, 10);
        assert_eq!(range.end.character, 30);

        // file:startLine:startCol-endLine:endCol
        let (_, range) = parse_file_and_range("  /path/file.res:10:20-30:15").unwrap();
        assert_eq!(range.start.line, 10);
        assert_eq!(range.start.character, 20);
        assert_eq!(range.end.line, 30);
        assert_eq!(range.end.character, 15);
    }

    #[test]
    fn test_warning_with_multiline_message() {
        let stderr = r#"
  Warning number 3
  /path/to/file.res:1:9-21

  1 │ let _ = Js.Array2.map([1, 2], v => v + 1)
  2 │ 

  deprecated: Js.Array2.map
  Use `Array.map` instead.

  This can be automatically migrated by the ReScript migration tool. Run `rescript-tools migrate-all <project-root>` to run all automatic migrations available in your project, or `rescript-tools migrate <file>` to migrate a single file.
"#;

        let diagnostics = parse_compiler_output(stderr);
        assert_eq!(diagnostics.len(), 1);
        let d = &diagnostics[0];
        assert_eq!(d.severity, Severity::Warning);
        assert!(d.message.contains("deprecated: Js.Array2.map"));
        assert!(d.message.contains("Use `Array.map` instead."));
        assert!(d.message.contains("rescript-tools migrate"));
    }

    #[test]
    fn test_parse_truncated_output_from_crash() {
        // When bsc crashes mid-output (e.g. "index out of bounds" when
        // -bs-read-stdin source doesn't match disk file), the diagnostic
        // header and location are printed but the message body is replaced
        // by a fatal error. The parser should gracefully return 0 diagnostics.
        let stderr = r#"
  We've found a bug for you!
  src/Log.res:52:15-33
Fatal error: exception Invalid_argument("index out of bounds")
"#;

        let diagnostics = parse_compiler_output(stderr);
        assert_eq!(diagnostics.len(), 0);
    }

    #[test]
    fn test_parse_type_error_with_conversion_hint() {
        // Full output from bsc -bs-read-stdin for a type mismatch, including
        // the code frame and conversion hint. This is the output produced
        // after fixing the crash (Location.stdin_source provides the correct
        // source for code frame display).
        let stderr = r#"
  We've found a bug for you!
  src/Log.res:52:15-35

  50 │ }
  51 │ 
  52 │ let x : int = "eeeebtteeeebleeee  "
  53 │ 

  This has type: string
  But it's expected to have type: int
  
  You can convert string to int with Int.fromString.
"#;

        let diagnostics = parse_compiler_output(stderr);
        assert_eq!(diagnostics.len(), 1);
        let d = &diagnostics[0];
        assert_eq!(d.file, PathBuf::from("src/Log.res"));
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.range.start.line, 52);
        assert_eq!(d.range.start.character, 15);
        assert_eq!(d.range.end.line, 52);
        assert_eq!(d.range.end.character, 35);
        assert!(d.message.contains("This has type: string"));
        assert!(d.message.contains("But it's expected to have type: int"));
        assert!(d.message.contains("Int.fromString"));
    }
}
