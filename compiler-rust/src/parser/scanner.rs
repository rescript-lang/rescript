//! Lexical scanner for ReScript source code.
//!
//! The scanner converts source text into a stream of tokens, handling:
//! - Identifiers and keywords
//! - Numeric literals (integers, floats, with various bases)
//! - String literals with escape sequences
//! - Comments (single-line, multi-line, doc comments)
//! - Template literals
//! - Regular expressions
//! - All operators and punctuation

use crate::location::{Position, PositionId};

use super::comment::{Comment, CommentStyle};
use super::diagnostics::{DiagnosticCategory, ParserDiagnostic};
use super::token::Token;
use super::utf8;

/// Special character value indicating end of file.
/// We use a value outside the valid char range.
const EOF_CHAR: char = '\u{FFFF}';

/// Scanner mode for context-sensitive tokenization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScannerMode {
    /// Diamond mode for type parameter parsing (affects `>` tokenization).
    Diamond,
}

/// A snapshot of scanner state for lookahead/restore.
#[derive(Debug, Clone)]
pub struct ScannerSnapshot {
    ch: char,
    offset: usize,
    line_offset: usize,
    lnum: i32,
    mode: Vec<ScannerMode>,
    prev_token: Option<Token>,
    diagnostics_len: usize,
    position_id_counter: u32,
}

/// The lexical scanner state.
#[derive(Debug)]
pub struct Scanner<'src> {
    /// Source filename.
    pub filename: String,
    /// Source code being scanned.
    src: &'src str,
    /// Source as bytes for efficient access (UTF-8 encoding of the string).
    src_bytes: &'src [u8],
    /// Character count (cached for efficiency).
    /// For Latin-1 source, this equals the original byte count.
    src_char_count: usize,
    /// Current character.
    ch: char,
    /// Current character offset in source (= byte offset in original Latin-1 file).
    offset: usize,
    /// Byte offset of current line start.
    line_offset: usize,
    /// Current line number (1-indexed).
    lnum: i32,
    /// Stack of scanner modes.
    mode: Vec<ScannerMode>,
    /// Collected diagnostics.
    diagnostics: Vec<ParserDiagnostic>,
    /// Previous non-comment token (for context-sensitive lexing like regex literals).
    prev_token: Option<Token>,
    /// Counter for generating unique PositionIds.
    /// Each scanner instance has its own counter to avoid global state.
    position_id_counter: u32,
}

/// Result of scanning a token.
#[derive(Debug, Clone)]
pub struct ScanResult {
    /// Start position of the token.
    pub start_pos: Position,
    /// End position of the token.
    pub end_pos: Position,
    /// The scanned token.
    pub token: Token,
}

impl<'src> Scanner<'src> {
    /// Create a new scanner for the given source.
    pub fn new(filename: impl Into<String>, src: &'src str) -> Self {
        let filename = filename.into();
        let src_bytes = src.as_bytes();
        let src_char_count = src.chars().count();
        let ch = if src.is_empty() {
            EOF_CHAR
        } else {
            src.chars().next().unwrap_or(EOF_CHAR)
        };

        Self {
            filename,
            src,
            src_bytes,
            src_char_count,
            ch,
            offset: 0,
            line_offset: 0,
            lnum: 1,
            mode: Vec::new(),
            diagnostics: Vec::new(),
            prev_token: None,
            // Start at 1 because 0 is reserved for default/uninitialized positions
            position_id_counter: 1,
        }
    }

    /// Get collected diagnostics.
    pub fn diagnostics(&self) -> &[ParserDiagnostic] {
        &self.diagnostics
    }

    /// Take collected diagnostics, leaving an empty vec.
    pub fn take_diagnostics(&mut self) -> Vec<ParserDiagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    /// Push diamond mode for type parameter parsing.
    pub fn set_diamond_mode(&mut self) {
        self.mode.push(ScannerMode::Diamond);
    }

    /// Pop diamond mode.
    pub fn pop_diamond_mode(&mut self) {
        if let Some(ScannerMode::Diamond) = self.mode.last() {
            self.mode.pop();
        }
    }

    /// Create a snapshot of the scanner state for lookahead.
    pub fn snapshot(&self) -> ScannerSnapshot {
        ScannerSnapshot {
            ch: self.ch,
            offset: self.offset,
            line_offset: self.line_offset,
            lnum: self.lnum,
            mode: self.mode.clone(),
            prev_token: self.prev_token.clone(),
            diagnostics_len: self.diagnostics.len(),
            position_id_counter: self.position_id_counter,
        }
    }

    /// Restore scanner state from a snapshot.
    pub fn restore(&mut self, snapshot: ScannerSnapshot) {
        self.ch = snapshot.ch;
        self.offset = snapshot.offset;
        self.line_offset = snapshot.line_offset;
        self.lnum = snapshot.lnum;
        self.mode = snapshot.mode;
        self.prev_token = snapshot.prev_token;
        self.diagnostics.truncate(snapshot.diagnostics_len);
        self.position_id_counter = snapshot.position_id_counter;
    }

    /// Get a reference to the source string.
    pub fn src(&self) -> &str {
        self.src
    }

    /// Check if in diamond mode.
    pub fn in_diamond_mode(&self) -> bool {
        matches!(self.mode.last(), Some(ScannerMode::Diamond))
    }

    /// Get the current position with a unique PositionId.
    ///
    /// Each call creates a NEW position with a unique ID.
    /// This mimics OCaml where each call to the scanner's position function
    /// allocates a new position record.
    ///
    /// For byte-accurate positions (OCaml parity), we use character offset directly.
    /// Since the source is Latin-1 encoded (each original byte → one char), the
    /// character offset equals the original byte offset.
    pub fn position(&mut self) -> Position {
        let id = PositionId::from_raw(self.position_id_counter);
        self.position_id_counter += 1;
        Position::new_with_id(
            &self.filename,
            self.lnum,
            self.line_offset as i32,
            self.offset as i32, // Use character offset (= byte offset in Latin-1)
            id,
        )
    }

    /// Report an error at the given positions.
    fn error(&mut self, start_pos: Position, end_pos: Position, category: DiagnosticCategory) {
        self.diagnostics
            .push(ParserDiagnostic::new(start_pos, end_pos, category));
    }

    /// Advance to the next character.
    ///
    /// For Latin-1 source (each original byte → one char), the character offset
    /// equals the byte offset. We use chars().nth() for proper character indexing.
    fn next(&mut self) {
        if self.offset >= self.src_char_count {
            self.ch = EOF_CHAR;
            return;
        }

        // Handle newlines - update line tracking
        if self.ch == '\n' {
            self.line_offset = self.offset + 1;
            self.lnum += 1;
        }

        // Move to next character
        self.offset += 1;
        self.ch = self.src.chars().nth(self.offset).unwrap_or(EOF_CHAR);
    }

    /// Advance by 2 characters.
    fn next2(&mut self) {
        self.next();
        self.next();
    }

    /// Advance by 3 characters.
    fn next3(&mut self) {
        self.next();
        self.next();
        self.next();
    }

    /// Peek at the next character without advancing.
    fn peek(&self) -> char {
        self.src.chars().nth(self.offset + 1).unwrap_or(EOF_CHAR)
    }

    /// Peek at the character 2 positions ahead.
    fn peek2(&self) -> char {
        self.src.chars().nth(self.offset + 2).unwrap_or(EOF_CHAR)
    }

    /// Peek at the character 3 positions ahead.
    fn peek3(&self) -> char {
        self.src.chars().nth(self.offset + 3).unwrap_or(EOF_CHAR)
    }

    /// Check if a character is whitespace.
    fn is_whitespace(ch: char) -> bool {
        matches!(ch, ' ' | '\t' | '\n' | '\r')
    }

    /// Skip whitespace characters.
    fn skip_whitespace(&mut self) {
        while Self::is_whitespace(self.ch) {
            self.next();
        }
    }

    /// Get the digit value of a character (0-15 for hex, 16+ for invalid).
    fn digit_value(ch: char) -> u32 {
        match ch {
            '0'..='9' => ch as u32 - '0' as u32,
            'a'..='f' => ch as u32 - 'a' as u32 + 10,
            'A'..='F' => ch as u32 - 'A' as u32 + 10,
            _ => 16,
        }
    }

    /// Extract a substring from the source.
    ///
    /// Note: start and end are character offsets. We convert them to byte offsets
    /// for slicing, since the source string is UTF-8 internally (Latin-1 chars >127
    /// become 2-byte UTF-8 sequences).
    fn substring(&self, start: usize, end: usize) -> &'src str {
        // Get byte indices for the character range
        let byte_start = self.src.char_indices().nth(start).map(|(i, _)| i).unwrap_or(self.src.len());
        let byte_end = self.src.char_indices().nth(end).map(|(i, _)| i).unwrap_or(self.src.len());
        &self.src[byte_start..byte_end]
    }

    /// Scan an identifier or keyword.
    fn scan_identifier(&mut self) -> Token {
        let start_off = self.offset;

        // Skip valid identifier characters
        while matches!(self.ch, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '\'') {
            self.next();
        }

        let ident = self.substring(start_off, self.offset);

        // Check for special cases: list{, dict{, let?
        match (self.ch, ident) {
            ('{', "list") => {
                self.next();
                Token::List
            }
            ('{', "dict") => {
                self.next();
                Token::Dict
            }
            ('?', "let") => {
                self.next();
                Token::Let { unwrap: true }
            }
            _ => Token::lookup_keyword(ident),
        }
    }

    /// Scan digits in the given base.
    fn scan_digits(&mut self, base: u32) -> bool {
        let mut found_digits = false;

        if base <= 10 {
            loop {
                match self.ch {
                    '0'..='9' => {
                        self.next();
                        found_digits = true;
                    }
                    '_' => {
                        self.next();
                    }
                    _ => break,
                }
            }
        } else {
            // Hex
            loop {
                match self.ch {
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        self.next();
                        found_digits = true;
                    }
                    '_' => {
                        self.next();
                    }
                    _ => break,
                }
            }
        }

        found_digits
    }

    /// Scan a number literal.
    fn scan_number(&mut self) -> Token {
        let start_off = self.offset;

        // Determine base
        let base = if self.ch == '0' {
            match self.peek() {
                'x' | 'X' => {
                    self.next2();
                    16
                }
                'o' | 'O' => {
                    self.next2();
                    8
                }
                'b' | 'B' => {
                    self.next2();
                    2
                }
                _ => {
                    self.next();
                    8
                }
            }
        } else {
            10
        };

        self.scan_digits(base);

        // Decimal part
        let mut is_float = false;
        if self.ch == '.' {
            self.next();
            self.scan_digits(base);
            is_float = true;
        }

        // Exponent part
        match self.ch {
            'e' | 'E' | 'p' | 'P' => {
                let start_pos = self.position();
                match self.peek() {
                    '+' | '-' => self.next2(),
                    _ => self.next(),
                }
                let end_pos = self.position();
                if !self.scan_digits(base) {
                    self.error(
                        start_pos,
                        end_pos,
                        DiagnosticCategory::message("Expected digits after exponential notation."),
                    );
                }
                is_float = true;
            }
            _ => {}
        }

        let literal = self.substring(start_off, self.offset).to_string();

        // Suffix
        let suffix = match self.ch {
            'g'..='z' | 'G'..='Z' => {
                let s = self.ch;
                self.next();
                Some(s)
            }
            _ => None,
        };

        if is_float {
            Token::Float { f: literal, suffix }
        } else {
            Token::Int { i: literal, suffix }
        }
    }

    /// Scan a string literal.
    fn scan_string(&mut self) -> Token {
        let start_pos = self.position();
        self.next(); // consume opening quote

        // OCaml keeps escape sequences as text, only converting decimal to hex.
        // We use a buffer to accumulate the result, copying text verbatim except
        // for decimal escapes which get converted to hex format.
        let mut result = String::new();
        let mut chunk_start = self.offset;

        loop {
            match self.ch {
                '"' => {
                    // Add remaining content
                    result.push_str(self.substring(chunk_start, self.offset));
                    self.next();
                    break;
                }
                '\\' => {
                    // Add content before the escape (including the backslash)
                    result.push_str(self.substring(chunk_start, self.offset));
                    result.push('\\');
                    self.next(); // consume backslash

                    // Check if this is a decimal escape that needs conversion
                    let escape_start = self.offset;
                    self.scan_string_escape_sequence_text(&start_pos);
                    let escape_end = self.offset;

                    // Check if it was a 3-digit decimal escape
                    let escape_text = self.substring(escape_start, escape_end);
                    if escape_text.len() == 3
                        && escape_text.chars().all(|c| c.is_ascii_digit())
                    {
                        // Convert decimal to hex format
                        if let Ok(value) = escape_text.parse::<u32>() {
                            if value <= 255 {
                                result.push_str(&format!("x{:02x}", value));
                            } else {
                                // Invalid value, keep as-is
                                result.push_str(escape_text);
                            }
                        } else {
                            result.push_str(escape_text);
                        }
                    } else {
                        // Keep other escapes as-is
                        result.push_str(escape_text);
                    }
                    chunk_start = self.offset;
                }
                c if c == EOF_CHAR => {
                    let end_pos = self.position();
                    self.error(start_pos, end_pos, DiagnosticCategory::unclosed_string());
                    result.push_str(self.substring(chunk_start, self.offset));
                    break;
                }
                _ => self.next(),
            }
        }

        Token::String(result)
    }

    /// Scan past a string escape sequence, advancing the scanner position.
    /// This doesn't interpret the escape, just skips past it.
    fn scan_string_escape_sequence_text(&mut self, start_pos: &Position) {
        match self.ch {
            'n' | 't' | 'b' | 'r' | '\\' | ' ' | '\'' | '"' => {
                self.next();
            }
            '0' if !self.peek().is_ascii_digit() => {
                // \0 when not followed by digit
                self.next();
            }
            '0'..='9' => {
                // Decimal escape \NNN (up to 3 digits)
                for _ in 0..3 {
                    if self.ch.is_ascii_digit() {
                        self.next();
                    } else {
                        break;
                    }
                }
            }
            'x' => {
                // Hex escape \xNN
                self.next();
                for _ in 0..2 {
                    if self.ch.is_ascii_hexdigit() {
                        self.next();
                    } else {
                        break;
                    }
                }
            }
            'u' => {
                self.next();
                if self.ch == '{' {
                    // Unicode escape \u{NNNN}
                    self.next();
                    while self.ch.is_ascii_hexdigit() {
                        self.next();
                    }
                    if self.ch == '}' {
                        self.next();
                    }
                } else {
                    // Unicode escape \uNNNN
                    for _ in 0..4 {
                        if self.ch.is_ascii_hexdigit() {
                            self.next();
                        } else {
                            break;
                        }
                    }
                }
            }
            c if c == EOF_CHAR => {
                let end_pos = self.position();
                self.error(
                    start_pos.clone(),
                    end_pos,
                    DiagnosticCategory::message("unclosed escape sequence"),
                );
            }
            _ => {
                // Unknown escape - skip the character
                self.next();
            }
        }
    }

    /// Scan a string escape sequence and return the converted character.
    /// Returns None if the escape sequence is invalid or produces no character.
    fn scan_string_escape_sequence_char(&mut self, start_pos: &Position) -> Option<char> {
        match self.ch {
            'n' => {
                self.next();
                Some('\n')
            }
            't' => {
                self.next();
                Some('\t')
            }
            'b' => {
                self.next();
                Some('\x08') // backspace
            }
            'r' => {
                self.next();
                Some('\r')
            }
            '\\' => {
                self.next();
                Some('\\')
            }
            ' ' => {
                self.next();
                Some(' ')
            }
            '\'' => {
                self.next();
                Some('\'')
            }
            '"' => {
                self.next();
                Some('"')
            }
            '0' if !self.peek().is_ascii_digit() => {
                self.next();
                Some('\0')
            }
            '0'..='9' => {
                // Octal/decimal escape \NNN (up to 3 digits)
                let mut value: u32 = 0;
                for _ in 0..3 {
                    if self.ch.is_ascii_digit() {
                        value = value * 10 + (self.ch as u32 - '0' as u32);
                        self.next();
                    } else {
                        break;
                    }
                }
                // Convert to char (Latin-1 range)
                char::from_u32(value)
            }
            'x' => {
                // Hex escape \xNN
                self.next();
                let mut value: u32 = 0;
                for _ in 0..2 {
                    let digit = Self::digit_value(self.ch);
                    if digit < 16 {
                        value = value * 16 + digit as u32;
                        self.next();
                    } else {
                        break;
                    }
                }
                char::from_u32(value)
            }
            'u' => {
                self.next();
                if self.ch == '{' {
                    // Unicode escape \u{NNNN}
                    self.next();
                    let mut value: u32 = 0;
                    while self.ch.is_ascii_hexdigit() {
                        value = value * 16 + Self::digit_value(self.ch) as u32;
                        self.next();
                    }
                    if self.ch == '}' {
                        self.next();
                    }
                    char::from_u32(value)
                } else {
                    // Unicode escape \uNNNN
                    let mut value: u32 = 0;
                    for _ in 0..4 {
                        let digit = Self::digit_value(self.ch);
                        if digit < 16 {
                            value = value * 16 + digit as u32;
                            self.next();
                        } else {
                            break;
                        }
                    }
                    char::from_u32(value)
                }
            }
            c if c == EOF_CHAR => {
                let end_pos = self.position();
                self.error(
                    start_pos.clone(),
                    end_pos,
                    DiagnosticCategory::message("unclosed escape sequence"),
                );
                None
            }
            c => {
                // Unknown escape - return the char as-is
                self.next();
                Some(c)
            }
        }
    }

    /// Scan a string escape sequence (legacy version, doesn't return char).
    fn scan_string_escape_sequence(&mut self, start_pos: &Position) {
        let _ = self.scan_string_escape_sequence_char(start_pos);
    }

    /// Scan a single-line comment.
    fn scan_single_line_comment(&mut self) -> Token {
        let start_off = self.offset;
        let start_pos = self.position();

        // Skip to end of line
        while !matches!(self.ch, '\n' | '\r') && self.ch != EOF_CHAR {
            self.next();
        }

        let end_pos = self.position();
        let content = self.substring(start_off, self.offset).to_string();

        Token::Comment(Comment {
            txt: content,
            style: CommentStyle::SingleLine,
            loc: crate::location::Location::from_positions(start_pos, end_pos),
            prev_tok_end_pos: Position::default(),
        })
    }

    /// Scan a multi-line comment.
    fn scan_multi_line_comment(&mut self) -> Token {
        let doc_comment = self.peek2() == '*' && self.peek3() != '/';
        let standalone = doc_comment && self.peek3() == '*';

        let content_start_off = self.offset
            + if doc_comment {
                if standalone { 4 } else { 3 }
            } else {
                2
            };

        let start_pos = self.position();
        let mut depth = 0;

        loop {
            match (self.ch, self.peek()) {
                ('/', '*') => {
                    self.next2();
                    depth += 1;
                }
                ('*', '/') => {
                    self.next2();
                    if depth > 1 {
                        depth -= 1;
                    } else {
                        break;
                    }
                }
                (c, _) if c == EOF_CHAR => {
                    let end_pos = self.position();
                    self.error(
                        start_pos.clone(),
                        end_pos,
                        DiagnosticCategory::unclosed_comment(),
                    );
                    break;
                }
                _ => self.next(),
            }
        }

        let content_end_off = self.offset.saturating_sub(2);
        let length = content_end_off.saturating_sub(content_start_off);
        let content = if content_start_off < self.src.len() {
            self.substring(content_start_off, content_start_off + length)
                .to_string()
        } else {
            String::new()
        };

        let end_pos = self.position();
        let loc = crate::location::Location::from_positions(start_pos, end_pos);

        if doc_comment {
            if standalone {
                Token::ModuleComment { loc, content }
            } else {
                Token::DocComment { loc, content }
            }
        } else {
            Token::Comment(Comment {
                txt: content,
                style: CommentStyle::MultiLine,
                loc,
                prev_tok_end_pos: Position::default(),
            })
        }
    }

    /// Scan a template literal token.
    pub fn scan_template_literal_token(&mut self) -> ScanResult {
        let start_off = self.offset;

        // If starting with }, consume it
        if self.ch == '}' {
            self.next();
        }

        let start_pos = self.position();

        loop {
            let last_pos = self.position();
            match self.ch {
                '`' => {
                    self.next();
                    let contents = self.substring(start_off, self.offset - 1).to_string();
                    let end_pos = self.position();
                    return ScanResult {
                        start_pos,
                        end_pos,
                        token: Token::TemplateTail {
                            text: contents,
                            pos: last_pos,
                        },
                    };
                }
                '$' if self.peek() == '{' => {
                    self.next2();
                    let contents = self.substring(start_off, self.offset - 2).to_string();
                    let end_pos = self.position();
                    return ScanResult {
                        start_pos,
                        end_pos,
                        token: Token::TemplatePart {
                            text: contents,
                            pos: last_pos,
                        },
                    };
                }
                '\\' => {
                    // Escape sequences in template literals
                    match self.peek() {
                        '`' | '\\' | '$' | '\n' | '\r' => self.next2(),
                        _ => self.next(),
                    }
                }
                c if c == EOF_CHAR => {
                    let end_pos = self.position();
                    self.error(
                        start_pos.clone(),
                        end_pos.clone(),
                        DiagnosticCategory::unclosed_template(),
                    );
                    let contents = self.substring(start_off, self.offset).to_string();
                    return ScanResult {
                        start_pos,
                        end_pos,
                        token: Token::TemplateTail {
                            text: contents,
                            pos: last_pos,
                        },
                    };
                }
                _ => self.next(),
            }
        }
    }

    /// Scan the next token.
    pub fn scan(&mut self) -> ScanResult {
        self.skip_whitespace();
        let start_pos = self.position();

        let token = match self.ch {
            // Identifiers and keywords
            'A'..='Z' | 'a'..='z' => self.scan_identifier(),

            // Numbers
            '0'..='9' => self.scan_number(),

            // Template literal
            '`' => {
                self.next();
                Token::Backtick
            }

            // Operators and punctuation
            '~' => {
                if self.peek() == '~' && self.peek2() == '~' {
                    self.next3();
                    Token::Bnot
                } else {
                    self.next();
                    Token::Tilde
                }
            }
            '?' => {
                self.next();
                Token::Question
            }
            ';' => {
                self.next();
                Token::Semicolon
            }
            '(' => {
                self.next();
                Token::Lparen
            }
            ')' => {
                self.next();
                Token::Rparen
            }
            '[' => {
                self.next();
                Token::Lbracket
            }
            ']' => {
                self.next();
                Token::Rbracket
            }
            '{' => {
                self.next();
                Token::Lbrace
            }
            '}' => {
                self.next();
                Token::Rbrace
            }
            ',' => {
                self.next();
                Token::Comma
            }
            '"' => self.scan_string(),

            '_' => {
                if matches!(self.peek(), 'A'..='Z' | 'a'..='z' | '0'..='9' | '_') {
                    self.scan_identifier()
                } else {
                    self.next();
                    Token::Underscore
                }
            }

            '#' => {
                if self.peek() == '=' {
                    self.next2();
                    Token::HashEqual
                } else {
                    self.next();
                    Token::Hash
                }
            }

            '*' => match self.peek() {
                '*' => {
                    self.next2();
                    Token::Exponentiation
                }
                '.' => {
                    self.next2();
                    Token::AsteriskDot
                }
                _ => {
                    self.next();
                    Token::Asterisk
                }
            },

            '@' => {
                if self.peek() == '@' {
                    self.next2();
                    Token::AtAt
                } else {
                    self.next();
                    Token::At
                }
            }

            '%' => {
                if self.peek() == '%' {
                    self.next2();
                    Token::PercentPercent
                } else {
                    self.next();
                    Token::Percent
                }
            }

            '|' => match (self.peek(), self.peek2()) {
                ('|', '|') => {
                    self.next3();
                    Token::Bor
                }
                ('|', _) => {
                    self.next2();
                    Token::Lor
                }
                _ => {
                    self.next();
                    Token::Bar
                }
            },

            '&' => match (self.peek(), self.peek2()) {
                ('&', '&') => {
                    self.next3();
                    Token::Band
                }
                ('&', _) => {
                    self.next2();
                    Token::Land
                }
                _ => {
                    self.next();
                    Token::Ampersand
                }
            },

            '^' => {
                if self.peek() == '^' && self.peek2() == '^' {
                    self.next3();
                    Token::Bxor
                } else {
                    self.next();
                    Token::Caret
                }
            }

            ':' => match self.peek() {
                '=' => {
                    self.next2();
                    Token::ColonEqual
                }
                '>' => {
                    self.next2();
                    Token::ColonGreaterThan
                }
                _ => {
                    self.next();
                    Token::Colon
                }
            },

            '/' => match self.peek() {
                '/' => {
                    self.next2();
                    self.scan_single_line_comment()
                }
                '*' => self.scan_multi_line_comment(),
                _ => {
                    if self.can_start_regex_literal() && !self.is_jsx_close_slash() {
                        if let Some(tok) = self.try_scan_regex_literal() {
                            tok
                        } else {
                            // Fall back to division operators
                            if self.peek() == '.' {
                                self.next2();
                                Token::ForwardslashDot
                            } else {
                                self.next();
                                Token::Forwardslash
                            }
                        }
                    } else if self.peek() == '.' {
                        self.next2();
                        Token::ForwardslashDot
                    } else {
                        self.next();
                        Token::Forwardslash
                    }
                }
            },

            '-' => match self.peek() {
                '.' => {
                    self.next2();
                    Token::MinusDot
                }
                '>' => {
                    self.next2();
                    Token::MinusGreater
                }
                _ => {
                    self.next();
                    Token::Minus
                }
            },

            '+' => match self.peek() {
                '.' => {
                    self.next2();
                    Token::PlusDot
                }
                '+' => {
                    self.next2();
                    Token::PlusPlus
                }
                '=' => {
                    self.next2();
                    Token::PlusEqual
                }
                _ => {
                    self.next();
                    Token::Plus
                }
            },

            '>' if !self.in_diamond_mode() => match self.peek() {
                '=' => {
                    self.next2();
                    Token::GreaterEqual
                }
                '>' => {
                    if self.peek2() == '>' {
                        self.next3();
                        Token::RightShiftUnsigned
                    } else {
                        self.next2();
                        Token::RightShift
                    }
                }
                _ => {
                    self.next();
                    Token::GreaterThan
                }
            },
            '>' => {
                self.next();
                Token::GreaterThan
            }

            '<' => match self.peek() {
                '<' if !self.in_diamond_mode() => {
                    self.next2();
                    Token::LeftShift
                }
                '=' => {
                    self.next2();
                    Token::LessEqual
                }
                _ => {
                    self.next();
                    Token::LessThan
                }
            },

            '.' => match (self.peek(), self.peek2()) {
                ('.', '.') => {
                    self.next3();
                    Token::DotDotDot
                }
                ('.', _) => {
                    self.next2();
                    Token::DotDot
                }
                _ => {
                    self.next();
                    Token::Dot
                }
            },

            '\'' => match (self.peek(), self.peek2()) {
                ('\\', '"') => {
                    self.next();
                    Token::SingleQuote
                }
                ('\\', _) => {
                    self.next2();
                    self.scan_escape()
                }
                (ch, '\'') => {
                    let offset = self.offset + 1;
                    self.next3();
                    Token::Codepoint {
                        c: ch as i32,
                        original: self.substring(offset, offset + 1).to_string(),
                    }
                }
                _ => {
                    self.next();
                    Token::SingleQuote
                }
            },

            '!' => match (self.peek(), self.peek2()) {
                ('=', '=') => {
                    self.next3();
                    Token::BangEqualEqual
                }
                ('=', _) => {
                    self.next2();
                    Token::BangEqual
                }
                _ => {
                    self.next();
                    Token::Bang
                }
            },

            '=' => match (self.peek(), self.peek2()) {
                ('=', '=') => {
                    self.next3();
                    Token::EqualEqualEqual
                }
                ('=', _) => {
                    self.next2();
                    Token::EqualEqual
                }
                ('>', _) => {
                    self.next2();
                    Token::EqualGreater
                }
                _ => {
                    self.next();
                    Token::Equal
                }
            },

            '\\' => {
                // Exotic identifier
                self.next();
                if self.ch == '"' {
                    self.scan_exotic_identifier()
                } else {
                    Token::Backslash
                }
            }

            c if c == EOF_CHAR => {
                self.next();
                Token::Eof
            }

            ch => {
                // Unknown character
                self.next();
                let end_pos = self.position();
                self.error(
                    start_pos.clone(),
                    end_pos,
                    DiagnosticCategory::unknown_uchar(ch),
                );
                // Continue scanning
                return self.scan();
            }
        };

        let end_pos = self.position();
        if !matches!(token, Token::Comment(_)) {
            self.prev_token = Some(token.clone());
        }
        ScanResult {
            start_pos,
            end_pos,
            token,
        }
    }

    fn is_jsx_close_slash(&self) -> bool {
        self.offset > 0 && self.src_bytes.get(self.offset - 1) == Some(&b'<')
    }

    fn can_start_regex_literal(&self) -> bool {
        match &self.prev_token {
            None => true,
            Some(prev) => {
                if prev.precedence() > 0 {
                    return true;
                }
                matches!(
                    prev,
                    Token::Lparen
                        | Token::Lbracket
                        | Token::Lbrace
                        | Token::Comma
                        | Token::Semicolon
                        | Token::Colon
                        | Token::Equal
                        | Token::EqualGreater
                        | Token::MinusGreater
                        | Token::Question
                        | Token::Bar
                        | Token::Lor
                        | Token::Land
                        | Token::And
                        | Token::As
                        | Token::If
                        | Token::Else
                        | Token::Try
                        | Token::When
                        | Token::In
                        | Token::For
                        | Token::While
                        | Token::Let { .. }
                        | Token::Await
                        | Token::Assert
                        | Token::Open
                        | Token::Module
                        | Token::Typ
                        | Token::External
                        | Token::Exception
                        | Token::Include
                        | Token::Of
                )
            }
        }
    }

    fn try_scan_regex_literal(&mut self) -> Option<Token> {
        let snapshot = self.snapshot();
        let start_pos = self.position();

        // Consume opening '/'
        self.next();
        let pattern_start = self.offset;

        let mut escaped = false;
        let mut in_char_class = false;

        loop {
            match self.ch {
                '\n' | '\r' => {
                    // Invalid regex literal, restore and fall back to '/'
                    self.restore(snapshot);
                    return None;
                }
                c if c == EOF_CHAR => {
                    // Invalid regex literal, restore and fall back to '/'
                    self.restore(snapshot);
                    return None;
                }
                '\\' if !escaped => {
                    escaped = true;
                    self.next();
                }
                '[' if !escaped => {
                    in_char_class = true;
                    self.next();
                }
                ']' if !escaped => {
                    in_char_class = false;
                    self.next();
                }
                '/' if !escaped && !in_char_class => {
                    break;
                }
                _ => {
                    escaped = false;
                    self.next();
                }
            }
        }

        // Current char is closing '/'
        let pattern = self.substring(pattern_start, self.offset).to_string();
        self.next(); // consume closing '/'

        let flags_start = self.offset;
        while self.ch.is_ascii_alphabetic() {
            self.next();
        }
        let flags = self.substring(flags_start, self.offset).to_string();

        let end_pos = self.position();
        // Record a scanner-level diagnostic if the literal is empty and we didn't advance.
        // (Helps debugging invalid recovery cases; parser can still proceed.)
        if pattern_start == flags_start && pattern.is_empty() {
            self.error(
                start_pos,
                end_pos,
                DiagnosticCategory::message("Empty regex literal".to_string()),
            );
        }

        Some(Token::Regex { pattern, flags })
    }

    /// Scan an escape sequence in a character literal.
    fn scan_escape(&mut self) -> Token {
        let offset = self.offset - 1;

        let codepoint = match self.ch {
            '0'..='9' => self.convert_number(3, 10),
            'b' => {
                self.next();
                8
            }
            'n' => {
                self.next();
                10
            }
            'r' => {
                self.next();
                13
            }
            't' => {
                self.next();
                9
            }
            'x' => {
                self.next();
                self.convert_number(2, 16)
            }
            'o' => {
                self.next();
                self.convert_number(3, 8)
            }
            'u' => {
                self.next();
                if self.ch == '{' {
                    self.next();
                    let mut x = 0;
                    while self.ch.is_ascii_hexdigit() {
                        x = x * 16 + Self::digit_value(self.ch) as i32;
                        self.next();
                    }
                    if self.ch == '}' {
                        self.next();
                    }
                    if utf8::is_valid_codepoint(x) {
                        x
                    } else {
                        utf8::REPLACEMENT_CHAR
                    }
                } else {
                    self.convert_number(4, 16)
                }
            }
            ch => {
                self.next();
                ch as i32
            }
        };

        // Consume closing quote
        if self.ch == '\'' {
            self.next();
        }

        let contents = self.substring(offset, self.offset).to_string();
        Token::Codepoint {
            c: codepoint,
            original: contents,
        }
    }

    /// Convert a sequence of digits to a number.
    fn convert_number(&mut self, n: usize, base: u32) -> i32 {
        let mut x = 0i32;
        for _ in 0..n {
            let d = Self::digit_value(self.ch);
            if d < base {
                x = x * base as i32 + d as i32;
                self.next();
            }
        }
        if utf8::is_valid_codepoint(x) {
            x
        } else {
            utf8::REPLACEMENT_CHAR
        }
    }

    /// Scan an exotic identifier (e.g., \"foo").
    /// Returns the identifier without surrounding quotes.
    fn scan_exotic_identifier(&mut self) -> Token {
        let start_pos = self.position();

        self.next(); // consume opening quote
        let content_start = self.offset; // start of identifier content (after opening quote)

        loop {
            match self.ch {
                '"' => {
                    let content_end = self.offset; // end of content (before closing quote)
                    self.next(); // consume closing quote
                    // Return identifier without quotes
                    let ident = self.substring(content_start, content_end).to_string();
                    return Token::Lident(ident);
                }
                '\n' | '\r' => {
                    let end_pos = self.position();
                    self.error(
                        start_pos.clone(),
                        end_pos,
                        DiagnosticCategory::message(
                            "A quoted identifier can't contain line breaks.",
                        ),
                    );
                    let ident = self.substring(content_start, self.offset).to_string();
                    self.next();
                    return Token::Lident(ident);
                }
                c if c == EOF_CHAR => {
                    let end_pos = self.position();
                    self.error(
                        start_pos.clone(),
                        end_pos,
                        DiagnosticCategory::message("Did you forget a \" here?"),
                    );
                    let ident = self.substring(content_start, self.offset).to_string();
                    return Token::Lident(ident);
                }
                _ => self.next(),
            }
        }
    }

    /// Check if an operator has whitespace on both sides (making it binary).
    pub fn is_binary_op(src: &str, start_cnum: usize, end_cnum: usize) -> bool {
        if start_cnum == 0 {
            return false;
        }

        let bytes = src.as_bytes();
        let left_ok = start_cnum > 0
            && start_cnum <= bytes.len()
            && Self::is_whitespace(bytes[start_cnum - 1] as char);

        let right_ok = end_cnum >= bytes.len() || Self::is_whitespace(bytes[end_cnum] as char);

        left_ok && right_ok
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_identifier() {
        let mut scanner = Scanner::new("test.res", "hello world");
        let result = scanner.scan();
        assert_eq!(result.token, Token::Lident("hello".to_string()));

        let result = scanner.scan();
        assert_eq!(result.token, Token::Lident("world".to_string()));
    }

    #[test]
    fn test_scan_keyword() {
        let mut scanner = Scanner::new("test.res", "let if else");
        assert_eq!(scanner.scan().token, Token::Let { unwrap: false });
        assert_eq!(scanner.scan().token, Token::If);
        assert_eq!(scanner.scan().token, Token::Else);
    }

    #[test]
    fn test_scan_number() {
        let mut scanner = Scanner::new("test.res", "42 3.14 0xFF");

        let result = scanner.scan();
        assert!(matches!(result.token, Token::Int { i, .. } if i == "42"));

        let result = scanner.scan();
        assert!(matches!(result.token, Token::Float { f, .. } if f == "3.14"));

        let result = scanner.scan();
        assert!(matches!(result.token, Token::Int { i, .. } if i == "0xFF"));
    }

    #[test]
    fn test_scan_string() {
        let mut scanner = Scanner::new("test.res", "\"hello\"");
        let result = scanner.scan();
        assert!(matches!(result.token, Token::String(s) if s == "hello"));
    }

    #[test]
    fn test_scan_operators() {
        let mut scanner = Scanner::new("test.res", "+ - * / == => ->");

        assert_eq!(scanner.scan().token, Token::Plus);
        assert_eq!(scanner.scan().token, Token::Minus);
        assert_eq!(scanner.scan().token, Token::Asterisk);
        assert_eq!(scanner.scan().token, Token::Forwardslash);
        assert_eq!(scanner.scan().token, Token::EqualEqual);
        assert_eq!(scanner.scan().token, Token::EqualGreater);
        assert_eq!(scanner.scan().token, Token::MinusGreater);
    }

    #[test]
    fn test_scan_comment() {
        let mut scanner = Scanner::new("test.res", "// comment\nfoo");

        let result = scanner.scan();
        assert!(matches!(result.token, Token::Comment(_)));

        let result = scanner.scan();
        assert_eq!(result.token, Token::Lident("foo".to_string()));
    }

    #[test]
    fn test_scan_multiline_comment() {
        let mut scanner = Scanner::new("test.res", "/* comment */ foo");

        let result = scanner.scan();
        assert!(matches!(result.token, Token::Comment(_)));

        let result = scanner.scan();
        assert_eq!(result.token, Token::Lident("foo".to_string()));
    }

    #[test]
    fn test_scan_eof() {
        let mut scanner = Scanner::new("test.res", "");
        assert_eq!(scanner.scan().token, Token::Eof);
    }

    #[test]
    fn test_position_tracking() {
        let mut scanner = Scanner::new("test.res", "let\nfoo");

        let result = scanner.scan();
        assert_eq!(result.start_pos.line, 1);

        let result = scanner.scan();
        assert_eq!(result.start_pos.line, 2);
    }

    #[test]
    fn test_diamond_mode() {
        let mut scanner = Scanner::new("test.res", ">> >");

        // Normal mode: >> is right shift
        let result = scanner.scan();
        assert_eq!(result.token, Token::RightShift);

        // In diamond mode: > is just greater than
        scanner.set_diamond_mode();
        let result = scanner.scan();
        assert_eq!(result.token, Token::GreaterThan);
    }

    #[test]
    fn test_list_dict_let_special() {
        let mut scanner = Scanner::new("test.res", "list{ dict{ let?");

        assert_eq!(scanner.scan().token, Token::List);
        assert_eq!(scanner.scan().token, Token::Dict);
        assert_eq!(scanner.scan().token, Token::Let { unwrap: true });
    }
}
