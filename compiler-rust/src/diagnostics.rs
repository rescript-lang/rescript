//! Diagnostics types for warnings and errors.
//!
//! This module provides the infrastructure for collecting and reporting
//! compilation diagnostics. Unlike the OCaml implementation which uses
//! global refs (`Warnings.current`, `Warnings.nerrors`), all state is
//! contained in a `DiagnosticsContext` that is passed through compilation.
//!
//! # Example
//!
//! ```rust
//! use rescript_compiler::diagnostics::{DiagnosticsContext, Warning, WarningConfig};
//! use rescript_compiler::location::Location;
//!
//! let mut ctx = DiagnosticsContext::new();
//!
//! // Report a warning
//! ctx.warn(Warning::UnusedVariable { name: "x".to_string() }, Location::none());
//!
//! // Check for warnings
//! assert!(ctx.has_warnings());
//! ```

use serde::{Deserialize, Serialize};
use std::fmt;

use crate::location::Location;

/// Warning types.
///
/// These correspond to `Warnings.t` in OCaml.
/// Warning numbers are preserved for compatibility.
/// Variant fields are internal representation matching the OCaml codebase.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum Warning {
    /// Warning 1: Suspicious-looking start-of-comment mark.
    CommentStart,
    /// Warning 2: Suspicious-looking end-of-comment mark.
    CommentNotEnd,
    /// Warning 3: Deprecated feature.
    Deprecated {
        message: String,
        def_loc: Location,
        use_loc: Location,
        can_be_automigrated: bool,
    },
    /// Warning 4: Fragile pattern matching.
    FragileMatch { typ: String },
    /// Warning 5: Partially applied function.
    PartialApplication,
    /// Warning 7: Method overridden.
    MethodOverride { methods: Vec<String> },
    /// Warning 8: Partial match (missing cases).
    PartialMatch { example: String },
    /// Warning 9: Missing fields in record pattern.
    NonClosedRecordPattern { fields: String },
    /// Warning 10: Statement has non-unit type.
    StatementType,
    /// Warning 11: Unused match case.
    UnusedMatch,
    /// Warning 12: Unused sub-pattern.
    UnusedPat,
    /// Warning 14: Illegal backslash escape.
    IllegalBackslash,
    /// Warning 16: Unerasable optional argument.
    UnerasableOptionalArgument,
    /// Warning 20: Unused argument.
    UnusedArgument,
    /// Warning 21: Non-returning statement.
    NonreturningStatement,
    /// Warning 22: Preprocessor warning.
    Preprocessor { message: String },
    /// Warning 23: Useless record with clause.
    UselessRecordWith,
    /// Warning 24: Bad module name.
    BadModuleName { name: String },
    /// Warning 26: Unused variable (let-bound or as-pattern).
    UnusedVariable { name: String },
    /// Warning 27: Unused variable (other).
    UnusedVariableStrict { name: String },
    /// Warning 28: Wildcard argument to constant constructor.
    WildcardArgToConstantConstr,
    /// Warning 29: End-of-line in string constant.
    EolInString,
    /// Warning 30: Duplicate definitions.
    DuplicateDefinitions {
        kind: String,
        name: String,
        type1: String,
        type2: String,
    },
    /// Warning 32: Unused value declaration.
    UnusedValueDeclaration { name: String },
    /// Warning 33: Unused open statement.
    UnusedOpen { name: String },
    /// Warning 34: Unused type declaration.
    UnusedTypeDeclaration { name: String },
    /// Warning 35: Unused for-loop index.
    UnusedForIndex { name: String },
    /// Warning 37: Unused constructor.
    UnusedConstructor {
        name: String,
        used_in_pattern: bool,
        is_private: bool,
    },
    /// Warning 38: Unused extension constructor.
    UnusedExtension {
        name: String,
        is_exception: bool,
        used_in_pattern: bool,
        is_private: bool,
    },
    /// Warning 39: Unused rec flag.
    UnusedRecFlag,
    /// Warning 41: Ambiguous name.
    AmbiguousName {
        names: Vec<String>,
        types: Vec<String>,
        is_label: bool,
    },
    /// Warning 43: Nonoptional label.
    NonoptionalLabel { name: String },
    /// Warning 47: Illegal attribute payload.
    AttributePayload { attr: String, message: String },
    /// Warning 48: Eliminated optional arguments.
    EliminatedOptionalArguments { args: Vec<String> },
    /// Warning 49: No cmi file found.
    NoCmiFile {
        name: String,
        reason: Option<String>,
    },
    /// Warning 52: Fragile literal pattern.
    FragileLiteralPattern,
    /// Warning 53: Misplaced attribute.
    MisplacedAttribute { name: String },
    /// Warning 54: Duplicated attribute.
    DuplicatedAttribute { name: String },
    /// Warning 56: Unreachable case.
    UnreachableCase,
    /// Warning 57: Ambiguous or-pattern variables.
    AmbiguousPattern { vars: Vec<String> },
    /// Warning 60: Unused module.
    UnusedModule { name: String },
    /// Warning 101: Unused ReScript attribute.
    BsUnusedAttribute { name: String },
    /// Warning 102: Polymorphic comparison.
    BsPolymorphicComparison,
    /// Warning 103: FFI warning.
    BsFfiWarning { message: String },
    /// Warning 104: @deriving warning.
    BsDeriveWarning { message: String },
    /// Warning 105: Fragile external.
    BsFragileExternal { name: String },
    /// Warning 106: Unimplemented primitive.
    BsUnimplementedPrimitive { name: String },
    /// Warning 107: Integer literal overflow.
    BsIntegerLiteralOverflow,
    /// Warning 108: Uninterpreted delimiters.
    BsUninterpretedDelimiters { delim: String },
    /// Warning 109: Toplevel expression is not unit.
    BsToplevelExpressionUnit {
        return_type: Option<String>,
        is_function_call: bool,
    },
    /// Warning 110: Todo found.
    BsTodo { message: Option<String> },
}

impl Warning {
    /// Get the warning number.
    pub fn number(&self) -> i32 {
        match self {
            Warning::CommentStart => 1,
            Warning::CommentNotEnd => 2,
            Warning::Deprecated { .. } => 3,
            Warning::FragileMatch { .. } => 4,
            Warning::PartialApplication => 5,
            Warning::MethodOverride { .. } => 7,
            Warning::PartialMatch { .. } => 8,
            Warning::NonClosedRecordPattern { .. } => 9,
            Warning::StatementType => 10,
            Warning::UnusedMatch => 11,
            Warning::UnusedPat => 12,
            Warning::IllegalBackslash => 14,
            Warning::UnerasableOptionalArgument => 16,
            Warning::UnusedArgument => 20,
            Warning::NonreturningStatement => 21,
            Warning::Preprocessor { .. } => 22,
            Warning::UselessRecordWith => 23,
            Warning::BadModuleName { .. } => 24,
            Warning::UnusedVariable { .. } => 26,
            Warning::UnusedVariableStrict { .. } => 27,
            Warning::WildcardArgToConstantConstr => 28,
            Warning::EolInString => 29,
            Warning::DuplicateDefinitions { .. } => 30,
            Warning::UnusedValueDeclaration { .. } => 32,
            Warning::UnusedOpen { .. } => 33,
            Warning::UnusedTypeDeclaration { .. } => 34,
            Warning::UnusedForIndex { .. } => 35,
            Warning::UnusedConstructor { .. } => 37,
            Warning::UnusedExtension { .. } => 38,
            Warning::UnusedRecFlag => 39,
            Warning::AmbiguousName { .. } => 41,
            Warning::NonoptionalLabel { .. } => 43,
            Warning::AttributePayload { .. } => 47,
            Warning::EliminatedOptionalArguments { .. } => 48,
            Warning::NoCmiFile { .. } => 49,
            Warning::FragileLiteralPattern => 52,
            Warning::MisplacedAttribute { .. } => 53,
            Warning::DuplicatedAttribute { .. } => 54,
            Warning::UnreachableCase => 56,
            Warning::AmbiguousPattern { .. } => 57,
            Warning::UnusedModule { .. } => 60,
            Warning::BsUnusedAttribute { .. } => 101,
            Warning::BsPolymorphicComparison => 102,
            Warning::BsFfiWarning { .. } => 103,
            Warning::BsDeriveWarning { .. } => 104,
            Warning::BsFragileExternal { .. } => 105,
            Warning::BsUnimplementedPrimitive { .. } => 106,
            Warning::BsIntegerLiteralOverflow => 107,
            Warning::BsUninterpretedDelimiters { .. } => 108,
            Warning::BsToplevelExpressionUnit { .. } => 109,
            Warning::BsTodo { .. } => 110,
        }
    }

    /// Get the warning message.
    pub fn message(&self) -> String {
        match self {
            Warning::CommentStart => "this is the start of a comment.".to_string(),
            Warning::CommentNotEnd => "this is not the end of a comment.".to_string(),
            Warning::Deprecated {
                message,
                can_be_automigrated,
                ..
            } => {
                let mut msg = format!("deprecated: {message}");
                if *can_be_automigrated {
                    msg.push_str(
                        "\n\n  This can be automatically migrated by the ReScript migration tool.",
                    );
                }
                msg
            }
            Warning::FragileMatch { typ } if typ.is_empty() => {
                "this pattern-matching is fragile.".to_string()
            }
            Warning::FragileMatch { typ } => {
                format!(
                    "this pattern-matching is fragile.\n\
                     It will remain exhaustive when constructors are added to type {typ}."
                )
            }
            Warning::PartialApplication => {
                "this function application is partial,\nmaybe some arguments are missing."
                    .to_string()
            }
            Warning::MethodOverride { methods } if methods.len() == 1 => {
                format!("the method {} is overridden.", methods[0])
            }
            Warning::MethodOverride { methods } => {
                format!(
                    "the following methods are overridden by the class {}:\n {}",
                    methods[0],
                    methods[1..].join(" ")
                )
            }
            Warning::PartialMatch { example } if example.is_empty() => {
                "You forgot to handle a possible case here, though we don't have more \
                 information on the value."
                    .to_string()
            }
            Warning::PartialMatch { example } => {
                format!(
                    "You forgot to handle a possible case here, for example: \n  {}",
                    example
                )
            }
            Warning::NonClosedRecordPattern { fields } => {
                format!(
                    "the following labels are not bound in this record pattern: {}\n\
                     Either bind these labels explicitly or add ', _' to the pattern.",
                    fields
                )
            }
            Warning::StatementType => {
                "This expression returns a value, but you're not doing anything with it. \
                 If this is on purpose, wrap it with `ignore`."
                    .to_string()
            }
            Warning::UnusedMatch => "this match case is unused.".to_string(),
            Warning::UnusedPat => "this sub-pattern is unused.".to_string(),
            Warning::IllegalBackslash => "illegal backslash escape in string.".to_string(),
            Warning::UnerasableOptionalArgument => {
                "This optional parameter in final position will, in practice, not be optional.\n\
                 Reorder the parameters so that at least one non-optional one is in final position."
                    .to_string()
            }
            Warning::UnusedArgument => {
                "this argument will not be used by the function.".to_string()
            }
            Warning::NonreturningStatement => {
                "this statement never returns (or has an unsound type.)".to_string()
            }
            Warning::Preprocessor { message } => message.clone(),
            Warning::UselessRecordWith => {
                "All the fields are already explicitly listed in this record. \
                 You can remove the `...` spread."
                    .to_string()
            }
            Warning::BadModuleName { name } => {
                format!(
                    "This file's name is potentially invalid. \
                     {} isn't a valid module name.",
                    name
                )
            }
            Warning::UnusedVariable { name } | Warning::UnusedVariableStrict { name } => {
                format!(
                    "unused variable {}.\n\n\
                     Fix this by:\n\
                     - Deleting the variable if it's not used anymore.\n\
                     - Prepending the variable name with `_` (like `_{}`) to ignore it.\n\
                     - Using the variable somewhere.",
                    name, name
                )
            }
            Warning::WildcardArgToConstantConstr => {
                "wildcard pattern given as argument to a constant constructor".to_string()
            }
            Warning::EolInString => {
                "unescaped end-of-line in a string constant (non-portable code)".to_string()
            }
            Warning::DuplicateDefinitions {
                kind,
                name,
                type1,
                type2,
            } => {
                format!(
                    "the {} {} is defined in both types {} and {}.",
                    kind, name, type1, type2
                )
            }
            Warning::UnusedValueDeclaration { name } => format!("unused value {}.", name),
            Warning::UnusedOpen { name } => format!("unused open {}.", name),
            Warning::UnusedTypeDeclaration { name } => format!("unused type {}.", name),
            Warning::UnusedForIndex { name } => format!("unused for-loop index {}.", name),
            Warning::UnusedConstructor {
                name,
                used_in_pattern,
                is_private,
            } => {
                if *used_in_pattern {
                    format!(
                        "constructor {} is never used to build values.\n\
                         (However, this constructor appears in patterns.)",
                        name
                    )
                } else if *is_private {
                    format!(
                        "constructor {} is never used to build values.\n\
                         Its type is exported as a private type.",
                        name
                    )
                } else {
                    format!("unused constructor {}.", name)
                }
            }
            Warning::UnusedExtension {
                name,
                is_exception,
                used_in_pattern,
                is_private,
            } => {
                let kind = if *is_exception {
                    "exception"
                } else {
                    "extension constructor"
                };
                if *used_in_pattern {
                    format!(
                        "{} {} is never used to build values.\n\
                         (However, this constructor appears in patterns.)",
                        kind, name
                    )
                } else if *is_private {
                    format!(
                        "{} {} is never used to build values.\n\
                         It is exported or rebound as a private extension.",
                        kind, name
                    )
                } else {
                    format!("unused {} {}", kind, name)
                }
            }
            Warning::UnusedRecFlag => "unused rec flag.".to_string(),
            Warning::AmbiguousName {
                names,
                types,
                is_label,
            } => {
                if *is_label {
                    format!(
                        "these field labels belong to several types: {}\n\
                         The first one was selected. Disambiguate if this is wrong.",
                        types.join(" ")
                    )
                } else if names.len() == 1 {
                    format!(
                        "{} belongs to several types: {}\n\
                         The first one was selected. Disambiguate if this is wrong.",
                        names[0],
                        types.join(" ")
                    )
                } else {
                    "ambiguous name".to_string()
                }
            }
            Warning::NonoptionalLabel { name } => format!("the label {} is not optional.", name),
            Warning::AttributePayload { attr, message } => {
                format!("illegal payload for attribute '{}'.\n{}", attr, message)
            }
            Warning::EliminatedOptionalArguments { args } => {
                let plural = if args.len() == 1 { "" } else { "s" };
                format!(
                    "implicit elimination of optional argument{} {}",
                    plural,
                    args.join(", ")
                )
            }
            Warning::NoCmiFile { name, reason } => match reason {
                Some(msg) => format!(
                    "no valid cmi file was found in path for module {}. {}",
                    name, msg
                ),
                None => format!("no cmi file was found in path for module {}", name),
            },
            Warning::FragileLiteralPattern => {
                "Code should not depend on the actual values of this constructor's arguments."
                    .to_string()
            }
            Warning::MisplacedAttribute { name } => {
                format!("the \"{}\" attribute cannot appear in this context", name)
            }
            Warning::DuplicatedAttribute { name } => {
                format!(
                    "the \"{}\" attribute is used more than once on this expression",
                    name
                )
            }
            Warning::UnreachableCase => "this match case is unreachable.\n\
                 Consider replacing it with a refutation case '<pat> -> .'"
                .to_string(),
            Warning::AmbiguousPattern { vars } => {
                let msg = if vars.len() == 1 {
                    format!("variable {}", vars[0])
                } else {
                    format!("variables {}", vars.join(","))
                };
                format!(
                    "Ambiguous or-pattern variables under guard;\n\
                     {} may match different arguments.",
                    msg
                )
            }
            Warning::UnusedModule { name } => format!("unused module {}.", name),
            Warning::BsUnusedAttribute { name } => {
                format!(
                    "Unused attribute: @{}\n\
                     This attribute has no effect here.",
                    name
                )
            }
            Warning::BsPolymorphicComparison => {
                "Polymorphic comparison introduced (maybe unsafe)".to_string()
            }
            Warning::BsFfiWarning { message } => format!("FFI warning: {}", message),
            Warning::BsDeriveWarning { message } => format!("@deriving warning: {}", message),
            Warning::BsFragileExternal { name } => {
                format!(
                    "{}: using an empty string as a shorthand to infer the external's name \
                     from the value's name is dangerous when refactoring",
                    name
                )
            }
            Warning::BsUnimplementedPrimitive { name } => {
                format!("Unimplemented primitive used: {}", name)
            }
            Warning::BsIntegerLiteralOverflow => {
                "Integer literal exceeds the range of representable integers of type int"
                    .to_string()
            }
            Warning::BsUninterpretedDelimiters { delim } => {
                format!("Uninterpreted delimiters {}", delim)
            }
            Warning::BsToplevelExpressionUnit {
                return_type,
                is_function_call,
            } => {
                let what = if *is_function_call {
                    "function call"
                } else {
                    "expression"
                };
                let ret = return_type
                    .as_deref()
                    .unwrap_or("something that is not `unit`");
                format!(
                    "This {} is at the top level and is expected to return `unit`. \
                     But it's returning {}.",
                    what, ret
                )
            }
            Warning::BsTodo { message } => match message {
                Some(text) => format!("Todo found: {}", text),
                None => "Todo found.".to_string(),
            },
        }
    }
}

impl fmt::Display for Warning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}

/// Warning configuration.
///
/// Controls which warnings are active and which are treated as errors.
#[derive(Debug, Clone)]
pub struct WarningConfig {
    /// Which warnings are active (indexed by warning number).
    active: Vec<bool>,
    /// Which warnings are errors (indexed by warning number).
    errors: Vec<bool>,
    /// Whether warnings are disabled globally.
    disabled: bool,
}

const MAX_WARNING_NUMBER: usize = 111;

impl Default for WarningConfig {
    fn default() -> Self {
        Self::new()
    }
}

impl WarningConfig {
    /// Create a new warning configuration with defaults.
    pub fn new() -> Self {
        let mut config = Self {
            active: vec![true; MAX_WARNING_NUMBER],
            errors: vec![false; MAX_WARNING_NUMBER],
            disabled: false,
        };
        // Apply default warning configuration
        config.parse("-40-42-44-45-48-60-6-52-9").ok();
        config
    }

    /// Disable all warnings.
    pub fn disable_all(&mut self) {
        self.disabled = true;
    }

    /// Enable all warnings.
    pub fn enable_all(&mut self) {
        self.disabled = false;
    }

    /// Check if a warning is active.
    pub fn is_active(&self, warning: &Warning) -> bool {
        if self.disabled {
            return false;
        }
        let n = warning.number() as usize;
        n < self.active.len() && self.active[n]
    }

    /// Check if a warning is an error.
    pub fn is_error(&self, warning: &Warning) -> bool {
        if self.disabled {
            return false;
        }
        let n = warning.number() as usize;
        n < self.errors.len() && self.errors[n]
    }

    /// Parse a warning specification string.
    ///
    /// Format: `+n` to enable, `-n` to disable, `@n` to make error.
    /// Can use ranges like `+1..10` or letters like `+A` for all.
    pub fn parse(&mut self, spec: &str) -> Result<(), String> {
        let mut chars = spec.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                '+' => self.apply_modifier(&mut chars, |active, _| *active = true)?,
                '-' => self.apply_modifier(&mut chars, |active, _| *active = false)?,
                '@' => self.apply_modifier(&mut chars, |active, error| {
                    *active = true;
                    *error = true;
                })?,
                'A'..='Z' => {
                    for n in letter_warnings(c.to_ascii_lowercase()) {
                        if n < self.active.len() {
                            self.active[n] = true;
                        }
                    }
                }
                'a'..='z' => {
                    for n in letter_warnings(c) {
                        if n < self.active.len() {
                            self.active[n] = false;
                        }
                    }
                }
                ' ' | '\t' | '\n' => {}
                _ => return Err(format!("invalid warning specification: {}", c)),
            }
        }

        Ok(())
    }

    fn apply_modifier<F>(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
        mut modifier: F,
    ) -> Result<(), String>
    where
        F: FnMut(&mut bool, &mut bool),
    {
        match chars.peek() {
            Some('0'..='9') => {
                let (start, end) = self.parse_range(chars)?;
                for n in start..=end {
                    if n < self.active.len() {
                        modifier(&mut self.active[n], &mut self.errors[n]);
                    }
                }
            }
            Some('A'..='Z') | Some('a'..='z') => {
                let letter = chars.next().unwrap().to_ascii_lowercase();
                for n in letter_warnings(letter) {
                    if n < self.active.len() {
                        modifier(&mut self.active[n], &mut self.errors[n]);
                    }
                }
            }
            _ => return Err("expected number or letter after +/-/@".to_string()),
        }
        Ok(())
    }

    fn parse_range(
        &self,
        chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    ) -> Result<(usize, usize), String> {
        let start = self.parse_number(chars)?;

        // Check for range ".."
        if chars.peek() == Some(&'.') {
            chars.next();
            if chars.peek() == Some(&'.') {
                chars.next();
                let end = self.parse_number(chars)?;
                if end < start {
                    return Err(format!("invalid range: {}..{}", start, end));
                }
                return Ok((start, end.min(MAX_WARNING_NUMBER - 1)));
            }
        }

        Ok((start, start))
    }

    fn parse_number(
        &self,
        chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    ) -> Result<usize, String> {
        let mut n = 0usize;
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                n = n * 10 + (c as usize - '0' as usize);
                chars.next();
            } else {
                break;
            }
        }
        Ok(n)
    }
}

/// Get warning numbers for a letter abbreviation.
fn letter_warnings(letter: char) -> Vec<usize> {
    match letter {
        'a' => (1..=MAX_WARNING_NUMBER).collect(),
        'c' => vec![1, 2],
        'd' => vec![3],
        'e' => vec![4],
        'f' => vec![5],
        'k' => vec![32, 33, 34, 35, 36, 37, 38, 39],
        'l' => vec![6],
        'm' => vec![7],
        'p' => vec![8],
        'r' => vec![9],
        's' => vec![10],
        'u' => vec![11, 12],
        'v' => vec![13],
        'x' => vec![14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 30],
        'y' => vec![26],
        'z' => vec![27],
        _ => vec![],
    }
}

/// A reported diagnostic (warning or error).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    /// The warning or error.
    pub kind: DiagnosticKind,
    /// Location in source.
    pub location: Location,
    /// Whether this is treated as an error.
    pub is_error: bool,
}

/// The kind of diagnostic.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DiagnosticKind {
    /// A warning.
    Warning(Warning),
    /// A compilation error.
    Error(CompileError),
}

/// Compilation error types.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompileError {
    /// Error message.
    pub message: String,
    /// Sub-errors for more context.
    pub sub: Vec<CompileError>,
    /// Alternative message for highlighted output.
    pub if_highlight: String,
}

impl CompileError {
    /// Create a simple error.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            sub: Vec::new(),
            if_highlight: String::new(),
        }
    }

    /// Create an error with sub-errors.
    pub fn with_sub(message: impl Into<String>, sub: Vec<CompileError>) -> Self {
        Self {
            message: message.into(),
            sub,
            if_highlight: String::new(),
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

/// Diagnostics context for collecting warnings and errors.
///
/// This replaces the global state in `Warnings` (`current`, `nerrors`, etc.).
/// Each compilation gets its own `DiagnosticsContext`.
#[derive(Debug, Default)]
pub struct DiagnosticsContext {
    /// Warning configuration.
    config: WarningConfig,
    /// Collected diagnostics.
    diagnostics: Vec<Diagnostic>,
    /// Number of errors (warnings treated as errors count).
    error_count: usize,
    /// Whether any warnings were reported.
    has_warnings: bool,
}

impl DiagnosticsContext {
    /// Create a new diagnostics context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create with custom warning configuration.
    pub fn with_config(config: WarningConfig) -> Self {
        Self {
            config,
            ..Default::default()
        }
    }

    /// Get the warning configuration.
    pub fn config(&self) -> &WarningConfig {
        &self.config
    }

    /// Get mutable access to the warning configuration.
    pub fn config_mut(&mut self) -> &mut WarningConfig {
        &mut self.config
    }

    /// Report a warning.
    pub fn warn(&mut self, warning: Warning, location: Location) {
        if !self.config.is_active(&warning) {
            return;
        }

        self.has_warnings = true;
        let is_error = self.config.is_error(&warning);
        if is_error {
            self.error_count += 1;
        }

        self.diagnostics.push(Diagnostic {
            kind: DiagnosticKind::Warning(warning),
            location,
            is_error,
        });
    }

    /// Report an error.
    pub fn error(&mut self, error: CompileError, location: Location) {
        self.error_count += 1;
        self.diagnostics.push(Diagnostic {
            kind: DiagnosticKind::Error(error),
            location,
            is_error: true,
        });
    }

    /// Check if any warnings were reported.
    pub fn has_warnings(&self) -> bool {
        self.has_warnings
    }

    /// Check if any errors were reported.
    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    /// Get the number of errors.
    pub fn error_count(&self) -> usize {
        self.error_count
    }

    /// Get all diagnostics.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Take all diagnostics, leaving the context empty.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.error_count = 0;
        self.has_warnings = false;
        std::mem::take(&mut self.diagnostics)
    }

    /// Reset the diagnostics context.
    pub fn reset(&mut self) {
        self.diagnostics.clear();
        self.error_count = 0;
        self.has_warnings = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_warning_number() {
        assert_eq!(Warning::CommentStart.number(), 1);
        assert_eq!(
            Warning::UnusedVariable {
                name: "x".to_string()
            }
            .number(),
            26
        );
        assert_eq!(Warning::BsTodo { message: None }.number(), 110);
    }

    #[test]
    fn test_warning_config_parse() {
        let mut config = WarningConfig::new();
        config.parse("+1-2@3").unwrap();

        assert!(config.active[1]);
        assert!(!config.active[2]);
        assert!(config.active[3]);
        assert!(config.errors[3]);
    }

    #[test]
    fn test_warning_config_range() {
        let mut config = WarningConfig::new();
        config.parse("-1..10").unwrap();

        for i in 1..=10 {
            assert!(!config.active[i]);
        }
    }

    #[test]
    fn test_diagnostics_context() {
        let mut ctx = DiagnosticsContext::new();

        ctx.warn(
            Warning::UnusedVariable {
                name: "x".to_string(),
            },
            Location::none(),
        );

        assert!(ctx.has_warnings());
        assert!(!ctx.has_errors());
        assert_eq!(ctx.diagnostics().len(), 1);
    }

    #[test]
    fn test_diagnostics_error() {
        let mut ctx = DiagnosticsContext::new();

        ctx.error(CompileError::new("test error"), Location::none());

        assert!(ctx.has_errors());
        assert_eq!(ctx.error_count(), 1);
    }

    #[test]
    fn test_diagnostics_context_is_send() {
        fn assert_send<T: Send>() {}
        assert_send::<DiagnosticsContext>();
    }
}
