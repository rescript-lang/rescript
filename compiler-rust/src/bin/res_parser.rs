//! ReScript Parser CLI
//!
//! This CLI provides a drop-in replacement for the OCaml `res_parser` CLI,
//! enabling the Rust parser to run the existing syntax tests.
//!
//! Uses the same single-dash CLI format as the OCaml parser for compatibility.
//!
//! Usage:
//!   res_parser_rust [options] <file>
//!
//! Options:
//!   -print <format>         Output format: ml, res, sexp, sexp-locs, sexp0-locs, binary, binary0, ast, comments, tokens
//!   -recover                Enable error recovery mode
//!   -interface              Parse as interface file
//!   -width <n>              Line width for printer (default: 100)
//!   -jsx-version <n>        JSX version (3 or 4)
//!   -jsx-module <mod>       JSX module (default: react)
//!   -test-ast-conversion    Test AST conversion roundtrip (parsetree -> parsetree0 -> parsetree)
//!   -typechecker            Parse for typechecker (not printer)

use clap::Parser as ClapParser;
use rescript_compiler::binary_ast::{mapper_from0, mapper_to0, Marshal, MarshalWriter};
use rescript_compiler::parser::{
    code_frame, jsx_ppx, ml_printer, module, print_signature, printer, printer2, sexp, sexp_locs, sexp_locs0, Parser,
    ParserMode, Scanner, SignatureItem, Structure,
};
use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process;

/// A Write adapter that collects UTF-8 output and stores it as chars in a String.
/// When the ML printer writes chars as UTF-8, this captures them and stores the
/// decoded chars, preserving the original Latin-1 encoding from the source.
struct StringWriter(String);

impl StringWriter {
    fn new() -> Self {
        StringWriter(String::new())
    }

    fn into_string(self) -> String {
        self.0
    }
}

impl Write for StringWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // buf contains UTF-8 encoded bytes from write! macro
        // Convert back to a &str (which is what was originally written)
        match std::str::from_utf8(buf) {
            Ok(s) => {
                self.0.push_str(s);
                Ok(buf.len())
            }
            Err(_) => {
                // Fallback: treat as Latin-1 bytes
                for &b in buf {
                    self.0.push(b as char);
                }
                Ok(buf.len())
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// ReScript Parser CLI - for testing purposes only
#[derive(ClapParser, Debug)]
#[command(name = "res_parser_rust")]
#[command(about = "ReScript parser CLI for testing - matches OCaml res_parser interface")]
struct Args {
    /// Input file to parse
    file: PathBuf,

    /// Output format: ml, res, sexp, ast, comments, tokens (default: res)
    #[arg(short = 'p', long = "print", default_value = "res")]
    print: String,

    /// Enable error recovery mode
    #[arg(short = 'r', long = "recover")]
    recover: bool,

    /// Parse as interface file (.resi)
    #[arg(short = 'i', long = "interface")]
    interface: bool,

    /// Line width for printer
    #[arg(short = 'w', long = "width", default_value = "100")]
    width: usize,

    /// JSX version (3 or 4)
    #[arg(long = "jsx-version", default_value = "-1")]
    jsx_version: i32,

    /// JSX module
    #[arg(long = "jsx-module", default_value = "react")]
    jsx_module: String,

    /// Parse for typechecker (not printer)
    #[arg(long = "typechecker")]
    typechecker: bool,

    /// Test AST conversion
    #[arg(long = "test-ast-conversion")]
    test_ast_conversion: bool,
}

/// Convert OCaml-style single-dash long options to GNU-style double-dash options.
/// This allows the same CLI to work with scripts written for the OCaml parser.
/// e.g., "-print" -> "--print", "-recover" -> "--recover"
fn normalize_args() -> Vec<String> {
    std::env::args()
        .map(|arg| {
            // Convert OCaml-style options to GNU-style
            match arg.as_str() {
                "-print" => "--print".to_string(),
                "-recover" => "--recover".to_string(),
                "-interface" => "--interface".to_string(),
                "-width" => "--width".to_string(),
                "-jsx-version" => "--jsx-version".to_string(),
                "-jsx-module" => "--jsx-module".to_string(),
                "-typechecker" => "--typechecker".to_string(),
                "-test-ast-conversion" => "--test-ast-conversion".to_string(),
                _ => arg,
            }
        })
        .collect()
}

fn main() {
    let args = Args::parse_from(normalize_args());

    // Read file as raw bytes, converting each byte to a char (Latin-1 encoding).
    // This preserves the original byte sequence through the compilation pipeline,
    // matching OCaml's behavior where strings are byte sequences, not Unicode.
    let source = match fs::read(&args.file) {
        Ok(bytes) => bytes.iter().map(|&b| b as char).collect::<String>(),
        Err(e) => {
            eprintln!("Error reading file {:?}: {}", args.file, e);
            process::exit(1);
        }
    };

    let filename = args.file.to_string_lossy().to_string();

    // Determine if we're parsing an interface
    let is_interface = args.interface || filename.ends_with(".resi") || filename.ends_with(".mli");

    // Determine parser mode based on output format
    // For sexp/res output (printer), use Default mode where strings get None delimiter
    // For ML output and type checking, use ParseForTypeChecker mode where strings get Some("js") delimiter
    // This matches OCaml's for_printer flag behavior
    let mode = match args.print.as_str() {
        "sexp" | "sexp-locs" | "sexp0-locs" | "res" | "ast" => ParserMode::Default,
        "ml" | _ => ParserMode::ParseForTypeChecker,
    };

    // Parse the file
    let mut parser = Parser::with_mode(&filename, &source, mode);

    // Parse structure or signature
    if is_interface {
        let signature = module::parse_signature(&mut parser);

        // Print diagnostics if any
        if parser.has_errors() {
            print_diagnostics(&parser, &source, &filename, &mut io::stdout());
            if !args.recover {
                process::exit(1);
            }
        }

        // Apply JSX PPX transformation
        let signature = jsx_ppx::transform_signature(signature, args.jsx_version, parser.arena_mut());

        // Apply AST conversion round-trip if requested (for testing)
        let signature = if args.test_ast_conversion {
            let sig0 = mapper_to0::map_signature(&signature);
            mapper_from0::map_signature(&sig0)
        } else {
            signature
        };

        // Print in requested format
        match args.print.as_str() {
            "ml" => {
                // Capture ML output using StringWriter, then convert chars to Latin-1 bytes.
                // This preserves the original file bytes through the round-trip.
                let mut writer = StringWriter::new();
                ml_printer::print_signature_ml(&signature, &mut writer);
                let output = writer.into_string();
                let bytes: Vec<u8> = output.chars().map(|c| c as u8).collect();
                let _ = io::stdout().write_all(&bytes);
            }
            "res" => print_signature_res(&signature, &mut io::stdout()),
            "sexp" => sexp::print_signature(&signature, &mut io::stdout()),
            "sexp-locs" => sexp_locs::print_signature(&signature, &mut io::stdout()),
            "sexp0-locs" => {
                let sig0 = mapper_to0::map_signature(&signature);
                sexp_locs0::print_signature(&sig0, &mut io::stdout());
            }
            "ast" => print_signature_ast(&signature, &mut io::stdout()),
            "binary" => {
                // OCaml -print binary format (current parsetree):
                // 1. Magic: "Caml1999N022" (raw string, not marshaled) for interfaces
                // 2. Marshaled filename (using output_value)
                // 3. Marshaled signature (using output_value)

                // Write magic
                let _ = io::stdout().write_all(b"Caml1999N022");

                // Marshal filename
                let mut w = MarshalWriter::new();
                filename.marshal(&mut w);
                let _ = io::stdout().write_all(&w.finish());

                // Marshal signature
                let mut w = MarshalWriter::new();
                signature.marshal(&mut w);
                let _ = io::stdout().write_all(&w.finish());
            }
            "binary0" => {
                // Parsetree0 binary format (frozen PPX-compatible format):
                // Same layout as binary but with parsetree0 types

                // Convert to parsetree0
                let sig0 = mapper_to0::map_signature(&signature);

                // Write magic
                let _ = io::stdout().write_all(b"Caml1999N022");

                // Marshal filename
                let mut w = MarshalWriter::new();
                filename.marshal(&mut w);
                let _ = io::stdout().write_all(&w.finish());

                // Marshal parsetree0 signature
                let mut w = MarshalWriter::new();
                sig0.marshal(&mut w);
                let _ = io::stdout().write_all(&w.finish());
            }
            "comments" => print_comments(&parser, &mut io::stdout()),
            "tokens" => print_tokens(&source, &filename, &mut io::stdout()),
            _ => {
                eprintln!("Unknown print format: {}", args.print);
                process::exit(1);
            }
        }
    } else {
        let structure = module::parse_structure(&mut parser);

        // Print diagnostics if any
        if parser.has_errors() {
            print_diagnostics(&parser, &source, &filename, &mut io::stdout());
            if !args.recover {
                process::exit(1);
            }
        }

        // Apply JSX PPX transformation
        let structure = jsx_ppx::transform_structure(structure, args.jsx_version, parser.arena_mut());

        // Apply AST conversion round-trip if requested (for testing)
        let structure = if args.test_ast_conversion {
            let str0 = mapper_to0::map_structure(&structure);
            mapper_from0::map_structure(&str0)
        } else {
            structure
        };

        // Print in requested format
        match args.print.as_str() {
            "ml" => {
                // Capture ML output using StringWriter, then convert chars to Latin-1 bytes.
                // This preserves the original file bytes through the round-trip.
                let mut writer = StringWriter::new();
                ml_printer::print_structure_ml(&structure, &mut writer);
                let output = writer.into_string();
                let bytes: Vec<u8> = output.chars().map(|c| c as u8).collect();
                let _ = io::stdout().write_all(&bytes);
            }
            "res" => {
                let output =
                    printer2::print_structure_with_comments(&structure, parser.comments().to_vec());
                if !output.is_empty() {
                    // Convert Latin-1 chars back to bytes to preserve original file encoding
                    let bytes: Vec<u8> = output.chars().map(|c| c as u8).collect();
                    let _ = io::stdout().write_all(&bytes);
                }
            }
            "sexp" => sexp::print_structure(&structure, &mut io::stdout()),
            "sexp-locs" => sexp_locs::print_structure(&structure, &mut io::stdout()),
            "sexp0-locs" => {
                let str0 = mapper_to0::map_structure(&structure);
                sexp_locs0::print_structure(&str0, &mut io::stdout());
            }
            "ast" => print_structure_ast(&structure, &mut io::stdout()),
            "binary" => {
                // OCaml -print binary format (current parsetree):
                // 1. Magic: "Caml1999M022" (raw string, not marshaled) for implementations
                // 2. Marshaled filename (using output_value)
                // 3. Marshaled structure (using output_value)

                // Write magic
                let _ = io::stdout().write_all(b"Caml1999M022");

                // Marshal filename
                let mut w = MarshalWriter::new();
                filename.marshal(&mut w);
                let _ = io::stdout().write_all(&w.finish());

                // Marshal structure
                let mut w = MarshalWriter::new();
                structure.marshal(&mut w);
                let _ = io::stdout().write_all(&w.finish());
            }
            "binary0" => {
                // Parsetree0 binary format (frozen PPX-compatible format):
                // Same layout as binary but with parsetree0 types

                // Convert to parsetree0
                let str0 = mapper_to0::map_structure(&structure);

                // Write magic
                let _ = io::stdout().write_all(b"Caml1999M022");

                // Marshal filename
                let mut w = MarshalWriter::new();
                filename.marshal(&mut w);
                let _ = io::stdout().write_all(&w.finish());

                // Marshal parsetree0 structure
                let mut w = MarshalWriter::new();
                str0.marshal(&mut w);
                let _ = io::stdout().write_all(&w.finish());
            }
            "comments" => print_comments(&parser, &mut io::stdout()),
            "tokens" => print_tokens(&source, &filename, &mut io::stdout()),
            _ => {
                eprintln!("Unknown print format: {}", args.print);
                process::exit(1);
            }
        }
    }
}

/// Print parser diagnostics in OCaml-compatible format
fn print_diagnostics(parser: &Parser, source: &str, filename: &str, out: &mut impl Write) {
    for diag in parser.diagnostics() {
        let error_msg = code_frame::format_error(
            filename,
            source,
            &diag.start_pos,
            &diag.end_pos,
            &diag.explain(),
        );
        let _ = out.write_all(error_msg.as_bytes());
    }
}

/// Print comments from parser
fn print_comments(parser: &Parser, out: &mut impl Write) {
    for comment in parser.comments() {
        let _ = writeln!(out, "{:?}", comment);
    }
}

/// Print tokens from source
fn print_tokens(source: &str, filename: &str, out: &mut impl Write) {
    let mut scanner = Scanner::new(filename, source);
    loop {
        let result = scanner.scan();
        let start_col = result.start_pos.cnum - result.start_pos.bol;
        let end_col = result.end_pos.cnum - result.end_pos.bol;
        let _ = writeln!(
            out,
            "{:?} @ {}:{}-{}:{}",
            result.token,
            result.start_pos.line,
            start_col + 1,
            result.end_pos.line,
            end_col + 1
        );
        if matches!(result.token, rescript_compiler::parser::Token::Eof) {
            break;
        }
    }
}

// ============================================================================
// ReScript Printer
// ============================================================================

fn print_signature_res(signature: &[SignatureItem], out: &mut impl Write) {
    // Use the built-in printer from the parser module
    let output = print_signature(signature);
    if !output.is_empty() {
        // Convert Latin-1 chars back to bytes to preserve original file encoding
        let bytes: Vec<u8> = output.chars().map(|c| c as u8).collect();
        let _ = out.write_all(&bytes);
        let _ = out.write_all(b"\n");
    }
}

// NOTE: S-expression printer moved to src/parser/sexp.rs for byte-for-byte
// parity with OCaml res_ast_debugger.ml output

// ============================================================================
// AST Debug Printer
// ============================================================================

fn print_structure_ast(structure: &Structure, out: &mut impl Write) {
    let _ = writeln!(out, "{:#?}", structure);
}

fn print_signature_ast(signature: &[SignatureItem], out: &mut impl Write) {
    let _ = writeln!(out, "{:#?}", signature);
}
