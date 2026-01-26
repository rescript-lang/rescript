//! Compiler driver utilities.
//!
//! This module wires together parsing, type-checking, Lambda conversion, and
//! JS IR codegen to produce JavaScript output.

use anyhow::{anyhow, Context, Result};

use crate::context::IdGenerator;
use crate::js_ir::printer::print_program;
use crate::lambda::compile::LambdaCompiler;
use crate::lambda::convert::LambdaConverter;
use crate::parser::{module, Parser};
use crate::types::sexp_typedtree::{print_typed_structure, print_typed_structure_with_locs};
use crate::types::{initial_env, type_structure, TypeCheckContext, TypeContext};

/// Compiler options that can be passed to the driver.
#[derive(Default, Clone)]
pub struct CompilerOptions {
    /// Print the Lambda IR to stderr (like OCaml's -drawlambda)
    pub dump_lambda: bool,
    /// Print the typed tree as sexp to stderr (like OCaml's -dtyped-sexp)
    pub dump_typed_sexp: bool,
    /// Print the typed tree as sexp with locations to stderr (like OCaml's -dtyped-sexp-locs)
    pub dump_typed_sexp_locs: bool,
}

/// Compile ReScript source to JavaScript.
pub fn compile_source_to_js(source: &str, filename: &str) -> Result<String> {
    compile_source_to_js_with_options(source, filename, &CompilerOptions::default())
}

/// Compile ReScript source to JavaScript with options.
pub fn compile_source_to_js_with_options(
    source: &str,
    filename: &str,
    options: &CompilerOptions,
) -> Result<String> {
    // Parse
    let mut parser = Parser::new(filename, source);
    let structure = module::parse_structure(&mut parser);
    if parser.has_errors() {
        return Err(anyhow!("Parse failed with {} error(s)", parser.diagnostics().len()));
    }

    // Type check
    let id_gen = IdGenerator::new();
    let type_ctx = TypeContext::new(&id_gen);
    let mut tctx = TypeCheckContext::new(&type_ctx);
    let env = initial_env(&type_ctx);
    let (typed_structure, _env) = type_structure(&mut tctx, &env, &structure)
        .map_err(|e| anyhow!("Type checking failed: {e:?}"))?;

    // Print typed tree if requested
    if options.dump_typed_sexp {
        let mut stderr = std::io::stderr();
        let _ = print_typed_structure(&type_ctx, &typed_structure, &mut stderr);
    }
    if options.dump_typed_sexp_locs {
        let mut stderr = std::io::stderr();
        let _ = print_typed_structure_with_locs(&type_ctx, &typed_structure, &mut stderr);
    }

    // Typedtree -> Lambda
    let mut converter = LambdaConverter::new();
    let lambda = converter.convert_structure(&typed_structure);

    // Print Lambda IR if requested
    if options.dump_lambda {
        eprintln!("{}", crate::lambda::print::LambdaPrinter::new(&lambda));
    }

    // Lambda -> JS IR
    let mut compiler = LambdaCompiler::new();
    let program = compiler.compile_program(&lambda);

    // JS IR -> JS source
    Ok(print_program(&program))
}

/// Compile a file to JavaScript.
pub fn compile_file_to_js(path: &std::path::Path) -> Result<String> {
    compile_file_to_js_with_options(path, &CompilerOptions::default())
}

/// Compile a file to JavaScript with options.
pub fn compile_file_to_js_with_options(
    path: &std::path::Path,
    options: &CompilerOptions,
) -> Result<String> {
    // Use lossy UTF-8 conversion (like OCaml) to handle files with invalid bytes
    let bytes = std::fs::read(path)
        .with_context(|| format!("Failed to read {}", path.display()))?;
    let source = String::from_utf8_lossy(&bytes).into_owned();
    let filename = path.to_string_lossy();
    compile_source_to_js_with_options(&source, &filename, options)
}
