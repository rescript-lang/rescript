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
use crate::types::{initial_env, type_structure, TypeCheckContext, TypeContext};

/// Compile ReScript source to JavaScript.
pub fn compile_source_to_js(source: &str, filename: &str) -> Result<String> {
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

    // Typedtree -> Lambda
    let mut converter = LambdaConverter::new();
    let lambda = converter.convert_structure(&typed_structure);

    // Lambda -> JS IR
    let mut compiler = LambdaCompiler::new();
    let program = compiler.compile_program(&lambda);

    // JS IR -> JS source
    Ok(print_program(&program))
}

/// Compile a file to JavaScript.
pub fn compile_file_to_js(path: &std::path::Path) -> Result<String> {
    // Use lossy UTF-8 conversion (like OCaml) to handle files with invalid bytes
    let bytes = std::fs::read(path)
        .with_context(|| format!("Failed to read {}", path.display()))?;
    let source = String::from_utf8_lossy(&bytes).into_owned();
    let filename = path.to_string_lossy();
    compile_source_to_js(&source, &filename)
}
