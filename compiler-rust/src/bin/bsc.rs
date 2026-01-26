//! Minimal Rust bsc replacement.
//!
//! This is a lightweight CLI that supports:
//! - `-bs-ast` parsing (produces a simple .ast file)
//! - compiling a .res/.resi (or .ast) file to JS using the Rust pipeline
//! - `-bs-package-output` for determining output directories
//! - `-bs-ns` for namespace suffix in module names
//!
//! All other flags are accepted and ignored.

use anyhow::{anyhow, Context, Result};
use rescript_compiler::binary_ast::{write_signature_ast, write_structure_ast};
use rescript_compiler::driver::{compile_file_to_js_with_options, CompilerOptions};
use rescript_compiler::parser::{module, Parser};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Default, Debug)]
struct Options {
    bs_ast: bool,
    dump_lambda: bool,
    dump_typed_sexp: bool,
    dump_typed_sexp_locs: bool,
    output: Option<PathBuf>,
    package_outputs: Vec<String>,
    namespace: Option<String>,
    module_name: Option<String>,
    input: Option<PathBuf>,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let mut opts = Options::default();
    let mut args = env::args().skip(1).peekable();

    while let Some(arg) = args.next() {
        if arg.starts_with('-') {
            match arg.as_str() {
                "-bs-ast" => opts.bs_ast = true,
                "-drawlambda" => opts.dump_lambda = true,
                "-dtyped-sexp" => opts.dump_typed_sexp = true,
                "-dtyped-sexp-locs" => opts.dump_typed_sexp_locs = true,
                "-o" | "-output" => {
                    if let Some(val) = args.next() {
                        opts.output = Some(PathBuf::from(val));
                    }
                }
                "-bs-package-output" => {
                    if let Some(val) = args.next() {
                        opts.package_outputs.push(val);
                    }
                }
                "-bs-ns" => {
                    if let Some(val) = args.next() {
                        opts.namespace = Some(val);
                    }
                }
                "-bs-module-name" => {
                    if let Some(val) = args.next() {
                        opts.module_name = Some(val);
                    }
                }
                "-impl" | "-intf" => {
                    if let Some(val) = args.next() {
                        opts.input = Some(PathBuf::from(val));
                    }
                }
                "-I"
                | "-open"
                | "-absname"
                | "-w"
                | "-warn-error"
                | "-color"
                | "-bs-package-name"
                | "-bs-read-cmi"
                | "-pp"
                | "-ppx"
                | "-ppxopt"
                | "-no-alias-deps"
                | "-bs-no-implicit-stdlib"
                | "-unsafe"
                | "-bs-super-errors"
                | "-bs-stop-after-cmj"
                | "-bs-no-check-cmi" => {
                    // Consume the next argument if present (value flags).
                    let _ = args.next();
                }
                _ => {
                    // Unknown flag: ignore, but if it looks like a value flag, try to skip.
                }
            }
        } else {
            opts.input = Some(PathBuf::from(arg));
        }
    }

    let input = opts
        .input
        .clone()
        .ok_or_else(|| anyhow!("Missing input file"))?;

    if opts.bs_ast {
        return generate_ast(&input, opts.output);
    }

    let source_path = if input.extension().and_then(|s| s.to_str()) == Some("ast") {
        read_ast_source_path(&input)?
    } else {
        input
    };

    let compiler_opts = CompilerOptions {
        dump_lambda: opts.dump_lambda,
        dump_typed_sexp: opts.dump_typed_sexp,
        dump_typed_sexp_locs: opts.dump_typed_sexp_locs,
    };
    let js = compile_file_to_js_with_options(&source_path, &compiler_opts)?;
    write_outputs(&source_path, &js, &opts)
}

fn generate_ast(input: &Path, output: Option<PathBuf>) -> Result<()> {
    // Use lossy UTF-8 conversion (like OCaml) to handle files with invalid bytes
    let bytes =
        fs::read(input).with_context(|| format!("Failed to read {}", input.display()))?;
    let source = String::from_utf8_lossy(&bytes).into_owned();

    // Use the original path as provided (matching OCaml's behavior)
    // OCaml does not canonicalize paths - it uses them as-is
    let source_path = input.to_string_lossy().to_string();

    let filename = input.to_string_lossy().to_string();

    // Determine if this is an interface (.resi) or implementation (.res)
    let is_interface = input
        .extension()
        .and_then(|s| s.to_str())
        .is_some_and(|ext| ext == "resi" || ext == "mli");

    // Determine output path and extension
    let out_path = output.unwrap_or_else(|| {
        if is_interface {
            input.with_extension("iast")
        } else {
            input.with_extension("ast")
        }
    });

    if let Some(parent) = out_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let mut parser = Parser::new(filename, &source);

    if is_interface {
        // Parse signature (interface file)
        let signature = module::parse_signature(&mut parser);
        if parser.has_errors() {
            return Err(anyhow!(
                "Parse failed with {} error(s)",
                parser.diagnostics().len()
            ));
        }
        write_signature_ast(&out_path, &source_path, &signature)
            .with_context(|| format!("Failed to write {}", out_path.display()))?;
    } else {
        // Parse structure (implementation file)
        let structure = module::parse_structure(&mut parser);
        if parser.has_errors() {
            return Err(anyhow!(
                "Parse failed with {} error(s)",
                parser.diagnostics().len()
            ));
        }
        write_structure_ast(&out_path, &source_path, &structure)
            .with_context(|| format!("Failed to write {}", out_path.display()))?;
    }

    Ok(())
}

fn read_ast_source_path(ast_path: &Path) -> Result<PathBuf> {
    let content =
        fs::read(ast_path).with_context(|| format!("Failed to read {}", ast_path.display()))?;

    if content.len() < 5 {
        return Err(anyhow!("Invalid .ast file: too small"));
    }

    // Read the dependency section size (4 bytes, big-endian)
    let dep_size = u32::from_be_bytes([content[0], content[1], content[2], content[3]]) as usize;

    // Skip past the dependency section to get to the source path
    let path_start = 4 + dep_size;
    if path_start >= content.len() {
        return Err(anyhow!("Invalid .ast file: missing source path"));
    }

    // Read until newline to get the source path
    let path_end = content[path_start..]
        .iter()
        .position(|&b| b == b'\n')
        .map(|pos| path_start + pos)
        .unwrap_or(content.len());

    let path_bytes = &content[path_start..path_end];
    let path_str = String::from_utf8_lossy(path_bytes);

    if path_str.is_empty() {
        return Err(anyhow!("Invalid .ast file: empty source path"));
    }

    Ok(PathBuf::from(path_str.into_owned()))
}

fn write_outputs(source_path: &Path, js: &str, opts: &Options) -> Result<()> {
    if let Some(out) = &opts.output {
        if let Some(parent) = out.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(out, js)?;
        return Ok(());
    }

    if !opts.package_outputs.is_empty() {
        for spec in &opts.package_outputs {
            if let Some((dir, suffix)) = parse_package_output(spec) {
                let module_name = module_name_for_file(
                    source_path,
                    opts.namespace.as_deref(),
                    opts.module_name.as_deref(),
                );
                let out_path = Path::new(&dir).join(format!("{module_name}{suffix}"));
                if let Some(parent) = out_path.parent() {
                    fs::create_dir_all(parent)?;
                }
                fs::write(out_path, js)?;
            }
        }
        return Ok(());
    }

    let out = source_path.with_extension("js");
    if let Some(parent) = out.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(out, js)?;
    Ok(())
}

fn parse_package_output(spec: &str) -> Option<(String, String)> {
    let mut parts = spec.splitn(3, ':');
    let _module = parts.next()?;
    let dir = parts.next()?.to_string();
    let suffix = parts.next()?.to_string();
    Some((dir, suffix))
}

fn module_name_for_file(path: &Path, ns: Option<&str>, override_name: Option<&str>) -> String {
    if let Some(name) = override_name {
        return name.to_string();
    }
    let stem = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Module");
    let base = if let Some(ns) = ns {
        if ns.is_empty() {
            stem.to_string()
        } else {
            format!("{stem}-{ns}")
        }
    } else {
        stem.to_string()
    };
    capitalize(&base)
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}
