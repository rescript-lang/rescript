use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use super::file_args::{BuildCommandStateExt, TypecheckArgs};
use crate::build::build_types::{BuildCommandState, CompileMode, OutputTarget};
use crate::build::diagnostics::{self, BscDiagnostic};

/// Typecheck a single file by piping unsaved content to `bsc -bs-read-stdin`.
///
/// Looks up the module in `build_state`, computes compiler args, then
/// delegates to [`typecheck_with_args`].
pub fn run(build_state: &BuildCommandState, file_path: &Path, content: &str) -> Vec<BscDiagnostic> {
    let args = match build_state.get_typecheck_args(
        file_path,
        content,
        OutputTarget::Lsp,
        CompileMode::TypecheckOnly,
    ) {
        Some(args) => args,
        None => {
            tracing::warn!(
                path = %file_path.display(),
                "typecheck: no module found for file"
            );
            return Vec::new();
        }
    };

    typecheck_with_args(&args, content)
}

/// Typecheck a single file from pre-computed [`TypecheckArgs`].
///
/// Pipes `content` to `bsc -bs-read-stdin` and returns parsed diagnostics
/// with paths remapped to absolute. Used by both the synchronous fallback
/// path ([`run`]) and the batched [`super::queue`].
pub fn typecheck_with_args(args: &TypecheckArgs, content: &str) -> Vec<BscDiagnostic> {
    // Insert -bs-read-stdin before the last argument (the source file path).
    // bsc reads source from stdin and uses the file path only for error
    // reporting and output prefix derivation.
    let mut full_args = args.compiler_args.clone();
    let source_arg = full_args.pop();
    full_args.push("-bs-read-stdin".into());
    if let Some(arg) = source_arg {
        full_args.push(arg);
    }

    let mut child = match Command::new(&args.bsc_path)
        .current_dir(&args.build_path_abs)
        .args(&full_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(child) => child,
        Err(e) => {
            tracing::warn!("typecheck: failed to spawn bsc: {e}");
            return Vec::new();
        }
    };

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(content.as_bytes());
    }

    let output = match child.wait_with_output() {
        Ok(output) => output,
        Err(e) => {
            tracing::warn!("typecheck: bsc invocation failed: {e}");
            return Vec::new();
        }
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    parse_and_remap_diagnostics(&stderr, &args.source_path, &args.package_path)
}

/// Parse bsc stderr output and remap relative source paths to absolute.
pub fn parse_and_remap_diagnostics(
    stderr: &str,
    source_path: &Path,
    package_path: &Path,
) -> Vec<BscDiagnostic> {
    let original_file = package_path.join(source_path);
    let mut diags = diagnostics::parse_compiler_output(stderr);
    for diag in &mut diags {
        if diag.file == *source_path {
            diag.file = original_file.clone();
        }
    }
    diags
}
