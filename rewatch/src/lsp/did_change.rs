use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

use tracing::instrument;

use crate::build::build_types::{BuildCommandState, BuildProfile, SourceType};
use crate::build::compile;
use crate::build::diagnostics::{self, BscDiagnostic};

/// Run a single-file typecheck after an unsaved edit (didChange).
///
/// Pipes the unsaved buffer content to `bsc -bs-read-stdin` which reads
/// source from stdin instead of from the file argument. The file argument
/// is still passed for error locations and module naming. No JS output is
/// produced. Dependents are not recompiled — that happens on `didSave`.
#[instrument(name = "lsp.did_change", skip_all, fields(file = %file_path.display()))]
pub fn run(build_state: &BuildCommandState, file_path: &Path, content: &str) -> Vec<BscDiagnostic> {
    let (module_name, package_name, is_interface) = match build_state.find_module_for_file(file_path) {
        Some(result) => result,
        None => {
            tracing::warn!(
                path = %file_path.display(),
                "didChange: no module found for file"
            );
            return Vec::new();
        }
    };

    let module = match build_state.build_state.modules.get(&module_name) {
        Some(m) => m,
        None => return Vec::new(),
    };

    let package = match build_state.build_state.packages.get(&package_name) {
        Some(p) => p,
        None => return Vec::new(),
    };

    let source_file = match &module.source_type {
        SourceType::SourceFile(sf) => sf,
        _ => return Vec::new(),
    };

    let source_path = if is_interface {
        source_file.interface.as_ref().map(|i| &i.path)
    } else {
        Some(&source_file.implementation.path)
    };
    let source_path = match source_path {
        Some(p) => p,
        None => return Vec::new(),
    };

    let has_interface = source_file.interface.is_some();
    let build_path = package.get_build_path_for_profile(BuildProfile::TypecheckOnly);

    // Build compiler args. The source_path is passed as the last arg — bsc
    // uses it for file kind classification, module naming, and error locations,
    // but with -bs-read-stdin it reads source from stdin instead of from disk.
    let args = match compile::compiler_args(
        &package.config,
        source_path,
        &source_file.implementation.path,
        is_interface,
        has_interface,
        &build_state.build_state.project_context,
        &Some(&build_state.build_state.packages),
        module.is_type_dev,
        package.is_local_dep,
        build_state.get_warn_error_override(),
        BuildProfile::TypecheckOnly,
    ) {
        Ok(args) => args,
        Err(e) => {
            tracing::warn!("didChange: failed to compute compiler args: {e}");
            return Vec::new();
        }
    };

    let build_path_abs = match build_path.canonicalize() {
        Ok(p) => p,
        Err(e) => {
            tracing::warn!("didChange: failed to canonicalize build path: {e}");
            return Vec::new();
        }
    };

    // Insert -bs-read-stdin before the last argument (the source file path).
    // bsc reads source from stdin and uses the file path only for error
    // reporting and output prefix derivation.
    let mut full_args = args;
    let source_arg = full_args.pop();
    full_args.push("-bs-read-stdin".into());
    if let Some(arg) = source_arg {
        full_args.push(arg);
    }

    let mut child = match Command::new(&build_state.build_state.compiler_info.bsc_path)
        .current_dir(&build_path_abs)
        .args(&full_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(child) => child,
        Err(e) => {
            tracing::warn!("didChange: failed to spawn bsc: {e}");
            return Vec::new();
        }
    };

    // Write content to bsc's stdin and close it so bsc can proceed.
    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(content.as_bytes());
    }

    let output = match child.wait_with_output() {
        Ok(output) => output,
        Err(e) => {
            tracing::warn!("didChange: bsc invocation failed: {e}");
            return Vec::new();
        }
    };

    // The original source file path (absolute) for remapping diagnostics.
    // bsc reports errors using the source_path we passed as the last arg,
    // which is relative. Remap to the absolute path for the editor.
    let original_file = package.path.join(source_path);

    let stderr = String::from_utf8_lossy(&output.stderr);
    let mut diags = diagnostics::parse_compiler_output(&stderr);
    for diag in &mut diags {
        if diag.file == *source_path {
            diag.file = original_file.clone();
        }
    }
    diags
}
