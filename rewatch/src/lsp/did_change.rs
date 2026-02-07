use std::path::Path;
use std::process::Command;

use tracing::instrument;

use crate::build::build_types::{BuildCommandState, BuildProfile, SourceType};
use crate::build::compile;
use crate::build::diagnostics::{self, BscDiagnostic};

/// Run a single-file typecheck after an unsaved edit (didChange).
///
/// Writes the unsaved buffer content to a temp `.res` file in the package's
/// LSP build directory, invokes `bsc` directly with `-bs-cmi-only`, and parses
/// diagnostics from stderr. No JS output is produced. Dependents are not
/// recompiled — that happens on `didSave`.
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

    let build_path = package.get_build_path_for_profile(BuildProfile::TypecheckOnly);

    // Write unsaved content to a temp file in the build directory.
    // Use the source's relative path so that `-I` includes resolve correctly
    // when bsc runs with current_dir set to the build path.
    let source_path = if is_interface {
        source_file.interface.as_ref().map(|i| &i.path)
    } else {
        Some(&source_file.implementation.path)
    };
    let source_path = match source_path {
        Some(p) => p,
        None => return Vec::new(),
    };

    let temp_file = build_path.join(source_path);
    if let Some(parent) = temp_file.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    if std::fs::write(&temp_file, content).is_err() {
        tracing::warn!("didChange: failed to write temp file {}", temp_file.display());
        return Vec::new();
    }

    let has_interface = source_file.interface.is_some();

    // Build compiler args — the temp file path goes as the last arg (replaces ast_path).
    // bsc accepts .res files directly (parses + typechecks in one shot).
    let args = match compile::compiler_args(
        &package.config,
        &temp_file,
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

    let result = Command::new(&build_state.build_state.compiler_info.bsc_path)
        .current_dir(&build_path_abs)
        .args(&args)
        .output();

    // Clean up temp file
    let _ = std::fs::remove_file(&temp_file);

    // The original source file path (absolute) for remapping diagnostics.
    let original_file = package.path.join(source_path);

    match result {
        Ok(output) => {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let mut diags = diagnostics::parse_compiler_output(&stderr);
            // bsc outputs diagnostics with the temp file path. Remap them
            // back to the original source file so the editor highlights the
            // correct buffer.
            for diag in &mut diags {
                if diag.file == temp_file || diag.file == *source_path {
                    diag.file = original_file.clone();
                }
            }
            diags
        }
        Err(e) => {
            tracing::warn!("didChange: bsc invocation failed: {e}");
            Vec::new()
        }
    }
}
