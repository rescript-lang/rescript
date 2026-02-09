use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use serde_json::json;
use tower_lsp::lsp_types::{Position, PrepareRenameResponse, Url, WorkspaceEdit};
use tracing::instrument;

use crate::build::build_types::BuildCommandState;
use crate::lsp::analysis;

/// Handle a prepareRename request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle_prepare_rename(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<PrepareRenameResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "prepare_rename")?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run_prepare_rename(build_state, file_path, &source, position)
}

/// Handle a rename request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle_rename(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    position: Position,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "rename")?;

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run_rename(build_state, file_path, &source, position, new_name)
}

#[instrument(name = "lsp.prepare_rename", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run_prepare_rename(
    build_state: &BuildCommandState,
    file_path: &Path,
    source: &str,
    position: Position,
) -> Option<PrepareRenameResponse> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    {
        let _guard = tracing::info_span!("lsp.prepare_rename.ensure_cmt").entered();
        analysis::ensure_cmt(build_state, package, source_file, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.prepare_rename.build_context").entered();
        analysis::build_context_json(
            build_state,
            source,
            &path_str,
            position,
            &root_path,
            &package.namespace,
            &package.config,
            root_config,
        )
    };

    let _guard = tracing::info_span!("lsp.prepare_rename.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "prepareRename"], &json_blob)?;

    serde_json::from_str::<PrepareRenameResponse>(&stdout).ok()
}

#[instrument(name = "lsp.rename", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run_rename(
    build_state: &BuildCommandState,
    file_path: &Path,
    source: &str,
    position: Position,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let (module_name, package_name, package, source_file) = analysis::resolve_module(build_state, file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let original_file = analysis::original_path(package, source_file);
    let path_str = original_file.to_string_lossy();

    {
        let _guard = tracing::info_span!("lsp.rename.ensure_cmt").entered();
        analysis::ensure_cmt(build_state, package, source_file, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let mut json_blob = {
        let _guard = tracing::info_span!("lsp.rename.build_context").entered();
        analysis::build_context_json(
            build_state,
            source,
            &path_str,
            position,
            &root_path,
            &package.namespace,
            &package.config,
            root_config,
        )
    };

    // Add newName to the context blob for the rename endpoint.
    if let serde_json::Value::Object(ref mut map) = json_blob {
        map.insert("newName".to_string(), json!(new_name));
    }

    let _guard = tracing::info_span!("lsp.rename.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "rename"], &json_blob)?;

    serde_json::from_str::<WorkspaceEdit>(&stdout).ok()
}
