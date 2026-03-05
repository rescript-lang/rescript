use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use serde_json::json;
use tower_lsp::lsp_types::{Position, PrepareRenameResponse, Url, WorkspaceEdit};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle a prepareRename request.
pub fn handle_prepare_rename(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<PrepareRenameResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "prepare_rename")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(uri, file_path, &source, position, true, None)?
    };

    let _span = tracing::info_span!(
        "lsp.prepare_rename",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "prepareRename"])?;
    serde_json::from_str::<PrepareRenameResponse>(&stdout).ok()
}

/// Handle a rename request.
pub fn handle_rename(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    position: Position,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "rename")?;

    let new_name = new_name.to_string();
    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(
            uri,
            file_path,
            &source,
            position,
            true,
            Some(&|map: &mut serde_json::Map<String, serde_json::Value>| {
                map.insert("newName".to_string(), json!(new_name));
            }),
        )?
    };

    let _span = tracing::info_span!(
        "lsp.rename",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "rename"])?;
    serde_json::from_str::<WorkspaceEdit>(&stdout).ok()
}
