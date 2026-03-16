use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{CodeActionOrCommand, CodeActionResponse, Position, Range, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle a code action request.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    range: Range,
) -> Option<CodeActionResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "code_action")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(
            uri,
            file_path,
            &source,
            Position {
                line: range.start.line,
                character: range.start.character,
            },
            true,
            Some(&|map: &mut serde_json::Map<String, serde_json::Value>| {
                map.insert(
                    "endPos".to_string(),
                    serde_json::json!([range.end.line, range.end.character]),
                );
            }),
        )?
    };

    let _span = tracing::info_span!(
        "lsp.code_action",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "codeAction"])?;
    let actions: Vec<CodeActionOrCommand> = serde_json::from_str(&stdout).ok()?;
    if actions.is_empty() { None } else { Some(actions) }
}
