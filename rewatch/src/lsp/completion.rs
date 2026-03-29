use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{CompletionItem, CompletionResponse, Documentation, Position, Url};

use crate::lsp::ProjectMap;
use crate::lsp::analysis;

/// Handle a completion request: resolve the source buffer, build analysis context
/// under lock, then spawn the analysis binary after releasing the lock.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    projects: &Mutex<ProjectMap>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<CompletionResponse> {
    let source = analysis::resolve_source(open_buffers, file_path, uri, "completion")?;

    let ctx = {
        let mut guard = projects.lock().ok()?;
        guard.build_analysis_context(uri, file_path, &source, position, true, None)?
    };

    let _span = tracing::info_span!(
        "lsp.completion",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    let stdout = ctx.spawn(&["rewatch", "completion"])?;
    let items: Vec<CompletionItem> = serde_json::from_str(&stdout).ok()?;
    Some(CompletionResponse::Array(items))
}

/// Handle a completionItem/resolve request: extract modulePath from the item's
/// data field, call the analysis binary to fetch the module docstring, and
/// enrich the item with documentation.
pub fn handle_resolve(projects: &Mutex<ProjectMap>, mut item: CompletionItem) -> CompletionItem {
    let Some(data) = &item.data else {
        return item;
    };
    let module_path = data.get("modulePath").and_then(|v| v.as_str());
    let file_path_str = data.get("filePath").and_then(|v| v.as_str());
    let Some((module_path, file_path_str)) = module_path.zip(file_path_str) else {
        return item;
    };
    let file_path = Path::new(file_path_str);

    let uri = match Url::from_file_path(file_path) {
        Ok(u) => u,
        Err(_) => return item,
    };

    let module_path_owned = module_path.to_string();

    let ctx = {
        let mut guard = match projects.lock() {
            Ok(g) => g,
            Err(_) => return item,
        };
        match guard.build_analysis_context(
            &uri,
            file_path,
            "",
            Position {
                line: 0,
                character: 0,
            },
            false,
            Some(&|map: &mut serde_json::Map<String, serde_json::Value>| {
                map.insert(
                    "modulePath".to_string(),
                    serde_json::Value::String(module_path_owned.clone()),
                );
            }),
        ) {
            Some(ctx) => ctx,
            None => return item,
        }
    };

    let _span = tracing::info_span!(
        "lsp.completion_resolve",
        file = %file_path.display(),
        module = %ctx.module_name,
        package = %ctx.package_name,
    )
    .entered();

    if let Some(stdout) = ctx.spawn(&["rewatch", "completionResolve"])
        && let Ok(doc) = serde_json::from_str::<String>(&stdout)
    {
        item.documentation = Some(Documentation::String(doc));
    }
    item
}
