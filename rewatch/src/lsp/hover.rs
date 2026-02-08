use std::collections::HashMap;
use std::path::Path;
use std::sync::Mutex;

use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position, Url};
use tracing::instrument;

use crate::build::build_types::{BuildCommandState, BuildProfile, SourceType};
use crate::helpers;
use crate::lsp::analysis;
use crate::lsp::did_change;

/// Handle a hover request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<Hover> {
    let source = open_buffers
        .lock()
        .ok()
        .and_then(|buffers| buffers.get(uri).cloned())
        .or_else(|| std::fs::read_to_string(file_path).ok());

    let source = match source {
        Some(s) => s,
        None => {
            tracing::warn!("hover: no buffer content available");
            return None;
        }
    };

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source, position)
}

/// Run a hover request by shelling out to `rescript-editor-analysis.exe rewatch hover`.
///
/// Builds a JSON blob with all the package/module context the analysis binary needs,
/// pipes it to stdin, and parses the JSON hover response from stdout.
#[instrument(name = "lsp.hover", skip_all, fields(
    file = %file_path.display(),
    module = tracing::field::Empty,
    package = tracing::field::Empty,
))]
fn run(build_state: &BuildCommandState, file_path: &Path, source: &str, position: Position) -> Option<Hover> {
    let (module_name, package_name, _is_interface) = build_state.find_module_for_file(file_path)?;

    let span = tracing::Span::current();
    span.record("module", &module_name);
    span.record("package", &package_name);

    let package = build_state.build_state.packages.get(&package_name)?;

    let module = build_state.build_state.modules.get(&module_name)?;
    let source_file = match &module.source_type {
        SourceType::SourceFile(sf) => sf,
        _ => return None,
    };

    // Determine the original file path (absolute) for the analysis binary.
    let original_file = package.path.join(&source_file.implementation.path);
    let path_str = original_file.to_string_lossy();

    // Ensure a .cmt exists for this module. If the editor triggers hover
    // before any didChange, lib/lsp may not have the .cmt yet.
    let build_path = package.get_build_path_for_profile(BuildProfile::TypecheckOnly);
    let impl_path = &source_file.implementation.path;
    let basename = helpers::file_path_to_compiler_asset_basename(impl_path, &package.namespace);
    let dir = impl_path.parent().unwrap_or(Path::new(""));
    let cmt_path = build_path.join(dir).join(format!("{}.cmt", basename));
    if !cmt_path.exists() {
        let _guard = tracing::info_span!("lsp.hover.ensure_cmt").entered();
        did_change::run(build_state, file_path, source);
    }

    let root_path = package.path.to_string_lossy();
    let root_config = build_state.build_state.get_root_config();

    let json_blob = {
        let _guard = tracing::info_span!("lsp.hover.build_context").entered();
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

    let _guard = tracing::info_span!("lsp.hover.analysis_binary").entered();

    let stdout = analysis::spawn_analysis_binary(build_state, &["rewatch", "hover"], &json_blob)?;

    parse_hover_response(&stdout)
}

/// Parse the JSON output from the analysis binary into an LSP Hover.
///
/// The analysis binary outputs: `{"contents": {"kind": "markdown", "value": "..."}}`
/// or `"null"` when no hover info is available.
fn parse_hover_response(stdout: &str) -> Option<Hover> {
    let json: serde_json::Value = serde_json::from_str(stdout).ok()?;

    let contents = json.get("contents")?;
    let value = contents.get("value")?.as_str()?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: value.to_string(),
        }),
        range: None,
    })
}
