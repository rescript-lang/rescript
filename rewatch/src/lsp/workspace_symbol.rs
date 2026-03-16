use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::Mutex;

use serde_json::{Value, json};
use tower_lsp::lsp_types::SymbolInformation;

use crate::build::build_types::{Module, OutputTarget};
use crate::helpers;
use crate::lsp::ProjectMap;

/// Handle a workspace/symbol request.
///
/// Collects all `.ast`/`.iast` file paths from the build state, sends them
/// to the analysis binary with the query string, and returns matching symbols.
pub fn handle(projects: &Mutex<ProjectMap>, query: &str) -> Option<Vec<SymbolInformation>> {
    let (analysis_binary_path, stdin_json) = {
        let guard = projects.lock().ok()?;
        let first_build_state = guard.all_build_states().next()?;

        let analysis_binary_path = first_build_state
            .build_state
            .compiler_info
            .bsc_path
            .parent()?
            .join("rescript-editor-analysis.exe");

        let mut files: Vec<Value> = Vec::new();

        for build_state in guard.all_build_states() {
            for (module_name, module) in &build_state.build_state.modules {
                let Module::SourceFile(sf_module) = module else {
                    continue;
                };
                // Only include modules that have been parsed — their .ast files exist.
                let stage = sf_module.compilation_stage();
                if stage.is_source_dirty() || stage.is_parse_error() {
                    continue;
                }
                let Some(package) = build_state.build_state.packages.get(&sf_module.package_name) else {
                    continue;
                };

                let build_path = package.get_build_path_for_output(OutputTarget::Lsp);
                let impl_path = &sf_module.source_file.implementation.path;
                let basename = helpers::file_path_to_compiler_asset_basename(impl_path, &package.namespace);
                let dir = impl_path.parent().unwrap_or(Path::new(""));

                if let Some(interface) = &sf_module.source_file.interface {
                    let iface_path = &interface.path;
                    let iface_basename =
                        helpers::file_path_to_compiler_asset_basename(iface_path, &package.namespace);
                    let iface_dir = iface_path.parent().unwrap_or(Path::new(""));
                    let iast_path = build_path.join(iface_dir).join(format!("{iface_basename}.iast"));
                    let source_path = package.path.join(iface_path);
                    files.push(json!({
                        "moduleName": module_name,
                        "astPath": iast_path.to_string_lossy(),
                        "sourcePath": source_path.to_string_lossy(),
                        "isInterface": true,
                    }));
                } else {
                    let ast_path = build_path.join(dir).join(format!("{basename}.ast"));
                    let source_path = package.path.join(impl_path);
                    files.push(json!({
                        "moduleName": module_name,
                        "astPath": ast_path.to_string_lossy(),
                        "sourcePath": source_path.to_string_lossy(),
                        "isInterface": false,
                    }));
                }
            }
        }

        let stdin_json = json!({
            "query": query,
            "files": files,
        });

        (analysis_binary_path, stdin_json)
    };

    let file_count = stdin_json["files"].as_array().map(|a| a.len()).unwrap_or(0);
    let _span =
        tracing::info_span!("lsp.workspace_symbol", query = %query, file_count = file_count).entered();

    let mut child = match Command::new(&analysis_binary_path)
        .args(["rewatch", "workspaceSymbol"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(child) => child,
        Err(e) => {
            tracing::warn!("workspace_symbol: failed to spawn analysis binary: {e}");
            return None;
        }
    };

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(stdin_json.to_string().as_bytes());
    }

    let output = match child.wait_with_output() {
        Ok(output) => output,
        Err(e) => {
            tracing::warn!("workspace_symbol: analysis binary invocation failed: {e}");
            return None;
        }
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.is_empty() {
        tracing::debug!(stderr = %stderr, "workspace_symbol: stderr");
    }

    let stdout = String::from_utf8_lossy(&output.stdout);

    #[allow(deprecated)]
    let symbols: Vec<SymbolInformation> = serde_json::from_str(&stdout).ok()?;

    if symbols.is_empty() { None } else { Some(symbols) }
}
