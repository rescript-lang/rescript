use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::Mutex;

use serde_json::{Value, json};
use tower_lsp::lsp_types::{CompletionItem, CompletionResponse, Position, Url};
use tracing::instrument;

use crate::build::build_types::{BuildCommandState, BuildProfile, SourceType};
use crate::build::packages::Namespace;
use crate::config;
use crate::helpers;
use crate::lsp::did_change;

/// Handle a completion request: resolve the source buffer, lock build state,
/// and delegate to the analysis binary.
pub fn handle(
    open_buffers: &Mutex<HashMap<Url, String>>,
    build_state: &Mutex<Option<BuildCommandState>>,
    file_path: &Path,
    uri: &Url,
    position: Position,
) -> Option<CompletionResponse> {
    let source = open_buffers
        .lock()
        .ok()
        .and_then(|buffers| buffers.get(uri).cloned())
        .or_else(|| std::fs::read_to_string(file_path).ok());

    let source = match source {
        Some(s) => s,
        None => {
            tracing::warn!("completion: no buffer content available");
            return None;
        }
    };

    let guard = build_state.lock().ok()?;
    let build_state = guard.as_ref()?;

    run(build_state, file_path, &source, position).map(CompletionResponse::Array)
}

/// Run a completion request by shelling out to `rescript-editor-analysis.exe completion-rewatch`.
///
/// Builds a JSON blob with all the package/module context the analysis binary needs,
/// pipes it to stdin, and parses the JSON completion items from stdout.
#[instrument(name = "lsp.completion", skip_all, fields(file = %file_path.display()))]
fn run(
    build_state: &BuildCommandState,
    file_path: &Path,
    source: &str,
    position: Position,
) -> Option<Vec<CompletionItem>> {
    let (module_name, package_name, _is_interface) = build_state.find_module_for_file(file_path)?;

    let package = build_state.build_state.packages.get(&package_name)?;

    let module = build_state.build_state.modules.get(&module_name)?;
    let source_file = match &module.source_type {
        SourceType::SourceFile(sf) => sf,
        _ => return None,
    };

    // Determine the original file path (absolute) for the analysis binary.
    let original_file = package.path.join(&source_file.implementation.path);
    let path_str = original_file.to_string_lossy();

    // Ensure a .cmt exists for this module. If the editor triggers completion
    // before any didChange, lib/lsp may not have the .cmt yet.
    let build_path = package.get_build_path_for_profile(BuildProfile::TypecheckOnly);
    let impl_path = &source_file.implementation.path;
    let basename = helpers::file_path_to_compiler_asset_basename(impl_path, &package.namespace);
    let dir = impl_path.parent().unwrap_or(Path::new(""));
    let cmt_path = build_path.join(dir).join(format!("{}.cmt", basename));
    if !cmt_path.exists() {
        tracing::debug!("completion: .cmt missing, running typecheck first");
        did_change::run(build_state, file_path, source);
    }

    let root_path = package.path.to_string_lossy();

    let root_config = build_state.build_state.get_root_config();

    let json_blob = build_completion_json(
        build_state,
        source,
        &path_str,
        position,
        &root_path,
        &package.namespace,
        &package.config,
        root_config,
    );

    let analysis_path = build_state
        .build_state
        .compiler_info
        .bsc_path
        .parent()?
        .join("rescript-editor-analysis.exe");

    let mut child = match Command::new(&analysis_path)
        .args(["completion-rewatch"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
    {
        Ok(child) => child,
        Err(e) => {
            tracing::warn!("completion: failed to spawn analysis binary: {e}");
            return None;
        }
    };

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(json_blob.to_string().as_bytes());
    }

    let output = match child.wait_with_output() {
        Ok(output) => output,
        Err(e) => {
            tracing::warn!("completion: analysis binary invocation failed: {e}");
            return None;
        }
    };

    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.is_empty() {
        tracing::debug!(stderr = %stderr, "completion: analysis stderr");
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    parse_completion_response(&stdout)
}

/// Build the JSON blob to send to `rescript-editor-analysis.exe completion-rewatch`.
#[allow(clippy::too_many_arguments)]
fn build_completion_json(
    build_state: &BuildCommandState,
    source: &str,
    path: &str,
    position: Position,
    root_path: &str,
    namespace: &Namespace,
    package_config: &config::Config,
    root_config: &config::Config,
) -> Value {
    let namespace_str = match namespace {
        Namespace::Namespace(ns) | Namespace::NamespaceWithEntry { namespace: ns, .. } => {
            Value::String(ns.clone())
        }
        Namespace::NoNamespace => Value::Null,
    };

    let suffix = root_config.suffix.clone().unwrap_or_else(|| ".js".to_string());

    let generic_jsx_module = match &root_config.jsx {
        Some(jsx) => match &jsx.module {
            Some(config::JsxModule::Other(name)) => Value::String(name.clone()),
            _ => Value::Null,
        },
        None => Value::Null,
    };

    let opens = build_opens(namespace, package_config);
    let paths_for_module = build_paths_for_module(build_state);
    let (project_files, dependencies_files) = build_file_sets(build_state);

    json!({
        "source": source,
        "path": path,
        "pos": [position.line, position.character],
        "rootPath": root_path,
        "namespace": namespace_str,
        "suffix": suffix,
        "rescriptVersion": [13, 0],
        "genericJsxModule": generic_jsx_module,
        "opens": opens,
        "pathsForModule": paths_for_module,
        "projectFiles": project_files,
        "dependenciesFiles": dependencies_files,
    })
}

/// Build the `opens` list matching the analysis binary's expectations.
///
/// Each entry is a list of strings ending with `"place holder"` as a sentinel.
/// The standard opens are:
/// 1. `["Stdlib", "place holder"]` and `["Pervasives", "JsxModules", "place holder"]` (unless `-nopervasives`)
/// 2. `[namespace_name, "place holder"]` if there's a namespace
/// 3. From `-open Foo.Bar` in compiler_flags: `["Foo", "Bar", "place holder"]`
fn build_opens(namespace: &Namespace, config: &config::Config) -> Value {
    let flags = config::flatten_flags(&config.compiler_flags);

    let no_pervasives = flags.iter().any(|f| f == "-nopervasives");

    let mut opens: Vec<Vec<String>> = Vec::new();

    // Standard pervasives opens
    if !no_pervasives {
        opens.push(vec!["Stdlib".to_string(), "place holder".to_string()]);
        opens.push(vec![
            "Pervasives".to_string(),
            "JsxModules".to_string(),
            "place holder".to_string(),
        ]);
    }

    // Namespace open
    match namespace {
        Namespace::Namespace(ns) | Namespace::NamespaceWithEntry { namespace: ns, .. } => {
            opens.push(vec![ns.clone(), "place holder".to_string()]);
        }
        Namespace::NoNamespace => {}
    }

    // Opens from compiler flags
    let mut i = 0;
    while i < flags.len() {
        if flags[i] == "-open"
            && let Some(name) = flags.get(i + 1)
        {
            let mut path: Vec<String> = name.split('.').map(|s| s.to_string()).collect();
            path.push("place holder".to_string());
            opens.push(path);
            i += 2;
            continue;
        }
        i += 1;
    }

    Value::Array(
        opens
            .into_iter()
            .map(|path| Value::Array(path.into_iter().map(Value::String).collect()))
            .collect(),
    )
}

/// Build the `pathsForModule` object mapping module names to their .cmt/.res paths.
///
/// Includes both project/dependency modules from the build state and runtime
/// modules from `lib/ocaml/` (which are pre-built and not in the build state).
fn build_paths_for_module(build_state: &BuildCommandState) -> Value {
    let mut result = serde_json::Map::new();

    // Add runtime modules from lib/ocaml/. These are pre-built and not
    // discovered as regular packages in the build state.
    let ocaml_dir = build_state
        .build_state
        .compiler_info
        .runtime_path
        .join("lib")
        .join("ocaml");
    if let Ok(entries) = std::fs::read_dir(&ocaml_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("cmt") {
                let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
                // Check for a matching .cmti (interface)
                let cmti_path = path.with_extension("cmti");
                let res_path = ocaml_dir.join(format!("{stem}.res"));
                let resi_path = ocaml_dir.join(format!("{stem}.resi"));

                if cmti_path.exists() && resi_path.exists() {
                    result.insert(
                        stem.to_string(),
                        json!({
                            "intfAndImpl": {
                                "cmti": cmti_path.to_string_lossy(),
                                "resi": resi_path.to_string_lossy(),
                                "cmt": path.to_string_lossy(),
                                "res": res_path.to_string_lossy(),
                            }
                        }),
                    );
                } else if res_path.exists() {
                    result.insert(
                        stem.to_string(),
                        json!({
                            "impl": {
                                "cmt": path.to_string_lossy(),
                                "res": res_path.to_string_lossy(),
                            }
                        }),
                    );
                }
            }
        }
    }

    for (module_name, module) in &build_state.build_state.modules {
        let Some(package) = build_state.build_state.packages.get(&module.package_name) else {
            continue;
        };

        match &module.source_type {
            SourceType::SourceFile(source_file) => {
                let build_path = package.get_build_path_for_profile(BuildProfile::TypecheckOnly);

                let impl_path = &source_file.implementation.path;
                let basename = helpers::file_path_to_compiler_asset_basename(impl_path, &package.namespace);

                // Build the cmt path: build_path / relative_dir / basename.cmt
                let dir = impl_path.parent().unwrap_or(Path::new(""));
                let cmt = build_path.join(dir).join(format!("{}.cmt", basename));
                let res = package.path.join(impl_path);

                if let Some(interface) = &source_file.interface {
                    let iface_path = &interface.path;
                    let iface_basename =
                        helpers::file_path_to_compiler_asset_basename(iface_path, &package.namespace);
                    let iface_dir = iface_path.parent().unwrap_or(Path::new(""));
                    let cmti = build_path
                        .join(iface_dir)
                        .join(format!("{}.cmti", iface_basename));
                    let resi = package.path.join(iface_path);

                    result.insert(
                        module_name.clone(),
                        json!({
                            "intfAndImpl": {
                                "cmti": cmti.to_string_lossy(),
                                "resi": resi.to_string_lossy(),
                                "cmt": cmt.to_string_lossy(),
                                "res": res.to_string_lossy(),
                            }
                        }),
                    );
                } else {
                    result.insert(
                        module_name.clone(),
                        json!({
                            "impl": {
                                "cmt": cmt.to_string_lossy(),
                                "res": res.to_string_lossy(),
                            }
                        }),
                    );
                }
            }
            SourceType::MlMap(_) => {
                let build_path = package.get_build_path_for_profile(BuildProfile::TypecheckOnly);
                let cmt = build_path.join(format!("{}.cmt", module_name));
                result.insert(
                    module_name.clone(),
                    json!({
                        "namespace": {
                            "cmt": cmt.to_string_lossy(),
                        }
                    }),
                );
            }
        }
    }

    Value::Object(result)
}

/// Partition module names into project files and dependency files.
///
/// Runtime modules from `lib/ocaml/` are included in dependency files.
fn build_file_sets(build_state: &BuildCommandState) -> (Value, Value) {
    let root_package_name = &build_state.build_state.get_root_config().name;

    let mut project_files = Vec::new();
    let mut dependencies_files = Vec::new();

    for (module_name, module) in &build_state.build_state.modules {
        if &module.package_name == root_package_name {
            project_files.push(Value::String(module_name.clone()));
        } else {
            dependencies_files.push(Value::String(module_name.clone()));
        }
    }

    // Add runtime modules as dependencies
    let ocaml_dir = build_state
        .build_state
        .compiler_info
        .runtime_path
        .join("lib")
        .join("ocaml");
    if let Ok(entries) = std::fs::read_dir(&ocaml_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("cmt")
                && let Some(stem) = path.file_stem().and_then(|s| s.to_str())
            {
                dependencies_files.push(Value::String(stem.to_string()));
            }
        }
    }

    (Value::Array(project_files), Value::Array(dependencies_files))
}

/// Parse the JSON output from the analysis binary into CompletionItems.
///
/// The analysis binary already outputs LSP-conformant completion items,
/// so we deserialize directly.
fn parse_completion_response(stdout: &str) -> Option<Vec<CompletionItem>> {
    serde_json::from_str(stdout).ok()
}
