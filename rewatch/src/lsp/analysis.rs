use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use serde_json::{Value, json};
use std::sync::Mutex;
use tower_lsp::lsp_types::{Position, Url};

use crate::build::build_types::{BuildCommandState, OutputTarget, SourceFile, SourceType};
use crate::build::packages::{Namespace, Package};
use crate::config;
use crate::helpers;
use crate::lsp::typecheck;

pub(crate) type ExtraFieldsFn<'a> = &'a dyn Fn(&mut serde_json::Map<String, Value>);

/// Cached results of scanning `lib/ocaml/` for runtime modules.
/// Populated once on first analysis request, never invalidated (runtime doesn't change).
pub(crate) struct RuntimeModuleData {
    /// `pathsForModule` entries for runtime modules (module_name -> JSON value).
    pub paths: serde_json::Map<String, Value>,
    /// Module names for the `dependenciesFiles` list.
    pub module_names: Vec<String>,
}

/// Scan `runtime_path/lib/ocaml/` for `.cmt` files and build both the
/// `pathsForModule` entries and dependency module name list in a single pass.
pub(crate) fn scan_runtime_modules(runtime_path: &Path) -> RuntimeModuleData {
    let mut paths = serde_json::Map::new();
    let mut module_names = Vec::new();

    let ocaml_dir = runtime_path.join("lib").join("ocaml");
    if let Ok(entries) = std::fs::read_dir(&ocaml_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) != Some("cmt") {
                continue;
            }
            let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
            module_names.push(stem.to_string());

            let cmti_path = path.with_extension("cmti");
            let res_path = ocaml_dir.join(format!("{stem}.res"));
            let resi_path = ocaml_dir.join(format!("{stem}.resi"));

            if cmti_path.exists() && resi_path.exists() {
                paths.insert(
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
                paths.insert(
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

    RuntimeModuleData { paths, module_names }
}

/// Pre-built analysis context that holds everything needed to run the analysis
/// binary. Built while holding the `ProjectMap` lock, then used after releasing it.
///
/// This decouples subprocess execution from lock lifetime: the expensive
/// `spawn()` call runs without holding any locks.
pub struct AnalysisContext {
    pub module_name: String,
    pub package_name: String,
    analysis_binary_path: PathBuf,
    context_json: Value,
}

impl AnalysisContext {
    /// Build an `AnalysisContext` from the current build state.
    ///
    /// This performs module resolution, optional `.cmt` generation, and JSON
    /// context building — all while the caller holds the `ProjectMap` lock.
    /// The returned context can then be used after the lock is released.
    ///
    /// Set `do_ensure_cmt` to `false` for purely syntactic operations
    /// (document_symbol, semantic_tokens) that don't need type information.
    ///
    /// Use `extra_fields` to add handler-specific fields to the JSON blob
    /// (e.g. `endPos` for code_action, `newName` for rename).
    pub fn new(
        build_state: &BuildCommandState,
        runtime: &RuntimeModuleData,
        file_path: &Path,
        source: &str,
        position: Position,
        do_ensure_cmt: bool,
        extra_fields: Option<ExtraFieldsFn<'_>>,
    ) -> Option<Self> {
        let (module_name, package_name, package, source_file) = resolve_module(build_state, file_path)?;
        let original_file = original_path(package, source_file);
        let path_str = original_file.to_string_lossy();

        if do_ensure_cmt {
            ensure_cmt(build_state, package, source_file, file_path, source);
        }

        let root_path = package.path.to_string_lossy();
        let root_config = build_state.build_state.get_root_config();

        let mut context_json = build_context_json(
            build_state,
            runtime,
            source,
            &path_str,
            position,
            &root_path,
            &package.namespace,
            &package.config,
            root_config,
        );

        if let Some(add_fields) = extra_fields
            && let Value::Object(ref mut map) = context_json
        {
            add_fields(map);
        }

        let analysis_binary_path = build_state
            .build_state
            .compiler_info
            .bsc_path
            .parent()?
            .join("rescript-editor-analysis.exe");

        Some(AnalysisContext {
            module_name,
            package_name,
            analysis_binary_path,
            context_json,
        })
    }

    /// Spawn the analysis binary with the pre-built context.
    ///
    /// This is the expensive operation — subprocess creation, stdin write,
    /// and stdout read. Designed to run **after** the `ProjectMap` lock
    /// has been released.
    pub fn spawn(&self, args: &[&str]) -> Option<String> {
        let mut child = match Command::new(&self.analysis_binary_path)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(child) => child,
            Err(e) => {
                tracing::warn!(args = ?args, "analysis: failed to spawn binary: {e}");
                return None;
            }
        };

        if let Some(mut stdin) = child.stdin.take() {
            let _ = stdin.write_all(self.context_json.to_string().as_bytes());
        }

        let output = match child.wait_with_output() {
            Ok(output) => output,
            Err(e) => {
                tracing::warn!(args = ?args, "analysis: binary invocation failed: {e}");
                return None;
            }
        };

        let stderr = String::from_utf8_lossy(&output.stderr);
        if !stderr.is_empty() {
            tracing::debug!(stderr = %stderr, args = ?args, "analysis: stderr");
        }

        Some(String::from_utf8_lossy(&output.stdout).into_owned())
    }
}

/// Resolve the source content for a file from open buffers (unsaved edits) or disk.
///
/// Prefers the in-memory buffer so analysis works on the latest editor content.
pub fn resolve_source(
    open_buffers: &Mutex<HashMap<Url, String>>,
    file_path: &Path,
    uri: &Url,
    label: &str,
) -> Option<String> {
    let source = open_buffers
        .lock()
        .ok()
        .and_then(|buffers| buffers.get(uri).cloned())
        .or_else(|| std::fs::read_to_string(file_path).ok());
    if source.is_none() {
        tracing::warn!("{label}: no buffer content available");
    }
    source
}

/// Resolve the module, package, and source file for a given file path.
///
/// Returns the module name, package name, package reference, and source file reference.
/// This is the common first step in completion, hover, and definition handlers.
fn resolve_module<'a>(
    build_state: &'a BuildCommandState,
    file_path: &Path,
) -> Option<(String, String, &'a Package, &'a SourceFile)> {
    let (module_name, package_name, _is_interface) = build_state.find_module_for_file(file_path)?;
    let package = build_state.build_state.packages.get(&package_name)?;
    let module = build_state.build_state.modules.get(&module_name)?;
    let source_file = match &module.source_type {
        SourceType::SourceFile(sf) => sf,
        _ => return None,
    };
    Some((module_name, package_name, package, source_file))
}

/// Compute the original source file path by joining the package root with the implementation path.
fn original_path(package: &Package, source_file: &SourceFile) -> std::path::PathBuf {
    package.path.join(&source_file.implementation.path)
}

/// Build the JSON context blob sent to `rescript-editor-analysis.exe rewatch <subcommand>`.
///
/// This contains all the package/module context the analysis binary needs,
/// shared by completion, hover, and other analysis endpoints.
#[allow(clippy::too_many_arguments)]
fn build_context_json(
    build_state: &BuildCommandState,
    runtime: &RuntimeModuleData,
    source: &str,
    path: &str,
    position: tower_lsp::lsp_types::Position,
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
    let paths_for_module = build_paths_for_module(build_state, runtime);
    let (project_files, dependencies_files) = build_file_sets(build_state, runtime);

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

/// Check whether a `.cmt` file exists for the given module. If missing,
/// run a single-file typecheck to produce it.
///
/// This is needed because the editor may request hover/completion/definition
/// before any `didChange` has produced the `.cmt` in `lib/lsp/`.
///
/// Callers should wrap this in their own `tracing::info_span!` (e.g.
/// `"lsp.hover.ensure_cmt"`) since the span name must be a string literal.
fn ensure_cmt(
    build_state: &BuildCommandState,
    package: &Package,
    source_file: &SourceFile,
    file_path: &Path,
    source: &str,
) {
    let build_path = package.get_build_path_for_output(OutputTarget::Lsp);
    let impl_path = &source_file.implementation.path;
    let basename = helpers::file_path_to_compiler_asset_basename(impl_path, &package.namespace);
    let dir = impl_path.parent().unwrap_or(Path::new(""));
    let cmt_path = build_path.join(dir).join(format!("{}.cmt", basename));
    if !cmt_path.exists() {
        typecheck::run(build_state, file_path, source);
    }
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
/// modules from the cached `RuntimeModuleData`.
fn build_paths_for_module(build_state: &BuildCommandState, runtime: &RuntimeModuleData) -> Value {
    let mut result = serde_json::Map::new();

    // Add cached runtime modules from lib/ocaml/
    for (name, value) in &runtime.paths {
        result.insert(name.clone(), value.clone());
    }

    for (module_name, module) in &build_state.build_state.modules {
        let Some(package) = build_state.build_state.packages.get(&module.package_name) else {
            continue;
        };

        match &module.source_type {
            SourceType::SourceFile(source_file) => {
                let build_path = package.get_build_path_for_output(OutputTarget::Lsp);

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
                let build_path = package.get_build_path_for_output(OutputTarget::Lsp);
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
/// Runtime modules from the cached `RuntimeModuleData` are included in dependency files.
fn build_file_sets(build_state: &BuildCommandState, runtime: &RuntimeModuleData) -> (Value, Value) {
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

    // Add cached runtime modules as dependencies
    for name in &runtime.module_names {
        dependencies_files.push(Value::String(name.clone()));
    }

    (Value::Array(project_files), Value::Array(dependencies_files))
}
