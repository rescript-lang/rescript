use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use ahash::AHashSet;
use tower_lsp::Client;
use tower_lsp::lsp_types::Url;
use tracing::instrument;

use super::super::{ProjectMap, group_by_file, publish_and_store};
use super::PendingFileBuild;
use crate::build;
use crate::build::build_types::{BuildCommandState, BuildConfig, CompileScope, OutputTarget, SourceType};
use crate::build::diagnostics::BscDiagnostic;
use crate::lsp::diagnostic_store::DiagnosticStore;

/// Result of a batched build for one project.
struct BatchBuildResult {
    diagnostics: Vec<BscDiagnostic>,
    /// Absolute paths of all source files that were compiled or typechecked,
    /// including files from the dependency and dependent closures.
    touched_files: HashSet<PathBuf>,
}

/// Run incremental builds for saved files. Groups files by project,
/// marks them dirty, compiles their dependency closure and typechecks
/// their dependent closure, then publishes diagnostics for all touched files.
pub(super) async fn run(
    compile_files: &HashMap<Url, PendingFileBuild>,
    projects: &Arc<Mutex<ProjectMap>>,
    client: &Client,
    diagnostic_store: Option<&DiagnosticStore>,
) {
    let file_paths: Vec<PathBuf> = compile_files.values().map(|b| b.file_path.clone()).collect();
    let projects = Arc::clone(projects);

    let parent_span = tracing::Span::current();
    let results = tokio::task::spawn_blocking(move || {
        let _entered = parent_span.enter();
        let mut guard = match projects.lock() {
            Ok(g) => g,
            Err(e) => {
                tracing::error!("projects mutex poisoned in build flush: {e}");
                return Vec::new();
            }
        };

        // Group files by project root
        let mut by_project: HashMap<PathBuf, Vec<PathBuf>> = HashMap::new();
        for path in &file_paths {
            if let Some(root) = guard.project_root_for_path(path) {
                by_project.entry(root).or_default().push(path.clone());
            }
        }

        // Mark dirty + build per project
        let mut results = Vec::new();
        for (root, paths) in &by_project {
            if let Some(build_state) = guard.states.get_mut(root) {
                let module_names: Vec<String> = paths
                    .iter()
                    .filter_map(|p| build_state.mark_file_parse_dirty(p))
                    .collect();
                if !module_names.is_empty() {
                    results.push(build_batch(build_state, module_names));
                }
            }
        }
        results
    })
    .await
    .unwrap_or_else(|e| {
        tracing::error!("build flush task panicked: {e}");
        Vec::new()
    });

    // Publish diagnostics for all touched files
    for result in &results {
        let by_file = group_by_file(&result.diagnostics);
        for touched in &result.touched_files {
            if let Ok(uri) = Url::from_file_path(touched) {
                let diags = by_file.get(&uri).cloned().unwrap_or_default();
                publish_and_store(client, diagnostic_store, uri, diags).await;
            }
        }
    }
}

/// Build one or more saved files and propagate diagnostics.
///
/// This is the core LSP save-build flow, executed in two steps:
///
/// ## Step 1: `compile_dependencies` — produce JS for the saved file
///
/// Compiles the saved file and every module it imports (recursively).
/// Imports compile first, the saved file compiles last — producing `.js`
/// output. This is the minimal set of modules needed to generate correct
/// JavaScript for the saved file.
///
/// ## Step 2: `typecheck_dependents` — check files that import the saved file
///
/// Finds every module that imports the saved file (and everything that
/// imports *those*, recursively). Re-typechecks them to surface errors
/// caused by API changes (e.g. a function signature changed). No
/// JavaScript is emitted — that happens when those files are saved.
///
/// ## Why two steps?
///
/// Keeping imports and importers as separate builds prevents the compile
/// loop from pulling in unrelated modules. For example, if the saved
/// file imports a shared library module, and an unrelated package also
/// imports that library, a single combined build would drag in that
/// unrelated package. If it has errors, the compile loop aborts before
/// the saved file gets compiled.
#[instrument(name = "lsp.build", skip_all, fields(file_count = module_names.len()))]
fn build_batch(build_state: &mut BuildCommandState, module_names: Vec<String>) -> BatchBuildResult {
    let (mut diagnostics, mut touched_files) = compile_dependencies(build_state, &module_names);

    let (dep_diagnostics, dep_touched) = typecheck_dependents(build_state, &module_names);

    diagnostics.extend(dep_diagnostics);
    touched_files.extend(dep_touched);

    BatchBuildResult {
        diagnostics,
        touched_files,
    }
}

/// Compile the saved modules and all their transitive imports to JS.
///
/// Collects every module the saved files import (recursively) and runs
/// `incremental_build` with `CompileScope::Scoped(closure)`, which
/// restricts the compile universe to only the dependency closure.
/// This produces type information and JavaScript output for just the
/// saved file and its imports, without touching the rest of the codebase.
#[instrument(name = "lsp.build.compile_dependencies", skip_all)]
fn compile_dependencies(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    let scope = CompileScope::CompileDependencies(module_names.iter().cloned().collect());
    let mode = scope.mode();
    let build_config = BuildConfig {
        output: OutputTarget::Lsp,
        scope,
    };

    // Parse dirty files and resolve deps so the dependency closure
    // reflects the saved content, not stale state.
    let parse_result = build::parse_and_resolve(
        build_state,
        build_config.output,
        mode,
        false,
        true,
        Some(std::time::Duration::ZERO),
        true,
    );

    let (parse_warnings, parse_diagnostics) = match parse_result {
        Ok(warnings) => {
            let diags = if warnings.is_empty() {
                Vec::new()
            } else {
                build::diagnostics::parse_compiler_output(&warnings)
            };
            (warnings, diags)
        }
        Err(e) => {
            // Parse failure — return diagnostics for the affected files, skip compilation
            let touched: HashSet<PathBuf> = e.diagnostics.iter().map(|d| d.file.clone()).collect();
            return (e.diagnostics, touched);
        }
    };

    let (diagnostics, touched_files) = match build::incremental_build(
        build_state,
        build_config,
        parse_warnings,
        Some(std::time::Duration::ZERO),
        false,
        false,
        true,
        false,
        true,
    ) {
        Ok(result) => (
            result.diagnostics,
            module_names_to_paths(build_state, &result.modules),
        ),
        Err(e) => {
            tracing::warn!("Incremental build completed with errors: {e}");
            (e.diagnostics, module_names_to_paths(build_state, &e.modules))
        }
    };

    let mut all_diagnostics = parse_diagnostics;
    all_diagnostics.extend(diagnostics);
    (all_diagnostics, touched_files)
}

/// Re-typecheck all modules that import the saved modules (recursively).
///
/// When a file's public API changes, modules that import it may now have
/// type errors. This step finds all such importers and re-typechecks them
/// to surface those errors as diagnostics. No JavaScript is emitted —
/// that happens when those files are themselves saved.
///
/// Uses `CompileScope::TypecheckDependents` to restrict the build to only
/// the relevant modules.
#[instrument(name = "lsp.build.typecheck_dependents", skip_all, fields(dependent_count = tracing::field::Empty))]
fn typecheck_dependents(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    let build_config = BuildConfig {
        output: OutputTarget::Lsp,
        scope: CompileScope::TypecheckDependents(module_names.iter().cloned().collect()),
    };

    let (diagnostics, touched_files) = match build::incremental_build(
        build_state,
        build_config,
        String::new(),
        Some(std::time::Duration::ZERO),
        false,
        false,
        true,
        false,
        true,
    ) {
        Ok(result) => {
            tracing::Span::current().record("dependent_count", result.modules.len());
            (
                result.diagnostics,
                module_names_to_paths(build_state, &result.modules),
            )
        }
        Err(e) => {
            tracing::warn!("Typecheck of dependents completed with errors: {e}");
            tracing::Span::current().record("dependent_count", e.modules.len());
            (e.diagnostics, module_names_to_paths(build_state, &e.modules))
        }
    };

    (diagnostics, touched_files)
}

fn module_names_to_paths(build_state: &BuildCommandState, names: &AHashSet<String>) -> HashSet<PathBuf> {
    let mut paths = HashSet::new();
    for name in names {
        let Some(module) = build_state.build_state.modules.get(name) else {
            continue;
        };
        let SourceType::SourceFile(source_file) = &module.source_type else {
            continue;
        };
        let Some(package) = build_state.build_state.packages.get(&module.package_name) else {
            continue;
        };
        paths.insert(package.path.join(&source_file.implementation.path));
        if let Some(interface) = &source_file.interface {
            paths.insert(package.path.join(&interface.path));
        }
    }
    paths
}
