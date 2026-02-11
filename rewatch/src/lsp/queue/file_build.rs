use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use ahash::AHashSet;
use tower_lsp::Client;
use tower_lsp::lsp_types::Url;
use tracing::instrument;

use super::super::{ProjectMap, dependency_closure, group_by_file, publish_and_store};
use super::PendingFileBuild;
use crate::build;
use crate::build::build_types::{BuildCommandState, BuildProfile, CompilationStage, SourceType};
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

#[instrument(name = "lsp.build.compile_dependencies", skip_all)]
fn compile_dependencies(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    let closure =
        dependency_closure::get_dependency_closure(&build_state.build_state.modules, module_names.to_vec());
    let touched_files = module_names_to_paths(build_state, &closure);

    let mut promoted: Vec<String> = Vec::new();
    for (name, module) in build_state.build_state.modules.iter_mut() {
        if !closure.contains(name) && module.compilation_stage == CompilationStage::TypeChecked {
            module.compilation_stage = CompilationStage::Built;
            promoted.push(name.clone());
        }
    }

    let diagnostics = match build::incremental_build(
        build_state,
        BuildProfile::TypecheckAndEmit,
        Some(std::time::Duration::ZERO),
        false,
        false,
        true,
        false,
        true,
    ) {
        Ok(result) => result.diagnostics,
        Err(e) => {
            tracing::warn!("Incremental build completed with errors: {e}");
            e.diagnostics
        }
    };

    for name in &promoted {
        if let Some(module) = build_state.build_state.modules.get_mut(name)
            && module.compilation_stage == CompilationStage::Built
        {
            module.compilation_stage = CompilationStage::TypeChecked;
        }
    }

    (diagnostics, touched_files)
}

#[instrument(name = "lsp.build.typecheck_dependents", skip_all, fields(dependent_count = tracing::field::Empty))]
fn typecheck_dependents(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    let dependents =
        dependency_closure::get_dependent_closure(&build_state.build_state.modules, module_names.to_vec());

    if dependents.is_empty() {
        return (Vec::new(), HashSet::new());
    }

    let touched_files = module_names_to_paths(build_state, &dependents);

    tracing::Span::current().record("dependent_count", dependents.len());

    for name in &dependents {
        if let Some(module) = build_state.build_state.modules.get_mut(name) {
            module.compilation_stage = CompilationStage::Dirty;
        }
    }

    let mut promoted: Vec<String> = Vec::new();
    for (name, module) in build_state.build_state.modules.iter_mut() {
        if !dependents.contains(name) && module.compilation_stage == CompilationStage::TypeChecked {
            module.compilation_stage = CompilationStage::Built;
            promoted.push(name.clone());
        }
    }

    let diagnostics = match build::incremental_build(
        build_state,
        BuildProfile::TypecheckOnly,
        Some(std::time::Duration::ZERO),
        false,
        false,
        true,
        false,
        true,
    ) {
        Ok(result) => result.diagnostics,
        Err(e) => {
            tracing::warn!("Typecheck of dependents completed with errors: {e}");
            e.diagnostics
        }
    };

    for name in &promoted {
        if let Some(module) = build_state.build_state.modules.get_mut(name)
            && module.compilation_stage == CompilationStage::Built
        {
            module.compilation_stage = CompilationStage::TypeChecked;
        }
    }

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
