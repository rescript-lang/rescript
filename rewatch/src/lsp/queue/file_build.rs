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
use crate::build::deps;
use crate::build::diagnostics::BscDiagnostic;
use crate::build::parse;
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
/// `incremental_build` with `TypecheckAndEmit`, which produces type
/// information and JavaScript output.
///
/// ### Why we temporarily mark other modules as `Built`
///
/// The initial LSP build only typechecks (no JS), so most modules are at
/// `TypeChecked` stage. When we now request `TypecheckAndEmit` (which
/// targets `Built`), the build system sees every `TypeChecked` module as
/// needing compilation — it would compile the entire codebase on the
/// first save.
///
/// To avoid this, we temporarily mark all modules *outside* the import
/// set as `Built` so the build system skips them. After the build
/// finishes, we restore them back to `TypeChecked`.
#[instrument(name = "lsp.build.compile_dependencies", skip_all)]
fn compile_dependencies(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    // Pre-parse dirty files and resolve their deps so the dependency
    // closure below reflects the saved content, not stale state.
    // Without this, a file that starts importing a new module would
    // compute a closure from its old deps, excluding the new dependency
    // from the compile universe and causing a "module not found" error.
    let parse_warnings = parse::generate_asts(build_state, BuildProfile::TypecheckAndEmit, || {});
    let mut parse_diagnostics = Vec::new();
    match parse_warnings {
        Ok(warnings) => {
            if !warnings.is_empty() {
                parse_diagnostics = build::diagnostics::parse_compiler_output(&warnings);
            }
        }
        Err(e) => {
            let err_str = e.to_string();
            parse_diagnostics = build::diagnostics::parse_compiler_output(&err_str);
        }
    }
    let deleted_modules = build_state.build_state.deleted_modules.clone();
    deps::get_deps(
        &mut build_state.build_state,
        &deleted_modules,
        BuildProfile::TypecheckAndEmit,
    );

    let closure =
        dependency_closure::get_dependency_closure(&build_state.build_state.modules, module_names.to_vec());
    let touched_files = module_names_to_paths(build_state, &closure);

    // Temporarily promote modules outside the closure so they don't enter
    // the compile universe (see doc comment above).
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
/// Uses the same temporary `Built` marking as `compile_dependencies` to
/// keep the build scoped to only the relevant modules.
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

    // Mark dependents as Dirty so they enter the compile universe.
    for name in &dependents {
        if let Some(module) = build_state.build_state.modules.get_mut(name) {
            module.compilation_stage = CompilationStage::Dirty;
        }
    }

    // Temporarily promote modules outside the dependent closure (see
    // compile_dependencies doc comment for explanation).
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
