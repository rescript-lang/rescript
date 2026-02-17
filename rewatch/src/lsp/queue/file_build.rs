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
use crate::build::build_types::{
    BuildCommandState, BuildConfig, CompileScope, Module, OutputMode, OutputTarget,
};
use crate::build::diagnostics::{BscDiagnostic, Severity};
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
///
/// Uses a take-build-replace pattern to minimize lock contention: each
/// project's `BuildCommandState` is removed from the `ProjectMap` under a
/// brief lock, built without holding the mutex, then inserted back. This
/// keeps the lock held only for HashMap bookkeeping rather than for the
/// entire duration of bsc subprocess invocations, so LSP handlers (hover,
/// completions, etc.) are not blocked during compilation.
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

        // Phase 1: group files by project and take states out (brief lock).
        // While the states are removed, LSP handlers for these projects
        // will get empty responses instead of blocking on the mutex.
        let mut project_work: Vec<(PathBuf, BuildCommandState, Vec<PathBuf>)> = Vec::new();
        {
            let mut guard = match projects.lock() {
                Ok(g) => g,
                Err(e) => {
                    tracing::error!("projects mutex poisoned in build flush: {e}");
                    return Vec::new();
                }
            };
            let mut by_project: HashMap<PathBuf, Vec<PathBuf>> = HashMap::new();
            for path in &file_paths {
                if let Some(root) = guard.project_root_for_path(path) {
                    by_project.entry(root).or_default().push(path.clone());
                }
            }
            for (root, paths) in by_project {
                if let Some(state) = guard.states.remove(&root) {
                    project_work.push((root, state, paths));
                }
            }
        } // lock released

        // Phase 2: build without holding the lock.
        let mut results = Vec::new();
        for (root, build_state, paths) in &mut project_work {
            let module_names: Vec<String> = paths
                .iter()
                .filter_map(|p| build_state.mark_file_parse_dirty(p))
                .collect();
            if module_names.is_empty() {
                tracing::warn!(
                    project = %root.display(),
                    "file_build: no modules resolved from {} dirty file(s)",
                    paths.len()
                );
            } else {
                tracing::debug!(
                    project = %root.display(),
                    modules = ?module_names,
                    "file_build: building modules"
                );
                results.push(build_batch(build_state, module_names));
            }
        }

        // Phase 3: put states back (brief lock).
        if let Ok(mut guard) = projects.lock() {
            for (root, state, _) in project_work {
                guard.uri_cache.retain(|_, r| *r != root);
                guard.states.insert(root, state);
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
        if !result.diagnostics.is_empty() {
            for diag in &result.diagnostics {
                let first_line = diag.message.lines().next().unwrap_or("");
                client
                    .log_message(
                        tower_lsp::lsp_types::MessageType::INFO,
                        format!(
                            "file_build: diagnostic {:?} in {} — {}",
                            diag.severity,
                            diag.file.display(),
                            first_line,
                        ),
                    )
                    .await;
            }
        }
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
/// This is the core LSP save-build flow, executed in three steps:
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
/// ## Step 3: `compile_errored` — produce JS for previously-errored dependents
///
/// If step 2 successfully typechecked modules that were previously at
/// `CompileError` (from a prior build where their dependency had a broken
/// API), those modules now need full compilation to produce JS output.
/// Without this step, they'd be stuck at `TypeChecked` with stale or
/// missing JavaScript.
///
/// ## Why three steps?
///
/// Keeping imports and importers as separate builds prevents the compile
/// loop from pulling in unrelated modules. For example, if the saved
/// file imports a shared library module, and an unrelated package also
/// imports that library, a single combined build would drag in that
/// unrelated package. If it has errors, the compile loop aborts before
/// the saved file gets compiled.
///
/// Step 3 exists because step 2 only typechecks (no JS). When a
/// dependency fix resolves a prior compile error in a dependent, that
/// dependent needs full recompilation to produce its JS output.
#[instrument(name = "lsp.flush.file_build.batch", skip_all, fields(modules = ?module_names, error_count = tracing::field::Empty))]
fn build_batch(build_state: &mut BuildCommandState, module_names: Vec<String>) -> BatchBuildResult {
    let (mut diagnostics, mut touched_files) = compile_dependencies(build_state, &module_names);

    // Snapshot modules at CompileError before typechecking dependents.
    // After typecheck, any that moved to TypeChecked need full compilation.
    let errored_before_typecheck: AHashSet<String> = build_state
        .build_state
        .modules
        .iter()
        .filter_map(|(name, module)| match module {
            Module::SourceFile(sf) if sf.compilation_stage().is_compile_error() => Some(name.clone()),
            _ => None,
        })
        .collect();

    let (dep_diagnostics, dep_touched) = typecheck_dependents(build_state, &module_names);

    diagnostics.extend(dep_diagnostics);
    touched_files.extend(dep_touched);

    // Step 3: compile modules that were at CompileError but are now TypeChecked
    // (meaning the typecheck succeeded after a dependency fix). These need
    // full compilation to produce JS output.
    let (err_diagnostics, err_touched) = compile_resolved_errors(build_state, &errored_before_typecheck);
    diagnostics.extend(err_diagnostics);
    touched_files.extend(err_touched);

    let error_count = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .count();
    if error_count > 0 {
        tracing::Span::current().record("error_count", error_count);
    }

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
#[instrument(name = "lsp.flush.file_build.compile_deps", skip_all, fields(error_count = tracing::field::Empty))]
fn compile_dependencies(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    let build_config = BuildConfig {
        output: OutputTarget::Lsp,
        scope: CompileScope::CompileDependencies(module_names.iter().cloned().collect()),
        output_mode: OutputMode::Silent,
    };

    // Parse dirty files and resolve deps so the dependency closure
    // reflects the saved content, not stale state.
    let parse_result = build::parse_and_resolve(build_state, &build_config, Some(std::time::Duration::ZERO));

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
        &build_config,
        parse_warnings,
        Some(std::time::Duration::ZERO),
    ) {
        Ok(result) => {
            tracing::debug!(
                compiled_modules = ?result.modules.iter().collect::<Vec<_>>(),
                "compile_dependencies: compiled module closure"
            );
            (
                result.diagnostics,
                module_names_to_paths(build_state, &result.modules),
            )
        }
        Err(e) => {
            tracing::warn!("Incremental build completed with errors: {e}");
            (e.diagnostics, module_names_to_paths(build_state, &e.modules))
        }
    };

    let mut all_diagnostics = parse_diagnostics;
    all_diagnostics.extend(diagnostics);

    let error_count = all_diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .count();
    if error_count > 0 {
        tracing::Span::current().record("error_count", error_count);
    }

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
#[instrument(name = "lsp.flush.file_build.typecheck_deps", skip_all, fields(dependent_count = tracing::field::Empty, error_count = tracing::field::Empty))]
fn typecheck_dependents(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    let build_config = BuildConfig {
        output: OutputTarget::Lsp,
        scope: CompileScope::TypecheckDependents(module_names.iter().cloned().collect()),
        output_mode: OutputMode::Silent,
    };

    let (diagnostics, touched_files) = match build::incremental_build(
        build_state,
        &build_config,
        String::new(),
        Some(std::time::Duration::ZERO),
    ) {
        Ok(result) => {
            tracing::Span::current().record("dependent_count", result.modules.len());
            tracing::debug!(
                dependent_modules = ?result.modules.iter().collect::<Vec<_>>(),
                "typecheck_dependents: typechecked dependent closure"
            );
            (
                result.diagnostics,
                module_names_to_paths(build_state, &result.modules),
            )
        }
        Err(e) => {
            tracing::warn!("Typecheck of dependents completed with errors: {e}");
            tracing::Span::current().record("dependent_count", e.modules.len());
            tracing::debug!(
                dependent_modules = ?e.modules.iter().collect::<Vec<_>>(),
                "typecheck_dependents: typechecked dependent closure (with errors)"
            );
            (e.diagnostics, module_names_to_paths(build_state, &e.modules))
        }
    };

    let error_count = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .count();
    if error_count > 0 {
        tracing::Span::current().record("error_count", error_count);
    }

    (diagnostics, touched_files)
}

/// Fully compile modules that were previously at `CompileError` and have
/// now been successfully typechecked (moved to `TypeChecked` by step 2).
///
/// Filters `errored_before_typecheck` down to modules that are now at
/// `TypeChecked`, then runs a full compile for those modules to produce
/// JS output. If none were resolved, this is a no-op (but the span is
/// still emitted for observability).
#[instrument(name = "lsp.flush.file_build.compile_resolved", skip_all, fields(modules = tracing::field::Empty, error_count = tracing::field::Empty))]
fn compile_resolved_errors(
    build_state: &mut BuildCommandState,
    errored_before_typecheck: &AHashSet<String>,
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    let resolved: Vec<String> = errored_before_typecheck
        .iter()
        .filter(|name| {
            build_state
                .build_state
                .modules
                .get(*name)
                .is_some_and(|m| matches!(m, Module::SourceFile(sf) if matches!(sf.compilation_stage(), crate::build::build_types::CompilationStage::TypeChecked { .. })))
        })
        .cloned()
        .collect();

    if !resolved.is_empty() {
        tracing::Span::current().record("modules", tracing::field::debug(&resolved));
    }

    if resolved.is_empty() {
        return (Vec::new(), HashSet::new());
    }

    let build_config = BuildConfig {
        output: OutputTarget::Lsp,
        scope: CompileScope::CompileDependencies(resolved.iter().cloned().collect()),
        output_mode: OutputMode::Silent,
    };

    let (diagnostics, touched_files) = match build::incremental_build(
        build_state,
        &build_config,
        String::new(),
        Some(std::time::Duration::ZERO),
    ) {
        Ok(result) => {
            tracing::debug!(
                compiled_modules = ?result.modules.iter().collect::<Vec<_>>(),
                "compile_resolved_errors: compiled previously-errored modules"
            );
            (
                result.diagnostics,
                module_names_to_paths(build_state, &result.modules),
            )
        }
        Err(e) => {
            tracing::warn!("Compilation of resolved errors completed with errors: {e}");
            (e.diagnostics, module_names_to_paths(build_state, &e.modules))
        }
    };

    let error_count = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .count();
    if error_count > 0 {
        tracing::Span::current().record("error_count", error_count);
    }

    (diagnostics, touched_files)
}

fn module_names_to_paths(build_state: &BuildCommandState, names: &AHashSet<String>) -> HashSet<PathBuf> {
    let mut paths = HashSet::new();
    for name in names {
        let Some(module) = build_state.build_state.modules.get(name) else {
            continue;
        };
        let Module::SourceFile(sf_module) = module else {
            continue;
        };
        let Some(package) = build_state.build_state.packages.get(&sf_module.package_name) else {
            continue;
        };
        paths.insert(package.path.join(&sf_module.source_file.implementation.path));
        if let Some(interface) = &sf_module.source_file.interface {
            paths.insert(package.path.join(&interface.path));
        }
    }
    paths
}
