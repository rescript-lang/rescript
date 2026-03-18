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
    BuildCommandState, CompilationStage, CompileMode, Module, OutputMode, OutputTarget,
};
use crate::build::diagnostics::{BscDiagnostic, Severity};
use crate::lsp::diagnostic_store::DiagnosticStore;

/// Result of a batched build for one project.
struct BatchBuildResult {
    diagnostics: Vec<BscDiagnostic>,
    /// Absolute paths of all source files that were compiled or typechecked,
    /// including files from the dependency and dependent closures.
    touched_files: HashSet<PathBuf>,
    /// Intent modules that could not be compiled yet (e.g. still at
    /// `CompileError`) and should be kept for future flushes.
    remaining_intent: HashSet<String>,
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
/// Returns:
/// - `errored_files`: URIs that had compile errors (callers skip post-build rechecks for these)
/// - `touched_files`: absolute source paths of all modules that were compiled or typechecked,
///   including reverse dependencies. Used by the db sync queue to know which modules' usages
///   may have changed.
pub(super) async fn run(
    compile_files: &HashMap<Url, PendingFileBuild>,
    full_compile_intent: &mut HashMap<PathBuf, HashSet<String>>,
    projects: &Arc<Mutex<ProjectMap>>,
    client: &Client,
    diagnostic_store: Option<&DiagnosticStore>,
) -> (HashSet<Url>, HashSet<PathBuf>) {
    let file_paths: Vec<PathBuf> = compile_files.values().map(|b| b.file_path.clone()).collect();
    let projects = Arc::clone(projects);

    // Take the intent map so we can move it into the blocking task.
    let mut intent_by_project: HashMap<PathBuf, HashSet<String>> = std::mem::take(full_compile_intent);

    let parent_span = tracing::Span::current();
    let (results, remaining_intent) = tokio::task::spawn_blocking(move || {
        let _entered = parent_span.enter();

        let mut remaining_intent: HashMap<PathBuf, HashSet<String>> = HashMap::new();

        // Phase 1: group files by project and take states out (brief lock).
        // Also take states for projects with pending intents but no file saves.
        // While the states are removed, LSP handlers for these projects
        // will get empty responses instead of blocking on the mutex.
        let mut project_work: Vec<(PathBuf, BuildCommandState, Vec<PathBuf>)> = Vec::new();
        {
            let mut guard = match projects.lock() {
                Ok(g) => g,
                Err(e) => {
                    tracing::error!("projects mutex poisoned in build flush: {e}");
                    return (Vec::new(), intent_by_project);
                }
            };
            let mut by_project: HashMap<PathBuf, Vec<PathBuf>> = HashMap::new();
            for path in &file_paths {
                if let Some(root) = guard.project_root_for_path(path) {
                    by_project.entry(root).or_default().push(path.clone());
                }
            }
            // Also include projects that only have intents (no file saves).
            for root in intent_by_project.keys() {
                by_project.entry(root.clone()).or_default();
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
            let intent = intent_by_project.remove(root);

            let module_names: Vec<String> = if paths.is_empty() {
                Vec::new()
            } else {
                let names: Vec<String> = paths
                    .iter()
                    .filter_map(|p| build_state.mark_file_parse_dirty(p))
                    .collect();
                if names.is_empty() {
                    tracing::warn!(
                        project = %root.display(),
                        "file_build: no modules resolved from {} dirty file(s)",
                        paths.len()
                    );
                }
                names
            };

            // Skip entirely if there are no saved modules and no intent.
            if module_names.is_empty() && intent.is_none() {
                continue;
            }

            if !module_names.is_empty() {
                tracing::debug!(
                    project = %root.display(),
                    modules = ?module_names,
                    "file_build: building modules"
                );
            }

            let mut result = build_batch(build_state, module_names, intent);
            let leftover = std::mem::take(&mut result.remaining_intent);
            if !leftover.is_empty() {
                remaining_intent.entry(root.clone()).or_default().extend(leftover);
            }
            if !result.diagnostics.is_empty() || !result.touched_files.is_empty() {
                results.push(result);
            }
        }

        // Phase 3: put states back (brief lock).
        if let Ok(mut guard) = projects.lock() {
            for (root, state, _) in project_work {
                guard.uri_cache.retain(|_, r| *r != root);
                guard.states.insert(root, state);
            }
        } else {
            tracing::error!("projects mutex poisoned in build flush phase 3");
        }

        (results, remaining_intent)
    })
    .await
    .unwrap_or_else(|e| {
        tracing::error!("build flush task panicked: {e}");
        (Vec::new(), HashMap::new())
    });

    // Restore remaining intents (modules that couldn't be compiled yet).
    *full_compile_intent = remaining_intent;

    // Publish diagnostics for all touched files, tracking which have errors.
    let mut errored_files = HashSet::new();
    let mut all_touched_files = HashSet::new();
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
                if diag.severity == Severity::Error
                    && let Ok(uri) = Url::from_file_path(&diag.file)
                {
                    errored_files.insert(uri);
                }
            }
        }
        let by_file = group_by_file(&result.diagnostics);
        for touched in &result.touched_files {
            if let Ok(uri) = Url::from_file_path(touched) {
                let diags = by_file.get(&uri).cloned().unwrap_or_default();
                publish_and_store(client, diagnostic_store, uri, diags).await;
            }
        }
        all_touched_files.extend(result.touched_files.iter().cloned());
    }
    (errored_files, all_touched_files)
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
/// ## Step 3: `compile_resolved_errors` — produce JS for resolved errors
///
/// If step 2 successfully typechecked modules that were previously at
/// `CompileError` from a `FullCompile` (i.e. the user saved that file
/// but it failed), those modules now need full compilation to produce
/// JS output. Without this step, they'd be stuck at `TypeChecked` with
/// missing JavaScript.
///
/// Modules whose `CompileError` came from `TypecheckOnly` (they were
/// only typechecked as dependents, never saved) are not snapshotted —
/// reaching `TypeChecked` in step 2 is already their intended state.
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
/// Step 3 exists because step 2 only typechecks (no JS). Modules that
/// previously had JS or that the user had saved need full recompilation
/// to restore or produce their JavaScript output.
#[instrument(name = "lsp.flush.file_build.batch", skip_all, fields(modules = ?module_names, error_count = tracing::field::Empty))]
fn build_batch(
    build_state: &mut BuildCommandState,
    module_names: Vec<String>,
    intent: Option<HashSet<String>>,
) -> BatchBuildResult {
    let mut diagnostics = Vec::new();
    let mut touched_files = HashSet::new();
    let mut skipped_modules = HashSet::new();

    if !module_names.is_empty() {
        // Steps 1–3: compile saved files, typecheck dependents, resolve errors.
        let dep_result = compile_dependencies(build_state, &module_names);
        diagnostics.extend(dep_result.diagnostics);
        touched_files.extend(dep_result.touched_files);
        skipped_modules = dep_result.skipped_modules;

        // Snapshot modules at CompileError(FullCompile) before typechecking
        // dependents. These are modules the user saved but that failed to
        // compile. If step 2 resolves the error, step 3 needs to emit JS.
        let needs_rebuild_after_typecheck: AHashSet<String> = build_state
            .build_state
            .modules
            .iter()
            .filter_map(|(name, module)| match module {
                Module::SourceFile(sf)
                    if matches!(
                        sf.compilation_stage(),
                        CompilationStage::CompileError {
                            compile_mode: CompileMode::FullCompile,
                            ..
                        }
                    ) =>
                {
                    Some(name.clone())
                }
                _ => None,
            })
            .collect();

        let (dep_diagnostics, dep_touched) = typecheck_dependents(build_state, &module_names);
        diagnostics.extend(dep_diagnostics);
        touched_files.extend(dep_touched);

        // Step 3: re-compile modules that were at CompileError(FullCompile) and are
        // now at TypeChecked after step 2 (meaning a dependency fix resolved the
        // error). These need full compilation to produce the JS the user requested.
        let (rebuild_diagnostics, rebuild_touched) =
            compile_resolved_errors(build_state, &needs_rebuild_after_typecheck);
        diagnostics.extend(rebuild_diagnostics);
        touched_files.extend(rebuild_touched);
    }

    // Step 4: drain full_compile_intent — compile intent modules that weren't
    // already handled by steps 1–3, plus any modules skipped because step 1
    // broke early due to a dependency error. Modules already at Built are
    // skipped. Modules not yet at TypeChecked (e.g. still at CompileError)
    // are kept for future flushes.
    let mut intent_names = intent.unwrap_or_default();
    if !skipped_modules.is_empty() {
        tracing::debug!(
            skipped = ?skipped_modules.iter().collect::<Vec<_>>(),
            "build_batch: registering skipped modules as FullCompile intent"
        );
        intent_names.extend(skipped_modules);
    }
    let remaining_intent = if intent_names.is_empty() {
        HashSet::new()
    } else {
        drain_full_compile_intent(build_state, intent_names, &mut diagnostics, &mut touched_files)
    };

    record_error_count(&diagnostics);

    BatchBuildResult {
        diagnostics,
        touched_files,
        remaining_intent,
    }
}

/// Compile the saved modules and all their transitive imports to JS.
///
/// Collects every module the saved files import (recursively) and compiles
/// them, restricting to only the dependency closure.
/// This produces type information and JavaScript output for just the
/// saved file and its imports, without touching the rest of the codebase.
/// Result of the compile_dependencies step.
struct CompileDependenciesResult {
    diagnostics: Vec<BscDiagnostic>,
    touched_files: HashSet<PathBuf>,
    /// Modules that were in the dependency closure but never attempted by bsc
    /// because the compile loop broke early (a dependency had errors).
    skipped_modules: HashSet<String>,
}

#[instrument(name = "lsp.flush.file_build.compile_dependencies", skip_all, fields(error_count = tracing::field::Empty))]
fn compile_dependencies(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> CompileDependenciesResult {
    use crate::build::build_types::BuildConfig;

    let build_config = BuildConfig {
        output: OutputTarget::Lsp,
        mode: CompileMode::FullCompile,
        output_mode: OutputMode::Silent,
    };

    // Parse dirty files and resolve deps so the dependency closure
    // reflects the saved content, not stale state.
    let parse_result = build::parse_and_resolve(build_state, &build_config, Some(std::time::Duration::ZERO));

    let parse_diagnostics = match parse_result {
        Ok(warnings) => {
            if warnings.is_empty() {
                Vec::new()
            } else {
                build::diagnostics::parse_compiler_output(&warnings)
            }
        }
        Err(e) => {
            // Parse failure — return diagnostics for the affected files, skip compilation
            let touched: HashSet<PathBuf> = e.diagnostics.iter().map(|d| d.file.clone()).collect();
            return CompileDependenciesResult {
                diagnostics: e.diagnostics,
                touched_files: touched,
                skipped_modules: HashSet::new(),
            };
        }
    };

    let module_set: AHashSet<String> = module_names.iter().cloned().collect();
    let (diagnostics, touched_files, skipped_modules) =
        match build::compile_dependencies::compile_dependencies(build_state, &module_set) {
            Ok(result) => {
                tracing::debug!(
                    compiled_modules = ?result.modules.iter().collect::<Vec<_>>(),
                    "compile_dependencies: compiled module closure"
                );
                (
                    result.diagnostics,
                    module_names_to_paths(build_state, &result.modules),
                    HashSet::new(),
                )
            }
            Err(e) => {
                tracing::warn!("Incremental build completed with errors: {e}");
                let skipped: HashSet<String> = e.skipped_modules.into_iter().collect();
                (
                    e.diagnostics,
                    module_names_to_paths(build_state, &e.modules),
                    skipped,
                )
            }
        };

    let mut all_diagnostics = parse_diagnostics;
    all_diagnostics.extend(diagnostics);
    record_error_count(&all_diagnostics);

    CompileDependenciesResult {
        diagnostics: all_diagnostics,
        touched_files,
        skipped_modules,
    }
}

/// Re-typecheck all modules that import the saved modules (recursively).
///
/// When a file's public API changes, modules that import it may now have
/// type errors. This step finds all such importers and re-typechecks them
/// to surface those errors as diagnostics. No JavaScript is emitted —
/// that happens when those files are themselves saved.
#[instrument(name = "lsp.flush.file_build.typecheck_dependents", skip_all, fields(dependent_count = tracing::field::Empty, error_count = tracing::field::Empty))]
fn typecheck_dependents(
    build_state: &mut BuildCommandState,
    module_names: &[String],
) -> (Vec<BscDiagnostic>, HashSet<PathBuf>) {
    let module_set: AHashSet<String> = module_names.iter().cloned().collect();

    let (diagnostics, touched_files) =
        match build::typecheck_dependents::typecheck_dependents(build_state, &module_set) {
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

    record_error_count(&diagnostics);

    (diagnostics, touched_files)
}

/// Compile modules whose errors were resolved by step 2.
///
/// The caller passes modules that were at `CompileError(FullCompile)`
/// before step 2 ran. This function filters to those now at `TypeChecked`
/// (meaning the typecheck succeeded after a dependency fix), then runs a
/// full compile to produce JS output. If none were resolved, this is a
/// no-op (but the span is still emitted for observability).
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

    let module_set: AHashSet<String> = resolved.iter().cloned().collect();

    let (diagnostics, touched_files) =
        match build::compile_dependencies::compile_dependencies(build_state, &module_set) {
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

    record_error_count(&diagnostics);

    (diagnostics, touched_files)
}

/// Step 4 of `build_batch`: drain full_compile_intent modules.
///
/// Filters out modules already at `Built` (compiled in steps 1–3 or via hash
/// promotion). Compiles remaining `TypeChecked` modules with `FullCompile`.
/// Returns module names that couldn't be compiled yet (e.g. still at
/// `CompileError`) for future flushes.
#[instrument(name = "lsp.flush.file_build.drain_intent", skip_all, fields(intent_count = names.len()))]
fn drain_full_compile_intent(
    build_state: &mut BuildCommandState,
    names: HashSet<String>,
    diagnostics: &mut Vec<BscDiagnostic>,
    touched_files: &mut HashSet<PathBuf>,
) -> HashSet<String> {
    // Partition intent modules into three buckets:
    // - already Built (skip — steps 1–3 or hash promotion handled them)
    // - TypeChecked (compile now)
    // - other (keep for future flushes)
    let mut pending = AHashSet::new();
    let mut not_ready = HashSet::new();

    let mut already_built = Vec::new();
    let mut discarded = Vec::new();

    for name in names {
        match build_state.build_state.modules.get(&name) {
            Some(Module::SourceFile(sf)) => match sf.compilation_stage() {
                CompilationStage::Built(..) => {
                    already_built.push(name);
                }
                CompilationStage::TypeChecked { .. } => {
                    pending.insert(name);
                }
                other => {
                    tracing::debug!(
                        module = %name,
                        stage = ?std::mem::discriminant(other),
                        "drain_intent: module not ready"
                    );
                    not_ready.insert(name);
                }
            },
            _ => {
                discarded.push(name);
            }
        }
    }

    tracing::debug!(
        built_count = already_built.len(),
        pending_count = pending.len(),
        not_ready_count = not_ready.len(),
        discarded_count = discarded.len(),
        built = ?already_built,
        pending_modules = ?pending.iter().collect::<Vec<_>>(),
        not_ready_modules = ?not_ready.iter().collect::<Vec<_>>(),
        "drain_intent: partition"
    );

    if pending.is_empty() {
        return not_ready;
    }

    tracing::debug!(
        modules = ?pending.iter().collect::<Vec<_>>(),
        "drain_intent: compiling {} modules",
        pending.len()
    );

    match build::compile_dependencies::compile_dependencies(build_state, &pending) {
        Ok(result) => {
            tracing::debug!(
                compiled_modules = ?result.modules.iter().collect::<Vec<_>>(),
                "drain_intent: compiled"
            );
            diagnostics.extend(result.diagnostics);
            touched_files.extend(module_names_to_paths(build_state, &result.modules));
        }
        Err(e) => {
            tracing::warn!("drain_intent completed with errors: {e}");
            if !e.skipped_modules.is_empty() {
                tracing::debug!(
                    skipped = ?e.skipped_modules.iter().collect::<Vec<_>>(),
                    "drain_intent: skipped modules from compile_dependencies"
                );
            }
            // Keep failed modules in intent for next attempt.
            let mut became_built = Vec::new();
            for name in &pending {
                if build_state.build_state.modules.get(name).is_some_and(|m| {
                    !matches!(
                        m,
                        Module::SourceFile(sf)
                            if matches!(sf.compilation_stage(), CompilationStage::Built(..))
                    )
                }) {
                    not_ready.insert(name.clone());
                } else {
                    became_built.push(name.clone());
                }
            }
            if !became_built.is_empty() {
                tracing::debug!(
                    modules = ?became_built,
                    "drain_intent: modules reached Built despite errors (dropped from intent)"
                );
            }
            diagnostics.extend(e.diagnostics);
            touched_files.extend(module_names_to_paths(build_state, &e.modules));
        }
    }

    not_ready
}

/// Record the error count on the current tracing span if there are any errors.
fn record_error_count(diagnostics: &[BscDiagnostic]) {
    let error_count = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .count();
    if error_count > 0 {
        tracing::Span::current().record("error_count", error_count);
    }
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
