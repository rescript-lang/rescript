//! Serialized work queue for daemon build operations.
//!
//! All state-mutating operations (build, clean, file-change-triggered rebuilds,
//! config changes) are submitted as `WorkItem`s to a single mpsc channel. The
//! `run_work_loop` function processes them sequentially, ensuring no two operations
//! run concurrently on the `BuildState`.
//!
//! Read-only operations (get_compiler_args, ping, flush) bypass the queue.

use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;

use tracing::info_span;

use ahash::AHashSet;
use tokio::sync::{mpsc, oneshot};

use crate::build::{self, BuildReporter, clean};
use crate::helpers;

use super::proto::BuildType;
use super::reporters::BroadcastReporter;
use super::state::{CompileType, DaemonState, InternalFileChange};

use super::file_change_handler::mark_file_dirty;

/// Context for executing a build operation within spawn_blocking.
struct BuildContext {
    state: Arc<DaemonState>,
    reporter: BroadcastReporter,
}

impl BuildContext {
    fn new(state: Arc<DaemonState>, client_id: u64) -> Self {
        let reporter = BroadcastReporter::new(state.clients.event_tx.clone(), client_id);
        Self { state, reporter }
    }

    fn root(&self) -> &Path {
        &self.state.root
    }

    fn lock_build_state(&self) -> std::sync::MutexGuard<'_, build::build_types::BuildState> {
        self.state.build_state.lock().unwrap_or_else(|e| e.into_inner())
    }
}

/// Performs the standard build sequence: read compile state, cleanup, incremental build, cleanup after, write ninja.
fn execute_standard_build(
    build_state: &mut build::build_types::BuildState,
    warn_error: Option<String>,
    filter: Option<regex::Regex>,
    reporter: &BroadcastReporter,
) -> Result<usize, String> {
    let _span = info_span!("build.execute_standard_build").entered();

    let compile_assets_state = {
        let _span = info_span!("build.read_compile_state").entered();
        build::read_compile_state::read(build_state, reporter).map_err(|e| format!("{:#}", e))?
    };

    let (cleaned, total) = {
        let _span = info_span!("build.cleanup_previous").entered();
        clean::cleanup_previous_build(build_state, compile_assets_state, reporter)
    };
    reporter.report(build::BuildProgress::Cleaned {
        cleaned_count: cleaned as i32,
        total_count: total as i32,
        duration_seconds: 0.0,
        due_to_compiler_update: false,
    });

    let result = {
        let _span = info_span!(
            "build.incremental_build",
            module_count = build_state.modules.len(),
        )
        .entered();
        build::incremental_build(build_state, warn_error, filter, None, true, reporter)
    };

    {
        let _span = info_span!("build.cleanup_after").entered();
        clean::cleanup_after_build(build_state);
    }

    {
        let _span = info_span!("build.write_ninja").entered();
        build::write_build_ninja(build_state);
    }

    handle_build_result(result, build_state.modules.len(), reporter)
}

/// Handles the result of incremental_build, converting to Result<usize, String>.
fn handle_build_result(
    result: Result<(), build::IncrementalBuildError>,
    module_count: usize,
    reporter: &BroadcastReporter,
) -> Result<usize, String> {
    match result {
        Ok(()) => Ok(module_count),
        Err(e) => {
            if let build::IncrementalBuildErrorKind::CompileError(Some(msg)) = &e.kind {
                reporter.report(build::BuildProgress::CompilerError(msg.clone()));
            }
            Err(e.to_string())
        }
    }
}

/// Emits build finished event and converts spawn_blocking result to WorkResult.
fn emit_work_result(
    state: &Arc<DaemonState>,
    client_id: u64,
    duration: f64,
    result: Result<Result<usize, String>, tokio::task::JoinError>,
    is_clean: bool,
) -> WorkResult {
    match result {
        Ok(Ok(module_count)) => {
            state.emit_build_finished(
                client_id,
                true,
                duration,
                Some(module_count as i32),
                None,
                is_clean,
            );
            WorkResult::Success {
                duration_seconds: duration,
                module_count: Some(module_count),
            }
        }
        Ok(Err(error)) => {
            state.emit_build_finished(client_id, false, duration, None, Some(error), is_clean);
            WorkResult::Failure {
                duration_seconds: duration,
            }
        }
        Err(e) => {
            let error = format!("Build task panicked: {}", e);
            state.emit_build_finished(client_id, false, duration, None, Some(error), is_clean);
            WorkResult::Failure {
                duration_seconds: duration,
            }
        }
    }
}

/// Result of a completed work item.
pub enum WorkResult {
    Success {
        duration_seconds: f64,
        module_count: Option<usize>,
    },
    Failure {
        duration_seconds: f64,
    },
}

/// A unit of work submitted to the queue.
pub enum WorkItem {
    /// Full build triggered by a build RPC.
    Build {
        client_id: u64,
        working_directory: String,
        warn_error: Option<String>,
        filter: Option<regex::Regex>,
        completion_tx: oneshot::Sender<WorkResult>,
        parent_span: tracing::Span,
    },

    /// Clean: remove artifacts from disk AND reset in-memory state.
    Clean {
        client_id: u64,
        working_directory: String,
        completion_tx: oneshot::Sender<WorkResult>,
        parent_span: tracing::Span,
    },

    /// Initial build for a watch client.
    WatchInitialBuild {
        client_id: u64,
        completion_tx: oneshot::Sender<WorkResult>,
        parent_span: tracing::Span,
    },

    /// Incremental build triggered by file changes (from file_change_handler).
    FileChangeBuild {
        client_id: u64,
        changes: Vec<InternalFileChange>,
        parent_span: tracing::Span,
    },

    /// Config change: re-read config and trigger rebuild.
    ConfigChange { config_path: PathBuf },

    /// Collect files for formatting (brief read of build state).
    FormatCollectFiles {
        client_id: u64,
        working_directory: PathBuf,
        completion_tx: oneshot::Sender<Vec<String>>,
        parent_span: tracing::Span,
    },
}

/// Precondition: ensure packages are discovered and sources are loaded for the given scope.
/// This is the single place that handles "is this the first request?" and
/// "do we have sources for these packages?" logic.
fn ensure_state_ready(
    build_state: &mut build::build_types::BuildState,
    scope: Option<&AHashSet<String>>,
    reporter: &impl BuildReporter,
) -> anyhow::Result<()> {
    let _span = info_span!("build.ensure_state_ready", scoped = scope.is_some(),).entered();

    {
        let _span = info_span!("build.initialize_packages").entered();
        if let Err(e) = build_state.initialize_packages(&None, reporter) {
            reporter.report(build::BuildProgress::InitializationError(format!("{:#}", e)));
            return Err(e);
        }
    }

    {
        let _span = info_span!("build.ensure_packages_loaded").entered();
        if let Err(e) = build_state.ensure_packages_loaded(scope, reporter) {
            reporter.report(build::BuildProgress::InitializationError(format!("{:#}", e)));
            return Err(e);
        }
    }

    Ok(())
}

/// Clear sources and modules from build state, forcing a full rescan on next load.
fn clear_sources_and_modules(build_state: &mut build::build_types::BuildState) {
    for package in build_state.packages.values_mut() {
        if package.is_local && package.sources.is_some() {
            package.sources = None;
        }
    }
    build_state.modules.clear();
    build_state.module_names.clear();
    build_state.deps_initialized = false;
}

/// Compute the build scope from working directory.
fn compute_scope(
    working_directory: &str,
    root: &Path,
    build_state: &build::build_types::BuildState,
) -> Option<AHashSet<String>> {
    let working_dir_path = PathBuf::from(working_directory);
    let scope_package = helpers::get_scope_package_from_working_dir(&working_dir_path, root);
    scope_package.map(|pkg| build::packages::compute_build_scope(&build_state.packages, &pkg))
}

/// The work loop that processes work items sequentially.
/// Only this loop locks the build state mutex — no other code path should.
pub async fn run_work_loop(state: Arc<DaemonState>, mut work_rx: mpsc::Receiver<WorkItem>) {
    while let Some(item) = work_rx.recv().await {
        match item {
            WorkItem::Build {
                client_id,
                working_directory,
                warn_error,
                filter,
                completion_tx,
                parent_span,
            } => {
                let result = handle_build(
                    &state,
                    client_id,
                    working_directory,
                    warn_error,
                    filter,
                    parent_span,
                )
                .await;
                let _ = completion_tx.send(result);
            }

            WorkItem::Clean {
                client_id,
                working_directory,
                completion_tx,
                parent_span,
            } => {
                let result = handle_clean(&state, client_id, working_directory, parent_span).await;
                let _ = completion_tx.send(result);
            }

            WorkItem::WatchInitialBuild {
                client_id,
                completion_tx,
                parent_span,
            } => {
                let result = handle_watch_initial_build(&state, client_id, parent_span).await;
                let _ = completion_tx.send(result);
            }

            WorkItem::FileChangeBuild {
                client_id,
                changes,
                parent_span,
            } => {
                handle_file_change_build(&state, client_id, changes, parent_span).await;
            }

            WorkItem::ConfigChange { config_path } => {
                handle_config_change(&state, config_path).await;
            }

            WorkItem::FormatCollectFiles {
                client_id,
                working_directory,
                completion_tx,
                parent_span,
            } => {
                let files =
                    handle_format_collect_files(&state, client_id, working_directory, parent_span).await;
                let _ = completion_tx.send(files);
            }
        }
    }
}

async fn handle_build(
    state: &Arc<DaemonState>,
    client_id: u64,
    working_directory: String,
    warn_error: Option<String>,
    filter: Option<regex::Regex>,
    parent_span: tracing::Span,
) -> WorkResult {
    // Create span as child of the RPC span
    let span = tracing::info_span!(
        parent: &parent_span,
        "work_queue.handle_build",
        client_id = client_id,
        working_dir = %working_directory,
        has_filter = filter.is_some(),
    );

    let start = Instant::now();
    let ctx = BuildContext::new(state.clone(), client_id);

    // Clone the span to move into spawn_blocking - use in_scope to establish parent-child relationships
    let span_clone = span.clone();
    let result = tokio::task::spawn_blocking(move || {
        span_clone.in_scope(|| {
            let root = ctx.root().to_path_buf();
            let mut guard = ctx.lock_build_state();
            let build_state = &mut *guard;

            // Precondition: rescan sources from disk (build RPC = full build)
            {
                let _span = info_span!("build.clear_sources_and_modules").entered();
                clear_sources_and_modules(build_state);
            }

            // Ensure packages are discovered before computing scope,
            // so compute_build_scope can walk transitive dependencies.
            {
                let _span = info_span!("build.initialize_packages").entered();
                if let Err(e) = build_state.initialize_packages(&None, &ctx.reporter) {
                    let msg = format!("{:#}", e);
                    ctx.reporter
                        .report(build::BuildProgress::InitializationError(msg.clone()));
                    return Err(msg);
                }
            }

            let scope = {
                let _span = info_span!(
                    "build.compute_scope",
                    working_dir = %working_directory,
                    root = %root.display(),
                )
                .entered();
                let scope = compute_scope(&working_directory, &root, build_state);
                // Record scope info as span event
                if let Some(ref s) = scope {
                    tracing::info!(scope_packages = s.len(), "computed build scope");
                } else {
                    tracing::info!("no scope restriction (building all packages)");
                }
                scope
            };
            tracing::debug!(
                working_dir = ?working_directory,
                root = ?root,
                scope = ?scope,
                "Build scope"
            );

            {
                let _span = info_span!("build.ensure_packages_loaded", scoped = scope.is_some(),).entered();
                if let Err(e) = build_state.ensure_packages_loaded(scope.as_ref(), &ctx.reporter) {
                    let msg = format!("{:#}", e);
                    ctx.reporter
                        .report(build::BuildProgress::InitializationError(msg.clone()));
                    return Err(msg);
                }
            }

            execute_standard_build(build_state, warn_error, filter, &ctx.reporter)
        })
    })
    .await;

    emit_work_result(state, client_id, start.elapsed().as_secs_f64(), result, false)
}

async fn handle_clean(
    state: &Arc<DaemonState>,
    client_id: u64,
    working_directory: String,
    parent_span: tracing::Span,
) -> WorkResult {
    let span = tracing::info_span!(
        parent: &parent_span,
        "work_queue.handle_clean",
        client_id = client_id,
        working_dir = %working_directory,
    );

    let start = Instant::now();
    let state_clone = state.clone();
    let event_tx = state.clients.event_tx.clone();

    let span_clone = span.clone();
    let result = tokio::task::spawn_blocking(move || {
        span_clone.in_scope(|| {
            let reporter = BroadcastReporter::new(event_tx, client_id);
            let root = &state_clone.root;

            // Precondition: need packages discovered for scoping
            {
                let _span = info_span!("clean.initialize_packages").entered();
                let mut guard = state_clone.build_state.lock().unwrap_or_else(|e| e.into_inner());
                if let Err(e) = guard.initialize_packages(&None, &reporter) {
                    let msg = format!("{:#}", e);
                    reporter.report(build::BuildProgress::InitializationError(msg.clone()));
                    return Err(msg);
                }
            }

            // Operation: clean filesystem
            let working_dir_path = PathBuf::from(&working_directory);
            let scope_package = helpers::get_scope_package_from_working_dir(&working_dir_path, root);

            {
                // Use "root" as sentinel value when not scoped to a specific package
                let scope_value = scope_package.as_deref().unwrap_or("root");
                let _span = info_span!("clean.execute", scope = scope_value,).entered();
                if let Err(e) = clean::clean(root, scope_package.as_deref(), &reporter) {
                    return Err(format!("{:#}", e));
                }
            }

            // Post-operation: invalidate in-memory state
            {
                let _span = info_span!("clean.clear_state").entered();
                let mut guard = state_clone.build_state.lock().unwrap_or_else(|e| e.into_inner());
                clear_sources_and_modules(&mut guard);
            }

            Ok(())
        })
    })
    .await;

    let duration = start.elapsed().as_secs_f64();
    match result {
        Ok(Ok(())) => {
            state.emit_build_finished(client_id, true, duration, None, None, true);
            WorkResult::Success {
                duration_seconds: duration,
                module_count: None,
            }
        }
        Ok(Err(error)) => {
            state.emit_build_finished(client_id, false, duration, None, Some(error), true);
            WorkResult::Failure {
                duration_seconds: duration,
            }
        }
        Err(e) => {
            let error = format!("Clean task panicked: {}", e);
            state.emit_build_finished(client_id, false, duration, None, Some(error), true);
            WorkResult::Failure {
                duration_seconds: duration,
            }
        }
    }
}

async fn handle_watch_initial_build(
    state: &Arc<DaemonState>,
    client_id: u64,
    parent_span: tracing::Span,
) -> WorkResult {
    let span = tracing::info_span!(
        parent: &parent_span,
        "work_queue.watch_initial_build",
        client_id = client_id,
    );

    let start = Instant::now();
    let ctx = BuildContext::new(state.clone(), client_id);

    let span_clone = span.clone();
    let result = tokio::task::spawn_blocking(move || {
        span_clone.in_scope(|| {
            let mut guard = ctx.lock_build_state();
            let build_state = &mut *guard;

            // Precondition: watch mode needs all packages loaded
            if let Err(e) = ensure_state_ready(build_state, None, &ctx.reporter) {
                return Err(format!("{:#}", e));
            }

            execute_standard_build(build_state, None, None, &ctx.reporter)
        })
    })
    .await;

    let duration = start.elapsed().as_secs_f64();

    // Watch initial build also emits watch paths on success
    if matches!(&result, Ok(Ok(_))) {
        state.emit_watch_paths(client_id);
    }

    emit_work_result(state, client_id, duration, result, false)
}

async fn handle_file_change_build(
    state: &Arc<DaemonState>,
    client_id: u64,
    changes: Vec<InternalFileChange>,
    parent_span: tracing::Span,
) {
    let span = tracing::info_span!(
        parent: &parent_span,
        "work_queue.handle_file_change_build",
        client_id = client_id,
        change_count = changes.len(),
    );

    // Emit a child span for each file change to provide visibility into what triggered the build
    for change in &changes {
        let change_type = match change.compile_type {
            CompileType::Incremental => "modified",
            CompileType::SourceCreated => "created",
            CompileType::SourceDeleted => "deleted",
            CompileType::SourceRenamed => "renamed",
            CompileType::ConfigChange => "config_changed",
        };
        let _file_span = tracing::info_span!(
            parent: &span,
            "file_change",
            path = %change.path,
            change_type = change_type,
        )
        .entered();
    }

    let start = Instant::now();
    let ctx = BuildContext::new(state.clone(), client_id);

    let needs_config_rebuild = changes
        .iter()
        .any(|c| c.compile_type == CompileType::ConfigChange);
    let needs_sources_rescan = changes.iter().any(|c| c.compile_type.needs_source_rescan());
    let needs_full = needs_config_rebuild || needs_sources_rescan;

    let build_type = if needs_full {
        BuildType::BuildFull
    } else {
        BuildType::BuildIncremental
    };
    state.emit_build_started(client_id, build_type, Some(changes.len() as i32), false);

    let span_clone = span.clone();
    let result = tokio::task::spawn_blocking(move || {
        span_clone.in_scope(|| {
            let mut guard = ctx.lock_build_state();
            let build_state = &mut *guard;

            // Precondition: if sources changed, clear and reload
            if needs_sources_rescan || needs_config_rebuild {
                let rescan_reasons: Vec<_> = changes
                    .iter()
                    .filter(|c| {
                        c.compile_type.needs_source_rescan() || c.compile_type == CompileType::ConfigChange
                    })
                    .map(|c| format!("{:?} {}", c.compile_type, c.path))
                    .collect();
                tracing::debug!(reasons = %rescan_reasons.join(", "), "Re-scanning sources");
                clear_sources_and_modules(build_state);
            }

            if let Err(e) = ensure_state_ready(build_state, None, &ctx.reporter) {
                return Err(format!("{:#}", e));
            }

            // Operation: build based on change type
            let build_result = if needs_config_rebuild {
                execute_config_change_build(build_state, &ctx.reporter)
            } else if needs_sources_rescan {
                execute_sources_rescan_build(build_state, &ctx.reporter)
            } else {
                execute_incremental_build(build_state, &changes, &ctx.reporter)
            };

            build_result.map_err(|e| {
                if let build::IncrementalBuildErrorKind::CompileError(Some(msg)) = &e.kind {
                    ctx.reporter
                        .report(build::BuildProgress::CompilerError(msg.clone()));
                }
                e.to_string()
            })
        })
    })
    .await;

    let duration = start.elapsed().as_secs_f64();

    // File change builds emit watch paths on success if sources changed
    let success = matches!(&result, Ok(Ok(())));
    emit_file_change_result(state, client_id, duration, result);

    if success && needs_full {
        state.emit_watch_paths(client_id);
    }
}

/// Build triggered by config change: skip cleanup_previous_build (stale timestamps).
fn execute_config_change_build(
    build_state: &mut build::build_types::BuildState,
    reporter: &BroadcastReporter,
) -> Result<(), build::IncrementalBuildError> {
    reporter.report(build::BuildProgress::Cleaned {
        cleaned_count: 0,
        total_count: build_state.modules.len() as i32,
        duration_seconds: 0.0,
        due_to_compiler_update: false,
    });

    let result = build::incremental_build(build_state, None, None, None, true, reporter);
    clean::cleanup_after_build(build_state);
    build::write_build_ninja(build_state);
    result
}

/// Build triggered by source file creation/deletion/rename.
fn execute_sources_rescan_build(
    build_state: &mut build::build_types::BuildState,
    reporter: &BroadcastReporter,
) -> Result<(), build::IncrementalBuildError> {
    let compile_assets_state =
        build::read_compile_state::read(build_state, reporter).map_err(|e| build::IncrementalBuildError {
            kind: build::IncrementalBuildErrorKind::InitializationError(format!("{:#}", e)),
        })?;

    let (cleaned, total) = clean::cleanup_previous_build(build_state, compile_assets_state, reporter);
    reporter.report(build::BuildProgress::Cleaned {
        cleaned_count: cleaned as i32,
        total_count: total as i32,
        duration_seconds: 0.0,
        due_to_compiler_update: false,
    });

    let result = build::incremental_build(build_state, None, None, None, true, reporter);
    clean::cleanup_after_build(build_state);
    build::write_build_ninja(build_state);
    result
}

/// Pure incremental build: mark changed files dirty and rebuild.
fn execute_incremental_build(
    build_state: &mut build::build_types::BuildState,
    changes: &[InternalFileChange],
    reporter: &BroadcastReporter,
) -> Result<(), build::IncrementalBuildError> {
    for change in changes {
        mark_file_dirty(build_state, Path::new(&change.path));
    }

    let result = build::incremental_build(build_state, None, None, None, true, reporter);
    clean::cleanup_after_build(build_state);
    result
}

/// Emits build finished for file change builds (returns () instead of module count).
fn emit_file_change_result(
    state: &Arc<DaemonState>,
    client_id: u64,
    duration: f64,
    result: Result<Result<(), String>, tokio::task::JoinError>,
) {
    match result {
        Ok(Ok(())) => {
            state.emit_build_finished(client_id, true, duration, None, None, false);
        }
        Ok(Err(error)) => {
            state.emit_build_finished(client_id, false, duration, None, Some(error), false);
        }
        Err(e) => {
            let error = format!("Build task panicked: {}", e);
            state.emit_build_finished(client_id, false, duration, None, Some(error), false);
        }
    }
}

async fn handle_config_change(state: &Arc<DaemonState>, config_path: PathBuf) {
    let span = info_span!(
        "work_queue.handle_config_change",
        config_path = %config_path.display(),
    );
    let _guard = span.enter();

    let state_clone = state.clone();

    let result = tokio::task::spawn_blocking(move || {
        let mut guard = state_clone.build_state.lock().unwrap_or_else(|e| e.into_inner());
        let build_state = &mut *guard;

        // Find the package that owns this config file
        let package_name = build_state
            .packages
            .values()
            .find(|p| p.path.join("rescript.json") == config_path)
            .map(|p| p.name.clone());

        let package_name = match package_name {
            Some(name) => name,
            None => return Err("No package found for config path".to_string()),
        };

        // Re-read the config from disk
        let package = build_state.packages.get_mut(&package_name).unwrap();
        let new_config = match crate::build::packages::read_config(&package.path) {
            Ok(config) => config,
            Err(e) => return Err(format!("Failed to read config: {:#}", e)),
        };

        // Update the package's config, source_folders, and clear sources
        let new_source_folders = match new_config.sources.clone() {
            Some(crate::config::OneOrMore::Single(source)) => {
                crate::build::packages::get_source_dirs(source, None)
            }
            Some(crate::config::OneOrMore::Multiple(sources)) => {
                let mut folders = AHashSet::new();
                for source in sources {
                    folders.extend(crate::build::packages::get_source_dirs(source, None));
                }
                folders
            }
            None => AHashSet::new(),
        };

        let is_root = package.is_root;
        package.config = new_config.clone();
        package.source_folders = new_source_folders;
        package.sources = None;

        if is_root {
            build_state.project_context.current_config = new_config;
            for pkg in build_state.packages.values_mut() {
                pkg.sources = None;
            }
        }

        build_state.modules.clear();
        build_state.module_names.clear();

        Ok(package_name)
    })
    .await;

    match result {
        Ok(Ok(package_name)) => {
            tracing::debug!(
                package = %package_name,
                "Config re-read, triggering full rebuild"
            );
        }
        Ok(Err(msg)) => {
            tracing::debug!(reason = %msg, "Config change rejected");
        }
        Err(e) => {
            tracing::info!(error = %e, "Config change task panicked");
        }
    }
}

async fn handle_format_collect_files(
    state: &Arc<DaemonState>,
    client_id: u64,
    working_directory: PathBuf,
    parent_span: tracing::Span,
) -> Vec<String> {
    let span = info_span!(
        parent: &parent_span,
        "work_queue.handle_format_collect_files",
        client_id = client_id,
        working_dir = %working_directory.display(),
    );

    let state_clone = state.clone();
    let event_tx = state.clients.event_tx.clone();

    // Clone the span to move into spawn_blocking - use in_scope to establish parent-child relationships
    let span_clone = span.clone();
    let result = tokio::task::spawn_blocking(move || {
        span_clone.in_scope(|| {
            let reporter = BroadcastReporter::new(event_tx, client_id);

            let mut guard = state_clone.build_state.lock().unwrap_or_else(|e| e.into_inner());
            let build_state = &mut *guard;

            // Precondition: ensure packages are loaded
            if let Err(e) = ensure_state_ready(build_state, None, &reporter) {
                tracing::debug!(error = %e, "Format precondition failed");
                return Vec::new();
            }

            // Determine which packages to format
            let packages_to_format: AHashSet<String> = {
                let mut matched_package: Option<&build::packages::Package> = None;
                for package in build_state.packages.values() {
                    if package.path == working_directory && package.is_local {
                        matched_package = Some(package);
                        break;
                    }
                }
                match matched_package {
                    Some(pkg) if !pkg.is_root => {
                        let mut set = AHashSet::new();
                        set.insert(pkg.name.clone());
                        set
                    }
                    _ => build_state.project_context.get_scoped_local_packages(),
                }
            };

            // Collect files to format
            let package_count = packages_to_format.len();
            let packages_list: Vec<&str> = packages_to_format.iter().map(|s| s.as_str()).collect();
            let _collect_span = info_span!(
                "format.collect_files",
                package_count = package_count,
                packages = ?packages_list,
            )
            .entered();

            let mut files: Vec<String> = Vec::new();
            for package in build_state.packages.values() {
                if packages_to_format.contains(&package.name)
                    && let Some(sources) = &package.sources
                {
                    for (path, _metadata) in &sources.files {
                        if let Some(extension) = path.extension()
                            && (extension == "res" || extension == "resi")
                        {
                            files.push(package.path.join(path).to_string_lossy().into_owned());
                        }
                    }
                }
            }

            tracing::info!(file_count = files.len(), "collected files for formatting");

            files
        })
    })
    .await;

    result.unwrap_or_default()
}
