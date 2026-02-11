//! Unified debounced queue for all LSP file events.
//!
//! All file events (didChange, didOpen, didSave, didChangeWatchedFiles) go
//! into a single unbounded channel. A background consumer task collects
//! events into a `HashMap<Url, PendingFile>`, applying promotion rules to
//! consolidate per-file state. After 100ms of silence the batch is flushed:
//!
//! 1. **Builds first** — saved files get a full incremental build (compile
//!    dependencies + typecheck dependents), holding the projects lock.
//! 2. **Typechecks second** — unsaved edits get a lightweight typecheck via
//!    `bsc -bs-read-stdin`, with brief lock for arg extraction only.
//! 3. **Post-build recheck** — if a saved file also had unsaved buffer
//!    content (didChange + didSave in the same window), a typecheck pass
//!    runs from the buffer so diagnostics match the editor.
//! 4. **`buildFinished` notification** — sent only when builds ran.
//!
//! Sequential execution within one consumer eliminates all races on
//! `lib/lsp/` artifacts.

use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use ahash::AHashSet;
use rayon::prelude::*;
use tokio::sync::mpsc;
use tokio::time::Instant;
use tower_lsp::Client;
use tower_lsp::lsp_types::Url;
use tracing::{Instrument, instrument};

use super::file_args::{BuildCommandStateExt, TypecheckArgs};
use super::{ProjectMap, group_by_file, notifications, to_lsp_diagnostic};
use crate::build;
use crate::build::build_types::{BuildCommandState, BuildProfile, CompilationStage, SourceType};
use crate::build::deps;
use crate::build::diagnostics::BscDiagnostic;
use crate::build::packages;
use crate::project_context::ProjectContext;

/// Monotonically increasing generation counter for staleness detection.
static GENERATION: AtomicU64 = AtomicU64::new(0);

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// What action a file needs in the next flush.
enum FileAction {
    /// Unsaved edit: typecheck only using buffer content, no JS output.
    Typecheck { content: String },
    /// Saved file: full incremental build from disk.
    /// `buffer_content` holds the latest unsaved content if a `didChange`
    /// arrived for this file in the same batch — used for post-build recheck.
    Build { buffer_content: Option<String> },
}

/// An event sent from LSP handlers into the unified queue channel.
enum QueueEvent {
    /// Unsaved buffer change (didChange / didOpen).
    Typecheck {
        uri: Url,
        file_path: PathBuf,
        content: String,
        generation: u64,
    },
    /// File saved to disk (didSave / didChangeWatchedFiles).
    Build { uri: Url, file_path: PathBuf },
    /// File created or deleted — requires full project re-initialization.
    FullBuild {
        project_root: PathBuf,
        deleted_uris: Vec<Url>,
    },
}

/// Per-file pending state accumulated between flushes.
struct PendingFile {
    uri: Url,
    file_path: PathBuf,
    action: FileAction,
    generation: u64,
}

/// All pending state accumulated between flushes.
struct PendingState {
    /// Per-file events (typecheck / incremental build).
    files: HashMap<Url, PendingFile>,
    /// Per-project full rebuilds (file creation / deletion).
    /// Maps project root → accumulated deleted URIs.
    full_builds: HashMap<PathBuf, Vec<Url>>,
}

/// Result of a batched build for one project.
struct BatchBuildResult {
    diagnostics: Vec<BscDiagnostic>,
    /// Absolute paths of all source files that were compiled or typechecked.
    touched_files: HashSet<PathBuf>,
}

/// Per-file metadata extracted from the build state under lock.
struct FileContext {
    uri: Url,
    content: String,
    generation: u64,
    file_args: TypecheckArgs,
}

/// Result of typechecking a single file.
struct TypecheckResult {
    uri: Url,
    generation: u64,
    diagnostics: Vec<BscDiagnostic>,
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Unified LSP queue. Accepts all file events and processes them
/// sequentially with debouncing.
pub struct Queue {
    tx: mpsc::UnboundedSender<QueueEvent>,
    /// Latest generation sent per URI, for staleness detection on typecheck results.
    generations: Arc<Mutex<HashMap<Url, u64>>>,
}

impl Queue {
    /// Create the queue and spawn the single background consumer task.
    pub fn new(projects: Arc<Mutex<ProjectMap>>, client: Client, root_span: tracing::Span) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        let generations = Arc::new(Mutex::new(HashMap::new()));
        tokio::spawn(consumer(rx, projects, Arc::clone(&generations), client).instrument(root_span));
        Queue { tx, generations }
    }

    /// Enqueue an unsaved buffer change for typecheck (didChange / didOpen).
    pub fn enqueue_typecheck(&self, uri: Url, file_path: PathBuf, content: String) {
        let generation = GENERATION.fetch_add(1, Ordering::Relaxed) + 1;
        match self.generations.lock() {
            Ok(mut gens) => {
                gens.insert(uri.clone(), generation);
            }
            Err(e) => tracing::error!("generations mutex poisoned: {e}"),
        }
        let _ = self.tx.send(QueueEvent::Typecheck {
            uri,
            file_path,
            content,
            generation,
        });
    }

    /// Enqueue a saved file for full build (didSave / didChangeWatchedFiles).
    pub fn enqueue_build(&self, uri: Url, file_path: PathBuf) {
        let _ = self.tx.send(QueueEvent::Build { uri, file_path });
    }

    /// Enqueue a full project re-initialization (file created / deleted).
    pub fn enqueue_full_build(&self, project_root: PathBuf, deleted_uris: Vec<Url>) {
        let _ = self.tx.send(QueueEvent::FullBuild {
            project_root,
            deleted_uris,
        });
    }
}

// ---------------------------------------------------------------------------
// Consumer loop
// ---------------------------------------------------------------------------

async fn consumer(
    mut rx: mpsc::UnboundedReceiver<QueueEvent>,
    projects: Arc<Mutex<ProjectMap>>,
    generations: Arc<Mutex<HashMap<Url, u64>>>,
    client: Client,
) {
    const DEBOUNCE: Duration = Duration::from_millis(100);
    let mut state = PendingState {
        files: HashMap::new(),
        full_builds: HashMap::new(),
    };

    loop {
        // Phase 1: wait for at least one event
        match rx.recv().await {
            Some(event) => state.merge(event),
            None => break,
        }

        // Phase 2: debounce — collect more events until 100ms of silence
        let deadline = tokio::time::sleep(DEBOUNCE);
        tokio::pin!(deadline);
        loop {
            tokio::select! {
                event = rx.recv() => {
                    match event {
                        Some(event) => {
                            state.merge(event);
                            deadline.as_mut().reset(Instant::now() + DEBOUNCE);
                        }
                        None => {
                            flush(&mut state, &projects, &generations, &client).await;
                            return;
                        }
                    }
                }
                _ = &mut deadline => break,
            }
        }

        // Phase 3: flush
        flush(&mut state, &projects, &generations, &client).await;
    }
}

impl PendingState {
    /// Merge a new event into the pending state, applying promotion rules.
    fn merge(&mut self, event: QueueEvent) {
        match event {
            QueueEvent::Typecheck {
                uri,
                file_path,
                content,
                generation,
            } => {
                match self.files.get_mut(&uri) {
                    Some(existing) => {
                        existing.generation = generation;
                        match &mut existing.action {
                            // Typecheck + Typecheck → keep latest content
                            FileAction::Typecheck { content: c } => {
                                *c = content;
                            }
                            // Build + Typecheck → stay Build, update buffer_content
                            FileAction::Build { buffer_content } => {
                                *buffer_content = Some(content);
                            }
                        }
                    }
                    None => {
                        self.files.insert(
                            uri.clone(),
                            PendingFile {
                                uri,
                                file_path,
                                action: FileAction::Typecheck { content },
                                generation,
                            },
                        );
                    }
                }
            }
            QueueEvent::Build { uri, file_path } => {
                match self.files.get_mut(&uri) {
                    Some(existing) => {
                        match &mut existing.action {
                            // Typecheck + Build → promote to Build, stash content
                            FileAction::Typecheck { content } => {
                                existing.action = FileAction::Build {
                                    buffer_content: Some(std::mem::take(content)),
                                };
                            }
                            // Build + Build → stay Build
                            FileAction::Build { .. } => {}
                        }
                    }
                    None => {
                        self.files.insert(
                            uri.clone(),
                            PendingFile {
                                uri,
                                file_path,
                                action: FileAction::Build { buffer_content: None },
                                generation: 0,
                            },
                        );
                    }
                }
            }
            QueueEvent::FullBuild {
                project_root,
                deleted_uris,
            } => {
                self.full_builds
                    .entry(project_root)
                    .or_default()
                    .extend(deleted_uris);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Flush orchestration
// ---------------------------------------------------------------------------

async fn flush(
    state: &mut PendingState,
    projects: &Arc<Mutex<ProjectMap>>,
    generations: &Arc<Mutex<HashMap<Url, u64>>>,
    client: &Client,
) {
    if state.files.is_empty() && state.full_builds.is_empty() {
        return;
    }

    // Step 0: Run full builds (file creation / deletion).
    // This also drops redundant incremental Build events from `state.files`.
    let has_full_builds = !state.full_builds.is_empty();
    if has_full_builds {
        run_full_build_flush(state, projects, client).await;
    }

    let entries: Vec<PendingFile> = state.files.drain().map(|(_, pf)| pf).collect();

    // Partition into build files and typecheck-only files
    let mut build_files: Vec<PendingFile> = Vec::new();
    let mut typecheck_files: Vec<PendingFile> = Vec::new();
    for pf in entries {
        match &pf.action {
            FileAction::Build { .. } => build_files.push(pf),
            FileAction::Typecheck { .. } => typecheck_files.push(pf),
        }
    }

    let has_incremental_builds = !build_files.is_empty();

    // Step 1: Run incremental builds (saved files)
    if has_incremental_builds {
        run_build_flush(&build_files, projects, client).await;
    }

    // Step 2: Run typechecks (unsaved edits)
    if !typecheck_files.is_empty() {
        run_typecheck_flush(typecheck_files, projects, generations, client).await;
    }

    // Step 3: Post-build recheck for files with buffer_content
    let recheck_files: Vec<PendingFile> = build_files
        .into_iter()
        .filter_map(|pf| {
            if let FileAction::Build {
                buffer_content: Some(content),
            } = pf.action
            {
                Some(PendingFile {
                    uri: pf.uri,
                    file_path: pf.file_path,
                    action: FileAction::Typecheck { content },
                    generation: pf.generation,
                })
            } else {
                None
            }
        })
        .collect();
    if !recheck_files.is_empty() {
        run_typecheck_flush(recheck_files, projects, generations, client).await;
    }

    // Step 4: Notify buildFinished (only if any builds ran)
    if has_full_builds || has_incremental_builds {
        client
            .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
            .await;
    }
}

// ---------------------------------------------------------------------------
// Full build flush (file creation / deletion)
// ---------------------------------------------------------------------------

/// Collect all source file URIs from a build state, for diagnostic clearing.
fn collect_source_uris(build_state: &BuildCommandState) -> HashSet<Url> {
    let mut uris = HashSet::new();
    for module in build_state.build_state.modules.values() {
        let SourceType::SourceFile(ref source_file) = module.source_type else {
            continue;
        };
        let Some(package) = build_state.build_state.packages.get(&module.package_name) else {
            continue;
        };
        let impl_path = package.path.join(&source_file.implementation.path);
        if let Ok(uri) = Url::from_file_path(&impl_path) {
            uris.insert(uri);
        }
        if let Some(ref interface) = source_file.interface {
            let iface_path = package.path.join(&interface.path);
            if let Ok(uri) = Url::from_file_path(&iface_path) {
                uris.insert(uri);
            }
        }
    }
    uris
}

/// Re-initialize a project from scratch (re-read packages, re-scan sources, rebuild).
/// Mirrors `initial_build::run()`.
#[instrument(name = "lsp.full_build", skip_all, fields(project = tracing::field::Empty))]
fn reinitialize_project(
    project_root: &Path,
    old_warn_error: Option<String>,
) -> Result<(BuildCommandState, Vec<BscDiagnostic>), String> {
    let project_context =
        ProjectContext::new(project_root).map_err(|e| format!("ProjectContext::new failed: {e}"))?;

    tracing::Span::current().record("project", project_context.get_root_config().name.as_str());

    let discovered_packages =
        packages::read_packages(&project_context, false).map_err(|e| format!("read_packages failed: {e}"))?;
    let packages_with_sources = packages::extend_with_children(&None, discovered_packages);

    let mut build_state = build::prepare_build(
        project_context,
        packages_with_sources,
        Some(std::time::Duration::ZERO),
        false,
        true,
        old_warn_error,
        BuildProfile::TypecheckOnly,
    )
    .map_err(|e| format!("prepare_build failed: {e}"))?;

    // TypecheckOnly doesn't produce .cmj files. Downgrade any Built modules.
    for module in build_state.build_state.modules.values_mut() {
        if module.compilation_stage == CompilationStage::Built {
            module.compilation_stage = CompilationStage::TypeChecked;
        }
    }

    let diagnostics = match build::incremental_build(
        &mut build_state,
        BuildProfile::TypecheckOnly,
        Some(std::time::Duration::ZERO),
        true,
        false,
        false,
        false,
        true,
    ) {
        Ok(result) => result.diagnostics,
        Err(e) => {
            tracing::warn!("Full build completed with errors: {e}");
            e.diagnostics
        }
    };

    Ok((build_state, diagnostics))
}

/// Result of a full build for one project.
struct FullBuildResult {
    diagnostics: Vec<BscDiagnostic>,
    old_uris: HashSet<Url>,
    new_uris: HashSet<Url>,
    deleted_uris: Vec<Url>,
}

async fn run_full_build_flush(state: &mut PendingState, projects: &Arc<Mutex<ProjectMap>>, client: &Client) {
    let entries: Vec<(PathBuf, Vec<Url>)> = state.full_builds.drain().collect();
    let projects_clone = Arc::clone(projects);

    let parent_span = tracing::Span::current();
    let results: Vec<FullBuildResult> = tokio::task::spawn_blocking(move || {
        let _entered = parent_span.enter();
        let mut results = Vec::new();

        for (project_root, deleted_uris) in entries {
            // Collect old URIs and warn_error under lock, then release
            let (old_uris, old_warn_error) = {
                let guard = match projects_clone.lock() {
                    Ok(g) => g,
                    Err(e) => {
                        tracing::error!("projects mutex poisoned in full build flush: {e}");
                        continue;
                    }
                };
                match guard.states.get(&project_root) {
                    Some(old_state) => (
                        collect_source_uris(old_state),
                        old_state.get_warn_error_override().clone(),
                    ),
                    None => {
                        tracing::warn!(
                            "No existing state for project root {}, skipping full build",
                            project_root.display()
                        );
                        continue;
                    }
                }
            };

            match reinitialize_project(&project_root, old_warn_error) {
                Ok((new_state, diagnostics)) => {
                    let new_uris = collect_source_uris(&new_state);

                    // Replace state under lock
                    if let Ok(mut guard) = projects_clone.lock() {
                        guard.states.insert(project_root.clone(), new_state);
                        // Invalidate uri_cache entries for this project
                        guard.uri_cache.retain(|_, root| root != &project_root);
                    }

                    results.push(FullBuildResult {
                        diagnostics,
                        old_uris,
                        new_uris,
                        deleted_uris,
                    });
                }
                Err(e) => {
                    tracing::error!("Full build failed for {}: {e}", project_root.display());
                }
            }
        }

        results
    })
    .await
    .unwrap_or_else(|e| {
        tracing::error!("full build flush task panicked: {e}");
        Vec::new()
    });

    // Publish diagnostics and clear stale ones
    for result in &results {
        let by_file = group_by_file(&result.diagnostics);

        // Publish diagnostics for all files in the new state
        for uri in &result.new_uris {
            let diags = by_file.get(uri).cloned().unwrap_or_default();
            client.publish_diagnostics(uri.clone(), diags, None).await;
        }

        // Clear diagnostics for files that existed in the old state but not the new
        for uri in &result.old_uris {
            if !result.new_uris.contains(uri) {
                client.publish_diagnostics(uri.clone(), vec![], None).await;
            }
        }

        // Clear diagnostics for explicitly deleted files
        for uri in &result.deleted_uris {
            client.publish_diagnostics(uri.clone(), vec![], None).await;
        }

        // Keep pending incremental Build events — the full build only does
        // TypecheckOnly, so saved files still need an incremental build to
        // emit JS output.
    }
}

// ---------------------------------------------------------------------------
// Build flush
// ---------------------------------------------------------------------------

async fn run_build_flush(build_files: &[PendingFile], projects: &Arc<Mutex<ProjectMap>>, client: &Client) {
    let file_paths: Vec<PathBuf> = build_files.iter().map(|pf| pf.file_path.clone()).collect();
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
                client.publish_diagnostics(uri, diags, None).await;
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
    use super::dependency_closure;

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
    use super::dependency_closure;

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

// ---------------------------------------------------------------------------
// Typecheck flush
// ---------------------------------------------------------------------------

async fn run_typecheck_flush(
    typecheck_files: Vec<PendingFile>,
    projects: &Arc<Mutex<ProjectMap>>,
    generations: &Arc<Mutex<HashMap<Url, u64>>>,
    client: &Client,
) {
    let requests: Vec<(Url, PathBuf, String, u64)> = typecheck_files
        .into_iter()
        .filter_map(|pf| {
            if let FileAction::Typecheck { content } = pf.action {
                Some((pf.uri, pf.file_path, content, pf.generation))
            } else {
                None
            }
        })
        .collect();

    if requests.is_empty() {
        return;
    }

    let projects_clone = Arc::clone(projects);
    let parent_span = tracing::Span::current();

    let results =
        tokio::task::spawn_blocking(move || batch_typecheck(requests, &projects_clone, &parent_span))
            .await
            .unwrap_or_else(|e| {
                tracing::error!("typecheck flush task panicked: {e}");
                Vec::new()
            });

    // Publish diagnostics for non-stale results
    for result in results {
        let is_stale = match generations.lock() {
            Ok(gens) => gens
                .get(&result.uri)
                .map(|&latest| result.generation < latest)
                .unwrap_or(false),
            Err(e) => {
                tracing::error!("generations mutex poisoned in staleness check: {e}");
                false
            }
        };

        if !is_stale {
            let diags: Vec<_> = result.diagnostics.iter().map(to_lsp_diagnostic).collect();
            client.publish_diagnostics(result.uri.clone(), diags, None).await;

            // Prune the generation entry if it still matches, so the map
            // doesn't grow unboundedly over long editing sessions.
            if let Ok(mut gens) = generations.lock()
                && gens.get(&result.uri) == Some(&result.generation)
            {
                gens.remove(&result.uri);
            }
        }
    }
}

fn batch_typecheck(
    requests: Vec<(Url, PathBuf, String, u64)>,
    projects: &Arc<Mutex<ProjectMap>>,
    parent_span: &tracing::Span,
) -> Vec<TypecheckResult> {
    let batch_span = tracing::info_span!(
        parent: parent_span,
        "lsp.typecheck",
        file_count = requests.len()
    );
    let _batch_entered = batch_span.enter();

    // Extract file contexts under lock (brief)
    let file_contexts: Vec<FileContext> = {
        let mut guard = match projects.lock() {
            Ok(g) => g,
            Err(e) => {
                tracing::error!("projects mutex poisoned in typecheck flush: {e}");
                return Vec::new();
            }
        };
        requests
            .into_iter()
            .filter_map(|(uri, file_path, content, generation)| {
                extract_file_context(&mut guard, uri, file_path, content, generation)
            })
            .collect()
    };
    // Lock is released here

    if file_contexts.is_empty() {
        return Vec::new();
    }

    // Single-file fast path: skip dependency analysis
    if file_contexts.len() == 1 {
        let ctx = &file_contexts[0];
        let child = tracing::info_span!(
            parent: &batch_span,
            "lsp.typecheck.file",
            module = %ctx.file_args.module_name
        );
        let _entered = child.enter();
        let diagnostics = typecheck_single_file(ctx);
        return vec![TypecheckResult {
            uri: ctx.uri.clone(),
            generation: ctx.generation,
            diagnostics,
        }];
    }

    // Multi-file batch: parse → compute deps → typecheck in waves
    batch_typecheck_multi(file_contexts)
}

fn extract_file_context(
    project_map: &mut ProjectMap,
    uri: Url,
    file_path: PathBuf,
    content: String,
    generation: u64,
) -> Option<FileContext> {
    let project_root = project_map.project_root_for(&uri)?;
    let build_state = project_map.states.get(&project_root)?;
    let file_args = build_state.get_typecheck_args(&file_path, &content, BuildProfile::TypecheckOnly)?;

    Some(FileContext {
        uri,
        content,
        generation,
        file_args,
    })
}

// ---------------------------------------------------------------------------
// Single-file fast path
// ---------------------------------------------------------------------------

fn typecheck_single_file(ctx: &FileContext) -> Vec<BscDiagnostic> {
    super::typecheck::typecheck_with_args(&ctx.file_args, &ctx.content)
}

// ---------------------------------------------------------------------------
// Multi-file batch: parse → deps → typecheck in waves
// ---------------------------------------------------------------------------

fn batch_typecheck_multi(file_contexts: Vec<FileContext>) -> Vec<TypecheckResult> {
    let mut results: Vec<TypecheckResult> = Vec::new();

    // Phase 1: Parse all files in parallel to produce .ast files
    let parse_span = tracing::info_span!("lsp.typecheck.parse", file_count = file_contexts.len());
    let parse_results: Vec<(usize, Result<(), Vec<BscDiagnostic>>)> = {
        let _entered = parse_span.enter();
        file_contexts
            .par_iter()
            .enumerate()
            .map(|(idx, ctx)| (idx, parse_file_to_ast(ctx, &parse_span)))
            .collect()
    };

    let mut successfully_parsed: Vec<usize> = Vec::new();
    for (idx, parse_result) in parse_results {
        match parse_result {
            Ok(()) => successfully_parsed.push(idx),
            Err(diags) => {
                results.push(TypecheckResult {
                    uri: file_contexts[idx].uri.clone(),
                    generation: file_contexts[idx].generation,
                    diagnostics: diags,
                });
            }
        }
    }

    if successfully_parsed.is_empty() {
        return results;
    }

    // Phase 2: Read deps from .ast files, compute in-batch ordering
    let in_batch_deps = compute_in_batch_deps(&file_contexts, &successfully_parsed);

    // Phase 3: Typecheck in waves (dependency order, parallel within waves)
    let typecheck_results = typecheck_in_waves(&file_contexts, &successfully_parsed, &in_batch_deps);
    results.extend(typecheck_results);

    results
}

fn parse_file_to_ast(ctx: &FileContext, parent: &tracing::Span) -> Result<(), Vec<BscDiagnostic>> {
    let _span = tracing::info_span!(
        parent: parent,
        "lsp.typecheck.parse_file",
        module = %ctx.file_args.module_name
    )
    .entered();

    let mut args = ctx.file_args.parser_args.clone();
    let source_arg = args.pop();
    args.push("-bs-read-stdin".into());
    if let Some(arg) = source_arg {
        args.push(arg);
    }

    // Ensure the .ast output directory exists
    if let Some(parent) = ctx
        .file_args
        .build_path_abs
        .join(&ctx.file_args.ast_path)
        .parent()
    {
        let _ = std::fs::create_dir_all(parent);
    }

    let mut child = Command::new(&ctx.file_args.bsc_path)
        .current_dir(&ctx.file_args.build_path_abs)
        .args(&args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| {
            tracing::warn!("parse_file_to_ast: failed to spawn bsc: {e}");
            Vec::new()
        })?;

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(ctx.content.as_bytes());
    }

    let output = child.wait_with_output().map_err(|e| {
        tracing::warn!("parse_file_to_ast: bsc invocation failed: {e}");
        Vec::<BscDiagnostic>::new()
    })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(super::typecheck::parse_and_remap_diagnostics(
            &stderr,
            &ctx.file_args.source_path,
            &ctx.file_args.package_path,
        ));
    }

    Ok(())
}

fn compute_in_batch_deps(
    file_contexts: &[FileContext],
    successfully_parsed: &[usize],
) -> HashMap<usize, Vec<usize>> {
    let mut name_to_idx: HashMap<String, usize> = HashMap::new();
    for &idx in successfully_parsed {
        let module_name = &file_contexts[idx].file_args.module_name;
        name_to_idx.insert(module_name.clone(), idx);
        if let Some(simple) = module_name.split('-').next()
            && simple != module_name
        {
            name_to_idx.insert(simple.to_string(), idx);
        }
    }

    let mut deps_map: HashMap<usize, Vec<usize>> = HashMap::new();
    for &idx in successfully_parsed {
        let ctx = &file_contexts[idx];
        let ast_file_path = ctx.file_args.build_path_abs.join(&ctx.file_args.ast_path);
        let raw_deps = deps::read_raw_deps(&ast_file_path);
        let dep_names: Vec<String> = raw_deps
            .iter()
            .filter_map(|d| d.split('.').next().map(|s| s.to_string()))
            .collect();

        let in_batch_deps: Vec<usize> = dep_names
            .iter()
            .filter_map(|name| name_to_idx.get(name).copied())
            .filter(|&dep_idx| dep_idx != idx)
            .collect();

        deps_map.insert(idx, in_batch_deps);
    }

    deps_map
}

fn typecheck_in_waves(
    file_contexts: &[FileContext],
    successfully_parsed: &[usize],
    in_batch_deps: &HashMap<usize, Vec<usize>>,
) -> Vec<TypecheckResult> {
    let mut results = Vec::new();
    let mut completed: HashSet<usize> = HashSet::new();
    let remaining: HashSet<usize> = successfully_parsed.iter().copied().collect();

    loop {
        let wave: Vec<usize> = remaining
            .difference(&completed)
            .copied()
            .filter(|&idx| {
                in_batch_deps
                    .get(&idx)
                    .map(|deps| deps.iter().all(|d| completed.contains(d)))
                    .unwrap_or(true)
            })
            .collect();

        if wave.is_empty() {
            // Either all done, or circular dependency — typecheck remaining best-effort
            let stuck: Vec<usize> = remaining.difference(&completed).copied().collect();
            if stuck.is_empty() {
                break;
            }
            let fallback_span = tracing::info_span!("lsp.typecheck.wave", file_count = stuck.len());
            let wave_results: Vec<TypecheckResult> = {
                let _entered = fallback_span.enter();
                stuck
                    .par_iter()
                    .map(|&idx| typecheck_from_ast(&file_contexts[idx], &fallback_span))
                    .collect()
            };
            results.extend(wave_results);
            break;
        }

        let wave_span = tracing::info_span!("lsp.typecheck.wave", file_count = wave.len());
        let wave_results: Vec<TypecheckResult> = {
            let _entered = wave_span.enter();
            wave.par_iter()
                .map(|&idx| typecheck_from_ast(&file_contexts[idx], &wave_span))
                .collect()
        };

        for &idx in &wave {
            completed.insert(idx);
        }
        results.extend(wave_results);
    }

    results
}

fn typecheck_from_ast(ctx: &FileContext, parent: &tracing::Span) -> TypecheckResult {
    let _span = tracing::info_span!(
        parent: parent,
        "lsp.typecheck.file",
        module = %ctx.file_args.module_name
    )
    .entered();

    let mut args = vec!["-I".to_string(), "src".to_string()];
    args.extend_from_slice(&ctx.file_args.ast_compiler_args);

    let output = Command::new(&ctx.file_args.bsc_path)
        .current_dir(&ctx.file_args.build_path_abs)
        .args(&args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output();

    let diagnostics = match output {
        Ok(output) => {
            let stderr = String::from_utf8_lossy(&output.stderr);
            super::typecheck::parse_and_remap_diagnostics(
                &stderr,
                &ctx.file_args.source_path,
                &ctx.file_args.package_path,
            )
        }
        Err(e) => {
            tracing::warn!("typecheck_from_ast: bsc invocation failed: {e}");
            Vec::new()
        }
    };

    TypecheckResult {
        uri: ctx.uri.clone(),
        generation: ctx.generation,
        diagnostics,
    }
}
