//! Unified debounced queue for all LSP file events.
//!
//! All file events (didChange, didOpen, didSave, didChangeWatchedFiles) go
//! into a single unbounded channel. A background consumer task collects
//! events into a `HashMap<Url, PendingFile>`, applying promotion rules to
//! consolidate per-file state. After a configurable debounce period
//! (default 100ms, set via `initializationOptions.queue_debounce_ms`) the batch
//! is flushed:
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

mod file_build;
mod file_typecheck;
mod project_build;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use tokio::sync::mpsc;
use tokio::time::Instant;
use tower_lsp::Client;
use tower_lsp::lsp_types::Url;
use tracing::Instrument;

use super::ProjectMap;
use super::diagnostic_store::{DiagnosticStore, FlushGuard};
use super::file_args::{is_rescript_config, is_rescript_source};
use super::notifications;

/// Monotonically increasing generation counter for staleness detection.
static GENERATION: AtomicU64 = AtomicU64::new(0);

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// A fact about what happened in the editor / filesystem.
/// The merge logic derives the correct build actions from these facts.
enum QueueEvent {
    /// A buffer was opened in the editor (didOpen).
    BufferOpened {
        uri: Url,
        file_path: PathBuf,
        content: String,
        generation: u64,
    },
    /// A buffer's content changed in the editor (didChange).
    BufferChanged {
        uri: Url,
        file_path: PathBuf,
        content: String,
        generation: u64,
    },
    /// A file changed on disk (didSave or didChangeWatchedFiles CHANGED).
    FileChangedOnDisk { uri: Url, file_path: PathBuf },
    /// A file was created on disk (didChangeWatchedFiles CREATED).
    FileCreated { file_path: PathBuf },
    /// A file was deleted from disk (didChangeWatchedFiles DELETED).
    FileDeleted { file_path: PathBuf },
    /// A `rescript.json` config file was saved — triggers a full project rebuild.
    ConfigChanged { file_path: PathBuf },
}

/// A file that needs typechecking (unsaved buffer content).
struct PendingFileTypecheck {
    file_path: PathBuf,
    content: String,
    generation: u64,
}

/// A file that needs an incremental build (saved to disk).
/// `buffer_content` holds the latest unsaved content if a `didChange`
/// arrived for this file in the same batch — used for post-build recheck.
struct PendingFileBuild {
    file_path: PathBuf,
    buffer_content: Option<String>,
    generation: u64,
}

/// Tracks file changes that require full project rebuilds.
/// Created and deleted files are tracked separately so that recreated files
/// (deleted then created in the same batch, e.g. git checkout) can be
/// identified and promoted to incremental builds for JS emission.
struct PendingProjectBuilds {
    created_files: HashSet<PathBuf>,
    deleted_files: HashSet<PathBuf>,
    /// `rescript.json` files that were saved, triggering a full rebuild.
    config_changed: HashSet<PathBuf>,
}

impl Default for PendingProjectBuilds {
    fn default() -> Self {
        Self::new()
    }
}

impl PendingProjectBuilds {
    fn new() -> Self {
        PendingProjectBuilds {
            created_files: HashSet::new(),
            deleted_files: HashSet::new(),
            config_changed: HashSet::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.created_files.is_empty() && self.deleted_files.is_empty() && self.config_changed.is_empty()
    }
}

/// All pending state accumulated between flushes.
struct PendingState {
    /// Files that need typechecking (unsaved buffer content).
    typechecks: HashMap<Url, PendingFileTypecheck>,
    /// Files that need an incremental build (saved to disk).
    compile_files: HashMap<Url, PendingFileBuild>,
    /// File changes that require full project rebuilds.
    /// Flush resolves project roots and groups these into per-project sets.
    build_projects: PendingProjectBuilds,
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
    /// Optional diagnostic store for the HTTP diagnostics endpoint.
    diagnostic_store: Option<Arc<DiagnosticStore>>,
}

impl Queue {
    /// Create the queue and spawn the single background consumer task.
    pub fn new(
        projects: Arc<Mutex<ProjectMap>>,
        client: Client,
        root_span: tracing::Span,
        debounce: Duration,
        diagnostic_store: Option<Arc<DiagnosticStore>>,
    ) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        let generations = Arc::new(Mutex::new(HashMap::new()));
        tokio::spawn(
            consumer(
                rx,
                projects,
                Arc::clone(&generations),
                client,
                debounce,
                diagnostic_store.clone(),
            )
            .instrument(root_span),
        );
        Queue {
            tx,
            generations,
            diagnostic_store,
        }
    }

    /// A buffer was opened in the editor (didOpen).
    pub fn notify_buffer_opened(&self, uri: Url, file_path: PathBuf, content: String) {
        if let Some(ref store) = self.diagnostic_store {
            store.mark_pending();
        }
        let generation = self.next_generation(&uri);
        let _ = self.tx.send(QueueEvent::BufferOpened {
            uri,
            file_path,
            content,
            generation,
        });
    }

    /// A buffer's content changed in the editor (didChange).
    pub fn notify_buffer_changed(&self, uri: Url, file_path: PathBuf, content: String) {
        if let Some(ref store) = self.diagnostic_store {
            store.mark_pending();
        }
        let generation = self.next_generation(&uri);
        let _ = self.tx.send(QueueEvent::BufferChanged {
            uri,
            file_path,
            content,
            generation,
        });
    }

    /// A file changed on disk (didSave or didChangeWatchedFiles CHANGED).
    /// Config files (`rescript.json`) are routed to a full project rebuild.
    pub fn notify_file_changed_on_disk(&self, uri: Url, file_path: PathBuf) {
        if let Some(ref store) = self.diagnostic_store {
            store.mark_pending();
        }
        if is_rescript_config(&file_path) {
            let _ = self.tx.send(QueueEvent::ConfigChanged { file_path });
        } else {
            let _ = self.tx.send(QueueEvent::FileChangedOnDisk { uri, file_path });
        }
    }

    /// A file was created on disk (didChangeWatchedFiles CREATED).
    pub fn notify_file_created(&self, file_path: PathBuf) {
        if !is_rescript_source(&file_path) {
            return;
        }
        if let Some(ref store) = self.diagnostic_store {
            store.mark_pending();
        }
        let _ = self.tx.send(QueueEvent::FileCreated { file_path });
    }

    /// A file was deleted from disk (didChangeWatchedFiles DELETED).
    pub fn notify_file_deleted(&self, file_path: PathBuf) {
        if !is_rescript_source(&file_path) {
            return;
        }
        if let Some(ref store) = self.diagnostic_store {
            store.mark_pending();
        }
        let _ = self.tx.send(QueueEvent::FileDeleted { file_path });
    }

    fn next_generation(&self, uri: &Url) -> u64 {
        let generation = GENERATION.fetch_add(1, Ordering::Relaxed) + 1;
        match self.generations.lock() {
            Ok(mut gens) => {
                gens.insert(uri.clone(), generation);
            }
            Err(e) => tracing::error!("generations mutex poisoned: {e}"),
        }
        generation
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
    debounce: Duration,
    diagnostic_store: Option<Arc<DiagnosticStore>>,
) {
    let mut state = PendingState::new();

    loop {
        // Phase 1: wait for at least one event
        match rx.recv().await {
            Some(event) => state.merge(event),
            None => break,
        }

        // Phase 2: debounce — collect more events until silence
        let deadline = tokio::time::sleep(debounce);
        tokio::pin!(deadline);
        loop {
            tokio::select! {
                event = rx.recv() => {
                    match event {
                        Some(event) => {
                            state.merge(event);
                            deadline.as_mut().reset(Instant::now() + debounce);
                        }
                        None => {
                            flush(&mut state, &projects, &generations, &client, &diagnostic_store).await;
                            return;
                        }
                    }
                }
                _ = &mut deadline => break,
            }
        }

        // Phase 3: flush
        flush(&mut state, &projects, &generations, &client, &diagnostic_store).await;
    }
}

impl PendingState {
    fn new() -> Self {
        PendingState {
            typechecks: HashMap::new(),
            compile_files: HashMap::new(),
            build_projects: PendingProjectBuilds::new(),
        }
    }

    /// Merge a new event into the pending state, applying promotion rules.
    fn merge(&mut self, event: QueueEvent) {
        match event {
            // Buffer events: unsaved content to typecheck.
            QueueEvent::BufferOpened {
                uri,
                file_path,
                content,
                generation,
            }
            | QueueEvent::BufferChanged {
                uri,
                file_path,
                content,
                generation,
            } => {
                if let Some(build) = self.compile_files.get_mut(&uri) {
                    // Already promoted to Build → stash content for post-build recheck
                    build.buffer_content = Some(content);
                    build.generation = generation;
                } else {
                    // Insert or update typecheck
                    match self.typechecks.get_mut(&uri) {
                        Some(existing) => {
                            existing.content = content;
                            existing.generation = generation;
                        }
                        None => {
                            self.typechecks.insert(
                                uri,
                                PendingFileTypecheck {
                                    file_path,
                                    content,
                                    generation,
                                },
                            );
                        }
                    }
                }
            }
            // Disk-save events: file on disk changed, do incremental build.
            QueueEvent::FileChangedOnDisk { uri, file_path } => {
                if self.compile_files.contains_key(&uri) {
                    // Already a Build → nothing to do
                    return;
                }
                // Promote from typecheck if present, stashing content
                let (buffer_content, generation) = match self.typechecks.remove(&uri) {
                    Some(tc) => (Some(tc.content), tc.generation),
                    None => (None, 0),
                };
                self.compile_files.insert(
                    uri,
                    PendingFileBuild {
                        file_path,
                        buffer_content,
                        generation,
                    },
                );
            }
            QueueEvent::FileCreated { file_path } => {
                if let Ok(uri) = Url::from_file_path(&file_path) {
                    self.typechecks.remove(&uri);
                    self.compile_files.remove(&uri);
                    // If this file was previously deleted in the same batch,
                    // it was recreated (e.g. git checkout). Schedule an
                    // incremental build to emit JS after the full rebuild.
                    if self.build_projects.deleted_files.contains(&file_path) {
                        self.compile_files.insert(
                            uri,
                            PendingFileBuild {
                                file_path: file_path.clone(),
                                buffer_content: None,
                                generation: 0,
                            },
                        );
                    }
                }
                self.build_projects.created_files.insert(file_path);
            }
            QueueEvent::FileDeleted { file_path } => {
                if let Ok(uri) = Url::from_file_path(&file_path) {
                    self.typechecks.remove(&uri);
                    self.compile_files.remove(&uri);
                }
                self.build_projects.deleted_files.insert(file_path);
            }
            QueueEvent::ConfigChanged { file_path } => {
                // The full project rebuild does a TypecheckOnly build of all
                // modules, so pending per-file typechecks are redundant.
                // Keep compile_files — the full build doesn't emit JS.
                // Only clear typechecks for files under this project root
                // to avoid discarding work for unrelated projects in a monorepo.
                if let Some(project_root) = file_path.parent() {
                    self.typechecks
                        .retain(|_, tc| !tc.file_path.starts_with(project_root));
                }
                self.build_projects.config_changed.insert(file_path);
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
    diagnostic_store: &Option<Arc<DiagnosticStore>>,
) {
    if state.typechecks.is_empty() && state.compile_files.is_empty() && state.build_projects.is_empty() {
        return;
    }

    client
        .log_message(
            tower_lsp::lsp_types::MessageType::INFO,
            format!(
                "flush: {} created + {} deleted file(s) (full rebuild), {} incremental build(s), {} typecheck(s)",
                state.build_projects.created_files.len(),
                state.build_projects.deleted_files.len(),
                state.compile_files.len(),
                state.typechecks.len(),
            ),
        )
        .await;

    // Create a FlushGuard that calls end_flush on drop (even on panic).
    let _flush_guard = diagnostic_store.as_ref().map(|s| FlushGuard::new(s));
    let store_ref = diagnostic_store.as_deref();

    // Step 0: Run full builds (file creation / deletion).
    let has_full_builds = !state.build_projects.is_empty();
    if has_full_builds {
        project_build::run(state, projects, client, store_ref).await;
    }

    let compile_files: HashMap<Url, PendingFileBuild> = std::mem::take(&mut state.compile_files);
    let typechecks: HashMap<Url, PendingFileTypecheck> = std::mem::take(&mut state.typechecks);

    let has_incremental_builds = !compile_files.is_empty();

    // Step 1: Run incremental builds (saved files)
    if has_incremental_builds {
        file_build::run(&compile_files, projects, client, store_ref).await;
    }

    // Step 2: Run typechecks (unsaved edits)
    if !typechecks.is_empty() {
        file_typecheck::run(typechecks, projects, generations, client, store_ref).await;
    }

    // Step 3: Post-build recheck for files with buffer_content
    let rechecks: HashMap<Url, PendingFileTypecheck> = compile_files
        .into_iter()
        .filter_map(|(uri, build)| {
            build.buffer_content.map(|content| {
                (
                    uri,
                    PendingFileTypecheck {
                        file_path: build.file_path,
                        content,
                        generation: build.generation,
                    },
                )
            })
        })
        .collect();
    if !rechecks.is_empty() {
        file_typecheck::run(rechecks, projects, generations, client, store_ref).await;
    }

    // Step 4: Notify buildFinished (only if any builds ran)
    if has_full_builds || has_incremental_builds {
        client
            .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
            .await;
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn test_path(name: &str) -> PathBuf {
        if cfg!(windows) {
            PathBuf::from(format!("C:\\tmp\\{name}"))
        } else {
            PathBuf::from(format!("/tmp/{name}"))
        }
    }

    fn test_uri(name: &str) -> Url {
        Url::from_file_path(test_path(name)).unwrap()
    }

    // -- Buffer events on empty state --

    #[test]
    fn buffer_changed_on_empty_creates_typecheck() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "let x = 1".into(),
            generation: 1,
        });

        assert_eq!(state.typechecks.len(), 1);
        let tc = state.typechecks.get(&test_uri("A.res")).unwrap();
        assert_eq!(tc.content, "let x = 1");
        assert_eq!(tc.generation, 1);
    }

    #[test]
    fn buffer_opened_on_empty_creates_typecheck() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::BufferOpened {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "let x = 1".into(),
            generation: 1,
        });

        assert_eq!(state.typechecks.len(), 1);
        let tc = state.typechecks.get(&test_uri("A.res")).unwrap();
        assert_eq!(tc.content, "let x = 1");
    }

    // -- Buffer + Buffer → latest content wins --

    #[test]
    fn buffer_then_buffer_keeps_latest_content() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "v1".into(),
            generation: 1,
        });
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "v2".into(),
            generation: 2,
        });

        assert_eq!(state.typechecks.len(), 1);
        let tc = state.typechecks.get(&test_uri("A.res")).unwrap();
        assert_eq!(tc.content, "v2");
        assert_eq!(tc.generation, 2);
    }

    // -- Buffer + Save → promote to Build with buffer_content --

    #[test]
    fn buffer_then_save_promotes_to_build_with_content() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "unsaved".into(),
            generation: 1,
        });
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
        });

        // Typecheck was removed, promoted to build
        assert!(state.typechecks.is_empty());
        let build = state.compile_files.get(&test_uri("A.res")).unwrap();
        assert_eq!(build.buffer_content.as_deref(), Some("unsaved"));
    }

    // -- Save on empty state → Build with no buffer_content --

    #[test]
    fn save_on_empty_creates_build() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
        });

        let build = state.compile_files.get(&test_uri("A.res")).unwrap();
        assert!(build.buffer_content.is_none());
    }

    // -- Save + Buffer → stay Build, update buffer_content --

    #[test]
    fn save_then_buffer_updates_buffer_content() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
        });
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "edited".into(),
            generation: 1,
        });

        let build = state.compile_files.get(&test_uri("A.res")).unwrap();
        assert_eq!(build.buffer_content.as_deref(), Some("edited"));
        // No typecheck entry — it went straight into the existing build
        assert!(state.typechecks.is_empty());
    }

    // -- Save + Save → stays Build --

    #[test]
    fn save_then_save_stays_build() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
        });
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
        });

        assert_eq!(state.compile_files.len(), 1);
        let build = state.compile_files.get(&test_uri("A.res")).unwrap();
        assert!(build.buffer_content.is_none());
    }

    // -- FileCreated / FileDeleted → build_projects --

    #[test]
    fn file_created_adds_to_build_projects() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileCreated {
            file_path: test_path("New.res"),
        });

        assert!(state.build_projects.created_files.contains(&test_path("New.res")));
    }

    #[test]
    fn file_deleted_adds_to_build_projects() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileDeleted {
            file_path: test_path("Old.res"),
        });

        assert!(state.build_projects.deleted_files.contains(&test_path("Old.res")));
    }

    #[test]
    fn file_deleted_removes_pending_typecheck() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "let x = 1".into(),
            generation: 1,
        });
        state.merge(QueueEvent::FileDeleted {
            file_path: test_path("A.res"),
        });

        assert!(state.typechecks.is_empty());
        assert!(state.build_projects.deleted_files.contains(&test_path("A.res")));
    }

    #[test]
    fn file_created_removes_pending_build() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
        });
        state.merge(QueueEvent::FileCreated {
            file_path: test_path("A.res"),
        });

        assert!(state.compile_files.is_empty());
        assert!(state.build_projects.created_files.contains(&test_path("A.res")));
    }

    // -- Multiple files stay independent --

    #[test]
    fn different_files_stay_independent() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "a".into(),
            generation: 1,
        });
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("B.res"),
            file_path: test_path("B.res"),
        });

        assert_eq!(state.typechecks.len(), 1);
        assert!(state.typechecks.contains_key(&test_uri("A.res")));
        assert_eq!(state.compile_files.len(), 1);
        assert!(state.compile_files.contains_key(&test_uri("B.res")));
    }

    // -- Cross-cutting: full build + per-file events --

    #[test]
    fn file_created_and_buffer_changed_are_independent() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileCreated {
            file_path: test_path("New.res"),
        });
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("Existing.res"),
            file_path: test_path("Existing.res"),
            content: "let x = 1".into(),
            generation: 1,
        });

        assert!(state.build_projects.created_files.contains(&test_path("New.res")));
        assert_eq!(state.typechecks.len(), 1);
        assert!(state.typechecks.contains_key(&test_uri("Existing.res")));
    }

    #[test]
    fn file_deleted_and_save_are_independent() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileDeleted {
            file_path: test_path("Removed.res"),
        });
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("Other.res"),
            file_path: test_path("Other.res"),
        });

        assert!(
            state
                .build_projects
                .deleted_files
                .contains(&test_path("Removed.res"))
        );
        assert_eq!(state.compile_files.len(), 1);
        let build = state.compile_files.get(&test_uri("Other.res")).unwrap();
        assert!(build.buffer_content.is_none());
    }

    #[test]
    fn multiple_structural_changes_accumulate() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileCreated {
            file_path: test_path("A.res"),
        });
        state.merge(QueueEvent::FileDeleted {
            file_path: test_path("B.res"),
        });
        state.merge(QueueEvent::FileCreated {
            file_path: test_path("C.res"),
        });

        assert_eq!(state.build_projects.created_files.len(), 2);
        assert!(state.build_projects.created_files.contains(&test_path("A.res")));
        assert!(state.build_projects.created_files.contains(&test_path("C.res")));
        assert_eq!(state.build_projects.deleted_files.len(), 1);
        assert!(state.build_projects.deleted_files.contains(&test_path("B.res")));
    }

    #[test]
    fn full_sequence_buffer_save_then_create() {
        let mut state = PendingState::new();

        // User edits a file
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("App.res"),
            file_path: test_path("App.res"),
            content: "let x = 1".into(),
            generation: 1,
        });

        // User saves it
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("App.res"),
            file_path: test_path("App.res"),
        });

        // Meanwhile a new file appears on disk
        state.merge(QueueEvent::FileCreated {
            file_path: test_path("New.res"),
        });

        // App.res should be promoted to Build with buffer_content
        let build = state.compile_files.get(&test_uri("App.res")).unwrap();
        assert_eq!(build.buffer_content.as_deref(), Some("let x = 1"));
        assert!(state.typechecks.is_empty());

        // New.res triggers a full build
        assert!(state.build_projects.created_files.contains(&test_path("New.res")));
    }

    // -- Recreated files (delete + create) → promoted to compile_files --

    #[test]
    fn delete_then_create_promotes_to_compile_files() {
        let mut state = PendingState::new();

        // File deleted (e.g. git checkout starts)
        state.merge(QueueEvent::FileDeleted {
            file_path: test_path("Pkmn.res"),
        });

        // Same file recreated (git checkout completes)
        state.merge(QueueEvent::FileCreated {
            file_path: test_path("Pkmn.res"),
        });

        // Should be in both build_projects sets
        assert!(
            state
                .build_projects
                .deleted_files
                .contains(&test_path("Pkmn.res"))
        );
        assert!(
            state
                .build_projects
                .created_files
                .contains(&test_path("Pkmn.res"))
        );
        // AND promoted to compile_files for JS emission
        assert!(state.compile_files.contains_key(&test_uri("Pkmn.res")));
        let build = state.compile_files.get(&test_uri("Pkmn.res")).unwrap();
        assert!(build.buffer_content.is_none());
    }

    #[test]
    fn create_without_prior_delete_not_in_compile_files() {
        let mut state = PendingState::new();

        // Brand new file, no prior delete
        state.merge(QueueEvent::FileCreated {
            file_path: test_path("New.res"),
        });

        assert!(state.build_projects.created_files.contains(&test_path("New.res")));
        // Should NOT be in compile_files — it's genuinely new
        assert!(state.compile_files.is_empty());
    }

    #[test]
    fn delete_without_create_not_in_compile_files() {
        let mut state = PendingState::new();

        state.merge(QueueEvent::FileDeleted {
            file_path: test_path("Old.res"),
        });

        assert!(state.build_projects.deleted_files.contains(&test_path("Old.res")));
        assert!(state.compile_files.is_empty());
    }

    // -- ConfigChanged clears typechecks but keeps compile_files --

    #[test]
    fn config_changed_clears_pending_typechecks() {
        let mut state = PendingState::new();

        // Unsaved edits in two files
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
            content: "let a = 1".into(),
            generation: 1,
        });
        state.merge(QueueEvent::BufferChanged {
            uri: test_uri("B.res"),
            file_path: test_path("B.res"),
            content: "let b = 2".into(),
            generation: 2,
        });
        assert_eq!(state.typechecks.len(), 2);

        // Config change — full rebuild will typecheck everything
        state.merge(QueueEvent::ConfigChanged {
            file_path: test_path("rescript.json"),
        });

        // Typechecks cleared — full build covers them
        assert!(state.typechecks.is_empty());
        assert!(
            state
                .build_projects
                .config_changed
                .contains(&test_path("rescript.json"))
        );
    }

    #[test]
    fn config_changed_keeps_pending_compile_files() {
        let mut state = PendingState::new();

        // A file was saved
        state.merge(QueueEvent::FileChangedOnDisk {
            uri: test_uri("A.res"),
            file_path: test_path("A.res"),
        });
        assert_eq!(state.compile_files.len(), 1);

        // Config change — full rebuild is TypecheckOnly, doesn't emit JS
        state.merge(QueueEvent::ConfigChanged {
            file_path: test_path("rescript.json"),
        });

        // compile_files kept — still need incremental build for JS output
        assert_eq!(state.compile_files.len(), 1);
        assert!(state.compile_files.contains_key(&test_uri("A.res")));
        assert!(
            state
                .build_projects
                .config_changed
                .contains(&test_path("rescript.json"))
        );
    }

    #[test]
    fn config_changed_only_clears_typechecks_for_same_project() {
        let mut state = PendingState::new();

        let project_a = if cfg!(windows) {
            PathBuf::from("C:\\repo\\packages\\a")
        } else {
            PathBuf::from("/repo/packages/a")
        };
        let project_b = if cfg!(windows) {
            PathBuf::from("C:\\repo\\packages\\b")
        } else {
            PathBuf::from("/repo/packages/b")
        };

        let file_a = project_a.join("src").join("A.res");
        let file_b = project_b.join("src").join("B.res");
        let uri_a = Url::from_file_path(&file_a).unwrap();
        let uri_b = Url::from_file_path(&file_b).unwrap();

        // Unsaved edits in both projects
        state.merge(QueueEvent::BufferChanged {
            uri: uri_a.clone(),
            file_path: file_a,
            content: "let a = 1".into(),
            generation: 1,
        });
        state.merge(QueueEvent::BufferChanged {
            uri: uri_b.clone(),
            file_path: file_b,
            content: "let b = 2".into(),
            generation: 2,
        });
        assert_eq!(state.typechecks.len(), 2);

        // Config change in project A only
        state.merge(QueueEvent::ConfigChanged {
            file_path: project_a.join("rescript.json"),
        });

        // Only project A's typecheck is cleared
        assert_eq!(state.typechecks.len(), 1);
        assert!(state.typechecks.contains_key(&uri_b));
        assert!(!state.typechecks.contains_key(&uri_a));
    }
}
