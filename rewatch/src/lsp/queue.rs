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

/// All pending state accumulated between flushes.
struct PendingState {
    /// Files that need typechecking (unsaved buffer content).
    typechecks: HashMap<Url, PendingFileTypecheck>,
    /// Files that need an incremental build (saved to disk).
    compile_files: HashMap<Url, PendingFileBuild>,
    /// File paths whose creation or deletion requires a full project rebuild.
    /// Flush resolves project roots and groups these into per-project sets.
    build_projects: HashSet<PathBuf>,
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
    pub fn new(
        projects: Arc<Mutex<ProjectMap>>,
        client: Client,
        root_span: tracing::Span,
        debounce: Duration,
    ) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        let generations = Arc::new(Mutex::new(HashMap::new()));
        tokio::spawn(
            consumer(rx, projects, Arc::clone(&generations), client, debounce).instrument(root_span),
        );
        Queue { tx, generations }
    }

    /// A buffer was opened in the editor (didOpen).
    pub fn notify_buffer_opened(&self, uri: Url, file_path: PathBuf, content: String) {
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
        let generation = self.next_generation(&uri);
        let _ = self.tx.send(QueueEvent::BufferChanged {
            uri,
            file_path,
            content,
            generation,
        });
    }

    /// A file changed on disk (didSave or didChangeWatchedFiles CHANGED).
    pub fn notify_file_changed_on_disk(&self, uri: Url, file_path: PathBuf) {
        let _ = self.tx.send(QueueEvent::FileChangedOnDisk { uri, file_path });
    }

    /// A file was created on disk (didChangeWatchedFiles CREATED).
    pub fn notify_file_created(&self, file_path: PathBuf) {
        let _ = self.tx.send(QueueEvent::FileCreated { file_path });
    }

    /// A file was deleted from disk (didChangeWatchedFiles DELETED).
    pub fn notify_file_deleted(&self, file_path: PathBuf) {
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
    fn new() -> Self {
        PendingState {
            typechecks: HashMap::new(),
            compile_files: HashMap::new(),
            build_projects: HashSet::new(),
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
            QueueEvent::FileCreated { file_path } | QueueEvent::FileDeleted { file_path } => {
                // A full rebuild covers this file, so drop any per-file work.
                if let Ok(uri) = Url::from_file_path(&file_path) {
                    self.typechecks.remove(&uri);
                    self.compile_files.remove(&uri);
                }
                self.build_projects.insert(file_path);
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
    if state.typechecks.is_empty() && state.compile_files.is_empty() && state.build_projects.is_empty() {
        return;
    }

    // Step 0: Run full builds (file creation / deletion).
    let has_full_builds = !state.build_projects.is_empty();
    if has_full_builds {
        project_build::run(state, projects, client).await;
    }

    let compile_files: HashMap<Url, PendingFileBuild> = std::mem::take(&mut state.compile_files);
    let typechecks: HashMap<Url, PendingFileTypecheck> = std::mem::take(&mut state.typechecks);

    let has_incremental_builds = !compile_files.is_empty();

    // Step 1: Run incremental builds (saved files)
    if has_incremental_builds {
        file_build::run(&compile_files, projects, client).await;
    }

    // Step 2: Run typechecks (unsaved edits)
    if !typechecks.is_empty() {
        file_typecheck::run(typechecks, projects, generations, client).await;
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
        file_typecheck::run(rechecks, projects, generations, client).await;
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

        assert!(state.build_projects.contains(&test_path("New.res")));
    }

    #[test]
    fn file_deleted_adds_to_build_projects() {
        let mut state = PendingState::new();
        state.merge(QueueEvent::FileDeleted {
            file_path: test_path("Old.res"),
        });

        assert!(state.build_projects.contains(&test_path("Old.res")));
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
        assert!(state.build_projects.contains(&test_path("A.res")));
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
        assert!(state.build_projects.contains(&test_path("A.res")));
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

        assert!(state.build_projects.contains(&test_path("New.res")));
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

        assert!(state.build_projects.contains(&test_path("Removed.res")));
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

        assert_eq!(state.build_projects.len(), 3);
        assert!(state.build_projects.contains(&test_path("A.res")));
        assert!(state.build_projects.contains(&test_path("B.res")));
        assert!(state.build_projects.contains(&test_path("C.res")));
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
        assert!(state.build_projects.contains(&test_path("New.res")));
    }
}
