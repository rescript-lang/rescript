use std::collections::HashMap;
use std::sync::Mutex;
use std::time::Duration;

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Url};

#[derive(PartialEq)]
enum FlushState {
    Idle,
    Pending(u64),
}

struct StoreState {
    diagnostics: HashMap<Url, Vec<Diagnostic>>,
    flush_state: FlushState,
    flush_in_progress: bool,
    next_pending_id: u64,
}

/// Thread-safe store for LSP diagnostics, with flush-aware idle tracking.
///
/// The store accumulates diagnostics as they are published by the LSP queue,
/// and exposes them to an HTTP endpoint. It tracks whether a build/typecheck
/// flush is pending or in-progress so the HTTP handler can block until
/// results are ready.
pub struct DiagnosticStore {
    state: Mutex<StoreState>,
    idle_notify: tokio::sync::Notify,
}

impl DiagnosticStore {
    pub fn new() -> Self {
        DiagnosticStore {
            state: Mutex::new(StoreState {
                diagnostics: HashMap::new(),
                flush_state: FlushState::Idle,
                flush_in_progress: false,
                next_pending_id: 1,
            }),
            idle_notify: tokio::sync::Notify::new(),
        }
    }

    /// Store diagnostics for a URI. Removes the entry if diagnostics are empty.
    /// Duplicates are removed so that multiple build phases producing the same
    /// diagnostic don't cause repeated entries in the HTTP snapshot.
    pub fn publish(&self, uri: Url, mut diagnostics: Vec<Diagnostic>) {
        if let Ok(mut state) = self.state.lock() {
            if diagnostics.is_empty() {
                state.diagnostics.remove(&uri);
            } else {
                diagnostics.dedup();
                state.diagnostics.insert(uri, diagnostics);
            }
        }
    }

    /// Called when an event enters the queue channel (before debounce).
    /// Transitions to `Pending(n)` so the HTTP endpoint knows to wait.
    pub fn mark_pending(&self) {
        if let Ok(mut state) = self.state.lock() {
            let id = state.next_pending_id;
            state.next_pending_id += 1;
            state.flush_state = FlushState::Pending(id);
        }
    }

    /// Called by the consumer right before a flush begins.
    /// Returns the current pending ID so the flush guard can pass it to `end_flush`.
    pub fn start_flush(&self) -> u64 {
        if let Ok(mut state) = self.state.lock() {
            state.flush_in_progress = true;
            match state.flush_state {
                FlushState::Pending(id) => id,
                FlushState::Idle => 0,
            }
        } else {
            0
        }
    }

    /// Called when a flush completes (or panics via `FlushGuard`).
    /// Only transitions to `Idle` if no newer events arrived during the flush.
    pub fn end_flush(&self, flushed_id: u64) {
        if let Ok(mut state) = self.state.lock() {
            state.flush_in_progress = false;
            if let FlushState::Pending(current_id) = state.flush_state
                && current_id <= flushed_id
            {
                state.flush_state = FlushState::Idle;
            }
            if state.flush_state == FlushState::Idle && !state.flush_in_progress {
                self.idle_notify.notify_waiters();
            }
        }
    }

    fn is_idle(&self) -> bool {
        match self.state.lock() {
            Ok(state) => state.flush_state == FlushState::Idle && !state.flush_in_progress,
            Err(_) => true,
        }
    }

    /// Wait until the store is idle (no pending events, no flush in progress),
    /// or until the timeout expires. Returns `true` if idle, `false` if timed out.
    pub async fn wait_for_idle(&self, timeout: Duration) -> bool {
        let deadline = tokio::time::Instant::now() + timeout;
        loop {
            if self.is_idle() {
                return true;
            }
            let remaining = deadline.saturating_duration_since(tokio::time::Instant::now());
            if remaining.is_zero() {
                return false;
            }
            tokio::select! {
                _ = self.idle_notify.notified() => {
                    if self.is_idle() {
                        return true;
                    }
                }
                _ = tokio::time::sleep(remaining) => {
                    return self.is_idle();
                }
            }
        }
    }

    /// Returns a snapshot of current diagnostics keyed by absolute file path.
    /// Files with no diagnostics are omitted.
    pub fn snapshot(&self) -> HashMap<String, Vec<DiagnosticSnapshot>> {
        let state = match self.state.lock() {
            Ok(s) => s,
            Err(_) => return HashMap::new(),
        };
        let mut result = HashMap::new();
        for (uri, diags) in &state.diagnostics {
            if diags.is_empty() {
                continue;
            }
            let path = match uri.to_file_path() {
                Ok(p) => p.to_string_lossy().into_owned(),
                Err(_) => continue,
            };
            let snapshots: Vec<DiagnosticSnapshot> = diags.iter().map(DiagnosticSnapshot::from).collect();
            result.insert(path, snapshots);
        }
        result
    }
}

/// A simplified diagnostic for JSON serialization over HTTP.
#[derive(serde::Serialize)]
pub struct DiagnosticSnapshot {
    pub range: RangeSnapshot,
    pub severity: String,
    pub message: String,
}

#[derive(serde::Serialize)]
pub struct RangeSnapshot {
    pub start: PositionSnapshot,
    pub end: PositionSnapshot,
}

#[derive(serde::Serialize)]
pub struct PositionSnapshot {
    pub line: u32,
    pub character: u32,
}

impl From<&Diagnostic> for DiagnosticSnapshot {
    fn from(diag: &Diagnostic) -> Self {
        let severity = match diag.severity {
            Some(DiagnosticSeverity::ERROR) => "error",
            Some(DiagnosticSeverity::WARNING) => "warning",
            Some(DiagnosticSeverity::INFORMATION) => "information",
            Some(DiagnosticSeverity::HINT) => "hint",
            _ => "unknown",
        };
        DiagnosticSnapshot {
            range: RangeSnapshot {
                start: PositionSnapshot {
                    line: diag.range.start.line,
                    character: diag.range.start.character,
                },
                end: PositionSnapshot {
                    line: diag.range.end.line,
                    character: diag.range.end.character,
                },
            },
            severity: severity.to_string(),
            message: diag.message.clone(),
        }
    }
}

/// Guard that calls `end_flush` on drop, ensuring the store never gets stuck
/// in `flush_in_progress = true` even if the flush panics.
pub struct FlushGuard<'a> {
    store: &'a DiagnosticStore,
    flushed_id: u64,
}

impl<'a> FlushGuard<'a> {
    pub fn new(store: &'a DiagnosticStore) -> Self {
        let flushed_id = store.start_flush();
        FlushGuard { store, flushed_id }
    }
}

impl Drop for FlushGuard<'_> {
    fn drop(&mut self) {
        self.store.end_flush(self.flushed_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::{Position, Range};

    fn test_uri(name: &str) -> Url {
        if cfg!(windows) {
            Url::from_file_path(format!("C:\\tmp\\{name}")).unwrap()
        } else {
            Url::from_file_path(format!("/tmp/{name}")).unwrap()
        }
    }

    fn make_diagnostic(message: &str) -> Diagnostic {
        Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 5,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            message: message.to_string(),
            ..Default::default()
        }
    }

    #[test]
    fn publish_stores_diagnostics() {
        let store = DiagnosticStore::new();
        let uri = test_uri("A.res");
        store.publish(uri.clone(), vec![make_diagnostic("error 1")]);

        let snap = store.snapshot();
        assert_eq!(snap.len(), 1);
    }

    #[test]
    fn publish_empty_removes_entry() {
        let store = DiagnosticStore::new();
        let uri = test_uri("A.res");
        store.publish(uri.clone(), vec![make_diagnostic("error 1")]);
        store.publish(uri, vec![]);

        let snap = store.snapshot();
        assert!(snap.is_empty());
    }

    #[test]
    fn starts_idle() {
        let store = DiagnosticStore::new();
        assert!(store.is_idle());
    }

    #[test]
    fn mark_pending_makes_not_idle() {
        let store = DiagnosticStore::new();
        store.mark_pending();
        assert!(!store.is_idle());
    }

    #[test]
    fn flush_cycle_returns_to_idle() {
        let store = DiagnosticStore::new();
        store.mark_pending();
        let id = store.start_flush();
        assert!(!store.is_idle());
        store.end_flush(id);
        assert!(store.is_idle());
    }

    #[test]
    fn event_during_flush_stays_pending() {
        let store = DiagnosticStore::new();
        store.mark_pending(); // Pending(1)
        let id = store.start_flush(); // captures 1
        store.mark_pending(); // Pending(2) arrives during flush
        store.end_flush(id); // flushed_id=1, current=Pending(2) → stays pending
        assert!(!store.is_idle());
    }

    #[test]
    fn second_flush_after_mid_flush_event_goes_idle() {
        let store = DiagnosticStore::new();
        store.mark_pending(); // Pending(1)
        let id1 = store.start_flush();
        store.mark_pending(); // Pending(2)
        store.end_flush(id1); // stays Pending(2)
        assert!(!store.is_idle());

        let id2 = store.start_flush(); // captures 2
        store.end_flush(id2); // 2 == 2 → Idle
        assert!(store.is_idle());
    }

    #[test]
    fn multiple_events_single_flush() {
        let store = DiagnosticStore::new();
        store.mark_pending(); // Pending(1)
        store.mark_pending(); // Pending(2)
        store.mark_pending(); // Pending(3)
        let id = store.start_flush(); // captures 3
        store.end_flush(id); // 3 == 3 → Idle
        assert!(store.is_idle());
    }

    #[test]
    fn flush_guard_calls_end_flush_on_drop() {
        let store = DiagnosticStore::new();
        store.mark_pending();
        {
            let _guard = FlushGuard::new(&store);
            assert!(!store.is_idle());
        }
        assert!(store.is_idle());
    }

    #[test]
    fn publish_deduplicates() {
        let store = DiagnosticStore::new();
        let uri = test_uri("A.res");
        let d = make_diagnostic("type error");
        store.publish(uri.clone(), vec![d.clone(), d]);

        let snap = store.snapshot();
        let path = if cfg!(windows) {
            "C:\\tmp\\A.res"
        } else {
            "/tmp/A.res"
        };
        assert_eq!(snap.get(path).unwrap().len(), 1);
    }

    #[test]
    fn flush_guard_with_mid_flush_event() {
        let store = DiagnosticStore::new();
        store.mark_pending(); // Pending(1)
        {
            let _guard = FlushGuard::new(&store);
            store.mark_pending(); // Pending(2)
        }
        // Guard dropped with flushed_id=1, but current is Pending(2) → not idle
        assert!(!store.is_idle());
    }
}
