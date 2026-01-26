//! Client management for the daemon.
//!
//! Tracks connected clients (build, watch, clean, debug, format), manages their
//! lifecycle, and provides a single broadcast channel for all daemon events.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

use chrono::Local;
use tokio::sync::{Mutex, broadcast, mpsc};

use super::proto::DaemonEvent;

/// Unique identifier for a connected client.
pub type ClientId = u64;

/// Type of client connection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClientType {
    Build,
    Watch,
    Clean,
    Debug,
    Format,
}

/// Information about a connected client.
#[derive(Debug, Clone)]
pub struct ClientInfo {
    pub client_type: ClientType,
    /// Working directory the client connected from.
    pub working_directory: String,
    /// When the client connected (formatted timestamp).
    pub connected_at: String,
}

/// Represents a change in client count for shutdown monitoring.
#[derive(Debug)]
pub enum ClientCountChange {
    /// A client connected.
    Connected,
    /// All clients disconnected.
    AllDisconnected,
}

/// Manages connected clients and their communication channels.
pub struct ClientManager {
    /// Next client ID to assign.
    next_id: AtomicU64,
    /// Currently connected clients.
    clients: Mutex<HashMap<ClientId, ClientInfo>>,
    /// Single broadcast channel for all daemon events.
    /// All clients receive the same stream; each filters for relevant events.
    pub event_tx: broadcast::Sender<DaemonEvent>,
    /// Channel to signal client count changes for shutdown logic.
    client_count_tx: mpsc::Sender<ClientCountChange>,
}

impl ClientManager {
    pub fn new(client_count_tx: mpsc::Sender<ClientCountChange>) -> Self {
        let (event_tx, _) = broadcast::channel(100);
        Self {
            next_id: AtomicU64::new(1),
            clients: Mutex::new(HashMap::new()),
            event_tx,
            client_count_tx,
        }
    }

    /// Register a new client and return its ID.
    pub async fn register(&self, client_type: ClientType, working_directory: String) -> ClientId {
        let id = self.next_id.fetch_add(1, Ordering::SeqCst);
        let info = ClientInfo {
            client_type,
            working_directory,
            connected_at: Local::now().format("%H:%M:%S").to_string(),
        };
        self.clients.lock().await.insert(id, info);

        // Notify that a client connected (cancels pending shutdown)
        let _ = self.client_count_tx.send(ClientCountChange::Connected).await;

        id
    }

    /// Unregister a client and trigger shutdown check if no clients remain.
    pub async fn unregister(&self, id: ClientId) {
        let is_empty = {
            let mut clients = self.clients.lock().await;
            let removed = clients.remove(&id);
            eprintln!(
                "[daemon] ClientManager::unregister({}): removed={}, remaining={}",
                id,
                removed.is_some(),
                clients.len()
            );
            clients.is_empty()
        };

        if is_empty {
            eprintln!("[daemon] All clients disconnected, sending AllDisconnected signal");
            // Signal that all clients disconnected
            let _ = self
                .client_count_tx
                .send(ClientCountChange::AllDisconnected)
                .await;
        }
    }

    /// Check if a watch client is currently connected.
    pub async fn has_watch_client(&self) -> bool {
        self.clients
            .lock()
            .await
            .values()
            .any(|c| c.client_type == ClientType::Watch)
    }

    /// Get the ID of the connected watch client, if any.
    pub async fn get_watch_client_id(&self) -> Option<ClientId> {
        self.clients
            .lock()
            .await
            .iter()
            .find(|(_, info)| info.client_type == ClientType::Watch)
            .map(|(id, _)| *id)
    }

    /// Get all connected clients.
    pub async fn get_all_clients(&self) -> Vec<(ClientId, ClientInfo)> {
        self.clients
            .lock()
            .await
            .iter()
            .map(|(id, info)| (*id, info.clone()))
            .collect()
    }

    /// Subscribe to daemon events.
    pub fn subscribe(&self) -> broadcast::Receiver<DaemonEvent> {
        self.event_tx.subscribe()
    }

    /// Send an event to all clients.
    pub fn send(&self, event: DaemonEvent) {
        let _ = self.event_tx.send(event);
    }
}
