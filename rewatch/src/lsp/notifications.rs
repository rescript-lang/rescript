use serde::{Deserialize, Serialize};
use tower_lsp::lsp_types::notification::Notification;

/// Custom notification sent after a build cycle completes.
/// Clients can wait for this to know when diagnostics are fully published.
pub enum BuildFinished {}

#[derive(Deserialize, Serialize)]
pub struct BuildFinishedParams {}

impl Notification for BuildFinished {
    type Params = BuildFinishedParams;
    const METHOD: &'static str = "rescript/buildFinished";
}
