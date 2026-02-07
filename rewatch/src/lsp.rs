mod dependency_closure;
mod did_save;
mod initial_build;
pub mod initialize;
mod notifications;

use std::collections::{HashMap, HashSet};
use std::sync::{Mutex, RwLock};

use crate::build::build_types::BuildCommandState;
use crate::build::diagnostics::{BscDiagnostic, Severity};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
    /// Workspace folder paths received during `initialize`.
    /// Stored so that `initialized` can read rescript.json and register scoped file watchers.
    workspace_folders: RwLock<Vec<String>>,
    /// Build state persisted across LSP requests for incremental builds.
    build_state: Mutex<Option<BuildCommandState>>,
    /// Files that had diagnostics published in the last build cycle.
    /// Used to clear stale diagnostics when errors are fixed.
    last_diagnostics_files: Mutex<HashSet<Url>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let folders: Vec<String> = params
            .workspace_folders
            .as_deref()
            .unwrap_or_default()
            .iter()
            .filter_map(|f| f.uri.to_file_path().ok())
            .map(|p| p.to_string_lossy().into_owned())
            .collect();

        tracing::info!(workspace_folders = ?folders, "LSP initialized with workspace folders");

        if let Ok(mut wf) = self.workspace_folders.write() {
            *wf = folders;
        }

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "rescript-lsp".to_string(),
                version: None,
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                    save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                        include_text: Some(false),
                    })),
                    ..Default::default()
                })),
                // hover_provider: Some(HoverProviderCapability::Simple(true)),
                // definition_provider: Some(OneOf::Left(true)),
                // type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                // references_provider: Some(OneOf::Left(true)),
                // code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                // rename_provider: Some(OneOf::Right(RenameOptions {
                //     prepare_provider: Some(true),
                //     work_done_progress_options: WorkDoneProgressOptions::default(),
                // })),
                // document_symbol_provider: Some(OneOf::Left(true)),
                // completion_provider: Some(CompletionOptions {
                //     trigger_characters: Some(
                //         [".", ">", "@", "~", "\"", "=", "("]
                //             .iter()
                //             .map(|s| s.to_string())
                //             .collect(),
                //     ),
                //     resolve_provider: Some(true),
                //     ..Default::default()
                // }),
                // document_formatting_provider: Some(OneOf::Left(true)),
                // inlay_hint_provider: Some(OneOf::Left(true)),
                // signature_help_provider: Some(SignatureHelpOptions {
                //     trigger_characters: Some(vec!["(".to_string()]),
                //     retrigger_characters: Some(vec!["=".to_string(), ",".to_string()]),
                //     ..Default::default()
                // }),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let workspace_folders = self
            .workspace_folders
            .read()
            .map(|wf| wf.clone())
            .unwrap_or_default();

        let workspaces = initialize::register_file_watchers(&self.client, &workspace_folders).await;

        let mut all_diagnostics: Vec<BscDiagnostic> = Vec::new();
        for workspace in workspaces {
            match initial_build::run(workspace) {
                Ok((state, diagnostics)) => {
                    all_diagnostics.extend(diagnostics);
                    if let Ok(mut bs) = self.build_state.lock() {
                        *bs = Some(state);
                    }
                }
                Err(e) => {
                    tracing::error!("Initial build failed: {e}");
                }
            }
        }

        self.publish_diagnostics(&all_diagnostics, true).await;

        self.client
            .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let file_path = match params.text_document.uri.to_file_path() {
            Ok(p) => p,
            Err(_) => {
                tracing::warn!(
                    uri = %params.text_document.uri,
                    "didSave: could not convert URI to file path"
                );
                return;
            }
        };

        let diagnostics = {
            let mut guard = match self.build_state.lock() {
                Ok(g) => g,
                Err(_) => return,
            };
            let Some(build_state) = guard.as_mut() else {
                tracing::warn!("didSave: no build state available");
                return;
            };
            did_save::run(build_state, &file_path)
        };

        self.publish_diagnostics(&diagnostics, false).await;

        self.client
            .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    /// Publish diagnostics grouped by file. Clears stale diagnostics from
    /// files that had errors in the previous cycle but not in the current one.
    ///
    /// When `is_initial` is true, no stale diagnostics are cleared (the editor
    /// starts with a clean slate).
    async fn publish_diagnostics(&self, diagnostics: &[BscDiagnostic], is_initial: bool) {
        let mut by_file: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
        for diag in diagnostics {
            let Some(uri) = Url::from_file_path(&diag.file).ok() else {
                tracing::warn!("Could not convert path to URI: {}", diag.file.display());
                continue;
            };
            by_file.entry(uri).or_default().push(to_lsp_diagnostic(diag));
        }

        let current_files: HashSet<Url> = by_file.keys().cloned().collect();

        // Collect stale URIs that need clearing (must drop lock before awaiting)
        let stale_uris: Vec<Url> = if !is_initial {
            self.last_diagnostics_files
                .lock()
                .ok()
                .map(|prev| prev.difference(&current_files).cloned().collect())
                .unwrap_or_default()
        } else {
            Vec::new()
        };

        // Clear diagnostics for files that no longer have errors
        for uri in stale_uris {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }

        // Publish current diagnostics
        for (uri, diags) in &by_file {
            self.client
                .publish_diagnostics(uri.clone(), diags.clone(), None)
                .await;
        }

        // Update tracking set
        if let Ok(mut prev) = self.last_diagnostics_files.lock() {
            *prev = current_files;
        }
    }
}

/// Convert a build-layer `BscDiagnostic` (1-based positions) to an LSP `Diagnostic` (0-based).
fn to_lsp_diagnostic(diag: &BscDiagnostic) -> Diagnostic {
    let severity = match diag.severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
    };
    Diagnostic {
        range: Range {
            start: Position {
                line: diag.range.start.line.saturating_sub(1),
                character: diag.range.start.character.saturating_sub(1),
            },
            end: Position {
                line: diag.range.end.line.saturating_sub(1),
                character: diag.range.end.character.saturating_sub(1),
            },
        },
        severity: Some(severity),
        source: Some("rescript".to_string()),
        message: diag.message.clone(),
        ..Default::default()
    }
}

pub async fn run_stdio() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        workspace_folders: RwLock::new(Vec::new()),
        build_state: Mutex::new(None),
        last_diagnostics_files: Mutex::new(HashSet::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
