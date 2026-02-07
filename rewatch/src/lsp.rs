mod initial_build;
pub mod initialize;
mod notifications;

use std::collections::HashMap;
use std::sync::RwLock;

use crate::build::diagnostics::{BscDiagnostic, Severity};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
    /// Workspace folder paths received during `initialize`.
    /// Stored so that `initialized` can read rescript.json and register scoped file watchers.
    workspace_folders: RwLock<Vec<String>>,
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
                // text_document_sync: Some(TextDocumentSyncCapability::Kind(
                //     TextDocumentSyncKind::FULL,
                // )),
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
                Ok(diagnostics) => {
                    all_diagnostics.extend(diagnostics);
                }
                Err(e) => {
                    tracing::error!("Initial build failed: {e}");
                }
            }
        }

        // Group diagnostics by file. Only files with actual diagnostics get a
        // notification — the editor starts with a clean slate so empty
        // publishDiagnostics are unnecessary for the initial build.
        let mut by_file: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
        for diag in all_diagnostics {
            let path = &diag.file;
            let Some(uri) = Url::from_file_path(path).ok() else {
                tracing::warn!("Could not convert path to URI: {}", path.display());
                continue;
            };
            let lsp_diag = to_lsp_diagnostic(&diag);
            by_file.entry(uri).or_default().push(lsp_diag);
        }
        for (uri, diagnostics) in by_file {
            self.client.publish_diagnostics(uri, diagnostics, None).await;
        }

        self.client
            .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
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
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
