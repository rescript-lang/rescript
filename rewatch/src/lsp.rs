mod analysis;
mod code_lens;
mod completion;
mod definition;
mod dependency_closure;
mod did_change;
mod did_save;
mod document_symbol;
mod formatting;
mod hover;
mod initial_build;
pub mod initialize;
mod inlay_hint;
mod notifications;
mod references;
mod rename;
mod signature_help;
mod type_definition;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::{Mutex, RwLock};

use crate::build::build_types::BuildCommandState;
use crate::build::diagnostics::{BscDiagnostic, Severity};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

/// Convert a URI to a file path, logging a warning on failure.
fn uri_to_file_path(uri: &Url, context: &str) -> Option<PathBuf> {
    match uri.to_file_path() {
        Ok(p) => Some(p),
        Err(_) => {
            tracing::warn!(uri = %uri, "{context}: could not convert URI to file path");
            None
        }
    }
}

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
    /// Unsaved buffer contents keyed by document URI.
    /// Updated on `didChange`, used by completion to get the latest editor buffer.
    open_buffers: Mutex<HashMap<Url, String>>,
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
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                        include_text: Some(false),
                    })),
                    ..Default::default()
                })),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                references_provider: Some(OneOf::Left(true)),
                // code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                })),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(
                        [".", ">", "@", "~", "\"", "=", "("]
                            .iter()
                            .map(|s| s.to_string())
                            .collect(),
                    ),
                    ..Default::default()
                }),
                document_formatting_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string()]),
                    retrigger_characters: Some(vec!["=".to_string(), ",".to_string()]),
                    ..Default::default()
                }),
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

        self.client
            .log_message(
                MessageType::INFO,
                format!("rescript-lsp initialized with workspace folders: {workspace_folders:?}"),
            )
            .await;

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

        self.publish_diagnostics(&all_diagnostics).await;

        self.client
            .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let file_display = uri_to_file_path(&params.text_document.uri, "didOpen")
            .map(|p| p.display().to_string())
            .unwrap_or_default();
        let _guard = tracing::info_span!(
            "lsp.did_open",
            file = %file_display
        )
        .entered();
        if let Ok(mut buffers) = self.open_buffers.lock() {
            buffers.insert(params.text_document.uri, params.text_document.text);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let file_display = uri_to_file_path(&params.text_document.uri, "didClose")
            .map(|p| p.display().to_string())
            .unwrap_or_default();
        let _guard = tracing::info_span!(
            "lsp.did_close",
            file = %file_display
        )
        .entered();
        if let Ok(mut buffers) = self.open_buffers.lock() {
            buffers.remove(&params.text_document.uri);
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(file_path) = uri_to_file_path(&params.text_document.uri, "didChange") else {
            return;
        };

        let content = match params.content_changes.into_iter().last() {
            Some(change) => change.text,
            None => return,
        };

        // Store the latest buffer content for completion requests.
        if let Ok(mut buffers) = self.open_buffers.lock() {
            buffers.insert(params.text_document.uri.clone(), content.clone());
        }

        let diagnostics = {
            let guard = match self.build_state.lock() {
                Ok(g) => g,
                Err(_) => return,
            };
            let Some(build_state) = guard.as_ref() else {
                tracing::warn!("didChange: no build state available");
                return;
            };
            did_change::run(build_state, &file_path, &content)
        };

        self.publish_diagnostics_for_file(&diagnostics, &params.text_document.uri)
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let Some(file_path) = uri_to_file_path(&params.text_document.uri, "didSave") else {
            return;
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

        self.publish_diagnostics_for_file(&diagnostics, &params.text_document.uri)
            .await;

        self.client
            .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "completion") else {
            return Ok(None);
        };

        Ok(completion::handle(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.text_document_position.position,
        ))
    }

    async fn completion_resolve(&self, item: CompletionItem) -> Result<CompletionItem> {
        Ok(completion::handle_resolve(&self.build_state, item))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "hover") else {
            return Ok(None);
        };

        Ok(hover::handle(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.text_document_position_params.position,
        ))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "signature_help") else {
            return Ok(None);
        };

        Ok(signature_help::handle(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.text_document_position_params.position,
        ))
    }

    async fn goto_type_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "type_definition") else {
            return Ok(None);
        };

        Ok(type_definition::handle(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.text_document_position_params.position,
        ))
    }

    async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "definition") else {
            return Ok(None);
        };

        Ok(definition::handle(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.text_document_position_params.position,
        ))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "references") else {
            return Ok(None);
        };

        Ok(references::handle(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.text_document_position.position,
        ))
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "prepare_rename") else {
            return Ok(None);
        };

        Ok(rename::handle_prepare_rename(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.position,
        ))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "rename") else {
            return Ok(None);
        };

        Ok(rename::handle_rename(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.text_document_position.position,
            &params.new_name,
        ))
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "document_symbol") else {
            return Ok(None);
        };

        Ok(document_symbol::handle(&self.build_state, &file_path))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "code_lens") else {
            return Ok(None);
        };

        Ok(code_lens::handle(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
        ))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "inlay_hint") else {
            return Ok(None);
        };

        Ok(inlay_hint::handle(
            &self.open_buffers,
            &self.build_state,
            &file_path,
            uri,
            params.range,
        ))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "formatting") else {
            return Ok(None);
        };

        let bsc_path = match self.build_state.lock() {
            Ok(guard) => match guard.as_ref() {
                Some(state) => state.build_state.compiler_info.bsc_path.clone(),
                None => {
                    tracing::warn!("formatting: no build state available");
                    return Ok(None);
                }
            },
            Err(_) => return Ok(None),
        };

        Ok(formatting::run(&bsc_path, &self.open_buffers, &file_path, uri))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

impl Backend {
    /// Publish diagnostics from the initial build. No stale diagnostics are
    /// cleared because the editor starts with a clean slate.
    async fn publish_diagnostics(&self, diagnostics: &[BscDiagnostic]) {
        let by_file = Self::group_by_file(diagnostics);

        for (uri, diags) in &by_file {
            self.client
                .publish_diagnostics(uri.clone(), diags.clone(), None)
                .await;
        }

        if let Ok(mut prev) = self.last_diagnostics_files.lock() {
            *prev = by_file.keys().cloned().collect();
        }
    }

    /// Publish diagnostics after a didChange or didSave. Clears stale
    /// diagnostics from files that had errors previously but not anymore,
    /// and always notifies for `origin_uri` even when it compiled cleanly.
    async fn publish_diagnostics_for_file(&self, diagnostics: &[BscDiagnostic], origin_uri: &Url) {
        let by_file = Self::group_by_file(diagnostics);
        let current_files: HashSet<Url> = by_file.keys().cloned().collect();

        // Clear diagnostics for files that no longer have errors
        let stale_uris: Vec<Url> = self
            .last_diagnostics_files
            .lock()
            .ok()
            .map(|prev| prev.difference(&current_files).cloned().collect())
            .unwrap_or_default();

        for uri in stale_uris {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }

        // Publish current diagnostics
        for (uri, diags) in &by_file {
            self.client
                .publish_diagnostics(uri.clone(), diags.clone(), None)
                .await;
        }

        // Always notify for the origin file, even when it compiled cleanly
        if !by_file.contains_key(origin_uri) {
            self.client
                .publish_diagnostics(origin_uri.clone(), Vec::new(), None)
                .await;
        }

        if let Ok(mut prev) = self.last_diagnostics_files.lock() {
            *prev = current_files;
        }
    }

    fn group_by_file(diagnostics: &[BscDiagnostic]) -> HashMap<Url, Vec<Diagnostic>> {
        let mut by_file: HashMap<Url, Vec<Diagnostic>> = HashMap::new();
        for diag in diagnostics {
            let Some(uri) = Url::from_file_path(&diag.file).ok() else {
                tracing::warn!("Could not convert path to URI: {}", diag.file.display());
                continue;
            };
            by_file.entry(uri).or_default().push(to_lsp_diagnostic(diag));
        }
        by_file
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
        open_buffers: Mutex::new(HashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
