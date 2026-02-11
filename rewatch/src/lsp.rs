mod analysis;
mod code_action;
mod code_lens;
mod completion;
mod definition;
mod dependency_closure;
mod document_symbol;
mod file_args;
mod formatting;
mod hover;
mod initial_build;
pub mod initialize;
mod inlay_hint;
mod notifications;
mod queue;
mod references;
mod rename;
mod semantic_tokens;
mod signature_help;
mod type_definition;
mod typecheck;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock};

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

/// Maps project roots to their build states, with a cached URI-to-root lookup.
pub(crate) struct ProjectMap {
    /// One `BuildCommandState` per project root (directory containing `rescript.json`).
    states: HashMap<PathBuf, BuildCommandState>,
    /// Cached mapping from file URI to its project root. Populated lazily.
    uri_cache: HashMap<Url, PathBuf>,
    /// Cached runtime module data from `lib/ocaml/`. Populated lazily on first
    /// analysis request, never invalidated (runtime doesn't change during a session).
    runtime_module_data: Option<analysis::RuntimeModuleData>,
}

impl ProjectMap {
    fn new() -> Self {
        ProjectMap {
            states: HashMap::new(),
            uri_cache: HashMap::new(),
            runtime_module_data: None,
        }
    }

    /// Find the project root for a file URI. Checks the cache first, then
    /// walks ancestor directories looking for a `rescript.json` whose
    /// directory is a known project root in `states`.
    fn project_root_for(&mut self, uri: &Url) -> Option<PathBuf> {
        if let Some(root) = self.uri_cache.get(uri) {
            return Some(root.clone());
        }
        let file_path = uri.to_file_path().ok()?;
        let root = file_path
            .ancestors()
            .find(|dir| self.states.contains_key(*dir))?
            .to_path_buf();
        self.uri_cache.insert(uri.clone(), root.clone());
        Some(root)
    }

    /// Find the project root for a file path (no URI conversion needed).
    /// Used by the build queue to group files by project.
    fn project_root_for_path(&self, path: &Path) -> Option<PathBuf> {
        path.ancestors()
            .find(|dir| self.states.contains_key(*dir))
            .map(|dir| dir.to_path_buf())
    }

    /// Get the build state for a file URI (immutable).
    pub(crate) fn get_for_uri(&mut self, uri: &Url) -> Option<&BuildCommandState> {
        let root = self.project_root_for(uri)?;
        self.states.get(&root)
    }

    /// Build an `AnalysisContext` for a file URI, using cached runtime module data.
    ///
    /// This is the single entry point for all analysis handlers. It resolves the
    /// project, ensures the runtime module cache is populated, and delegates to
    /// `AnalysisContext::new()`.
    pub(crate) fn build_analysis_context(
        &mut self,
        uri: &Url,
        file_path: &std::path::Path,
        source: &str,
        position: Position,
        do_ensure_cmt: bool,
        extra_fields: Option<analysis::ExtraFieldsFn<'_>>,
    ) -> Option<analysis::AnalysisContext> {
        let root = self.project_root_for(uri)?;
        let build_state = self.states.get(&root)?;
        let runtime_path = build_state.build_state.compiler_info.runtime_path.clone();
        let runtime = self
            .runtime_module_data
            .get_or_insert_with(|| analysis::scan_runtime_modules(&runtime_path));
        analysis::AnalysisContext::new(
            build_state,
            runtime,
            file_path,
            source,
            position,
            do_ensure_cmt,
            extra_fields,
        )
    }
}

struct Backend {
    client: Client,
    /// Workspace folder paths received during `initialize`.
    /// Stored so that `initialized` can read rescript.json and register scoped file watchers.
    workspace_folders: RwLock<Vec<String>>,
    /// Build states for all discovered projects, keyed by project root.
    projects: Arc<Mutex<ProjectMap>>,
    /// Unsaved buffer contents keyed by document URI.
    /// Updated on `didChange`, used by completion to get the latest editor buffer.
    open_buffers: Mutex<HashMap<Url, String>>,
    /// Unified debounced queue for all file events (didChange, didOpen,
    /// didSave, didChangeWatchedFiles). `None` until the initial build completes.
    queue: Mutex<Option<queue::Queue>>,
    /// The `rewatch.lsp` root span, captured at construction so spawned tasks
    /// can set it as their explicit parent.
    root_span: tracing::Span,
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
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
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
                semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
                    SemanticTokensOptions {
                        legend: SemanticTokensLegend {
                            token_types: vec![
                                SemanticTokenType::OPERATOR,            // 0
                                SemanticTokenType::VARIABLE,            // 1
                                SemanticTokenType::TYPE,                // 2
                                SemanticTokenType::new("jsxTag"),       // 3
                                SemanticTokenType::NAMESPACE,           // 4
                                SemanticTokenType::ENUM_MEMBER,         // 5
                                SemanticTokenType::PROPERTY,            // 6
                                SemanticTokenType::new("jsxLowercase"), // 7
                            ],
                            token_modifiers: vec![],
                        },
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                        range: None,
                        ..Default::default()
                    },
                )),
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
        self.initial_build(workspaces).await;
        self.recheck_open_buffers().await;

        // Start the unified queue now that initial build state is available.
        if let Ok(mut q) = self.queue.lock() {
            *q = Some(queue::Queue::new(
                Arc::clone(&self.projects),
                self.client.clone(),
                self.root_span.clone(),
            ));
        }

        self.client
            .send_notification::<notifications::BuildFinished>(notifications::BuildFinishedParams {})
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let Some(file_path) = uri_to_file_path(&params.text_document.uri, "didOpen") else {
            return;
        };

        let content = params.text_document.text;
        let needs_fallback = {
            let _span = tracing::info_span!("lsp.did_open", file = %file_path.display()).entered();

            if let Ok(mut buffers) = self.open_buffers.lock() {
                buffers.insert(params.text_document.uri.clone(), content.clone());
            }

            // Enqueue into the queue if available (post-initial-build).
            if let Ok(q) = self.queue.lock()
                && let Some(ref queue) = *q
            {
                queue.enqueue_typecheck(
                    params.text_document.uri.clone(),
                    file_path.clone(),
                    content.clone(),
                );
                false
            } else {
                true
            }
        };

        // Fall back to synchronous typecheck if opened before the initial build.
        if needs_fallback {
            self.typecheck_buffer(&params.text_document.uri, &file_path, &content)
                .await;
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
        let _span = tracing::info_span!("lsp.did_change", file = %file_path.display()).entered();

        let content = match params.content_changes.into_iter().last() {
            Some(change) => change.text,
            None => return,
        };

        // Store the latest buffer content for completion requests.
        if let Ok(mut buffers) = self.open_buffers.lock() {
            buffers.insert(params.text_document.uri.clone(), content.clone());
        }

        // Enqueue into the unified queue (debounced, batched).
        if let Ok(q) = self.queue.lock()
            && let Some(ref queue) = *q
        {
            queue.enqueue_typecheck(params.text_document.uri, file_path, content);
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let Some(file_path) = uri_to_file_path(&params.text_document.uri, "didSave") else {
            return;
        };
        let _span = tracing::info_span!("lsp.did_save", file = %file_path.display()).entered();
        if let Ok(q) = self.queue.lock()
            && let Some(ref queue) = *q
        {
            queue.enqueue_build(params.text_document.uri, file_path);
        }
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let _span =
            tracing::info_span!("lsp.did_change_watched_files", file_count = params.changes.len()).entered();
        if let Ok(q) = self.queue.lock()
            && let Some(ref queue) = *q
        {
            // Collect full-build requests grouped by project root to avoid
            // sending redundant FullBuild events for the same project.
            let mut full_build_requests: HashMap<PathBuf, Vec<Url>> = HashMap::new();

            for event in &params.changes {
                let Some(file_path) = uri_to_file_path(&event.uri, "didChangeWatchedFiles") else {
                    continue;
                };
                if !matches!(
                    file_path.extension().and_then(|e| e.to_str()),
                    Some("res" | "resi")
                ) {
                    continue;
                }

                match event.typ {
                    FileChangeType::CHANGED => {
                        let _span =
                            tracing::info_span!("lsp.enqueue_build", file = %file_path.display()).entered();
                        queue.enqueue_build(event.uri.clone(), file_path);
                    }
                    FileChangeType::CREATED | FileChangeType::DELETED => {
                        let event_kind = if event.typ == FileChangeType::CREATED {
                            "created"
                        } else {
                            "deleted"
                        };
                        let _span = tracing::info_span!("lsp.enqueue_full_build", file = %file_path.display(), kind = event_kind).entered();
                        // Look up which project this file belongs to
                        let project_root = self
                            .projects
                            .lock()
                            .ok()
                            .and_then(|guard| guard.project_root_for_path(&file_path));

                        if let Some(root) = project_root {
                            let deleted_uri = if event.typ == FileChangeType::DELETED {
                                vec![event.uri.clone()]
                            } else {
                                vec![]
                            };
                            full_build_requests.entry(root).or_default().extend(deleted_uri);
                        } else {
                            tracing::debug!(
                                file = %file_path.display(),
                                "File does not belong to any known project, ignoring"
                            );
                        }
                    }
                    _ => {}
                }
            }

            for (root, deleted_uris) in full_build_requests {
                queue.enqueue_full_build(root, deleted_uris);
            }
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "completion") else {
            return Ok(None);
        };

        Ok(completion::handle(
            &self.open_buffers,
            &self.projects,
            &file_path,
            uri,
            params.text_document_position.position,
        ))
    }

    async fn completion_resolve(&self, item: CompletionItem) -> Result<CompletionItem> {
        Ok(completion::handle_resolve(&self.projects, item))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "hover") else {
            return Ok(None);
        };

        Ok(hover::handle(
            &self.open_buffers,
            &self.projects,
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
            &self.projects,
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
            &self.projects,
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
            &self.projects,
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
            &self.projects,
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
            &self.projects,
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
            &self.projects,
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

        Ok(document_symbol::handle(
            &self.open_buffers,
            &self.projects,
            &file_path,
            uri,
        ))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "code_lens") else {
            return Ok(None);
        };

        Ok(code_lens::handle(
            &self.open_buffers,
            &self.projects,
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
            &self.projects,
            &file_path,
            uri,
            params.range,
        ))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "code_action") else {
            return Ok(None);
        };

        Ok(code_action::handle(
            &self.open_buffers,
            &self.projects,
            &file_path,
            uri,
            params.range,
        ))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "semantic_tokens") else {
            return Ok(None);
        };

        Ok(semantic_tokens::handle(
            &self.open_buffers,
            &self.projects,
            &file_path,
            uri,
        ))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let Some(file_path) = uri_to_file_path(uri, "formatting") else {
            return Ok(None);
        };

        let bsc_path = match self.projects.lock() {
            Ok(mut guard) => match guard.get_for_uri(uri) {
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
    /// Run the initial build for all workspaces and publish diagnostics.
    async fn initial_build(&self, workspaces: Vec<initialize::DiscoveredWorkspace>) {
        let mut all_diagnostics: Vec<BscDiagnostic> = Vec::new();
        for (state, diagnostics) in initial_build::run_all(workspaces) {
            all_diagnostics.extend(diagnostics);
            if let Ok(mut map) = self.projects.lock() {
                let root = state.build_state.project_context.get_root_path().to_path_buf();
                map.states.insert(root, state);
            }
        }

        self.publish_diagnostics(&all_diagnostics).await;

        let roots: Vec<_> = self
            .projects
            .lock()
            .ok()
            .map(|map| map.states.keys().map(|p| p.display().to_string()).collect())
            .unwrap_or_default();
        self.client
            .log_message(
                MessageType::INFO,
                format!("rescript-lsp build states for project roots: {roots:?}"),
            )
            .await;
    }

    /// Re-typecheck any buffers that were already open before the build finished.
    ///
    /// The initial build diagnostics come from the on-disk file, but the buffer
    /// content may differ. This brings diagnostics in sync with the actual buffer.
    ///
    /// Uses `typecheck_buffer()` (synchronous, pre-queue) because this runs
    /// before the typecheck queue is started in `initialized()`.
    async fn recheck_open_buffers(&self) {
        let open_buffers: Vec<(Url, String)> = self
            .open_buffers
            .lock()
            .ok()
            .map(|buffers| buffers.iter().map(|(u, c)| (u.clone(), c.clone())).collect())
            .unwrap_or_default();

        for (uri, content) in open_buffers {
            let Some(file_path) = uri_to_file_path(&uri, "recheck_open_buffers") else {
                continue;
            };
            self.typecheck_buffer(&uri, &file_path, &content).await;
        }
    }

    /// Typecheck a single buffer against the current build state and publish diagnostics.
    ///
    /// Does nothing if the build state is not yet available for this URI.
    async fn typecheck_buffer(&self, uri: &Url, file_path: &Path, content: &str) {
        let diagnostics = {
            let mut guard = match self.projects.lock() {
                Ok(g) => g,
                Err(_) => return,
            };
            let Some(build_state) = guard.get_for_uri(uri) else {
                return;
            };
            typecheck::run(build_state, file_path, content)
        };

        let diags: Vec<Diagnostic> = diagnostics.iter().map(to_lsp_diagnostic).collect();
        self.client.publish_diagnostics(uri.clone(), diags, None).await;
    }

    /// Publish diagnostics grouped by file.
    async fn publish_diagnostics(&self, diagnostics: &[BscDiagnostic]) {
        let by_file = group_by_file(diagnostics);

        for (uri, diags) in &by_file {
            self.client
                .publish_diagnostics(uri.clone(), diags.clone(), None)
                .await;
        }
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

#[tracing::instrument(name = "rewatch.lsp", skip_all)]
pub async fn run_stdio() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        workspace_folders: RwLock::new(Vec::new()),
        projects: Arc::new(Mutex::new(ProjectMap::new())),
        open_buffers: Mutex::new(HashMap::new()),
        queue: Mutex::new(None),
        root_span: tracing::Span::current(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
