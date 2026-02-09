mod analysis;
mod code_action;
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
mod semantic_tokens;
mod signature_help;
mod type_definition;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Mutex, RwLock};

use rayon::prelude::*;

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
}

impl ProjectMap {
    fn new() -> Self {
        ProjectMap {
            states: HashMap::new(),
            uri_cache: HashMap::new(),
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

    /// Get the build state for a file URI (immutable).
    pub(crate) fn get_for_uri(&mut self, uri: &Url) -> Option<&BuildCommandState> {
        let root = self.project_root_for(uri)?;
        self.states.get(&root)
    }

    /// Get the build state for a file URI (mutable).
    fn get_mut_for_uri(&mut self, uri: &Url) -> Option<&mut BuildCommandState> {
        let root = self.project_root_for(uri)?;
        self.states.get_mut(&root)
    }
}

struct Backend {
    client: Client,
    /// Workspace folder paths received during `initialize`.
    /// Stored so that `initialized` can read rescript.json and register scoped file watchers.
    workspace_folders: RwLock<Vec<String>>,
    /// Build states for all discovered projects, keyed by project root.
    projects: Mutex<ProjectMap>,
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

        // Partition workspaces into conflict groups. Workspaces sharing a
        // package path must build sequentially (they write to the same
        // lib/lsp/ directory). Independent groups run in parallel.
        let groups = Self::partition_workspaces(workspaces);

        // Capture the current tracing span so rayon tasks can use it as an
        // explicit parent.  Without this, rayon work-stealing causes spans to
        // randomly nest under whichever span was last entered on that thread.
        let parent_span = tracing::Span::current();

        // Each group builds its members sequentially, but groups run in
        // parallel with each other via rayon.
        let all_results: Vec<_> = groups
            .into_par_iter()
            .flat_map(|group| {
                group
                    .into_iter()
                    .filter_map(|workspace| match initial_build::run(workspace, &parent_span) {
                        Ok((state, diagnostics)) => Some((state, diagnostics)),
                        Err(e) => {
                            tracing::error!("Initial build failed: {e}");
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        let mut all_diagnostics: Vec<BscDiagnostic> = Vec::new();
        for (state, diagnostics) in all_results {
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
            let mut guard = match self.projects.lock() {
                Ok(g) => g,
                Err(_) => return,
            };
            let Some(build_state) = guard.get_for_uri(&params.text_document.uri) else {
                tracing::warn!("didChange: no build state available");
                return;
            };
            did_change::run(build_state, &file_path, &content)
        };

        let by_file = Self::group_by_file(&diagnostics);
        let uri = &params.text_document.uri;
        let diags = by_file.get(uri).cloned().unwrap_or_default();
        self.client.publish_diagnostics(uri.clone(), diags, None).await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let Some(file_path) = uri_to_file_path(&params.text_document.uri, "didSave") else {
            return;
        };

        let result = {
            let mut guard = match self.projects.lock() {
                Ok(g) => g,
                Err(_) => return,
            };
            let Some(build_state) = guard.get_mut_for_uri(&params.text_document.uri) else {
                tracing::warn!("didSave: no build state available");
                return;
            };
            did_save::run(build_state, &file_path)
        };

        let by_file = Self::group_by_file(&result.diagnostics);
        for touched in &result.touched_files {
            if let Ok(uri) = Url::from_file_path(touched) {
                let diags = by_file.get(&uri).cloned().unwrap_or_default();
                self.client.publish_diagnostics(uri, diags, None).await;
            }
        }

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
    /// Publish diagnostics grouped by file.
    async fn publish_diagnostics(&self, diagnostics: &[BscDiagnostic]) {
        let by_file = Self::group_by_file(diagnostics);

        for (uri, diags) in &by_file {
            self.client
                .publish_diagnostics(uri.clone(), diags.clone(), None)
                .await;
        }
    }

    /// Partition workspaces into groups that can be built in parallel.
    ///
    /// Two workspaces conflict when they share a package path — both would write
    /// to the same `lib/lsp/` directory, causing a race. We use union-find to
    /// merge conflicting workspaces into the same group. Groups are sorted by
    /// the project name of their first workspace so the build order is deterministic.
    /// Within each group, workspaces are also sorted by project name.
    fn partition_workspaces(
        workspaces: Vec<initialize::DiscoveredWorkspace>,
    ) -> Vec<Vec<initialize::DiscoveredWorkspace>> {
        use std::collections::HashSet;

        let n = workspaces.len();
        if n <= 1 {
            return vec![workspaces];
        }

        // Collect the set of package paths for each workspace.
        let pkg_paths: Vec<HashSet<PathBuf>> = workspaces
            .iter()
            .map(|ws| ws.packages.values().map(|p| p.path.clone()).collect())
            .collect();

        // Union-find: parent[i] is the representative of workspace i's group.
        let mut parent: Vec<usize> = (0..n).collect();
        fn find(parent: &mut [usize], mut i: usize) -> usize {
            while parent[i] != i {
                parent[i] = parent[parent[i]];
                i = parent[i];
            }
            i
        }
        fn union(parent: &mut [usize], a: usize, b: usize) {
            let ra = find(parent, a);
            let rb = find(parent, b);
            if ra != rb {
                parent[rb] = ra;
            }
        }

        // Merge workspaces that share any package path.
        for i in 0..n {
            for j in (i + 1)..n {
                if !pkg_paths[i].is_disjoint(&pkg_paths[j]) {
                    union(&mut parent, i, j);
                }
            }
        }

        // Group workspaces by their root representative.
        let mut group_map: HashMap<usize, Vec<usize>> = HashMap::new();
        for i in 0..n {
            group_map.entry(find(&mut parent, i)).or_default().push(i);
        }

        // Sort groups deterministically by first workspace's project name.
        let mut groups: Vec<Vec<usize>> = group_map.into_values().collect();
        for group in &mut groups {
            group.sort_by(|a, b| {
                let name_a = &workspaces[*a].project_context.get_root_config().name;
                let name_b = &workspaces[*b].project_context.get_root_config().name;
                name_a.cmp(name_b)
            });
        }
        groups.sort_by(|a, b| {
            let name_a = &workspaces[a[0]].project_context.get_root_config().name;
            let name_b = &workspaces[b[0]].project_context.get_root_config().name;
            name_a.cmp(name_b)
        });

        // Convert indices to actual workspaces. We need to consume the vec,
        // so we move workspaces into an indexed vec of Options first.
        let mut slots: Vec<Option<initialize::DiscoveredWorkspace>> =
            workspaces.into_iter().map(Some).collect();

        groups
            .into_iter()
            .map(|indices| indices.into_iter().filter_map(|i| slots[i].take()).collect())
            .collect()
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
        projects: Mutex::new(ProjectMap::new()),
        open_buffers: Mutex::new(HashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
