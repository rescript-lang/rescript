mod initialize;

use std::sync::RwLock;

use ahash::AHashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::build::packages::Package;

struct Backend {
    client: Client,
    /// Workspace folder paths received during `initialize`.
    /// Stored so that `initialized` can read rescript.json and register scoped file watchers.
    workspace_folders: RwLock<Vec<String>>,
    /// Packages discovered during initialization.
    /// Available for the rest of the LSP lifecycle (builds, diagnostics, analysis).
    packages: RwLock<AHashMap<String, Package>>,
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
        let folders = self
            .workspace_folders
            .read()
            .map(|wf| wf.clone())
            .unwrap_or_default();

        let packages = initialize::register_file_watchers(&self.client, &folders).await;

        if let Ok(mut pkgs) = self.packages.write() {
            *pkgs = packages;
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub async fn run_stdio() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        workspace_folders: RwLock::new(Vec::new()),
        packages: RwLock::new(AHashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
