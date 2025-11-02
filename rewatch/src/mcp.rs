use anyhow::Result;
use mcp_server::router::{CapabilitiesBuilder, Router, RouterService};
use mcp_server::{ByteTransport, Server};
use mcp_spec::{
    content::Content, prompt::Prompt, protocol::ServerCapabilities, resource::Resource, tool::Tool,
};
use serde_json::json;
use std::path::Path;
use std::pin::Pin;
use std::process::Command;

use crate::{build, helpers, project_context::ProjectContext};

#[derive(Clone)]
pub struct RewatchMcp;

impl RewatchMcp {}

impl Router for RewatchMcp {
    fn name(&self) -> String {
        "rescript-mcp".to_string()
    }

    fn instructions(&self) -> String {
        "ReScript MCP server.\n\n\
        Tools:\n\
        - diagnose(path): Quick per-file diagnostics after edits. Prefer this before a full build.\n\
          No writes. Only this file (not dependents). Returns 'OK' if clean."
            .to_string()
    }

    fn capabilities(&self) -> ServerCapabilities {
        CapabilitiesBuilder::new().with_tools(false).build()
    }

    fn list_tools(&self) -> Vec<mcp_spec::tool::Tool> {
        vec![Tool::new(
            "diagnose",
            "Quick per-file diagnostics after edits; prefer before full build; no writes; only this file.",
            json!({
                "type": "object",
                "properties": {"path": {"type": "string"}},
                "required": ["path"],
                "additionalProperties": false
            }),
        )]
    }

    fn call_tool(
        &self,
        tool_name: &str,
        arguments: serde_json::Value,
    ) -> Pin<
        Box<
            dyn futures::Future<Output = Result<Vec<Content>, mcp_spec::handler::ToolError>> + Send + 'static,
        >,
    > {
        let name = tool_name.to_string();
        Box::pin(async move {
            match name.as_str() {
                "diagnose" => {
                    let args = arguments;
                    match tokio::task::spawn_blocking(move || diagnose_impl(args)).await {
                        Ok(Ok((msg, is_error))) => {
                            let text = msg.unwrap_or_else(|| "OK".into());
                            if is_error {
                                Err(mcp_spec::handler::ToolError::ExecutionError(text))
                            } else {
                                Ok(vec![Content::text(text)])
                            }
                        }
                        Ok(Err(e)) => Err(mcp_spec::handler::ToolError::ExecutionError(e.to_string())),
                        Err(join_err) => Err(mcp_spec::handler::ToolError::ExecutionError(format!(
                            "Join error: {join_err}"
                        ))),
                    }
                }
                other => Err(mcp_spec::handler::ToolError::NotFound(format!(
                    "Unknown tool: {}",
                    other
                ))),
            }
        })
    }

    fn list_resources(&self) -> Vec<Resource> {
        Vec::new()
    }

    fn read_resource(
        &self,
        _uri: &str,
    ) -> Pin<
        Box<dyn futures::Future<Output = Result<String, mcp_spec::handler::ResourceError>> + Send + 'static>,
    > {
        Box::pin(async move { Ok(String::new()) })
    }

    fn list_prompts(&self) -> Vec<Prompt> {
        Vec::new()
    }

    fn get_prompt(
        &self,
        _prompt_name: &str,
    ) -> Pin<Box<dyn futures::Future<Output = Result<String, mcp_spec::handler::PromptError>> + Send + 'static>>
    {
        Box::pin(async move { Err(mcp_spec::handler::PromptError::NotFound("prompt".into())) })
    }
}

pub fn run() -> Result<()> {
    let rt = tokio::runtime::Runtime::new()?;
    rt.block_on(async move {
        let router = RewatchMcp;
        let service = RouterService(router);
        let server = Server::new(service);
        let transport = ByteTransport::new(tokio::io::stdin(), tokio::io::stdout());
        server.run(transport).await.map_err(|e| anyhow::anyhow!(e))
    })
}

fn diagnose_impl(arguments: serde_json::Value) -> anyhow::Result<(Option<String>, bool)> {
    use anyhow::anyhow;

    let path = arguments
        .get("path")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required string argument 'path'"))?;

    let file_abs = helpers::get_abs_path(Path::new(path));
    match file_abs.extension().and_then(|e| e.to_str()) {
        Some("res") | Some("resi") => {}
        _ => {
            return Err(anyhow!(
                "Unsupported file extension. Expected a .res or .resi file: {}",
                file_abs.display()
            ));
        }
    }
    if !file_abs.exists() {
        return Err(anyhow!(format!("File not found: {}", file_abs.display())));
    }

    let package_root = helpers::get_nearest_config(&file_abs).ok_or_else(|| {
        anyhow!(format!(
            "Could not find rescript.json/bsconfig.json for {}",
            file_abs.display()
        ))
    })?;

    let lib_bs_dir = build::packages::get_build_path(&package_root);

    let project = ProjectContext::new(&package_root)?;
    let rel = file_abs
        .strip_prefix(&package_root)
        .map_err(|_| anyhow!("File is not inside project root"))?;
    let contents = helpers::read_file(&file_abs).map_err(|e| anyhow!(format!("Failed to read file: {e}")))?;

    let ppx_flags = build::parse::ppx_flags_for_contents(&project, &project.current_config, &contents)?;
    let is_interface = file_abs.extension().map(|e| e == "resi").unwrap_or(false);
    let has_interface = if is_interface {
        true
    } else {
        file_abs.with_extension("resi").exists()
    };
    let is_type_dev = project.current_config.find_is_type_dev_for_path(rel);

    let compiler_args_vec = build::compile::compiler_args_for_diagnostics(
        &project.current_config,
        &file_abs,
        is_interface,
        has_interface,
        &project,
        &None,
        is_type_dev,
        true,
        None,
        ppx_flags,
    )?;

    let bsc = build::get_compiler_info(&project)?.bsc_path;
    let output = Command::new(&bsc)
        .current_dir(&lib_bs_dir)
        .args(&compiler_args_vec)
        .output()
        .map_err(|e| anyhow!(format!("Failed to run bsc (incremental): {e}")))?;

    let mut diag = String::new();
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    if helpers::contains_ascii_characters(&stderr) {
        diag.push_str(&stderr);
    }
    if helpers::contains_ascii_characters(&stdout) {
        if !diag.is_empty() && !diag.ends_with('\n') {
            diag.push('\n');
        }
        diag.push_str(&stdout);
    }

    let is_error = !output.status.success();
    let message_opt = if diag.trim().is_empty() { None } else { Some(diag) };
    Ok((message_opt, is_error))
}
