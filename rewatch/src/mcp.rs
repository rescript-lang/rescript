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
use serde::Deserialize;

// Suggested actions mirror compiler/ext/suggested_actions.ml
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "PascalCase")]
enum SuggestedAction {
    ApplyAutomaticMigrationsForFullProject,
    ApplyAutomaticMigrationsForCurrentFile,
}

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
          No writes. Only this file (not dependents). Returns 'OK' if clean.\n\
        - perform-action(path, actionId): Applies an action suggested by the compiler/diagnosis."
            .to_string()
    }

    fn capabilities(&self) -> ServerCapabilities {
        CapabilitiesBuilder::new().with_tools(false).build()
    }

    fn list_tools(&self) -> Vec<mcp_spec::tool::Tool> {
        vec![
            Tool::new(
                "diagnose",
                "Quick per-file diagnostics (shows compiler warnings, errors) after edits; prefer before full build; no writes; only this file.",
                json!({
                    "type": "object",
                    "properties": {"path": {"type": "string"}},
                    "required": ["path"],
                    "additionalProperties": false
                }),
            ),
            Tool::new(
                "perform-action",
                "Applies an action suggested by the compiler/diagnosis.",
                json!({
                    "type": "object",
                    "properties": {
                        "path": {"type": "string"},
                        // Accept either a string or an object so clients can pass raw JSON, not stringified JSON
                        "actionId": {"anyOf": [{"type": "string"}, {"type": "object"}]}
                    },
                    "required": ["path", "actionId"],
                    "additionalProperties": false
                }),
            ),
        ]
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
                "perform-action" => {
                    let args = arguments;
                    match tokio::task::spawn_blocking(move || perform_action_impl(args)).await {
                        Ok(Ok(())) => Ok(vec![Content::text("OK")]),
                        Ok(Err(e)) => Err(mcp_spec::handler::ToolError::ExecutionError(e.to_string())),
                        Err(join_err) => Err(mcp_spec::handler::ToolError::ExecutionError(format!(
                            "Join error: {join_err}"
                        ))),
                    }
                }
                other => Err(mcp_spec::handler::ToolError::NotFound(format!(
                    "Unknown tool: {other}"
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

fn perform_action_impl(arguments: serde_json::Value) -> anyhow::Result<()> {
    use anyhow::anyhow;

    // Validate required arguments
    let path_arg = arguments
        .get("path")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required string argument 'path'"))?;

    let action_id_str = arguments
        .get("actionId")
        .and_then(|v| v.as_str())
        .ok_or_else(|| anyhow!("Missing required string argument 'actionId'"))?;

    // actionId might be:
    // - A plain string like: "ApplyAutomaticMigrationsForCurrentFile"
    // - Stringified JSON of a string: "\"ApplyAutomaticMigrationsForCurrentFile\""
    // - Stringified JSON of an object with an action string (future-proof)
    // Accept them in this order: plain string → JSON enum string → JSON object
    let action: SuggestedAction = match action_id_str {
        // Plain string case
        "ApplyAutomaticMigrationsForCurrentFile" => SuggestedAction::ApplyAutomaticMigrationsForCurrentFile,
        "ApplyAutomaticMigrationsForFullProject" => SuggestedAction::ApplyAutomaticMigrationsForFullProject,
        // Otherwise try JSON parsing paths
        _ => {
            // Try to parse as a JSON string directly into enum (e.g. "\"Apply…\"")
            match serde_json::from_str::<SuggestedAction>(action_id_str) {
                Ok(a) => a,
                Err(_) => {
                    // Try a generic JSON value to support string or object shapes
                    let v: serde_json::Value = serde_json::from_str(action_id_str)
                        .map_err(|e| anyhow!(format!("Invalid actionId JSON or unsupported action: {e}")))?;
                    match v {
                        serde_json::Value::String(s) => match s.as_str() {
                            "ApplyAutomaticMigrationsForCurrentFile" => {
                                SuggestedAction::ApplyAutomaticMigrationsForCurrentFile
                            }
                            "ApplyAutomaticMigrationsForFullProject" => {
                                SuggestedAction::ApplyAutomaticMigrationsForFullProject
                            }
                            other => return Err(anyhow!(format!("Unknown action kind: {other}"))),
                        },
                        serde_json::Value::Object(map) => {
                            let s = map
                                .get("action")
                                .and_then(|v| v.as_str())
                                .or_else(|| map.get("kind").and_then(|v| v.as_str()))
                                .or_else(|| map.get("type").and_then(|v| v.as_str()))
                                .ok_or_else(|| {
                                    anyhow!("actionId JSON did not contain a recognizable action string")
                                })?;
                            match s {
                                "ApplyAutomaticMigrationsForCurrentFile" => {
                                    SuggestedAction::ApplyAutomaticMigrationsForCurrentFile
                                }
                                "ApplyAutomaticMigrationsForFullProject" => {
                                    SuggestedAction::ApplyAutomaticMigrationsForFullProject
                                }
                                other => return Err(anyhow!(format!("Unknown action kind: {other}"))),
                            }
                        }
                        _ => {
                            return Err(anyhow!(
                                "actionId JSON did not contain a recognizable action string"
                            ));
                        }
                    }
                }
            }
        }
    };

    let abs_path = helpers::get_abs_path(Path::new(path_arg));

    // Find project root (nearest config) for both actions
    let package_root = helpers::get_nearest_config(&abs_path).ok_or_else(|| {
        anyhow!(format!(
            "Could not find rescript.json/bsconfig.json for {}",
            abs_path.display()
        ))
    })?;

    let project = ProjectContext::new(&package_root)?;
    let bin_dir = build::get_compiler_info(&project)?
        .bsc_path
        .parent()
        .ok_or_else(|| anyhow!("Could not determine bin directory from bsc path"))?
        .to_path_buf();
    let rescript_tools = bin_dir.join("rescript-tools.exe");
    if !rescript_tools.exists() {
        return Err(anyhow!(format!(
            "Could not find rescript-tools at {}",
            rescript_tools.display()
        )));
    }

    let run = |args: Vec<&str>, cwd: &Path| -> anyhow::Result<()> {
        let output = Command::new(&rescript_tools)
            .current_dir(cwd)
            .args(args)
            .output()
            .map_err(|e| anyhow!(format!("Failed to run rescript-tools: {e}")))?;
        if output.status.success() {
            Ok(())
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            let mut msg = String::new();
            if helpers::contains_ascii_characters(&stderr) {
                msg.push_str(&stderr);
            }
            if helpers::contains_ascii_characters(&stdout) {
                if !msg.is_empty() && !msg.ends_with('\n') {
                    msg.push('\n');
                }
                msg.push_str(&stdout);
            }
            if msg.trim().is_empty() {
                Err(anyhow!("rescript-tools failed"))
            } else {
                Err(anyhow!(msg.trim().to_string()))
            }
        }
    };

    match action {
        SuggestedAction::ApplyAutomaticMigrationsForCurrentFile => {
            if abs_path.is_dir() {
                return Err(anyhow!("Expected a file path for single-file migration"));
            }
            run(
                vec!["migrate", abs_path.to_string_lossy().as_ref()],
                &package_root,
            )
        }
        SuggestedAction::ApplyAutomaticMigrationsForFullProject => {
            // Use the project root for migrate-all
            run(
                vec!["migrate-all", package_root.to_string_lossy().as_ref()],
                &package_root,
            )
        }
    }
}
