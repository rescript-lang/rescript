//! Build progress reporters for streaming progress through broadcast channel.
//!
//! These reporters implement the `BuildReporter` trait and send progress messages
//! through a broadcast channel, allowing build progress to be streamed to gRPC clients.
//!
//! Progress events are sent to both:
//! - Broadcast channel (for client UX - build output, errors, warnings)
//! - OpenTelemetry spans (for observability - timing, counts, debugging)

use chrono::Local;
use tokio::sync::broadcast;

use crate::build::{BuildProgress, BuildReporter};

use super::proto::{
    CircularDependency, Cleaned, CleanedCompilerAssets, CleanedJsFiles, Compiled, CompilerError,
    CompilerWarning, Compiling, ConfigWarning, ConfigWarningKind as ProtoConfigWarningKind, DaemonEvent,
    DuplicatedPackage, InitializationError, JsPostBuildOutput, MissingImplementation, ModuleNotFound,
    PackageNameMismatch, PackageTreeError, Parsed, UnallowedDependency,
    UnallowedDependencyGroup as ProtoUnallowedDependencyGroup, daemon_event::Event,
};

/// Reporter that sends progress messages through a tokio broadcast channel.
/// All clients receive updates; each client filters by their client_id.
/// Converts BuildProgress to DaemonEvent before sending.
pub struct BroadcastReporter {
    tx: broadcast::Sender<DaemonEvent>,
    client_id: u64,
}

impl BroadcastReporter {
    pub fn new(tx: broadcast::Sender<DaemonEvent>, client_id: u64) -> Self {
        Self { tx, client_id }
    }
}

impl BuildReporter for BroadcastReporter {
    fn report(&self, progress: BuildProgress) {
        // Emit OpenTelemetry span events for key progress types
        emit_tracing_event(&progress, self.client_id);

        // Skip high-frequency per-module events that are only useful as OTEL traces.
        if matches!(progress, BuildProgress::GeneratingAst { .. }) {
            return;
        }

        // Send to broadcast channel for client UX
        let _ = self.tx.send(progress_to_event(progress, self.client_id));
    }
}

/// Emit tracing events for observability.
/// These go to OpenTelemetry when configured, providing timing and count data.
fn emit_tracing_event(progress: &BuildProgress, client_id: u64) {
    match progress {
        BuildProgress::Cleaned {
            cleaned_count,
            total_count,
            duration_seconds,
            due_to_compiler_update,
        } => {
            tracing::info!(
                client_id = client_id,
                cleaned_count = cleaned_count,
                total_count = total_count,
                duration_seconds = duration_seconds,
                due_to_compiler_update = due_to_compiler_update,
                "build.cleaned"
            );
        }
        BuildProgress::Parsed {
            parsed_count,
            duration_seconds,
        } => {
            tracing::info!(
                client_id = client_id,
                parsed_count = parsed_count,
                duration_seconds = duration_seconds,
                "build.parsed"
            );
        }
        BuildProgress::Compiled {
            compiled_count,
            duration_seconds,
        } => {
            tracing::info!(
                client_id = client_id,
                compiled_count = compiled_count,
                duration_seconds = duration_seconds,
                "build.compiled"
            );
        }
        BuildProgress::CompilerError(message) => {
            tracing::error!(
                client_id = client_id,
                message = %message,
                "build.compiler_error"
            );
        }
        BuildProgress::CircularDependency { cycle_description } => {
            tracing::error!(
                client_id = client_id,
                cycle = %cycle_description,
                "build.circular_dependency"
            );
        }
        BuildProgress::InitializationError(message) => {
            tracing::error!(
                client_id = client_id,
                message = %message,
                "build.initialization_error"
            );
        }
        // Debug-level events for verbose progress
        BuildProgress::Compiling {
            current_count,
            total_count,
        } => {
            tracing::debug!(
                client_id = client_id,
                current_count = current_count,
                total_count = total_count,
                "build.compiling"
            );
        }
        BuildProgress::GeneratingAst { module_name } => {
            tracing::info!(
                client_id = client_id,
                module_name = %module_name,
                "build.generating_ast"
            );
        }
        BuildProgress::JsPostBuildOutput {
            command,
            js_file,
            stdout,
            stderr,
        } => {
            tracing::info!(
                client_id = client_id,
                command = %command,
                js_file = %js_file,
                has_stdout = stdout.is_some(),
                has_stderr = stderr.is_some(),
                "build.js_post_build_output"
            );
        }
        // Other events don't need tracing (warnings go to broadcast only)
        _ => {}
    }
}

/// Helper to create a timestamp string.
fn timestamp() -> String {
    Local::now().format("%H:%M:%S%.3f").to_string()
}

/// Convert BuildProgress to DaemonEvent with client_id.
pub fn progress_to_event(progress: BuildProgress, client_id: u64) -> DaemonEvent {
    let event = match progress {
        BuildProgress::Cleaned {
            cleaned_count,
            total_count,
            duration_seconds,
            due_to_compiler_update,
        } => Event::Cleaned(Cleaned {
            client_id,
            cleaned_count,
            total_count,
            duration_seconds,
            due_to_compiler_update,
        }),
        BuildProgress::Parsed {
            parsed_count,
            duration_seconds,
        } => Event::Parsed(Parsed {
            client_id,
            parsed_count,
            duration_seconds,
        }),
        BuildProgress::Compiling {
            current_count,
            total_count,
        } => Event::Compiling(Compiling {
            client_id,
            current_count,
            total_count,
        }),
        BuildProgress::GeneratingAst { .. } => {
            unreachable!("GeneratingAst is filtered before reaching progress_to_event")
        }
        BuildProgress::Compiled {
            compiled_count,
            duration_seconds,
        } => Event::Compiled(Compiled {
            client_id,
            compiled_count,
            duration_seconds,
        }),
        BuildProgress::CompilerWarning(message) => {
            Event::CompilerWarning(CompilerWarning { client_id, message })
        }
        BuildProgress::CompilerError(message) => Event::CompilerError(CompilerError { client_id, message }),
        BuildProgress::CleanedCompilerAssets { duration_seconds } => {
            Event::CleanedCompilerAssets(CleanedCompilerAssets {
                client_id,
                duration_seconds,
            })
        }
        BuildProgress::CleanedJsFiles {
            suffix,
            duration_seconds,
        } => Event::CleanedJsFiles(CleanedJsFiles {
            client_id,
            suffix,
            duration_seconds,
        }),
        BuildProgress::CircularDependency { cycle_description } => {
            Event::CircularDependency(CircularDependency {
                client_id,
                cycle_description,
            })
        }
        BuildProgress::UnallowedDependency { package_name, groups } => {
            Event::UnallowedDependency(UnallowedDependency {
                client_id,
                package_name,
                groups: groups
                    .into_iter()
                    .map(|g| ProtoUnallowedDependencyGroup {
                        deps_type: g.deps_type,
                        deps: g.deps,
                    })
                    .collect(),
            })
        }
        BuildProgress::PackageTreeError { package_name, error } => {
            Event::PackageTreeError(PackageTreeError {
                client_id,
                package_name,
                error,
            })
        }
        BuildProgress::ModuleNotFound { module_name } => Event::ModuleNotFound(ModuleNotFound {
            client_id,
            module_name,
        }),
        BuildProgress::ConfigWarning {
            package_name,
            field_name,
            kind,
        } => {
            let proto_kind = match kind {
                crate::build::ConfigWarningKind::Unsupported => ProtoConfigWarningKind::ConfigUnsupported,
                crate::build::ConfigWarningKind::Unknown => ProtoConfigWarningKind::ConfigUnknown,
            };
            Event::ConfigWarning(ConfigWarning {
                client_id,
                package_name,
                field_name,
                kind: proto_kind.into(),
            })
        }
        BuildProgress::DuplicatedPackage {
            package_name,
            chosen_path,
            duplicate_path,
            parent_path,
        } => Event::DuplicatedPackage(DuplicatedPackage {
            client_id,
            package_name,
            chosen_path,
            duplicate_path,
            parent_path,
        }),
        BuildProgress::MissingImplementation { interface_file } => {
            Event::MissingImplementation(MissingImplementation {
                client_id,
                interface_file,
            })
        }
        BuildProgress::InitializationError(message) => {
            Event::InitializationError(InitializationError { client_id, message })
        }
        BuildProgress::PackageNameMismatch {
            package_path,
            package_json_name,
            rescript_json_name,
        } => Event::PackageNameMismatch(PackageNameMismatch {
            client_id,
            package_path,
            package_json_name,
            rescript_json_name,
        }),
        BuildProgress::JsPostBuildOutput {
            command,
            js_file,
            stdout,
            stderr,
        } => Event::JsPostBuildOutput(JsPostBuildOutput {
            client_id,
            command,
            js_file,
            stdout,
            stderr,
        }),
    };

    DaemonEvent {
        timestamp: timestamp(),
        event: Some(event),
    }
}
