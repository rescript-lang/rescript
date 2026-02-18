use super::build_types::*;
use super::compile;
use super::log_config_warnings;
use super::logs;
use crate::build::compiler_info::write_compiler_info;
use crate::helpers;
use crate::helpers::emojis::*;
use crate::sourcedirs;
use console::style;
use indicatif::{ProgressBar, ProgressStyle};
use log::log_enabled;
use std::time::{Duration, Instant};
use tracing::{info_span, instrument};

fn format_step(current: usize, total: usize) -> console::StyledObject<String> {
    style(format!("[{current}/{total}]")).bold().dim()
}

/// Full build: compile all dirty modules + dependents, emit JS.
///
/// Handles progress bars, logs, sourcedirs, compiler-info, and config warnings.
/// Called from CLI `build()` and watcher incremental/full compile.
#[instrument(name = "full_build", skip_all, fields(
    module_count = build_state.modules.len(),
    output = "standard",
))]
pub fn full_build(
    build_state: &mut BuildCommandState,
    output_mode: &OutputMode,
    default_timing: Option<Duration>,
) -> Result<IncrementalBuildResult, IncrementalBuildError> {
    let show_progress = output_mode.show_progress();
    let plain_output = output_mode.plain_output();
    let initial_build = output_mode.initial_build();
    let only_incremental = !initial_build;

    let (all, needs_compile) = super::dirty_modules_for_compile(build_state);

    let current_step = if only_incremental { 2 } else { 3 };
    let total_steps = if only_incremental { 2 } else { 3 };

    if log_enabled!(log::Level::Trace) {
        for (module_name, module) in build_state.modules.iter() {
            if module.needs_compile_for_mode(CompileMode::FullCompile)
                && let Module::SourceFile(sf) = module
            {
                println!(
                    "needs compile: {module_name} (stage: {:?})",
                    sf.compilation_stage()
                );
            }
        }
    };

    let params = CompileParams {
        modules: all,
        filter: CompileFilter::DirtyOnly(needs_compile),
        mode: CompileMode::FullCompile,
        scoped: false,
        output: OutputTarget::Standard,
        show_progress,
    };

    let start_compiling = Instant::now();
    let pb = if !plain_output && show_progress {
        ProgressBar::new(build_state.modules.len().try_into().unwrap())
    } else {
        ProgressBar::hidden()
    };
    pb.set_style(
        ProgressStyle::with_template(&format!(
            "{} {}Compiling... {{spinner}} {{pos}}/{{len}} {{msg}}",
            format_step(current_step, total_steps),
            SWORDS
        ))
        .unwrap(),
    );

    let result =
        match compile::process_in_waves(build_state, &params, || pb.inc(1), |size| pb.set_length(size)) {
            Ok(result) => result,
            Err(e) => {
                return Err(IncrementalBuildError {
                    kind: IncrementalBuildErrorKind::CompileError(Some(e.to_string())),
                    output_mode: output_mode.clone(),
                    diagnostics: vec![],
                    modules: params.modules,
                });
            }
        };

    let compile_duration = start_compiling.elapsed();

    if !output_mode.is_silent() {
        logs::finalize(&build_state.packages);
    }
    sourcedirs::print(build_state, OutputTarget::Standard);
    pb.finish();

    if !result.compile_errors.is_empty() {
        let _error_span = info_span!("build.compile_error", error = %result.compile_errors).entered();
        if show_progress {
            if plain_output {
                eprintln!("Compiled {} modules", result.num_compiled_modules)
            } else {
                eprintln!(
                    "{}{} {}Compiled {} modules in {:.2}s",
                    LINE_CLEAR,
                    format_step(current_step, total_steps),
                    CROSS,
                    result.num_compiled_modules,
                    default_timing.unwrap_or(compile_duration).as_secs_f64()
                );
            }
        }
        if !output_mode.is_silent() && helpers::contains_ascii_characters(&result.compile_warnings) {
            let _warning_span =
                info_span!("build.compile_warning", warning = %result.compile_warnings).entered();
            eprintln!("{}", &result.compile_warnings);
        }
        if initial_build {
            log_config_warnings(build_state);
        }
        if !output_mode.is_silent() && helpers::contains_ascii_characters(&result.compile_errors) {
            eprintln!("{}", &result.compile_errors);
        }
        Err(IncrementalBuildError {
            kind: IncrementalBuildErrorKind::CompileError(None),
            output_mode: output_mode.clone(),
            diagnostics: result.to_diagnostics(),
            modules: params.modules,
        })
    } else {
        if show_progress {
            if plain_output {
                println!("Compiled {} modules", result.num_compiled_modules)
            } else {
                println!(
                    "{}{} {}Compiled {} modules in {:.2}s",
                    LINE_CLEAR,
                    format_step(current_step, total_steps),
                    SWORDS,
                    result.num_compiled_modules,
                    default_timing.unwrap_or(compile_duration).as_secs_f64()
                );
            }
        }

        if !output_mode.is_silent() && helpers::contains_ascii_characters(&result.compile_warnings) {
            eprintln!("{}", &result.compile_warnings);
        }
        if initial_build {
            log_config_warnings(build_state);
        }

        write_compiler_info(build_state, OutputTarget::Standard);

        Ok(IncrementalBuildResult {
            diagnostics: result.to_diagnostics(),
            modules: params.modules,
        })
    }
}
