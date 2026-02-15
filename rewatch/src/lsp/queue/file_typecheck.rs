use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};

use rayon::prelude::*;
use tower_lsp::Client;
use tower_lsp::lsp_types::Url;

use super::super::file_args::{BuildCommandStateExt, TypecheckArgs};
use super::super::{ProjectMap, publish_and_store, to_lsp_diagnostic};
use super::PendingFileTypecheck;
use crate::build::build_types::{CompileMode, OutputTarget};
use crate::build::deps;
use crate::build::diagnostics::BscDiagnostic;
use crate::lsp::diagnostic_store::DiagnosticStore;

/// Per-file metadata extracted from the build state under lock.
struct FileContext {
    uri: Url,
    content: String,
    generation: u64,
    file_args: TypecheckArgs,
}

/// Result of typechecking a single file.
struct TypecheckResult {
    uri: Url,
    generation: u64,
    diagnostics: Vec<BscDiagnostic>,
}

/// Typecheck unsaved buffer contents. Extracts compiler args under a brief
/// lock, then runs `bsc` in parallel (with wave-based ordering for
/// cross-file consistency). Stale results are discarded before publishing.
pub(super) async fn run(
    typechecks: HashMap<Url, PendingFileTypecheck>,
    projects: &Arc<Mutex<ProjectMap>>,
    generations: &Arc<Mutex<HashMap<Url, u64>>>,
    client: &Client,
    diagnostic_store: Option<&DiagnosticStore>,
) {
    let requests: Vec<(Url, PathBuf, String, u64)> = typechecks
        .into_iter()
        .map(|(uri, tc)| (uri, tc.file_path, tc.content, tc.generation))
        .collect();

    if requests.is_empty() {
        return;
    }

    let projects_clone = Arc::clone(projects);
    let parent_span = tracing::Span::current();

    let results =
        tokio::task::spawn_blocking(move || batch_typecheck(requests, &projects_clone, &parent_span))
            .await
            .unwrap_or_else(|e| {
                tracing::error!("typecheck flush task panicked: {e}");
                Vec::new()
            });

    // Publish diagnostics for non-stale results
    for result in results {
        let is_stale = match generations.lock() {
            Ok(gens) => gens
                .get(&result.uri)
                .map(|&latest| result.generation < latest)
                .unwrap_or(false),
            Err(e) => {
                tracing::error!("generations mutex poisoned in staleness check: {e}");
                false
            }
        };

        if !is_stale {
            let diags: Vec<_> = result.diagnostics.iter().map(to_lsp_diagnostic).collect();
            publish_and_store(client, diagnostic_store, result.uri.clone(), diags).await;

            // Prune the generation entry if it still matches, so the map
            // doesn't grow unboundedly over long editing sessions.
            if let Ok(mut gens) = generations.lock()
                && gens.get(&result.uri) == Some(&result.generation)
            {
                gens.remove(&result.uri);
            }
        }
    }
}

fn batch_typecheck(
    requests: Vec<(Url, PathBuf, String, u64)>,
    projects: &Arc<Mutex<ProjectMap>>,
    parent_span: &tracing::Span,
) -> Vec<TypecheckResult> {
    let batch_span = tracing::info_span!(
        parent: parent_span,
        "lsp.flush.file_typecheck",
        file_count = requests.len()
    );
    let _batch_entered = batch_span.enter();

    // Extract file contexts under lock (brief)
    let file_contexts: Vec<FileContext> = {
        let mut guard = match projects.lock() {
            Ok(g) => g,
            Err(e) => {
                tracing::error!("projects mutex poisoned in typecheck flush: {e}");
                return Vec::new();
            }
        };
        requests
            .into_iter()
            .filter_map(|(uri, file_path, content, generation)| {
                extract_file_context(&mut guard, uri, file_path, content, generation)
            })
            .collect()
    };
    // Lock is released here

    if file_contexts.is_empty() {
        return Vec::new();
    }

    // Single-file fast path: skip dependency analysis
    if file_contexts.len() == 1 {
        let ctx = &file_contexts[0];
        let child = tracing::info_span!(
            parent: &batch_span,
            "lsp.flush.file_typecheck.file",
            module = %ctx.file_args.module_name
        );
        let _entered = child.enter();
        let diagnostics = typecheck_single_file(ctx);
        return vec![TypecheckResult {
            uri: ctx.uri.clone(),
            generation: ctx.generation,
            diagnostics,
        }];
    }

    // Multi-file batch: parse → compute deps → typecheck in waves
    batch_typecheck_multi(file_contexts)
}

fn extract_file_context(
    project_map: &mut ProjectMap,
    uri: Url,
    file_path: PathBuf,
    content: String,
    generation: u64,
) -> Option<FileContext> {
    let project_root = project_map.project_root_for(&uri)?;
    let build_state = project_map.states.get(&project_root)?;
    let file_args = build_state.get_typecheck_args(
        &file_path,
        &content,
        OutputTarget::Lsp,
        CompileMode::TypecheckOnly,
    )?;

    Some(FileContext {
        uri,
        content,
        generation,
        file_args,
    })
}

fn typecheck_single_file(ctx: &FileContext) -> Vec<BscDiagnostic> {
    super::super::typecheck::typecheck_with_args(&ctx.file_args, &ctx.content)
}

fn batch_typecheck_multi(file_contexts: Vec<FileContext>) -> Vec<TypecheckResult> {
    let mut results: Vec<TypecheckResult> = Vec::new();

    // Phase 1: Parse all files in parallel to produce .ast files
    let parse_span = tracing::info_span!("lsp.flush.file_typecheck.parse", file_count = file_contexts.len());
    let parse_results: Vec<(usize, Result<(), Vec<BscDiagnostic>>)> = {
        let _entered = parse_span.enter();
        file_contexts
            .par_iter()
            .enumerate()
            .map(|(idx, ctx)| (idx, parse_file_to_ast(ctx, &parse_span)))
            .collect()
    };

    let mut successfully_parsed: Vec<usize> = Vec::new();
    for (idx, parse_result) in parse_results {
        match parse_result {
            Ok(()) => successfully_parsed.push(idx),
            Err(diags) => {
                results.push(TypecheckResult {
                    uri: file_contexts[idx].uri.clone(),
                    generation: file_contexts[idx].generation,
                    diagnostics: diags,
                });
            }
        }
    }

    if successfully_parsed.is_empty() {
        return results;
    }

    // Phase 2: Read deps from .ast files, compute in-batch ordering
    let in_batch_deps = compute_in_batch_deps(&file_contexts, &successfully_parsed);

    // Phase 3: Typecheck in waves (dependency order, parallel within waves)
    let typecheck_results = typecheck_in_waves(&file_contexts, &successfully_parsed, &in_batch_deps);
    results.extend(typecheck_results);

    results
}

fn parse_file_to_ast(ctx: &FileContext, parent: &tracing::Span) -> Result<(), Vec<BscDiagnostic>> {
    let _span = tracing::info_span!(
        parent: parent,
        "lsp.flush.file_typecheck.parse_file",
        module = %ctx.file_args.module_name
    )
    .entered();

    let mut args = ctx.file_args.parser_args.clone();
    let source_arg = args.pop();
    args.push("-bs-read-stdin".into());
    if let Some(arg) = source_arg {
        args.push(arg);
    }

    // Ensure the .ast output directory exists
    if let Some(parent) = ctx
        .file_args
        .build_path_abs
        .join(&ctx.file_args.ast_path)
        .parent()
    {
        let _ = std::fs::create_dir_all(parent);
    }

    let mut child = Command::new(&ctx.file_args.bsc_path)
        .current_dir(&ctx.file_args.build_path_abs)
        .args(&args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| {
            tracing::warn!("parse_file_to_ast: failed to spawn bsc: {e}");
            Vec::new()
        })?;

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(ctx.content.as_bytes());
    }

    let output = child.wait_with_output().map_err(|e| {
        tracing::warn!("parse_file_to_ast: bsc invocation failed: {e}");
        Vec::<BscDiagnostic>::new()
    })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(super::super::typecheck::parse_and_remap_diagnostics(
            &stderr,
            &ctx.file_args.source_path,
            &ctx.file_args.package_path,
        ));
    }

    Ok(())
}

fn compute_in_batch_deps(
    file_contexts: &[FileContext],
    successfully_parsed: &[usize],
) -> HashMap<usize, Vec<usize>> {
    let mut name_to_idx: HashMap<String, usize> = HashMap::new();
    for &idx in successfully_parsed {
        let module_name = &file_contexts[idx].file_args.module_name;
        name_to_idx.insert(module_name.clone(), idx);
        if let Some(simple) = module_name.split('-').next()
            && simple != module_name
        {
            name_to_idx.insert(simple.to_string(), idx);
        }
    }

    let mut deps_map: HashMap<usize, Vec<usize>> = HashMap::new();
    for &idx in successfully_parsed {
        let ctx = &file_contexts[idx];
        let ast_file_path = ctx.file_args.build_path_abs.join(&ctx.file_args.ast_path);
        let raw_deps = deps::read_raw_deps(&ast_file_path);
        let dep_names: Vec<String> = raw_deps
            .iter()
            .filter_map(|d| d.split('.').next().map(|s| s.to_string()))
            .collect();

        let in_batch_deps: Vec<usize> = dep_names
            .iter()
            .filter_map(|name| name_to_idx.get(name).copied())
            .filter(|&dep_idx| dep_idx != idx)
            .collect();

        deps_map.insert(idx, in_batch_deps);
    }

    deps_map
}

fn typecheck_in_waves(
    file_contexts: &[FileContext],
    successfully_parsed: &[usize],
    in_batch_deps: &HashMap<usize, Vec<usize>>,
) -> Vec<TypecheckResult> {
    let mut results = Vec::new();
    let mut completed: HashSet<usize> = HashSet::new();
    let remaining: HashSet<usize> = successfully_parsed.iter().copied().collect();

    loop {
        let wave: Vec<usize> = remaining
            .difference(&completed)
            .copied()
            .filter(|&idx| {
                in_batch_deps
                    .get(&idx)
                    .map(|deps| deps.iter().all(|d| completed.contains(d)))
                    .unwrap_or(true)
            })
            .collect();

        if wave.is_empty() {
            // Either all done, or circular dependency — typecheck remaining best-effort
            let stuck: Vec<usize> = remaining.difference(&completed).copied().collect();
            if stuck.is_empty() {
                break;
            }
            let fallback_span =
                tracing::info_span!("lsp.flush.file_typecheck.wave", file_count = stuck.len());
            let wave_results: Vec<TypecheckResult> = {
                let _entered = fallback_span.enter();
                stuck
                    .par_iter()
                    .map(|&idx| typecheck_from_ast(&file_contexts[idx], &fallback_span))
                    .collect()
            };
            results.extend(wave_results);
            break;
        }

        let wave_span = tracing::info_span!("lsp.flush.file_typecheck.wave", file_count = wave.len());
        let wave_results: Vec<TypecheckResult> = {
            let _entered = wave_span.enter();
            wave.par_iter()
                .map(|&idx| typecheck_from_ast(&file_contexts[idx], &wave_span))
                .collect()
        };

        for &idx in &wave {
            completed.insert(idx);
        }
        results.extend(wave_results);
    }

    results
}

fn typecheck_from_ast(ctx: &FileContext, parent: &tracing::Span) -> TypecheckResult {
    let _span = tracing::info_span!(
        parent: parent,
        "lsp.flush.file_typecheck.file",
        module = %ctx.file_args.module_name
    )
    .entered();

    let mut args = vec!["-I".to_string(), "src".to_string()];
    args.extend_from_slice(&ctx.file_args.ast_compiler_args);

    let output = Command::new(&ctx.file_args.bsc_path)
        .current_dir(&ctx.file_args.build_path_abs)
        .args(&args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output();

    let diagnostics = match output {
        Ok(output) => {
            let stderr = String::from_utf8_lossy(&output.stderr);
            super::super::typecheck::parse_and_remap_diagnostics(
                &stderr,
                &ctx.file_args.source_path,
                &ctx.file_args.package_path,
            )
        }
        Err(e) => {
            tracing::warn!("typecheck_from_ast: bsc invocation failed: {e}");
            Vec::new()
        }
    };

    TypecheckResult {
        uri: ctx.uri.clone(),
        generation: ctx.generation,
        diagnostics,
    }
}
