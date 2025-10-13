use super::build_types::{BuildCommandState, Implementation, Interface, Module, SourceType};
use super::logs;
use super::packages::Package;
use crate::config::{EmbedGenerator, EmbedsConfig};
use ahash::AHashSet;
use anyhow::{Context, Result, anyhow};
use rayon::ThreadPoolBuilder;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant, SystemTime};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmbedRangePos {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmbedRange {
    pub start: EmbedRangePos,
    pub end: EmbedRangePos,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmbedEntry {
    pub tag: String,
    pub context: String,
    pub occurrence_index: u32,
    pub range: EmbedRange,
    pub embed_string: String,
    pub literal_hash: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmbedIndexFile {
    pub version: u32,
    pub module: String,
    pub source_path: String,
    pub embeds: Vec<EmbedEntry>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct ResolutionMapEntry {
    tag: String,
    occurrence_index: u32,
    literal_hash: String,
    target_module: String,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct ResolutionMap {
    version: u32,
    module: String,
    entries: Vec<ResolutionMapEntry>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct GeneratorInput<'a> {
    version: u32,
    tag: &'a str,
    embed_string: &'a str,
    source: GeneratorSource<'a>,
    occurrence_index: u32,
    config: GeneratorConfig<'a>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct GeneratorSource<'a> {
    path: &'a str,
    module: &'a str,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct GeneratorConfig<'a> {
    extra_sources: &'a [String],
    #[serde(skip_serializing_if = "Option::is_none")]
    options: Option<serde_json::Value>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase", tag = "status")]
enum GeneratorOutput {
    #[serde(rename_all = "camelCase")]
    Ok {
        code: String,
        #[serde(default)]
        suffix: Option<String>,
    },
    #[serde(rename_all = "camelCase")]
    Error { errors: serde_json::Value },
}

// Diagnostics shape emitted by generators (best-effort typed parsing)
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GenDiagPos {
    line: u32,
    column: u32,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct GenDiagItem {
    message: String,
    #[serde(default)]
    severity: Option<String>,
    #[serde(default)]
    code: Option<String>,
    #[serde(default)]
    start: Option<GenDiagPos>,
    #[serde(default)]
    end: Option<GenDiagPos>,
}

fn map_embed_pos_to_abs(embed: &EmbedEntry, rel: &GenDiagPos) -> (u32, u32) {
    // Lines and columns are 1-based. When moving beyond the first line, columns reset.
    let abs_line = embed.range.start.line.saturating_add(rel.line.saturating_sub(1));
    let abs_col = if rel.line <= 1 {
        embed.range.start.column.saturating_add(rel.column)
    } else {
        rel.column
    };
    (abs_line, abs_col)
}

fn read_file_lines(path: &Path) -> Vec<String> {
    match fs::read_to_string(path) {
        Ok(s) => s.lines().map(|l| l.to_string()).collect(),
        Err(_) => vec![],
    }
}

fn clamp<T: Ord>(v: T, lo: T, hi: T) -> T {
    std::cmp::min(std::cmp::max(v, lo), hi)
}

fn render_code_frame(
    file_abs: &Path,
    abs_line: u32,
    abs_col: u32,
    abs_end_line: Option<u32>,
    abs_end_col: Option<u32>,
    context: usize,
) -> String {
    let lines = read_file_lines(file_abs);
    if lines.is_empty() {
        return String::new();
    }
    let total = lines.len() as u32;
    let line = clamp(abs_line, 1, total);
    let start_idx = line.saturating_sub(context as u32).saturating_sub(1) as usize;
    let end_idx = std::cmp::min(total, line + context as u32) as usize;
    let mut out = String::new();
    for (i, lno) in ((start_idx + 1)..=end_idx).enumerate() {
        let idx = start_idx + i;
        if lno as u32 == line {
            // caret line
            out.push_str(&format!("> {:>4} | {}\n", lno, lines[idx]));
            // Calculate underline for single-line spans; for multi-line, mark just the start col
            let col = if abs_col == 0 { 1 } else { abs_col } as usize;
            let underline_len = match (abs_end_line, abs_end_col) {
                (Some(el), Some(ec)) if el == line && ec > abs_col => (ec - abs_col) as usize,
                _ => 1,
            };
            let mut marker = String::new();
            for _ in 0..(col + 7) {
                marker.push(' ');
            } // 7 accounts for "> XXXX | "
            for _ in 0..underline_len {
                marker.push('^');
            }
            out.push_str(&format!("{marker}\n"));
        } else {
            out.push_str(&format!("  {:>4} | {}\n", lno, lines[idx]));
        }
    }
    out
}

#[derive(Debug, Clone)]
pub struct GeneratedModuleInfo {
    pub module_name: String,
    pub rel_path: PathBuf,
}

fn normalize_tag(tag: &str) -> String {
    tag.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

fn sanitize_suffix(s: &str) -> String {
    let mut out = String::new();
    let mut prev_underscore = false;
    for ch in s.chars() {
        let c = if ch.is_ascii_alphanumeric() { ch } else { '_' };
        if c == '_' {
            if !prev_underscore {
                out.push(c);
                prev_underscore = true;
            }
        } else {
            out.push(c);
            prev_underscore = false;
        }
    }
    if out.is_empty() { "_1".to_string() } else { out }
}

fn embeds_index_path_for_ast(ast_rel: &Path) -> PathBuf {
    let stem = ast_rel
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();
    ast_rel
        .parent()
        .unwrap_or_else(|| Path::new(""))
        .join(format!("{stem}.embeds.json"))
}

fn resolution_map_path_for_ast(ast_rel: &Path) -> PathBuf {
    let stem = ast_rel
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();
    ast_rel
        .parent()
        .unwrap_or_else(|| Path::new(""))
        .join(format!("{stem}.embeds.map.json"))
}

fn read_index(index_path_abs: &Path) -> Result<EmbedIndexFile> {
    let data = fs::read_to_string(index_path_abs)
        .with_context(|| format!("Failed reading embed index at {}", index_path_abs.display()))?;
    let idx: EmbedIndexFile = serde_json::from_str(&data)
        .with_context(|| format!("Failed parsing embed index JSON at {}", index_path_abs.display()))?;
    Ok(idx)
}

fn find_generator<'a>(cfg: &'a EmbedsConfig, tag: &str) -> Option<&'a EmbedGenerator> {
    cfg.generators.iter().find(|g| g.tags.iter().any(|t| t == tag))
}

fn run_generator(
    generator: &EmbedGenerator,
    package: &Package,
    input: &GeneratorInput,
) -> Result<GeneratorOutput> {
    let mut cmd = Command::new(&generator.cmd);
    cmd.args(&generator.args);
    let cwd = generator
        .cwd
        .as_ref()
        .map(|p| package.path.join(p))
        .unwrap_or_else(|| package.path.clone());
    cmd.current_dir(&cwd);
    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    if let Some(envs) = &generator.env {
        for (k, v) in envs {
            let val = if let Some(stripped) = v.strip_prefix("env:") {
                std::env::var(stripped).unwrap_or_default()
            } else {
                v.clone()
            };
            cmd.env(k, val);
        }
    }
    let mut child = cmd.spawn().with_context(|| {
        format!(
            "Failed to spawn generator '{}' (cmd: {}), cwd: {}",
            generator.id,
            generator.cmd,
            cwd.display()
        )
    })?;

    // Write input JSON
    if let Some(mut stdin) = child.stdin.take() {
        let json = serde_json::to_string(input)?;
        stdin
            .write_all(json.as_bytes())
            .context("Failed to write generator stdin")?;
    }

    // Timeout handling
    let timeout = Duration::from_millis(generator.timeout_ms.unwrap_or(10_000));
    let start = Instant::now();
    let output = loop {
        if let Some(_status) = child.try_wait().context("Failed to poll generator")? {
            // Child exited; collect stdout/stderr
            let out = child
                .wait_with_output()
                .context("Failed to read generator output")?;
            break out;
        }
        if start.elapsed() >= timeout {
            // Kill on timeout and report failure
            let _ = child.kill();
            return Err(anyhow!(
                "Generator '{}' timed out after {}ms",
                generator.id,
                timeout.as_millis()
            ));
        }
        std::thread::sleep(Duration::from_millis(10));
    };

    if !output.status.success() {
        return Err(anyhow!(
            "Generator '{}' failed with status {}",
            generator.id,
            output.status
        ));
    }
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let parsed: GeneratorOutput = serde_json::from_str(&stdout).with_context(|| {
        format!(
            "Generator '{}' returned invalid JSON output: {}",
            generator.id, stdout
        )
    })?;
    Ok(parsed)
}

#[allow(clippy::too_many_arguments)]
fn write_generated_file(
    out_dir_abs: &Path,
    file_name: &str,
    header_hash: &str,
    header_tag: &str,
    src_path: &str,
    idx: u32,
    suffix: &str,
    gen_id: &str,
    code: &str,
) -> Result<PathBuf> {
    fs::create_dir_all(out_dir_abs).with_context(|| format!("Failed to create {}", out_dir_abs.display()))?;
    let out_path = out_dir_abs.join(file_name);
    let mut f = fs::File::create(&out_path)
        .with_context(|| format!("Failed to create generated file {}", out_path.display()))?;
    // Fast header line + extended header
    writeln!(f, "// @sourceHash {header_hash}")?;
    writeln!(
        f,
        "/* rewatch-embed: v1; tag={header_tag}; src={src_path}; idx={idx}; suffix={suffix}; entry=default; hash={header_hash}; gen={gen_id} */",
    )?;
    f.write_all(code.as_bytes())?;
    Ok(out_path)
}

pub fn process_module_embeds(
    build_state: &mut BuildCommandState,
    package: Package,
    _module_rel: &Path,
    ast_rel_path: &Path,
) -> Result<Vec<GeneratedModuleInfo>> {
    let Some(effective) = package
        .config
        .get_effective_embeds_config(&build_state.project_context)
    else {
        // No embeds configured; still remove any stale generated files for this module
        cleanup_stale_generated_for_module(&package, ast_rel_path, &[])?;
        return Ok(vec![]);
    };

    let build_dir = package.get_build_path();
    let index_rel = embeds_index_path_for_ast(ast_rel_path);
    let index_abs = build_dir.join(&index_rel);
    if !index_abs.exists() {
        // No index for this module (no embeds found) — perform cleanup
        cleanup_stale_generated_for_module(&package, ast_rel_path, &[])?;
        return Ok(vec![]);
    }

    let index = read_index(&index_abs)?;
    if index.embeds.is_empty() {
        // No embeds present — perform cleanup
        cleanup_stale_generated_for_module(&package, ast_rel_path, &[])?;
        return Ok(vec![]);
    }

    // Prepare outDir
    let out_dir_abs = package.config.get_embeds_out_dir(&package.path);
    let mut res_entries: Vec<ResolutionMapEntry> = Vec::new();
    let mut generated: Vec<GeneratedModuleInfo> = Vec::new();
    let mut seen_suffix: AHashSet<(String, String)> = AHashSet::new(); // (tag, suffix)
    let mut count_generated = 0u32;
    let mut count_reused = 0u32;
    let mut count_failed = 0u32;

    log::debug!(
        "Embeds: module {} — discovered {} embed(s)",
        index.module,
        index.embeds.len()
    );

    // Build jobs for parallel execution
    struct OkGen {
        code: String,
        suffix: String,
        tag_norm: String,
        tag: String,
        occurrence_index: u32,
        literal_hash: String,
        generator_id: String,
    }
    enum JobResult {
        Reused {
            module_name: String,
            rel_path: PathBuf,
            entry: ResolutionMapEntry,
        },
        Ok(OkGen),
        Failed,
    }

    let jobs: Vec<(usize, &EmbedEntry)> = index.embeds.iter().enumerate().collect();
    let thread_cap = std::cmp::max(1, num_cpus::get() / 2);
    let pool = ThreadPoolBuilder::new()
        .num_threads(std::cmp::min(thread_cap, jobs.len()))
        .build()?;

    let job_results: Vec<JobResult> = pool.install(|| {
        jobs.par_iter()
            .map(|(_idx_pos, embed)| {
                let generator = match find_generator(effective, &embed.tag) {
                    Some(g) => g,
                    None => {
                        log::error!(
                            "EMBED_NO_GENERATOR: No generator configured for tag '{}' (module {})",
                            embed.tag,
                            index.module
                        );
                        return JobResult::Failed;
                    }
                };

                let tag_norm = normalize_tag(&embed.tag);
                log::debug!(
                    "Embeds: {} #{} '{}': start",
                    index.module,
                    embed.occurrence_index,
                    embed.tag
                );

                if let Some((existing_module_name, existing_rel_path)) =
                    find_cached_generated(&out_dir_abs, &index.module, &tag_norm, embed, generator, &package)
                {
                    log::debug!(
                        "Embeds: {} #{} '{}': cache hit -> {}",
                        index.module,
                        embed.occurrence_index,
                        embed.tag,
                        existing_module_name
                    );
                    return JobResult::Reused {
                        module_name: existing_module_name.clone(),
                        rel_path: existing_rel_path,
                        entry: ResolutionMapEntry {
                            tag: embed.tag.clone(),
                            occurrence_index: embed.occurrence_index,
                            literal_hash: embed.literal_hash.clone(),
                            target_module: existing_module_name,
                        },
                    };
                }

                log::debug!(
                    "Embeds: {} #{} '{}': cache miss — run '{}'",
                    index.module,
                    embed.occurrence_index,
                    embed.tag,
                    generator.id
                );

                let input = GeneratorInput {
                    version: 1,
                    tag: &embed.tag,
                    embed_string: &embed.embed_string,
                    source: GeneratorSource {
                        path: &index.source_path,
                        module: &index.module,
                    },
                    occurrence_index: embed.occurrence_index,
                    config: GeneratorConfig {
                        extra_sources: &generator.extra_sources,
                        options: None,
                    },
                };
                let output = match run_generator(generator, &package, &input) {
                    Ok(o) => o,
                    Err(e) => {
                        log::error!(
                            "EMBED_GENERATOR_FAILED: {}:{} -> {}",
                            index.source_path,
                            embed.occurrence_index,
                            e
                        );
                        // Also emit to compiler log for editor consumption
                        let file_abs = package.get_build_path().join(&index.source_path);
                        let mut msg = String::new();
                        msg.push_str("  Syntax error!\n");
                        msg.push_str(&format!(
                            "  {}:{}:{}\n",
                            file_abs.display(),
                            embed.range.start.line,
                            embed.range.start.column
                        ));
                        msg.push_str(&format!(
                            "  Generator '{}' failed to run: {}\n\n",
                            generator.id, e
                        ));
                        logs::append(&package, &msg);
                        return JobResult::Failed;
                    }
                };
                match output {
                    GeneratorOutput::Ok { code, suffix } => {
                        let mut suffix = suffix.unwrap_or_default();
                        if suffix.is_empty() {
                            suffix = format!("_{}", embed.occurrence_index);
                        }
                        JobResult::Ok(OkGen {
                            code,
                            suffix,
                            tag_norm,
                            tag: embed.tag.clone(),
                            occurrence_index: embed.occurrence_index,
                            literal_hash: embed.literal_hash.clone(),
                            generator_id: generator.id.clone(),
                        })
                    }
                    GeneratorOutput::Error { errors } => {
                        let build_dir = package.get_build_path();
                        let src_abs = build_dir.join(&index.source_path);
                        let diags: Vec<GenDiagItem> = match &errors {
                            serde_json::Value::Array(arr) => arr
                                .clone()
                                .into_iter()
                                .filter_map(|v| serde_json::from_value::<GenDiagItem>(v).ok())
                                .collect(),
                            _ => vec![],
                        };
                        if diags.is_empty() {
                            log::error!(
                                "EMBED_GENERATOR_FAILED: {}:{} -> {}",
                                index.source_path,
                                embed.occurrence_index,
                                errors
                            );
                            // Emit a generic compiler-log entry
                            let file_abs = package.get_build_path().join(&index.source_path);
                            let mut msg = String::new();
                            msg.push_str("  Syntax error!\n");
                            msg.push_str(&format!(
                                "  {}:{}:{}\n",
                                file_abs.display(),
                                embed.range.start.line,
                                embed.range.start.column
                            ));
                            msg.push_str(&format!("  Generator '{}' reported an error.\n\n", generator.id));
                            logs::append(&package, &msg);
                        } else {
                            for d in diags {
                                let (abs_line, abs_col, end_line, end_col) = match (&d.start, &d.end) {
                                    (Some(s), Some(e)) => {
                                        let (sl, sc) = map_embed_pos_to_abs(embed, s);
                                        let (el, ec) = map_embed_pos_to_abs(embed, e);
                                        (sl, sc, Some(el), Some(ec))
                                    }
                                    (Some(s), None) => {
                                        let (sl, sc) = map_embed_pos_to_abs(embed, s);
                                        (sl, sc, None, None)
                                    }
                                    _ => (embed.range.start.line, embed.range.start.column, None, None),
                                };
                                let frame =
                                    render_code_frame(&src_abs, abs_line, abs_col, end_line, end_col, 1);
                                let code_sfx = d.code.as_deref().unwrap_or("");
                                let sev = d.severity.as_deref().unwrap_or("error");
                                if code_sfx.is_empty() {
                                    log::error!(
                                        "EMBED_GENERATOR_FAILED ({sev}) at {}:{}:{}\n{}\n{}",
                                        index.source_path,
                                        abs_line,
                                        abs_col,
                                        d.message,
                                        frame
                                    );
                                } else {
                                    log::error!(
                                        "EMBED_GENERATOR_FAILED[{code}] ({sev}) at {}:{}:{}\n{}\n{}",
                                        index.source_path,
                                        abs_line,
                                        abs_col,
                                        d.message,
                                        frame,
                                        code = code_sfx
                                    );
                                }

                                // Emit editor-friendly diagnostics in .compiler.log
                                let mut out = String::new();
                                match sev {
                                    "warning" => out.push_str("  Warning number 999\n"),
                                    _ => out.push_str("  Syntax error!\n"),
                                }
                                let file_abs = package.get_build_path().join(&index.source_path);
                                // Range line: file:line:col[-end] or file:line:col-endCol (same line)
                                let range_suffix = match (end_line, end_col) {
                                    (Some(el), Some(ec)) if el != abs_line => format!("-{}:{}", el, ec),
                                    (Some(_), Some(ec)) => format!("-{}", ec),
                                    _ => String::new(),
                                };
                                out.push_str(&format!(
                                    "  {}:{}:{}{}\n",
                                    file_abs.display(),
                                    abs_line,
                                    abs_col,
                                    range_suffix
                                ));
                                // Message lines
                                for line in d.message.lines() {
                                    out.push_str("  ");
                                    out.push_str(line);
                                    out.push('\n');
                                }
                                if !frame.is_empty() {
                                    for line in frame.lines() {
                                        out.push_str("  ");
                                        out.push_str(line);
                                        out.push('\n');
                                    }
                                }
                                out.push('\n');
                                logs::append(&package, &out);
                            }
                        }
                        JobResult::Failed
                    }
                }
            })
            .collect()
    });

    // Merge results in stable order (original discovery order)
    let mut ordered: Vec<(usize, JobResult)> = jobs.into_iter().map(|(i, _)| i).zip(job_results).collect();
    ordered.sort_by_key(|(i, _)| *i);

    for (_i, jr) in ordered.into_iter() {
        match jr {
            JobResult::Reused {
                module_name,
                rel_path,
                entry,
            } => {
                res_entries.push(entry);
                generated.push(GeneratedModuleInfo {
                    module_name,
                    rel_path,
                });
                count_reused += 1;
            }
            JobResult::Ok(ok) => {
                let suffix = sanitize_suffix(&ok.suffix);
                let key = (ok.tag.clone(), suffix.clone());
                if seen_suffix.contains(&key) {
                    log::error!(
                        "EMBED_SUFFIX_COLLISION: duplicate suffix '{}' for tag '{}' in module {}",
                        suffix,
                        ok.tag,
                        index.module
                    );
                    count_failed += 1;
                    continue;
                }
                seen_suffix.insert(key);

                let gen_file_name = format!("{}__embed_{}_{}.res", index.module, ok.tag_norm, suffix);
                let out_path_abs = write_generated_file(
                    &out_dir_abs,
                    &gen_file_name,
                    &ok.literal_hash,
                    &ok.tag,
                    &index.source_path,
                    ok.occurrence_index,
                    &suffix,
                    // generator id omitted here (unknown); use a placeholder for header
                    // but better carry it - adjust above to include; for now leave blank
                    &ok.generator_id,
                    &ok.code,
                )?;
                let rel_path = out_path_abs
                    .strip_prefix(&package.path)
                    .unwrap_or(&out_path_abs)
                    .to_path_buf();
                let module_name = Path::new(&gen_file_name)
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .to_string();
                res_entries.push(ResolutionMapEntry {
                    tag: ok.tag.clone(),
                    occurrence_index: ok.occurrence_index,
                    literal_hash: ok.literal_hash.clone(),
                    target_module: module_name.clone(),
                });
                generated.push(GeneratedModuleInfo {
                    module_name,
                    rel_path,
                });
                count_generated += 1;
            }
            JobResult::Failed => {
                count_failed += 1;
            }
        }
    }

    // Always write resolution map and attempt rewrite, even if entries are empty.
    // This ensures missing mappings surface as EMBED_MAP_MISMATCH instead of a generic
    // "Uninterpreted extension" later in the pipeline.
    let map_rel = resolution_map_path_for_ast(ast_rel_path);
    let map_abs = build_dir.join(&map_rel);
    if let Some(parent) = map_abs.parent() {
        let _ = fs::create_dir_all(parent);
    }
    let map = ResolutionMap {
        version: 1,
        module: index.module.clone(),
        entries: res_entries,
    };
    let data = serde_json::to_string(&map)?;
    fs::write(&map_abs, data)?;
    log::debug!(
        "Embeds: module {} — generated {}, reused {}, failed {}; rewriting {} entry(ies)",
        index.module,
        count_generated,
        count_reused,
        count_failed,
        map.entries.len()
    );

    // Run rewrite: bsc -rewrite-embeds -ast <ast> -map <map> -o <ast>
    let bsc = &build_state.compiler_info.bsc_path;
    let args = vec![
        "-rewrite-embeds".to_string(),
        "-ast".to_string(),
        ast_rel_path.to_string_lossy().to_string(),
        "-map".to_string(),
        map_rel.to_string_lossy().to_string(),
        "-o".to_string(),
        ast_rel_path.to_string_lossy().to_string(),
    ];
    let output = Command::new(bsc)
        .current_dir(&build_dir)
        .args(&args)
        .output()
        .with_context(|| format!("Failed to run bsc -rewrite-embeds for {}", ast_rel_path.display()))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        log::error!("rewrite-embeds failed: {stderr}");
        // Surface to compiler log so the editor can pick it up
        logs::append(&package, &stderr);
        // Surface as an error to stop pipeline early; avoids later generic errors.
        return Err(anyhow!("rewrite-embeds failed"));
    }

    // Mark original module for recompilation so rewrite takes effect
    if let Some(orig) = build_state.build_state.modules.get_mut(&index.module) {
        orig.compile_dirty = true;
        orig.deps_dirty = true;
    }

    // Cleanup: remove any stale generated files for this module that weren't produced this run
    cleanup_stale_generated_for_module(&package, ast_rel_path, &generated)?;

    Ok(generated)
}

pub fn count_planned_invocations(
    build_state: &BuildCommandState,
    package: &Package,
    ast_rel_path: &Path,
) -> Result<(u32, u32)> {
    let Some(effective) = package
        .config
        .get_effective_embeds_config(&build_state.project_context)
    else {
        return Ok((0, 0));
    };

    let build_dir = package.get_build_path();
    let index_rel = embeds_index_path_for_ast(ast_rel_path);
    let index_abs = build_dir.join(&index_rel);
    if !index_abs.exists() {
        return Ok((0, 0));
    }
    let index = read_index(&index_abs)?;
    if index.embeds.is_empty() {
        return Ok((0, 0));
    }

    let out_dir_abs = package.config.get_embeds_out_dir(&package.path);
    let mut reused = 0u32;
    let mut invocations = 0u32;
    for embed in &index.embeds {
        let Some(generator) = find_generator(effective, &embed.tag) else {
            continue;
        };
        let tag_norm = normalize_tag(&embed.tag);
        if let Some(_hit) =
            find_cached_generated(&out_dir_abs, &index.module, &tag_norm, embed, generator, package)
        {
            reused += 1;
        } else {
            invocations += 1;
        }
    }
    Ok((invocations, reused))
}

fn read_first_line(path: &Path) -> Option<String> {
    use std::io::{BufRead, BufReader};
    let f = fs::File::open(path).ok()?;
    let mut reader = BufReader::new(f);
    let mut line = String::new();
    let _ = reader.read_line(&mut line).ok()?;
    Some(line)
}

fn header_hash_from_file(path: &Path) -> Option<String> {
    let line = read_first_line(path)?;
    let prefix = "// @sourceHash ";
    if line.starts_with(prefix) {
        Some(line.trim()[prefix.len()..].to_string())
    } else {
        None
    }
}

fn find_cached_generated(
    out_dir_abs: &Path,
    module_name: &str,
    tag_norm: &str,
    embed: &EmbedEntry,
    generator: &EmbedGenerator,
    package: &Package,
) -> Option<(String, PathBuf)> {
    let prefix = format!("{module_name}__embed_{tag_norm}_");
    let dir_iter = fs::read_dir(out_dir_abs).ok()?;
    for entry in dir_iter.flatten() {
        let p = entry.path();
        if !p.is_file() {
            continue;
        }
        if p.extension().and_then(|s| s.to_str()) != Some("res") {
            continue;
        }
        let fname = p.file_name()?.to_string_lossy().to_string();
        if !fname.starts_with(&prefix) {
            continue;
        }
        // Quick hash check
        if let Some(h) = header_hash_from_file(&p) {
            if h != embed.literal_hash {
                continue;
            }
            // Extra sources mtime check
            let file_mtime = p.metadata().and_then(|m| m.modified()).ok()?;
            let extra_newer = generator.extra_sources.iter().any(|rel| {
                let ap = package.path.join(rel);
                ap.metadata()
                    .and_then(|m| m.modified())
                    .map(|t| t > file_mtime)
                    .unwrap_or(false)
            });
            if extra_newer {
                continue;
            }
            let module = p.file_stem()?.to_string_lossy().to_string();
            // Return rel path to package root
            let rel = p.strip_prefix(&package.path).unwrap_or(&p).to_path_buf();
            return Some((module, rel));
        }
    }
    None
}

fn cleanup_stale_generated_for_module(
    package: &Package,
    ast_rel_path: &Path,
    generated: &[GeneratedModuleInfo],
) -> Result<()> {
    let out_dir_abs = package.config.get_embeds_out_dir(&package.path);
    let module_name = ast_rel_path
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();
    let prefix = format!("{module_name}__embed_");
    let keep_stems: AHashSet<String> = generated.iter().map(|g| g.module_name.clone()).collect();
    if let Ok(entries) = fs::read_dir(&out_dir_abs) {
        for entry in entries.flatten() {
            let p = entry.path();
            if !p.is_file() {
                continue;
            }
            let fname = p.file_name().and_then(|s| s.to_str()).unwrap_or("");
            let stem = p.file_stem().and_then(|s| s.to_str()).unwrap_or("");
            if fname.starts_with(&prefix) && !keep_stems.contains(stem) {
                let _ = fs::remove_file(&p);
                log::debug!("Embeds: removed stale generated file {}", p.display());
            }
        }
    }
    Ok(())
}

pub fn add_generated_modules_to_state(
    state: &mut BuildCommandState,
    package: Package,
    generated: &[GeneratedModuleInfo],
) {
    for g in generated {
        let path = g.rel_path.clone();
        let abs = package.path.join(&path);
        let modified = abs
            .metadata()
            .and_then(|m| m.modified())
            .unwrap_or(SystemTime::now());
        let is_type_dev = package.is_source_file_type_dev(&path);
        let module = Module {
            source_type: SourceType::SourceFile(super::build_types::SourceFile {
                implementation: Implementation {
                    path: path.clone(),
                    parse_state: super::build_types::ParseState::Pending,
                    compile_state: super::build_types::CompileState::Pending,
                    last_modified: modified,
                    parse_dirty: true,
                },
                interface: None::<Interface>,
            }),
            deps: AHashSet::new(),
            dependents: AHashSet::new(),
            package_name: package.name.clone(),
            compile_dirty: true,
            last_compiled_cmi: None,
            last_compiled_cmt: None,
            deps_dirty: true,
            is_type_dev,
        };
        state.insert_module(&g.module_name, module);
    }
}
