use super::build_types::{BuildCommandState, SourceType, Implementation, Interface, Module};
use super::packages::Package;
use crate::config::{EmbedGenerator, EmbedsConfig};
use crate::helpers;
use ahash::{AHashMap, AHashSet};
use anyhow::{anyhow, Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::SystemTime;

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
    Ok { code: String, #[serde(default)] suffix: Option<String> },
    #[serde(rename_all = "camelCase")]
    Error { errors: serde_json::Value },
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
        .join(format!("{}.embeds.json", stem))
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
        .join(format!("{}.embeds.map.json", stem))
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

    let output = child
        .wait_with_output()
        .context("Failed to read generator output")?;

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
    fs::create_dir_all(out_dir_abs)
        .with_context(|| format!("Failed to create {}", out_dir_abs.display()))?;
    let out_path = out_dir_abs.join(file_name);
    let mut f = fs::File::create(&out_path)
        .with_context(|| format!("Failed to create generated file {}", out_path.display()))?;
    // Fast header line + extended header
    writeln!(f, "// @sourceHash {}", header_hash)?;
    writeln!(
        f,
        "/* rewatch-embed: v1; tag={}; src={}; idx={}; suffix={}; entry=default; hash={}; gen={} */",
        header_tag, src_path, idx, suffix, header_hash, gen_id
    )?;
    f.write_all(code.as_bytes())?;
    Ok(out_path)
}

pub fn process_module_embeds(
    build_state: &mut BuildCommandState,
    package: Package,
    module_rel: &Path,
    ast_rel_path: &Path,
) -> Result<Vec<GeneratedModuleInfo>> {
    let Some(effective) = package
        .config
        .get_effective_embeds_config(&build_state.project_context)
        else { return Ok(vec![]) };

    let build_dir = package.get_build_path();
    let index_rel = embeds_index_path_for_ast(ast_rel_path);
    let index_abs = build_dir.join(&index_rel);
    if !index_abs.exists() {
        return Ok(vec![]);
    }

    let index = read_index(&index_abs)?;
    if index.embeds.is_empty() {
        return Ok(vec![]);
    }

    // Prepare outDir
    let out_dir_abs = package.config.get_embeds_out_dir(&package.path);
    let mut res_entries: Vec<ResolutionMapEntry> = Vec::new();
    let mut generated: Vec<GeneratedModuleInfo> = Vec::new();
    let mut seen_suffix: AHashSet<(String, String)> = AHashSet::new(); // (tag, suffix)

    for embed in &index.embeds {
        let Some(generator) = find_generator(effective, &embed.tag) else {
            // Unknown tag: skip with warning
            log::error!(
                "EMBED_NO_GENERATOR: No generator configured for tag '{}' (module {})",
                embed.tag, index.module
            );
            continue;
        };

        let input = GeneratorInput {
            version: 1,
            tag: &embed.tag,
            embed_string: &embed.embed_string,
            source: GeneratorSource {
                path: &index.source_path,
                module: &index.module,
            },
            occurrence_index: embed.occurrence_index,
            config: GeneratorConfig { extra_sources: &generator.extra_sources, options: None },
        };

        let output = run_generator(generator, &package, &input)?;
        let (code, mut suffix) = match output {
            GeneratorOutput::Ok { code, suffix } => (code, suffix.unwrap_or_default()),
            GeneratorOutput::Error { errors } => {
                // Print generator error details
                log::error!(
                    "EMBED_GENERATOR_FAILED: Generator '{}' reported errors for {}:{} => {}",
                    generator.id,
                    index.source_path,
                    embed.occurrence_index,
                    errors
                );
                continue;
            }
        };
        if suffix.is_empty() {
            suffix = format!("_{}", embed.occurrence_index);
        }
        let suffix = sanitize_suffix(&suffix);
        let tag_norm = normalize_tag(&embed.tag);
        // Collision per (tag, suffix) within file
        let key = (embed.tag.clone(), suffix.clone());
        if seen_suffix.contains(&key) {
            log::error!(
                "EMBED_SUFFIX_COLLISION: duplicate suffix '{}' for tag '{}' in module {}",
                suffix, embed.tag, index.module
            );
            continue;
        }
        seen_suffix.insert(key);

        let gen_file_name = format!(
            "{}__embed_{}_{}.res",
            index.module, tag_norm, suffix
        );
        let out_path_abs = write_generated_file(
            &out_dir_abs,
            &gen_file_name,
            &embed.literal_hash,
            &embed.tag,
            &index.source_path,
            embed.occurrence_index,
            &suffix,
            &generator.id,
            &code,
        )?;

        // Compute rel path to package root
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
            tag: embed.tag.clone(),
            occurrence_index: embed.occurrence_index,
            literal_hash: embed.literal_hash.clone(),
            target_module: module_name.clone(),
        });
        generated.push(GeneratedModuleInfo { module_name, rel_path });
    }

    // Write resolution map next to AST
    if !res_entries.is_empty() {
        let map_rel = resolution_map_path_for_ast(ast_rel_path);
        let map_abs = build_dir.join(&map_rel);
        if let Some(parent) = map_abs.parent() { let _ = fs::create_dir_all(parent); }
        let map = ResolutionMap { version: 1, module: index.module.clone(), entries: res_entries };
        let data = serde_json::to_string(&map)?;
        fs::write(&map_abs, data)?;

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
            log::error!("rewrite-embeds failed: {}", stderr);
        }
    }

    Ok(generated)
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
