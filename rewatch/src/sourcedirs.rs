use crate::build::build_types::BuildState;
use crate::build::packages::Package;
use ahash::{AHashMap, AHashSet};
use rayon::prelude::*;
use serde::Serialize;
use serde_json::json;
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

type Dir = PathBuf;
type PackageName = String;
type AbsolutePath = PathBuf;
type Pkg = (PackageName, AbsolutePath);

/// `reanalyze` consumes `.sourcedirs.json` to find `.cmt/.cmti` files.
///
/// Historically, this file contained a single `"dirs"` list which was interpreted as being
/// under the *root* `lib/bs/`. That doesn't hold for `rewatch` monorepos, where each package has
/// its own `lib/bs/`.
///
/// To avoid reanalyze-side "package resolution", v2 includes an explicit `cmt_scan` plan:
/// a list of build roots (`.../lib/bs`) and the subdirectories within those roots to scan.
#[derive(Serialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct CmtScanEntry {
    /// Path to a `lib/bs` directory, relative to the workspace root.
    pub build_root: PathBuf,
    /// Subdirectories (relative to `build_root`) to scan for `.cmt/.cmti`.
    pub scan_dirs: Vec<PathBuf>,
    /// Whether to also scan `build_root` itself for `.cmt/.cmti` (namespaces/mlmap often land here).
    pub also_scan_build_root: bool,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceDirs {
    pub version: u8,
    pub dirs: Vec<Dir>,
    pub pkgs: Vec<Pkg>,
    pub generated: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cmt_scan: Option<Vec<CmtScanEntry>>,
}

fn package_to_dirs(package: &Package, root_package_path: &Path) -> AHashSet<Dir> {
    match package.path.strip_prefix(root_package_path) {
        Err(_) => AHashSet::new(),
        Ok(relative_path) => package
            .sources
            .as_ref()
            .map(|s| &s.dirs)
            .unwrap_or(&AHashSet::new())
            .iter()
            .map(|path| relative_path.join(path))
            .collect::<AHashSet<PathBuf>>(),
    }
}

fn deps_to_pkgs<'a>(
    packages: &'a AHashMap<String, Package>,
    dependencies: &'a Option<Vec<String>>,
) -> AHashSet<Pkg> {
    dependencies
        .as_ref()
        .unwrap_or(&vec![])
        .iter()
        .filter_map(|name| {
            packages
                .get(name)
                .map(|package| (name.to_owned(), package.path.to_owned()))
        })
        .collect::<AHashSet<Pkg>>()
}

fn write_sourcedirs_files(path: &Path, source_dirs: &SourceDirs) -> Result<usize, std::io::Error> {
    let mut source_dirs_json = File::create(path.join(".sourcedirs.json"))?;
    source_dirs_json.write(json!(source_dirs).to_string().as_bytes())
}

fn sort_paths(mut v: Vec<PathBuf>) -> Vec<PathBuf> {
    v.sort_by(|a, b| a.to_string_lossy().cmp(&b.to_string_lossy()));
    v
}

pub fn print(buildstate: &BuildState) {
    // Find Root Package
    let (_name, root_package) = buildstate
        .packages
        .iter()
        .find(|(_name, package)| package.is_root)
        .expect("Could not find root package");

    // We only support a single `.sourcedirs.json` at the workspace root.
    // Remove any stale per-package `.sourcedirs.json` from older builds to avoid confusion.
    buildstate
        .packages
        .iter()
        .filter(|(_name, package)| !package.is_root)
        .for_each(|(_name, package)| {
            let path = package.get_build_path().join(".sourcedirs.json");
            let _ = std::fs::remove_file(&path);
        });

    // Take all local packages with source files.
    // In the case of a monorepo, the root package typically won't have any source files.
    // But in the case of a single package, it will be both local, root and have source files.
    let collected: Vec<(AHashSet<Dir>, AHashMap<PackageName, AbsolutePath>, CmtScanEntry)> = buildstate
        .packages
        .par_iter()
        .filter(|(_name, package)| package.is_local && package.sources.is_some())
        .map(|(_name, package)| {
            // Extract Directories
            let dirs = package_to_dirs(package, &root_package.path);

            // Extract Pkgs
            let pkgs = [&package.config.dependencies, &package.config.dev_dependencies]
                .into_iter()
                .map(|dependencies| deps_to_pkgs(&buildstate.packages, dependencies));

            // Build scan plan entry for the root `.sourcedirs.json`.
            // `build_root` is `<pkg_rel_to_root>/lib/bs`.
            let pkg_rel = package
                .path
                .strip_prefix(&root_package.path)
                .unwrap_or(Path::new(""))
                .to_path_buf();
            let build_root = pkg_rel.join("lib").join("bs");
            let scan_dirs = sort_paths(
                package
                    .sources
                    .as_ref()
                    .map(|s| &s.dirs)
                    .unwrap_or(&AHashSet::new())
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>(),
            );

            // NOTE: We intentionally do NOT write per-package `.sourcedirs.json`.
            // The root package's `.sourcedirs.json` contains a complete `cmt_scan` plan
            // and is the only file `reanalyze` needs for root-level monorepo analysis.

            (
                dirs,
                pkgs.flatten().collect::<AHashMap<PackageName, AbsolutePath>>(),
                CmtScanEntry {
                    build_root,
                    scan_dirs,
                    // Namespaces/mlmap artifacts can land in `lib/bs` itself; scanning it is cheap and avoids misses.
                    also_scan_build_root: true,
                },
            )
        })
        .collect();

    let mut dirs: Vec<AHashSet<Dir>> = Vec::with_capacity(collected.len());
    let mut pkgs: Vec<AHashMap<PackageName, AbsolutePath>> = Vec::with_capacity(collected.len());
    let mut cmt_scan_entries: Vec<CmtScanEntry> = Vec::with_capacity(collected.len());
    for (d, p, s) in collected {
        dirs.push(d);
        pkgs.push(p);
        cmt_scan_entries.push(s);
    }

    let mut merged_dirs: AHashSet<Dir> = AHashSet::new();
    let mut merged_pkgs: AHashMap<PackageName, AbsolutePath> = AHashMap::new();

    dirs.into_iter().for_each(|dir_set| merged_dirs.extend(dir_set));
    pkgs.into_iter().for_each(|pkg_set| merged_pkgs.extend(pkg_set));

    // Root `.sourcedirs.json`: merged view + explicit scan plan for reanalyze.
    let root_sourcedirs = SourceDirs {
        version: 2,
        dirs: sort_paths(merged_dirs.into_iter().collect::<Vec<Dir>>()),
        pkgs: {
            let mut v = merged_pkgs.into_iter().collect::<Vec<Pkg>>();
            v.sort_by(|(a, _), (b, _)| a.cmp(b));
            v
        },
        generated: vec![],
        cmt_scan: Some({
            // Ensure deterministic order (use the serialized string form).
            let mut v = cmt_scan_entries;
            v.sort_by(|a, b| {
                a.build_root
                    .to_string_lossy()
                    .cmp(&b.build_root.to_string_lossy())
            });
            v
        }),
    };
    write_sourcedirs_files(&root_package.get_build_path(), &root_sourcedirs)
        .expect("Could not write sourcedirs.json");
}
