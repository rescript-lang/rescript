use crate::build::packages;
use std::ffi::OsString;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::io::{self, BufRead};
use std::path::{Component, Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

pub type StdErr = String;

pub mod deserialize {
    pub fn default_false() -> bool {
        false
    }

    pub fn default_true() -> bool {
        true
    }
}

pub mod emojis {
    use console::Emoji;
    pub static COMMAND: Emoji<'_, '_> = Emoji("🏃 ", "");
    pub static TREE: Emoji<'_, '_> = Emoji("📦 ", "");
    pub static SWEEP: Emoji<'_, '_> = Emoji("🧹 ", "");
    pub static LOOKING_GLASS: Emoji<'_, '_> = Emoji("👀 ", "");
    pub static CODE: Emoji<'_, '_> = Emoji("🧱 ", "");
    pub static SWORDS: Emoji<'_, '_> = Emoji("🤺 ", "");
    pub static DEPS: Emoji<'_, '_> = Emoji("🌴 ", "");
    pub static CHECKMARK: Emoji<'_, '_> = Emoji("✅ ", "");
    pub static CROSS: Emoji<'_, '_> = Emoji("❌ ", "");
    pub static SPARKLES: Emoji<'_, '_> = Emoji("✨ ", "");
    pub static COMPILE_STATE: Emoji<'_, '_> = Emoji("📝 ", "");
    pub static LINE_CLEAR: &str = "\x1b[2K\r";
}

/// This trait is used to strip the verbatim prefix from a Windows path.
/// On non-Windows systems, it simply returns the original path.
/// This is needed until the rescript compiler can handle such paths.
pub trait StrippedVerbatimPath {
    fn to_stripped_verbatim_path(self) -> PathBuf;
}

impl StrippedVerbatimPath for PathBuf {
    fn to_stripped_verbatim_path(self) -> PathBuf {
        if cfg!(not(target_os = "windows")) {
            return self;
        }

        let mut stripped = PathBuf::new();
        for component in self.components() {
            match component {
                Component::Prefix(prefix_component) => {
                    if prefix_component.kind().is_verbatim() {
                        stripped.push(
                            prefix_component
                                .as_os_str()
                                .to_string_lossy()
                                .strip_prefix("\\\\?\\")
                                .unwrap(),
                        );
                    } else {
                        stripped.push(prefix_component.as_os_str());
                    }
                }
                Component::RootDir => {
                    stripped.push(Component::RootDir);
                }
                Component::CurDir => {
                    stripped.push(Component::CurDir);
                }
                Component::ParentDir => {
                    stripped.push(Component::ParentDir);
                }
                Component::Normal(os_str) => {
                    stripped.push(Component::Normal(os_str));
                }
            }
        }
        stripped
    }
}

pub trait LexicalAbsolute {
    fn to_lexical_absolute(&self) -> std::io::Result<PathBuf>;
}

impl LexicalAbsolute for Path {
    fn to_lexical_absolute(&self) -> std::io::Result<PathBuf> {
        let mut absolute = if self.is_absolute() {
            PathBuf::new()
        } else {
            std::env::current_dir()?
        };
        for component in self.components() {
            match component {
                Component::CurDir => {}
                Component::ParentDir => {
                    absolute.pop();
                }
                component => absolute.push(component.as_os_str()),
            }
        }
        Ok(absolute)
    }
}

pub fn package_path(root: &Path, package_name: &str) -> PathBuf {
    root.join("node_modules").join(package_name)
}

pub fn get_abs_path(path: &Path) -> PathBuf {
    let abs_path_buf = PathBuf::from(path);

    return abs_path_buf
        .to_lexical_absolute()
        .expect("Could not canonicalize");
}

pub fn get_basename(path: &Path) -> String {
    return path
        .file_stem()
        .expect("Could not get basename")
        .to_str()
        .expect("Could not get basename 2")
        .to_string();
}

/// Capitalizes the first character in s.
fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn add_suffix(base: &str, namespace: &packages::Namespace) -> String {
    match namespace {
        packages::Namespace::NamespaceWithEntry { namespace: _, entry } if entry == base => base.to_string(),
        packages::Namespace::Namespace(_)
        | packages::Namespace::NamespaceWithEntry {
            namespace: _,
            entry: _,
        } => base.to_string() + "-" + &namespace.to_suffix().unwrap(),
        packages::Namespace::NoNamespace => base.to_string(),
    }
}

pub fn module_name_with_namespace(module_name: &str, namespace: &packages::Namespace) -> String {
    capitalize(&add_suffix(module_name, namespace))
}

// this doesn't capitalize the module name! if the rescript name of the file is "foo.res" the
// compiler assets are foo-Namespace.cmt and foo-Namespace.cmj, but the module name is Foo
pub fn file_path_to_compiler_asset_basename(path: &Path, namespace: &packages::Namespace) -> String {
    let base = get_basename(path);
    add_suffix(&base, namespace)
}

pub fn file_path_to_module_name(path: &Path, namespace: &packages::Namespace) -> String {
    capitalize(&file_path_to_compiler_asset_basename(path, namespace))
}

pub fn contains_ascii_characters(str: &str) -> bool {
    for chr in str.chars() {
        if chr.is_ascii_alphanumeric() {
            return true;
        }
    }
    false
}

pub fn create_path(path: &Path) {
    fs::DirBuilder::new().recursive(true).create(path).unwrap();
}

pub fn create_path_for_path(path: &Path) {
    fs::DirBuilder::new().recursive(true).create(path).unwrap();
}

pub fn get_bsc(root_path: &Path, workspace_root: &Option<PathBuf>) -> PathBuf {
    let subfolder = match (std::env::consts::OS, std::env::consts::ARCH) {
        ("macos", "aarch64") => "darwin-arm64",
        ("macos", _) => "darwin-x64",
        ("linux", "aarch64") => "linux-arm64",
        ("linux", _) => "linux-x64",
        ("windows", "aarch64") => "win-arm64",
        ("windows", _) => "win-x64",
        _ => panic!("Unsupported architecture"),
    };

    match (
        root_path
            .join("node_modules")
            .join("@rescript")
            .join(subfolder)
            .join("bin")
            .join("bsc.exe")
            .canonicalize()
            .map(StrippedVerbatimPath::to_stripped_verbatim_path),
        workspace_root.as_ref().map(|workspace_root| {
            workspace_root
                .join("node_modules")
                .join("@rescript")
                .join(subfolder)
                .join("bin")
                .join("bsc.exe")
                .canonicalize()
                .map(StrippedVerbatimPath::to_stripped_verbatim_path)
        }),
    ) {
        (Ok(path), _) => path,
        (_, Some(Ok(path))) => path,
        _ => panic!("Could not find bsc.exe"),
    }
}

pub fn string_ends_with_any(s: &Path, suffixes: &[&str]) -> bool {
    suffixes
        .iter()
        .any(|&suffix| s.extension().unwrap_or(&OsString::new()).to_str().unwrap_or("") == suffix)
}

fn path_to_ast_extension(path: &Path) -> &str {
    let extension = path.extension().unwrap().to_str().unwrap();
    if extension.ends_with("i") {
        ".iast"
    } else {
        ".ast"
    }
}

pub fn get_ast_path(source_file: &Path) -> PathBuf {
    let source_path = source_file;
    let basename = file_path_to_compiler_asset_basename(source_file, &packages::Namespace::NoNamespace);
    let extension = path_to_ast_extension(source_path);

    source_path
        .parent()
        .unwrap()
        .join(format!("{}{}", basename, extension))
}

pub fn get_compiler_asset(
    package: &packages::Package,
    namespace: &packages::Namespace,
    source_file: &Path,
    extension: &str,
) -> PathBuf {
    let namespace = match extension {
        "ast" | "iast" => &packages::Namespace::NoNamespace,
        _ => namespace,
    };
    let basename = file_path_to_compiler_asset_basename(source_file, namespace);
    package
        .get_ocaml_build_path()
        .join(format!("{}.{}", basename, extension))
}

pub fn canonicalize_string_path(path: &str) -> Option<PathBuf> {
    return Path::new(path)
        .canonicalize()
        .map(StrippedVerbatimPath::to_stripped_verbatim_path)
        .ok();
}

pub fn get_bs_compiler_asset(
    package: &packages::Package,
    namespace: &packages::Namespace,
    source_file: &Path,
    extension: &str,
) -> String {
    let namespace = match extension {
        "ast" | "iast" => &packages::Namespace::NoNamespace,
        _ => namespace,
    };

    let dir = source_file.parent().unwrap();
    let basename = file_path_to_compiler_asset_basename(source_file, namespace);

    package
        .get_build_path()
        .join(dir)
        .join(format!("{}{}", basename, extension))
        .to_str()
        .unwrap()
        .to_owned()
}

pub fn get_namespace_from_module_name(module_name: &str) -> Option<String> {
    let mut split = module_name.split('-');
    let _ = split.next();
    split.next().map(|s| s.to_string())
}

pub fn is_interface_ast_file(file: &Path) -> bool {
    file.extension()
        .map(|extension| extension.eq_ignore_ascii_case("iast"))
        .unwrap_or(false)
}

pub fn read_lines(filename: &Path) -> io::Result<io::Lines<io::BufReader<fs::File>>> {
    let file = fs::File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

pub fn get_system_time() -> u128 {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH).expect("Time went backwards");
    since_the_epoch.as_millis()
}

pub fn is_interface_file(extension: &str) -> bool {
    matches!(extension, "resi" | "mli" | "rei")
}

pub fn is_implementation_file(extension: &str) -> bool {
    matches!(extension, "res" | "ml" | "re")
}

pub fn is_source_file(extension: &str) -> bool {
    is_interface_file(extension) || is_implementation_file(extension)
}

pub fn is_non_exotic_module_name(module_name: &str) -> bool {
    let mut chars = module_name.chars();
    if chars.next().unwrap().is_ascii_uppercase() && chars.all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return true;
    }
    false
}

pub fn get_extension(path: &Path) -> String {
    return path
        .extension()
        .expect("Could not get extension")
        .to_str()
        .expect("Could not get extension 2")
        .to_string();
}

pub fn format_namespaced_module_name(module_name: &str) -> String {
    // from ModuleName-Namespace to Namespace.ModuleName
    // also format ModuleName-@Namespace to Namespace.ModuleName
    let mut split = module_name.split('-');
    let module_name = split.next().unwrap();
    let namespace = split.next();
    let namespace = namespace.map(|ns| ns.trim_start_matches('@'));
    match namespace {
        None => module_name.to_string(),
        Some(ns) => ns.to_string() + "." + module_name,
    }
}

pub fn compute_file_hash(path: &Path) -> Option<blake3::Hash> {
    match fs::read(path) {
        Ok(str) => Some(blake3::hash(&str)),
        Err(_) => None,
    }
}

fn has_rescript_config(path: &Path) -> bool {
    path.join("bsconfig.json").exists() || path.join("rescript.json").exists()
}

pub fn get_workspace_root(package_root: &Path) -> Option<PathBuf> {
    std::path::PathBuf::from(&package_root)
        .parent()
        .and_then(get_nearest_config)
}

// traverse up the directory tree until we find a config.json, if not return None
pub fn get_nearest_config(path_buf: &Path) -> Option<PathBuf> {
    let mut current_dir = path_buf.to_owned();
    loop {
        if has_rescript_config(&current_dir) {
            return Some(current_dir);
        }
        match current_dir.parent() {
            None => return None,
            Some(parent) => current_dir = parent.to_path_buf(),
        }
    }
}

pub fn get_rescript_version(bsc_path: &Path) -> String {
    let version_cmd = Command::new(bsc_path)
        .args(["-v"])
        .output()
        .expect("failed to find version");

    std::str::from_utf8(&version_cmd.stdout)
        .expect("Could not read version from rescript")
        .replace('\n', "")
        .replace("ReScript ", "")
}

pub fn read_file(path: &Path) -> Result<String, std::io::Error> {
    let mut file = File::open(path).expect("file not found");
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

pub fn get_source_file_from_rescript_file(path: &Path, suffix: &str) -> PathBuf {
    path.with_extension(
        // suffix.to_string includes the ., so we need to remove it
        &suffix.to_string()[1..],
    )
}
