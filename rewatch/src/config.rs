use crate::build::packages;
use crate::helpers;
use crate::helpers::deserialize::*;
use crate::project_context::ProjectContext;
use anyhow::{Result, anyhow};
use convert_case::{Case, Casing};
use serde::de::{Error as DeError, Visitor};
use serde::{Deserialize, Deserializer};
use std::collections::HashMap;
use std::fs;
use std::path::{MAIN_SEPARATOR, Path, PathBuf};

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum OneOrMore<T> {
    Multiple(Vec<T>),
    Single(T),
}

#[derive(Deserialize, Debug, Clone, PartialEq, Hash)]
#[serde(untagged)]
pub enum Subdirs {
    Qualified(Vec<Source>),
    Recurse(bool),
}
impl Eq for Subdirs {}

#[derive(Deserialize, Debug, Clone, PartialEq, Hash)]
pub struct PackageSource {
    pub dir: String,
    pub subdirs: Option<Subdirs>,
    #[serde(rename = "type")]
    pub type_: Option<String>,
}

impl PackageSource {
    pub fn is_type_dev(&self) -> bool {
        match &self.type_ {
            Some(type_) => type_ == "dev",
            None => false,
        }
    }
}

impl Eq for PackageSource {}

#[derive(Deserialize, Debug, Clone, PartialEq, Hash)]
#[serde(untagged)]
pub enum Source {
    Shorthand(String),
    Qualified(PackageSource),
}

impl Source {
    /// When reading, we should propagate the sources all the way through the tree
    pub fn get_type(&self) -> Option<String> {
        match self {
            Source::Shorthand(_) => None,
            Source::Qualified(PackageSource { type_, .. }) => type_.clone(),
        }
    }
    pub fn set_type(&self, type_: Option<String>) -> Source {
        match (self, type_) {
            (Source::Shorthand(dir), Some(type_)) => Source::Qualified(PackageSource {
                dir: dir.to_string(),
                subdirs: None,
                type_: Some(type_),
            }),
            (Source::Qualified(package_source), type_) => Source::Qualified(PackageSource {
                type_,
                ..package_source.clone()
            }),
            (source, _) => source.clone(),
        }
    }

    /// `to_qualified_without_children` takes a tree like structure of dependencies, coming in from
    /// `rescript.json`, and turns it into a flat list. The main thing we extract here are the source
    /// folders, and optional subdirs, where potentially, the subdirs recurse or not.
    pub fn to_qualified_without_children(&self, sub_path: Option<PathBuf>) -> PackageSource {
        match self {
            Source::Shorthand(dir) => PackageSource {
                dir: sub_path
                    .map(|p| p.join(Path::new(dir)))
                    .unwrap_or(Path::new(dir).to_path_buf())
                    .to_string_lossy()
                    .to_string(),
                subdirs: None,
                type_: self.get_type(),
            },
            Source::Qualified(PackageSource {
                dir,
                type_,
                subdirs: Some(Subdirs::Recurse(should_recurse)),
            }) => PackageSource {
                dir: sub_path
                    .map(|p| p.join(Path::new(dir)))
                    .unwrap_or(Path::new(dir).to_path_buf())
                    .to_string_lossy()
                    .to_string(),
                subdirs: Some(Subdirs::Recurse(*should_recurse)),
                type_: type_.to_owned(),
            },
            Source::Qualified(PackageSource { dir, type_, .. }) => PackageSource {
                dir: sub_path
                    .map(|p| p.join(Path::new(dir)))
                    .unwrap_or(Path::new(dir).to_path_buf())
                    .to_string_lossy()
                    .to_string(),
                subdirs: None,
                type_: type_.to_owned(),
            },
        }
    }

    fn find_is_type_dev_for_sub_folder(
        &self,
        relative_parent_path: &Path,
        target_source_folder: &Path,
    ) -> bool {
        match &self {
            Source::Shorthand(sub_folder) => {
                relative_parent_path.join(Path::new(sub_folder)) == *target_source_folder
            }
            Source::Qualified(package_source) => {
                // Note that we no longer check if type_ is dev of the nested subfolder.
                // A parent was type:dev, so we assume all subfolders are as well.
                let next_parent_path = relative_parent_path.join(Path::new(&package_source.dir));
                if next_parent_path == *target_source_folder {
                    return true;
                };

                match &package_source.subdirs {
                    None => false,
                    Some(Subdirs::Recurse(false)) => false,
                    Some(Subdirs::Recurse(true)) => target_source_folder.starts_with(&next_parent_path),
                    Some(Subdirs::Qualified(nested_sources)) => nested_sources.iter().any(|nested_source| {
                        nested_source.find_is_type_dev_for_sub_folder(&next_parent_path, target_source_folder)
                    }),
                }
            }
        }
    }
}

impl Eq for Source {}

#[derive(Deserialize, Debug, Clone)]
pub struct PackageSpec {
    pub module: PackageModule,
    #[serde(rename = "in-source", default = "default_true")]
    pub in_source: bool,
    pub suffix: Option<String>,
}

#[derive(Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum PackageModule {
    #[serde(rename = "commonjs")]
    CommonJs,
    #[serde(rename = "esmodule")]
    EsModule,
}

impl PackageModule {
    pub fn as_str(&self) -> &'static str {
        match self {
            PackageModule::CommonJs => "commonjs",
            PackageModule::EsModule => "esmodule",
        }
    }
}

impl PackageSpec {
    pub fn get_out_of_source_dir(&self) -> String {
        match self.module {
            PackageModule::CommonJs => "js",
            PackageModule::EsModule => "es6",
        }
        .to_string()
    }

    pub fn is_common_js(&self) -> bool {
        self.module == PackageModule::CommonJs
    }

    pub fn get_suffix(&self) -> Option<String> {
        self.suffix.to_owned()
    }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum Error {
    Catchall(bool),
    Qualified(String),
}

#[derive(Deserialize, Debug, Clone)]
pub struct Warnings {
    pub number: Option<String>,
    pub error: Option<Error>,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum NamespaceConfig {
    Bool(bool),
    String(String),
}

#[derive(Deserialize, Debug, Clone, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum JsxMode {
    Classic,
    Automatic,
}

#[derive(Deserialize, Debug, Clone, Eq, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum JsxModule {
    React,
    Other(String),
}

#[derive(Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct JsxSpecs {
    pub version: Option<i32>,
    pub module: Option<JsxModule>,
    pub mode: Option<JsxMode>,
    #[serde(rename = "v3-dependencies")]
    pub v3_dependencies: Option<Vec<String>>,
    pub preserve: Option<bool>,
}

/// We do not care about the internal structure because the gentype config is loaded by bsc.
pub type GenTypeConfig = serde_json::Value;

/// Configuration for running a command after each JavaScript file is compiled.
/// Note: Unlike bsb, rewatch passes absolute paths to the command for clarity.
#[derive(Deserialize, Debug, Clone)]
pub struct JsPostBuild {
    pub cmd: String,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum DeprecationWarning {}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ExperimentalFeature {
    LetUnwrap,
    JsxText,
}

impl<'de> serde::Deserialize<'de> for ExperimentalFeature {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct EFVisitor;
        impl<'de> Visitor<'de> for EFVisitor {
            type Value = ExperimentalFeature;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a valid experimental feature id (e.g. LetUnwrap)")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: DeError,
            {
                match v {
                    "LetUnwrap" => Ok(ExperimentalFeature::LetUnwrap),
                    "JsxText" => Ok(ExperimentalFeature::JsxText),
                    other => {
                        let available = ["LetUnwrap", "JsxText"].join(", ");
                        Err(DeError::custom(format!(
                            "Unknown experimental feature '{other}'. Available features: {available}",
                        )))
                    }
                }
            }
        }
        deserializer.deserialize_any(EFVisitor)
    }
}

/// # rescript.json representation
/// This is tricky, there is a lot of ambiguity. This is probably incomplete.
#[derive(Deserialize, Debug, Clone, Default)]
pub struct Config {
    pub name: String,
    // In the case of monorepos, the root source won't necessarily have to have sources. It can
    // just be sources in packages
    pub sources: Option<OneOrMore<Source>>,
    #[serde(rename = "package-specs")]
    pub package_specs: Option<OneOrMore<PackageSpec>>,
    pub warnings: Option<Warnings>,
    pub suffix: Option<String>,
    pub dependencies: Option<Vec<String>>,
    #[serde(rename = "dev-dependencies")]
    pub dev_dependencies: Option<Vec<String>>,
    #[serde(rename = "ppx-flags")]
    pub ppx_flags: Option<Vec<OneOrMore<String>>>,

    #[serde(rename = "compiler-flags")]
    pub compiler_flags: Option<Vec<OneOrMore<String>>>,

    pub namespace: Option<NamespaceConfig>,
    pub jsx: Option<JsxSpecs>,
    #[serde(rename = "experimental-features")]
    pub experimental_features: Option<HashMap<ExperimentalFeature, bool>>,
    #[serde(rename = "gentypeconfig")]
    pub gentype_config: Option<GenTypeConfig>,
    #[serde(rename = "js-post-build")]
    pub js_post_build: Option<JsPostBuild>,
    // Used by the VS Code extension; ignored by rewatch but should not emit warnings.
    // Payload is not validated here, only in the VS Code extension.
    pub editor: Option<serde_json::Value>,
    // Used by rescript-tools reanalyze; ignored by rewatch but should not emit warnings.
    // Payload is not validated here, only in reanalyze.
    pub reanalyze: Option<serde_json::Value>,
    // this is a new feature of rewatch, and it's not part of the rescript.json spec
    #[serde(rename = "namespace-entry")]
    pub namespace_entry: Option<String>,
    // this is a new feature of rewatch, and it's not part of the rescript.json spec
    #[serde(rename = "allowed-dependents")]
    pub allowed_dependents: Option<Vec<String>>,

    // Holds all deprecation warnings for the config struct
    #[serde(skip)]
    deprecation_warnings: Vec<DeprecationWarning>,

    // Holds unknown fields we encountered while parsing
    #[serde(skip, default)]
    unknown_fields: Vec<String>,

    #[serde(default = "default_path")]
    pub path: PathBuf,
}

fn default_path() -> PathBuf {
    PathBuf::from("./rescript.json")
}

/// This flattens string flags
pub fn flatten_flags(flags: &Option<Vec<OneOrMore<String>>>) -> Vec<String> {
    match flags {
        None => vec![],
        Some(xs) => xs
            .iter()
            .flat_map(|x| match x {
                OneOrMore::Single(y) => vec![y.to_owned()],
                OneOrMore::Multiple(ys) => ys.to_owned(),
            })
            .collect::<Vec<String>>()
            .iter()
            .flat_map(|str| str.split(' '))
            .map(|str| str.to_string())
            .collect::<Vec<String>>(),
    }
}

/// Since ppx-flags could be one or more, and could potentially be nested, this function takes the
/// flags and flattens them.
pub fn flatten_ppx_flags(
    project_context: &ProjectContext,
    package_config: &Config,
    flags: &Option<Vec<OneOrMore<String>>>,
) -> Result<Vec<String>> {
    match flags {
        None => Ok(vec![]),
        Some(flags) => flags.iter().try_fold(Vec::new(), |mut acc, x| {
            match x {
                OneOrMore::Single(y) => {
                    let first_character = y.chars().next();
                    match first_character {
                        Some('.') => {
                            let path = helpers::try_package_path(
                                package_config,
                                project_context,
                                &format!("{}{}{}", &package_config.name, MAIN_SEPARATOR, y),
                            )
                            .map(|p| p.to_string_lossy().to_string())?;

                            acc.push(String::from("-ppx"));
                            acc.push(path);
                        }
                        _ => {
                            acc.push(String::from("-ppx"));
                            let path = helpers::try_package_path(package_config, project_context, y)
                                .map(|p| p.to_string_lossy().to_string())?;
                            acc.push(path);
                        }
                    }
                }
                OneOrMore::Multiple(ys) if ys.is_empty() => (),
                OneOrMore::Multiple(ys) => {
                    let first_character = ys[0].chars().next();
                    let ppx = match first_character {
                        Some('.') => helpers::try_package_path(
                            package_config,
                            project_context,
                            &format!("{}{}{}", package_config.name, MAIN_SEPARATOR, &ys[0]),
                        )
                        .map(|p| p.to_string_lossy().to_string())?,
                        _ => helpers::try_package_path(package_config, project_context, &ys[0])
                            .map(|p| p.to_string_lossy().to_string())?,
                    };
                    acc.push(String::from("-ppx"));
                    acc.push(
                        vec![ppx]
                            .into_iter()
                            .chain(ys[1..].to_owned())
                            .collect::<Vec<String>>()
                            .join(" "),
                    );
                }
            };
            Ok(acc)
        }),
    }
}

fn namespace_from_package_name(package_name: &str) -> String {
    let len = package_name.len();
    let mut buf = String::with_capacity(len);

    fn aux(s: &str, capital: bool, buf: &mut String, off: usize) {
        if off >= s.len() {
            return;
        }

        let ch = s.as_bytes()[off] as char;
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                let new_capital = false;
                buf.push(if capital { ch.to_ascii_uppercase() } else { ch });
                aux(s, new_capital, buf, off + 1);
            }
            '/' | '-' => {
                aux(s, true, buf, off + 1);
            }
            _ => {
                aux(s, capital, buf, off + 1);
            }
        }
    }

    aux(package_name, true, &mut buf, 0);
    buf
}

impl Config {
    /// Try to convert a config from a certain path to a config struct
    pub fn new(path: &Path) -> Result<Self> {
        let read = fs::read_to_string(path)?;
        let mut config = Config::new_from_json_string(&read)?;
        config.set_path(path.to_path_buf())?;
        Ok(config)
    }

    /// Try to convert a config from a string to a config struct
    pub fn new_from_json_string(config_str: &str) -> Result<Self> {
        if let Ok(value) = serde_json::from_str::<serde_json::Value>(config_str) {
            validate_package_specs_value(&value)?;
        }

        let mut deserializer = serde_json::Deserializer::from_str(config_str);
        let mut tracker = serde_path_to_error::Track::new();
        let path_deserializer = serde_path_to_error::Deserializer::new(&mut deserializer, &mut tracker);
        let mut unknown_fields = Vec::new();
        let mut config: Config =
            serde_ignored::deserialize(path_deserializer, |path| unknown_fields.push(path.to_string()))
                .map_err(|err: serde_json::Error| {
                    let path = tracker.path().to_string();
                    if path.is_empty() {
                        anyhow!("Failed to parse rescript.json: {err}")
                    } else {
                        anyhow!("Failed to parse rescript.json at {path}: {err}")
                    }
                })?;

        config.handle_deprecations()?;
        config.unknown_fields = unknown_fields;

        Ok(config)
    }

    fn set_path(&mut self, path: PathBuf) -> Result<()> {
        self.path = path;
        Ok(())
    }

    pub fn get_namespace(&self) -> packages::Namespace {
        let namespace_from_package = namespace_from_package_name(&self.name);
        match (self.namespace.as_ref(), self.namespace_entry.as_ref()) {
            (Some(NamespaceConfig::Bool(false)), _) => packages::Namespace::NoNamespace,
            (None, _) => packages::Namespace::NoNamespace,
            (Some(NamespaceConfig::Bool(true)), None) => {
                packages::Namespace::Namespace(namespace_from_package)
            }
            (Some(NamespaceConfig::Bool(true)), Some(entry)) => packages::Namespace::NamespaceWithEntry {
                namespace: namespace_from_package,
                entry: entry.to_string(),
            },
            (Some(NamespaceConfig::String(str)), None) => match str.as_str() {
                "true" => packages::Namespace::Namespace(namespace_from_package),
                namespace if namespace.is_case(Case::UpperFlat) => {
                    packages::Namespace::Namespace(namespace.to_string())
                }
                namespace => packages::Namespace::Namespace(namespace_from_package_name(namespace)),
            },
            (Some(self::NamespaceConfig::String(str)), Some(entry)) => match str.as_str() {
                "true" => packages::Namespace::NamespaceWithEntry {
                    namespace: namespace_from_package,
                    entry: entry.to_string(),
                },
                namespace if namespace.is_case(Case::UpperFlat) => packages::Namespace::NamespaceWithEntry {
                    namespace: namespace.to_string(),
                    entry: entry.to_string(),
                },
                namespace => packages::Namespace::NamespaceWithEntry {
                    namespace: namespace.to_string().to_case(Case::Pascal),
                    entry: entry.to_string(),
                },
            },
        }
    }

    pub fn get_jsx_args(&self) -> Vec<String> {
        match self.jsx.to_owned() {
            Some(jsx) => match jsx.version {
                Some(version) if version == 4 => {
                    vec!["-bs-jsx".to_string(), version.to_string()]
                }
                Some(version) => panic!("JSX version {version} is unsupported"),
                None => vec![],
            },
            None => vec![],
        }
    }

    pub fn get_jsx_mode_args(&self) -> Vec<String> {
        match self.jsx.to_owned() {
            Some(jsx) => match jsx.mode {
                Some(JsxMode::Classic) => {
                    vec!["-bs-jsx-mode".to_string(), "classic".to_string()]
                }
                Some(JsxMode::Automatic) => {
                    vec!["-bs-jsx-mode".to_string(), "automatic".to_string()]
                }

                None => vec![],
            },
            _ => vec![],
        }
    }

    pub fn get_jsx_module_args(&self) -> Vec<String> {
        match self.jsx.to_owned() {
            Some(jsx) => match jsx.module {
                Some(JsxModule::React) => {
                    vec!["-bs-jsx-module".to_string(), "react".to_string()]
                }
                Some(JsxModule::Other(module)) => {
                    vec!["-bs-jsx-module".to_string(), module]
                }
                None => vec![],
            },
            _ => vec![],
        }
    }

    pub fn get_jsx_preserve_args(&self) -> Vec<String> {
        match self.jsx.to_owned() {
            Some(jsx) => match jsx.preserve {
                Some(true) => vec!["-bs-jsx-preserve".to_string()],
                _ => vec![],
            },
            _ => vec![],
        }
    }

    pub fn get_experimental_features_args(&self) -> Vec<String> {
        match &self.experimental_features {
            None => vec![],
            Some(map) => map
                .iter()
                .filter_map(|(k, v)| if *v { Some(k) } else { None })
                .flat_map(|feature| {
                    vec![
                        "-enable-experimental".to_string(),
                        match feature {
                            ExperimentalFeature::LetUnwrap => "LetUnwrap",
                            ExperimentalFeature::JsxText => "JsxText",
                        }
                        .to_string(),
                    ]
                })
                .collect(),
        }
    }

    pub fn get_gentype_arg(&self) -> Vec<String> {
        match &self.gentype_config {
            Some(_) => vec!["-bs-gentype".to_string()],
            None => vec![],
        }
    }

    pub fn get_warning_args(&self, is_local_dep: bool, warn_error_override: Option<String>) -> Vec<String> {
        // Ignore warning config for non local dependencies (node_module dependencies)
        if !is_local_dep {
            return vec![];
        }

        // Command-line --warn-error flag takes precedence over rescript.json configuration
        // This follows the same precedence behavior as the legacy bsb build system
        if let Some(warn_error_str) = warn_error_override {
            return vec!["-warn-error".to_string(), warn_error_str];
        }

        match self.warnings {
            None => vec![],
            Some(ref warnings) => {
                let warn_number = match warnings.number {
                    None => vec![],
                    Some(ref warnings) => {
                        vec!["-w".to_string(), warnings.to_string()]
                    }
                };

                let warn_error = match warnings.error {
                    Some(Error::Catchall(true)) => {
                        vec!["-warn-error".to_string(), "A".to_string()]
                    }
                    Some(Error::Qualified(ref errors)) => {
                        vec!["-warn-error".to_string(), errors.to_string()]
                    }
                    _ => vec![],
                };

                [warn_number, warn_error].concat()
            }
        }
    }

    pub fn get_package_specs(&self) -> Vec<PackageSpec> {
        match self.package_specs.clone() {
            None => vec![PackageSpec {
                module: PackageModule::EsModule,
                in_source: true,
                suffix: Some(".js".to_string()),
            }],
            Some(OneOrMore::Single(spec)) => vec![spec],
            Some(OneOrMore::Multiple(vec)) => vec,
        }
    }

    pub fn get_suffix(&self, spec: &PackageSpec) -> String {
        spec.get_suffix()
            .or(self.suffix.clone())
            .unwrap_or(".js".to_string())
    }

    pub fn find_is_type_dev_for_path(&self, relative_path: &Path) -> bool {
        let relative_parent = match relative_path.parent() {
            None => return false,
            Some(parent) => Path::new(parent),
        };

        let package_sources = match self.sources.as_ref() {
            None => vec![],
            Some(OneOrMore::Single(Source::Shorthand(_))) => vec![],
            Some(OneOrMore::Single(Source::Qualified(source))) => vec![source],
            Some(OneOrMore::Multiple(multiple)) => multiple
                .iter()
                .filter_map(|source| match source {
                    Source::Shorthand(_) => None,
                    Source::Qualified(package_source) => Some(package_source),
                })
                .collect(),
        };

        package_sources.iter().any(|package_source| {
            if !package_source.is_type_dev() {
                false
            } else {
                let dir_path = Path::new(&package_source.dir);
                if dir_path == relative_parent {
                    return true;
                };

                match &package_source.subdirs {
                    None => false,
                    Some(Subdirs::Recurse(false)) => false,
                    Some(Subdirs::Recurse(true)) => relative_path.starts_with(dir_path),
                    Some(Subdirs::Qualified(sub_dirs)) => sub_dirs
                        .iter()
                        .any(|sub_dir| sub_dir.find_is_type_dev_for_sub_folder(dir_path, relative_parent)),
                }
            }
        })
    }

    pub fn get_deprecations(&self) -> &[DeprecationWarning] {
        &self.deprecation_warnings
    }

    pub fn get_unknown_fields(&self) -> Vec<String> {
        self.unknown_fields
            .iter()
            .filter(|field| !self.is_unsupported_field(field))
            .cloned()
            .collect()
    }

    pub fn get_unsupported_fields(&self) -> Vec<String> {
        self.unknown_fields
            .iter()
            .filter(|field| self.is_unsupported_field(field))
            .cloned()
            .collect::<Vec<_>>()
    }

    fn is_unsupported_field(&self, field: &str) -> bool {
        const UNSUPPORTED_TOP_LEVEL_FIELDS: &[&str] = &[
            "ignored-dirs",
            "generators",
            "cut-generators",
            "pp-flags",
            "entries",
            "bs-external-includes",
        ];

        let top_level = field.split(|c| ['.', '['].contains(&c)).next().unwrap_or(field);

        UNSUPPORTED_TOP_LEVEL_FIELDS.contains(&top_level)
    }

    fn handle_deprecations(&mut self) -> Result<()> {
        Ok(())
    }
}

fn validate_package_specs_value(value: &serde_json::Value) -> Result<()> {
    let specs = match value.get("package-specs") {
        Some(specs) => specs,
        None => return Ok(()),
    };

    let top_level_suffix = value
        .get("suffix")
        .and_then(|suffix| suffix.as_str())
        .unwrap_or(".js")
        .to_string();
    let mut seen_suffixes = std::collections::HashSet::new();

    match specs {
        serde_json::Value::Array(specs) => {
            for spec in specs {
                validate_package_spec_value(spec)?;
                if let Some(suffix) = resolve_spec_suffix(spec, &top_level_suffix) {
                    let in_source = resolve_spec_in_source(spec);
                    if !seen_suffixes.insert((suffix.clone(), in_source)) {
                        return Err(anyhow!(
                            "Duplicate package-spec suffix \"{suffix}\" is not allowed."
                        ));
                    }
                }
            }
        }
        serde_json::Value::Object(_) => {
            validate_package_spec_value(specs)?;
            if let Some(suffix) = resolve_spec_suffix(specs, &top_level_suffix) {
                let in_source = resolve_spec_in_source(specs);
                if !seen_suffixes.insert((suffix.clone(), in_source)) {
                    return Err(anyhow!(
                        "Duplicate package-spec suffix \"{suffix}\" is not allowed."
                    ));
                }
            }
        }
        _ => {}
    }

    Ok(())
}

fn resolve_spec_suffix(spec: &serde_json::Value, top_level_suffix: &str) -> Option<String> {
    if !spec.is_object() {
        return None;
    }

    match spec.get("suffix").and_then(|suffix| suffix.as_str()) {
        Some(suffix) => Some(suffix.to_string()),
        None => Some(top_level_suffix.to_string()),
    }
}

fn resolve_spec_in_source(spec: &serde_json::Value) -> bool {
    if !spec.is_object() {
        return true;
    }

    spec.get("in-source")
        .and_then(|in_source| in_source.as_bool())
        .unwrap_or(true)
}

fn validate_package_spec_value(value: &serde_json::Value) -> Result<()> {
    let module = match value.get("module") {
        Some(module) => module,
        None => return Ok(()),
    };

    let module = match module.as_str() {
        Some(module) => module,
        None => return Ok(()),
    };

    match module {
        "commonjs" | "esmodule" => Ok(()),
        other => Err(anyhow!(
            "Module system \"{other}\" is unsupported. Expected \"commonjs\" or \"esmodule\"."
        )),
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    pub struct CreateConfigArgs {
        pub name: String,
        pub bs_deps: Vec<String>,
        pub build_dev_deps: Vec<String>,
        pub allowed_dependents: Option<Vec<String>>,
        pub path: PathBuf,
    }

    pub fn create_config(args: CreateConfigArgs) -> Config {
        Config {
            name: args.name,
            sources: Some(crate::config::OneOrMore::Single(Source::Shorthand(String::from(
                "Source",
            )))),
            package_specs: None,
            warnings: None,
            suffix: None,
            dependencies: Some(args.bs_deps),
            dev_dependencies: Some(args.build_dev_deps),
            ppx_flags: None,
            compiler_flags: None,
            namespace: None,
            jsx: None,
            gentype_config: None,
            js_post_build: None,
            editor: None,
            reanalyze: None,
            namespace_entry: None,
            deprecation_warnings: vec![],
            experimental_features: None,
            allowed_dependents: args.allowed_dependents,
            unknown_fields: vec![],
            path: args.path,
        }
    }

    #[test]
    fn test_getters() {
        let json = r#"
        {
            "name": "my-monorepo",
            "sources": [ { "dir": "src/", "subdirs": true } ],
            "package-specs": [ { "module": "esmodule", "in-source": true } ],
            "suffix": ".mjs",
            "dependencies": [ "@teamwalnut/app" ]
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        let specs = config.get_package_specs();
        assert_eq!(specs.len(), 1);
        let spec = specs.first().unwrap();
        assert_eq!(spec.module, PackageModule::EsModule);
        assert_eq!(config.get_suffix(spec), ".mjs");
    }

    #[test]
    fn test_package_specs_duplicate_suffix_default() {
        let json = r#"
        {
            "name": "dup-suffix-default",
            "sources": ".",
            "package-specs": [
                { "module": "commonjs", "in-source": true },
                { "module": "esmodule", "in-source": true }
            ]
        }
        "#;

        let error = Config::new_from_json_string(json).unwrap_err().to_string();
        assert!(error.contains("Duplicate package-spec suffix"));
    }

    #[test]
    fn test_package_specs_duplicate_suffix_explicit() {
        let json = r#"
        {
            "name": "dup-suffix-explicit",
            "sources": ".",
            "package-specs": [
                { "module": "commonjs", "in-source": true, "suffix": ".mjs" },
                { "module": "esmodule", "in-source": true, "suffix": ".mjs" }
            ]
        }
        "#;

        let error = Config::new_from_json_string(json).unwrap_err().to_string();
        assert!(error.contains("Duplicate package-spec suffix"));
    }

    #[test]
    fn test_package_specs_duplicate_suffix_different_in_source_ok() {
        let json = r#"
        {
            "name": "dup-suffix-different-in-source",
            "sources": ".",
            "package-specs": [
                { "module": "esmodule", "in-source": true, "suffix": ".res.js" },
                { "module": "esmodule", "in-source": false, "suffix": ".res.js" }
            ]
        }
        "#;

        let config = Config::new_from_json_string(json).unwrap();
        assert_eq!(config.get_package_specs().len(), 2);
    }

    #[test]
    fn test_sources() {
        let json = r#"
        {
          "name": "@rescript/core",
          "version": "0.5.0",
          "sources": {
              "dir": "test",
              "subdirs": ["intl"],
              "type": "dev"
          },
          "suffix": ".mjs",
          "package-specs": {
            "module": "esmodule",
            "in-source": true
          },
          "dev-dependencies": ["@rescript/tools"],
          "warnings": {
            "error": "+101"
          }
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        if let Some(OneOrMore::Single(source)) = config.sources {
            let source = source.to_qualified_without_children(None);
            assert_eq!(source.type_, Some(String::from("dev")));
        } else {
            dbg!(config.sources);
            unreachable!()
        }
    }

    #[test]
    fn test_dev_sources_multiple() {
        let json = r#"
        {
            "name": "@rescript/core",
            "version": "0.5.0",
            "sources": [
                { "dir": "src" },
                { "dir": "test", "type": "dev" }
            ],
            "package-specs": {
              "module": "esmodule",
              "in-source": true
            },
            "dev-dependencies": ["@rescript/tools"],
            "suffix": ".mjs",
            "warnings": {
              "error": "+101"
            }
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        if let Some(OneOrMore::Multiple(sources)) = config.sources {
            assert_eq!(sources.len(), 2);
            let test_dir = sources[1].to_qualified_without_children(None);

            assert_eq!(test_dir.type_, Some(String::from("dev")));
        } else {
            dbg!(config.sources);
            unreachable!()
        }
    }

    #[test]
    fn test_detect_gentypeconfig() {
        let json = r#"
        {
            "name": "my-monorepo",
            "sources": [ { "dir": "src/", "subdirs": true } ],
            "package-specs": [ { "module": "esmodule", "in-source": true } ],
            "suffix": ".mjs",
            "dependencies": [ "@teamwalnut/app" ],
            "gentypeconfig": {
                "module": "esmodule",
                "generatedFileExtension": ".gen.tsx"
            }
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert!(config.gentype_config.is_some());
        assert_eq!(config.get_gentype_arg(), vec!["-bs-gentype".to_string()]);
    }

    #[test]
    fn test_other_jsx_module() {
        let json = r#"
        {
            "name": "my-monorepo",
            "sources": [ { "dir": "src/", "subdirs": true } ],
            "package-specs": [ { "module": "esmodule", "in-source": true } ],
            "suffix": ".mjs",
            "dependencies": [ "@teamwalnut/app" ],
            "jsx": {
                "module": "Voby.JSX"
            }
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert!(config.jsx.is_some());
        assert_eq!(
            config.jsx.unwrap(),
            JsxSpecs {
                version: None,
                module: Some(JsxModule::Other(String::from("Voby.JSX"))),
                mode: None,
                v3_dependencies: None,
                preserve: None,
            },
        );
    }

    #[test]
    fn test_jsx_preserve() {
        let json = r#"
        {
            "name": "my-monorepo",
            "sources": [ { "dir": "src/", "subdirs": true } ],
            "package-specs": [ { "module": "esmodule", "in-source": true } ],
            "suffix": ".mjs",
            "dependencies": [ "@teamwalnut/app" ],
            "jsx": { "version": 4, "preserve": true }
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert!(config.jsx.is_some());
        assert_eq!(
            config.jsx.unwrap(),
            JsxSpecs {
                version: Some(4),
                module: None,
                mode: None,
                v3_dependencies: None,
                preserve: Some(true),
            },
        );
    }

    #[test]
    fn test_get_suffix() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "package-specs": [
                {
                "module": "esmodule",
                "in-source": true
                }
            ],
            "suffix": ".mjs"
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert_eq!(
            config.get_suffix(config.get_package_specs().first().unwrap()),
            ".mjs"
        );
    }

    #[test]
    fn test_dependencies() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "package-specs": [
                {
                "module": "esmodule",
                "in-source": true
                }
            ],
            "suffix": ".mjs",
            "dependencies": [ "@testrepo/main" ]
        }
        "#;

        let config = Config::new_from_json_string(json).expect("a valid json string");
        assert_eq!(config.dependencies, Some(vec!["@testrepo/main".to_string()]));
        assert!(config.get_deprecations().is_empty());
    }

    #[test]
    fn test_dev_dependencies() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "package-specs": [
                {
                "module": "esmodule",
                "in-source": true
                }
            ],
            "suffix": ".mjs",
            "dev-dependencies": [ "@testrepo/main" ]
        }
        "#;

        let config = Config::new_from_json_string(json).expect("a valid json string");
        assert_eq!(config.dev_dependencies, Some(vec!["@testrepo/main".to_string()]));
        assert!(config.get_deprecations().is_empty());
    }

    #[test]
    fn test_package_specs_es6_global_deprecation() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "package-specs": [
                {
                "module": "es6-global",
                "in-source": true
                }
            ],
            "suffix": ".mjs"
        }
        "#;

        let err = Config::new_from_json_string(json).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("Module system \"es6-global\" is unsupported"));
    }

    #[test]
    fn test_package_specs_es6_deprecation() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "package-specs": [
                {
                "module": "es6",
                "in-source": true
                }
            ],
            "suffix": ".mjs"
        }
        "#;

        let err = Config::new_from_json_string(json).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("Module system \"es6\" is unsupported"));
    }

    #[test]
    fn test_unknown_fields_are_collected() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "some-new-field": true
        }
        "#;

        let config = Config::new_from_json_string(json).expect("a valid json string");
        assert_eq!(config.get_unknown_fields(), vec!["some-new-field".to_string()]);
        assert!(config.get_unsupported_fields().is_empty());
    }

    #[test]
    fn test_unsupported_fields_are_collected() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "ignored-dirs": ["scripts"]
        }
        "#;

        let config = Config::new_from_json_string(json).expect("a valid json string");
        assert_eq!(config.get_unsupported_fields(), vec!["ignored-dirs".to_string()]);
        assert!(config.get_unknown_fields().is_empty());
    }

    #[test]
    fn test_editor_field_supported() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "editor": {
                "reason": {
                    "profile": "development"
                }
            }
        }
        "#;

        let config = Config::new_from_json_string(json).expect("a valid json string");
        assert!(config.get_unsupported_fields().is_empty());
        assert!(config.get_unknown_fields().is_empty());
    }

    #[test]
    fn test_compiler_flags() {
        let json = r#"
        {
            "name": "testrepo",
            "sources": {
                "dir": "src",
                "subdirs": true
            },
            "package-specs": [
                {
                "module": "esmodule",
                "in-source": true
                }
            ],
            "suffix": ".mjs",
            "compiler-flags": [ "-open ABC" ]
        }
        "#;

        let config = Config::new_from_json_string(json).expect("a valid json string");
        if let Some(flags) = &config.compiler_flags {
            if let Some(OneOrMore::Single(flag)) = flags.first() {
                assert_eq!(flag.as_str(), "-open ABC");
            } else {
                dbg!(config.compiler_flags);
                unreachable!("Expected first flag to be OneOrMore::Single");
            }
        } else {
            dbg!(config.compiler_flags);
            unreachable!("Expected compiler flags to be Some");
        }
        assert!(config.get_deprecations().is_empty());
    }

    fn test_find_is_type_dev(source: OneOrMore<Source>, path: &Path, expected: bool) {
        let config = Config {
            name: String::from("testrepo"),
            sources: Some(source),
            ..Default::default()
        };
        let result = config.find_is_type_dev_for_path(path);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_find_is_type_dev_for_exact_match() {
        test_find_is_type_dev(
            OneOrMore::Single(Source::Qualified(PackageSource {
                dir: String::from("src"),
                subdirs: None,
                type_: Some(String::from("dev")),
            })),
            Path::new("src/Foo.res"),
            true,
        )
    }

    #[test]
    fn test_find_is_type_dev_for_none_dev() {
        test_find_is_type_dev(
            OneOrMore::Single(Source::Qualified(PackageSource {
                dir: String::from("src"),
                subdirs: None,
                type_: None,
            })),
            Path::new("src/Foo.res"),
            false,
        )
    }

    #[test]
    fn test_find_is_type_dev_for_multiple_sources() {
        test_find_is_type_dev(
            OneOrMore::Multiple(vec![Source::Qualified(PackageSource {
                dir: String::from("src"),
                subdirs: None,
                type_: Some(String::from("dev")),
            })]),
            Path::new("src/Foo.res"),
            true,
        )
    }

    #[test]
    fn test_find_is_type_dev_for_shorthand() {
        test_find_is_type_dev(
            OneOrMore::Multiple(vec![Source::Shorthand(String::from("src"))]),
            Path::new("src/Foo.res"),
            false,
        )
    }

    #[test]
    fn test_find_is_type_dev_for_recursive_folder() {
        test_find_is_type_dev(
            OneOrMore::Multiple(vec![Source::Qualified(PackageSource {
                dir: String::from("src"),
                subdirs: Some(Subdirs::Recurse(true)),
                type_: Some(String::from("dev")),
            })]),
            Path::new("src/bar/Foo.res"),
            true,
        )
    }

    #[test]
    fn test_find_is_type_dev_for_sub_folder() {
        test_find_is_type_dev(
            OneOrMore::Multiple(vec![Source::Qualified(PackageSource {
                dir: String::from("src"),
                subdirs: Some(Subdirs::Qualified(vec![Source::Qualified(PackageSource {
                    dir: String::from("bar"),
                    subdirs: None,
                    type_: None,
                })])),
                type_: Some(String::from("dev")),
            })]),
            Path::new("src/bar/Foo.res"),
            true,
        )
    }

    #[test]
    fn test_find_is_type_dev_for_sub_folder_shorthand() {
        test_find_is_type_dev(
            OneOrMore::Multiple(vec![Source::Qualified(PackageSource {
                dir: String::from("src"),
                subdirs: Some(Subdirs::Qualified(vec![Source::Shorthand(String::from("bar"))])),
                type_: Some(String::from("dev")),
            })]),
            Path::new("src/bar/Foo.res"),
            true,
        )
    }

    #[test]
    fn test_get_warning_args_with_override() {
        let config = create_config(CreateConfigArgs {
            name: "test".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });

        // Test that warn_error_override takes precedence
        let args = config.get_warning_args(true, Some("+3+8+11".to_string()));
        assert_eq!(args, vec!["-warn-error".to_string(), "+3+8+11".to_string()]);
    }

    #[test]
    fn test_get_warning_args_without_override() {
        let mut config = create_config(CreateConfigArgs {
            name: "test".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });

        // Set up warnings in config
        config.warnings = Some(Warnings {
            number: Some("+8+32".to_string()),
            error: Some(Error::Catchall(true)),
        });

        // Test that config warnings are used when no override
        let args = config.get_warning_args(true, None);
        assert_eq!(
            args,
            vec![
                "-w".to_string(),
                "+8+32".to_string(),
                "-warn-error".to_string(),
                "A".to_string()
            ]
        );
    }

    #[test]
    fn test_get_warning_args_non_local_dep() {
        let config = create_config(CreateConfigArgs {
            name: "test".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });

        // Test that non-local deps ignore warning config
        let args: Vec<String> = config.get_warning_args(false, None);
        assert_eq!(args, Vec::<String>::new());
    }

    #[test]
    fn test_get_warning_args_override_ignores_config() {
        let mut config = create_config(CreateConfigArgs {
            name: "test".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });

        // Set up warnings in config
        config.warnings = Some(Warnings {
            number: Some("+8+32".to_string()),
            error: Some(Error::Catchall(true)),
        });

        // Test that override completely ignores config warnings
        let args = config.get_warning_args(true, Some("+3+8+11".to_string()));
        assert_eq!(args, vec!["-warn-error".to_string(), "+3+8+11".to_string()]);
    }

    #[test]
    fn test_get_warning_args_non_local_dep_ignores_override() {
        let config = create_config(CreateConfigArgs {
            name: "test".to_string(),
            bs_deps: vec![],
            build_dev_deps: vec![],
            allowed_dependents: None,
            path: PathBuf::from("./rescript.json"),
        });

        // Non-local dependency should never receive warning args, even if override is provided
        let args = config.get_warning_args(false, Some("+3+8+11".to_string()));
        assert_eq!(args, Vec::<String>::new());
    }
}
