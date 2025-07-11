use crate::build::packages;
use crate::helpers::deserialize::*;
use anyhow::Result;
use convert_case::{Case, Casing};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

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
    /// `bsconfig`, and turns it into a flat list. The main thing we extract here are the source
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
}

impl Eq for Source {}

#[derive(Deserialize, Debug, Clone)]
pub struct PackageSpec {
    pub module: String,
    #[serde(rename = "in-source", default = "default_true")]
    pub in_source: bool,
    pub suffix: Option<String>,
}

impl PackageSpec {
    pub fn get_out_of_source_dir(&self) -> String {
        match self.module.as_str() {
            "commonjs" => "js",
            _ => "es6",
        }
        .to_string()
    }

    pub fn is_common_js(&self) -> bool {
        match self.module.as_str() {
            "commonjs" => true,
            _ => false,
        }
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

#[derive(Deserialize, Debug, Clone, PartialEq, Hash)]
#[serde(untagged)]
pub enum Reason {
    Versioned {
        #[serde(rename = "react-jsx")]
        react_jsx: i32,
    },
    Unversioned(bool),
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

/// # bsconfig.json representation
/// This is tricky, there is a lot of ambiguity. This is probably incomplete.
#[derive(Deserialize, Debug, Clone)]
pub struct Config {
    pub name: String,
    // In the case of monorepos, the root source won't necessarily have to have sources. It can
    // just be sources in packages
    pub sources: Option<OneOrMore<Source>>,
    #[serde(rename = "package-specs")]
    pub package_specs: Option<OneOrMore<PackageSpec>>,
    pub warnings: Option<Warnings>,
    pub suffix: Option<String>,
    #[serde(rename = "pinned-dependencies")]
    pub pinned_dependencies: Option<Vec<String>>,
    #[serde(rename = "dependencies", alias = "bs-dependencies")]
    pub bs_dependencies: Option<Vec<String>>,
    #[serde(rename = "bs-dev-dependencies", alias = "dev-dependencies")]
    pub bs_dev_dependencies: Option<Vec<String>>,
    #[serde(rename = "ppx-flags")]
    pub ppx_flags: Option<Vec<OneOrMore<String>>>,
    #[serde(rename = "bsc-flags", alias = "compiler-flags")]
    pub bsc_flags: Option<Vec<OneOrMore<String>>>,
    pub reason: Option<Reason>,
    pub namespace: Option<NamespaceConfig>,
    pub jsx: Option<JsxSpecs>,
    #[serde(rename = "gentypeconfig")]
    pub gentype_config: Option<GenTypeConfig>,
    // this is a new feature of rewatch, and it's not part of the bsconfig.json spec
    #[serde(rename = "namespace-entry")]
    pub namespace_entry: Option<String>,
    // this is a new feature of rewatch, and it's not part of the bsconfig.json spec
    #[serde(rename = "allowed-dependents")]
    pub allowed_dependents: Option<Vec<String>>,
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

/// Since ppx-flags could be one or more, and could be nested potentiall, this function takes the
/// flags and flattens them outright.
pub fn flatten_ppx_flags(
    node_modules_dir: &Path,
    flags: &Option<Vec<OneOrMore<String>>>,
    package_name: &String,
) -> Vec<String> {
    match flags {
        None => vec![],
        Some(flags) => flags
            .iter()
            .flat_map(|x| match x {
                OneOrMore::Single(y) => {
                    let first_character = y.chars().next();
                    match first_character {
                        Some('.') => {
                            vec![
                                "-ppx".to_string(),
                                node_modules_dir
                                    .join(package_name)
                                    .join(y)
                                    .to_string_lossy()
                                    .to_string(),
                            ]
                        }
                        _ => vec![
                            "-ppx".to_string(),
                            node_modules_dir.join(y).to_string_lossy().to_string(),
                        ],
                    }
                }
                OneOrMore::Multiple(ys) if ys.is_empty() => vec![],
                OneOrMore::Multiple(ys) => {
                    let first_character = ys[0].chars().next();
                    let ppx = match first_character {
                        Some('.') => node_modules_dir
                            .join(package_name)
                            .join(&ys[0])
                            .to_string_lossy()
                            .to_string(),
                        _ => node_modules_dir.join(&ys[0]).to_string_lossy().to_string(),
                    };
                    vec![
                        "-ppx".to_string(),
                        vec![ppx]
                            .into_iter()
                            .chain(ys[1..].to_owned())
                            .collect::<Vec<String>>()
                            .join(" "),
                    ]
                }
            })
            .collect::<Vec<String>>(),
    }
}

/// Try to convert a bsconfig from a certain path to a bsconfig struct
pub fn read(path: &Path) -> Result<Config> {
    let read = fs::read_to_string(path)?;
    let parse = serde_json::from_str::<Config>(&read)?;

    Ok(parse)
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
        match (self.reason.to_owned(), self.jsx.to_owned()) {
            (_, Some(jsx)) => match jsx.version {
                Some(version) if version == 3 || version == 4 => {
                    vec!["-bs-jsx".to_string(), version.to_string()]
                }
                Some(_version) => panic!("Unsupported JSX version"),
                None => vec![],
            },
            (Some(Reason::Versioned { react_jsx }), None) => {
                vec!["-bs-jsx".to_string(), format!("{}", react_jsx)]
            }
            (Some(Reason::Unversioned(true)), None) => {
                // If Reason is 'true' - we should default to the latest
                vec!["-bs-jsx".to_string()]
            }
            _ => vec![],
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

    pub fn get_gentype_arg(&self) -> Vec<String> {
        match &self.gentype_config {
            Some(_) => vec!["-bs-gentype".to_string()],
            None => vec![],
        }
    }

    pub fn get_warning_args(&self, is_local_dep: bool) -> Vec<String> {
        // Ignore warning config for non local dependencies (node_module dependencies)
        if !is_local_dep {
            return vec![];
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
                module: "commonjs".to_string(),
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_getters() {
        let json = r#"
        {
            "name": "my-monorepo",
            "sources": [ { "dir": "src/", "subdirs": true } ],
            "package-specs": [ { "module": "es6", "in-source": true } ],
            "suffix": ".mjs",
            "pinned-dependencies": [ "@teamwalnut/app" ],
            "bs-dependencies": [ "@teamwalnut/app" ]
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        let specs = config.get_package_specs();
        assert_eq!(specs.len(), 1);
        let spec = specs.first().unwrap();
        assert_eq!(spec.module, "es6");
        assert_eq!(config.get_suffix(spec), ".mjs");
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
          "bs-dev-dependencies": ["@rescript/tools"],
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
            "bs-dev-dependencies": ["@rescript/tools"],
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
            "package-specs": [ { "module": "es6", "in-source": true } ],
            "suffix": ".mjs",
            "pinned-dependencies": [ "@teamwalnut/app" ],
            "bs-dependencies": [ "@teamwalnut/app" ],
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
            "package-specs": [ { "module": "es6", "in-source": true } ],
            "suffix": ".mjs",
            "pinned-dependencies": [ "@teamwalnut/app" ],
            "bs-dependencies": [ "@teamwalnut/app" ],
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
            "package-specs": [ { "module": "es6", "in-source": true } ],
            "suffix": ".mjs",
            "pinned-dependencies": [ "@teamwalnut/app" ],
            "bs-dependencies": [ "@teamwalnut/app" ],
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
                "module": "es6",
                "in-source": true
                }
            ],
            "suffix": ".mjs"
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert_eq!(
            config.get_suffix(&config.get_package_specs().first().unwrap()),
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
                "module": "es6",
                "in-source": true
                }
            ],
            "suffix": ".mjs",
            "bs-dependencies": [ "@testrepo/main" ]
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert_eq!(config.bs_dependencies, Some(vec!["@testrepo/main".to_string()]));
    }

    #[test]
    fn test_dependencies_alias() {
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
            "suffix": ".mjs",
            "dependencies": [ "@testrepo/main" ]
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert_eq!(config.bs_dependencies, Some(vec!["@testrepo/main".to_string()]));
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
                "module": "es6",
                "in-source": true
                }
            ],
            "suffix": ".mjs",
            "bs-dev-dependencies": [ "@testrepo/main" ]
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert_eq!(
            config.bs_dev_dependencies,
            Some(vec!["@testrepo/main".to_string()])
        );
    }

    #[test]
    fn test_dev_dependencies_alias() {
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
            "suffix": ".mjs",
            "dev-dependencies": [ "@testrepo/main" ]
        }
        "#;

        let config = serde_json::from_str::<Config>(json).unwrap();
        assert_eq!(
            config.bs_dev_dependencies,
            Some(vec!["@testrepo/main".to_string()])
        );
    }
}
