use crate::build::packages::{Namespace, Package};
use ahash::{AHashMap, AHashSet};
use std::{fmt::Display, path::PathBuf, time::SystemTime};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseState {
    Pending,
    ParseError,
    Warning,
    Success,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompileState {
    Pending,
    Error,
    Warning,
    Success,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub path: PathBuf,
    pub parse_state: ParseState,
    pub compile_state: CompileState,
    pub last_modified: SystemTime,
    pub parse_dirty: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Implementation {
    pub path: PathBuf,
    pub parse_state: ParseState,
    pub compile_state: CompileState,
    pub last_modified: SystemTime,
    pub parse_dirty: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    pub implementation: Implementation,
    pub interface: Option<Interface>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MlMap {
    pub parse_dirty: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SourceType {
    SourceFile(SourceFile),
    MlMap(MlMap),
}

impl Display for SourceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceType::SourceFile(_) => write!(f, "SourceFile"),
            SourceType::MlMap(_) => write!(f, "MlMap"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub source_type: SourceType,
    pub deps: AHashSet<String>,
    pub dependents: AHashSet<String>,
    pub package_name: String,
    pub compile_dirty: bool,
    pub last_compiled_cmi: Option<SystemTime>,
    pub last_compiled_cmt: Option<SystemTime>,
    pub deps_dirty: bool,
}

impl Module {
    pub fn is_mlmap(&self) -> bool {
        matches!(self.source_type, SourceType::MlMap(_))
    }

    pub fn get_interface(&self) -> &Option<Interface> {
        match &self.source_type {
            SourceType::SourceFile(source_file) => &source_file.interface,
            _ => &None,
        }
    }
}

#[derive(Debug)]
pub struct BuildState {
    pub modules: AHashMap<String, Module>,
    pub packages: AHashMap<String, Package>,
    pub module_names: AHashSet<String>,
    pub project_root: PathBuf,
    pub root_config_name: String,
    pub deleted_modules: AHashSet<String>,
    pub rescript_version: String,
    pub bsc_path: PathBuf,
    pub workspace_root: Option<PathBuf>,
    pub deps_initialized: bool,
}

impl BuildState {
    pub fn get_package(&self, package_name: &str) -> Option<&Package> {
        self.packages.get(package_name)
    }

    pub fn get_module(&self, module_name: &str) -> Option<&Module> {
        self.modules.get(module_name)
    }
    pub fn new(
        project_root: PathBuf,
        root_config_name: String,
        packages: AHashMap<String, Package>,
        workspace_root: Option<PathBuf>,
        rescript_version: String,
        bsc_path: PathBuf,
    ) -> Self {
        Self {
            module_names: AHashSet::new(),
            modules: AHashMap::new(),
            packages,
            project_root,
            root_config_name,
            deleted_modules: AHashSet::new(),
            workspace_root,
            rescript_version,
            bsc_path,
            deps_initialized: false,
        }
    }

    pub fn insert_module(&mut self, module_name: &str, module: Module) {
        self.modules.insert(module_name.to_owned(), module);
        self.module_names.insert(module_name.to_owned());
    }
}

#[derive(Debug)]
pub struct AstModule {
    pub module_name: String,
    pub package_name: String,
    pub namespace: Namespace,
    pub last_modified: SystemTime,
    pub ast_file_path: PathBuf,
    pub is_root: bool,
    pub suffix: String,
}

#[derive(Debug)]
pub struct CompileAssetsState {
    pub ast_modules: AHashMap<PathBuf, AstModule>,
    pub cmi_modules: AHashMap<String, SystemTime>,
    pub cmt_modules: AHashMap<String, SystemTime>,
    pub ast_rescript_file_locations: AHashSet<PathBuf>,
    pub rescript_file_locations: AHashSet<PathBuf>,
}
