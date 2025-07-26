use std::{
    fs,
    hash::{DefaultHasher, Hash, Hasher},
    path::Path,
};

use crate::{config::Config, helpers, project_context::ProjectContext};

pub struct BuildMetadata {
    pub version: String,
    pub config_hash: String,
}

impl BuildMetadata {
    pub fn new(version: &str, config: &Config) -> Self {
        let mut default_hasher = DefaultHasher::new();
        config.hash(&mut default_hasher);
        let config_hash = default_hasher.finish().to_string();
        Self {
            version: version.to_string(),
            config_hash,
        }
    }
}

const BUILD_METADATA_FILE_NAME: &str = ".build-metadata";

pub fn read_build_metadata(path: &Path) -> Option<BuildMetadata> {
    let file_path = path.join(BUILD_METADATA_FILE_NAME);
    let file_content = fs::read_to_string(file_path).ok()?;
    let file_content = file_content.trim();

    let mut lines = file_content.lines();
    let version = lines.next()?.to_owned();
    let config_hash = lines.next()?.to_owned();

    Some(BuildMetadata { version, config_hash })
}

pub fn current_build_metadata(path: &Path, version: &str) -> BuildMetadata {
    let config = helpers::get_nearest_config(&path).expect("Couldn't find package root");
    let project_context = ProjectContext::new(&config).expect("Couldn't create project context");

    BuildMetadata::new(version, project_context.get_root_config())
}

pub fn is_build_metadata_different(
    last_build_metadata: Option<&BuildMetadata>,
    current_build_metadata: &BuildMetadata,
) -> bool {
    match last_build_metadata {
        Some(last_metadata) => {
            last_metadata.version != current_build_metadata.version
                || last_metadata.config_hash != current_build_metadata.config_hash
        }
        None => true,
    }
}

pub fn write_build_metadata(path: &Path, build_metadata: &BuildMetadata) {
    let file_path = path.join(BUILD_METADATA_FILE_NAME);
    let mut file_content = String::from("");

    file_content.push_str(&format!("{}\n", build_metadata.version));
    file_content.push_str(&format!("{}\n", build_metadata.config_hash));

    fs::write(file_path, file_content).unwrap();
}
