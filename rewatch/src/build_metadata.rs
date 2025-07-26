use std::{fs, path::Path};

pub struct BuildMetadata {
    pub version: String,
}

const BUILD_METADATA_FILE_NAME: &str = ".build-metadata";

pub fn read_build_metadata(path: &Path) -> BuildMetadata {
    let file_path = path.join(BUILD_METADATA_FILE_NAME);
    let file_content = fs::read_to_string(file_path).unwrap();
    let file_content = file_content.trim();

    let mut lines = file_content.lines();
    let version = lines.next().unwrap_or_default().to_owned();

    BuildMetadata { version }
}

pub fn write_build_metadata(path: &Path, build_metadata: &BuildMetadata) {
    let file_path = path.join(BUILD_METADATA_FILE_NAME);
    let file_content = build_metadata.version.to_owned();
    fs::write(file_path, file_content).unwrap();
}
