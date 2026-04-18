use crate::build::build_types::BuildCommandState;
use ahash::AHashSet;
use anyhow::{Result, anyhow};
use serde::Serialize;
use std::fs::File;
use std::io::Write;
use std::path::Path;

#[derive(Serialize)]
struct Module<'a> {
    pub name: &'a str,
    pub deps: &'a AHashSet<String>,
    pub dependents: &'a AHashSet<String>,
}

pub fn flush(build_state: &BuildCommandState, folder: &str) -> Result<()> {
    println!("Flushing build state");
    let state = build_state
        .build_state
        .modules
        .iter()
        .map(|(name, module)| Module {
            name,
            deps: &module.deps,
            dependents: &module.dependents,
        })
        .collect::<Vec<_>>();

    let json = serde_json::to_string(&state)?;
    println!("Flushing build state json {:?}", json);

    let project_folder = Path::new(folder);
    if !project_folder.exists() {
        eprintln!("Build folder does not exist");
        return Err(anyhow!("Build folder does not exist"));
    }

    let lib_dir = project_folder.join("lib");
    let location = lib_dir.join("build.json");

    let mut file = File::create(&location)?;
    file.write_all(&json.into_bytes())?;

    Ok(())
}
