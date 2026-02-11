use crate::build::packages::Package;
use crate::helpers;
use ahash::AHashMap;
use log::error;
use rayon::prelude::*;
use regex::Regex;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;

use super::packages;

enum Location {
    Bs,
    Ocaml,
}

fn get_log_file_path(package: &packages::Package, subfolder: Location) -> PathBuf {
    let build_folder = match subfolder {
        Location::Bs => package.get_build_path(),
        Location::Ocaml => package.get_ocaml_build_path(),
    };

    build_folder.join(".compiler.log")
}

pub fn does_flat_build_dir_have_artifacts(
    package: &packages::Package,
    build_profile: super::build_types::BuildProfile,
) -> bool {
    let flat_dir = package.get_ocaml_build_path_for_profile(build_profile);
    flat_dir.exists()
        && std::fs::read_dir(&flat_dir)
            .map(|mut entries| entries.next().is_some())
            .unwrap_or(false)
}

fn escape_colours(str: &str) -> String {
    let re = Regex::new(r"[\u001b\u009b]\[[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]")
        .expect("Could not create regex");
    re.replace_all(str, "").to_string()
}

fn write_to_log_file(mut file: File, package_name: &str, content: &str) {
    let res = file.write(escape_colours(content).as_bytes()).map_err(|e| {
        error!(
            "Could not create compiler log file. {}. \n{:?}",
            &package_name, &e
        );
    });

    match res {
        Ok(_) => {}
        Err(e) => error!(
            "Could not create compiler log file. {}. \n{:?}",
            &package_name, &e
        ),
    }
}

pub fn initialize(packages: &AHashMap<String, Package>) {
    packages.par_iter().for_each(|(name, package)| {
        File::create(get_log_file_path(package, Location::Bs))
            .map(|file| write_to_log_file(file, name, &format!("#Start({})\n", helpers::get_system_time())))
            .unwrap_or_else(|err| {
                panic!("Cannot create compiler log for package {name}: {err}");
            });
    })
}

pub fn append(package: &packages::Package, str: &str) {
    let log_path = get_log_file_path(package, Location::Bs);
    // If the log file doesn't exist (e.g. LSP profile skips logs::initialize),
    // silently skip rather than panicking.
    if let Ok(file) = File::options().append(true).open(&log_path) {
        write_to_log_file(file, &package.name, str);
    }
}

pub fn finalize(packages: &AHashMap<String, Package>) {
    packages.par_iter().for_each(|(name, package)| {
        let _ = File::options()
            .append(true)
            .open(get_log_file_path(package, Location::Bs))
            .map(|file| write_to_log_file(file, name, &format!("#Done({})\n", helpers::get_system_time())));

        let _ = std::fs::copy(
            get_log_file_path(package, Location::Bs),
            get_log_file_path(package, Location::Ocaml),
        );
    })
}
