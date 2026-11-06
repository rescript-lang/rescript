open Shared_types

type cached = {
  project_files: File_set.t;
  dependencies_files: File_set.t;
  paths_for_module: (file, paths) Hashtbl.t;
}

let write_cache filename (data : cached) =
  let oc = open_out_bin filename in
  Marshal.to_channel oc data [];
  close_out oc

let read_cache filename =
  if !Cfg.read_project_config_cache && Sys.file_exists filename then
    try
      let ic = open_in_bin filename in
      let data : cached = Marshal.from_channel ic in
      close_in ic;
      Some data
    with _ -> None
  else None

let delete_cache filename = try Sys.remove filename with _ -> ()

let target_file_from_artifact_dir artifact_dir =
  Filename.concat artifact_dir ".project-files-cache"

let cache_project (package : package) =
  let cached =
    {
      project_files = package.project_files;
      dependencies_files = package.dependencies_files;
      paths_for_module = package.paths_for_module;
    }
  in
  match Build_system.get_artifact_dir package.root_path with
  | None -> print_endline "\"ERR\""
  | Some artifact_dir ->
    let target_file = target_file_from_artifact_dir artifact_dir in
    write_cache target_file cached;
    print_endline "\"OK\""
