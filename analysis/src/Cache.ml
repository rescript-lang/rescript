open SharedTypes

type cached = {
  project_files: FileSet.t;
  dependencies_files: FileSet.t;
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

let target_file_from_lib_bs lib_bs =
  Filename.concat lib_bs ".project-files-cache"

let cache_project (package : package) =
  let cached =
    {
      project_files = package.project_files;
      dependencies_files = package.dependencies_files;
      paths_for_module = package.paths_for_module;
    }
  in
  match BuildSystem.get_lib_bs package.root_path with
  | None -> print_endline "\"ERR\""
  | Some lib_bs ->
    let target_file = target_file_from_lib_bs lib_bs in
    write_cache target_file cached;
    print_endline "\"OK\""
