open SharedTypes

type cached = {
  projectFiles: FileSet.t;
  dependenciesFiles: FileSet.t;
  pathsForModule: (file, paths) Hashtbl.t;
}

let writeCache filename (data : cached) =
  let oc = open_out_bin filename in
  Marshal.to_channel oc data [];
  close_out oc

let readCache filename =
  if !Cfg.readProjectConfigCache && Sys.file_exists filename then
    try
      let ic = open_in_bin filename in
      let data : cached = Marshal.from_channel ic in
      close_in ic;
      Some data
    with _ -> None
  else None

let deleteCache filename = try Sys.remove filename with _ -> ()

let targetFileFromLibBs libBs = Filename.concat libBs ".project-files-cache"

let cacheProject (package : package) =
  let cached =
    {
      projectFiles = package.projectFiles;
      dependenciesFiles = package.dependenciesFiles;
      pathsForModule = package.pathsForModule;
    }
  in
  match BuildSystem.getLibBs package.rootPath with
  | None -> print_endline "\"ERR\""
  | Some libBs ->
    let targetFile = targetFileFromLibBs libBs in
    writeCache targetFile cached;
    print_endline "\"OK\""
