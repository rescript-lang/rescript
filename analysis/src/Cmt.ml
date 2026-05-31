open SharedTypes

let full_for_cmt ~module_name ~package ~uri cmt =
  match Shared.try_read_cmt cmt with
  | None -> None
  | Some infos ->
    let file = ProcessCmt.file_for_cmt_infos ~module_name ~uri infos in
    let extra = ProcessExtra.get_extra ~file ~infos in
    Some {file; extra; package}

let full_from_uri ~uri =
  let path = Uri.to_path uri in
  match Packages.get_package ~uri with
  | None -> None
  | Some package -> (
    let module_name =
      BuildSystem.namespaced_name package.namespace (FindFiles.get_name path)
    in
    let incremental =
      if !Cfg.in_incremental_typechecking_mode then
        let incremental_cmt_path =
          package.root_path ^ "/lib/bs/___incremental" ^ "/" ^ module_name
          ^
          match Files.classify_source_file path with
          | Resi -> ".cmti"
          | _ -> ".cmt"
        in
        full_for_cmt ~module_name ~package ~uri incremental_cmt_path
      else None
    in
    match incremental with
    | Some cmt_info ->
      if Debug.verbose () then Printf.printf "[cmt] Found incremental cmt\n";
      Some cmt_info
    | None -> (
      match Hashtbl.find_opt package.paths_for_module module_name with
      | Some paths ->
        let cmt = get_cmt_path ~uri paths in
        full_for_cmt ~module_name ~package ~uri cmt
      | None ->
        prerr_endline ("can't find module " ^ module_name);
        None))

let fulls_from_module ~package ~module_name =
  if Hashtbl.mem package.paths_for_module module_name then
    let paths = Hashtbl.find package.paths_for_module module_name in
    let uris = get_uris paths in
    uris |> List.filter_map (fun uri -> full_from_uri ~uri)
  else []

let load_full_cmt_from_path ~path =
  let uri = Uri.from_path path in
  full_from_uri ~uri

let load_cmt_infos_from_path ~path =
  let uri = Uri.from_path path in
  match Packages.get_package ~uri with
  | None -> None
  | Some package -> (
    let module_name =
      BuildSystem.namespaced_name package.namespace (FindFiles.get_name path)
    in
    match Hashtbl.find_opt package.paths_for_module module_name with
    | Some paths ->
      let cmt = get_cmt_path ~uri paths in
      Shared.try_read_cmt cmt
    | None -> None)
