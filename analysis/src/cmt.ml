open Shared_types

let full_for_cmt ~module_name ~package ~uri cmt =
  match Shared.try_read_cmt cmt with
  | None -> None
  | Some infos ->
    let file = Process_cmt.file_for_cmt_infos ~module_name ~uri infos in
    let extra = Process_extra.get_extra ~file ~infos in
    Some {file; extra; package}

let full_for_incremental_cmt ~package ~module_name ~uri =
  if !Cfg.in_incremental_typechecking_mode then
    let path = Uri.to_path uri in
    let incremental_cmt_path =
      package.root_path ^ "/lib/bs/___incremental" ^ "/" ^ module_name
      ^
      match Files.classify_source_file path with
      | Resi -> ".cmti"
      | _ -> ".cmt"
    in
    match full_for_cmt ~module_name ~package ~uri incremental_cmt_path with
    | Some cmt_info ->
      if Debug.verbose () then
        Printf.printf "[cmt] Found incremental cmt: %s\n"
          (Filename.basename incremental_cmt_path);
      Some cmt_info
    | None -> None
  else None

let full_from_module_uri ~package ~module_name ~uri ~paths =
  match full_for_incremental_cmt ~package ~module_name ~uri with
  | Some cmt_info -> Some cmt_info
  | None ->
    let cmt = get_cmt_path ~uri paths in
    full_for_cmt ~module_name ~package ~uri cmt

let full_from_uri ~state ~uri =
  let path = Uri.to_path uri in
  match Packages.get_package ~state ~uri with
  | None -> None
  | Some package -> (
    let module_name =
      Build_system.namespaced_name package.namespace (Find_files.get_name path)
    in
    match full_for_incremental_cmt ~package ~module_name ~uri with
    | Some cmt_info -> Some cmt_info
    | None -> (
      match Hashtbl.find_opt package.paths_for_module module_name with
      | Some paths ->
        let cmt = get_cmt_path ~uri paths in
        full_for_cmt ~module_name ~package ~uri cmt
      | None ->
        if Debug.verbose () then
          prerr_endline ("can't find module " ^ module_name);
        None))

let full_from_module ~package ~module_name =
  Option.bind (Hashtbl.find_opt package.paths_for_module module_name)
  @@ fun paths ->
  let uri = get_uri paths in
  full_from_module_uri ~package ~module_name ~uri ~paths

let fulls_from_module ~package ~module_name =
  match Hashtbl.find_opt package.paths_for_module module_name with
  | None -> []
  | Some paths ->
    let uris = get_uris paths in
    uris
    |> List.filter_map (fun uri ->
           full_from_module_uri ~package ~module_name ~uri ~paths)

let load_full_cmt_from_path ~state ~path =
  let uri = Uri.from_path path in
  full_from_uri ~state ~uri

let load_cmt_infos_from_path ~state ~path =
  let uri = Uri.from_path path in
  match Packages.get_package ~state ~uri with
  | None -> None
  | Some package -> (
    let module_name =
      Build_system.namespaced_name package.namespace (Find_files.get_name path)
    in
    match Hashtbl.find_opt package.paths_for_module module_name with
    | Some paths ->
      let cmt = get_cmt_path ~uri paths in
      Shared.try_read_cmt cmt
    | None -> None)
