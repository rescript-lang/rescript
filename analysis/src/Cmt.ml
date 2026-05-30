open SharedTypes

let fullForCmt ~moduleName ~package ~uri cmt =
  match Shared.tryReadCmt cmt with
  | None -> None
  | Some infos ->
    let file = ProcessCmt.fileForCmtInfos ~moduleName ~uri infos in
    let extra = ProcessExtra.getExtra ~file ~infos in
    Some {file; extra; package}

let fullForIncrementalCmt ~package ~moduleName ~uri =
  if !Cfg.inIncrementalTypecheckingMode then
    let path = Uri.toPath uri in
    let incrementalCmtPath =
      package.rootPath ^ "/lib/bs/___incremental" ^ "/" ^ moduleName
      ^
      match Files.classifySourceFile path with
      | Resi -> ".cmti"
      | _ -> ".cmt"
    in
    match fullForCmt ~moduleName ~package ~uri incrementalCmtPath with
    | Some cmtInfo ->
      if Debug.verbose () then Printf.printf "[cmt] Found incremental cmt\n";
      Some cmtInfo
    | None -> None
  else None

let fullFromModuleUri ~package ~moduleName ~uri ~paths =
  match fullForIncrementalCmt ~package ~moduleName ~uri with
  | Some cmtInfo -> Some cmtInfo
  | None ->
    let cmt = getCmtPath ~uri paths in
    fullForCmt ~moduleName ~package ~uri cmt

let fullFromUri ~uri =
  let path = Uri.toPath uri in
  match Packages.getPackage ~uri with
  | None -> None
  | Some package -> (
    let moduleName =
      BuildSystem.namespacedName package.namespace (FindFiles.getName path)
    in
    match fullForIncrementalCmt ~package ~moduleName ~uri with
    | Some cmtInfo -> Some cmtInfo
    | None -> (
      match Hashtbl.find_opt package.pathsForModule moduleName with
      | Some paths ->
        let cmt = getCmtPath ~uri paths in
        fullForCmt ~moduleName ~package ~uri cmt
      | None ->
        prerr_endline ("can't find module " ^ moduleName);
        None))

let fullFromModule ~package ~moduleName =
  Option.bind (Hashtbl.find_opt package.pathsForModule moduleName)
  @@ fun paths ->
  let uri = getUri paths in
  fullFromModuleUri ~package ~moduleName ~uri ~paths

let fullsFromModule ~package ~moduleName =
  match Hashtbl.find_opt package.pathsForModule moduleName with
  | None -> []
  | Some paths ->
    let uris = getUris paths in
    uris
    |> List.filter_map (fun uri ->
           fullFromModuleUri ~package ~moduleName ~uri ~paths)

let loadFullCmtFromPath ~path =
  let uri = Uri.fromPath path in
  fullFromUri ~uri

let loadCmtInfosFromPath ~path =
  let uri = Uri.fromPath path in
  match Packages.getPackage ~uri with
  | None -> None
  | Some package -> (
    let moduleName =
      BuildSystem.namespacedName package.namespace (FindFiles.getName path)
    in
    match Hashtbl.find_opt package.pathsForModule moduleName with
    | Some paths ->
      let cmt = getCmtPath ~uri paths in
      Shared.tryReadCmt cmt
    | None -> None)
