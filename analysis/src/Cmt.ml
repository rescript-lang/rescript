open SharedTypes

module FullCache = struct
  let key : (string, full) Hashtbl.t Domain.DLS.key =
    AnalysisCache.make_hashtbl 64

  let get () : (string, full) Hashtbl.t = AnalysisCache.get_hashtbl key
end

let fullForCmt ~moduleName ~package ~uri cmt =
  match Shared.tryReadCmt cmt with
  | None -> None
  | Some infos ->
    let file = ProcessCmt.fileForCmtInfos ~moduleName ~uri infos in
    let extra = ProcessExtra.getExtra ~file ~infos in
    Some {file; extra; package}

let fullFromUriWithPackage ~package ~uri =
  let path = Uri.toPath uri in
  let moduleName =
    BuildSystem.namespacedName package.namespace (FindFiles.getName path)
  in
  let cached_full cmt_path =
    let cache = FullCache.get () in
    match Hashtbl.find_opt cache cmt_path with
    | Some v -> Some v
    | None -> (
      match fullForCmt ~moduleName ~package ~uri cmt_path with
      | Some v as res ->
        Hashtbl.replace cache cmt_path v;
        res
      | None -> None)
  in
  if !Cfg.inIncrementalTypecheckingMode then
    let incrementalCmtPath =
      package.rootPath ^ "/lib/bs/___incremental" ^ "/" ^ moduleName
      ^
      match Files.classifySourceFile path with
      | Resi -> ".cmti"
      | _ -> ".cmt"
    in
    match cached_full incrementalCmtPath with
    | Some _ as x -> x
    | None -> (
      (* Fallback to non-incremental *)
      match Hashtbl.find_opt package.pathsForModule moduleName with
      | Some paths ->
        let cmt = getCmtPath ~uri paths in
        cached_full cmt
      | None ->
        prerr_endline ("can't find module " ^ moduleName);
        None)
  else
    match Hashtbl.find_opt package.pathsForModule moduleName with
    | Some paths ->
      let cmt = getCmtPath ~uri paths in
      cached_full cmt
    | None ->
      prerr_endline ("can't find module " ^ moduleName);
      None

let fullFromUri ~uri =
  match Packages.getPackage ~uri with
  | None -> None
  | Some package -> fullFromUriWithPackage ~package ~uri

let fullsFromModule ~package ~moduleName =
  match Hashtbl.find_opt package.pathsForModule moduleName with
  | None -> []
  | Some paths ->
    let uris = getUris paths in
    uris
    |> List.filter_map (fun uri ->
           let cmt = getCmtPath ~uri paths in
           fullForCmt ~moduleName ~package ~uri cmt)

let loadFullCmtFromPath ~path =
  let uri = Uri.fromPath path in
  fullFromUri ~uri
