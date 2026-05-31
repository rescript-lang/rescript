open SharedTypes

type resolution =
  | Exported of QueryEnv.t * file_path
  | Global of file_path * file_path list
  | GlobalMod of file_path
  | NotFound
  | Stamp of int

let rec join_paths module_path path =
  match module_path with
  | Path.Pident ident -> (ident.stamp, ident.name, path)
  | Papply (fn_path, _argPath) -> join_paths fn_path path
  | Pdot (inner, name, _) -> join_paths inner (name :: path)

let rec make_path ~(env : QueryEnv.t) module_path =
  match module_path with
  | Path.Pident ident when ident.stamp == 0 -> GlobalMod ident.name
  | Pident ident -> Stamp ident.stamp
  | Papply (fn_path, _argPath) -> make_path ~env fn_path
  | Pdot (inner, name, _) -> (
    match join_paths inner [name] with
    | 0, module_name, path -> Global (module_name, path)
    | stamp, _moduleName, path -> (
      let res =
        match Stamps.find_module env.file.stamps stamp with
        | None -> None
        | Some {item = kind} -> find_in_module ~env kind path
      in
      match res with
      | None -> NotFound
      | Some (`Local (env, name)) -> Exported (env, name)
      | Some (`Global (module_name, full_path)) -> Global (module_name, full_path)))

and resolve_path_inner ~(env : QueryEnv.t) ~path =
  match path with
  | [] -> None
  | [name] -> Some (`Local (env, name))
  | sub_name :: sub_path -> (
    match Exported.find env.exported Exported.Module sub_name with
    | None -> None
    | Some stamp -> (
      match Stamps.find_module env.file.stamps stamp with
      | None -> None
      | Some {item} -> find_in_module ~env item sub_path))

and find_in_module ~(env : QueryEnv.t) module_ path =
  match module_ with
  | Structure structure ->
    resolve_path_inner ~env:(QueryEnv.enter_structure env structure) ~path
  | Constraint (_, module1) -> find_in_module ~env module1 path
  | Ident module_path -> (
    let stamp, module_name, full_path = join_paths module_path path in
    if stamp = 0 then Some (`Global (module_name, full_path))
    else
      match Stamps.find_module env.file.stamps stamp with
      | None -> None
      | Some {item} -> find_in_module ~env item full_path)

let rec resolve_path ~env ~path ~package =
  Log.log ("resolvePath path:" ^ path_to_string path);
  match resolve_path_inner ~env ~path with
  | None -> None
  | Some result -> (
    match result with
    | `Local (env, name) -> Some (env, name)
    | `Global (module_name, full_path) -> (
      Log.log
        ("resolvePath Global path:" ^ path_to_string full_path ^ " module:"
       ^ module_name);
      match ProcessCmt.file_for_module ~package module_name with
      | None -> None
      | Some file ->
        resolve_path ~env:(QueryEnv.from_file file) ~path:full_path ~package))

let from_compiler_path ~(env : QueryEnv.t) path : resolution =
  match make_path ~env path with
  | Stamp stamp -> Stamp stamp
  | GlobalMod name -> GlobalMod name
  | NotFound -> NotFound
  | Exported (env, name) -> Exported (env, name)
  | Global (module_name, full_path) -> Global (module_name, full_path)

let resolve_module_from_compiler_path ~env ~package path =
  match from_compiler_path ~env path with
  | Global (module_name, path) -> (
    match ProcessCmt.file_for_module ~package module_name with
    | None -> None
    | Some file -> (
      let env = QueryEnv.from_file file in
      match resolve_path ~env ~package ~path with
      | None -> None
      | Some (env, name) -> (
        match Exported.find env.exported Exported.Module name with
        | None -> None
        | Some stamp -> (
          match Stamps.find_module env.file.stamps stamp with
          | None -> None
          | Some declared -> Some (env, Some declared)))))
  | Stamp stamp -> (
    match Stamps.find_module env.file.stamps stamp with
    | None -> None
    | Some declared -> Some (env, Some declared))
  | GlobalMod module_name -> (
    match ProcessCmt.file_for_module ~package module_name with
    | None -> None
    | Some file ->
      let env = QueryEnv.from_file file in
      Some (env, None))
  | NotFound -> None
  | Exported (env, name) -> (
    match Exported.find env.exported Exported.Module name with
    | None -> None
    | Some stamp -> (
      match Stamps.find_module env.file.stamps stamp with
      | None -> None
      | Some declared -> Some (env, Some declared)))

let resolve_from_compiler_path ~env ~package path =
  match from_compiler_path ~env path with
  | Global (module_name, path) -> (
    let res =
      match ProcessCmt.file_for_module ~package module_name with
      | None -> None
      | Some file ->
        let env = QueryEnv.from_file file in
        resolve_path ~env ~package ~path
    in
    match res with
    | None -> NotFound
    | Some (env, name) -> Exported (env, name))
  | Stamp stamp -> Stamp stamp
  | GlobalMod _ -> NotFound
  | NotFound -> NotFound
  | Exported (env, name) -> Exported (env, name)

let rec get_source_uri ~(env : QueryEnv.t) ~package (path : ModulePath.t) =
  match path with
  | File (uri, _moduleName) -> uri
  | NotVisible -> env.file.uri
  | IncludedModule (path, inner) -> (
    Log.log "INCLUDED MODULE";
    match resolve_module_from_compiler_path ~env ~package path with
    | None ->
      Log.log "NOT FOUND";
      get_source_uri ~env ~package inner
    | Some (env, _declared) -> env.file.uri)
  | ExportedModule {module_path = inner} -> get_source_uri ~env ~package inner
