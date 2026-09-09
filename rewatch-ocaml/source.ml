type module_ = {
  name: string;
  implementation: string;
  interface: string option;
  is_dev: bool;
  feature: string option;
  mutable deps: string list;
}

type discovery = {
  modules: module_ list;
  inventory_files: string list;
  gentype_dirs: string list;
}

exception Error of string

let source_extension path =
  match Filename.extension path with
  | ".res" -> Some false
  | ".resi" -> Some true
  | _ -> None

let module_name path =
  path |> Filename.basename |> Filename.remove_extension
  |> String.capitalize_ascii

let display_path ~display_root root path =
  let absolute =
    if Filename.is_relative path then Filename.concat root path else path
  in
  let display_root = Unix.realpath display_root in
  let prefix = Filename.concat display_root "" in
  let comparable = Platform.normalize_path_for_comparison in
  if String.starts_with ~prefix:(comparable prefix) (comparable absolute) then
    String.sub absolute (String.length prefix)
      (String.length absolute - String.length prefix)
  else absolute

let duplicate_error ~display_root root name first second =
  let first, second =
    let paths =
      [
        display_path ~display_root root first;
        display_path ~display_root root second;
      ]
      |> List.sort String.compare
    in
    match paths with [first; second] -> (first, second) | _ -> assert false
  in
  Error
    (Printf.sprintf
       "Could not initialize build: Duplicate module name: %s. Found in %s and %s. Rename one of these files."
       name first second)

let interface_mismatch_error implementation interface =
  Error
    (Printf.sprintf
       "Could not initialize build: Implementation and interface have different path names or different cases: `%s` vs `%s`"
       implementation interface)

(* A package source tree has three consumers with deliberately different
   recursion rules. Compilation follows directory links and honors source
   activation/subdirs; cleanup inventories every real descendant but treats
   links as leaves; GenType records every configured directory that its
   subdirs setting reaches. Keeping the views in one walk matches Rust's
   package-state ownership without weakening stale-output cleanup. *)
let scan_source ~root (source : Config.source) ~discover_modules ~on_missing
    ~visited_dirs ~collect_gentype ~visited_gentype_dirs candidates
    inventory_files gentype_dirs =
  let rec scan_directory ~relative ~collect_inventory ~discover_requested
      ~collect_gentype =
    let absolute = Filename.concat root relative in
    let canonical =
      if not (discover_requested || collect_gentype) then None
      else
        try
          Some (Unix.realpath absolute)
        with Sys_error _ | Unix.Unix_error _ ->
          if discover_requested then on_missing absolute;
          None
    in
    let discover_here =
      match canonical with
      | Some canonical
        when discover_requested && not (Hashtbl.mem visited_dirs canonical) ->
        Hashtbl.add visited_dirs canonical ();
        true
      | None | Some _ -> false
    in
    let gentype_here =
      match canonical with
      | Some canonical
        when collect_gentype
             && not (Hashtbl.mem visited_gentype_dirs canonical) ->
        Hashtbl.add visited_gentype_dirs canonical ();
        gentype_dirs := relative :: !gentype_dirs;
        true
      | None | Some _ -> false
    in
    let entries =
      try Sys.readdir absolute |> Array.to_list |> List.sort String.compare
      with Sys_error _ | Unix.Unix_error _ ->
        if discover_here then on_missing absolute;
        []
    in
    List.iter
      (fun name ->
        let relative_path = Filename.concat relative name in
        let absolute_path = Filename.concat root relative_path in
        try
          match (Unix.lstat absolute_path).Unix.st_kind with
          | Unix.S_DIR ->
            scan_directory ~relative:relative_path ~collect_inventory
              ~discover_requested:(discover_here && source.recurse)
              ~collect_gentype:(gentype_here && source.recurse)
          | Unix.S_LNK -> (
            match (Unix.stat absolute_path).Unix.st_kind with
            | Unix.S_DIR ->
              if collect_inventory then
                inventory_files := absolute_path :: !inventory_files;
              if
                (discover_here || gentype_here) && source.recurse
              then
                scan_directory ~relative:relative_path ~collect_inventory:false
                  ~discover_requested:discover_here
                  ~collect_gentype:gentype_here
            | _ ->
              if collect_inventory then
                inventory_files := absolute_path :: !inventory_files;
              if discover_here then
                match source_extension name with
                | None -> ()
                | Some is_interface ->
                  candidates :=
                    (relative_path, is_interface, source.is_dev) :: !candidates)
          | _ ->
            if collect_inventory then
              inventory_files := absolute_path :: !inventory_files;
            if discover_here then
              match source_extension name with
              | None -> ()
              | Some is_interface ->
                candidates :=
                  (relative_path, is_interface, source.is_dev) :: !candidates
        with Sys_error _ | Unix.Unix_error _ -> ())
      entries
  in
  let relative = source.dir in
  let absolute = Filename.concat root relative in
  try
    match (Unix.lstat absolute).Unix.st_kind with
    | Unix.S_DIR ->
      scan_directory ~relative ~collect_inventory:true
        ~discover_requested:discover_modules
        ~collect_gentype
    | Unix.S_LNK -> (
      match (Unix.stat absolute).Unix.st_kind with
      | Unix.S_DIR ->
        inventory_files := absolute :: !inventory_files;
        scan_directory ~relative ~collect_inventory:false
          ~discover_requested:discover_modules
          ~collect_gentype
      | _ ->
        inventory_files := absolute :: !inventory_files;
        if discover_modules then on_missing absolute)
    | _ ->
      inventory_files := absolute :: !inventory_files;
      if discover_modules then on_missing absolute
  with Sys_error _ | Unix.Unix_error _ ->
    if discover_modules then on_missing absolute

let discover_with_inventory ?(on_orphan = fun _ -> ())
    ?(on_missing = fun path ->
      Printf.eprintf "Could not read folder %s\n%!" path)
    ?(display_root = Sys.getcwd ()) (config : Config.t) ~prod ~features ~filter =
  let matches_filter =
    match filter with
    | None -> fun _ -> true
    | Some pattern ->
      let regex = try Str.regexp pattern with Failure _ -> raise (Error ("invalid filter regex: " ^ pattern)) in
      fun path -> try ignore (Str.search_forward regex path 0); true with Not_found -> false
  in
  let active_features = Hashtbl.create 16 in
  let raise_feature_cycle feature visiting =
    let chain = List.rev (feature :: visiting) |> String.concat " -> " in
    raise (Error ("Cycle detected in `features` map: " ^ chain))
  in
  let rec validate_feature feature visiting =
    if List.mem feature visiting then
      raise_feature_cycle feature visiting;
    match List.assoc_opt feature config.features with
    | None -> ()
    | Some implied -> List.iter (fun name -> validate_feature name (feature :: visiting)) implied
  in
  List.iter (fun (name, _) -> validate_feature name []) config.features;
  let rec activate feature visiting =
    if List.mem feature visiting then
      raise_feature_cycle feature visiting;
    if not (Hashtbl.mem active_features feature) then (
      Hashtbl.add active_features feature ();
      match List.assoc_opt feature config.features with
      | None -> ()
      | Some implied -> List.iter (fun name -> activate name (feature :: visiting)) implied)
  in
  List.iter (fun feature -> activate feature []) (Option.value features ~default:[]);
  let all_features = features = None in
  let visited_dirs = Hashtbl.create 32 in
  let visited_gentype_dirs = Hashtbl.create 32 in
  let files = ref [] in
  let inventory_files = ref [] in
  let gentype_dirs = ref [] in
  config.sources
  |> List.iter (fun (source : Config.source) ->
       let feature_enabled =
         all_features
         || Option.fold ~none:true
              ~some:(fun feature -> Hashtbl.mem active_features feature)
              source.feature
       in
       let discover_modules =
         not (prod && source.is_dev) && feature_enabled
       in
       scan_source ~root:config.root source ~discover_modules ~on_missing
         ~visited_dirs
         ~collect_gentype:(config.gentype_args <> [] && feature_enabled)
         ~visited_gentype_dirs files inventory_files gentype_dirs);
  let files = !files in
  let table = Hashtbl.create (List.length files) in
  List.iter
    (fun (path, is_interface, is_dev) ->
      let name = module_name path in
      let implementation, interface, old_dev =
        match Hashtbl.find_opt table name with
        | None -> (None, None, is_dev)
        | Some values -> values
      in
      if is_interface then
        match interface with
        | Some previous ->
          raise
            (duplicate_error ~display_root config.root name previous path)
        | None ->
          Hashtbl.replace table name
            (implementation, Some path, old_dev || is_dev)
      else
        match implementation with
        | Some previous ->
          raise
            (duplicate_error ~display_root config.root name previous path)
        | None ->
          Hashtbl.replace table name (Some path, interface, old_dev || is_dev))
    (List.filter (fun (path, _, _) -> matches_filter path) files);
  Hashtbl.iter
    (fun _ (implementation, interface, _) ->
      match implementation, interface with
      | Some implementation, Some interface
        when Filename.remove_extension implementation
             <> Filename.remove_extension interface ->
        raise (interface_mismatch_error implementation interface)
      | _ -> ())
    table;
  Hashtbl.to_seq table
  |> Seq.filter_map (fun (_, (implementation, interface, _)) ->
       match implementation, interface with
       | None, Some interface -> Some interface
       | _ -> None)
  |> List.of_seq |> List.sort String.compare |> List.iter on_orphan;
  let modules =
    Hashtbl.to_seq table
    |> Seq.filter_map (fun (name, (implementation, interface, is_dev)) ->
         match implementation with
         | None -> None
         | Some implementation ->
           Some
             {name; implementation; interface; is_dev; feature = None; deps = []})
    |> List.of_seq
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  {
    modules;
    inventory_files = List.sort_uniq String.compare !inventory_files;
    gentype_dirs = List.sort_uniq String.compare !gentype_dirs;
  }

let discover ?on_orphan ?on_missing ?display_root config ~prod ~features ~filter
    =
  (discover_with_inventory ?on_orphan ?on_missing ?display_root config ~prod
     ~features ~filter)
    .modules

let ast_path path =
  Filename.remove_extension path
  ^ if Filename.extension path = ".resi" then ".iast" else ".ast"

let compiler_basename config module_name =
  match config.Config.namespace, config.namespace_entry with
  | Some _, Some entry when entry = module_name -> module_name
  | Some namespace, Some _ -> module_name ^ "-@" ^ namespace
  | Some namespace, _ -> module_name ^ "-" ^ namespace
  | None, _ -> module_name

(* Compiler artifacts preserve the source filename's case, while dependency
   graph module names are capitalized. Keep those two names distinct. *)
let compiler_asset_basename config path =
  let basename =
    path |> Filename.basename |> Filename.remove_extension
  in
  match config.Config.namespace, config.namespace_entry with
  | Some _, Some entry when entry = module_name path -> basename
  | Some namespace, Some _ -> basename ^ "-@" ^ namespace
  | Some namespace, _ -> basename ^ "-" ^ namespace
  | None, _ -> basename
