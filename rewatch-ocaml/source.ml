type module_ = {
  name: string;
  implementation: string;
  interface: string option;
  is_dev: bool;
}

type discovery = {
  modules: module_ list;
  source_mtimes: (string * float) list;
  inventory_files: string list;
  present_files: string list;
  gentype_dirs: string list;
}

type discovered_file = {path: string; modified: float}
type source_kind = Implementation | Interface

type scanned_file = {file: discovered_file; kind: source_kind; is_dev: bool}

type scanned_sources = {
  files: scanned_file list;
  inventory_files: string list;
  present_files: string list;
  gentype_dirs: string list;
}

exception Error of string

let source_kind path =
  match Filename.extension path with
  | ".res" -> Some Implementation
  | ".resi" -> Some Interface
  | _ -> None

let module_name path =
  path |> Filename.basename |> Filename.remove_extension
  |> String.capitalize_ascii

let is_non_exotic_module_name name =
  let is_ascii_uppercase = function
    | 'A' .. 'Z' -> true
    | _ -> false
  in
  let is_ascii_alphanumeric = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
    | _ -> false
  in
  let rec valid_tail index =
    if index = String.length name then true
    else
      let character = name.[index] in
      (is_ascii_alphanumeric character || character = '_')
      && valid_tail (index + 1)
  in
  String.length name > 0 && is_ascii_uppercase name.[0] && valid_tail 1

let display_path ~display_root root path =
  let absolute =
    if Filename.is_relative path then Filename.concat root path else path
  in
  let display_root = Platform.canonicalize_path display_root in
  Project_context.relative_or_absolute ~root:display_root absolute

let duplicate_error ~display_root root name first second =
  let first, second =
    let first = display_path ~display_root root first in
    let second = display_path ~display_root root second in
    if String.compare first second <= 0 then (first, second) else (second, first)
  in
  Error
    (Printf.sprintf
       "Could not initialize build: Duplicate module name: %s. Found in %s and \
        %s. Rename one of these files."
       name first second)

let interface_mismatch_error implementation interface =
  Error
    (Printf.sprintf
       "Could not initialize build: Implementation and interface have \
        different path names or different cases: `%s` vs `%s`"
       implementation interface)

(* A package source tree has three consumers with deliberately different
   recursion rules. Compilation follows directory links and honors source
   activation/subdirs; cleanup inventories every real descendant but treats
   links as leaves; GenType records every configured directory that its
   subdirs setting reaches. Keeping the views in one walk gives every consumer
   the same filesystem snapshot without weakening stale-output cleanup. *)
let scan_source ~root (source : Config.source) ~discover_modules ~on_missing
    ~visited_dirs ~collect_inventory ~collect_gentype ~visited_gentype_dirs
    candidates inventory_files present_files gentype_dirs =
  let rec scan_directory ~relative ~collect_inventory ~discover_requested
      ~collect_gentype ~identity =
    let absolute = Filename.concat root relative in
    let coverage table requested =
      if not requested then (false, false)
      else
        match Hashtbl.find_opt table identity with
        | None ->
          Hashtbl.add table identity source.recurse;
          (true, source.recurse)
        | Some true -> (false, false)
        | Some false when source.recurse ->
          Hashtbl.replace table identity true;
          (false, true)
        | Some false -> (false, false)
    in
    let discover_here, discover_children =
      coverage visited_dirs discover_requested
    in
    let gentype_here, gentype_children =
      coverage visited_gentype_dirs collect_gentype
    in
    if gentype_here then gentype_dirs := relative :: !gentype_dirs;
    let entries =
      try Sys.readdir absolute |> Array.to_list |> List.sort String.compare
      with error ->
        if File_util.path_is_missing absolute then (
          if discover_here || discover_children then on_missing absolute;
          [])
        else raise error
    in
    List.iter
      (fun name ->
        let relative_path = Filename.concat relative name in
        let absolute_path = Filename.concat root relative_path in
        try
          let metadata = Unix.lstat absolute_path in
          match metadata.Unix.st_kind with
          | Unix.S_DIR ->
            if collect_inventory || discover_children || gentype_children then
              let identity =
                Platform.directory_identity ~path:absolute_path metadata
              in
              scan_directory ~relative:relative_path ~collect_inventory
                ~discover_requested:discover_children
                ~collect_gentype:gentype_children ~identity
          | Unix.S_LNK -> (
            let target_metadata = Unix.stat absolute_path in
            match target_metadata.Unix.st_kind with
            | Unix.S_DIR ->
              if collect_inventory then
                inventory_files := absolute_path :: !inventory_files;
              if discover_children || gentype_children then
                let identity =
                  Platform.directory_identity ~path:absolute_path
                    target_metadata
                in
                scan_directory ~relative:relative_path ~collect_inventory:false
                  ~discover_requested:discover_children
                  ~collect_gentype:gentype_children ~identity
            | _ -> (
              present_files := absolute_path :: !present_files;
              if collect_inventory then
                inventory_files := absolute_path :: !inventory_files;
              if discover_here then
                match source_kind name with
                | None -> ()
                | Some kind ->
                  candidates :=
                    {
                      file =
                        {
                          path = relative_path;
                          modified = target_metadata.Unix.st_mtime;
                        };
                      kind;
                      is_dev = source.is_dev;
                    }
                    :: !candidates))
          | _ -> (
            present_files := absolute_path :: !present_files;
            if collect_inventory then
              inventory_files := absolute_path :: !inventory_files;
            if discover_here then
              match source_kind name with
              | None -> ()
              | Some kind ->
                candidates :=
                  {
                    file =
                      {path = relative_path; modified = metadata.Unix.st_mtime};
                    kind;
                    is_dev = source.is_dev;
                  }
                  :: !candidates)
        with error ->
          if File_util.path_is_missing absolute_path then () else raise error)
      entries
  in
  let relative = source.dir in
  let absolute = Filename.concat root relative in
  try
    let metadata = Unix.lstat absolute in
    match metadata.Unix.st_kind with
    | Unix.S_DIR ->
      let identity = Platform.directory_identity ~path:absolute metadata in
      scan_directory ~relative ~collect_inventory
        ~discover_requested:discover_modules ~collect_gentype ~identity
    | Unix.S_LNK -> (
      let target_metadata = Unix.stat absolute in
      match target_metadata.Unix.st_kind with
      | Unix.S_DIR ->
        inventory_files := absolute :: !inventory_files;
        let identity =
          Platform.directory_identity ~path:absolute target_metadata
        in
        scan_directory ~relative ~collect_inventory:false
          ~discover_requested:discover_modules ~collect_gentype ~identity
      | _ ->
        present_files := absolute :: !present_files;
        inventory_files := absolute :: !inventory_files;
        if discover_modules then on_missing absolute)
    | _ ->
      present_files := absolute :: !present_files;
      inventory_files := absolute :: !inventory_files;
      if discover_modules then on_missing absolute
  with error ->
    if File_util.path_is_missing absolute then (
      if discover_modules then on_missing absolute)
    else raise error

let resolve_active_features (config : Config.t) requested =
  let active_features = Hashtbl.create 16 in
  let raise_feature_cycle feature visiting =
    let chain = List.rev (feature :: visiting) |> String.concat " -> " in
    raise (Error ("Cycle detected in `features` map: " ^ chain))
  in
  let rec activate feature visiting =
    if List.mem feature visiting then raise_feature_cycle feature visiting;
    if not (Hashtbl.mem active_features feature) then (
      Hashtbl.add active_features feature ();
      match List.assoc_opt feature config.features with
      | None -> ()
      | Some implied ->
        List.iter (fun name -> activate name (feature :: visiting)) implied)
  in
  List.iter (fun feature -> activate feature []) requested;
  active_features

let source_is_active ~prod ~all_features ~active_features
    (source : Config.source) =
  let feature_enabled =
    all_features
    || Option.fold ~none:true
         ~some:(fun feature -> Hashtbl.mem active_features feature)
         source.feature
  in
  (not (prod && source.is_dev)) && feature_enabled

let active_sources (config : Config.t) ~prod ~features =
  let active_features =
    resolve_active_features config (Option.value features ~default:[])
  in
  let all_features = Option.is_none features in
  List.filter
    (source_is_active ~prod ~all_features ~active_features)
    config.sources

let scan_sources ~on_missing (config : Config.t) ~prod ~features
    ~collect_inventory ~collect_gentype =
  let active_features =
    resolve_active_features config (Option.value features ~default:[])
  in
  let all_features = features = None in
  let visited_dirs = Hashtbl.create 32 in
  let visited_gentype_dirs = Hashtbl.create 32 in
  let files = ref [] in
  let inventory_files = ref [] in
  let present_files = ref [] in
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
        source_is_active ~prod ~all_features ~active_features source
      in
      scan_source ~root:config.root source ~discover_modules ~on_missing
        ~visited_dirs ~collect_inventory
        ~collect_gentype:(collect_gentype && feature_enabled)
        ~visited_gentype_dirs files inventory_files present_files gentype_dirs);
  {
    files = !files;
    inventory_files = List.sort_uniq String.compare !inventory_files;
    present_files = List.sort_uniq String.compare !present_files;
    gentype_dirs = List.sort_uniq String.compare !gentype_dirs;
  }

let discover_for_cleanup
    ?(on_missing =
      fun path -> Printf.eprintf "Could not read folder %s\n%!" path)
    (config : Config.t) ~prod =
  let scanned =
    scan_sources ~on_missing config ~prod ~features:None
      ~collect_inventory:false ~collect_gentype:false
  in
  let implementations =
    scanned.files
    |> List.filter_map (fun source ->
        match source.kind with
        | Interface -> None
        | Implementation -> Some source.file.path)
    |> List.sort_uniq String.compare
  in
  implementations

let discover_files
    ?(on_missing =
      fun path -> Printf.eprintf "Could not read folder %s\n%!" path)
    (config : Config.t) ~prod ~features ~filter =
  let matches_filter =
    match filter with
    | None -> fun _ -> true
    | Some filter -> Source_filter.matches_basename filter
  in
  let scanned =
    scan_sources ~on_missing config ~prod ~features ~collect_inventory:false
      ~collect_gentype:false
  in
  scanned.files
  |> List.filter_map (fun source ->
      if matches_filter source.file.path then Some source.file.path else None)
  |> List.sort_uniq String.compare

let discover_with_inventory ?(on_orphan = fun _ -> ())
    ?(on_missing =
      fun path -> Printf.eprintf "Could not read folder %s\n%!" path)
    ?(display_root = Sys.getcwd ()) (config : Config.t) ~prod ~features ~filter
    =
  let matches_filter =
    match filter with
    | None -> fun _ -> true
    | Some filter -> Source_filter.matches_basename filter
  in
  let scanned =
    scan_sources ~on_missing config ~prod ~features ~collect_inventory:true
      ~collect_gentype:(config.gentype_args <> [])
  in
  let files = scanned.files in
  let table = Hashtbl.create (List.length files) in
  List.iter
    (fun source ->
      let file = source.file in
      let is_dev = source.is_dev in
      let name = module_name file.path in
      let implementation, interface, old_dev =
        match Hashtbl.find_opt table name with
        | None -> (None, None, is_dev)
        | Some values -> values
      in
      match source.kind with
      | Interface -> (
        match interface with
        | Some previous ->
          raise
            (duplicate_error ~display_root config.root name previous.path
               file.path)
        | None ->
          Hashtbl.replace table name
            (implementation, Some file, old_dev || is_dev))
      | Implementation -> (
        match implementation with
        | Some previous ->
          raise
            (duplicate_error ~display_root config.root name previous.path
               file.path)
        | None ->
          Hashtbl.replace table name (Some file, interface, old_dev || is_dev)))
    (List.filter (fun source -> matches_filter source.file.path) files);
  Hashtbl.iter
    (fun _ (implementation, interface, _) ->
      match (implementation, interface) with
      | Some implementation, Some interface
        when Filename.remove_extension implementation.path
             <> Filename.remove_extension interface.path ->
        raise (interface_mismatch_error implementation.path interface.path)
      | _ -> ())
    table;
  Hashtbl.to_seq table
  |> Seq.filter_map (fun (_, (implementation, interface, _)) ->
      match (implementation, interface) with
      | None, Some interface -> Some interface.path
      | _ -> None)
  |> List.of_seq |> List.sort String.compare |> List.iter on_orphan;
  let modules =
    Hashtbl.to_seq table
    |> Seq.filter_map (fun (name, (implementation, interface, is_dev)) ->
        match implementation with
        | None -> None
        | Some implementation ->
          Some
            {
              name;
              implementation = implementation.path;
              interface = Option.map (fun file -> file.path) interface;
              is_dev;
            })
    |> List.of_seq
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  let source_mtimes =
    Hashtbl.to_seq_values table
    |> Seq.flat_map (fun (implementation, interface, _) ->
        List.to_seq (Option.to_list implementation @ Option.to_list interface))
    |> Seq.map (fun file -> (file.path, file.modified))
    |> List.of_seq
  in
  {
    modules;
    source_mtimes;
    inventory_files = scanned.inventory_files;
    present_files = scanned.present_files;
    gentype_dirs = scanned.gentype_dirs;
  }

let ast_path path =
  Filename.remove_extension path
  ^ if Filename.extension path = ".resi" then ".iast" else ".ast"

let compiler_basename config module_name =
  Config.namespaced_module_name config.Config.namespace module_name

(* Compiler artifacts preserve the source filename's case, while dependency
   graph module names are capitalized. Keep those two names distinct. *)
let compiler_asset_basename config path =
  let basename = path |> Filename.basename |> Filename.remove_extension in
  match config.Config.namespace with
  | Config.No_namespace -> basename
  | Config.Namespace namespace -> basename ^ "-" ^ namespace
  | Config.Namespace_with_entry {name; entry} ->
    if entry = module_name path then basename else basename ^ "-@" ^ name
