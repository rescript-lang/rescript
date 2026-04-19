open GenTypeCommon
module ModuleNameMap = Map.Make (ModuleName)

let ( +++ ) = Filename.concat

(** Read all the dirs from a library in node_modules *)
let read_bs_dependencies_dirs ~root =
  let dirs = ref [] in
  let rec find_sub_dirs dir =
    let abs_dir =
      match dir = "" with
      | true -> root
      | false -> root +++ dir
    in
    if Sys.file_exists abs_dir && Sys.is_directory abs_dir then (
      dirs := dir :: !dirs;
      abs_dir |> Sys.readdir |> Array.iter (fun d -> find_sub_dirs (dir +++ d)))
  in
  find_sub_dirs "";
  !dirs

(** Build a map of source filenames (with the given extensions) back to the
    directory where they belong. Source directories and dependency install
    paths are provided by the build system through CLI flags. *)
let sourcedirs_to_map ~(config : Config.t) ~extensions ~exclude_file =
  let rec chop_extensions fname =
    match fname |> Filename.chop_extension with
    | fname_chopped -> fname_chopped |> chop_extensions
    | exception _ -> fname
  in
  let file_map = ref ModuleNameMap.empty in
  let bs_dependencies_file_map = ref ModuleNameMap.empty in
  let filter_given_extension file_name =
    extensions |> List.exists (fun ext -> Filename.check_suffix file_name ext)
    && not (exclude_file file_name)
  in
  let add_dir ~dir_on_disk ~dir_emitted ~filter ~map =
    dir_on_disk |> Sys.readdir
    |> Array.iter (fun fname ->
           if fname |> filter then
             map :=
               !map
               |> ModuleNameMap.add
                    (fname |> chop_extensions |> ModuleName.from_string_unsafe)
                    dir_emitted)
  in
  config.sources
  |> List.iter (fun dir ->
         let dir_on_disk = config.project_root +++ dir in
         if Sys.file_exists dir_on_disk && Sys.is_directory dir_on_disk then
           add_dir ~dir_emitted:dir ~dir_on_disk ~filter:filter_given_extension
             ~map:file_map);
  config.bs_dependencies
  |> List.iter (fun package_name ->
         match Hashtbl.find config.dep_paths package_name with
         | path ->
           let root = ["lib"; "bs"] |> List.fold_left ( +++ ) path in
           let filter file_name =
             [".cmt"; ".cmti"]
             |> List.exists (fun ext -> Filename.check_suffix file_name ext)
           in
           read_bs_dependencies_dirs ~root
           |> List.iter (fun dir ->
                  let dir_on_disk = root +++ dir in
                  let dir_emitted = package_name +++ dir in
                  add_dir ~dir_emitted ~dir_on_disk ~filter
                    ~map:bs_dependencies_file_map)
         | exception Not_found -> ());
  (!file_map, !bs_dependencies_file_map)

type case = Lowercase | Uppercase

type resolver = {
  lazy_find:
    (use_bs_dependencies:bool -> ModuleName.t -> (string * case * bool) option)
    Lazy.t;
}

let create_lazy_resolver ~config ~extensions ~exclude_file =
  {
    lazy_find =
      lazy
        (let module_name_map, bs_dependencies_file_map =
           sourcedirs_to_map ~config ~extensions ~exclude_file
         in
         let find ~bs_dependencies ~map module_name =
           match map |> ModuleNameMap.find module_name with
           | resolved_module_dir ->
             Some (resolved_module_dir, Uppercase, bs_dependencies)
           | exception Not_found -> (
             match
               map |> ModuleNameMap.find (module_name |> ModuleName.uncapitalize)
             with
             | resolved_module_dir ->
               Some (resolved_module_dir, Lowercase, bs_dependencies)
             | exception Not_found -> None)
         in
         fun ~use_bs_dependencies module_name ->
           match
             module_name |> find ~bs_dependencies:false ~map:module_name_map
           with
           | None when use_bs_dependencies ->
             module_name
             |> find ~bs_dependencies:true ~map:bs_dependencies_file_map
           | res -> res);
  }

let apply ~resolver ~use_bs_dependencies module_name =
  module_name |> Lazy.force resolver.lazy_find ~use_bs_dependencies

(** Resolve a reference to ModuleName, and produce a path suitable for require.
   E.g. require "../foo/bar/ModuleName.ext" where ext is ".res" or ".js". *)
let resolve_module ~(config : Config.t) ~import_extension ~output_file_relative
    ~resolver ~use_bs_dependencies module_name =
  let output_file_relative_dir =
    (* e.g. src if we're generating src/File.bs.js *)
    Filename.dirname output_file_relative
  in
  let output_file_absolute_dir =
    config.project_root +++ output_file_relative_dir
  in
  let module_name_res_file =
    (* Check if the module is in the same directory as the file being generated.
       So if e.g. project_root/src/ModuleName.res exists. *)
    output_file_absolute_dir +++ (ModuleName.to_string module_name ^ ".res")
  in
  let candidate =
    (* e.g. import "./Modulename.ext" *)
    module_name
    |> ImportPath.from_module ~dir:Filename.current_dir_name ~import_extension
  in
  if Sys.file_exists module_name_res_file then candidate
  else
    let rec path_to_list path =
      let is_root = path |> Filename.basename = path in
      match is_root with
      | true -> [path]
      | false ->
        (path |> Filename.basename) :: (path |> Filename.dirname |> path_to_list)
    in
    match module_name |> apply ~resolver ~use_bs_dependencies with
    | None -> candidate
    | Some (resolved_module_dir, case, bs_dependencies) ->
      (* e.g. "dst" in case of dst/ModuleName.res *)
      let walk_up_output_dir =
        output_file_relative_dir |> path_to_list
        |> List.map (fun _ -> Filename.parent_dir_name)
        |> fun l ->
        match l with
        | [] -> ""
        | _ :: rest -> rest |> List.fold_left ( +++ ) Filename.parent_dir_name
      in
      let from_output_dir_to_module_dir =
        (* e.g. "../dst" *)
        match bs_dependencies with
        | true -> resolved_module_dir
        | false -> walk_up_output_dir +++ resolved_module_dir
      in
      (* e.g. import "../dst/ModuleName.ext" *)
      (match case = Uppercase with
      | true -> module_name
      | false -> module_name |> ModuleName.uncapitalize)
      |> ImportPath.from_module ~dir:from_output_dir_to_module_dir
           ~import_extension

let resolve_generated_module ~config ~output_file_relative ~resolver module_name
    =
  if !Debug.module_resolution then
    Log_.item "Resolve Generated Module: %s\n"
      (module_name |> ModuleName.to_string);
  let import_path =
    resolve_module ~config
      ~import_extension:(ModuleExtension.generated_module_extension ~config)
      ~output_file_relative ~resolver ~use_bs_dependencies:true module_name
  in
  if !Debug.module_resolution then
    Log_.item "Import Path: %s\n" (import_path |> ImportPath.dump);
  import_path

(** Returns the path to import a given Reason module name. *)
let import_path_for_reason_module_name ~(config : Config.t)
    ~output_file_relative ~resolver module_name =
  if !Debug.module_resolution then
    Log_.item "Resolve Reason Module: %s\n" (module_name |> ModuleName.to_string);
  match config.shims_map |> ModuleNameMap.find module_name with
  | shim_module_name ->
    if !Debug.module_resolution then
      Log_.item "ShimModuleName: %s\n" (shim_module_name |> ModuleName.to_string);
    let import_extension =
      ModuleExtension.shim_ts_output_file_extension ~config
    in
    let import_path =
      resolve_module ~config ~import_extension ~output_file_relative ~resolver
        ~use_bs_dependencies:false shim_module_name
    in
    if !Debug.module_resolution then
      Log_.item "Import Path: %s\n" (import_path |> ImportPath.dump);
    import_path
  | exception Not_found ->
    module_name
    |> resolve_generated_module ~config ~output_file_relative ~resolver
