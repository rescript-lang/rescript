open Gentype_common
module Module_name_map = Config.Module_name_map

let ( +++ ) = Filename.concat

(** The source path in the header of an AST file, read from its first bytes
    (a header is a few hundred bytes), or [None] when the file is not an AST. *)
let ast_source path =
  match open_in_bin path with
  | exception Sys_error _ -> None
  | channel ->
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let read length =
          let contents = really_input_string channel length in
          Binary_ast.read_header contents
          |> Option.map (Binary_ast.header_source contents)
        in
        let length = in_channel_length channel in
        match read (min length 4096) with
        | Some source -> Some source
        | None when length > 4096 ->
          seek_in channel 0;
          read length
        | None -> None)

(** The directories of a dependency's sources, relative to its root, by the
    name of their ASTs ([X] for [X.ast]): each AST records its source. *)
let read_source_dirs ~root ~artifact_dir names =
  let prefix = root ^ Filename.dir_sep in
  let source_dirs = Hashtbl.create 64 in
  names
  |> Array.iter (fun name ->
      if Filename.check_suffix name ".ast" then
        match ast_source (artifact_dir +++ name) with
        | Some source when String.starts_with ~prefix source ->
          let relative =
            String.sub source (String.length prefix)
              (String.length source - String.length prefix)
          in
          let dir = Filename.dirname relative in
          Hashtbl.replace source_dirs
            (Filename.chop_suffix name ".ast")
            (if dir = Filename.current_dir_name then "" else dir)
        | Some _ | None -> ());
  source_dirs

(* Every gentype compile of a build resolves the same dependencies, and their
   ASTs, unlike their compiled files, are complete once the build parsed. So
   the AST headers are read once per build (a generation of the build file
   store). *)
let source_dirs_cache = Hashtbl.create 8
let source_dirs_mutex = Mutex.create ()

let source_dirs ~root ~artifact_dir names =
  if not (Build_file_store.enabled ()) then
    read_source_dirs ~root ~artifact_dir names
  else
    let generation = Build_file_store.generation () in
    let locked f =
      Mutex.lock source_dirs_mutex;
      Fun.protect ~finally:(fun () -> Mutex.unlock source_dirs_mutex) f
    in
    match locked (fun () -> Hashtbl.find_opt source_dirs_cache root) with
    | Some (cached_generation, source_dirs) when cached_generation = generation
      ->
      source_dirs
    | Some _ | None ->
      let source_dirs = read_source_dirs ~root ~artifact_dir names in
      locked (fun () ->
          Hashtbl.replace source_dirs_cache root (generation, source_dirs));
      source_dirs

(** The directories of a dependency's modules, relative to its root, by the
    name of their compiled files. Its artifacts are flat in [lib/rescript],
    so a module's [X.cmt] or [X-Ns.cmt] lies in the directory of the source
    that [X.ast] names. *)
let dependency_module_dirs ~root =
  let artifact_dir = ["lib"; "rescript"] |> List.fold_left ( +++ ) root in
  if not (Sys.file_exists artifact_dir && Sys.is_directory artifact_dir) then []
  else
    let names = Sys.readdir artifact_dir in
    let source_dirs = source_dirs ~root ~artifact_dir names in
    (* A namespaced module's compiled files append "-" and the namespace,
       which has no "-" of its own, to the source file name. The namespace
       itself has a map instead of a source, and belongs to the package root. *)
    let source_dir compiled =
      match Hashtbl.find_opt source_dirs compiled with
      | Some dir -> Some dir
      | None when Array.mem (compiled ^ ".mlmap") names -> Some ""
      | None -> (
        match String.rindex_opt compiled '-' with
        | Some index ->
          Hashtbl.find_opt source_dirs (String.sub compiled 0 index)
        | None -> None)
    in
    names |> Array.to_list
    |> List.filter_map (fun name ->
        if
          Filename.check_suffix name ".cmt"
          || Filename.check_suffix name ".cmti"
        then
          source_dir (Filename.remove_extension name)
          |> Option.map (fun dir -> (name, dir))
        else None)

(** Build a map of source filenames (with the given extensions) back to the
    directory where they belong. Source directories and dependency install
    paths are provided by the build system through CLI flags. *)
let sourcedirs_to_map ~(config : Config.t) ~extensions ~exclude_file =
  let rec chop_extensions fname =
    match fname |> Filename.chop_extension with
    | fname_chopped -> fname_chopped |> chop_extensions
    | exception _ -> fname
  in
  let file_map = ref Module_name_map.empty in
  let bs_dependencies_file_map = ref Module_name_map.empty in
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
            |> Module_name_map.add
                 (fname |> chop_extensions |> Module_name.from_string_unsafe)
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
        dependency_module_dirs ~root:path
        |> List.iter (fun (fname, dir) ->
            bs_dependencies_file_map :=
              !bs_dependencies_file_map
              |> Module_name_map.add
                   (fname |> chop_extensions |> Module_name.from_string_unsafe)
                   (package_name +++ dir))
      | exception Not_found -> ());
  (!file_map, !bs_dependencies_file_map)

type case = Lowercase | Uppercase

type resolver = {
  lazy_find:
    (use_bs_dependencies:bool -> Module_name.t -> (string * case * bool) option)
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
           match map |> Module_name_map.find module_name with
           | resolved_module_dir ->
             Some (resolved_module_dir, Uppercase, bs_dependencies)
           | exception Not_found -> (
             match
               map
               |> Module_name_map.find (module_name |> Module_name.uncapitalize)
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

let rec path_to_list path =
  let parent = Filename.dirname path in
  (* On Windows, the basename of a drive root omits the drive prefix. *)
  if Filename.basename path = path || parent = path then [path]
  else (path |> Filename.basename) :: path_to_list parent

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
    output_file_absolute_dir +++ (Module_name.to_string module_name ^ ".res")
  in
  let candidate =
    (* e.g. import "./Modulename.ext" *)
    module_name
    |> Import_path.from_module ~dir:Filename.current_dir_name ~import_extension
  in
  if Sys.file_exists module_name_res_file then candidate
  else
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
        | false -> module_name |> Module_name.uncapitalize)
      |> Import_path.from_module ~dir:from_output_dir_to_module_dir
           ~import_extension

let resolve_generated_module ~config ~output_file_relative ~resolver module_name
    =
  if !(Debug.module_resolution ()) then
    Log_.item "Resolve Generated Module: %s\n"
      (module_name |> Module_name.to_string);
  let import_path =
    resolve_module ~config
      ~import_extension:(Module_extension.generated_module_extension ~config)
      ~output_file_relative ~resolver ~use_bs_dependencies:true module_name
  in
  if !(Debug.module_resolution ()) then
    Log_.item "Import Path: %s\n" (import_path |> Import_path.dump);
  import_path

(** Returns the path to import a given Reason module name. *)
let import_path_for_reason_module_name ~(config : Config.t)
    ~output_file_relative ~resolver module_name =
  if !(Debug.module_resolution ()) then
    Log_.item "Resolve Reason Module: %s\n"
      (module_name |> Module_name.to_string);
  match config.shims_map |> Module_name_map.find module_name with
  | shim_module_name ->
    if !(Debug.module_resolution ()) then
      Log_.item "ShimModuleName: %s\n"
        (shim_module_name |> Module_name.to_string);
    let import_extension =
      Module_extension.shim_ts_output_file_extension ~config
    in
    let import_path =
      resolve_module ~config ~import_extension ~output_file_relative ~resolver
        ~use_bs_dependencies:false shim_module_name
    in
    if !(Debug.module_resolution ()) then
      Log_.item "Import Path: %s\n" (import_path |> Import_path.dump);
    import_path
  | exception Not_found ->
    module_name
    |> resolve_generated_module ~config ~output_file_relative ~resolver
