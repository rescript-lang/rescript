let lib_path root directory = File_util.path_of_parts root ["lib"; directory]

(* Every artifact of a package, flat and keyed by the compiler's file name of
   its module: ASTs, compiler outputs, logs, build state and locks. *)
let artifact_dir root = lib_path root "rescript"

(* Where ReScript 12 and earlier kept the artifacts: nested by source
   directory in lib/bs, and copied flat into lib/ocaml. *)
let legacy_artifact_dirs root = [lib_path root "bs"; lib_path root "ocaml"]

(* Locks live in the artifact directory of the project that takes them, next
   to the candidates they are published from (see Build_lock). *)
let lock_names = ["build.lock"; "watch.lock"]
let lock_candidate_prefixes = [".build-lock-"; ".watch-lock-"]

let is_lock_file name =
  List.exists (fun lock -> name = lock || name = lock ^ ".takeover") lock_names
  || List.exists
       (fun prefix -> String.starts_with ~prefix name)
       lock_candidate_prefixes

(* Removes every artifact of a package, and the directories of earlier
   versions. A clean runs while it holds the lock, so locks stay. *)
let clean root =
  let directory = artifact_dir root in
  (try File_util.directory_entries directory
   with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> [])
  |> List.iter (fun name ->
      if not (is_lock_file name) then
        File_util.remove_tree (Filename.concat directory name));
  List.iter File_util.remove_tree (legacy_artifact_dirs root)

let relative_output_directory path (spec : Config.package_spec) =
  let directory = Filename.dirname path in
  if spec.in_source then directory
  else
    Filename.concat
      (match spec.module_format with
      | Config.Esmodule -> lib_path "" "es6"
      | Config.Commonjs -> lib_path "" "js")
      directory

let generated_js_path (config : Config.t) path (spec : Config.package_spec) =
  let output_dir = relative_output_directory path spec in
  (* The output of a module of the package root is "A.js" or "lib/es6/A.js",
     as the output inventory spells it, not "./A.js" or "lib/es6/./A.js". *)
  Filename.concat config.root
    (Source.without_current_dir_segments
       (Filename.concat output_dir
          (Filename.remove_extension (Filename.basename path)
          ^ Config.package_spec_suffix config spec)))

let ast_path ~artifact_dir source =
  Filename.concat artifact_dir (Source.ast_path source)

(* The next build parses the source of an AST older than it, so dating the AST
   back makes the next build parse it again while keeping it: it is the only
   copy, and a later compile of this build or of the watch session may still
   read it. (A time of 0 would mean now to Unix.utimes.) *)
let invalidate_ast path =
  try Unix.utimes path 1. 1.
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let remove_public_outputs (config : Config.t) implementation_paths =
  List.iter
    (fun implementation ->
      List.iter
        (fun spec ->
          let output = generated_js_path config implementation spec in
          File_util.remove_file output;
          File_util.remove_file (output ^ ".map"))
        config.package_specs)
    implementation_paths

let generated_output_suffixes =
  [
    ".bs.mjs";
    ".bs.cjs";
    ".bs.js";
    ".res.mjs";
    ".res.cjs";
    ".res.js";
    ".mjs";
    ".cjs";
    ".js";
  ]

let generated_output_details_for_suffixes suffixes path =
  let output_path =
    if Filename.check_suffix path ".map" then Filename.chop_suffix path ".map"
    else path
  in
  suffixes
  |> List.find_map (fun suffix ->
      if Filename.check_suffix output_path suffix then
        Some
          ( ( Filename.basename output_path |> fun basename ->
              Filename.chop_suffix basename suffix ),
            suffix,
            output_path )
      else None)

type cleanup_result = {
  removed_modules: string list;
  present_public_outputs: (string, unit) Hashtbl.t;
}

let cleanup_stale ~artifact_files ~ast_sources ~present_source_files
    ~on_removed_module ~root (config : Config.t) modules =
  (* The caller passes one inventory of the artifact directory to avoid
     repeating directory and metadata work during every cleanup phase. Paths
     removed below can safely remain in the inventory: later phases only
     classify their names or call the idempotent File_util.remove_file. *)
  let output_files =
    [lib_path "" "es6"; lib_path "" "js"]
    |> List.concat_map (fun directory ->
        File_util.files_under (Filename.concat root directory))
  in
  let configured_suffixes =
    List.map (Config.package_spec_suffix config) config.package_specs
  in
  let output_suffixes =
    configured_suffixes @ generated_output_suffixes
    |> List.sort_uniq (fun left right ->
        let length_order =
          Int.compare (String.length right) (String.length left)
        in
        if length_order = 0 then String.compare left right else length_order)
  in
  let output_details = generated_output_details_for_suffixes output_suffixes in
  let present_public_outputs = Hashtbl.create 64 in
  present_source_files @ output_files
  |> List.iter (fun path ->
      if Option.is_some (output_details path) && File_util.is_regular_file path
      then Hashtbl.replace present_public_outputs path ());
  (* Expected outputs may use arbitrary configured suffixes, including suffixes
     that do not resemble JavaScript. Probe those paths directly rather than
     treating a filename heuristic as the source of truth. *)
  let expected_outputs =
    Hashtbl.create (List.length modules * List.length config.package_specs)
  in
  List.iter
    (fun module_ ->
      List.iter
        (fun spec ->
          let output =
            generated_js_path config module_.Source.implementation spec
          in
          Hashtbl.replace expected_outputs output ();
          if
            (not (Hashtbl.mem present_public_outputs output))
            && File_util.is_regular_file output
          then Hashtbl.replace present_public_outputs output ())
        config.package_specs)
    modules;
  let current_sources = Hashtbl.create (List.length modules * 2) in
  List.iter
    (fun module_ ->
      module_.Source.implementation :: Option.to_list module_.Source.interface
      |> List.iter (fun source ->
          Filename.concat root source |> Platform.normalize_path_for_comparison
          |> fun source -> Hashtbl.replace current_sources source ()))
    modules;
  (* An AST records its source, so the AST of a source that is gone proves
     which public outputs the build generated for it. *)
  let stale_ast_basenames = Hashtbl.create 8 in
  let removed_output_paths = Hashtbl.create 8 in
  List.iter
    (fun (ast_source : Compile_assets.ast_source) ->
      let source =
        Platform.normalize_path_for_comparison ast_source.source_path
      in
      if not (Hashtbl.mem current_sources source) then (
        Hashtbl.replace stale_ast_basenames
          (Filename.basename ast_source.ast_path)
          ();
        Project_context.relative_to_opt root ast_source.source_path
        |> Option.iter (fun relative_source ->
            List.iter
              (fun spec ->
                Hashtbl.replace removed_output_paths
                  (generated_js_path config relative_source spec)
                  ())
              config.package_specs)))
    ast_sources;
  let expected_artifacts = Hashtbl.create (List.length modules * 8) in
  let add_expected base extensions =
    List.iter
      (fun extension ->
        Hashtbl.replace expected_artifacts (base ^ extension) ())
      extensions
  in
  List.iter
    (fun module_ ->
      let source_base =
        module_.Source.implementation |> Filename.basename
        |> Filename.remove_extension
      in
      let compiler_base =
        Source.compiler_asset_basename config module_.Source.implementation
      in
      add_expected source_base [".ast"];
      if Option.is_some module_.Source.interface then (
        add_expected source_base [".iast"];
        add_expected compiler_base [".cmti"]);
      add_expected compiler_base [".cmi"; ".cmj"; ".cmt"])
    modules;
  Config.namespace_compiler_name config.namespace
  |> Option.iter (fun namespace ->
      add_expected namespace [".cmi"; ".cmj"; ".cmt"; ".mlmap"]);
  (* The dev map's .mlmap is in dev/, which is not scanned for stale files. *)
  Source.dev_namespace_compiler_name config modules
  |> Option.iter (fun dev_namespace ->
      add_expected dev_namespace [".cmi"; ".cmj"; ".cmt"]);
  let removed_modules = ref [] in
  artifact_files
  |> List.iter (fun path ->
      let basename = Filename.basename path in
      let is_ast =
        Filename.check_suffix basename ".ast"
        || Filename.check_suffix basename ".iast"
      in
      if
        Compile_assets.is_managed_basename basename
        && ((not (Hashtbl.mem expected_artifacts basename))
           || Hashtbl.mem stale_ast_basenames basename)
      then (
        if is_ast then (
          let module_name = Source.module_name basename in
          on_removed_module module_name;
          removed_modules := module_name :: !removed_modules);
        (* A removed module's interface goes at once: dependents that still
           name the module must not compile against it. *)
        File_util.remove_file path));
  removed_output_paths |> Hashtbl.to_seq_keys |> List.of_seq
  |> List.filter (fun path -> not (Hashtbl.mem expected_outputs path))
  |> List.sort String.compare
  |> List.iter (fun path ->
      Hashtbl.remove present_public_outputs path;
      Hashtbl.remove present_public_outputs (path ^ ".map");
      File_util.remove_file path;
      File_util.remove_file (path ^ ".map"));
  {removed_modules = !removed_modules; present_public_outputs}
