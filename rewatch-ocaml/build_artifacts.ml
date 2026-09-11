let lib_path root directory = File_util.path_of_parts root ["lib"; directory]

let generated_js_path (config : Config.t) path (spec : Config.package_spec) =
  let directory = Filename.dirname path in
  let output_dir =
    if spec.in_source then directory
    else
      Filename.concat
        (match spec.module_format with
        | Config.Esmodule -> lib_path "" "es6"
        | Config.Commonjs -> lib_path "" "js")
        directory
  in
  Filename.concat config.root
    (Filename.concat output_dir
       (Filename.remove_extension (Filename.basename path)
       ^ Config.package_spec_suffix config spec))

let generated_build_js_path ~build_dir (config : Config.t) path
    (spec : Config.package_spec) =
  Filename.concat build_dir
    (Filename.remove_extension path ^ Config.package_spec_suffix config spec)

let remove_public_outputs (config : Config.t) modules =
  List.iter
    (fun module_ ->
      List.iter
        (fun spec ->
          let output =
            generated_js_path config module_.Source.implementation spec
          in
          File_util.remove_file output;
          File_util.remove_file (output ^ ".map"))
        config.package_specs)
    modules

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

let generated_output_details path =
  generated_output_details_for_suffixes generated_output_suffixes path

let is_generated_output_path path =
  Option.is_some (generated_output_details path)

let with_root_options (config : Config.t) (root_config : Config.t) =
  {
    config with
    (* Every package in one invocation must use the root project's requested
       module systems and suffixes. This produces consistent output and ensures
       dependency CMIs advertise a module system that their dependents can
       consume. *)
    package_specs = root_config.package_specs;
    suffix = root_config.suffix;
    jsx_args = root_config.jsx_args;
    source_map_args = root_config.source_map_args;
    source_map_dev = root_config.source_map_dev;
    experimental_args = root_config.experimental_args;
    gentype_args =
      (if config.gentype_args = [] then []
       else
         config.gentype_args
         @ ["-bs-gentype-bsb-project-root"; root_config.root]);
  }

type cleanup_result = {
  removed_modules: string list;
  previous_ast_count: int;
  deferred_artifacts: string list;
  present_public_outputs: (string, unit) Hashtbl.t;
}

let cleanup_stale ?ocaml_files ?ast_sources ?source_files ?present_source_files
    ~root ~ocaml_dir ~is_local (config : Config.t) modules =
  let build_dir = lib_path root "bs" in
  (* Keep one inventory of each artifact tree to avoid repeating directory and
     metadata work during every cleanup phase. Paths removed below can safely
     remain in the inventory: later phases only classify their names or call
     the idempotent File_util.remove_file. *)
  let ocaml_files =
    match ocaml_files with
    | Some files -> files
    | None -> File_util.files_under ocaml_dir
  in
  (* Published ASTs contain the absolute source path used to create them, which
     is enough to address their working artifacts without scanning for them.
     Keep the recursive walk lazy for malformed or legacy ASTs that cannot be
     mapped; normal unchanged builds must not inventory the whole lib/bs tree. *)
  let ast_sources = Option.value ast_sources ~default:[] in
  let fallback_build_files = lazy (File_util.files_under build_dir) in
  let source_files =
    match source_files with
    | Some files -> files
    | None ->
      config.sources
      |> List.concat_map (fun source ->
          File_util.files_under (Filename.concat root source.Config.dir))
  in
  let present_source_files =
    Option.value present_source_files ~default:source_files
  in
  let output_files =
    [lib_path "" "es6"; lib_path "" "js"]
    |> List.map (fun directory ->
        let output_dir = Filename.concat root directory in
        (output_dir, File_util.files_under output_dir))
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
  present_source_files @ List.concat_map snd output_files
  |> List.iter (fun path ->
      if Option.is_some (output_details path) then
        Hashtbl.replace present_public_outputs path ());
  (* Expected outputs may use arbitrary configured suffixes, including suffixes
     that do not resemble JavaScript. Probe those paths directly rather than
     treating a filename heuristic as the source of truth. *)
  List.iter
    (fun module_ ->
      List.iter
        (fun spec ->
          let output =
            generated_js_path config module_.Source.implementation spec
          in
          if
            (not (Hashtbl.mem present_public_outputs output))
            && Sys.file_exists output
          then Hashtbl.replace present_public_outputs output ())
        config.package_specs)
    modules;
  let expected_artifacts = Hashtbl.create (List.length modules * 8) in
  let owned_output_names = Hashtbl.create (List.length modules * 2) in
  let add_expected base extensions =
    List.iter
      (fun extension ->
        Hashtbl.replace expected_artifacts (base ^ extension) ())
      extensions
  in
  let previous_ast_count = ref 0 in
  ocaml_files
  |> List.iter (fun path ->
      let basename = Filename.basename path in
      if Filename.check_suffix basename ".ast" then (
        incr previous_ast_count;
        Hashtbl.replace owned_output_names
          (Filename.chop_suffix basename ".ast")
          ())
      else if Filename.check_suffix basename ".iast" then (
        incr previous_ast_count;
        Hashtbl.replace owned_output_names
          (Filename.chop_suffix basename ".iast")
          ()));
  List.iter
    (fun module_ ->
      let source_base =
        module_.Source.implementation |> Filename.basename
        |> Filename.remove_extension
      in
      let compiler_base =
        Source.compiler_asset_basename config module_.Source.implementation
      in
      Hashtbl.replace owned_output_names source_base ();
      add_expected source_base [".ast"; ".res"];
      if Option.is_some module_.Source.interface then (
        add_expected source_base [".iast"; ".resi"];
        add_expected compiler_base [".cmti"]);
      add_expected compiler_base [".cmi"; ".cmj"; ".cmt"])
    modules;
  Option.iter
    (fun namespace ->
      let base =
        match config.namespace_entry with
        | Some _ -> "@" ^ namespace
        | None -> namespace
      in
      add_expected base [".cmi"; ".cmj"; ".cmt"; ".mlmap"])
    config.namespace;
  let removed_modules = ref [] in
  let deferred_artifacts = ref [] in
  (* Once the published CMI is removed, bsc still consults the working CMI to
     produce its source-located missing-module diagnostic. Keep only that copy
     through compilation; the command finalizer removes every deferred path. *)
  let defer_working_cmi_until_after_compile basename =
    Filename.check_suffix basename ".cmi"
  in
  let relative_to_root path =
    let normalize = Platform.normalize_path_for_comparison in
    let prefix = Filename.concat root "" in
    let normalized_path = normalize path in
    let normalized_prefix = normalize prefix in
    if String.starts_with ~prefix:normalized_prefix normalized_path then
      Some
        (String.sub path (String.length prefix)
           (String.length path - String.length prefix))
    else None
  in
  let mapped_source_directories =
    Hashtbl.create (List.length ast_sources * 2)
  in
  let add_mapped_source_directory key directory =
    let directories =
      Hashtbl.find_opt mapped_source_directories key |> Option.value ~default:[]
    in
    Hashtbl.replace mapped_source_directories key (directory :: directories)
  in
  List.iter
    (fun (_, source) ->
      relative_to_root source
      |> Option.iter (fun relative_source ->
          let directory = Filename.dirname relative_source in
          let source_base =
            relative_source |> Filename.basename |> Filename.remove_extension
          in
          add_mapped_source_directory source_base directory;
          add_mapped_source_directory
            (Source.compiler_asset_basename config relative_source)
            directory))
    ast_sources;
  let directly_mapped_working_paths basename =
    let extension = Filename.extension basename in
    if extension = ".mlmap" then [Filename.concat build_dir basename]
    else
      Hashtbl.find_opt mapped_source_directories
        (Filename.remove_extension basename)
      |> Option.value ~default:[]
      |> List.map (fun directory ->
          Filename.concat build_dir (Filename.concat directory basename))
      |> List.sort_uniq String.compare
  in
  let working_paths basename =
    match directly_mapped_working_paths basename with
    | _ :: _ as paths -> paths
    | [] ->
      Lazy.force fallback_build_files
      |> List.filter (fun path -> Filename.basename path = basename)
  in
  ocaml_files
  |> List.iter (fun path ->
      let basename = Filename.basename path in
      let managed =
        List.exists
          (Filename.check_suffix basename)
          [
            ".cmi";
            ".cmj";
            ".cmt";
            ".cmti";
            ".ast";
            ".iast";
            ".res";
            ".resi";
            ".mlmap";
          ]
      in
      if managed && not (Hashtbl.mem expected_artifacts basename) then (
        if Filename.check_suffix basename ".ast" then
          removed_modules := Source.module_name basename :: !removed_modules
        else if Filename.check_suffix basename ".iast" then
          removed_modules := Source.module_name basename :: !removed_modules;
        File_util.remove_file path;
        working_paths basename
        |> List.iter (fun build_path ->
            if defer_working_cmi_until_after_compile basename then
              if Sys.file_exists build_path then
                deferred_artifacts := build_path :: !deferred_artifacts
              else ()
            else File_util.remove_file build_path)));
  let relative_under directory path =
    let prefix = directory ^ Filename.dir_sep in
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  in
  let expected_outputs =
    Hashtbl.create (List.length modules * List.length config.package_specs)
  in
  List.iter
    (fun module_ ->
      List.iter
        (fun spec ->
          Hashtbl.replace expected_outputs
            (generated_js_path config module_.Source.implementation spec)
            ())
        config.package_specs)
    modules;
  let should_remove_output ~build_relative path =
    output_details path
    |> Option.fold ~none:false ~some:(fun (name, suffix, output_path) ->
        Hashtbl.mem owned_output_names name
        && (not (Hashtbl.mem expected_outputs output_path))
        &&
        let removed =
          List.mem (String.capitalize_ascii name) !removed_modules
        in
        (removed && List.mem suffix configured_suffixes)
        ||
        (* A map alone is not enough provenance to delete a public file. The
            mirrored output has the same relative path below lib/bs, so probe
            that one path instead of scanning the entire working tree. *)
        (is_local && Sys.file_exists (Filename.concat build_dir build_relative)))
  in
  let planned_outputs = Hashtbl.create 16 in
  let plan_output ~build_relative path =
    output_details path
    |> Option.iter (fun (_, _, output_path) ->
        if should_remove_output ~build_relative output_path then
          Hashtbl.replace planned_outputs output_path build_relative)
  in
  source_files
  |> List.iter (fun path ->
      output_details path
      |> Option.iter (fun (_, _, output_path) ->
          plan_output
            ~build_relative:(relative_under root output_path)
            output_path));
  ast_sources
  |> List.iter (fun (_, source) ->
      relative_to_root source
      |> Option.iter (fun relative_source ->
          List.iter
            (fun spec ->
              let output = generated_js_path config relative_source spec in
              plan_output ~build_relative:(relative_under root output) output)
            config.package_specs));
  output_files
  |> List.iter (fun (output_dir, files) ->
      files
      |> List.iter (fun path ->
          output_details path
          |> Option.iter (fun (_, _, output_path) ->
              plan_output
                ~build_relative:(relative_under output_dir output_path)
                output_path)));
  let remove_output (path, build_relative) =
    Hashtbl.remove present_public_outputs path;
    Hashtbl.remove present_public_outputs (path ^ ".map");
    File_util.remove_file path;
    File_util.remove_file (path ^ ".map");
    let working_output = Filename.concat build_dir build_relative in
    File_util.remove_file working_output;
    File_util.remove_file (working_output ^ ".map")
  in
  planned_outputs |> Hashtbl.to_seq |> List.of_seq
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> List.iter remove_output;
  {
    removed_modules = !removed_modules;
    previous_ast_count = !previous_ast_count;
    deferred_artifacts = !deferred_artifacts;
    present_public_outputs;
  }
