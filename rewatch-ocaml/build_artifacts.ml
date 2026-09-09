let path_of_parts root parts = List.fold_left Filename.concat root parts
let lib_path root directory = path_of_parts root ["lib"; directory]

let ensure_dir path =
  let rec loop path =
    if path = "" || path = "." || Sys.file_exists path then ()
    else (
      loop (Filename.dirname path);
      Unix.mkdir path 0o755)
  in
  loop path

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let copy_file source destination =
  if Sys.file_exists source then (
    ensure_dir (Filename.dirname destination);
    let input = open_in_bin source in
    let output = open_out_bin destination in
    Fun.protect
      ~finally:(fun () ->
        close_in_noerr input;
        close_out_noerr output)
      (fun () ->
        really_input_string input (in_channel_length input)
        |> output_string output))

let files_equal first second =
  if not (Sys.file_exists first && Sys.file_exists second) then false
  else
    let first_stat = Unix.stat first in
    let second_stat = Unix.stat second in
    first_stat.Unix.st_size = second_stat.Unix.st_size
    && let first_channel = open_in_bin first in
       let second_channel = open_in_bin second in
       Fun.protect
         ~finally:(fun () ->
           close_in_noerr first_channel;
           close_in_noerr second_channel)
         (fun () ->
           let buffer_size = 65_536 in
           let first_buffer = Bytes.create buffer_size in
           let second_buffer = Bytes.create buffer_size in
           let rec loop () =
             let first_count = input first_channel first_buffer 0 buffer_size in
             let second_count = input second_channel second_buffer 0 buffer_size in
             first_count = second_count
             && (first_count = 0
                || (Bytes.sub first_buffer 0 first_count
                    = Bytes.sub second_buffer 0 second_count
                   && loop ()))
           in
           loop ())

let copy_file_if_changed source destination =
  if not (files_equal source destination) then copy_file source destination

let modification_time path =
  if Sys.file_exists path then Some (Unix.stat path).Unix.st_mtime else None

let remove_file path =
  if Sys.file_exists path then (try Sys.remove path with Sys_error _ -> ())

let rec remove_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    | _ -> Sys.remove path
  with Sys_error _ | Unix.Unix_error _ -> ()

let rec files_under directory =
  try
    if not (Sys.file_exists directory) then []
    else if (Unix.lstat directory).Unix.st_kind <> Unix.S_DIR then [directory]
    else
      Sys.readdir directory |> Array.to_list
      |> List.concat_map (fun name ->
           files_under (Filename.concat directory name))
  with Sys_error _ | Unix.Unix_error _ -> []

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

let generated_output_details path =
  let output_path =
    if Filename.check_suffix path ".map" then Filename.chop_suffix path ".map"
    else path
  in
  generated_output_suffixes
  |> List.find_map (fun suffix ->
       if Filename.check_suffix output_path suffix then
         Some
           ( (Filename.basename output_path |> fun basename ->
               Filename.chop_suffix basename suffix),
             suffix,
             output_path )
       else None)

let generated_output_owner path =
  generated_output_details path
  |> Option.map (fun (owner, _, _) -> owner)

let watch_sidecar_suffixes = [".rewatch-pending"; ".rewatch-backup"]

let is_watch_output_sidecar path =
  List.exists
    (fun sidecar_suffix ->
      Filename.check_suffix path sidecar_suffix
      &&
      let output = Filename.chop_suffix path sidecar_suffix in
      Option.is_some (generated_output_details output))
    watch_sidecar_suffixes

let cleanup_watch_output_sidecars ~root (config : Config.t) =
  let directories =
    List.map
      (fun (source : Config.source) -> Filename.concat root source.dir)
      config.sources
    @ [Filename.concat root (lib_path "" "es6"); Filename.concat root (lib_path "" "js")]
    |> List.sort_uniq String.compare
  in
  directories
  |> List.iter (fun directory ->
       files_under directory
       |> List.iter (fun path ->
            if is_watch_output_sidecar path then remove_file path))

let prepare_watch_output watch_outputs watch_output_paths ~dirty_ast output =
  if
    (not (Sys.file_exists output))
    && not (Hashtbl.mem watch_output_paths output)
  then (
    let pending = output ^ ".rewatch-pending" in
    remove_file pending;
    Hashtbl.add watch_output_paths output ();
    watch_outputs := (output, pending, dirty_ast) :: !watch_outputs)

let with_root_options (config : Config.t) (root_config : Config.t) =
  {
    config with
    (* Like the Rust implementation, one invocation compiles every package for
       the root project's requested module systems and suffixes. Apart from
       producing consistent output, this ensures dependency CMIs advertise a
       module system that their dependents can consume. *)
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
}

let cleanup_stale ?ocaml_files ~root ~ocaml_dir ~is_local (config : Config.t)
    modules =
  let build_dir = lib_path root "bs" in
  (* Keep one inventory of each artifact tree. Rewalking these trees for every
     cleanup phase made unchanged builds perform several times Rust's directory
     and metadata work. Paths removed below can safely remain in the inventory:
     later phases only classify their names or call the idempotent remove_file. *)
  let ocaml_files =
    match ocaml_files with
    | Some files -> files
    | None -> files_under ocaml_dir
  in
  let build_files = files_under build_dir in
  let source_files =
    List.map
      (fun source ->
        (source, files_under (Filename.concat root source.Config.dir)))
      config.sources
  in
  let output_files =
    [lib_path "" "es6"; lib_path "" "js"]
    |> List.map (fun directory ->
         let output_dir = Filename.concat root directory in
         (output_dir, files_under output_dir))
  in
  (List.concat_map snd source_files @ List.concat_map snd output_files)
  |> List.iter (fun path ->
       if is_watch_output_sidecar path then remove_file path);
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
           (Filename.chop_suffix basename ".ast") ())
       else if Filename.check_suffix basename ".iast" then (
         incr previous_ast_count;
         Hashtbl.replace owned_output_names
           (Filename.chop_suffix basename ".iast") ()));
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
      if Option.is_some module_.Source.interface then
        add_expected source_base [".iast"; ".resi"];
      add_expected compiler_base [".cmi"; ".cmj"; ".cmt"; ".cmti"])
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
         remove_file path;
         build_files
         |> List.iter (fun build_path ->
              if Filename.basename build_path = basename then
                if defer_working_cmi_until_after_compile basename then
                  deferred_artifacts := build_path :: !deferred_artifacts
                else remove_file build_path)));
  let configured_suffixes =
    List.map (Config.package_spec_suffix config) config.package_specs
  in
  let relative_under directory path =
    let prefix = directory ^ Filename.dir_sep in
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  in
  let previously_generated = Hashtbl.create 32 in
  build_files
  |> List.iter (fun path ->
       generated_output_details path
       |> Option.iter (fun (_, _, output_path) ->
            (* A map alone is not enough provenance to delete a public file. *)
            if path = output_path then
              Hashtbl.replace previously_generated
                (relative_under build_dir output_path) ()));
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
    generated_output_details path
    |> Option.fold ~none:false ~some:(fun (name, suffix, output_path) ->
         Hashtbl.mem owned_output_names name
         && not (Hashtbl.mem expected_outputs output_path)
         &&
         let removed =
           List.mem (String.capitalize_ascii name) !removed_modules
         in
         (removed && List.mem suffix configured_suffixes)
         || (is_local && Hashtbl.mem previously_generated build_relative))
  in
  let removed_outputs = Hashtbl.create 16 in
  let remove_output ~build_relative path =
    generated_output_details path
    |> Option.iter (fun _ -> Hashtbl.replace removed_outputs build_relative ());
    remove_file path
  in
  source_files
  |> List.iter (fun (_, files) ->
       files
       |> List.iter (fun path ->
            generated_output_details path
            |> Option.iter (fun (_, _, output_path) ->
                 let build_relative = relative_under root output_path in
                 if should_remove_output ~build_relative path then
                   remove_output ~build_relative path)));
  output_files
  |> List.iter (fun (output_dir, files) ->
       files
       |> List.iter (fun path ->
            generated_output_details path
            |> Option.iter (fun (_, _, output_path) ->
                 let build_relative = relative_under output_dir output_path in
                 if should_remove_output ~build_relative path then
                   remove_output ~build_relative path)));
  build_files
  |> List.iter (fun path ->
       generated_output_details path
       |> Option.iter (fun (_, _, output_path) ->
            if
              Hashtbl.mem removed_outputs
                (relative_under build_dir output_path)
            then remove_file path));
  {
    removed_modules = !removed_modules;
    previous_ast_count = !previous_ast_count;
    deferred_artifacts = !deferred_artifacts;
  }
