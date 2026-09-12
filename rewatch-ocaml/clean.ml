type package = {
  root: string;
  name: string;
  output_config: Config.t;
  implementation_files: string list;
}

(* The complete cleanup plan is validated before deletion starts so a malformed
   dependency cannot leave only the packages visited before it partially
   cleaned. The resulting order remains dependency-first for progress output. *)
let prepare ~(root_config : Config.t) ~resolution ~seen ~prod ~is_local =
  Package_diagnostics.validate_metadata root_config;
  let packages = ref [] in
  let rec visit (config : Config.t) ~is_local =
    let root = config.root in
    if not (Hashtbl.mem seen root) then (
      Hashtbl.add seen root ();
      Package_diagnostics.report_missing_sources
        ~is_root:(root = root_config.root) config;
      (* A consumer clean owns dependencies previously built in this build
         context, but not an independently built package's published tree. *)
      let owns_outputs =
        root <> root_config.root && Compiler_info.owns_outputs config
      in
      if not owns_outputs then (
        let dependencies = Package_traversal.requests ~prod ~is_local config in
        let resolved_dependencies =
          List.map
            (fun request ->
              Package_traversal.resolve resolution ~package_root:root request)
            dependencies
        in
        List.iter
          (fun (resolved : Package_traversal.resolved) ->
            visit resolved.dependency.config
              ~is_local:resolved.dependency.is_local)
          resolved_dependencies;
        let implementation_files =
          Source.discover_for_cleanup config ~prod:(prod || not is_local)
            ~on_missing:
              (Package_diagnostics.report_missing_source_folder config)
        in
        let output_config = Config.with_root_options config root_config in
        packages :=
          {root; name = config.name; output_config; implementation_files}
          :: !packages))
  in
  visit root_config ~is_local;
  List.rev !packages

let remove_compiler_assets packages ~on_clean =
  List.iter
    (fun package ->
      on_clean package.name;
      List.iter
        (fun dir -> File_util.remove_tree (Filename.concat package.root dir))
        [Build_artifacts.lib_path "" "bs"; Build_artifacts.lib_path "" "ocaml"])
    packages

let remove_generated_outputs packages =
  List.iter
    (fun package ->
      List.iter
        (fun implementation ->
          List.iter
            (fun spec ->
              let output =
                Build_artifacts.generated_js_path package.output_config
                  implementation spec
              in
              File_util.remove_file output;
              File_util.remove_file (output ^ ".map"))
            package.output_config.package_specs)
        package.implementation_files)
    packages

let run ~poll ~verbosity ~folder ~prod =
  let root = Project_context.canonical_project_root folder in
  let show_progress = verbosity >= 0 in
  let interactive = Unix.isatty Unix.stdout && Unix.isatty Unix.stderr in
  let colors = Output.colors_enabled ~interactive in
  let print_cleaning ~step target =
    if interactive && show_progress then
      Printf.printf "%s%!"
        (Output.cleaning_command_message ~color:colors ~step target)
  in
  let print_cleaned ~step ~target ~started_at =
    if interactive && show_progress then
      print_endline
        (Output.cleaned_command_message ~color:colors ~step ~target
           ~seconds:(Unix.gettimeofday () -. started_at))
  in
  Build_lock.with_build ~poll (Project_context.workspace_lock_root root)
    (fun ~release:_ ->
      poll ();
      let root_config = Config.load_root root in
      let resolution = Package_resolution.create root_config in
      let cleanup =
        prepare ~root_config ~resolution ~seen:(Hashtbl.create 32) ~prod
          ~is_local:true
      in
      let compiler_assets = "compiler assets" in
      let compiler_started = Unix.gettimeofday () in
      remove_compiler_assets cleanup ~on_clean:(fun name ->
          if show_progress then
            if interactive then print_cleaning ~step:"1/2" name
            else Printf.printf "Cleaning %s\n%!" name);
      print_cleaned ~step:"1/2" ~target:compiler_assets
        ~started_at:compiler_started;
      poll ();
      let suffixes =
        root_config.package_specs
        |> List.filter_map (fun (spec : Config.package_spec) ->
            if spec.in_source then
              Some (Config.package_spec_suffix root_config spec)
            else None)
        |> String.concat ", "
      in
      let generated_files = suffixes ^ " files" in
      let generated_started = Unix.gettimeofday () in
      print_cleaning ~step:"2/2" generated_files;
      remove_generated_outputs cleanup;
      poll ();
      print_cleaned ~step:"2/2" ~target:generated_files
        ~started_at:generated_started)
