type package = {
  root: string;
  name: string option;
  generated_outputs: (Config.t * string list) option;
}

type t = package list

(* The complete cleanup plan is validated before deletion starts so a malformed
   dependency cannot leave only the packages visited before it partially
   cleaned. The resulting order remains dependency-first for progress output. *)
let prepare ~(root_config : Config.t) ~dependency_context ~seen ~root ~prod
    ~is_local =
  let packages = ref [] in
  let rec visit ~root ~is_local =
    if not (Hashtbl.mem seen root) then (
      Hashtbl.add seen root ();
      let config_path = Config.path_in_root root in
      if Config.exists_in_root root then (
        let config = Config.load config_path in
        Package_diagnostics.validate_metadata config;
        Package_diagnostics.report_missing_sources
          ~is_root:(root = root_config.root) config;
        (* A consumer clean owns dependencies previously built in this build
           context, but not an independently built package's published tree. *)
        let owns_outputs =
          root <> root_config.root && Compiler_info.owns_outputs config
        in
        if not owns_outputs then (
          let dependencies =
            config.dependencies
            @ if prod || not is_local then [] else config.dev_dependencies
          in
          List.iter
            (fun (dependency : Config.dependency) ->
              let directory =
                Project_context.require_dependency_directory
                  ~context:dependency_context root dependency
              in
              try
                visit ~root:directory
                  ~is_local:
                    (Project_context.dependency_is_local_canonical
                       dependency_context directory)
              with Config.Error message ->
                raise
                  (Project_context.Package_error
                     (Printf.sprintf
                        "Could not build package tree for '%s' at path '%s'. Error: %s"
                        dependency.name root_config.root message)))
            dependencies;
          let implementation_files, _inventory_files =
            Source.discover_for_cleanup config
              ~prod:(prod || not is_local)
              ~on_missing:
                (Package_diagnostics.report_missing_source_folder config)
          in
          let output_config =
            Build_artifacts.with_root_options config root_config
          in
          packages :=
            {
              root;
              name = Some config.name;
              generated_outputs = Some (output_config, implementation_files);
            }
            :: !packages))
      else
        packages := {root; name = None; generated_outputs = None} :: !packages)
  in
  visit ~root ~is_local;
  List.rev !packages

let remove_compiler_assets packages ~on_clean =
  List.iter
    (fun package ->
      Option.iter on_clean package.name;
      List.iter
        (fun dir -> File_util.remove_tree (Filename.concat package.root dir))
        [
          Build_artifacts.lib_path "" "bs";
          Build_artifacts.lib_path "" "ocaml";
        ])
    packages

let remove_generated_outputs packages =
  List.iter
    (fun package ->
      match package.generated_outputs with
      | None -> ()
      | Some (output_config, implementation_files) ->
        List.iter
          (fun implementation ->
            List.iter
              (fun spec ->
                let output =
                  Build_artifacts.generated_js_path output_config implementation
                    spec
                in
                File_util.remove_file output;
                File_util.remove_file (output ^ ".map"))
              output_config.package_specs)
          implementation_files)
    packages
