type package = {
  root: string;
  name: string;
  output_config: Config.t;
  implementation_files: string list;
}

type t = package list

(* The complete cleanup plan is validated before deletion starts so a malformed
   dependency cannot leave only the packages visited before it partially
   cleaned. The resulting order remains dependency-first for progress output. *)
let prepare ~(root_config : Config.t) ~resolution ~seen ~root ~prod ~is_local =
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
        let dependencies =
          config.dependencies
          @ if prod || not is_local then [] else config.dev_dependencies
        in
        let resolved_dependencies =
          List.map
            (Package_resolution.resolve resolution ~package_root:root)
            dependencies
        in
        List.iter
          (fun (resolved : Package_resolution.dependency) ->
            visit resolved.config ~is_local:resolved.is_local)
          resolved_dependencies;
        let implementation_files =
          Source.discover_for_cleanup config ~prod:(prod || not is_local)
            ~on_missing:
              (Package_diagnostics.report_missing_source_folder config)
        in
        let output_config =
          Build_artifacts.with_root_options config root_config
        in
        packages :=
          {root; name = config.name; output_config; implementation_files}
          :: !packages))
  in
  if root <> root_config.root then
    invalid_arg "cleanup root does not match its prepared configuration";
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
