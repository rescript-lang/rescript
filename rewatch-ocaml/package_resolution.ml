type dependency = {
  name: string;
  directory: string;
  config: Config.t;
  is_local: bool;
}

type diagnostic_mode = Report_diagnostics | Suppress_diagnostics

type t = {
  root_config: Config.t;
  context: Project_context.dependency_context;
  loaded: (string, Config.t) Hashtbl.t;
  edges: (string * string, dependency) Hashtbl.t;
  selected: (string, dependency) Hashtbl.t;
  reported_duplicates: (string * string, unit) Hashtbl.t;
  diagnostic_mode: diagnostic_mode;
  root_package_name: string;
}

let create ?(diagnostic_mode = Report_diagnostics) root_config =
  let loaded = Hashtbl.create 32 in
  Hashtbl.add loaded root_config.Config.root root_config;
  {
    root_config;
    context = Project_context.dependency_context root_config;
    loaded;
    edges = Hashtbl.create 32;
    selected = Hashtbl.create 32;
    reported_duplicates = Hashtbl.create 8;
    diagnostic_mode;
    root_package_name =
      Package_diagnostics.package_identity
        ~report_diagnostics:(diagnostic_mode = Report_diagnostics)
        root_config;
  }

let load_config resolution root =
  match Hashtbl.find_opt resolution.loaded root with
  | Some config -> config
  | None ->
    let config = Config.load_root root in
    Hashtbl.add resolution.loaded root config;
    config

let is_local resolution directory =
  Project_context.dependency_is_local_canonical resolution.context directory

let dependency_path resolution ~package_root name =
  Project_context.dependency_path_in resolution.context package_root name

let dependency_candidates resolution ~package_root name =
  Project_context.dependency_candidates_in resolution.context package_root name

let root_package_name resolution = resolution.root_package_name

let resolve resolution ~package_root (declaration : Config.dependency) =
  let edge_key = (package_root, declaration.name) in
  match Hashtbl.find_opt resolution.edges edge_key with
  | Some identity -> identity
  | None ->
    let candidate =
      Project_context.require_dependency_directory ~context:resolution.context
        package_root declaration
    in
    let dependency =
      match Hashtbl.find_opt resolution.selected declaration.name with
      | Some selected ->
        (if selected.directory <> candidate then
           let key = (declaration.name, candidate) in
           if
             resolution.diagnostic_mode = Report_diagnostics
             && not (Hashtbl.mem resolution.reported_duplicates key)
           then (
             Hashtbl.add resolution.reported_duplicates key ();
             Printf.eprintf "Duplicated package: %s %s (chosen) vs %s in %s\n%!"
               declaration.name
               (Project_context.display_path ~root:resolution.root_config.root
                  selected.directory)
               (Project_context.display_path ~root:resolution.root_config.root
                  candidate)
               (Project_context.display_path ~root:resolution.root_config.root
                  package_root)));
        selected
      | None ->
        let config =
          try load_config resolution candidate
          with Config.Error message ->
            raise
              (Project_context.Package_error
                 (Printf.sprintf
                    "Could not build package tree for '%s' at path '%s'. \
                     Error: %s"
                    declaration.name resolution.root_config.root message))
        in
        let name =
          Package_diagnostics.package_identity
            ~report_diagnostics:(resolution.diagnostic_mode = Report_diagnostics)
            config
        in
        if name <> declaration.name then
          raise
            (Project_context.Package_error
               (Printf.sprintf
                  "Could not build package tree reading dependency '%s' at \
                   path '%s'. Error: resolved package identity '%s' does not \
                   match the requested dependency name"
                  declaration.name candidate name));
        let identity =
          {
            name;
            directory = candidate;
            config;
            is_local = is_local resolution candidate;
          }
        in
        Hashtbl.add resolution.selected declaration.name identity;
        identity
    in
    Hashtbl.add resolution.edges edge_key dependency;
    dependency
