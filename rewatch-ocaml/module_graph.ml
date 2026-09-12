type cycle_info = {
  cycle: string list;
  blocked: string list;
  nodes_by_key: (string, cycle_node) Hashtbl.t;
}

and cycle_node = {
  key: string;
  package_root: string;
  source_path: string option;
  display_name: string;
}

type module_node = {
  key: string;
  package_name: string;
  package_root: string;
  source_path: string;
  namespace: Config.namespace;
  visible_packages: (string, unit) Hashtbl.t;
  mutable raw_dependencies: string list;
}

type namespace_map = {
  key: string;
  compiler_name: string;
  namespace: string;
  package_name: string;
  package_root: string;
  members: string list;
}

let namespace_map_key package_root = "\000namespace:" ^ package_root

let dependency_head dependency =
  match String.split_on_char '.' dependency with
  | head :: _ -> head
  | [] -> dependency

let compiler_namespace (config : Config.t) =
  Config.namespace_compiler_name config.namespace

let validate_visible_namespaces ~(root_config : Config.t)
    (package_plans : Package_plan.t list) =
  let by_root = Hashtbl.create (List.length package_plans) in
  List.iter
    (fun (package : Package_plan.t) ->
      Hashtbl.replace by_root package.root package)
    package_plans;
  package_plans
  |> List.sort (fun first second ->
      String.compare first.Package_plan.root second.root)
  |> List.iter (fun consumer ->
      let visible =
        consumer
        :: (consumer.Package_plan.dependencies
           |> List.filter_map (fun dependency ->
               Hashtbl.find_opt by_root dependency.Package_plan.directory))
      in
      let namespaces = Hashtbl.create (List.length visible) in
      visible
      |> List.sort (fun first second ->
          String.compare first.Package_plan.root second.root)
      |> List.iter (fun package ->
          compiler_namespace package.Package_plan.compile_config
          |> Option.iter (fun namespace ->
              match Hashtbl.find_opt namespaces namespace with
              | None -> Hashtbl.add namespaces namespace package
              | Some previous when previous.Package_plan.root = package.root ->
                ()
              | Some previous ->
                let display package =
                  Printf.sprintf "%s (%s)" package.Package_plan.config.name
                    (Project_context.display_path ~root:root_config.root
                       package.root)
                in
                raise
                  (Project_context.Error
                     (Printf.sprintf
                        "Could not initialize build: Namespace %s is provided \
                         by both %s and %s while building %s. Give the \
                         packages distinct namespaces."
                        namespace (display previous) (display package)
                        (display consumer))))))

let resolve_dependency ~find_module ~find_namespace_maps (node : module_node)
    dependency =
  let raw_name = dependency_head dependency in
  let local_name =
    match
      (Config.namespace_name node.namespace, String.split_on_char '.' dependency)
    with
    | Some namespace, first :: second :: _ when first = namespace -> second
    | _ -> raw_name
  in
  let local_key = Config.namespaced_module_name node.namespace local_name in
  let is_visible (dependency_node : module_node) =
    Hashtbl.mem node.visible_packages dependency_node.package_name
  in
  match find_module local_key with
  | Some dependency_node when is_visible dependency_node -> [local_key]
  | _ when Config.namespace_name node.namespace = Some raw_name -> []
  | _ -> (
    match find_module raw_name with
    | Some dependency_node when is_visible dependency_node -> [raw_name]
    | _ -> (
      let explicit_namespaced_module =
        match String.split_on_char '.' dependency with
        | namespace :: module_name :: _ ->
          [module_name ^ "-" ^ namespace; module_name ^ "-@" ^ namespace]
          |> List.find_opt (fun key ->
              match find_module key with
              | Some dependency_node
                when Config.namespace_name dependency_node.namespace
                     = Some namespace
                     && is_visible dependency_node ->
                true
              | Some _ | None -> false)
        | _ -> None
      in
      match explicit_namespaced_module with
      | Some key -> [key]
      | None ->
        find_namespace_maps raw_name
        |> Option.value ~default:[]
        |> List.filter_map (fun (namespace_map : namespace_map) ->
            if Hashtbl.mem node.visible_packages namespace_map.package_name then
              Some namespace_map.key
            else None)))

let resolved_dependencies ~find_module ~find_namespace_maps (node : module_node)
    =
  let parsed =
    node.raw_dependencies
    |> List.concat_map
         (resolve_dependency ~find_module ~find_namespace_maps node)
  in
  let implicit_namespace_entry =
    match node.namespace with
    | Config.Namespace_with_entry {name = namespace; entry}
      when Source.module_name node.source_path = entry ->
      find_namespace_maps namespace
      |> Option.value ~default:[]
      |> List.find_map (fun (namespace_map : namespace_map) ->
          if namespace_map.package_root = node.package_root then
            Some namespace_map.key
          else None)
      |> Option.to_list
    | Config.Namespace_with_entry _ | Config.Namespace _ | Config.No_namespace
      ->
      []
  in
  parsed @ implicit_namespace_entry
  |> List.filter (fun dependency -> dependency <> node.key)
  |> List.sort_uniq String.compare

type initialized = {
  nodes: module_node list;
  namespace_maps: namespace_map list;
  build_state: Build_state.t;
  use_existing_ast_paths: string list;
}

let initialize ~(root_config : Config.t) ~package_plans ~compile_assets
    ~failed_parse_paths =
  let nodes = ref [] in
  let use_existing_ast_paths = ref [] in
  List.iter
    (fun (package : Package_plan.t) ->
      let visible_packages =
        Hashtbl.create (List.length package.dependencies + 1)
      in
      Hashtbl.replace visible_packages package.name ();
      List.iter
        (fun (dependency : Package_plan.dependency) ->
          Hashtbl.replace visible_packages dependency.declaration.name ())
        package.dependencies;
      List.iter
        (fun module_ ->
          let dependencies path =
            if
              Hashtbl.mem failed_parse_paths (Filename.concat package.root path)
            then []
            else
              Compiler_process.ast_dependencies ~build_dir:package.build_dir
                (Source.ast_path path)
          in
          let raw_dependencies =
            List.sort_uniq String.compare
              (dependencies module_.Source.implementation
              @
              match module_.Source.interface with
              | None -> []
              | Some path -> dependencies path)
          in
          let compiler_base =
            Source.compiler_basename package.compile_config module_.Source.name
          in
          if Option.is_none (Compile_assets.cmt compile_assets compiler_base)
          then
            use_existing_ast_paths :=
              Filename.concat package.root module_.Source.implementation
              :: !use_existing_ast_paths;
          nodes :=
            {
              key = compiler_base;
              package_name = package.name;
              package_root = package.root;
              source_path = module_.Source.implementation;
              namespace = package.compile_config.namespace;
              visible_packages;
              raw_dependencies;
            }
            :: !nodes)
        package.modules)
    package_plans;
  let nodes =
    List.sort
      (fun (first : module_node) second -> String.compare first.key second.key)
      !nodes
  in
  let by_key : (string, module_node) Hashtbl.t =
    Hashtbl.create (List.length nodes)
  in
  List.iter
    (fun (node : module_node) ->
      match Hashtbl.find_opt by_key node.key with
      | None -> Hashtbl.add by_key node.key node
      | Some previous ->
        raise
          (Source.duplicate_error ~display_root:root_config.root "" node.key
             (Filename.concat previous.package_root previous.source_path)
             (Filename.concat node.package_root node.source_path)))
    nodes;
  let namespace_maps =
    package_plans
    |> List.filter_map (fun (package : Package_plan.t) ->
        let namespace = package.Package_plan.compile_config.namespace in
        let namespace_details =
          match namespace with
          | Config.No_namespace -> None
          | Config.Namespace name -> Some (name, name, None)
          | Config.Namespace_with_entry {name; entry} ->
            Some ("@" ^ name, name, Some entry)
        in
        namespace_details
        |> Option.map (fun (compiler_name, name, namespace_entry) ->
            let members =
              Source.namespace_members ~entry:namespace_entry package.modules
              |> List.map (fun module_ ->
                  Source.compiler_basename package.compile_config
                    module_.Source.name)
              |> List.sort_uniq String.compare
            in
            {
              key = namespace_map_key package.root;
              compiler_name;
              namespace = name;
              package_name = package.name;
              package_root = package.root;
              members;
            }))
  in
  let namespace_maps_by_name = Hashtbl.create (List.length namespace_maps) in
  List.iter
    (fun (namespace_map : namespace_map) ->
      let existing =
        Hashtbl.find_opt namespace_maps_by_name namespace_map.namespace
        |> Option.value ~default:[]
      in
      Hashtbl.replace namespace_maps_by_name namespace_map.namespace
        (namespace_map :: existing))
    namespace_maps;
  let source_graph_nodes =
    List.map
      (fun (node : module_node) ->
        ( node,
          resolved_dependencies ~find_module:(Hashtbl.find_opt by_key)
            ~find_namespace_maps:(Hashtbl.find_opt namespace_maps_by_name)
            node ))
      nodes
  in
  let build_state =
    Build_state.create
      (List.length source_graph_nodes + List.length namespace_maps)
  in
  let modified = Option.map (fun entry -> entry.Compile_assets.modified) in
  List.iter
    (fun ((node : module_node), _) ->
      Build_state.add build_state ~key:node.key ~kind:Build_state.Source_module
        ~last_compiled_cmi:
          (Compile_assets.cmi compile_assets node.key |> modified)
        ~last_compiled_cmt:
          (Compile_assets.cmt compile_assets node.key |> modified))
    source_graph_nodes;
  List.iter
    (fun (namespace_map : namespace_map) ->
      Build_state.add build_state ~key:namespace_map.key
        ~kind:Build_state.Namespace_map
        ~last_compiled_cmi:
          (Compile_assets.cmi compile_assets namespace_map.compiler_name
          |> modified)
        ~last_compiled_cmt:
          (Compile_assets.cmt compile_assets namespace_map.compiler_name
          |> modified))
    namespace_maps;
  List.iter
    (fun ((node : module_node), dependencies) ->
      Build_state.set_dependencies build_state ~key:node.key dependencies)
    source_graph_nodes;
  List.iter
    (fun (namespace_map : namespace_map) ->
      Build_state.set_dependencies build_state ~key:namespace_map.key
        namespace_map.members)
    namespace_maps;
  {
    nodes;
    namespace_maps;
    build_state;
    use_existing_ast_paths = !use_existing_ast_paths;
  }

let find_cycle modules namespace_maps build_state =
  let nodes_by_key =
    Hashtbl.create (List.length modules + List.length namespace_maps)
  in
  List.iter
    (fun (node : module_node) ->
      let key = node.key in
      let module_name = Source.module_name node.source_path in
      let display_name =
        match node.namespace with
        | Config.Namespace_with_entry {name = namespace; entry}
          when entry <> module_name ->
          namespace ^ "." ^ module_name
        | Config.Namespace namespace -> namespace ^ "." ^ module_name
        | Config.Namespace_with_entry _ | Config.No_namespace -> module_name
      in
      Hashtbl.add nodes_by_key key
        {
          key;
          package_root = node.package_root;
          source_path = Some node.source_path;
          display_name;
        })
    modules;
  List.iter
    (fun (namespace_map : namespace_map) ->
      let key = namespace_map.key in
      Hashtbl.add nodes_by_key key
        {
          key;
          package_root = namespace_map.package_root;
          source_path = None;
          display_name = namespace_map.compiler_name;
        })
    namespace_maps;
  let graph_nodes =
    Hashtbl.to_seq_values nodes_by_key
    |> List.of_seq
    |> List.sort (fun (first : cycle_node) second ->
        String.compare first.key second.key)
    |> List.map (fun (node : cycle_node) ->
        (node, (Build_state.find_exn build_state node.key).dependencies))
  in
  let name ((node : cycle_node), _) = node.key in
  let blocked_nodes = Graph.cycle_blocked_nodes graph_nodes ~name ~deps:snd in
  match blocked_nodes with
  | [] -> None
  | _ ->
    let cycle =
      match Graph.shortest_cycle blocked_nodes ~name ~deps:snd with
      | Some cycle -> cycle
      | None ->
        raise
          (Project_context.Error
             "cycle-blocked dependency graph contains no detectable cycle")
    in
    Some {cycle; blocked = List.map name blocked_nodes; nodes_by_key}
