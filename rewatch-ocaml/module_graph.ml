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

let dependency_head dependency =
  match String.split_on_char '.' dependency with
  | head :: _ -> head
  | [] -> dependency

let compiler_namespace (config : Config.t) =
  Config.namespace_compiler_name config.namespace

let validate_visible_namespaces ~(root_config : Config.t)
    (graph_packages : Build_types.graph_package list) =
  let by_root = Hashtbl.create (List.length graph_packages) in
  List.iter
    (fun (package : Build_types.graph_package) ->
      Hashtbl.replace by_root package.graph_root package)
    graph_packages;
  graph_packages
  |> List.sort (fun first second ->
      String.compare first.Build_types.graph_root second.graph_root)
  |> List.iter (fun consumer ->
      let visible =
        consumer
        :: (consumer.Build_types.graph_dependency_directories
           |> List.filter_map (fun dependency ->
               Hashtbl.find_opt by_root dependency.Build_types.directory))
      in
      let namespaces = Hashtbl.create (List.length visible) in
      visible
      |> List.sort (fun first second ->
          String.compare first.Build_types.graph_root second.graph_root)
      |> List.iter (fun package ->
          compiler_namespace package.Build_types.graph_compile_config
          |> Option.iter (fun namespace ->
              match Hashtbl.find_opt namespaces namespace with
              | None -> Hashtbl.add namespaces namespace package
              | Some previous
                when previous.Build_types.graph_root = package.graph_root ->
                ()
              | Some previous ->
                let display package =
                  Printf.sprintf "%s (%s)" package.Build_types.graph_config.name
                    (Project_context.display_path ~root:root_config.root
                       package.graph_root)
                in
                raise
                  (Project_context.Error
                     (Printf.sprintf
                        "Could not initialize build: Namespace %s is provided \
                         by both %s and %s while building %s. Give the \
                         packages distinct namespaces."
                        namespace (display previous) (display package)
                        (display consumer))))))

let resolve_dependency ~find_module ~find_namespace_maps
    (node : Build_types.global_module) dependency =
  let raw_name = dependency_head dependency in
  let local_name =
    match
      (Config.namespace_name node.namespace, String.split_on_char '.' dependency)
    with
    | Some namespace, first :: second :: _ when first = namespace -> second
    | _ -> raw_name
  in
  let local_key = Config.namespaced_module_name node.namespace local_name in
  let is_visible (dependency_node : Build_types.global_module) =
    dependency_node.Build_types.package_name = node.package_name
    || List.mem dependency_node.package_name node.allowed_dependencies
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
                when Config.namespace_name dependency_node.Build_types.namespace
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
        |> List.filter_map (fun (namespace_map : Build_types.namespace_map) ->
            if
              namespace_map.package_name = node.package_name
              || List.mem namespace_map.package_name node.allowed_dependencies
            then Some namespace_map.key
            else None)))

let resolved_dependencies ~find_module ~find_namespace_maps
    (node : Build_types.global_module) =
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
      |> List.find_map (fun (namespace_map : Build_types.namespace_map) ->
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

let find_cycle modules namespace_maps build_state =
  let nodes_by_key =
    Hashtbl.create (List.length modules + List.length namespace_maps)
  in
  List.iter
    (fun (node : Build_types.global_module) ->
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
    (fun (namespace_map : Build_types.namespace_map) ->
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
    |> List.sort (fun first second -> String.compare first.key second.key)
    |> List.map (fun node ->
        (node, (Build_state.find_exn build_state node.key).dependencies))
  in
  let name (node, _) = node.key in
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
