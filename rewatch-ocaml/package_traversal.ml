type dependency_kind = Regular | Development

let source_discovery_prod ~prod ~is_local = prod || not is_local

type request = {kind: dependency_kind; declaration: Config.dependency}

type resolved = {request: request; dependency: Package_resolution.dependency}

type package = {config: Config.t; is_local: bool; dependencies: resolved list}

type feature_selection = All_features | Selected_features of string list
type feature_requests = (string, feature_selection) Hashtbl.t
type graph = {packages: package list; feature_requests: feature_requests}

let add_feature_request requests root request =
  match (Hashtbl.find_opt requests root, request) with
  | None, None -> Hashtbl.add requests root All_features
  | None, Some requested ->
    Hashtbl.add requests root (Selected_features requested)
  | Some All_features, _ | Some _, None ->
    Hashtbl.replace requests root All_features
  | Some (Selected_features current), Some requested ->
    Hashtbl.replace requests root
      (Selected_features (List.sort_uniq String.compare (current @ requested)))

let find_feature_selection graph root =
  Hashtbl.find_opt graph.feature_requests root

let feature_selection_to_option = function
  | All_features -> None
  | Selected_features features -> Some features

let dependency_kind_name = function
  | Regular -> "dependencies"
  | Development -> "dev-dependencies"

let requests ~prod ~is_local (config : Config.t) =
  List.map
    (fun declaration -> {kind = Regular; declaration})
    config.dependencies
  @
  if prod || not is_local then []
  else
    List.map
      (fun declaration -> {kind = Development; declaration})
      config.dev_dependencies

let resolve resolution ~package_root request =
  {
    request;
    dependency =
      Package_resolution.resolve resolution ~package_root request.declaration;
  }

let traverse ~root_config ~prod ~features ~resolve =
  let visited = Hashtbl.create 32 in
  let feature_requests = Hashtbl.create 32 in
  let packages = ref [] in
  let rec visit ~is_local ~features (config : Config.t) =
    add_feature_request feature_requests config.root features;
    if not (Hashtbl.mem visited config.root) then (
      Hashtbl.add visited config.root ();
      let dependencies =
        requests ~prod ~is_local config
        |> List.filter_map (fun request -> resolve config request)
      in
      List.iter
        (fun resolved ->
          visit ~is_local:resolved.dependency.is_local
            ~features:resolved.request.declaration.features
            resolved.dependency.config)
        dependencies;
      packages := {config; is_local; dependencies} :: !packages)
  in
  visit ~is_local:true ~features root_config;
  {packages = !packages; feature_requests}

let discover ~root_config ~prod ~features ~resolution =
  traverse ~root_config ~prod ~features ~resolve:(fun config request ->
      Some (resolve resolution ~package_root:config.root request))

module For_test = struct
  let create_feature_requests () = Hashtbl.create 4
  let add_feature_request = add_feature_request
  let find_feature_selection requests root = Hashtbl.find_opt requests root
end
