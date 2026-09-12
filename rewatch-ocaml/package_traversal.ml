type dependency_kind = Regular | Development

type request = {kind: dependency_kind; declaration: Config.dependency}

type resolved = {request: request; dependency: Package_resolution.dependency}

type package = {config: Config.t; is_local: bool; dependencies: resolved list}

type graph = {packages: package list; feature_requests: Feature_requests.t}

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
  let feature_requests = Feature_requests.create () in
  let packages = ref [] in
  let rec visit ~is_local ~features (config : Config.t) =
    Feature_requests.add feature_requests config.root features;
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
