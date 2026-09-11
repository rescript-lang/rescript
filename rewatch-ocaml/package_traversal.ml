type dependency_kind = Regular | Development

type request = {kind: dependency_kind; declaration: Config.dependency}

type resolved = {request: request; dependency: Package_resolution.dependency}

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

let add_feature_request feature_requests resolved =
  Feature_requests.add feature_requests resolved.dependency.directory
    resolved.request.declaration.features
