type dependency_kind = Regular | Development

type request = {kind: dependency_kind; declaration: Config.dependency}

type resolved = {request: request; dependency: Package_resolution.dependency}

type package = {config: Config.t; is_local: bool; dependencies: resolved list}

type graph = {packages: package list; feature_requests: Feature_requests.t}

val dependency_kind_name : dependency_kind -> string
val requests : prod:bool -> is_local:bool -> Config.t -> request list

val resolve : Package_resolution.t -> package_root:string -> request -> resolved

val traverse :
  root_config:Config.t ->
  prod:bool ->
  features:string list option ->
  resolve:(Config.t -> request -> resolved option) ->
  graph

val discover :
  root_config:Config.t ->
  prod:bool ->
  features:string list option ->
  resolution:Package_resolution.t ->
  graph
