type dependency_kind = Regular | Development

type request = {kind: dependency_kind; declaration: Config.dependency}

type resolved = {request: request; dependency: Package_resolution.dependency}

type package = {config: Config.t; is_local: bool; dependencies: resolved list}

type feature_selection = All_features | Selected_features of string list
type feature_requests
type graph = {packages: package list; feature_requests: feature_requests}

val find_feature_selection : graph -> string -> feature_selection option
val feature_selection_to_option : feature_selection -> string list option

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

module For_test : sig
  val create_feature_requests : unit -> feature_requests
  val add_feature_request :
    feature_requests -> string -> string list option -> unit
  val find_feature_selection :
    feature_requests -> string -> feature_selection option
end
