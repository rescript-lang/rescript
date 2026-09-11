type dependency_kind = Regular | Development

type request = {kind: dependency_kind; declaration: Config.dependency}

type resolved = {request: request; dependency: Package_resolution.dependency}

val dependency_kind_name : dependency_kind -> string
val requests : prod:bool -> is_local:bool -> Config.t -> request list

val resolve : Package_resolution.t -> package_root:string -> request -> resolved

val add_feature_request : Feature_requests.t -> resolved -> unit
