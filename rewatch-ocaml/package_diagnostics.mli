val for_package : is_local:bool -> Config.t -> string list
val report_missing_source_folder : Config.t -> string -> unit
val report_missing_sources : is_root:bool -> Config.t -> unit
val validate_metadata : Config.t -> unit
