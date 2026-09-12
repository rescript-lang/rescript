val for_package : is_local:bool -> Config.t -> string list
val report_missing_source_folder : Config.t -> string -> unit
val report_missing_sources : is_root:bool -> Config.t -> unit
val validate_metadata : Config.t -> unit

module For_test : sig
  val package_name : string -> (string option, string) result
  val issue_tracker_url : string -> string option
end
