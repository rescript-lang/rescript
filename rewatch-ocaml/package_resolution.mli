type t

type dependency = {directory: string; config: Config.t; is_local: bool}

type diagnostic_mode = Report_diagnostics | Suppress_diagnostics

val create : ?diagnostic_mode:diagnostic_mode -> Config.t -> t
val resolve : t -> package_root:string -> Config.dependency -> dependency
val dependency_path : t -> package_root:string -> string -> string option
val dependency_candidates : t -> package_root:string -> string -> string list
val is_local : t -> string -> bool
