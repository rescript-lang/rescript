exception Error of string

val bundled_bsc : cwd:string -> executable:string -> string
val runtime : find_package:(string -> string option) -> string
