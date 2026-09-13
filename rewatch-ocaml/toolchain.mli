exception Error of string

val sibling_bsc_candidate : cwd:string -> executable:string -> string
val bsc : unit -> string
val runtime : find_package:(string -> string option) -> string
