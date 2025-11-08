type suggested_action =
  | ApplyAutomaticMigrationsForFullProject
  | ApplyAutomaticMigrationsForCurrentFile

type record = {loc: Warnings.loc; action: suggested_action}

val add : record -> unit
val clear : unit -> unit
val has : record -> bool
val print_block_if_any : unit -> unit
