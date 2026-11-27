type t

type admission =
  | Skip
  | Visit_current
  | Visit_current_and_descendants
  | Visit_descendants

val admit : (string, t) Hashtbl.t -> string -> recursive:bool -> admission
val visits_current : admission -> bool
val visits_descendants : admission -> bool
