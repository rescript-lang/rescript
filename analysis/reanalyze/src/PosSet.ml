(** Position set.
    Used for tracking sets of source positions in dead code analysis. *)

include Set.Make (struct
  type t = Lexing.position

  let compare = compare
end)
