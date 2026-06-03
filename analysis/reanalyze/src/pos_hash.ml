(** Position-keyed hashtable.
    Used throughout dead code analysis for mapping source positions to data. *)

include Hashtbl.Make (struct
  type t = Lexing.position

  let hash x =
    let s = Filename.basename x.Lexing.pos_fname in
    Hashtbl.hash (x.Lexing.pos_cnum, s)

  let equal (x : t) y = x = y
end)
