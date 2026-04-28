(** Position utilities. *)

(** Format a position as "filename:line:col" *)
let toString (pos : Lexing.position) =
  let file = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (file |> Filename.basename)
  ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col
