type t = Location.t

let start (loc : t) = Pos.of_lexing loc.loc_start
let end_ (loc : t) = Pos.of_lexing loc.loc_end
let range loc : Range.t = (start loc, end_ loc)

let to_string (loc : t) =
  (if loc.loc_ghost then "__ghost__" else "") ^ (loc |> range |> Range.to_string)

let has_pos ~pos loc = start loc <= pos && pos < end_ loc

(** Allows the character after the end to be included. Ie when the cursor is at the
    end of the word, like `someIdentifier<cursor>`. Useful in some scenarios. *)
let has_pos_inclusive_end ~pos loc = start loc <= pos && pos <= end_ loc

let mk_position (pos : Pos.t) =
  let line, character = pos in
  Lsp.Types.Position.create ~line ~character

let range_of_loc (loc : t) =
  let start = loc |> start |> mk_position in
  let end_ = loc |> end_ |> mk_position in
  Lsp.Types.Range.create ~start ~end_

let is_inside (x : t) (y : t) =
  x.loc_start.pos_cnum >= y.loc_start.pos_cnum
  && x.loc_end.pos_cnum <= y.loc_end.pos_cnum
  && x.loc_start.pos_lnum >= y.loc_start.pos_lnum
  && x.loc_end.pos_lnum <= y.loc_end.pos_lnum
