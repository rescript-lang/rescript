type t = Location.t

let start (loc : t) = Pos.ofLexing loc.loc_start
let end_ (loc : t) = Pos.ofLexing loc.loc_end
let range loc : Range.t = (start loc, end_ loc)

let toString (loc : t) =
  (if loc.loc_ghost then "__ghost__" else "") ^ (loc |> range |> Range.toString)

let hasPos ~pos loc = start loc <= pos && pos < end_ loc

(** Allows the character after the end to be included. Ie when the cursor is at the 
    end of the word, like `someIdentifier<cursor>`. Useful in some scenarios. *)
let hasPosInclusiveEnd ~pos loc = start loc <= pos && pos <= end_ loc

let mkPosition (pos : Pos.t) =
  let line, character = pos in
  {Protocol.line; character}

let rangeOfLoc (loc : t) =
  let start = loc |> start |> mkPosition in
  let end_ = loc |> end_ |> mkPosition in
  {Protocol.start; end_}
