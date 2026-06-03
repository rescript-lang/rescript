type t = Pos.t * Pos.t

let to_string ((pos_start, pos_end) : t) =
  Printf.sprintf "[%s->%s]" (Pos.to_string pos_start) (Pos.to_string pos_end)

let has_pos ~pos ((pos_start, pos_end) : t) = pos_start <= pos && pos < pos_end
