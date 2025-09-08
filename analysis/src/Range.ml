type t = Pos.t * Pos.t

let toString ((posStart, posEnd) : t) =
  Printf.sprintf "[%s->%s]" (Pos.toString posStart) (Pos.toString posEnd)

let hasPos ~pos ((posStart, posEnd) : t) = posStart <= pos && pos < posEnd
