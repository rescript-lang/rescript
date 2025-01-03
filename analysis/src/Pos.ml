type t = int * int

let ofLexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  (pos_lnum - 1, pos_cnum - pos_bol)

let toString (loc, col) = Printf.sprintf "%d:%d" loc col

let offsetOfLine text line =
  let ln = String.length text in
  let rec loop i lno =
    if i >= ln then None
    else
      match text.[i] with
      | '\n' -> if lno = line - 1 then Some (i + 1) else loop (i + 1) (lno + 1)
      | _ -> loop (i + 1) lno
  in
  match line with
  | 0 -> Some 0
  | _ -> loop 0 0

let positionToOffset text (line, character) =
  match offsetOfLine text line with
  | None -> None
  | Some bol ->
    if bol + character <= String.length text then Some (bol + character)
    else None

let posBeforeCursor pos = (fst pos, max 0 (snd pos - 1))

let posOfDot text ~(pos : int * int) ~offset =
  let rec loop i =
    if i < 0 then None
    else
      match text.[i] with
      | '.' -> Some (i + 1)
      | '\n' -> None
      | _ -> loop (i - 1)
  in
  match loop (offset - 1) with
  | None -> None
  | Some offsetBeforeDot ->
    let line, col = pos in
    let newCol = max 0 (col - (offset - offsetBeforeDot)) in
    Some (line, newCol)
