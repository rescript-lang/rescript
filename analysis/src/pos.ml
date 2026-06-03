type t = int * int

let of_lexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  (pos_lnum - 1, pos_cnum - pos_bol)

let to_string (loc, col) = Printf.sprintf "%d:%d" loc col

let offset_of_line text line =
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

let position_to_offset text (line, character) =
  match offset_of_line text line with
  | None -> None
  | Some bol ->
    let offset = bol + character in
    if offset >= 0 && offset <= String.length text then Some offset else None

let pos_before_cursor pos = (fst pos, max 0 (snd pos - 1))

let pos_of_dot text ~(pos : int * int) ~offset =
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
  | Some offset_before_dot ->
    let line, col = pos in
    let new_col = max 0 (col - (offset - offset_before_dot)) in
    Some (line, new_col)
