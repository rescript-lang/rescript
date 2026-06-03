type t = string option

let break ~indent =
  match indent with
  | None -> ""
  | Some s -> "\n" ^ s
let more indent =
  match indent with
  | None -> None
  | Some s -> Some ("  " ^ s)

let heuristic_fields ~indent fields =
  let threshold = 2 in
  match fields |> List.length > threshold && indent = None with
  | true -> Some ""
  | false -> indent

let heuristic_variants ~indent rendered =
  let threshold = 40 in
  let break = rendered |> String.concat " " |> String.length > threshold in
  match break && indent = None with
  | true -> Some "  "
  | false -> indent
