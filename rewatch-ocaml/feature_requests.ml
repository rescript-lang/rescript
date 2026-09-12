type selection = All | Selected of string list
type t = (string, selection) Hashtbl.t

let create () = Hashtbl.create 32

let add requests root request =
  match (Hashtbl.find_opt requests root, request) with
  | None, None -> Hashtbl.add requests root All
  | None, Some requested -> Hashtbl.add requests root (Selected requested)
  | Some All, _ | Some _, None -> Hashtbl.replace requests root All
  | Some (Selected current), Some requested ->
    Hashtbl.replace requests root
      (Selected (List.sort_uniq String.compare (current @ requested)))

let find requests root = Hashtbl.find_opt requests root
let to_option = function
  | All -> None
  | Selected features -> Some features
