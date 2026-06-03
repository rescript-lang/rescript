module Exn_set = Set.Make (Exn)

type t = Exn_set.t

let add = Exn_set.add
let diff = Exn_set.diff
let empty = Exn_set.empty
let from_list = Exn_set.of_list
let to_list = Exn_set.elements
let is_empty = Exn_set.is_empty
let iter = Exn_set.iter
let union = Exn_set.union

let pp ~exn_table ppf exceptions =
  let is_first = ref true in
  let pp_exn exn =
    let separator = if !is_first then "" else ", " in
    is_first := false;
    let name = Exn.to_string exn in
    match exn_table with
    | Some exn_table -> (
      match Hashtbl.find_opt exn_table exn with
      | Some loc_set ->
        let positions =
          loc_set |> Loc_set.elements
          |> List.map (fun loc -> loc.Location.loc_start)
        in
        Format.fprintf ppf "%s@{<info>%s@} (@{<filename>%s@})" separator name
          (positions |> List.map Pos.to_string |> String.concat " ")
      | None -> Format.fprintf ppf "%s@{<info>%s@}" separator name)
    | None -> Format.fprintf ppf "%s@{<info>%s@}" separator name
  in
  let is_list = exceptions |> Exn_set.cardinal > 1 in
  if is_list then Format.fprintf ppf "[";
  exceptions |> Exn_set.iter pp_exn;
  if is_list then Format.fprintf ppf "]"
