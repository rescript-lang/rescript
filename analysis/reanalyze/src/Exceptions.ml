module ExnSet = Set.Make (Exn)

type t = ExnSet.t

let add = ExnSet.add
let diff = ExnSet.diff
let empty = ExnSet.empty
let from_list = ExnSet.of_list
let to_list = ExnSet.elements
let is_empty = ExnSet.is_empty
let iter = ExnSet.iter
let union = ExnSet.union

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
          loc_set |> LocSet.elements
          |> List.map (fun loc -> loc.Location.loc_start)
        in
        Format.fprintf ppf "%s@{<info>%s@} (@{<filename>%s@})" separator name
          (positions |> List.map Pos.to_string |> String.concat " ")
      | None -> Format.fprintf ppf "%s@{<info>%s@}" separator name)
    | None -> Format.fprintf ppf "%s@{<info>%s@}" separator name
  in
  let is_list = exceptions |> ExnSet.cardinal > 1 in
  if is_list then Format.fprintf ppf "[";
  exceptions |> ExnSet.iter pp_exn;
  if is_list then Format.fprintf ppf "]"
