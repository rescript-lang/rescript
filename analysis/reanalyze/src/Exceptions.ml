open Common

type t = ExnSet.t

let add = ExnSet.add
let diff = ExnSet.diff
let empty = ExnSet.empty
let fromList = ExnSet.of_list
let toList = ExnSet.elements
let isEmpty = ExnSet.is_empty
let iter = ExnSet.iter
let union = ExnSet.union

let pp ~exnTable ppf exceptions =
  let isFirst = ref true in
  let ppExn exn =
    let separator = if !isFirst then "" else ", " in
    isFirst := false;
    let name = Exn.toString exn in
    match exnTable with
    | Some exnTable -> (
      match Hashtbl.find_opt exnTable exn with
      | Some locSet ->
        let positions =
          locSet |> Common.LocSet.elements
          |> List.map (fun loc -> loc.Location.loc_start)
        in
        Format.fprintf ppf "%s@{<info>%s@} (@{<filename>%s@})" separator name
          (positions |> List.map posToString |> String.concat " ")
      | None -> Format.fprintf ppf "%s@{<info>%s@}" separator name)
    | None -> Format.fprintf ppf "%s@{<info>%s@}" separator name
  in
  let isList = exceptions |> ExnSet.cardinal > 1 in
  if isList then Format.fprintf ppf "[";
  exceptions |> ExnSet.iter ppExn;
  if isList then Format.fprintf ppf "]"
