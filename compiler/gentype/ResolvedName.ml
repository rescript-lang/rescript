type t = string list

let dot s x = x @ [s]
let from_string x = [x]
let to_list x = x
let to_string x = x |> String.concat "_"

type eq = t * t

module NameSet = Set.Make (struct
  type nonrec t = t

  let rec compare (x : t) (y : t) =
    match (x, y) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> -1
    | s1 :: rest1, s2 :: rest2 -> (
      let n = String.compare s1 s2 in
      match n <> 0 with
      | true -> n
      | false -> compare rest1 rest2)
end)

let rec apply_equation ~(el : t) (eq : eq) : t list =
  match (eq, el) with
  | ([], rhs), _ -> [rhs @ el]
  | (s1 :: rest1, rhs), s2 :: rest2 -> (
    match s1 = s2 with
    | true -> (rest1, rhs) |> apply_equation ~el:rest2
    | false -> [])
  | (_ :: _, _), [] -> []

let rec apply_equations_to_elements ~(eqs : eq list) ~seen (elements : t list) :
    eq list =
  let apply_eqs el =
    let fresh_elements =
      eqs
      |> List.map (apply_equation ~el)
      |> List.concat
      |> List.filter (fun y -> not (NameSet.mem y seen))
    in
    fresh_elements |> List.map (fun el_fresh -> (el_fresh, el))
  in
  let new_equations = elements |> List.map apply_eqs |> List.concat in
  let new_elements = new_equations |> List.map fst in
  let new_seen = NameSet.union seen (new_elements |> NameSet.of_list) in
  match new_equations = [] with
  | true -> new_equations
  | false ->
    new_equations
    @ (new_elements |> apply_equations_to_elements ~eqs ~seen:new_seen)

(* Apply equations of the form e.g. X.Y = A from the alias: module A = X.Y.
   Return a list of equations on types.
   E.g. if the element is X.Y.t, return equation A.t = X.Y.t *)

let apply_equations ~(eqs : eq list) (el : t) : eq list =
  [el] |> apply_equations_to_elements ~eqs ~seen:NameSet.empty
