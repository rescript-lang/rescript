(** Immutable record tracking optional argument usage.
    - unused: args that have never been passed
    - alwaysUsed: args that are always passed (when count > 0)
    - count: number of calls observed *)

module StringSet = Set.Make (String)

type t = {count: int; unused: StringSet.t; alwaysUsed: StringSet.t}

let empty = {unused = StringSet.empty; alwaysUsed = StringSet.empty; count = 0}

let fromList l =
  {unused = StringSet.of_list l; alwaysUsed = StringSet.empty; count = 0}

let isEmpty x = StringSet.is_empty x.unused

(** Apply a call to the optional args state. Returns new state. *)
let apply_call ~argNames ~argNamesMaybe x =
  let nameSet = argNames |> StringSet.of_list in
  let nameSetMaybe = argNamesMaybe |> StringSet.of_list in
  let nameSetAlways = StringSet.diff nameSet nameSetMaybe in
  let alwaysUsed =
    if x.count = 0 then nameSetAlways
    else StringSet.inter nameSetAlways x.alwaysUsed
  in
  let unused =
    argNames
    |> List.fold_left (fun acc name -> StringSet.remove name acc) x.unused
  in
  {count = x.count + 1; unused; alwaysUsed}

(** Combine two optional args states (for function references).
    Returns a pair of updated states with intersected unused/alwaysUsed. *)
let combine_pair x y =
  let unused = StringSet.inter x.unused y.unused in
  let alwaysUsed = StringSet.inter x.alwaysUsed y.alwaysUsed in
  ({x with unused; alwaysUsed}, {y with unused; alwaysUsed})

let iterUnused f x = StringSet.iter f x.unused
let iterAlwaysUsed f x = StringSet.iter (fun s -> f s x.count) x.alwaysUsed

let foldUnused f x init = StringSet.fold f x.unused init

let foldAlwaysUsed f x init =
  StringSet.fold (fun s acc -> f s x.count acc) x.alwaysUsed init
