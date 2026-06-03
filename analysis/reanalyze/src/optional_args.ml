(** Immutable record tracking optional argument usage.
    - unused: args that have never been passed
    - alwaysUsed: args that are always passed (when count > 0)
    - count: number of calls observed *)

module String_set = Set.Make (String)

type t = {count: int; unused: String_set.t; always_used: String_set.t}

let empty =
  {unused = String_set.empty; always_used = String_set.empty; count = 0}

let from_list l =
  {unused = String_set.of_list l; always_used = String_set.empty; count = 0}

let is_empty x = String_set.is_empty x.unused

(** Apply a call to the optional args state. Returns new state. *)
let apply_call ~arg_names ~arg_names_maybe x =
  let name_set = arg_names |> String_set.of_list in
  let name_set_maybe = arg_names_maybe |> String_set.of_list in
  let name_set_always = String_set.diff name_set name_set_maybe in
  let always_used =
    if x.count = 0 then name_set_always
    else String_set.inter name_set_always x.always_used
  in
  let unused =
    arg_names
    |> List.fold_left (fun acc name -> String_set.remove name acc) x.unused
  in
  {count = x.count + 1; unused; always_used}

(** Combine two optional args states (for function references).
    Returns a pair of updated states with intersected unused/alwaysUsed. *)
let combine_pair x y =
  let unused = String_set.inter x.unused y.unused in
  let always_used = String_set.inter x.always_used y.always_used in
  ({x with unused; always_used}, {y with unused; always_used})

let iter_unused f x = String_set.iter f x.unused
let iter_always_used f x = String_set.iter (fun s -> f s x.count) x.always_used

let fold_unused f x init = String_set.fold f x.unused init

let fold_always_used f x init =
  String_set.fold (fun s acc -> f s x.count acc) x.always_used init
