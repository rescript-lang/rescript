type t = Shallow | Recursive

type admission =
  | Skip
  | Visit_current
  | Visit_current_and_descendants
  | Visit_descendants

let admit visited identity ~recursive =
  match (Hashtbl.find_opt visited identity, recursive) with
  | None, false ->
    Hashtbl.add visited identity Shallow;
    Visit_current
  | None, true ->
    Hashtbl.add visited identity Recursive;
    Visit_current_and_descendants
  | Some Shallow, true ->
    Hashtbl.replace visited identity Recursive;
    Visit_descendants
  | Some Shallow, false | Some Recursive, _ -> Skip

let visits_current = function
  | Visit_current | Visit_current_and_descendants -> true
  | Skip | Visit_descendants -> false

let visits_descendants = function
  | Visit_current_and_descendants | Visit_descendants -> true
  | Skip | Visit_current -> false
