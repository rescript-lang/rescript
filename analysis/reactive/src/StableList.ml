type 'a t = 'a list

let unsafe_of_list (l : 'a list) : 'a t = l

let of_list (l : 'a list) : 'a t =
  ignore (Stable.of_value l);
  l

let to_stable (l : 'a t) : 'a t Stable.t = Stable.unsafe_of_value l
let maybe_to_stable (m : 'a t Maybe.t) : 'a t Maybe.t Stable.t =
  Stable.unsafe_of_value m

let empty () : 'a t = []

let is_empty = function
  | [] -> true
  | _ -> false

let rec length_list acc = function
  | [] -> acc
  | _ :: rest -> length_list (acc + 1) rest

let length xs = length_list 0 xs

let[@inline] stable x = Stable.unsafe_of_value x

let rec iter_list f = function
  | [] -> ()
  | x :: rest ->
    f (stable x);
    iter_list f rest

let iter f xs = iter_list f xs

let rec iter_list_with f arg = function
  | [] -> ()
  | x :: rest ->
    f arg (stable x);
    iter_list_with f arg rest

let iter_with f arg xs = iter_list_with f arg xs

let rec exists_list f = function
  | [] -> false
  | x :: rest -> f (stable x) || exists_list f rest

let exists f xs = exists_list f xs

let rec exists_list_with f arg = function
  | [] -> false
  | x :: rest -> f arg (stable x) || exists_list_with f arg rest

let exists_with f arg xs = exists_list_with f arg xs
