type 'a inner = 'a list
type 'a t = 'a inner Offheap.t

let unsafe_of_list = Offheap.unsafe_of_value
let of_list = Offheap.of_value
let list_of = Offheap.unsafe_to_value
let unsafe_of_offheap_list xs = unsafe_of_list (Offheap.unsafe_to_value xs)

let empty () : 'a t = unsafe_of_list []

let is_empty xs =
  match list_of xs with
  | [] -> true
  | _ -> false

let rec length_list acc = function
  | [] -> acc
  | _ :: rest -> length_list (acc + 1) rest

let length xs = length_list 0 (list_of xs)

let rec iter_list f = function
  | [] -> ()
  | x :: rest ->
    f x;
    iter_list f rest

let iter f xs = iter_list f (list_of xs)

let rec iter_list_with f arg = function
  | [] -> ()
  | x :: rest ->
    f arg x;
    iter_list_with f arg rest

let iter_with f arg xs = iter_list_with f arg (list_of xs)

let rec exists_list f = function
  | [] -> false
  | x :: rest -> f x || exists_list f rest

let exists f xs = exists_list f (list_of xs)

let rec exists_list_with f arg = function
  | [] -> false
  | x :: rest -> f arg x || exists_list_with f arg rest

let exists_with f arg xs = exists_list_with f arg (list_of xs)
