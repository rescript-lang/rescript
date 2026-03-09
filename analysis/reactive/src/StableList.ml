type 'a t = 'a Stable.t list

(* Zero-cost reinterpretation: Stable.t is identity at runtime,
   so 'a list and 'a Stable.t list have the same representation. *)
external reinterpret : 'a list -> 'a Stable.t list = "%identity"

let unsafe_of_list (l : 'a list) : 'a t = reinterpret l

let of_list (l : 'a list) : 'a t =
  ignore (Stable.of_value l);
  reinterpret l

let to_stable (l : 'a t) : 'a t Stable.t = Stable.unsafe_of_value l

let maybe_to_stable (m : 'a t Maybe.t) : 'a t Maybe.t Stable.t =
  Stable.unsafe_of_value m

let empty () : 'a t = []
let is_empty xs = xs = []
let length = List.length
let iter = List.iter
let exists = List.exists

let rec iter_with f arg = function
  | [] -> ()
  | x :: rest ->
    f arg x;
    iter_with f arg rest

let rec exists_with f arg = function
  | [] -> false
  | x :: rest -> f arg x || exists_with f arg rest
