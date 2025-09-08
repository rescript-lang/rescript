type ('key, +'a) t = private
  | Empty
  | Leaf of {k: 'key; v: 'a}
  | Node of {l: ('key, 'a) t; k: 'key; v: 'a; r: ('key, 'a) t; h: int}

val cardinal : ('a, 'b) t -> int

val bindings : ('a, 'b) t -> ('a * 'b) list

val fill_array_with_f : ('a, 'b) t -> int -> 'c array -> ('a -> 'b -> 'c) -> int

val fill_array_aux : ('a, 'b) t -> int -> ('a * 'b) array -> int

val to_sorted_array : ('key, 'a) t -> ('key * 'a) array

val to_sorted_array_with_f : ('a, 'b) t -> ('a -> 'b -> 'c) -> 'c array

val keys : ('a, 'b) t -> 'a list

val height : ('a, 'b) t -> int

val singleton : 'a -> 'b -> ('a, 'b) t

val unsafe_node : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t -> int -> ('a, 'b) t

val unsafe_two_elements : 'a -> 'b -> 'a -> 'b -> ('a, 'b) t
(** smaller comes first *)

val bal : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val empty : ('a, 'b) t

val is_empty : ('a, 'b) t -> bool

val merge : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val iter : ('a, 'b) t -> ('a -> 'b -> unit) -> unit

val map : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t

val mapi : ('a, 'b) t -> ('a -> 'b -> 'c) -> ('a, 'c) t

val fold : ('a, 'b) t -> 'c -> ('a -> 'b -> 'c -> 'c) -> 'c

val for_all : ('a, 'b) t -> ('a -> 'b -> bool) -> bool

val exists : ('a, 'b) t -> ('a -> 'b -> bool) -> bool

val join : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

val concat : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val concat_or_join : ('a, 'b) t -> 'a -> 'b option -> ('a, 'b) t -> ('a, 'b) t

module type S = sig
  type key

  type +'a t

  val empty : 'a t

  val compare_key : key -> key -> int

  val is_empty : 'a t -> bool

  val mem : 'a t -> key -> bool

  val to_sorted_array : 'a t -> (key * 'a) array

  val to_sorted_array_with_f : 'a t -> (key -> 'a -> 'b) -> 'b array

  val add : 'a t -> key -> 'a -> 'a t

  val adjust : 'a t -> key -> ('a option -> 'a) -> 'a t

  val singleton : key -> 'a -> 'a t

  val remove : 'a t -> key -> 'a t

  (* val merge :
     'a t -> 'b t -> (key -> 'a option -> 'b option -> 'c option) -> 'c t *)
  val disjoint_merge_exn : 'a t -> 'a t -> (key -> 'a -> 'a -> exn) -> 'a t

  val iter : 'a t -> (key -> 'a -> unit) -> unit

  val fold : 'a t -> 'b -> (key -> 'a -> 'b -> 'b) -> 'b

  val for_all : 'a t -> (key -> 'a -> bool) -> bool

  val exists : 'a t -> (key -> 'a -> bool) -> bool

  (* val filter : 'a t -> (key -> 'a -> bool) -> 'a t *)
  (* val partition : 'a t -> (key -> 'a -> bool) -> 'a t * 'a t *)
  val cardinal : 'a t -> int

  val bindings : 'a t -> (key * 'a) list

  val keys : 'a t -> key list
  (* val choose : 'a t -> key * 'a *)

  val find_exn : 'a t -> key -> 'a

  val find_opt : 'a t -> key -> 'a option

  val find_default : 'a t -> key -> 'a -> 'a

  val map : 'a t -> ('a -> 'b) -> 'b t

  val mapi : 'a t -> (key -> 'a -> 'b) -> 'b t

  val of_list : (key * 'a) list -> 'a t

  val of_array : (key * 'a) array -> 'a t

  val add_list : (key * 'b) list -> 'b t -> 'b t
end
