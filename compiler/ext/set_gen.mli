type 'a t = private
  | Empty
  | Leaf of 'a
  | Node of {l: 'a t; v: 'a; r: 'a t; h: int}

val empty : 'a t

val is_empty : 'a t -> bool

val unsafe_two_elements : 'a -> 'a -> 'a t

val cardinal : 'a t -> int

val elements : 'a t -> 'a list

val choose : 'a t -> 'a

val iter : 'a t -> ('a -> unit) -> unit

val fold : 'a t -> 'c -> ('a -> 'c -> 'c) -> 'c

val for_all : 'a t -> ('a -> bool) -> bool

val exists : 'a t -> ('a -> bool) -> bool

val check : 'a t -> unit

val bal : 'a t -> 'a -> 'a t -> 'a t

val remove_min_elt : 'a t -> 'a t

val singleton : 'a -> 'a t

val internal_merge : 'a t -> 'a t -> 'a t

val internal_join : 'a t -> 'a -> 'a t -> 'a t

val internal_concat : 'a t -> 'a t -> 'a t

val partition : 'a t -> ('a -> bool) -> 'a t * 'a t

val of_sorted_array : 'a array -> 'a t

val is_ordered : cmp:('a -> 'a -> int) -> 'a t -> bool

val invariant : cmp:('a -> 'a -> int) -> 'a t -> bool

module type S = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val iter : t -> (elt -> unit) -> unit

  val fold : t -> 'a -> (elt -> 'a -> 'a) -> 'a

  val for_all : t -> (elt -> bool) -> bool

  val exists : t -> (elt -> bool) -> bool

  val singleton : elt -> t

  val cardinal : t -> int

  val elements : t -> elt list

  val choose : t -> elt

  val mem : t -> elt -> bool

  val add : t -> elt -> t

  val remove : t -> elt -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val of_list : elt list -> t

  val of_sorted_array : elt array -> t

  val invariant : t -> bool

  val print : Format.formatter -> t -> unit
end
