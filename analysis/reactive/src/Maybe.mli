(** Zero-allocation unboxed optional values.

    An ['a t] is either [none] (a unique sentinel) or [some v].
    Unlike [option], wrapping a value with [some] performs no allocation —
    it is a plain [Obj.repr] cast.

    {b Safety contract:} [get] must only be called after [is_some] returns
    [true]. Calling [get] on [none] is undefined behavior. *)

type 'a t

val none : 'a t
(** Unique sentinel representing the absent case. *)

val none_stable : 'a t Stable.t
(** Stable-marked form of [none]. Safe because the sentinel is allocated
    outside the minor heap and kept reachable for the lifetime of the process. *)

val some : 'a -> 'a t
val is_none : 'a t -> bool
val is_some : 'a t -> bool
val unsafe_get : 'a t -> 'a

val to_option : 'a t -> 'a option

val to_stable : 'a Stable.t t -> 'a t Stable.t
(** Reorder [Stable.t] outside [Maybe.t]. Zero allocation. *)

val of_stable : 'a t Stable.t -> 'a Stable.t t
(** Reorder [Stable.t] inside [Maybe.t]. Zero allocation. *)
