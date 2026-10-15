(** State that belongs to one compiler request.

    A key behaves like a [Domain.DLS] key, and {!with_fresh} gives every key a
    new value from its initializer for the duration of a request. A request
    therefore starts from the same state as a new compiler process, and no key
    can be forgotten when requests run one after another on a domain. *)

type 'a key = 'a Domain.DLS.key

val new_key : (unit -> 'a) -> 'a key
(** Create a key during module initialization, before any worker domain
    starts. *)

val get : 'a key -> 'a
val set : 'a key -> 'a -> unit

val with_fresh : (unit -> 'a) -> 'a
(** Run with a fresh value for every key and restore the previous values
    afterwards, also when the action raises, so requests can nest. *)
