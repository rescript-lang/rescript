(** Array-based FIFO queue. After [clear], subsequent [push] calls
    reuse existing array slots — zero allocation until the array
    needs to grow beyond its high-water mark. *)

type 'a t

val create : unit -> 'a t
val clear : 'a t -> unit
val push : 'a t -> 'a -> unit
val is_empty : 'a t -> bool

val pop : 'a t -> 'a
(** @raise Invalid_argument if the queue is empty. *)
