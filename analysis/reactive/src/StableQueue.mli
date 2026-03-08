(** Stable FIFO queues for reactive internals. *)

type 'a t

val create : unit -> 'a t
(** Create an empty FIFO queue. *)

val destroy : 'a t -> unit
(** Release the queue's owned stable storage. The queue must not be used
    afterwards. *)

val clear : 'a t -> unit
(** Remove all elements while keeping the current storage. *)

val push : 'a t -> 'a Stable.t -> unit
(** Add an element at the tail of the queue. *)

val is_empty : 'a t -> bool
(** Whether the queue currently holds no elements. *)

val pop : 'a t -> 'a Stable.t
(** Remove and return the next element.

    @raise Invalid_argument if the queue is empty. *)
