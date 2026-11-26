val map : ('a -> 'b) -> 'a list -> 'b list
(** [map f items] applies [f] to every item on up to the compiler worker count
    of domains, for many short, independent, mostly I/O-bound items such as
    reading artifact headers. The results keep the order of [items]. If [f]
    raises, the exception of the first failing item is raised once every
    domain has stopped. *)
